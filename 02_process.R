# =============================================================================
# 02_process.R
# Nest box microclimate & incubation logger — data processing
#
# Input:  tof_clean.csv, dht_clean.csv, log_clean.csv  (from 01_ingest.R)
#         METADATA.TXT  (same deployment folder)
# Output: tof_processed.csv  — smoothed ToF with state labels and bout IDs
#         bout_summary.csv   — one row per incubation bout
#         day_summary.csv    — one row per calendar day
#         dht_processed.csv  — DHT data with gap-interpolated timestamps
#
# State detection overview
# ------------------------
# The algorithm is threshold-free and drift-resistant. It works in two passes:
#
# Pass 1 — Smooth:
#   Replace the on-device EMA (which resets on every reboot) with a centered
#   rolling median computed in R. Rolling medians are robust to outliers and
#   do not require the signal to converge after a cold start.
#
# Pass 2 — Detect regimes:
#   Use PELT (Pruned Exact Linear Time) change-point detection on the smoothed
#   signal to locate sustained shifts in the mean. PELT makes no assumption
#   about the number of change points; the MBIC penalty keeps it from over-
#   segmenting on sensor noise. Each segment between change points is then
#   classified as "present" or "absent" by comparing its median to the
#   deployment-wide grand median of the smoothed signal. A segment whose
#   median lies below (grand median − sensitivity × IQR) is "present"; above
#   (grand median + sensitivity × IQR) is "absent"; otherwise "uncertain".
#   The IQR-based scaling means the threshold adapts to the actual spread of
#   the signal in each deployment.
#
# Why this approach?
#   • Drift-resistant: no hard-coded distance threshold; uses relative deviations
#   • Lenient: change-points require *sustained* shifts, not instantaneous ones
#   • Correctable: the Shiny app can override any segment's label
# =============================================================================

library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)
library(zoo)          # rollmedian
library(changepoint)  # cpt.mean / PELT


# =============================================================================
# USER SETTINGS  — these are also exposed as sliders in the Shiny app
# =============================================================================

DEPLOYMENT_DIR <- "."

# Rolling median window for R-side smoothing (seconds).
# Must be an odd number of readings. At 2 s per reading, 75 s = 37 readings.
# Wider = smoother but slower to respond to rapid state changes.
SMOOTH_WINDOW_SEC <- 75

# PELT penalty for change-point detection.
# "MBIC" is conservative and works well; "BIC" gives more change points.
# See ?changepoint::cpt.mean for alternatives.
CPT_PENALTY <- "MBIC"

# State classification sensitivity (multiplier on the deployment IQR).
# Lower values are more sensitive (more readings classified as present/absent);
# higher values shrink the "uncertain" band.
# Default 0.5 is a good starting point; adjust per deployment in the Shiny app.
SENSITIVITY <- 0.5

# Minimum bout duration to include in summary (seconds).
# Bouts shorter than this are likely sensor artefacts or brief disturbances.
MIN_BOUT_SEC <- 60


# =============================================================================
# HELPERS
# =============================================================================

`%||%` <- function(a, b) if (!is.null(a) && nchar(a) > 0) a else b

parse_metadata <- function(path) {
  lines <- readLines(path, warn = FALSE)
  lines <- lines[nchar(trimws(lines)) > 0]
  lines <- lines[grepl("=", lines, fixed = TRUE)]
  pairs <- stringr::str_split_fixed(lines, "=", n = 2)
  setNames(as.list(trimws(pairs[, 2])), trimws(pairs[, 1]))
}

# Ensure rolling window is a valid odd integer for rollmedian
valid_window <- function(sec, interval_sec = 2) {
  k <- round(sec / interval_sec)
  if (k %% 2 == 0) k <- k + 1
  max(k, 3L)
}


# =============================================================================
# 1. LOAD CLEANED DATA
# =============================================================================

message("\n=== 02_process.R ===\n")

dir_path <- normalizePath(DEPLOYMENT_DIR, mustWork = TRUE)

tof_clean <- read_csv(file.path(dir_path, "tof_clean.csv"),
                      show_col_types = FALSE) |>
  mutate(timestamp = ymd_hms(timestamp))

dht_clean <- read_csv(file.path(dir_path, "dht_clean.csv"),
                      show_col_types = FALSE) |>
  mutate(timestamp = ymd_hms(timestamp))

log_clean <- read_csv(file.path(dir_path, "log_clean.csv"),
                      show_col_types = FALSE) |>
  mutate(timestamp = ymd_hms(timestamp))

meta <- parse_metadata(file.path(dir_path, "METADATA.TXT"))

message("  Deployment : ", meta$deployment_id %||% "(unknown)")
message("  Device     : ", meta$device_id     %||% "(unknown)")
message("  TOF rows   : ", nrow(tof_clean))
message("  DHT rows   : ", nrow(dht_clean))


# =============================================================================
# 2. TOF SMOOTHING — rolling median (R-side, replaces on-device EMA)
# =============================================================================

message("\nSmoothing TOF data ...")

# Median sampling interval (seconds) across valid, non-warmup readings
# Used to convert time-based window sizes into row counts
typical_interval <- tof_clean |>
  filter(!flag_tof_invalid, !flag_ema_warmup, !is.na(dt_sec)) |>
  pull(dt_sec) |>
  median(na.rm = TRUE)

message("  Typical TOF sampling interval: ", round(typical_interval, 1), " s")

k_smooth <- valid_window(SMOOTH_WINDOW_SEC, typical_interval)
message("  Rolling median window: ", k_smooth, " readings (~",
        round(k_smooth * typical_interval), " s)")

# Apply rolling median session-by-session so reboots don't bleed across sessions.
# Invalid and warm-up rows are treated as NA during smoothing; their smoothed
# values will also be NA (fill = NA propagates).
tof_proc <- tof_clean |>
  group_by(session_id) |>
  mutate(
    tof_smooth_r = rollmedian(
      if_else(flag_tof_invalid | flag_ema_warmup, NA_real_, tof_raw_mm),
      k     = k_smooth,
      fill  = NA,
      align = "center"
    )
  ) |>
  ungroup()


# =============================================================================
# 3. STATE DETECTION — PELT change-point segmentation
# =============================================================================

message("\nDetecting incubation states ...")

# Work only on valid, non-warmup rows with a smoothed value.
# We store the row indices so results can be joined back to the full dataset.
valid_idx <- which(!tof_proc$flag_tof_invalid &
                   !tof_proc$flag_ema_warmup  &
                   !is.na(tof_proc$tof_smooth_r))

message("  Valid rows for state detection: ", length(valid_idx),
        " of ", nrow(tof_proc), " total")

if (length(valid_idx) < 10) {
  warning("Too few valid rows for state detection. ",
          "Check for excessive invalid/warm-up flags.")
  tof_proc$state_auto <- NA_character_
  tof_proc$segment_id <- NA_integer_
} else {
  signal <- tof_proc$tof_smooth_r[valid_idx]

  # PELT change-point detection on the mean of the smoothed signal.
  # MBIC penalty avoids over-segmentation on sensor noise.
  cpt_result <- tryCatch(
    cpt.mean(signal, method = "PELT", penalty = CPT_PENALTY),
    error = function(e) {
      message("  Change-point detection failed: ", conditionMessage(e))
      NULL
    }
  )

  if (is.null(cpt_result)) {
    # Fallback: treat whole deployment as one segment
    segment_labels <- rep(1L, length(signal))
  } else {
    cpt_positions <- c(0L, cpts(cpt_result), length(signal))
    n_segments    <- length(cpt_positions) - 1L
    message("  Change points detected: ", n_segments - 1L,
            " → ", n_segments, " segment(s)")

    # rep() is cleaner than a loop for assigning run-length segment IDs
    segment_labels <- rep(seq_len(n_segments), times = diff(cpt_positions))
  }

  # --- Classify each segment as present / absent / uncertain ----------------
  # Each segment is classified independently based on where its median falls
  # in the deployment-wide distribution. No assumptions are made about state
  # ordering (e.g. on→off→on) — wild bird behaviour is irregular and any
  # sequence of states is valid.
  #
  # "present" (female on nest) → lower distance reading (bird closer to sensor)
  # "absent"  (female off nest) → higher distance reading (nest cup or material)
  # "uncertain" → segment median within ±SENSITIVITY × IQR of the grand median
  #
  # IQR-based thresholds adapt to each deployment's actual signal spread.
  grand_median <- median(signal, na.rm = TRUE)
  grand_iqr    <- IQR(signal,    na.rm = TRUE)

  # Guard against near-zero IQR (very stable signal, e.g. bench test data)
  grand_iqr <- max(grand_iqr, 2)

  threshold_present <- grand_median - SENSITIVITY * grand_iqr
  threshold_absent  <- grand_median + SENSITIVITY * grand_iqr

  message("  Grand median (smoothed): ", round(grand_median, 1), " mm")
  message("  Grand IQR   (smoothed): ", round(grand_iqr,    1), " mm")
  message("  Present threshold  < ", round(threshold_present, 1), " mm  (female closer to sensor)")
  message("  Absent  threshold  > ", round(threshold_absent,  1), " mm  (nest cup / material)")

  # Compute per-segment median with map_dbl, classify with case_when
  seg_ids <- unique(segment_labels)

  seg_class <- tibble(segment_id = seg_ids) |>
    mutate(
      seg_median = map_dbl(segment_id,
                           ~median(signal[segment_labels == .x], na.rm = TRUE)),
      state_auto = case_when(
        seg_median < threshold_present ~ "present",
        seg_median > threshold_absent  ~ "absent",
        TRUE                           ~ "uncertain"
      )
    )

  # Join classifications back onto the valid-row indices, then onto tof_proc
  valid_classified <- tibble(
    row_idx    = valid_idx,
    segment_id = segment_labels
  ) |>
    left_join(seg_class, by = "segment_id")

  tof_proc <- tof_proc |>
    mutate(
      row_idx    = row_number(),
      segment_id = NA_integer_,
      state_auto = NA_character_
    ) |>
    rows_update(
      valid_classified |> select(row_idx, segment_id, state_auto),
      by = "row_idx"
    ) |>
    select(-row_idx) |>
    mutate(state_auto = factor(state_auto,
                               levels = c("present", "absent", "uncertain")))

  state_counts <- count(tof_proc, state_auto, .drop = FALSE)
  message("  State counts: ",
          paste(state_counts$state_auto, state_counts$n, sep = " = ", collapse = ", "))
}

# Corrected state column — initially identical to auto; will be modified by app
tof_proc <- tof_proc |>
  mutate(
    state_corrected = state_auto,   # factor; Shiny app updates this column
    correction_source = if_else(is.na(state_auto), NA_character_, "auto")
    # correction_source will become "manual" for user-overridden rows
  )


# =============================================================================
# 4. BOUT IDENTIFICATION
# =============================================================================

message("\nIdentifying incubation bouts ...")

# A bout is a contiguous run of readings with the same state (using state_corrected).
# Transitions to/from "uncertain" are treated as interruptions (conservative).
# Short bouts below MIN_BOUT_SEC are flagged but kept in the data.

# Step 1: Add a run-length ID to tof_proc BEFORE computing bout summaries,
# so the same IDs can be used for both the summary table and the join-back.
# NA rows (invalid/warmup) are grouped as their own runs so they don't bridge
# across real state transitions.
tof_proc <- tof_proc |>
  arrange(timestamp) |>
  mutate(
    state_chr = if_else(is.na(state_corrected), "__NA__",
                        as.character(state_corrected)),
    state_run = cumsum(state_chr != lag(state_chr, default = first(state_chr)))
  ) |>
  select(-state_chr)

# Step 2: Summarise bouts from valid (non-NA-state) runs only
bouts <- tof_proc |>
  filter(!is.na(state_corrected)) |>
  group_by(state_run, state_corrected) |>
  summarise(
    bout_start    = min(timestamp),
    bout_end      = max(timestamp),
    bout_duration = as.numeric(difftime(max(timestamp), min(timestamp),
                                        units = "secs")),
    n_readings    = n(),
    mean_tof_mm   = round(mean(tof_smooth_r, na.rm = TRUE), 1),
    .groups       = "drop"
  ) |>
  arrange(bout_start) |>
  mutate(
    bout_id    = row_number(),
    flag_short = bout_duration < MIN_BOUT_SEC
  )

# Step 3: Join bout_id back using state_run (consistent between steps 1 and 2)
bout_lookup <- bouts |> select(state_run, bout_id)
tof_proc <- tof_proc |>
  left_join(bout_lookup, by = "state_run") |>
  select(-state_run)

n_present_bouts <- sum(bouts$state_corrected == "present", na.rm = TRUE)
n_absent_bouts  <- sum(bouts$state_corrected == "absent",  na.rm = TRUE)
message("  Present bouts (on-nest)  : ", n_present_bouts)
message("  Absent bouts  (off-nest) : ", n_absent_bouts)
message("  Short bouts flagged      : ", sum(bouts$flag_short, na.rm = TRUE))


# =============================================================================
# 5. DAY SUMMARY
# =============================================================================

message("\nBuilding day summary ...")

# Total deployment span from first to last valid timestamp
first_ts <- min(tof_proc$timestamp, na.rm = TRUE)
last_ts  <- max(tof_proc$timestamp, na.rm = TRUE)
n_days   <- as.numeric(difftime(last_ts, first_ts, units = "days"))
message("  Deployment span: ", round(n_days, 1), " days")

# Downtime: gaps > 5 minutes that are not explained by the normal 2-second sampling
downtime_df <- tof_proc |>
  filter(!is.na(dt_sec), dt_sec > 300) |>
  mutate(
    gap_start    = timestamp - seconds(dt_sec),
    gap_end      = timestamp,
    downtime_min = round(dt_sec / 60, 1)
  ) |>
  select(gap_start, gap_end, downtime_min)

total_downtime_min <- sum(downtime_df$downtime_min, na.rm = TRUE)

# Pre-compute per-day off-bout stats directly from the bouts table.
# This is cleaner than trying to aggregate inside tof_proc's summarise.
off_bout_by_day <- bouts |>
  filter(state_corrected == "absent", !flag_short) |>
  mutate(date = as_date(bout_start)) |>
  group_by(date) |>
  summarise(
    n_off_bouts       = n(),
    mean_off_bout_min = round(mean(bout_duration / 60, na.rm = TRUE), 1),
    .groups = "drop"
  )

# Per-day statistics (calendar day)
day_summary <- tof_proc |>
  filter(!is.na(state_corrected)) |>
  mutate(date = as_date(timestamp)) |>
  group_by(date) |>
  summarise(
    total_valid_sec = n() * typical_interval,
    on_nest_sec     = sum(state_corrected == "present",   na.rm = TRUE) * typical_interval,
    off_nest_sec    = sum(state_corrected == "absent",    na.rm = TRUE) * typical_interval,
    uncertain_sec   = sum(state_corrected == "uncertain", na.rm = TRUE) * typical_interval,
    pct_day_on_nest = round(on_nest_sec / 86400 * 100, 1),
    .groups = "drop"
  ) |>
  left_join(off_bout_by_day, by = "date") |>
  mutate(
    n_off_bouts       = replace_na(n_off_bouts, 0L),
    mean_off_bout_min = mean_off_bout_min   # NA when no off-bouts on that day
  )

# Device health summary (deployment-level)
device_summary <- tibble(
  deployment_id           = meta$deployment_id %||% NA_character_,
  device_id               = meta$device_id     %||% NA_character_,
  first_timestamp         = first_ts,
  last_timestamp          = last_ts,
  deployment_span_days    = round(n_days, 2),
  n_sessions              = n_distinct(tof_proc$session_id, na.rm = TRUE),
  n_reboots               = nrow(log_clean |> filter(tolower(event) == "start")) - 1L,
  total_downtime_min      = total_downtime_min,
  pct_downtime            = round(total_downtime_min / (n_days * 1440) * 100, 2),
  n_tof_total             = nrow(tof_proc),
  n_tof_invalid           = sum(tof_proc$flag_tof_invalid, na.rm = TRUE),
  pct_tof_invalid         = round(mean(tof_proc$flag_tof_invalid, na.rm = TRUE) * 100, 2),
  n_tof_warmup            = sum(tof_proc$flag_ema_warmup, na.rm = TRUE),
  n_dht_bad               = sum(dht_clean$flag_dht_bad, na.rm = TRUE),
  pct_dht_bad             = round(mean(dht_clean$flag_dht_bad, na.rm = TRUE) * 100, 2)
)


# =============================================================================
# 6. DHT PROCESSING
# =============================================================================

message("\nProcessing DHT data ...")

dht_proc <- dht_clean |>
  arrange(timestamp) |>
  mutate(
    # Temperature differential (inside minus outside)
    temp_diff = round(temp_in - temp_out, 2),
    # Humidity differential
    hum_diff  = round(hum_in  - hum_out,  2)
  )

message("  DHT rows: ", nrow(dht_proc))


# =============================================================================
# 7. WRITE OUTPUTS
# =============================================================================

# Final column selection for tof_processed.csv
tof_out <- tof_proc |>
  select(
    timestamp, session_id, dt_sec,
    tof_raw_mm, tof_smooth_ema, tof_smooth_r,
    flag_tof_invalid, flag_ema_warmup,
    segment_id, state_auto, state_corrected, correction_source,
    bout_id
  )

bout_out <- bouts |>
  select(bout_id, state_corrected, bout_start, bout_end,
         bout_duration, n_readings, mean_tof_mm, flag_short)

dht_out <- dht_proc |>
  select(timestamp, session_id, dt_sec,
         temp_in, hum_in, temp_out, hum_out,
         temp_diff, hum_diff, flag_dht_bad)

write_csv(tof_out,      file.path(dir_path, "tof_processed.csv"))
write_csv(bout_out,     file.path(dir_path, "bout_summary.csv"))
write_csv(day_summary,  file.path(dir_path, "day_summary.csv"))
write_csv(device_summary, file.path(dir_path, "device_summary.csv"))
write_csv(dht_out,      file.path(dir_path, "dht_processed.csv"))

message("\n=== Processing complete ===")
message("  tof_processed.csv  -> ", file.path(dir_path, "tof_processed.csv"))
message("  bout_summary.csv   -> ", file.path(dir_path, "bout_summary.csv"))
message("  day_summary.csv    -> ", file.path(dir_path, "day_summary.csv"))
message("  device_summary.csv -> ", file.path(dir_path, "device_summary.csv"))
message("  dht_processed.csv  -> ", file.path(dir_path, "dht_processed.csv"))
message("\nNext step: launch the Shiny app (app.R)\n")
