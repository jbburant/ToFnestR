# =============================================================================
# 01_ingest.R
# Nest box microclimate & incubation logger — raw data ingestion
#
# Input:  a deployment folder containing METADATA.TXT, LOG.CSV, DHT.CSV, TOF.CSV
# Output: dht_clean.csv, tof_clean.csv, log_clean.csv  (written to the same folder)
#
# What this script does:
#   1. Parses METADATA.TXT into a named list
#   2. Resolves the recording year (from metadata or user prompt)
#   3. Reads and timestamps LOG.CSV; cross-checks device_id against metadata
#   4. Assigns session IDs to all data streams based on boot events
#   5. Reads DHT.CSV; renames sensors (1 = inside, 2 = outside); flags bad readings
#   6. Reads TOF.CSV; flags invalid values (-1, 8190); labels on-device EMA smooth;
#      marks EMA warm-up rows after each reboot
#   7. Checks for unexpectedly large temporal gaps (possible multi-deployment files)
#   8. Writes three clean CSVs
# =============================================================================

library(readr)
library(dplyr)
library(lubridate)
library(stringr)

# Coalesce operator: return `a` if non-NULL and non-empty, else `b`
`%||%` <- function(a, b) if (!is.null(a) && nchar(a) > 0) a else b


# =============================================================================
# USER SETTINGS
# =============================================================================

# Path to the deployment folder (trailing slash optional)
DEPLOYMENT_DIR <- "."

# Large-gap threshold: gaps longer than this (in minutes) trigger a warning.
# Gaps from normal reboots are a few minutes; anything much larger is suspicious.
GAP_THRESHOLD_MIN <- 120   # 2 hours

# Number of TOF readings to mark as EMA warm-up after each reboot.
# With alpha = 0.3, the EMA weight of the initial (zero) value falls below 1%
# after ~15 readings. We use 20 to be conservative.
EMA_WARMUP_N <- 20


# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

#' Parse a METADATA.TXT key=value file into a named list.
#' Blank lines and lines without '=' are silently skipped.
parse_metadata <- function(path) {
  lines <- readLines(path, warn = FALSE)
  lines <- lines[nchar(trimws(lines)) > 0]          # drop blanks
  lines <- lines[grepl("=", lines, fixed = TRUE)]   # keep key=value lines
  pairs <- str_split_fixed(lines, "=", n = 2)
  vals  <- setNames(trimws(pairs[, 2]), trimws(pairs[, 1]))
  as.list(vals)
}


#' Add year to a "MM-DD HH:MM:SS" timestamp vector and return POSIXct.
#'
#' Handles year-boundary deployments: if the month ever decreases (i.e. Dec → Jan),
#' the year is incremented for all subsequent rows.
#'
#' @param ts   Character vector of "MM-DD HH:MM:SS" strings
#' @param year Integer year to use as the starting year
add_year <- function(ts, year) {
  full <- paste0(year, "-", ts)
  dt   <- ymd_hms(full, quiet = TRUE)

  # Detect year-boundary crossings (month goes from 12 back to 1)
  months <- month(dt)
  crossings <- which(diff(months) < -6)  # threshold of 6 avoids false positives
  if (length(crossings) > 0) {
    message("  Year boundary detected at row(s): ",
            paste(crossings + 1, collapse = ", "),
            " — incrementing year for subsequent rows.")
    for (cx in crossings) {
      dt[(cx + 1):length(dt)] <- dt[(cx + 1):length(dt)] + years(1)
    }
  }
  dt
}


#' Assign a session ID to each row based on boot-event timestamps.
#'
#' Session 1 starts at the first boot. If a row precedes the first boot
#' (which should not happen in well-formed data but is handled gracefully),
#' it is assigned session 0.
#'
#' @param timestamps POSIXct vector of data timestamps
#' @param boot_times POSIXct vector of boot event timestamps (sorted)
assign_session <- function(timestamps, boot_times) {
  boot_times <- sort(boot_times)
  session <- findInterval(as.numeric(timestamps), as.numeric(boot_times))
  session  # 0 = before first boot; 1 = after boot 1; etc.
}


#' Check for large temporal gaps within a POSIXct vector.
#' Returns a data frame of gap locations (or an empty data frame if none found).
check_gaps <- function(timestamps, threshold_min = GAP_THRESHOLD_MIN) {
  gaps_sec <- as.numeric(diff(timestamps), units = "secs")
  bad      <- which(gaps_sec > threshold_min * 60)
  if (length(bad) == 0) return(invisible(NULL))
  data.frame(
    after_row   = bad,
    before_time = timestamps[bad],
    after_time  = timestamps[bad + 1],
    gap_minutes = round(gaps_sec[bad] / 60, 1)
  )
}


# =============================================================================
# 1. RESOLVE PATHS AND LOAD METADATA
# =============================================================================

message("\n=== 01_ingest.R ===\n")

dir_path <- normalizePath(DEPLOYMENT_DIR, mustWork = TRUE)
meta_path <- file.path(dir_path, "METADATA.TXT")
log_path  <- file.path(dir_path, "LOG.CSV")
dht_path  <- file.path(dir_path, "DHT.CSV")
tof_path  <- file.path(dir_path, "TOF.CSV")

for (p in c(meta_path, log_path, dht_path, tof_path)) {
  if (!file.exists(p))
    stop("Required file not found: ", p)
}

message("Loading metadata from: ", meta_path)
meta <- parse_metadata(meta_path)
message("  device_id     : ", meta$device_id %||% "(not set)")
message("  deployment_id : ", meta$deployment_id %||% "(not set)")
message("  deployment_date: ", meta$deployment_date %||% "(not set)")


# =============================================================================
# 2. RESOLVE RECORDING YEAR
# =============================================================================

# Try to extract year from deployment_date (accepts YYYY-MM-DD or DD/MM/YYYY etc.)
infer_year_from_date <- function(date_str) {
  if (is.null(date_str) || nchar(trimws(date_str)) == 0) return(NULL)
  # Try common formats; lubridate::parse_date_time is flexible
  dt <- suppressWarnings(parse_date_time(date_str,
        orders = c("Ymd", "dmY", "mdY", "dmy", "mdy"), quiet = TRUE))
  if (is.na(dt)) return(NULL)
  year(dt)
}

recording_year <- infer_year_from_date(meta$deployment_date %||% "")

if (is.null(recording_year)) {
  message("\n  deployment_date is blank or unparseable in METADATA.TXT.")
  message("  Please enter the year the data were recorded (e.g. 2026): ")
  user_input <- trimws(readline())
  recording_year <- suppressWarnings(as.integer(user_input))
  if (is.na(recording_year) || recording_year < 2020 || recording_year > 2100)
    stop("Invalid year entered: '", user_input, "'. Please re-run and enter a 4-digit year.")
}

message("  Recording year : ", recording_year)


# =============================================================================
# 3. LOAD AND CLEAN LOG.CSV
# =============================================================================

message("\nLoading LOG.CSV ...")
log_raw <- read_csv(log_path, col_types = cols(.default = "c"), show_col_types = FALSE)
log_raw <- log_raw |> mutate(across(everything(), trimws))

# Parse timestamps
log_raw <- log_raw |>
  mutate(timestamp = add_year(timestamp, recording_year))

# Cross-check device_id
log_device_ids <- unique(log_raw$device_id)
meta_device_id <- meta$device_id %||% NA_character_

if (!is.null(meta$device_id) && nchar(meta$device_id) > 0) {
  if (!all(log_device_ids %in% meta_device_id)) {
    warning(
      "device_id mismatch!\n",
      "  METADATA.TXT : ", meta_device_id, "\n",
      "  LOG.CSV      : ", paste(log_device_ids, collapse = ", "), "\n",
      "  Check that files from different devices have not been mixed."
    )
  } else {
    message("  device_id cross-check passed: ", meta_device_id)
  }
}

log_clean <- log_raw |>
  arrange(timestamp) |>
  mutate(
    file        = "LOG.CSV",
    flag_notes  = NA_character_
  )

# Extract boot times for session assignment
boot_times <- log_clean |>
  filter(tolower(event) == "start") |>
  pull(timestamp)

message("  Boot events found: ", length(boot_times))
message("  ", paste(format(boot_times, "%m-%d %H:%M:%S"), collapse = " | "))


# =============================================================================
# 4. LOAD AND CLEAN DHT.CSV
# =============================================================================

message("\nLoading DHT.CSV ...")
dht_raw <- read_csv(dht_path, col_types = cols(.default = "c"), show_col_types = FALSE)
dht_raw <- dht_raw |> mutate(across(everything(), trimws))

dht_clean <- dht_raw |>
  mutate(
    timestamp = add_year(timestamp, recording_year),

    # Coerce sensor columns to numeric (handles "NA", "NaN", empty strings)
    temp_in  = suppressWarnings(as.numeric(temp1)),
    hum_in   = suppressWarnings(as.numeric(hum1)),
    temp_out = suppressWarnings(as.numeric(temp2)),
    hum_out  = suppressWarnings(as.numeric(hum2))
  ) |>
  select(-temp1, -hum1, -temp2, -hum2) |>   # drop original column names
  arrange(timestamp) |>
  mutate(
    # Flag rows where any sensor returned NA/NaN
    flag_dht_bad = is.na(temp_in) | is.na(hum_in) | is.na(temp_out) | is.na(hum_out),

    # Assign session ID
    session_id = assign_session(timestamp, boot_times),

    # Time since previous reading (seconds); NA for first row
    dt_sec = c(NA_real_, as.numeric(diff(timestamp), units = "secs"))
  )

n_bad_dht <- sum(dht_clean$flag_dht_bad, na.rm = TRUE)
message("  Rows loaded     : ", nrow(dht_clean))
message("  Bad DHT readings: ", n_bad_dht)

# Check for large gaps
dht_gaps <- check_gaps(dht_clean$timestamp)
if (!is.null(dht_gaps)) {
  message("\n  WARNING: Large gap(s) detected in DHT.CSV:")
  print(dht_gaps)
  message("  If this deployment spans multiple field seasons,")
  message("  ensure you are not inadvertently mixing data from different deployments.")
}


# =============================================================================
# 5. LOAD AND CLEAN TOF.CSV
# =============================================================================

message("\nLoading TOF.CSV ...")
tof_raw <- read_csv(tof_path, col_types = cols(.default = "c"), show_col_types = FALSE)
tof_raw <- tof_raw |> mutate(across(everything(), trimws))

tof_clean <- tof_raw |>
  mutate(
    timestamp = add_year(timestamp, recording_year),

    # Coerce to numeric
    tof_raw_mm      = suppressWarnings(as.numeric(tof_raw)),
    tof_smooth_ema  = suppressWarnings(as.numeric(tof_smooth))  # on-device EMA (alpha=0.3)
  ) |>
  select(-tof_raw, -tof_smooth) |>
  arrange(timestamp) |>
  mutate(
    # Flag invalid sentinel values: -1 (device timeout/error) and 8190 (out of range)
    flag_tof_invalid = tof_raw_mm %in% c(-1L, 8190L) | is.na(tof_raw_mm),

    # Set invalid raw values to NA (keep them flagged for diagnostics)
    tof_raw_mm     = if_else(flag_tof_invalid, NA_real_, tof_raw_mm),
    tof_smooth_ema = if_else(flag_tof_invalid, NA_real_, tof_smooth_ema),

    # Assign session ID
    session_id = assign_session(timestamp, boot_times),

    # Time since previous reading (seconds); NA for first row
    dt_sec = c(NA_real_, as.numeric(diff(timestamp), units = "secs"))
  )

# Flag EMA warm-up rows: the first EMA_WARMUP_N valid readings after each boot.
# The device initialises its EMA at 0, so readings ramp up toward the true value.
tof_clean <- tof_clean |>
  group_by(session_id) |>
  mutate(
    row_in_session  = row_number(),
    flag_ema_warmup = row_in_session <= EMA_WARMUP_N
  ) |>
  ungroup() |>
  select(-row_in_session)

n_invalid_tof <- sum(tof_clean$flag_tof_invalid, na.rm = TRUE)
n_warmup_tof  <- sum(tof_clean$flag_ema_warmup,  na.rm = TRUE)
message("  Rows loaded          : ", nrow(tof_clean))
message("  Invalid TOF readings : ", n_invalid_tof)
message("  EMA warm-up rows     : ", n_warmup_tof,
        " (", EMA_WARMUP_N, " rows x ", length(boot_times), " session(s))")

# Check for large gaps
tof_gaps <- check_gaps(tof_clean$timestamp)
if (!is.null(tof_gaps)) {
  message("\n  WARNING: Large gap(s) detected in TOF.CSV:")
  print(tof_gaps)
}


# =============================================================================
# 6. FINAL COLUMN ORDER AND OUTPUT
# =============================================================================

log_out <- log_clean |>
  select(timestamp, event, device_id, notes, flag_notes)

dht_out <- dht_clean |>
  select(timestamp, session_id, dt_sec,
         temp_in, hum_in, temp_out, hum_out,
         flag_dht_bad)

tof_out <- tof_clean |>
  select(timestamp, session_id, dt_sec,
         tof_raw_mm, tof_smooth_ema,
         flag_tof_invalid, flag_ema_warmup)

out_log <- file.path(dir_path, "log_clean.csv")
out_dht <- file.path(dir_path, "dht_clean.csv")
out_tof <- file.path(dir_path, "tof_clean.csv")

write_csv(log_out, out_log)
write_csv(dht_out, out_dht)
write_csv(tof_out, out_tof)

message("\n=== Ingestion complete ===")
message("  log_clean.csv  -> ", out_log)
message("  dht_clean.csv  -> ", out_dht)
message("  tof_clean.csv  -> ", out_tof)
message("\nNext step: run 02_process.R\n")
