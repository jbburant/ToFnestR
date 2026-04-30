# =============================================================================
# R/pipeline_functions.R
# Shared pipeline logic for the Nest Box Incubation Monitor.
# Sourced by 01_ingest.R, 02_process.R, and app.R.
#
# All functions accept explicit arguments rather than relying on global
# USER SETTINGS constants, making them safe to call from within Shiny.
# =============================================================================

# Required packages — loaded by the calling script/app, not here.
# readr, dplyr, tidyr, purrr, lubridate, zoo, changepoint, stringr

# NULL/empty-string coalescing operator.
# Returns `b` when `a` is NULL or a zero-length string; otherwise returns `a`.
# Defined here so the file is self-contained when sourced by app.R or scripts.
`%||%` <- function(a, b) if (!is.null(a) && nchar(a) > 0) a else b


# =============================================================================
# UTILITIES
# =============================================================================

#' Parse a METADATA.TXT key=value file into a named list.
parse_metadata <- function(path) {
  lines <- readLines(path, warn = FALSE)
  lines <- lines[nchar(trimws(lines)) > 0 & grepl("=", lines, fixed = TRUE)]
  pairs <- stringr::str_split_fixed(lines, "=", n = 2)
  setNames(as.list(trimws(pairs[, 2])), trimws(pairs[, 1]))
}

#' Try to extract a 4-digit year from a date string (or return NULL if not possible).
infer_year_from_date <- function(date_str) {
  if (is.null(date_str) || nchar(trimws(date_str)) == 0) return(NULL)
  dt <- suppressWarnings(
    lubridate::parse_date_time(date_str,
      orders = c("Ymd", "dmY", "mdY", "dmy", "mdy"), quiet = TRUE)
  )
  if (is.na(dt)) return(NULL)
  lubridate::year(dt)
}

#' Add year to "MM-DD HH:MM:SS" timestamps and return POSIXct.
#' Handles year-boundary crossings (Dec → Jan) with a plain for-loop
#' rather than walk() + <<-, which is clearer about the mutation.
add_year <- function(ts, year) {
  dt        <- lubridate::ymd_hms(paste0(year, "-", ts), quiet = TRUE)
  crossings <- which(diff(lubridate::month(dt)) < -6)
  for (cx in crossings) {
    dt[(cx + 1):length(dt)] <- dt[(cx + 1):length(dt)] + lubridate::years(1)
  }
  dt
}

#' Parse deployment_datetime and retrieval_datetime from a metadata list.
#'
#' Combines `deployment_date` + `deployment_time` (and their retrieval
#' equivalents) into POSIXct. Time defaults to 00:00:00 if omitted.
#' Returns a list with two elements, either POSIXct or NULL.
parse_deployment_datetimes <- function(meta, recording_year = NULL) {
  combine_dt <- function(date_key, time_key) {
    d <- meta[[date_key]] %||% ""
    t <- meta[[time_key]] %||% "00:00:00"
    if (nchar(trimws(d)) == 0) return(NULL)
    # If date lacks a year, prepend recording_year
    if (!grepl("^[0-9]{4}", trimws(d)) && !is.null(recording_year))
      d <- paste0(recording_year, "-", trimws(d))
    parsed <- suppressWarnings(
      lubridate::ymd_hms(paste(trimws(d), trimws(t)), quiet = TRUE)
    )
    if (is.na(parsed)) NULL else parsed
  }
  list(
    deployment_datetime = combine_dt("deployment_date", "deployment_time"),
    retrieval_datetime  = combine_dt("retrieval_date",  "retrieval_time")
  )
}

#' Assign session IDs based on boot-event timestamps.
assign_session <- function(timestamps, boot_times) {
  findInterval(as.numeric(timestamps), as.numeric(sort(boot_times)))
}

#' Return the smallest valid odd integer >= round(sec / interval_sec), min 3.
valid_window <- function(sec, interval_sec = 2) {
  k <- round(sec / interval_sec)
  if (k %% 2 == 0) k <- k + 1
  max(k, 3L)
}


# =============================================================================
# INGESTION
# =============================================================================

#' Ingest raw device files from a deployment folder.
#'
#' Reads METADATA.TXT, LOG.CSV, DHT.CSV, TOF.CSV; attaches year to timestamps;
#' renames and flags columns; writes *_clean.csv files to the folder.
#'
#' @param folder Path to the deployment folder.
#' @param year   Integer year. If NULL, inferred from metadata; if still
#'               unknown, throws error "recording_year_needed" which the
#'               Shiny app catches and handles via a modal dialog.
#' @param gap_threshold_min  Gap longer than this triggers a warning (default 120 min).
#' @param ema_warmup_n  TOF rows to flag as EMA warm-up after each reboot (default 20).
#'
#' @param out_folder Folder to write clean CSVs into. Defaults to `folder`
#'   (same location as raw files). Set to a separate directory to keep raw and
#'   processed data apart. METADATA.TXT is copied to out_folder so the
#'   processed directory is self-contained for loading.
#' @return Invisibly, the out_folder path.  Side-effect: writes clean CSVs.
ingest_deployment <- function(folder,
                               year              = NULL,
                               gap_threshold_min = 120,
                               ema_warmup_n      = 20L,
                               out_folder        = NULL) {
  folder <- normalizePath(folder, mustWork = TRUE)
  if (is.null(out_folder)) out_folder <- folder
  if (!dir.exists(out_folder)) dir.create(out_folder, recursive = TRUE)

  # --- Metadata & year -------------------------------------------------------
  meta <- parse_metadata(file.path(folder, "METADATA.TXT"))
  recording_year <- year %||% infer_year_from_date(meta$deployment_date %||% "")
  if (is.null(recording_year)) {
    stop("recording_year_needed")
  }

  # --- Log -------------------------------------------------------------------
  log_clean <- readr::read_csv(
    file.path(folder, "LOG.CSV"),
    col_types = readr::cols(.default = "c"), show_col_types = FALSE
  ) |>
    dplyr::mutate(
      across(everything(), trimws),
      timestamp = add_year(timestamp, recording_year)
    ) |>
    dplyr::arrange(timestamp)

  # device_id cross-check
  meta_id <- meta$device_id %||% ""
  log_ids <- unique(log_clean$device_id)
  if (nchar(meta_id) > 0 && !all(log_ids %in% meta_id)) {
    warning("device_id mismatch — METADATA: ", meta_id,
            "  LOG: ", paste(log_ids, collapse = ", "),
            "\n  Check that files from different devices have not been mixed.")
  }

  boot_times <- log_clean |>
    dplyr::filter(tolower(event) == "start") |>
    dplyr::pull(timestamp)

  # --- DHT -------------------------------------------------------------------
  dht_clean <- readr::read_csv(
    file.path(folder, "DHT.CSV"),
    col_types = readr::cols(.default = "c"), show_col_types = FALSE
  ) |>
    dplyr::mutate(
      across(everything(), trimws),
      timestamp = add_year(timestamp, recording_year),
      temp_in   = suppressWarnings(as.numeric(temp1)),
      hum_in    = suppressWarnings(as.numeric(hum1)),
      temp_out  = suppressWarnings(as.numeric(temp2)),
      hum_out   = suppressWarnings(as.numeric(hum2))
    ) |>
    dplyr::select(-temp1, -hum1, -temp2, -hum2) |>
    dplyr::arrange(timestamp) |>
    dplyr::mutate(
      session_id   = assign_session(timestamp, boot_times),
      dt_sec       = c(NA_real_, as.numeric(diff(timestamp), units = "secs")),
      flag_dht_bad = is.na(temp_in) | is.na(hum_in) | is.na(temp_out) | is.na(hum_out),
      temp_diff    = round(temp_in - temp_out, 2),
      hum_diff     = round(hum_in  - hum_out,  2)
    )

  # Large-gap warning
  dht_gaps <- dht_clean |>
    dplyr::filter(!is.na(dt_sec), dt_sec > gap_threshold_min * 60) |>
    dplyr::mutate(gap_start = timestamp - lubridate::seconds(dt_sec),
                  gap_min   = round(dt_sec / 60, 1)) |>
    dplyr::select(gap_start, gap_end = timestamp, gap_min)
  if (nrow(dht_gaps) > 0) {
    warning("Large gap(s) in DHT.CSV — possible multi-deployment file:\n",
            paste(format(dht_gaps$gap_start), "→", format(dht_gaps$gap_end),
                  paste0("(", dht_gaps$gap_min, " min)"), collapse = "\n"))
  }

  # --- TOF -------------------------------------------------------------------
  tof_clean <- readr::read_csv(
    file.path(folder, "TOF.CSV"),
    col_types = readr::cols(.default = "c"), show_col_types = FALSE
  ) |>
    dplyr::mutate(
      across(everything(), trimws),
      timestamp      = add_year(timestamp, recording_year),
      tof_raw_mm     = suppressWarnings(as.numeric(tof_raw)),
      tof_smooth_ema = suppressWarnings(as.numeric(tof_smooth))
    ) |>
    dplyr::select(-tof_raw, -tof_smooth) |>
    dplyr::arrange(timestamp) |>
    dplyr::mutate(
      flag_tof_invalid = tof_raw_mm %in% c(-1, 8190) | is.na(tof_raw_mm),
      tof_raw_mm       = dplyr::if_else(flag_tof_invalid, NA_real_, tof_raw_mm),
      tof_smooth_ema   = dplyr::if_else(flag_tof_invalid, NA_real_, tof_smooth_ema),
      session_id       = assign_session(timestamp, boot_times),
      dt_sec           = c(NA_real_, as.numeric(diff(timestamp), units = "secs"))
    ) |>
    dplyr::group_by(session_id) |>
    dplyr::mutate(flag_ema_warmup = dplyr::row_number() <= ema_warmup_n) |>
    dplyr::ungroup()

  # --- Write -----------------------------------------------------------------
  readr::write_csv(log_clean, file.path(out_folder, "log_clean.csv"))
  readr::write_csv(dht_clean, file.path(out_folder, "dht_clean.csv"))
  readr::write_csv(tof_clean, file.path(out_folder, "tof_clean.csv"))

  # Copy METADATA.TXT so out_folder is self-contained (needed by load_deployment)
  if (!identical(folder, out_folder))
    file.copy(file.path(folder, "METADATA.TXT"),
              file.path(out_folder, "METADATA.TXT"), overwrite = TRUE)

  invisible(out_folder)
}


# =============================================================================
# BOUT IDENTIFICATION  (separated out so Shiny can call it after corrections)
# =============================================================================

#' Derive incubation bouts from the state_corrected column of a TOF data frame.
#'
#' Adds a `state_run` column (run-length group IDs) to `tof`, then summarises
#' each run into a bouts table.
#'
#' @return A list with two elements:
#'   $tof   — the input data frame with `state_run` column added
#'   $bouts — one-row-per-bout tibble with `state_run` for joining back
recompute_bouts <- function(tof, min_bout_sec = 60) {
  # Forward-fill the last valid state across invalid / warmup / NA rows so
  # that isolated sensor errors don't split an otherwise-continuous bout.
  # Rows before the first classified reading (e.g. EMA warmup at start) get
  # a distinct sentinel so they stay as their own un-classifiable group.
  tof_rl <- tof |>
    dplyr::mutate(
      state_filled = zoo::na.locf(
        dplyr::if_else(!is.na(state_corrected),
                       as.character(state_corrected), NA_character_),
        na.rm = FALSE
      ),
      state_filled = dplyr::if_else(is.na(state_filled),
                                     "__UNCLASSIFIED__", state_filled),
      state_run    = cumsum(
        state_filled != dplyr::lag(state_filled,
                                    default = dplyr::first(state_filled))
      )
    ) |>
    dplyr::select(-state_filled)

  bouts <- tof_rl |>
    dplyr::filter(!is.na(state_corrected)) |>
    dplyr::group_by(state_run, state_corrected) |>
    dplyr::summarise(
      bout_start    = min(timestamp),
      bout_end      = max(timestamp),
      bout_duration = as.numeric(difftime(max(timestamp), min(timestamp),
                                          units = "secs")),
      n_readings    = dplyr::n(),
      mean_tof_mm   = round(mean(tof_smooth_r, na.rm = TRUE), 1),
      .groups       = "drop"
    ) |>
    dplyr::arrange(bout_start) |>
    dplyr::mutate(
      bout_id    = dplyr::row_number(),
      flag_short = bout_duration < min_bout_sec
    )

  list(tof = tof_rl, bouts = bouts)
}


# =============================================================================
# SUMMARIES  (reactive in Shiny — recomputed after every correction)
# =============================================================================

#' Recompute day_summary and device_summary from corrected TOF + bouts.
#'
#' @param tof              Processed TOF data frame (with state_corrected).
#' @param bouts            Bouts table from recompute_bouts().
#' @param meta             Named metadata list.
#' @param log_clean        Clean log data frame (for reboot count).
#' @param typical_interval Median TOF sampling interval in seconds.
#' @param lat              Decimal-degree latitude  (enables active-day stats via suncalc).
#' @param lon              Decimal-degree longitude.
recompute_summaries <- function(tof, bouts, meta, log_clean,
                                 typical_interval = 2,
                                 lat = NULL, lon = NULL) {
  # Off-bouts by day (for joining)
  off_by_day <- bouts |>
    dplyr::filter(state_corrected == "absent", !flag_short) |>
    dplyr::mutate(date = lubridate::as_date(bout_start)) |>
    dplyr::group_by(date) |>
    dplyr::summarise(
      n_off_bouts       = dplyr::n(),
      mean_off_bout_min = round(mean(bout_duration / 60, na.rm = TRUE), 1),
      .groups = "drop"
    )

  day_summary <- tof |>
    dplyr::filter(!is.na(state_corrected)) |>
    dplyr::mutate(date = lubridate::as_date(timestamp)) |>
    dplyr::group_by(date) |>
    dplyr::summarise(
      total_valid_sec = dplyr::n() * typical_interval,
      on_nest_sec     = sum(state_corrected == "present",   na.rm = TRUE) * typical_interval,
      off_nest_sec    = sum(state_corrected == "absent",    na.rm = TRUE) * typical_interval,
      uncertain_sec   = sum(state_corrected == "uncertain", na.rm = TRUE) * typical_interval,
      pct_day_on_nest = round(on_nest_sec / 86400 * 100, 1),
      .groups         = "drop"
    ) |>
    dplyr::left_join(off_by_day, by = "date") |>
    dplyr::mutate(n_off_bouts = tidyr::replace_na(n_off_bouts, 0L))

  # Deployment-span statistics
  first_ts <- min(tof$timestamp, na.rm = TRUE)
  last_ts  <- max(tof$timestamp, na.rm = TRUE)
  n_days   <- as.numeric(difftime(last_ts, first_ts, units = "days"))

  downtime_min <- tof |>
    dplyr::filter(!is.na(dt_sec), dt_sec > 300) |>
    dplyr::summarise(total = sum(dt_sec / 60, na.rm = TRUE)) |>
    dplyr::pull(total)

  n_reboots <- if (!is.null(log_clean)) {
    sum(tolower(log_clean$event) == "start", na.rm = TRUE) - 1L
  } else NA_integer_

  device_summary <- tibble::tibble(
    deployment_id        = meta$deployment_id   %||% NA_character_,
    device_id            = meta$device_id       %||% NA_character_,
    nestbox              = meta$nestbox          %||% NA_character_,
    species              = meta$species          %||% NA_character_,
    first_timestamp      = first_ts,
    last_timestamp       = last_ts,
    span_days            = round(n_days, 2),
    n_sessions           = dplyr::n_distinct(tof$session_id, na.rm = TRUE),
    n_reboots            = n_reboots,
    total_downtime_min   = round(downtime_min, 1),
    pct_downtime         = round(downtime_min / (n_days * 1440) * 100, 2),
    n_tof_total          = nrow(tof),
    n_tof_invalid        = sum(tof$flag_tof_invalid, na.rm = TRUE),
    pct_tof_invalid      = round(mean(tof$flag_tof_invalid, na.rm = TRUE) * 100, 2),
    n_tof_warmup         = sum(tof$flag_ema_warmup,  na.rm = TRUE),
    n_dht_bad            = NA_integer_,   # filled by caller if dht available
    pct_dht_bad          = NA_real_
  )

  # --- Active-day statistics (sunrise → sunset only) --------------------------
  # Requires coordinates and the suncalc package. Falls back gracefully when
  # either is absent — only the 24-h columns appear in that case.
  sun_times <- NULL
  if (!is.null(lat) && !is.null(lon) &&
      requireNamespace("suncalc", quietly = TRUE)) {
    sun_times <- suncalc::getSunlightTimes(
      date = day_summary$date, lat = lat, lon = lon,
      keep = c("sunrise", "sunset"), tz = "UTC"
    )

    # On-nest time restricted to the active day window
    active_on_nest <- tof |>
      dplyr::filter(!is.na(state_corrected), !is.na(timestamp)) |>
      dplyr::mutate(date = lubridate::as_date(timestamp)) |>
      dplyr::left_join(
        sun_times |> dplyr::select(date, sunrise, sunset),
        by = "date"
      ) |>
      dplyr::filter(timestamp >= sunrise, timestamp <= sunset) |>
      dplyr::group_by(date) |>
      dplyr::summarise(
        on_nest_active_sec = sum(state_corrected == "present", na.rm = TRUE) *
                              typical_interval,
        .groups = "drop"
      )

    day_summary <- day_summary |>
      dplyr::left_join(
        sun_times |>
          dplyr::mutate(
            active_day_hr = round(
              as.numeric(difftime(sunset, sunrise, units = "hours")), 2)
          ) |>
          dplyr::select(date, sunrise, sunset, active_day_hr),
        by = "date"
      ) |>
      dplyr::left_join(active_on_nest, by = "date") |>
      dplyr::mutate(
        pct_active_day_on_nest = dplyr::if_else(
          !is.na(on_nest_active_sec) & active_day_hr > 0,
          round(on_nest_active_sec / (active_day_hr * 3600) * 100, 1),
          NA_real_
        )
      )
  }

  list(day_summary = day_summary, device_summary = device_summary,
       sun_times = sun_times)
}


# =============================================================================
# PROCESSING  (smoothing + state detection)
# =============================================================================

#' Process a deployment folder: smooth TOF, detect states, compute summaries.
#'
#' Reads the *_clean.csv files written by ingest_deployment() and writes
#' *_processed.csv files.
#'
#' @return A named list suitable for storing in the Shiny rv$deployments.
process_deployment <- function(folder,
                                smooth_sec             = 75,
                                cpt_penalty            = "MBIC",
                                sensitivity            = 0.3,
                                min_bout_sec           = 60,
                                lat                    = NULL,
                                lon                    = NULL,
                                classification_method  = "gmm") {
  folder <- normalizePath(folder, mustWork = TRUE)
  meta      <- parse_metadata(file.path(folder, "METADATA.TXT"))
  tof_clean <- readr::read_csv(file.path(folder, "tof_clean.csv"),
                                show_col_types = FALSE) |>
    dplyr::mutate(timestamp = lubridate::ymd_hms(timestamp))
  dht_clean <- readr::read_csv(file.path(folder, "dht_clean.csv"),
                                show_col_types = FALSE) |>
    dplyr::mutate(timestamp = lubridate::ymd_hms(timestamp))
  log_clean <- readr::read_csv(file.path(folder, "log_clean.csv"),
                                show_col_types = FALSE) |>
    dplyr::mutate(timestamp = lubridate::ymd_hms(timestamp))

  # --- Typical interval -------------------------------------------------------
  typical_interval <- tof_clean |>
    dplyr::filter(!flag_tof_invalid, !flag_ema_warmup, !is.na(dt_sec)) |>
    dplyr::pull(dt_sec) |>
    median(na.rm = TRUE)

  k_smooth <- valid_window(smooth_sec, typical_interval)

  # --- Smoothing (per session) ------------------------------------------------
  tof_proc <- tof_clean |>
    dplyr::group_by(session_id) |>
    dplyr::mutate(
      tof_smooth_r = zoo::rollmedian(
        dplyr::if_else(flag_tof_invalid | flag_ema_warmup, NA_real_, tof_raw_mm),
        k = k_smooth, fill = NA, align = "center"
      )
    ) |>
    dplyr::ungroup()

  # --- Day/night marking (requires suncalc + coordinates) --------------------
  # When coordinates are available, nights are auto-classified as "present"
  # and excluded from PELT. This prevents the stable overnight signal from
  # compressing the IQR used to calibrate the daytime present/absent threshold.
  sun_times <- NULL
  has_coords <- !is.null(lat) && !is.null(lon) &&
                requireNamespace("suncalc", quietly = TRUE)

  if (has_coords) {
    dates     <- unique(lubridate::as_date(tof_proc$timestamp[!is.na(tof_proc$timestamp)]))
    sun_times <- suncalc::getSunlightTimes(
      date = dates, lat = lat, lon = lon,
      keep = c("sunrise", "sunset"), tz = "UTC"
    )
    tof_proc <- tof_proc |>
      dplyr::mutate(date_col = lubridate::as_date(timestamp)) |>
      dplyr::left_join(
        sun_times |> dplyr::select(date, sunrise, sunset),
        by = c("date_col" = "date")
      ) |>
      dplyr::mutate(
        is_night = !is.na(timestamp) &
                   (timestamp < sunrise | timestamp > sunset)
      ) |>
      dplyr::select(-date_col, -sunrise, -sunset)
  } else {
    tof_proc <- dplyr::mutate(tof_proc, is_night = FALSE)
  }

  # --- State detection (PELT) -------------------------------------------------
  # Restrict to daytime valid readings when coordinates are available so the
  # grand median / IQR reflect only the active incubation period.
  valid_idx <- which(!tof_proc$flag_tof_invalid &
                     !tof_proc$flag_ema_warmup  &
                     !is.na(tof_proc$tof_smooth_r) &
                     !tof_proc$is_night)
  signal <- tof_proc$tof_smooth_r[valid_idx]

  segment_labels <- if (length(valid_idx) < 10) {
    rep(1L, length(valid_idx))
  } else {
    cpt_res <- tryCatch(
      changepoint::cpt.mean(signal, method = "PELT", penalty = cpt_penalty),
      error = function(e) NULL
    )
    if (is.null(cpt_res)) {
      rep(1L, length(signal))
    } else {
      cpt_pos <- c(0L, changepoint::cpts(cpt_res), length(signal))
      rep(seq_len(length(cpt_pos) - 1L), times = diff(cpt_pos))
    }
  }

  # ── Segment classification ───────────────────────────────────────────────────
  # Two methods are supported; both find two clusters in the daytime segment
  # medians. The `sensitivity` slider controls the size of the "uncertain"
  # buffer using the same 0–1 scale for both:
  #   0 = hard split, nothing uncertain
  #   1 = entire gap between clusters is uncertain
  #
  # K-MEANS  (classification_method = "kmeans")
  #   Hard assignment, equal-variance assumption. Uses the midpoint between
  #   cluster centres as the split, with ± sensitivity × half-gap as the
  #   uncertain zone.
  #
  # GMM  (classification_method = "gmm")
  #   Fits a 2-component Gaussian mixture (via mclust) which allows each
  #   cluster its own variance. Gives a posterior probability per segment;
  #   segments with max(posterior) < (0.5 + sensitivity × 0.5) are uncertain.
  #   Falls back to k-means if mclust is unavailable or fitting fails.
  #
  # Per-deployment fine-tuning hook:
  #   When manual corrections exist, their segment medians can be used to
  #   initialise cluster centres / GMM component means before fitting,
  #   giving the algorithm a labelled head-start on the deployment.

  seg_class <- tibble::tibble(segment_id = unique(segment_labels)) |>
    dplyr::mutate(
      seg_median = purrr::map_dbl(segment_id,
                                   ~median(signal[segment_labels == .x], na.rm = TRUE))
    )

  viable <- length(unique(round(seg_class$seg_median))) >= 2 &&
            diff(range(seg_class$seg_median))             >= 5

  classify_result <- if (viable && classification_method == "gmm" &&
                          requireNamespace("mclust", quietly = TRUE)) {

    # ── GMM path ────────────────────────────────────────────────────────────
    fit <- tryCatch(
      mclust::Mclust(seg_class$seg_median, G = 2L, verbose = FALSE),
      error = function(e) NULL
    )

    if (!is.null(fit)) {
      lower_comp <- which.min(fit$parameters$mean)   # "present" component
      upper_comp <- which.max(fit$parameters$mean)   # "absent"  component

      # sensitivity 0→1 maps to min_posterior 0.5→1.0
      min_posterior <- 0.5 + sensitivity * 0.5

      dplyr::mutate(seg_class,
        state_auto = dplyr::case_when(
          pmax(fit$z[, lower_comp], fit$z[, upper_comp]) < min_posterior ~ "uncertain",
          fit$z[, lower_comp] > fit$z[, upper_comp]                      ~ "present",
          TRUE                                                            ~ "absent"
        )
      )
    } else NULL   # fall through to k-means

  } else NULL

  if (is.null(classify_result)) {
    # ── K-means path (also used as GMM fallback) ─────────────────────────
    km <- if (viable) {
      tryCatch(
        stats::kmeans(seg_class$seg_median, centers = 2L, nstart = 20L),
        error = function(e) NULL
      )
    } else NULL

    classify_result <- if (!is.null(km)) {
      lower_c  <- min(km$centers)
      upper_c  <- max(km$centers)
      midpoint <- (lower_c + upper_c) / 2
      margin   <- sensitivity * (upper_c - lower_c) / 2

      dplyr::mutate(seg_class,
        state_auto = dplyr::case_when(
          seg_median < midpoint - margin ~ "present",
          seg_median > midpoint + margin ~ "absent",
          TRUE                           ~ "uncertain"
        )
      )
    } else {
      dplyr::mutate(seg_class, state_auto = "uncertain")
    }
  }

  seg_class <- classify_result

  valid_tbl <- tibble::tibble(row_idx = valid_idx, segment_id = segment_labels) |>
    dplyr::left_join(seg_class, by = "segment_id")

  tof_proc <- tof_proc |>
    dplyr::mutate(row_idx = dplyr::row_number(), segment_id = NA_integer_,
                  state_auto = NA_character_) |>
    dplyr::rows_update(
      valid_tbl |> dplyr::select(row_idx, segment_id, state_auto),
      by = "row_idx"
    ) |>
    dplyr::select(-row_idx) |>
    dplyr::mutate(
      state_auto = factor(state_auto, levels = c("present", "absent", "uncertain")),
      # Night readings are always "present" — override PELT output for any
      # nighttime row that the algorithm touched, and mark source distinctly
      # so manual corrections can distinguish auto-day from auto-night.
      state_auto = dplyr::if_else(
        is_night,
        factor("present", levels = c("present", "absent", "uncertain")),
        state_auto
      ),
      state_corrected   = state_auto,
      correction_source = dplyr::case_when(
        is_night          ~ "night_auto",
        !is.na(state_auto) ~ "auto",
        TRUE               ~ NA_character_
      )
    ) |>
    dplyr::select(-is_night)

  # --- Bouts & summaries ------------------------------------------------------
  bout_res   <- recompute_bouts(tof_proc, min_bout_sec)
  tof_rl     <- bout_res$tof     # has state_run
  bouts      <- bout_res$bouts   # has state_run + bout_id

  summaries  <- recompute_summaries(tof_rl, bouts, meta, log_clean, typical_interval,
                                     lat = lat, lon = lon)
  summaries$device_summary <- summaries$device_summary |>
    dplyr::mutate(
      n_dht_bad   = sum(dht_clean$flag_dht_bad, na.rm = TRUE),
      pct_dht_bad = round(mean(dht_clean$flag_dht_bad, na.rm = TRUE) * 100, 2)
    )

  # Join bout_id back and drop state_run from the output file
  tof_out <- tof_rl |>
    dplyr::left_join(bouts |> dplyr::select(state_run, bout_id), by = "state_run") |>
    dplyr::select(-state_run)

  bouts_out <- bouts |> dplyr::select(-state_run)

  # --- Write ------------------------------------------------------------------
  readr::write_csv(tof_out,                         file.path(folder, "tof_processed.csv"))
  readr::write_csv(bouts_out,                       file.path(folder, "bout_summary.csv"))
  readr::write_csv(summaries$day_summary,           file.path(folder, "day_summary.csv"))
  readr::write_csv(summaries$device_summary,        file.path(folder, "device_summary.csv"))
  readr::write_csv(dht_clean,                       file.path(folder, "dht_processed.csv"))

  # Return the in-memory representation used by the app
  dts <- parse_deployment_datetimes(meta, recording_year = NULL)
  list(
    meta                = meta,
    tof                 = tof_out,
    dht                 = dht_clean,
    bouts               = bouts_out,
    day_summary         = summaries$day_summary,
    device_summary      = summaries$device_summary,
    typical_interval    = typical_interval,
    lat                 = lat,
    lon                 = lon,
    sun_times           = sun_times,
    folder              = folder,
    deployment_datetime = dts$deployment_datetime,
    retrieval_datetime  = dts$retrieval_datetime,
    trim_start          = dts$deployment_datetime,
    trim_end            = dts$retrieval_datetime
  )
}


# =============================================================================
# LOADING  (when processed files already exist)
# =============================================================================

#' Load a previously processed deployment folder into memory.
load_deployment <- function(folder) {
  folder <- normalizePath(folder, mustWork = TRUE)
  meta   <- parse_metadata(file.path(folder, "METADATA.TXT"))
  dts    <- parse_deployment_datetimes(meta)
  list(
    meta           = meta,
    tof            = readr::read_csv(file.path(folder, "tof_processed.csv"),
                                     show_col_types = FALSE) |>
                       dplyr::mutate(
                         timestamp       = lubridate::ymd_hms(timestamp),
                         state_auto      = factor(state_auto,
                                                  levels = c("present","absent","uncertain")),
                         state_corrected = factor(state_corrected,
                                                  levels = c("present","absent","uncertain"))
                       ),
    dht            = readr::read_csv(file.path(folder, "dht_processed.csv"),
                                     show_col_types = FALSE) |>
                       dplyr::mutate(timestamp = lubridate::ymd_hms(timestamp)),
    bouts          = readr::read_csv(file.path(folder, "bout_summary.csv"),
                                     show_col_types = FALSE) |>
                       dplyr::mutate(across(c(bout_start, bout_end), lubridate::ymd_hms)),
    day_summary    = readr::read_csv(file.path(folder, "day_summary.csv"),
                                     show_col_types = FALSE),
    device_summary = readr::read_csv(file.path(folder, "device_summary.csv"),
                                     show_col_types = FALSE),
    typical_interval    = 2,
    lat                 = NULL,
    lon                 = NULL,
    sun_times           = NULL,
    folder              = folder,
    deployment_datetime = dts$deployment_datetime,
    retrieval_datetime  = dts$retrieval_datetime,
    trim_start          = dts$deployment_datetime,  # default trim = deployment window
    trim_end            = dts$retrieval_datetime
  )
}

#' Detect which raw/clean/processed files are present and act accordingly.
#'
#' Loads processed files if they exist; otherwise runs ingestion + processing.
#' Throws "recording_year_needed" if year cannot be inferred and `year` is NULL.
load_or_process_deployment <- function(folder,
                                        year                   = NULL,
                                        smooth_sec             = 75,
                                        sensitivity            = 0.3,
                                        min_bout_sec           = 60,
                                        lat                    = NULL,
                                        lon                    = NULL,
                                        classification_method  = "gmm",
                                        out_folder             = NULL) {
  folder     <- normalizePath(folder, mustWork = TRUE)
  out_folder <- if (is.null(out_folder)) folder else {
    dir.create(out_folder, showWarnings = FALSE, recursive = TRUE)
    normalizePath(out_folder, mustWork = TRUE)
  }

  processed <- c("tof_processed.csv", "dht_processed.csv",
                 "bout_summary.csv", "day_summary.csv", "device_summary.csv")
  clean     <- c("tof_clean.csv", "dht_clean.csv", "log_clean.csv")

  if (all(file.exists(file.path(out_folder, processed)))) {
    return(load_deployment(out_folder))
  }

  if (!all(file.exists(file.path(out_folder, clean)))) {
    # may throw "recording_year_needed"
    ingest_deployment(folder, year = year, out_folder = out_folder)
  }

  process_deployment(out_folder,
                     smooth_sec            = smooth_sec,
                     sensitivity           = sensitivity,
                     min_bout_sec          = min_bout_sec,
                     lat                   = lat,
                     lon                   = lon,
                     classification_method = classification_method)
}


# =============================================================================
# EXPORT  (write corrected outputs back to disk)
# =============================================================================

#' Re-derive bouts/summaries from corrected TOF and write all processed files.
export_deployment <- function(dep, folder = dep$folder) {
  folder <- normalizePath(folder, mustWork = TRUE)

  bout_res  <- recompute_bouts(dep$tof, min_bout_sec = 60)
  bouts     <- bout_res$bouts
  summaries <- recompute_summaries(
    bout_res$tof, bouts, dep$meta,
    log_clean        = NULL,
    typical_interval = dep$typical_interval
  )

  tof_out   <- bout_res$tof |>
    dplyr::left_join(bouts |> dplyr::select(state_run, bout_id), by = "state_run") |>
    dplyr::select(-state_run)

  readr::write_csv(tof_out,                   file.path(folder, "tof_processed.csv"))
  readr::write_csv(bouts |> dplyr::select(-state_run),
                                               file.path(folder, "bout_summary.csv"))
  readr::write_csv(summaries$day_summary,      file.path(folder, "day_summary.csv"))
  readr::write_csv(summaries$device_summary,   file.path(folder, "device_summary.csv"))

  invisible(folder)
}
