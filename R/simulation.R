# =============================================================================
# R/simulation.R
# Generate a synthetic nest-box deployment for testing and demonstration.
# Sourced by app.R; also used by simulate_data.R (standalone script).
#
# Main entry point: generate_demo_deployment(folder, n_days, seed)
# =============================================================================

#' Generate a synthetic nest-box deployment and write the four device files.
#'
#' @param folder  Path to write TOF.CSV, DHT.CSV, LOG.CSV, METADATA.TXT.
#'                Created if it does not exist.
#' @param n_days  Number of days to simulate (default 5).
#' @param seed    RNG seed for reproducibility (default 2026).
#' @return Invisibly, the folder path.
generate_demo_deployment <- function(folder, n_days = 5L, seed = 2026L) {

  set.seed(seed)
  dir.create(folder, showWarnings = FALSE, recursive = TRUE)

  # ---- Parameters -----------------------------------------------------------

  START_TS   <- lubridate::ymd_hms("2026-04-15 04:00:00", tz = "UTC")
  TOF_INT    <- 2L
  DHT_INT    <- 30L
  EMA_ALPHA  <- 0.3
  P_INVALID  <- 0.003
  P_DHT_FAIL <- 0.004

  DIST <- list(
    on_night = list(mean = 85,  sd = 1.5, lo = 75,  hi = 100),
    on_day   = list(mean = 92,  sd = 6,   lo = 65,  hi = 115),
    off_cov  = list(mean = 107, sd = 8,   lo = 80,  hi = 135),
    off_exp  = list(mean = 128, sd = 9,   lo = 95,  hi = 160)
  )
  P_COVER    <- 0.40
  ON_SHAPE   <- 4.0;  ON_RATE  <- 0.44
  OFF_SHAPE  <- 4.0;  OFF_RATE <- 0.60

  rdist <- function(n, p) pmax(p$lo, pmin(p$hi, round(stats::rnorm(n, p$mean, p$sd))))

  # ---- Sunrise/sunset -------------------------------------------------------

  sunrise_min <- function(d) as.integer(round(388 - d * 4.0))
  sunset_min  <- function(d) as.integer(round(1235 + d * 3.5))

  # ---- Behavioural schedule -------------------------------------------------

  build_schedule <- function() {
    bouts    <- vector("list", 600L)
    bout_idx <- 0L
    add_bout <- function(s, e, st, cv = FALSE) {
      bout_idx <<- bout_idx + 1L
      bouts[[bout_idx]] <<- list(start = s, end = e, state = st, covered = cv)
    }

    current <- START_TS
    for (d in seq_len(n_days) - 1L) {
      date_d     <- lubridate::as_date(START_TS) + lubridate::days(d)
      sunrise_ts <- date_d + lubridate::minutes(sunrise_min(d))
      sunset_ts  <- date_d + lubridate::minutes(sunset_min(d))
      dep_ts     <- sunrise_ts + lubridate::minutes(sample(10:22, 1))

      add_bout(current, dep_ts, "on")
      current <- dep_ts

      off_dur <- sample(14:26, 1)
      add_bout(current, current + lubridate::minutes(off_dur), "off",
               cv = stats::runif(1) < P_COVER)
      current <- current + lubridate::minutes(off_dur)

      while (current < sunset_ts - lubridate::minutes(35)) {
        on_dur <- as.integer(max(4L, round(stats::rgamma(1, ON_SHAPE, ON_RATE))))
        on_end <- current + lubridate::minutes(on_dur)
        if (on_end >= sunset_ts - lubridate::minutes(20)) {
          add_bout(current, on_end, "on"); current <- on_end; break
        }
        add_bout(current, on_end, "on"); current <- on_end

        hf     <- lubridate::hour(current) + lubridate::minute(current) / 60
        mf     <- 1 + 0.9 * exp(-((hf - 13)^2) / 8)
        od     <- as.integer(max(2L, min(26L, round(stats::rgamma(1, OFF_SHAPE, OFF_RATE) * mf))))
        add_bout(current, current + lubridate::minutes(od), "off",
                 cv = stats::runif(1) < P_COVER)
        current <- current + lubridate::minutes(od)
      }
    }
    end_ts <- lubridate::as_date(START_TS) + lubridate::days(n_days) + lubridate::hours(6)
    add_bout(current, end_ts, "on")
    dplyr::bind_rows(bouts[seq_len(bout_idx)])
  }

  schedule <- build_schedule()

  # ---- TOF time series ------------------------------------------------------

  end_ts  <- lubridate::as_date(START_TS) + lubridate::days(n_days) + lubridate::hours(6)
  tof_ts  <- seq(START_TS, end_ts, by = TOF_INT)
  n_tof   <- length(tof_ts)

  bout_starts  <- as.numeric(schedule$start)
  bout_state   <- schedule$state
  bout_covered <- schedule$covered
  bi           <- pmax(1L, findInterval(as.numeric(tof_ts), bout_starts))
  state        <- bout_state[bi]
  covered      <- bout_covered[bi]
  is_night     <- !(lubridate::hour(tof_ts) >= 6 & lubridate::hour(tof_ts) <= 21)

  tof_raw <- integer(n_tof)
  tof_raw[state == "on"  & is_night]    <- rdist(sum(state == "on"  & is_night),    DIST$on_night)
  tof_raw[state == "on"  & !is_night]   <- rdist(sum(state == "on"  & !is_night),   DIST$on_day)
  tof_raw[state == "off" & covered]     <- rdist(sum(state == "off" & covered),     DIST$off_cov)
  tof_raw[state == "off" & !covered]    <- rdist(sum(state == "off" & !covered),    DIST$off_exp)

  # Within-bout drift (state-dependent AR-1)
  drift <- numeric(n_tof)
  for (i in seq(2L, n_tof)) {
    if (bi[i] != bi[i - 1L]) {
      drift[i] <- 0
    } else {
      sigma    <- if (is_night[i] && state[i] == "on") 0.25 else 0.8
      drift[i] <- drift[i - 1L] * 0.95 + stats::rnorm(1L, 0, sigma)
    }
  }
  tof_raw <- pmax(40L, pmin(200L, as.integer(round(tof_raw + drift))))

  # Nighttime behavioural anomalies (brief spikes)
  night_on <- which(
    schedule$state == "on" &
    as.numeric(difftime(schedule$end, schedule$start, units = "hours")) > 2
  )
  for (nb in night_on) {
    rows <- which(bi == nb)
    if (length(rows) < 20L) next
    for (k in seq_len(stats::rpois(1, 1.5))) {
      ss  <- sample(seq_along(rows), 1)
      sl  <- sample(2:5, 1)
      idx <- rows[ss:min(ss + sl, length(rows))]
      mag <- stats::runif(1, 10, 28)
      tof_raw[idx] <- pmin(200L,
                           as.integer(round(tof_raw[idx] +
                                              mag * exp(-seq(0, 3, length.out = length(idx))))))
    }
  }

  # Invalid readings and on-device EMA
  tof_raw[stats::runif(n_tof) < P_INVALID] <- -1L
  tof_smooth <- integer(n_tof)
  ema        <- 0
  for (i in seq_len(n_tof)) {
    if (tof_raw[i] > 0L) ema <- EMA_ALPHA * tof_raw[i] + (1 - EMA_ALPHA) * ema
    tof_smooth[i] <- round(ema)
  }

  tof_df <- tibble::tibble(
    timestamp  = format(tof_ts, "%m-%d %H:%M:%S"),
    tof_raw    = tof_raw,
    tof_smooth = tof_smooth
  )

  # ---- DHT time series -------------------------------------------------------

  dht_ts  <- seq(START_TS, end_ts, by = DHT_INT)
  n_dht   <- length(dht_ts)
  day_frac  <- as.numeric(difftime(dht_ts, START_TS, units = "days"))
  hour_frac <- (lubridate::hour(dht_ts) + lubridate::minute(dht_ts) / 60) / 24

  mean_out  <- 11.2 + 0.12 * day_frac
  amp_out   <- 4.8  + 0.20 * sin(2 * pi * day_frac / 7)
  temp_base <- mean_out - amp_out * cos(2 * pi * (hour_frac - 5 / 24))
  weather   <- cumsum(stats::rnorm(n_dht, 0, 0.04))
  weather   <- (weather - mean(weather)) * 0.7
  temp_out  <- round(temp_base + weather + stats::rnorm(n_dht, 0, 0.35), 1)

  dht_bi    <- pmax(1L, findInterval(as.numeric(dht_ts), bout_starts))
  body_heat <- dplyr::if_else(bout_state[dht_bi] == "on",
                               stats::rnorm(n_dht, 1.3, 0.3), 0)
  temp_in   <- round(temp_out * 0.82 + 3.8 + body_heat + stats::rnorm(n_dht, 0, 0.3), 1)

  dewpoint  <- 7.0
  rh_from_t <- function(t, td = dewpoint)
    100 * exp(17.27 * td / (237.3 + td)) / exp(17.27 * t / (237.3 + t))
  hum_out   <- round(pmax(35, pmin(98, rh_from_t(temp_out) + stats::rnorm(n_dht, 0, 2))), 1)
  hum_in    <- round(pmax(35, pmin(98, rh_from_t(temp_in)  + stats::rnorm(n_dht, 0, 2))), 1)

  fail <- stats::runif(n_dht) < P_DHT_FAIL
  temp_out[fail] <- NA_real_; hum_out[fail] <- NA_real_
  temp_in[fail]  <- NA_real_; hum_in[fail]  <- NA_real_

  dht_df <- tibble::tibble(
    timestamp = format(dht_ts, "%m-%d %H:%M:%S"),
    temp1     = temp_in,
    hum1      = hum_in,
    temp2     = temp_out,
    hum2      = hum_out
  )

  # ---- Log and metadata ------------------------------------------------------

  log_df <- tibble::tibble(
    timestamp = format(START_TS + lubridate::seconds(2), "%m-%d %H:%M:%S"),
    event     = "START",
    device_id = "DEV_DEMO",
    notes     = "boot"
  )

  meta_lines <- c(
    "device_id=DEV_DEMO",
    "deployment_id=DEMO_2026",
    "",
    "nestbox=NB_DEMO",
    "species=Cyanistes caeruleus",
    "eggs=7",
    "deployment_date=2026-04-15",
    "retrieval_date=2026-04-20",
    "latitude=52.09",
    "longitude=5.12",
    "",
    "notes=Simulated demo deployment — Netherlands April 2026"
  )

  # ---- Write -----------------------------------------------------------------

  readr::write_csv(tof_df, file.path(folder, "TOF.CSV"),      na = "NA")
  readr::write_csv(dht_df, file.path(folder, "DHT.CSV"),      na = "NA")
  readr::write_csv(log_df, file.path(folder, "LOG.CSV"))
  writeLines(meta_lines,   file.path(folder, "METADATA.TXT"))

  invisible(folder)
}
