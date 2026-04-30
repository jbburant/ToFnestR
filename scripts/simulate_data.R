# =============================================================================
# simulate_data.R
# Generates 5 days of realistic nest-box sensor data for testing the
# Nest Box Incubation Monitor pipeline and Shiny app.
#
# Ecological context:
#   Species  : small cavity-nesting passerine (e.g. blue tit / great tit)
#   Location : Netherlands (~52°N), mid-April 2026
#   Stage    : active incubation (day 1 of 14)
#   Behaviour: female incubates through the night, leaves at first light,
#              iterant incubation through the day (short on/off bouts),
#              returns to roost well before sunset.
#
# Output (written to ./simulation/):
#   TOF.CSV, DHT.CSV, LOG.CSV, METADATA.TXT
#   — copy this folder into the app and import as a single deployment.
#
# Run with: source("simulate_data.R")
# Requires: dplyr, lubridate, purrr, readr  (all tidyverse)
# =============================================================================

library(dplyr)
library(lubridate)
library(purrr)
library(readr)

set.seed(2026)

# =============================================================================
# PARAMETERS
# =============================================================================

START_TS    <- ymd_hms("2026-04-15 04:00:00", tz = "UTC")
N_DAYS      <- 5L
TOF_INT     <- 2L    # seconds between TOF readings
DHT_INT     <- 30L   # seconds between DHT readings
EMA_ALPHA   <- 0.3   # matches device firmware
P_INVALID   <- 0.003 # proportion of TOF readings that fail (-1)
P_DHT_FAIL  <- 0.004 # proportion of DHT readings that fail (NA)

# Distance distributions (mm) —————————————————————————————————————
# Measurements are from the sensor on the roof DOWN to whatever is below.
# Lower values = something closer to the sensor (female or nest material).

DIST <- list(
  on_night = list(mean = 85, sd = 1.5, lo = 75, hi = 100),  # asleep, very stable
  on_day   = list(mean = 92, sd =  6,  lo = 65, hi = 115),  # upright, incubating
  off_cov  = list(mean = 107, sd = 8,  lo = 80, hi = 135),  # eggs covered w/ material
  off_exp  = list(mean = 128, sd = 9,  lo = 95, hi = 160)   # eggs exposed in cup
)

P_COVER <- 0.40   # probability female covers eggs on a given off-bout

# Bout duration parameters (gamma distribution, in minutes) ———————
ON_SHAPE  <- 4.0;  ON_RATE  <- 0.44   # mean ~9.1 min
OFF_SHAPE <- 4.0;  OFF_RATE <- 0.60   # mean ~6.7 min (longer than before)


# =============================================================================
# SUNRISE / SUNSET  (Netherlands, 52°N, mid-April)
# =============================================================================

# Sunrise advances ~4 min/day; sunset delays ~3.5 min/day over this period.
# Values in minutes from local midnight.

sunrise_min <- function(day_idx) as.integer(round(388 - day_idx * 4.0))  # 06:28 → 06:12
sunset_min  <- function(day_idx) as.integer(round(1235 + day_idx * 3.5)) # 20:35 → 20:53


# =============================================================================
# BUILD BEHAVIOURAL SCHEDULE
# =============================================================================
# Returns a data frame with one row per bout:
#   start, end (POSIXct), state ("on"/"off"), covered (logical)

build_schedule <- function(start_ts, n_days) {

  bouts    <- vector("list", 500L)  # pre-allocate
  bout_idx <- 0L
  add_bout <- function(start, end, state, covered = FALSE) {
    bout_idx <<- bout_idx + 1L
    bouts[[bout_idx]] <<- list(start = start, end = end,
                                state = state, covered = covered)
  }

  current <- start_ts   # pointer to end of last scheduled event

  for (d in seq_len(n_days) - 1L) {
    date_d     <- as_date(start_ts) + days(d)
    sunrise_ts <- date_d + minutes(sunrise_min(d))
    sunset_ts  <- date_d + minutes(sunset_min(d))

    # ── Night / pre-dawn: female on nest until first departure ──────────
    departure_ts <- sunrise_ts + minutes(sample(10:22, 1))
    add_bout(current, departure_ts, "on")
    current <- departure_ts

    # ── Dawn departure: first off-bout (longer than typical) ────────────
    off_dur <- sample(14:26, 1)
    add_bout(current, current + minutes(off_dur), "off",
             covered = runif(1) < P_COVER)
    current <- current + minutes(off_dur)

    # ── Iterant incubation through the day ──────────────────────────────
    # Alternate on/off bouts until ~35 min before sunset.
    # Off-bout durations are slightly longer around midday (heat / food).
    while (current < sunset_ts - minutes(35)) {

      # On-bout
      on_dur <- as.integer(max(4L, round(rgamma(1, ON_SHAPE, ON_RATE))))
      on_end <- current + minutes(on_dur)

      if (on_end >= sunset_ts - minutes(20)) {
        # Close enough to sunset — female settles and stays overnight.
        # Do NOT add a terminal off-bout; just let the on-bout run into night.
        add_bout(current, on_end, "on")
        current <- on_end
        break
      }

      add_bout(current, on_end, "on")
      current <- on_end

      # Off-bout — longer on warm afternoons (peak ~13:00, up to ~25 min)
      hour_frac     <- hour(current) + minute(current) / 60
      midday_factor <- 1 + 0.9 * exp(-((hour_frac - 13)^2) / 8)
      off_dur <- as.integer(max(2L, min(26L, round(rgamma(1, OFF_SHAPE, OFF_RATE) * midday_factor))))
      add_bout(current, current + minutes(off_dur), "off",
               covered = runif(1) < P_COVER)
      current <- current + minutes(off_dur)
    }
  }

  # Final on-bout covers the remainder of the simulation window
  end_ts <- as_date(start_ts) + days(n_days) + hours(6)
  add_bout(current, end_ts, "on")

  bind_rows(bouts[seq_len(bout_idx)])
}

message("Building behavioural schedule ...")
schedule <- build_schedule(START_TS, N_DAYS)

n_on  <- schedule |> filter(state == "on")  |> nrow()
n_off <- schedule |> filter(state == "off") |> nrow()
message("  ", n_on, " on-bouts, ", n_off, " off-bouts")


# =============================================================================
# HELPER: sample distances with clipping
# =============================================================================

rdist <- function(n, params) {
  pmax(params$lo, pmin(params$hi, round(rnorm(n, params$mean, params$sd))))
}


# =============================================================================
# GENERATE TOF TIME SERIES
# =============================================================================

end_ts  <- as_date(START_TS) + days(N_DAYS) + hours(6)
tof_ts  <- seq(START_TS, end_ts, by = TOF_INT)
n_tof   <- length(tof_ts)
message("Generating ", format(n_tof, big.mark = ","), " TOF readings ...")

# Assign each timestamp to a bout via interval lookup on start times
bout_starts  <- as.numeric(schedule$start)
bout_state   <- schedule$state
bout_covered <- schedule$covered

ts_num   <- as.numeric(tof_ts)
bi       <- pmax(1L, findInterval(ts_num, bout_starts))
state    <- bout_state[bi]
covered  <- bout_covered[bi]
is_night <- !(hour(tof_ts) >= 6 & hour(tof_ts) <= 21)

# Distance sampling per state ─────────────────────────────────────────────────
tof_raw <- integer(n_tof)

mask_night_on <- state == "on"  & is_night
mask_day_on   <- state == "on"  & !is_night
mask_off_cov  <- state == "off" & covered
mask_off_exp  <- state == "off" & !covered

tof_raw[mask_night_on] <- rdist(sum(mask_night_on), DIST$on_night)
tof_raw[mask_day_on]   <- rdist(sum(mask_day_on),   DIST$on_day)
tof_raw[mask_off_cov]  <- rdist(sum(mask_off_cov),  DIST$off_cov)
tof_raw[mask_off_exp]  <- rdist(sum(mask_off_exp),  DIST$off_exp)

# Within-bout positional drift (AR-1, reset at every bout boundary) ──────────
# State-dependent noise keeps night measurements tight (1-3 mm excursion)
# while allowing realistic daytime shuffle. Lower rho (0.95 vs 0.985) prevents
# the process drifting to ±10 mm over long overnight bouts.
message("  Adding within-bout positional drift ...")
drift <- numeric(n_tof)
for (i in seq(2L, n_tof)) {
  if (bi[i] != bi[i - 1L]) {
    drift[i] <- 0L  # hard reset at every state transition
  } else {
    sigma    <- if (is_night[i] && state[i] == "on") 0.25 else 0.8
    drift[i] <- drift[i - 1L] * 0.95 + rnorm(1L, 0, sigma)
  }
}
tof_raw <- pmax(40L, pmin(200L, as.integer(round(tof_raw + drift))))

# Occasional behavioural anomalies during night on-bouts ─────────────────────
# Female briefly stands, turns, or rearranges nest material — produces a sharp
# but short-lived spike of 10–30 mm, then settles back within a few readings.
night_on_bouts <- which(schedule$state == "on" &
                         as.numeric(difftime(schedule$end, schedule$start,
                                             units = "hours")) > 2)
for (nb in night_on_bouts) {
  n_events <- rpois(1, 1.5)     # ~1-2 anomalies per long on-bout (overnight)
  if (n_events == 0L) next
  rows_in_bout <- which(bi == nb)
  if (length(rows_in_bout) < 20L) next
  spike_starts <- sample(seq_along(rows_in_bout), n_events, replace = FALSE)
  for (ss in spike_starts) {
    spike_len <- sample(2:5, 1)   # 4-10 seconds
    spike_mag <- runif(1, 10, 28)
    idx       <- rows_in_bout[ss:min(ss + spike_len, length(rows_in_bout))]
    decay     <- exp(-seq(0, 3, length.out = length(idx)))
    tof_raw[idx] <- pmin(200L, as.integer(round(tof_raw[idx] + spike_mag * decay)))
  }
}

# Invalid readings (-1) ───────────────────────────────────────────────────────
inv_mask        <- runif(n_tof) < P_INVALID
tof_raw[inv_mask] <- -1L

# On-device EMA smooth (alpha = 0.3, initialises at 0) ───────────────────────
# Matches firmware behaviour exactly, including warmup artefact.
message("  Computing on-device EMA ...")
tof_smooth <- integer(n_tof)
ema        <- 0
for (i in seq_len(n_tof)) {
  if (tof_raw[i] > 0L) ema <- EMA_ALPHA * tof_raw[i] + (1 - EMA_ALPHA) * ema
  tof_smooth[i] <- round(ema)
}

tof_df <- tibble(
  timestamp  = format(tof_ts, "%m-%d %H:%M:%S"),
  tof_raw    = tof_raw,
  tof_smooth = tof_smooth
)


# =============================================================================
# GENERATE DHT TIME SERIES
# =============================================================================

dht_ts <- seq(START_TS, end_ts, by = DHT_INT)
n_dht  <- length(dht_ts)
message("Generating ", format(n_dht, big.mark = ","), " DHT readings ...")

day_frac  <- as.numeric(difftime(dht_ts, START_TS, units = "days"))
hour_frac <- (hour(dht_ts) + minute(dht_ts) / 60) / 24

# Outside temperature ─────────────────────────────────────────────────────────
# Netherlands April: mean ~11°C, diurnal amplitude ~5°C.
# Trough ~05:00, peak ~14:00. Slight warming trend over the 5 days.
mean_out <- 11.2 + 0.12 * day_frac
amp_out  <-  4.8 + 0.20 * sin(2 * pi * day_frac / 7)  # gentle weekly beat

# Phase offset so minimum lands at 05:00 (5/24 of day)
phase     <- 2 * pi * (hour_frac - 5 / 24)
temp_base <- mean_out - amp_out * cos(phase)

# Slow autocorrelated weather noise (cloudy days = cooler/more variable)
weather   <- cumsum(rnorm(n_dht, 0, 0.04))
weather   <- (weather - mean(weather)) * 0.7
temp_out  <- round(temp_base + weather + rnorm(n_dht, 0, 0.35), 1)

# Inside temperature ──────────────────────────────────────────────────────────
# Box is a few degrees warmer than outside (thermal inertia + solar gain).
# Female body heat adds ~1-1.5°C when incubating.
dht_bi     <- pmax(1L, findInterval(as.numeric(dht_ts), bout_starts))
dht_state  <- bout_state[dht_bi]
body_heat  <- if_else(dht_state == "on", rnorm(n_dht, 1.3, 0.3), 0)
temp_in    <- round(temp_out * 0.82 + 3.8 + body_heat + rnorm(n_dht, 0, 0.3), 1)

# Humidity ────────────────────────────────────────────────────────────────────
# Approximate RH from dewpoint (~7°C typical for Netherlands April).
# RH falls when temperature rises (dewpoint stays roughly constant).
dewpoint  <- 7.0
rh_approx <- function(temp, td = dewpoint) {
  100 * exp(17.27 * td / (237.3 + td)) /
        exp(17.27 * temp / (237.3 + temp))
}
hum_out <- round(pmax(35, pmin(98, rh_approx(temp_out) + rnorm(n_dht, 0, 2))), 1)
hum_in  <- round(pmax(35, pmin(98, rh_approx(temp_in)  + rnorm(n_dht, 0, 2))), 1)

# DHT sensor read failures (NA rows) ─────────────────────────────────────────
fail_mask          <- runif(n_dht) < P_DHT_FAIL
temp_out[fail_mask] <- NA_real_
hum_out[fail_mask]  <- NA_real_
temp_in[fail_mask]  <- NA_real_
hum_in[fail_mask]   <- NA_real_

dht_df <- tibble(
  timestamp = format(dht_ts, "%m-%d %H:%M:%S"),
  temp1     = temp_in,    # inside  (sensor 1, in box)
  hum1      = hum_in,
  temp2     = temp_out,   # outside (sensor 2, in housing)
  hum2      = hum_out
)


# =============================================================================
# LOG AND METADATA
# =============================================================================

log_df <- tibble(
  timestamp = format(START_TS + seconds(2), "%m-%d %H:%M:%S"),
  event     = "START",
  device_id = "DEV_26-01",
  notes     = "boot"
)

metadata_lines <- c(
  "device_id=DEV_26-01",
  "deployment_id=DEP_2026_SIM",
  "",
  "nestbox=NB_042",
  "species=Cyanistes caeruleus",
  "eggs=7",
  "deployment_date=2026-04-15",
  "deployment_time=04:20:00",
  "retrieval_date=2026-04-20",
  "retrieval_time=05:40:00",
  "",
  "notes=Simulated 5-day incubation dataset for pipeline and app testing"
)


# =============================================================================
# WRITE OUTPUT
# =============================================================================

out_dir <- file.path("/Users/josephburant/Downloads/simulation")
dir.create(out_dir, showWarnings = FALSE)

write_csv(tof_df,   file.path(out_dir, "TOF.CSV"),      na = "NA")
write_csv(dht_df,   file.path(out_dir, "DHT.CSV"),      na = "NA")
write_csv(log_df,   file.path(out_dir, "LOG.CSV"))
writeLines(metadata_lines, file.path(out_dir, "METADATA.TXT"))

message("\n=== Simulation complete ===")
message("  Output folder : ", out_dir)
message("  TOF.CSV       : ", format(nrow(tof_df), big.mark = ","), " rows  (",
        round(file.size(file.path(out_dir, "TOF.CSV")) / 1e6, 1), " MB)")
message("  DHT.CSV       : ", format(nrow(dht_df), big.mark = ","), " rows")
message("  Period        : ", format(START_TS, "%Y-%m-%d"),
        " → ", format(end_ts, "%Y-%m-%d"))
message("\nNext step: import ./simulation/ as a deployment in the Shiny app,")
message("  entering 2026 when prompted for the recording year.")

