# ToFnestR — User Guide

ToFnestR ingests raw CSV data from the nest-box sensor device, applies automated
incubation-state classification to the time-of-flight (ToF) distance signal, and
lets you review and manually correct classifications before exporting clean,
analysis-ready outputs.

---

## Contents

1. [Quick start](#1-quick-start)
2. [Import & setup](#2-import--setup)
3. [Overview tab](#3-overview-tab)
4. [Correct States tab](#4-correct-states-tab)
5. [Bout Summary tab](#5-bout-summary-tab)
6. [Climate tab](#6-climate-tab)
7. [Deployment Summary tab](#7-deployment-summary-tab)
8. [Processing parameters](#8-processing-parameters)
9. [Classification methods](#9-classification-methods)
10. [Output files](#10-output-files)
11. [METADATA.TXT fields](#11-metadatatxt-fields)

---

## 1. Quick start

1. Place your deployment folder (containing `TOF.CSV`, `DHT.CSV`, `LOG.CSV`, `METADATA.TXT`) inside `raw_data/`
2. Click **Select folder** in the sidebar and choose the deployment
3. Enter the recording year and site coordinates when prompted
4. Review classifications in the **Overview** tab
5. Correct any misclassified readings in the **Correct States** tab
6. Click **Download this deployment** to export a corrected ZIP

---

## 2. Import & setup

### Selecting a deployment

Click **Select folder** and navigate to a deployment folder. Tick
**Multiple deployments (subdirectories)** to load all subfolders in a directory
at once — useful for batch processing.

<!-- screenshot: import folder picker -->

### Year prompt

If `deployment_date` is absent or blank in `METADATA.TXT`, the app will ask you
to enter the recording year. This is required to reconstruct full timestamps from
the `MM-DD HH:MM:SS` format used by the device.

### Coordinates prompt

If `latitude` and `longitude` are absent, the app will prompt you (with a Skip
option). Providing coordinates enables:

- Sunrise/sunset lines on all time-series plots
- Night readings automatically classified as present (see [Classification methods](#9-classification-methods))
- Active-day on-nest percentage in the [Deployment Summary](#7-deployment-summary-tab)

> **Tip:** add `latitude=` and `longitude=` to `METADATA.TXT` before deploying to avoid the prompt entirely.

### Demo data

Click **Load demo data** to generate and load a synthetic 5-day deployment
(Netherlands, April 2026, blue tit). No files are required. Use this to explore
the app before working with real data.

---

## 3. Overview tab

The Overview tab shows the full deployment as a smoothed ToF distance time series.

<!-- screenshot: overview tab with state shading and sunrise/sunset markers -->

### Reading the plot

| Visual element | Meaning |
|---|---|
| <span style="background:#d1e5f0; border-left:4px solid #2166ac; padding:1px 6px; border-radius:3px;">Blue shading</span> | Female present (on nest) |
| <span style="background:#fddbc7; border-left:4px solid #d6604d; padding:1px 6px; border-radius:3px;">Red shading</span> | Female absent (off nest) |
| <span style="background:#e8e8e8; border-left:4px solid #878787; padding:1px 6px; border-radius:3px;">Grey shading</span> | Uncertain classification |
| Amber dotted line ↑ | Sunrise |
| Brown dotted line ↓ | Sunset |
| Blue dashed line ▶ | Deployment start (trim boundary) |
| Red dashed line ◀ | Retrieval end (trim boundary) |
| Red dashed vertical | Device reboot |

Use the range selector at the bottom to zoom in on a region of interest before
switching to the Correct States tab.

---

## 4. Correct States tab

The Correct States tab lets you manually reassign classifications for any region
of the time series.

<!-- screenshot: correct states tab with box selection active -->

### Making a selection

1. Select a **day** from the dropdown at the top right of the plot
2. In the plotly toolbar, click the **Box Select** tool (dashed-rectangle icon — usually the third tool from the left)
3. Drag a rectangle over the readings you want to reassign

The selection captures all readings within **both** the time range and the
distance range of the box — including non-visible downsampled points. This means
you can draw a narrow horizontal band to target readings at a specific distance
(e.g. select only "uncertain" readings in the 105–130 mm range) without affecting
clearly-present readings at shorter distances in the same time window.

The feedback line below the plot shows how many readings are selected, their
current state breakdown, and the exact time and distance range of the selection.

<!-- screenshot: selection feedback showing count and state breakdown -->

### Applying a correction

After making a selection, click one of the state buttons:

| Button | Action |
|---|---|
| **Mark Present** | Female on nest |
| **Mark Absent** | Female off nest |
| **Mark Uncertain** | Return to uncertain |
| **Reset to Auto** | Revert to original automated classification |

Corrections update the Bout Summary and Deployment Summary immediately.

> **Note:** data outside the trim window is shown in grey and remains correctable,
> but will not appear in exported files.

### Setting trim boundaries

The trim window defines the active deployment period. Readings outside this window
are excluded from all summaries and exports. Raw sensor data is never modified.

| Field | Format |
|---|---|
| Trim start | `YYYY-MM-DD HH:MM:SS` |
| Trim end   | `YYYY-MM-DD HH:MM:SS` |

Trim boundaries are pre-populated automatically from `deployment_date` +
`deployment_time` and `retrieval_date` + `retrieval_time` in `METADATA.TXT`
(times default to `00:00:00` if absent). They can be adjusted freely at any time
— click **Apply trim** to update.

> **Planned:** a prompt for missing deployment/retrieval times on import, consistent
> with the year and coordinates prompts.

---

## 5. Bout Summary tab

The Bout Summary tab shows all incubation bouts within the trim window.

Toggle between **Table** and **Timeline** views using the radio buttons in the
card header.

<!-- screenshot: bout summary table -->

### Table view

One row per bout. Columns:

| Column | Description |
|---|---|
| ID | Bout identifier |
| State | Present / Absent / Uncertain |
| Start / End | Bout boundaries (`YYYY-MM-DD HH:MM`) |
| Duration (min) | Bout length in minutes |
| Readings | Number of ToF readings in the bout |
| Mean ToF (mm) | Mean smoothed distance during the bout |
| Short? | `TRUE` if below the minimum bout duration threshold |

### Timeline view

Each bout is drawn as a horizontal coloured segment. The Y-axis is ordered:

```
Absent     (top)
Uncertain
Present    (bottom)
```

This mirrors the ToF signal — lower distance = female closer to sensor = present.
Dashed vertical lines mark state transitions.

<!-- screenshot: bout summary timeline view -->

---

## 6. Climate tab

The Climate tab shows inside and outside temperature (°C) and relative humidity
(%) for the deployment period, trimmed to the active window.

<!-- screenshot: climate tab -->

Sunrise/sunset markers appear on both plots when site coordinates are available.
Use the range selector to zoom in on a period of interest.

---

## 7. Deployment Summary tab

### Daily statistics

One row per calendar day within the trim window.

| Column | Description |
|---|---|
| Date | Calendar date |
| Sunrise / Sunset | Local time (when coordinates provided) |
| Active day (h) | Daylight hours (sunrise to sunset) |
| % active day on-nest | On-nest time as % of daylight period |
| % 24-h on-nest | On-nest time as % of full 24-hour day |
| On-nest (h) | Hours spent on nest |
| Off-bouts (n) | Number of off-nest bouts |
| Mean off-bout (min) | Average off-bout duration |

### Device health

Deployment-level summary of sensor performance.

| Field | Description |
|---|---|
| span_days | Total deployment duration |
| n_reboots | Number of unplanned device restarts |
| total_downtime_min | Combined gap duration (> 5 min gaps) |
| pct_tof_invalid | Proportion of invalid ToF readings |
| n_tof_warmup | Readings flagged as EMA warm-up |
| n_dht_bad | Failed DHT temperature/humidity readings |

---

## 8. Processing parameters

### Smoothing window (s)

Width of the rolling-median filter applied to raw ToF readings before state
detection. Wider = smoother signal, slower response to rapid transitions.

**Default: 75 s**

### Uncertain zone / confidence threshold

Controls how confident the algorithm must be before making a hard present/absent
call.

- `0` — classify everything (no uncertain zone)
- `0.3` — recommended starting point
- `1.0` — only near-certain segments are labelled; everything else is uncertain

The meaning differs slightly between methods (see [Classification methods](#9-classification-methods)).

**Default: 0.3**

### Min. bout duration (s)

Bouts shorter than this threshold are flagged `flag_short = TRUE` in exported
files but kept in the data. Adjust if very short bouts are artefacts in your
dataset.

**Default: 60 s**

---

## 9. Classification methods

State detection works in two stages:

1. **PELT change-point detection** finds where the distance signal shifts
   sustained mean level — these become segment boundaries
2. **Segment classification** assigns each segment a state based on its median
   distance relative to the deployment's overall distribution

When site coordinates are provided, night readings (sunset → sunrise) are
**automatically classified as present** and excluded from stage 2. This prevents
the stable overnight signal from compressing the classification threshold for
daytime data.

### Gaussian mixture model (GMM) — recommended

Fits a two-component Gaussian distribution to the daytime segment medians. Each
component has its own mean *and* variance — useful because absent readings tend to
be more variable than present ones (eggs may be exposed or covered, at slightly
different distances). Returns a posterior probability per segment; the confidence
threshold controls the width of the uncertain band around the cluster boundary.

### K-means

Assigns each segment to the nearest of two cluster centres. Simpler and slightly
faster, but assumes equal spread in both states.

> Both methods classify each segment **independently**, with no assumption about
> the order of states — wild bird behaviour is irregular and any sequence is valid.

---

## 10. Output files

All files are trimmed to the active deployment window before export. Raw sensor
data is never modified.

### `tof_processed.csv`

Every ToF reading with the following columns:

| Column | Description |
|---|---|
| `timestamp` | Full datetime (UTC) |
| `tof_raw_mm` | Raw distance reading (mm); `-1` = sensor error |
| `tof_smooth_ema` | On-device exponential moving average (α = 0.3) |
| `tof_smooth_r` | R-side rolling-median smooth (used for state detection) |
| `flag_tof_invalid` | `TRUE` if reading is invalid (−1 or 8190) |
| `flag_ema_warmup` | `TRUE` for first 20 readings after each reboot |
| `segment_id` | PELT change-point segment identifier |
| `state_auto` | Automated classification (present / absent / uncertain) |
| `state_corrected` | Final classification after manual corrections |
| `correction_source` | `auto`, `manual`, or `night_auto` |
| `bout_id_auto` | Bout ID from original automated processing |
| `bout_id_corrected` | Bout ID after corrections and trimming |

### `bout_summary.csv`

One row per incubation bout:

| Column | Description |
|---|---|
| `bout_id` | Unique identifier |
| `state_corrected` | State for this bout |
| `bout_start` / `bout_end` | Bout boundaries (UTC) |
| `bout_duration` | Duration in seconds |
| `n_readings` | ToF readings in the bout |
| `mean_tof_mm` | Mean smoothed distance |
| `flag_short` | `TRUE` if below the minimum duration threshold |

### `day_summary.csv`

One row per calendar day within the trim window. Daily statistics are derived
from bouts (ToF → Bouts → Daily). When coordinates are provided, bouts are
filtered to those starting during the active day (sunrise → sunset) and
day length is the denominator for proportions; otherwise the full 24-hour day
is used.

| Column | Description |
|---|---|
| `date` | Calendar date (YYYY-MM-DD) |
| `sunrise` | Sunrise time (HH:MM, when coordinates provided) |
| `sunset` | Sunset time (HH:MM, when coordinates provided) |
| `day_length_h` | Active day length in hours (sunrise → sunset, or 24) |
| `first_off_start` | Start time of the first off-bout (HH:MM) |
| `last_off_end` | End time of the last off-bout (HH:MM) |
| `prop_on_nest` | Sum of present-bout durations / day length (0–1) |
| `prop_off_nest` | Sum of absent-bout durations / day length (0–1) |
| `prop_uncertain` | Sum of uncertain-bout durations / day length (0–1) |
| `n_on_bouts` | Count of present bouts during active day (integer) |
| `n_off_bouts` | Count of absent bouts during active day (integer) |
| `mean_on_bout_min` | Mean present-bout duration (min); NA if no on-bouts |
| `mean_off_bout_min` | Mean absent-bout duration (min); NA if no off-bouts |

### `device_summary.csv`

Single-row deployment health card (see [Device health](#device-health)).

### `dht_processed.csv`

Temperature and humidity readings trimmed to the deployment window.

---

## 11. METADATA.TXT fields

```
device_id=          # matches firmware (required)
deployment_id=      # unique deployment label (required)

nestbox=            # nest box identifier
species=            # species name
eggs=               # clutch size
deployment_date=    # YYYY-MM-DD  — used to infer recording year
deployment_time=    # HH:MM:SS   — trim start time (defaults to 00:00:00)
retrieval_date=     # YYYY-MM-DD  — trim end date
retrieval_time=     # HH:MM:SS   — trim end time (defaults to 00:00:00)
latitude=           # decimal degrees N — enables sunrise/sunset + active-day stats
longitude=          # decimal degrees E
notes=
```

> **Tip:** populate `latitude`, `longitude`, `deployment_date`, `deployment_time`,
> `retrieval_date`, and `retrieval_time` before downloading data from the device.
> This allows the app to auto-configure coordinates, recording year, and trim
> boundaries without prompting.

---

*ToFnestR was vibe coded with the assistance of [Claude](https://claude.ai)
(Sonnet 4.6, Anthropic) through iterative prompting, testing, and feedback.
The classification algorithms have been reviewed and approved by the developer.*
