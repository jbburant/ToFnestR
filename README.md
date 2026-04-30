# ToFnestR

An R pipelines and Shiny application for processing and visualising avian incubation behaviour from nest box sensor data in a semi-automated way. The application ingests raw CSV output from a custom Arduino-based logger (time-of-flight distance sensor + DHT temperature/humidity sensors), classifies female incubation presence/absence using one of as set of tunable algorithms, and supports manual correction of automated classifications.

![screenshot of local app deployment](app_demo.png?raw=true)

---

## Installation

First, clone the repository (or download the ZIP from GitHub and unzip it):

```bash
git clone https://github.com/jburant/ToFnestR.git
cd ToFnestR
```

Then choose a deployment option:

<details>
<summary><strong>Option A — Docker (recommended, no R installation required)</strong></summary>

Docker packages the entire R environment into a container. You only need Docker Desktop — no R, no packages, no configuration.

**Prerequisites (one-time)**

1. Install [Docker Desktop](https://www.docker.com/products/docker-desktop) — download and run the installer
2. **Apple Silicon Macs only (M1/M2/M3):** open Terminal and run:
   ```bash
   softwareupdate --install-rosetta
   ```
3. Open Docker Desktop from Applications and wait for the whale icon in the menu bar to stop animating (~30 seconds)

**First-time setup**

1. Create the data folders inside the repo:
   ```bash
   mkdir raw_data processed_data
   ```
   Place deployment folders inside `raw_data/`. The app reads from there and writes processed outputs to the matching subfolder in `processed_data/`.
   ```
   raw_data/
     DEP_2026_NB01/
       TOF.CSV  DHT.CSV  LOG.CSV  METADATA.TXT
   processed_data/
     DEP_2026_NB01/        ← created automatically on first save
       tof_processed.csv  bout_summary.csv  day_summary.csv  ...
   ```

2. Build and launch (10–20 minutes the first time while packages download):
   ```bash
   docker compose up --build
   ```

3. Open **http://localhost:3838** in your browser.

**Every subsequent launch**

```bash
docker compose up
```

Then open http://localhost:3838. Takes about 10 seconds.

**Stopping the app**

Press `Ctrl+C` in the Terminal window, or:
```bash
docker compose down
```

**Updating when new code is available**

```bash
git pull
docker compose up --build
```

</details>

<details>
<summary><strong>Option B — Local R</strong></summary>

**Prerequisites:** R ≥ 4.2

Restore the package environment from the lockfile:
```r
install.packages("renv")
renv::restore()
```

Launch:
```r
shiny::runApp()
```

Running locally, the app reads and writes to the same deployment folder (no `raw_data`/`processed_data` split needed).

</details>

---

## Device file format

Each deployment folder must contain four files:

| File | Contents |
|------|----------|
| `TOF.CSV` | `timestamp, tof_raw, tof_smooth` — 2 s intervals |
| `DHT.CSV` | `timestamp, temp1, hum1, temp2, hum2` — 30 s intervals |
| `LOG.CSV` | `timestamp, event, device_id, notes` |
| `METADATA.TXT` | Key=value pairs (see below) |

Timestamps use `MM-DD HH:MM:SS` format (no year). The recording year is inferred from `deployment_date` in metadata, or entered on import.

### METADATA.TXT fields

```
device_id=          # matches firmware ID (required)
deployment_id=      # unique deployment label (required)
nestbox=            # nest box identifier
species=            # species name
eggs=               # clutch size
deployment_date=    # YYYY-MM-DD — used to infer recording year
retrieval_date=     # YYYY-MM-DD
latitude=           # decimal degrees N — enables sunrise/sunset and active-day stats
longitude=          # decimal degrees E
notes=
```

---

## Demo

Click **Load demo data** in the sidebar to generate and load a synthetic 5-day deployment (April 2026, Netherlands, blue tit). No files are required. Use **Download this deployment** to export the processed outputs as a ZIP.

---

## Standalone pipeline scripts

The pipeline can also be run outside the app:

```r
source("01_ingest.R")   # raw CSVs → *_clean.csv
source("02_process.R")  # *_clean.csv → *_processed.csv
```

Set `DEPLOYMENT_DIR` at the top of each script to your deployment folder.

---

## For developers — maintaining renv

`renv.lock` pins exact package versions. If you add or update packages:

```r
renv::snapshot()   # update the lockfile
```

Commit `renv.lock`, then rebuild the Docker image:

```bash
docker compose up --build
```

---

## License

GPL-3. See `renv.lock` for the licenses of individual dependencies.
