# ToFnestR

An R pipelines and Shiny application for processing and visualising avian incubation behaviour from nest box sensor data in a semi-automated way. The application ingests raw CSV output from a custom Arduino-based logger (time-of-flight distance sensor + DHT temperature/humidity sensors), classifies female incubation presence/absence using one of as set of tunable algorithms, and supports manual correction of automated classifications.

![screenshot of lccal app deployment](https://github.com/jbburant/ToFnestR/blob/main/app_demo.png)

## Running the app

### Option A — Docker (recommended for shared use, no R installation required)

**Prerequisites:** [Docker Desktop](https://www.docker.com/products/docker-desktop/)

```bash
# First time — generate renv.lock in R (see note below), then:
docker-compose up --build

# Subsequent runs
docker-compose up
```

Open **http://localhost:3838** in your browser.

Place deployment folders inside `./data/` (created next to the repo). They will appear under the **Data** root in the folder picker.

```
data/
  DEP_2026_NB01/
    TOF.CSV  DHT.CSV  LOG.CSV  METADATA.TXT
  DEP_2026_NB02/
    ...
```

To point at a different data directory:
```bash
INCUBER_DATA=/path/to/your/data docker-compose up
```

### Option B — Local R

**Prerequisites:** R ≥ 4.2, then restore the package environment:

```r
install.packages("renv")
renv::restore()   # installs all packages from renv.lock
```

Launch:
```r
shiny::runApp()
```

## First-time setup (generate renv.lock)

`renv.lock` pins exact package versions for reproducibility. Generate it once in your local R session before the first Docker build:

```r
renv::init()       # sets up renv, installs packages
renv::snapshot()   # writes renv.lock
```

Commit `renv.lock` to git. Re-run `renv::snapshot()` whenever you add or update packages.

## Device file format

Each deployment folder must contain:

| File | Contents |
|------|----------|
| `TOF.CSV` | `timestamp, tof_raw, tof_smooth` — 2 s intervals |
| `DHT.CSV` | `timestamp, temp1, hum1, temp2, hum2` — 30 s intervals |
| `LOG.CSV` | `timestamp, event, device_id, notes` |
| `METADATA.TXT` | Key=value pairs: `device_id`, `deployment_date`, `latitude`, `longitude`, etc. |

Timestamps are in `MM-DD HH:MM:SS` format (no year — enter on import or add `deployment_date` to metadata).

## Demo

Click **Load demo data** in the sidebar to generate and load a synthetic 5-day deployment (April 2026, Netherlands blue tit). No files are required.

## Standalone pipeline scripts

The pre-processing pipeline can also be run outside the app:

```r
source("01_ingest.R")   # raw CSVs → *_clean.csv
source("02_process.R")  # *_clean.csv → *_processed.csv
```

Set `DEPLOYMENT_DIR` at the top of each script to your deployment folder.

## License

GPL-3. See dependencies in `renv.lock` for their individual licenses.
