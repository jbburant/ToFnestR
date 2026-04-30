# =============================================================================
# Dockerfile — incubeR nest-box incubation monitor
#
# Build:   docker-compose up --build
# Access:  http://localhost:3838
#
# The app is served directly by Shiny (no Shiny Server overhead).
# R packages are installed from renv.lock for exact reproducibility.
# Generate renv.lock locally first: renv::init() then renv::snapshot()
# =============================================================================

FROM rocker/r-ver:4.4.1

# ---- System libraries -------------------------------------------------------
# Required by several R packages (curl, xml, fonts, image processing)
RUN apt-get update && apt-get install -y --no-install-recommends \
      libssl-dev \
      libcurl4-openssl-dev \
      libxml2-dev \
      libfontconfig1-dev \
      libharfbuzz-dev \
      libfribidi-dev \
      libfreetype6-dev \
      libpng-dev \
      libtiff5-dev \
      libjpeg-dev \
      libgit2-dev \
      pandoc \
    && rm -rf /var/lib/apt/lists/*

# ---- renv package restore ---------------------------------------------------
# Copy lockfile first so Docker caches the package install layer.
# Packages are only re-installed when renv.lock changes.
WORKDIR /app

COPY renv.lock renv.lock
RUN R -e "install.packages('renv', repos = 'https://cloud.r-project.org')" \
 && R -e "renv::restore(prompt = FALSE)"

# ---- Copy application -------------------------------------------------------
COPY . /app

# ---- Runtime ----------------------------------------------------------------
EXPOSE 3838

CMD ["Rscript", "-e", \
     "shiny::runApp('/app', port = 3838, host = '0.0.0.0', launch.browser = FALSE)"]
