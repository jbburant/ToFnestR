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

# rocker/tidyverse includes R + the full tidyverse (dplyr, ggplot2, tibble,
# readr, purrr, tidyr, stringr, lubridate, etc.) so renv only needs to
# install our non-standard packages rather than the entire dependency tree.
FROM --platform=linux/amd64 rocker/tidyverse:4.4.1

# ---- System libraries -------------------------------------------------------
RUN apt-get update && apt-get install -y --no-install-recommends \
      curl \
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

# ---- R packages -------------------------------------------------------------
# rocker/tidyverse already includes the full tidyverse (dplyr, ggplot2,
# readr, purrr, tidyr, lubridate, stringr, tibble, etc.) + ggplot2/plotly deps.
# We only need to install the packages specific to this app.
# This layer is cached by Docker and only re-runs if this RUN line changes.
RUN R -e "install.packages( \
      c('shiny', 'shinyFiles', 'bslib', \
        'dygraphs', 'xts', 'plotly', 'DT', \
        'zoo', 'changepoint', 'mclust', 'suncalc', 'zip'), \
      repos = 'https://cloud.r-project.org', \
      Ncpus = parallel::detectCores())"

# ---- Copy application -------------------------------------------------------
WORKDIR /app
COPY . /app

# ---- Runtime ----------------------------------------------------------------
EXPOSE 3838

CMD ["Rscript", "-e", \
     "shiny::runApp('/app', port = 3838, host = '0.0.0.0', launch.browser = FALSE)"]
