# =============================================================================
# app.R — ToFnestR: Nest Box Incubation Monitor
# =============================================================================

library(shiny)
library(shinyFiles)
library(bslib)
library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(lubridate)
library(stringr)
library(tibble)
library(zoo)
library(changepoint)
library(dygraphs)
library(xts)
library(plotly)
library(DT)
library(suncalc)
library(mclust)
library(zip)

source("R/pipeline_functions.R")
source("R/simulation.R")


# =============================================================================
# CONSTANTS
# =============================================================================

STATE_COLORS <- c(
  present   = "#2166ac",
  absent    = "#d6604d",
  uncertain = "#878787"
)

STATE_SHADING <- c(
  present   = "#d1e5f0",
  absent    = "#fddbc7",
  uncertain = "#e8e8e8"
)

MAX_DISPLAY_PTS <- 8000L


# =============================================================================
# UI
# =============================================================================

ui <- page_sidebar(
  title = tagList(
    "Nest Box Incubation Monitor",
    input_dark_mode(id = "dark_mode", mode = "light")
  ),
  theme = bs_theme(
    bootswatch  = "flatly",
    base_font   = font_google("Roboto Mono"),
    "font-size-base" = "0.88rem"
  ),

  tags$head(tags$style(HTML("
    .nav-tabs .nav-link { color: var(--bs-body-color) !important; opacity: 0.6; }
    .nav-tabs .nav-link.active, .nav-tabs .nav-link:hover {
      color: var(--bs-body-color) !important; opacity: 1; font-weight: 600; }
    .dygraph-axis-label, .dygraph-label {
      color: var(--bs-secondary-color) !important; font-family: inherit !important; }
    .dygraph-legend {
      background: var(--bs-body-bg) !important; color: var(--bs-body-color) !important;
      border: 1px solid var(--bs-border-color) !important; border-radius: 4px; }
    .js-plotly-plot .plotly .main-svg,
    .js-plotly-plot .plotly .main-svg .bg { background: transparent !important; fill: transparent !important; }
    [data-bs-theme='dark'] table.dataTable,
    [data-bs-theme='dark'] table.dataTable td,
    [data-bs-theme='dark'] table.dataTable th {
      background-color: var(--bs-body-bg) !important;
      color: var(--bs-body-color) !important;
      border-color: var(--bs-border-color) !important; }
    [data-bs-theme='dark'] table.dataTable tbody tr:hover td {
      background-color: var(--bs-tertiary-bg) !important; }
    [data-bs-theme='dark'] .dataTables_wrapper,
    [data-bs-theme='dark'] .dataTables_wrapper .dataTables_info,
    [data-bs-theme='dark'] .dataTables_wrapper .dataTables_length label,
    [data-bs-theme='dark'] .dataTables_wrapper .dataTables_filter label,
    [data-bs-theme='dark'] .dataTables_wrapper .dataTables_paginate {
      color: var(--bs-body-color) !important; }
    [data-bs-theme='dark'] .dataTables_wrapper input,
    [data-bs-theme='dark'] .dataTables_wrapper select {
      background-color: var(--bs-body-bg) !important;
      color: var(--bs-body-color) !important;
      border-color: var(--bs-border-color) !important; }
    [data-bs-theme='dark'] .dataTables_wrapper .paginate_button {
      color: var(--bs-body-color) !important; }
    .pagination {
      --bs-pagination-color: var(--bs-body-color);
      --bs-pagination-bg: var(--bs-body-bg);
      --bs-pagination-border-color: var(--bs-border-color);
      --bs-pagination-hover-color: var(--bs-body-color);
      --bs-pagination-hover-bg: var(--bs-tertiary-bg);
      --bs-pagination-hover-border-color: var(--bs-border-color);
      --bs-pagination-focus-color: var(--bs-body-color);
      --bs-pagination-focus-bg: var(--bs-tertiary-bg);
      --bs-pagination-active-color: var(--bs-body-color);
      --bs-pagination-active-bg: var(--bs-secondary-bg);
      --bs-pagination-active-border-color: var(--bs-border-color);
      --bs-pagination-disabled-color: var(--bs-secondary-color);
      --bs-pagination-disabled-bg: var(--bs-body-bg);
      --bs-pagination-disabled-border-color: var(--bs-border-color); }
  "))),

  sidebar = sidebar(
    width = 290,

    tags$h6("Data", class = "text-uppercase text-muted fw-bold mt-1 mb-2"),

    shinyDirButton("import_dir", label = "Select folder",
                   title = "Choose a deployment folder (or top-level for bulk import)",
                   icon = icon("folder-open"),
                   class = "btn-outline-primary btn-sm w-100"),

    checkboxInput("bulk_import", "Multiple deployments (subdirectories)", value = FALSE),

    actionButton("load_demo", label = "Load demo data",
                 icon = icon("flask"), class = "btn-outline-secondary btn-sm w-100 mt-1"),

    uiOutput("import_status_ui"),

    conditionalPanel("output.has_deployments == true",

      hr(class = "my-2"),

      selectInput("selected_dep", label = "Deployment", choices = NULL, width = "100%"),

      hr(class = "my-2"),

      tags$h6("Processing Parameters", class = "text-uppercase text-muted fw-bold mb-2"),

      selectInput("classification_method", label = "Classification method",
                  choices = c("Gaussian mixture (GMM)" = "gmm", "K-means" = "kmeans"),
                  selected = "gmm", width = "100%"),

      tags$label("Smoothing window (s)", class = "form-label mb-0 small"),
      div(class = "d-flex gap-2 align-items-center mb-2",
        div(class = "flex-grow-1",
            sliderInput("smooth_sec", label = NULL, min = 15, max = 300, value = 75, step = 5)),
        div(style = "width:70px; flex-shrink:0;",
            numericInput("smooth_sec_num", label = NULL, value = 75, min = 15, max = 300, step = 5))),

      tags$label("Uncertain zone / confidence threshold", class = "form-label mb-0 small"),
      tags$small(class = "text-muted d-block mb-1",
                 "GMM: 0 = classify all · 1 = only near-certain", tags$br(),
                 "K-means: 0 = hard split · 1 = entire gap uncertain"),
      div(class = "d-flex gap-2 align-items-center mb-2",
        div(class = "flex-grow-1",
            sliderInput("sensitivity", label = NULL, min = 0.0, max = 1.0, value = 0.3, step = 0.05)),
        div(style = "width:70px; flex-shrink:0;",
            numericInput("sensitivity_num", label = NULL, value = 0.3, min = 0.0, max = 1.0, step = 0.05))),

      tags$label("Min. bout duration (s)", class = "form-label mb-0 small"),
      div(class = "d-flex gap-2 align-items-center mb-2",
        div(class = "flex-grow-1",
            sliderInput("min_bout", label = NULL, min = 10, max = 300, value = 60, step = 10)),
        div(style = "width:70px; flex-shrink:0;",
            numericInput("min_bout_num", label = NULL, value = 60, min = 10, max = 300, step = 10))),

      actionButton("reprocess", "Reprocess", icon = icon("rotate"),
                   class = "btn-warning btn-sm w-100 mb-1"),

      hr(class = "my-2"),

      tags$h6("Export", class = "text-uppercase text-muted fw-bold mb-2"),

      downloadButton("download_current", "Download this deployment",
                     icon = icon("download"), class = "btn-primary btn-sm w-100 mb-1"),

      actionButton("export_current", "Save to folder", icon = icon("floppy-disk"),
                   class = "btn-outline-secondary btn-sm w-100 mb-1"),
      actionButton("export_all", "Save all to folder", icon = icon("floppy-disk"),
                   class = "btn-outline-secondary btn-sm w-100")
    )
  ),

  uiOutput("main_ui")
)


# =============================================================================
# SERVER
# =============================================================================

server <- function(input, output, session) {

  # ---------------------------------------------------------------------------
  # Helpers
  # ---------------------------------------------------------------------------

  downsample_ts <- function(df, max_pts = MAX_DISPLAY_PTS) {
    n <- nrow(df)
    if (n <= max_pts) return(df)
    df[as.integer(round(seq(1L, n, length.out = max_pts))), ]
  }

  # Apply dep$trim_start / dep$trim_end to a data frame (export only).
  # The full logical mask is built from the original ts vector before any
  # subsetting, so both conditions are always the same length as df.
  trim_df <- function(df, dep, time_col = "timestamp") {
    ts   <- df[[time_col]]
    keep <- !is.na(ts)
    if (!is.null(dep$trim_start)) keep <- keep & ts >= dep$trim_start
    if (!is.null(dep$trim_end))   keep <- keep & ts <= dep$trim_end
    df[keep, ]
  }

  attach_sun_times <- function(dep, lat, lon) {
    dates <- unique(as_date(dep$tof$timestamp[!is.na(dep$tof$timestamp)]))
    dep$lat       <- lat
    dep$lon       <- lon
    dep$sun_times <- getSunlightTimes(date = dates, lat = lat, lon = lon,
                                       keep = c("sunrise", "sunset"), tz = "UTC")
    dep
  }

  show_coords_modal <- function(dep_id) {
    showModal(modalDialog(
      title = "Site coordinates",
      tags$p("Latitude and longitude enable sunrise/sunset lines and active-day",
             "statistics. You can skip and add them to ", tags$code("METADATA.TXT"), " later."),
      fluidRow(
        column(6, numericInput("lat_input", "Latitude (°N)", value = 52.0, min = -90,  max =  90, step = 0.01)),
        column(6, numericInput("lon_input", "Longitude (°E)", value =  5.0, min = -180, max = 180, step = 0.01))
      ),
      footer = tagList(modalButton("Skip"),
                       actionButton("coords_confirm", "Apply", class = "btn-primary")),
      easyClose = FALSE
    ))
    rv$pending_coords_dep <- dep_id
  }

  plot_colors <- function() {
    dark <- isTRUE(input$dark_mode == "dark")
    list(dark = dark,
         grid = if (dark) "#3d3d3d" else "#e2e2e2",
         axis = if (dark) "#888888" else "#555555",
         text = if (dark) "#cccccc" else "#333333",
         smooth_tof = if (dark) "#e8e8e8" else "#333333",
         bg   = "rgba(0,0,0,0)")
  }

  # ---------------------------------------------------------------------------
  # Reactive storage
  # ---------------------------------------------------------------------------

  rv <- reactiveValues(
    deployments        = list(),
    pending_folder     = NULL,
    pending_coords_dep = NULL,
    year_modal_open    = FALSE
  )

  fs_roots  <- c(Home = path.expand("~"))
  raw_dir   <- Sys.getenv("TOFNESTR_RAW_DIR",       unset = "")
  proc_dir  <- Sys.getenv("TOFNESTR_PROCESSED_DIR", unset = "")
  if (nchar(raw_dir)  > 0 && dir.exists(raw_dir))  fs_roots["Raw data"] <- raw_dir
  if (nchar(proc_dir) == 0) proc_dir <- NULL

  shinyDirChoose(input, "import_dir", roots = fs_roots,
                 filetypes = c("csv", "CSV", "txt", "TXT"))

  # ---------------------------------------------------------------------------
  # Slider sync
  # ---------------------------------------------------------------------------

  observeEvent(input$smooth_sec, {
    if (!isTRUE(all.equal(input$smooth_sec, input$smooth_sec_num)))
      updateNumericInput(session, "smooth_sec_num", value = input$smooth_sec)
  }, ignoreInit = TRUE)
  observeEvent(input$smooth_sec_num, {
    req(!is.na(input$smooth_sec_num))
    val <- max(15, min(300, input$smooth_sec_num))
    if (!isTRUE(all.equal(val, input$smooth_sec)))
      updateSliderInput(session, "smooth_sec", value = val)
  }, ignoreInit = TRUE)

  observeEvent(input$sensitivity, {
    if (!isTRUE(all.equal(input$sensitivity, input$sensitivity_num)))
      updateNumericInput(session, "sensitivity_num", value = input$sensitivity)
  }, ignoreInit = TRUE)
  observeEvent(input$sensitivity_num, {
    req(!is.na(input$sensitivity_num))
    val <- max(0.0, min(1.0, input$sensitivity_num))
    if (!isTRUE(all.equal(val, input$sensitivity)))
      updateSliderInput(session, "sensitivity", value = val)
  }, ignoreInit = TRUE)

  observeEvent(input$min_bout, {
    if (!isTRUE(all.equal(input$min_bout, input$min_bout_num)))
      updateNumericInput(session, "min_bout_num", value = input$min_bout)
  }, ignoreInit = TRUE)
  observeEvent(input$min_bout_num, {
    req(!is.na(input$min_bout_num))
    val <- max(10, min(300, input$min_bout_num))
    if (!isTRUE(all.equal(val, input$min_bout)))
      updateSliderInput(session, "min_bout", value = val)
  }, ignoreInit = TRUE)

  # ---------------------------------------------------------------------------
  # Demo data
  # ---------------------------------------------------------------------------

  observeEvent(input$load_demo, {
    withProgress(message = "Generating demo deployment…", value = 0, {
      incProgress(0.2, detail = "Simulating sensor data")
      tmp <- file.path(tempdir(), "DEMO_2026")
      generate_demo_deployment(tmp, n_days = 5L, seed = 2026L)

      incProgress(0.4, detail = "Running pipeline")
      dep <- tryCatch(
        load_or_process_deployment(tmp, year = 2026L,
                                   smooth_sec = isolate(input$smooth_sec),
                                   sensitivity = isolate(input$sensitivity),
                                   min_bout_sec = isolate(input$min_bout),
                                   classification_method = isolate(input$classification_method)),
        error = function(e) { showNotification(paste("Demo failed:", conditionMessage(e)), type = "error"); NULL }
      )
      if (!is.null(dep)) {
        incProgress(0.4, detail = "Loading")
        rv$deployments[["DEMO_2026"]] <- dep
        updateSelectInput(session, "selected_dep",
                          choices = names(rv$deployments), selected = "DEMO_2026")
        showNotification("Demo deployment loaded.", type = "message", duration = 4)
      }
    })
  })

  # ---------------------------------------------------------------------------
  # Import
  # ---------------------------------------------------------------------------

  observeEvent(input$import_dir, {
    req(is.list(input$import_dir))
    top_dir <- parseDirPath(fs_roots, input$import_dir)
    req(nchar(top_dir) > 0)
    folders <- if (isTRUE(input$bulk_import))
      list.dirs(top_dir, recursive = FALSE, full.names = TRUE) else top_dir
    if (length(folders) == 0) {
      showNotification("No subdirectories found.", type = "warning"); return()
    }
    import_folders(folders)
  })

  import_folders <- function(folders, year = NULL) {
    withProgress(message = "Loading deployments…", value = 0, {
      n <- length(folders)
      walk(seq_along(folders), function(i) {
        folder <- folders[[i]]
        incProgress(1 / n, detail = basename(folder))
        out_folder <- if (!is.null(proc_dir)) file.path(proc_dir, basename(folder)) else NULL

        dep <- tryCatch(
          load_or_process_deployment(folder, year = year,
                                     smooth_sec = isolate(input$smooth_sec),
                                     sensitivity = isolate(input$sensitivity),
                                     min_bout_sec = isolate(input$min_bout),
                                     classification_method = isolate(input$classification_method),
                                     out_folder = out_folder),
          error = function(e) {
            msg <- conditionMessage(e)
            if (startsWith(msg, "recording_year_needed")) {
              rv$pending_folder <- folder; show_year_modal(); NULL
            } else {
              showNotification(paste0("Error loading ", basename(folder), ": ", msg),
                               type = "error", duration = 10); NULL
            }
          }
        )
        if (!is.null(dep)) {
          dep_id <- dep$meta$deployment_id %||% basename(folder)
          meta_lat <- suppressWarnings(as.numeric(dep$meta$latitude  %||% ""))
          meta_lon <- suppressWarnings(as.numeric(dep$meta$longitude %||% ""))
          if (!is.na(meta_lat) && !is.na(meta_lon))
            dep <- attach_sun_times(dep, meta_lat, meta_lon)
          rv$deployments[[dep_id]] <- dep
          if (is.null(dep$sun_times)) show_coords_modal(dep_id)
        }
      })
    })
    choices <- names(rv$deployments)
    updateSelectInput(session, "selected_dep", choices = choices, selected = choices[length(choices)])
  }

  # ---------------------------------------------------------------------------
  # Year modal
  # ---------------------------------------------------------------------------

  show_year_modal <- function() {
    showModal(modalDialog(
      title = "Recording year required",
      tags$p("The metadata file does not include a deployment date. Please enter the year:"),
      numericInput("year_input", "Year", value = as.integer(format(Sys.Date(), "%Y")),
                   min = 2020, max = 2100, step = 1),
      footer = tagList(modalButton("Cancel"), actionButton("year_confirm", "OK", class = "btn-primary")),
      easyClose = FALSE
    ))
  }

  observeEvent(input$year_confirm, {
    removeModal()
    req(rv$pending_folder)
    folder <- rv$pending_folder; rv$pending_folder <- NULL
    dep <- tryCatch(
      load_or_process_deployment(folder, year = as.integer(input$year_input),
                                 smooth_sec = isolate(input$smooth_sec),
                                 sensitivity = isolate(input$sensitivity),
                                 min_bout_sec = isolate(input$min_bout),
                                 classification_method = isolate(input$classification_method),
                                 out_folder = if (!is.null(proc_dir)) file.path(proc_dir, basename(folder)) else NULL),
      error = function(e) { showNotification(paste("Error:", conditionMessage(e)), type = "error", duration = 10); NULL }
    )
    if (!is.null(dep)) {
      dep_id <- dep$meta$deployment_id %||% basename(folder)
      rv$deployments[[dep_id]] <- dep
      choices <- names(rv$deployments)
      updateSelectInput(session, "selected_dep", choices = choices, selected = dep_id)
    }
  })

  # ---------------------------------------------------------------------------
  # Coordinates modal
  # ---------------------------------------------------------------------------

  observeEvent(input$coords_confirm, {
    removeModal()
    dep_id <- rv$pending_coords_dep
    req(dep_id, dep_id %in% names(rv$deployments))
    lat <- input$lat_input; lon <- input$lon_input
    req(!is.na(lat), !is.na(lon))
    rv$deployments[[dep_id]] <- attach_sun_times(rv$deployments[[dep_id]], lat, lon)
    rv$pending_coords_dep <- NULL
    showNotification(paste0("Sun times computed (lat ", round(lat,3), "°, lon ", round(lon,3), "°)"),
                     type = "message", duration = 5)
  })

  # ---------------------------------------------------------------------------
  # Reprocess
  # ---------------------------------------------------------------------------

  observeEvent(input$reprocess, {
    dep_id <- req(input$selected_dep)
    showModal(modalDialog(
      title = "Reprocess with new parameters?",
      "Manual corrections for this deployment will be lost.",
      footer = tagList(modalButton("Cancel"),
                       actionButton("reprocess_confirm", "Reprocess", class = "btn-warning"))
    ))
  })

  observeEvent(input$reprocess_confirm, {
    removeModal()
    dep_id <- req(input$selected_dep)
    folder <- rv$deployments[[dep_id]]$folder
    withProgress(message = "Reprocessing…", {
      dep <- tryCatch(
        process_deployment(folder,
                           smooth_sec = input$smooth_sec, sensitivity = input$sensitivity,
                           min_bout_sec = input$min_bout,
                           lat = rv$deployments[[dep_id]]$lat, lon = rv$deployments[[dep_id]]$lon,
                           classification_method = input$classification_method),
        error = function(e) { showNotification(paste("Error:", conditionMessage(e)), type = "error"); NULL }
      )
      if (!is.null(dep)) rv$deployments[[dep_id]] <- dep
    })
  })

  # ---------------------------------------------------------------------------
  # Current deployment accessors
  # ---------------------------------------------------------------------------

  current_dep <- reactive({
    req(input$selected_dep, input$selected_dep %in% names(rv$deployments))
    rv$deployments[[input$selected_dep]]
  })

  current_tof <- reactive({ req(current_dep()); current_dep()$tof })

  # Trimmed view — used for bouts, summaries, and timeline.
  # The correction plot uses current_tof() so users can still see and correct
  # data outside the trim window (shown as semi-transparent grey).
  current_tof_trimmed <- reactive({
    tof <- current_tof()
    dep <- current_dep()
    if (!is.null(dep$trim_start)) tof <- filter(tof, timestamp >= dep$trim_start)
    if (!is.null(dep$trim_end))   tof <- filter(tof, timestamp <= dep$trim_end)
    tof
  })

  current_dht <- reactive({ req(current_dep()); current_dep()$dht })

  current_dht_trimmed <- reactive({
    dht <- current_dht()
    dep <- current_dep()
    keep <- !is.na(dht$timestamp)
    if (!is.null(dep$trim_start)) keep <- keep & dht$timestamp >= dep$trim_start
    if (!is.null(dep$trim_end))   keep <- keep & dht$timestamp <= dep$trim_end
    dht[keep, ]
  })

  current_bouts_result <- reactive({
    req(current_tof_trimmed())
    recompute_bouts(current_tof_trimmed(), isolate(input$min_bout))
  })

  current_bouts <- reactive({ current_bouts_result()$bouts })

  current_tof_with_bouts <- reactive({
    br <- current_bouts_result()
    br$tof |> left_join(br$bouts |> select(state_run, bout_id), by = "state_run") |> select(-state_run)
  })

  current_day_summary <- reactive({
    req(current_tof_trimmed(), current_bouts())
    dep <- current_dep()
    recompute_summaries(tof = current_tof_trimmed(), bouts = current_bouts(),
                        meta = dep$meta, log_clean = NULL,
                        typical_interval = dep$typical_interval,
                        lat = dep$lat, lon = dep$lon)$day_summary
  })

  # ---------------------------------------------------------------------------
  # UI helpers
  # ---------------------------------------------------------------------------

  output$has_deployments <- reactive({ length(rv$deployments) > 0 })
  outputOptions(output, "has_deployments", suspendWhenHidden = FALSE)

  output$import_status_ui <- renderUI({
    n <- length(rv$deployments)
    if (n == 0) return(NULL)
    tags$small(class = "text-muted", icon("circle-check", class = "text-success"),
               paste(n, "deployment(s) loaded"))
  })

  # ---------------------------------------------------------------------------
  # Main panel
  # ---------------------------------------------------------------------------

  output$main_ui <- renderUI({
    if (length(rv$deployments) == 0) {
      div(class = "d-flex justify-content-center align-items-center", style = "height: 60vh;",
          div(class = "text-center text-muted",
              icon("folder-open", class = "fa-3x mb-3"),
              tags$h5("No data loaded"),
              tags$p("Select a deployment folder using the sidebar.")))
    } else {
      navset_card_tab(

        # ── OVERVIEW ──────────────────────────────────────────────────────────
        nav_panel("Overview",
          card(full_screen = TRUE,
            card_header("TOF distance — full deployment"),
            dygraphOutput("tof_overview", height = "380px"),
            uiOutput("overview_legend")
          )
        ),

        # ── CORRECT STATES ────────────────────────────────────────────────────
        nav_panel("Correct States",
          card(
            card_header(
              div(class = "d-flex align-items-center gap-3 flex-wrap",
                span("Select a region, then assign a state."),
                tags$small(class = "text-muted",
                  "Tip: use the", tags$strong("Box Select"),
                  "tool (dashed rectangle) in the plotly toolbar before dragging."),
                div(class = "ms-auto d-flex align-items-center gap-2",
                  tags$small(class = "text-muted", "Day:"),
                  div(style = "width: 130px;",
                    selectInput("correction_date", label = NULL, choices = NULL, width = "100%")))
              )
            ),
            plotlyOutput("tof_correction", height = "350px"),
            div(class = "d-flex gap-2 mt-2 flex-wrap",
              actionButton("mark_present",   "Mark Present",   icon = icon("circle-check"),
                           class = "btn-sm",
                           style = paste0("background-color:", STATE_COLORS[["present"]],
                                          "; border-color:", STATE_COLORS[["present"]], "; color:#fff;")),
              actionButton("mark_absent",    "Mark Absent",    icon = icon("circle-xmark"),
                           class = "btn-sm",
                           style = paste0("background-color:", STATE_COLORS[["absent"]],
                                          "; border-color:", STATE_COLORS[["absent"]], "; color:#fff;")),
              actionButton("mark_uncertain", "Mark Uncertain", icon = icon("circle-question"),
                           class = "btn-sm",
                           style = paste0("background-color:", STATE_COLORS[["uncertain"]],
                                          "; border-color:", STATE_COLORS[["uncertain"]], "; color:#fff;")),
              actionButton("reset_auto", "Reset to Auto", icon = icon("rotate-left"),
                           class = "btn-outline-secondary btn-sm")
            ),
            uiOutput("selection_info_ui"),
            # ── Trim controls ──────────────────────────────────────────────
            # Trim only affects exported files — raw data is never modified.
            # Pre-populated from deployment/retrieval date+time in METADATA.TXT.
            hr(class = "my-2"),
            div(class = "d-flex gap-2 flex-wrap align-items-end",
              div(
                tags$label("Trim start", class = "form-label mb-1 small text-muted"),
                textInput("trim_start_input", label = NULL,
                          placeholder = "YYYY-MM-DD HH:MM:SS", width = "200px")
              ),
              div(
                tags$label("Trim end", class = "form-label mb-1 small text-muted"),
                textInput("trim_end_input", label = NULL,
                          placeholder = "YYYY-MM-DD HH:MM:SS", width = "200px")
              ),
              actionButton("apply_trim", "Apply trim",
                           icon = icon("scissors"), class = "btn-sm btn-outline-warning mb-1")
            ),
            tags$small(class = "text-muted d-block mt-1",
              icon("circle-info"),
              " Trim boundaries affect bouts, summaries, and exported files.",
              " Data outside the window is shown in grey. Raw sensor data is never modified.")
          )
        ),

        # ── BOUT SUMMARY ──────────────────────────────────────────────────────
        nav_panel("Bout Summary",
          card(
            card_header(
              div(class = "d-flex align-items-center",
                "Incubation bouts",
                div(class = "ms-auto",
                  radioButtons("bout_view", label = NULL,
                               choices = c("Table" = "table", "Timeline" = "timeline"),
                               selected = "table", inline = TRUE))
              )
            ),
            conditionalPanel("input.bout_view === 'table'",  DTOutput("bout_table")),
            conditionalPanel("input.bout_view === 'timeline'", plotlyOutput("bout_timeline", height = "320px"))
          )
        ),

        # ── CLIMATE ───────────────────────────────────────────────────────────
        nav_panel("Climate",
          card(full_screen = TRUE,
            card_header("Temperature (°C)"),
            dygraphOutput("temp_plot", height = "280px")),
          card(full_screen = TRUE,
            card_header("Relative humidity (%)"),
            dygraphOutput("hum_plot", height = "280px"))
        ),

        # ── DEPLOYMENT SUMMARY ────────────────────────────────────────────────
        nav_panel("Deployment Summary",
          layout_columns(col_widths = c(8, 4),
            card(card_header("Daily statistics"), DTOutput("day_table")),
            card(card_header("Device health"),    DTOutput("device_table"))
          )
        ),

        # ── GUIDE ─────────────────────────────────────────────────────────────
        nav_panel(
          title = tagList(icon("circle-question"), "Guide"),
          card(
            card_header("ToFnestR — user guide"),
            div(class = "p-2", style = "max-width: 860px;",
              tags$h5("What this app does"),
              tags$p("Ingests raw CSV data from the nest-box sensor device, applies automated
                     incubation-state classification to the time-of-flight (ToF) distance signal,
                     and lets you review and manually correct classifications before exporting
                     clean, analysis-ready outputs."),
              tags$hr(),
              tags$h5("Workflow"),
              tags$ol(
                tags$li(tags$strong("Import"), " — click ", tags$em("Select folder"),
                  " and choose a deployment folder (", tags$code("TOF.CSV"), ", ",
                  tags$code("DHT.CSV"), ", ", tags$code("LOG.CSV"), ", ", tags$code("METADATA.TXT"),
                  "). Tick ", tags$em("Multiple deployments"), " to load all subfolders at once."),
                tags$li(tags$strong("Provide year and coordinates"), " — if ",
                  tags$code("deployment_date"), ", ", tags$code("latitude"), ", or ",
                  tags$code("longitude"), " are absent from metadata, the app will prompt you."),
                tags$li(tags$strong("Set trim boundaries"), " — in the ", tags$em("Correct States"),
                  " tab, enter the exact deployment start and retrieval end times to exclude",
                  " pre/post-deployment readings from all summaries and exports.",
                  " Pre-populated automatically from ", tags$code("deployment_date"),
                  " + ", tags$code("deployment_time"), " and ",
                  tags$code("retrieval_date"), " + ", tags$code("retrieval_time"),
                  " in metadata (times default to 00:00:00 if absent). Can be entered",
                  " or adjusted manually at any time.",
                  tags$em(" Note: a prompt for missing deployment/retrieval times is planned",
                          " for a future version, consistent with the year and coordinates prompts.")),
                tags$li(tags$strong("Review"), " — use the ", tags$em("Overview"),
                  " tab to inspect the full deployment. Blue shading = present, red = absent.",
                  " Amber/brown dotted lines = sunrise/sunset.",
                  " Blue/red dashed lines = trim window boundaries."),
                tags$li(tags$strong("Correct"), " — switch to ", tags$em("Correct States"),
                  ". Select a day from the dropdown, activate ",
                  tags$strong("Box Select"), " in the plot toolbar (dashed-rectangle icon),",
                  " drag a rectangle over the readings to reassign, then click a state button.",
                  " Data outside the trim window is shown in grey and still correctable,",
                  " but will not appear in exports."),
                tags$li(tags$strong("Review bouts"), " — the ", tags$em("Bout Summary"),
                  " tab shows incubation bouts within the trim window. Toggle between",
                  tags$strong("Table"), " and ", tags$strong("Timeline"), " views.",
                  " The timeline shows each bout as a coloured horizontal segment",
                  " (blue = present, red = absent, grey = uncertain) ordered from",
                  " present at the bottom to absent at the top, with dashed vertical lines",
                  " at state transitions."),
                tags$li(tags$strong("Export"), " — click ", tags$em("Download this deployment"),
                  " to receive a ZIP of corrected, trimmed output files.",
                  " Trim is applied at export time; raw sensor data is never modified.")
              ),
              tags$hr(),
              tags$h5("Processing parameters"),
              tags$dl(
                tags$dt("Smoothing window (s)"),
                tags$dd("Width of the rolling-median filter applied to raw ToF readings before",
                        " state detection. Wider = smoother signal, slower response to rapid",
                        " transitions. Default: 75 s."),
                tags$dt("Uncertain zone / confidence threshold"),
                tags$dd("Controls how confident the algorithm must be before making a hard",
                        " present/absent call. At 0: classify everything; at 1: only near-certain",
                        " segments are labelled — everything else becomes ", tags$em("uncertain"),
                        " (grey). Default: 0.3."),
                tags$dt("Min. bout duration (s)"),
                tags$dd("Bouts shorter than this are flagged ", tags$code("flag_short = TRUE"),
                        " in exports but kept in the data. Default: 60 s.")
              ),
              tags$hr(),
              tags$h5("Classification methods"),
              tags$dl(
                tags$dt("Gaussian mixture (GMM) — recommended"),
                tags$dd("Fits a two-component Gaussian mixture to daytime segment medians.",
                        " Each component has its own mean and variance (absent readings tend",
                        " to be more variable than present ones). Outputs a posterior probability",
                        " per segment; the confidence threshold controls the uncertain band."),
                tags$dt("K-means"),
                tags$dd("Hard assignment to two clusters. Simpler and slightly faster, but",
                        " assumes equal spread in both states.")
              ),
              tags$p(tags$em("Night readings (sunset → sunrise) are always classified as present",
                             " and excluded from state detection when site coordinates are provided.",
                             " This prevents the stable overnight signal from compressing the",
                             " classification threshold for daytime data.")),
              tags$hr(),
              tags$h5("Output files"),
              tags$dl(
                tags$dt(tags$code("tof_processed.csv")),
                tags$dd("Every ToF reading with smoothed distance, state labels (",
                        tags$code("state_auto"), " and ", tags$code("state_corrected"),
                        "), bout ID, and correction source (",
                        tags$code("auto"), ", ", tags$code("manual"), ", or ",
                        tags$code("night_auto"), ")."),
                tags$dt(tags$code("bout_summary.csv")),
                tags$dd("One row per incubation bout within the trim window: start, end,",
                        " duration (s), mean ToF distance, short-bout flag."),
                tags$dt(tags$code("day_summary.csv")),
                tags$dd("One row per calendar day within the trim window: on-nest time,",
                        " off-bout count and mean duration, % 24-h on-nest.",
                        " Also includes % active-day on-nest, sunrise, and sunset when",
                        " site coordinates are available."),
                tags$dt(tags$code("device_summary.csv")),
                tags$dd("Deployment-level device health: downtime, invalid and warmup",
                        " reading rates, reboot count.")
              )
            )
          )
        )
      )
    }
  })

  # ---------------------------------------------------------------------------
  # TAB 1 — Overview
  # ---------------------------------------------------------------------------

  output$tof_overview <- renderDygraph({
    pal <- plot_colors(); tof <- current_tof(); bts <- current_bouts()
    req(nrow(tof) > 0)

    valid_tof <- tof |>
      filter(!flag_tof_invalid, !is.na(timestamp), !is.na(tof_smooth_r)) |>
      select(timestamp, tof_smooth_r) |> downsample_ts()

    tof_xts <- xts(valid_tof |> select(tof_smooth_r), order.by = valid_tof$timestamp)

    dg <- dygraph(tof_xts) |>
      dySeries("tof_smooth_r", label = "Distance — smoothed (mm)",
               color = pal$smooth_tof, strokeWidth = 1.5) |>
      dyOptions(useDataTimezone = TRUE, drawGrid = TRUE,
                gridLineColor = pal$grid, axisLineColor = pal$axis, fillAlpha = 0.05) |>
      dyAxis("x", label = "Time",          axisLabelColor = pal$text) |>
      dyAxis("y", label = "Distance (mm)", axisLabelColor = pal$text) |>
      dyRangeSelector(height = 30) |>
      dyLegend(show = "always", width = 300)

    if (!is.null(bts) && nrow(bts) > 0) {
      shade_bts <- bts |> filter(state_corrected %in% c("absent", "uncertain"), !is.na(bout_start))
      if (nrow(shade_bts) > 0) {
        shading_args <- shade_bts |> select(bout_start, bout_end, state_corrected) |>
          pmap(function(bout_start, bout_end, state_corrected)
            list(from = bout_start, to = bout_end,
                 color = STATE_SHADING[[as.character(state_corrected)]]))
        dg <- reduce(shading_args, function(g, a) g |> dyShading(from=a$from, to=a$to, color=a$color), .init=dg)
      }
    }

    log_path <- file.path(current_dep()$folder, "log_clean.csv")
    if (file.exists(log_path)) {
      log_df <- read_csv(log_path, show_col_types = FALSE) |>
        mutate(timestamp = ymd_hms(timestamp)) |> filter(tolower(event) == "start")
      dg <- reduce(seq_len(nrow(log_df)), function(g, i)
        g |> dyEvent(log_df$timestamp[i], "reboot", labelLoc = "bottom",
                     color = "#cc0000", strokePattern = "dashed"), .init = dg)
    }

    # Sun events
    dg <- add_sun_events(dg)

    # Trim boundary markers
    dep_obj <- current_dep()
    if (!is.null(dep_obj$trim_start))
      dg <- dg |> dyEvent(dep_obj$trim_start, "▶ deploy", labelLoc = "bottom",
                           color = "#2166ac", strokePattern = "dashed")
    if (!is.null(dep_obj$trim_end))
      dg <- dg |> dyEvent(dep_obj$trim_end, "◀ retrieve", labelLoc = "bottom",
                           color = "#d6604d", strokePattern = "dashed")
    dg
  })

  output$overview_legend <- renderUI({
    div(class = "d-flex gap-3 mt-1 small",
      pmap(list(names(STATE_SHADING), STATE_SHADING, names(STATE_COLORS)),
           function(state, bg, fg)
             div(style = paste0("background:", bg, "; border-left: 4px solid ", fg,
                                "; padding: 2px 8px; border-radius: 3px;"),
                 str_to_title(state))))
  })

  # ---------------------------------------------------------------------------
  # TAB 2 — Correct States
  # ---------------------------------------------------------------------------

  # Populate day selector and trim inputs when deployment changes
  observeEvent(current_dep(), {
    dep   <- current_dep()
    tof   <- current_tof()
    dates <- sort(unique(as_date(tof$timestamp[!is.na(tof$timestamp)])))
    updateSelectInput(session, "correction_date",
                      choices = as.character(dates), selected = as.character(dates[1]))
    if (!is.null(dep$trim_start))
      updateTextInput(session, "trim_start_input",
                      value = format(dep$trim_start, "%Y-%m-%d %H:%M:%S"))
    if (!is.null(dep$trim_end))
      updateTextInput(session, "trim_end_input",
                      value = format(dep$trim_end, "%Y-%m-%d %H:%M:%S"))
  })

  # Apply trim — updates dep$trim_start/end reactively, which invalidates
  # current_tof_trimmed() and everything downstream (bouts, summaries)
  observeEvent(input$apply_trim, {
    dep_id <- req(input$selected_dep)
    ts <- suppressWarnings(ymd_hms(input$trim_start_input, quiet = TRUE))
    te <- suppressWarnings(ymd_hms(input$trim_end_input,   quiet = TRUE))
    if (!is.na(ts)) rv$deployments[[dep_id]]$trim_start <- ts
    if (!is.na(te)) rv$deployments[[dep_id]]$trim_end   <- te
    showNotification("Trim boundaries updated.", type = "message", duration = 3)
  })

  selected_range <- reactiveVal(NULL)

  selected_rows <- reactive({
    rng <- selected_range()
    if (is.null(rng)) return(integer(0))
    tof <- current_tof()
    which(!is.na(tof$timestamp) & tof$timestamp >= rng[1] & tof$timestamp <= rng[2])
  })

  output$tof_correction <- renderPlotly({
    pal      <- plot_colors()
    date_sel <- req(input$correction_date)
    dep      <- current_dep()

    # Flag whether each point is within the trim window
    tof <- current_tof() |>
      filter(!flag_tof_invalid, !is.na(tof_smooth_r),
             as_date(timestamp) == as_date(date_sel)) |>
      mutate(
        in_trim = (is.null(dep$trim_start) | timestamp >= dep$trim_start) &
                  (is.null(dep$trim_end)   | timestamp <= dep$trim_end),
        state_corrected = factor(state_corrected, levels = c("present","absent","uncertain")),
        state_label = if_else(
          in_trim,
          str_to_title(replace_na(as.character(state_corrected), "unclassified")),
          "Outside trim"
        )
      ) |> downsample_ts()

    req(nrow(tof) > 0)

    axis_style <- list(color = pal$text, gridcolor = pal$grid, zerolinecolor = pal$grid)

    # Build color map including "Outside trim"
    colors_ext <- c(setNames(unname(STATE_COLORS), str_to_title(names(STATE_COLORS))),
                    "Outside trim" = "#bbbbbb")

    plot_ly(data = tof, x = ~timestamp, y = ~tof_smooth_r,
            type = "scatter", mode = "markers",
            color = ~state_label, colors = colors_ext,
            marker = list(size = 3, opacity = ~if_else(in_trim, 0.75, 0.25)),
            text = ~paste0(format(timestamp, "%H:%M:%S"), "<br>",
                           round(tof_smooth_r, 1), " mm<br>", state_label),
            hoverinfo = "text", source = "correction_plot") |>
      layout(dragmode = "select",
             paper_bgcolor = pal$bg, plot_bgcolor = pal$bg,
             font = list(color = pal$text),
             xaxis = c(axis_style, list(title = "Time")),
             yaxis = c(axis_style, list(title = "Distance (mm)")),
             showlegend = TRUE,
             legend = list(orientation = "h", y = -0.15, font = list(color = pal$text))) |>
      event_register("plotly_selected")
  })

  observeEvent(event_data("plotly_selected", source = "correction_plot"), {
    sel <- event_data("plotly_selected", source = "correction_plot")
    if (!is.data.frame(sel) || nrow(sel) == 0) { selected_range(NULL); return() }
    x <- sel$x
    if (is.character(x)) {
      t_min <- ymd_hms(min(x), quiet = TRUE); t_max <- ymd_hms(max(x), quiet = TRUE)
    } else {
      t_min <- as.POSIXct(min(as.numeric(x)) / 1000, origin = "1970-01-01", tz = "UTC")
      t_max <- as.POSIXct(max(as.numeric(x)) / 1000, origin = "1970-01-01", tz = "UTC")
    }
    if (is.na(t_min) || is.na(t_max)) { selected_range(NULL); return() }
    selected_range(c(t_min, t_max))
  })

  output$selection_info_ui <- renderUI({
    rng <- selected_range()
    if (is.null(rng))
      return(tags$small(class = "text-muted mt-2 d-block",
                        "Draw a box on the plot to select a time window."))
    rows <- selected_rows(); n <- length(rows)
    if (n == 0)
      return(tags$small(class = "text-muted mt-2 d-block", "No readings in selected window."))
    counts <- current_tof()[rows, ] |>
      count(state_corrected, .drop = FALSE) |>
      mutate(label = paste0(str_to_title(as.character(state_corrected)), ": ", n))
    tags$small(class = "text-muted mt-2 d-block",
               paste(n, "readings selected —"), paste(counts$label, collapse = ", "),
               paste0("(", format(rng[1], "%H:%M"), "–", format(rng[2], "%H:%M"), ")"))
  })

  apply_correction <- function(new_state) {
    rows <- selected_rows(); req(length(rows) > 0, input$selected_dep)
    dep_id <- input$selected_dep
    tof_new <- rv$deployments[[dep_id]]$tof
    tof_new$state_corrected[rows] <- factor(new_state, levels = c("present","absent","uncertain"))
    tof_new$correction_source[rows] <- "manual"
    rv$deployments[[dep_id]]$tof <- tof_new
    selected_range(NULL)
  }

  observeEvent(input$mark_present,   { apply_correction("present")   })
  observeEvent(input$mark_absent,    { apply_correction("absent")    })
  observeEvent(input$mark_uncertain, { apply_correction("uncertain") })

  observeEvent(input$reset_auto, {
    rows <- selected_rows(); req(length(rows) > 0, input$selected_dep)
    dep_id <- input$selected_dep
    tof_new <- rv$deployments[[dep_id]]$tof
    tof_new$state_corrected[rows] <- tof_new$state_auto[rows]
    tof_new$correction_source[rows] <- "auto"
    rv$deployments[[dep_id]]$tof <- tof_new
    selected_range(NULL)
  })

  # ---------------------------------------------------------------------------
  # TAB 3 — Climate
  # ---------------------------------------------------------------------------

  make_dht_xts <- function(col_in, col_out) {
    dht <- current_dht_trimmed() |> filter(!is.na(.data[[col_in]]), !is.na(timestamp)) |> downsample_ts()
    req(nrow(dht) > 0)
    xts(dht |> select(all_of(c(col_in, col_out))), order.by = dht$timestamp)
  }

  add_sun_events <- function(dg) {
    sun <- current_dep()$sun_times
    if (is.null(sun) || nrow(sun) == 0) return(dg)
    reduce(seq_len(nrow(sun)), function(g, i)
      g |> dyEvent(sun$sunrise[i], "↑", labelLoc = "top", color = "#e8a020", strokePattern = "dotted") |>
           dyEvent(sun$sunset[i],  "↓", labelLoc = "top", color = "#c06030", strokePattern = "dotted"),
      .init = dg)
  }

  output$temp_plot <- renderDygraph({
    pal <- plot_colors(); dat <- make_dht_xts("temp_in", "temp_out")
    dygraph(dat) |>
      dySeries("temp_in",  label = "Inside (°C)",  color = "#c0663a") |>
      dySeries("temp_out", label = "Outside (°C)", color = "#4e9a6e") |>
      dyOptions(useDataTimezone = TRUE, drawGrid = TRUE, gridLineColor = pal$grid, axisLineColor = pal$axis) |>
      dyAxis("y", label = "Temperature (°C)", axisLabelColor = pal$text) |>
      dyAxis("x", axisLabelColor = pal$text) |>
      dyRangeSelector(height = 20) |> dyLegend(show = "always") |> add_sun_events()
  })

  output$hum_plot <- renderDygraph({
    pal <- plot_colors(); dat <- make_dht_xts("hum_in", "hum_out")
    dygraph(dat) |>
      dySeries("hum_in",  label = "Inside (%)",  color = "#c0663a") |>
      dySeries("hum_out", label = "Outside (%)", color = "#4e9a6e") |>
      dyOptions(useDataTimezone = TRUE, drawGrid = TRUE, gridLineColor = pal$grid, axisLineColor = pal$axis) |>
      dyAxis("y", label = "Relative humidity (%)", axisLabelColor = pal$text) |>
      dyAxis("x", axisLabelColor = pal$text) |>
      dyRangeSelector(height = 20) |> dyLegend(show = "always") |> add_sun_events()
  })

  # ---------------------------------------------------------------------------
  # TAB 4 — Bout Summary
  # ---------------------------------------------------------------------------

  output$bout_table <- renderDT({
    bouts <- current_bouts() |>
      transmute(bout_id = as.integer(bout_id),
                state        = str_to_title(as.character(state_corrected)),
                start        = format(bout_start, "%Y-%m-%d %H:%M"),
                end          = format(bout_end,   "%Y-%m-%d %H:%M"),
                duration_min = as.numeric(round(bout_duration / 60, 1)),
                n_readings   = as.integer(n_readings),
                mean_tof_mm  = as.numeric(mean_tof_mm),
                flag_short   = as.logical(flag_short))

    js_colors <- paste0("{'Present':'", STATE_SHADING[["present"]], "',",
                        " 'Absent':'",  STATE_SHADING[["absent"]],  "',",
                        " 'Uncertain':'",STATE_SHADING[["uncertain"]],"'}")

    datatable(bouts, rownames = FALSE,
              colnames = c("ID","State","Start","End","Duration (min)","Readings","Mean ToF (mm)","Short?"),
              options = list(pageLength = 20, scrollX = TRUE,
                             rowCallback = JS(sprintf(
                               "function(row,data){var c=%s;var s=data[1];
                                if(c[s])$('td:eq(1)',row).css('background-color',c[s]);}", js_colors))))
  })

  # Bout timeline — horizontal coloured segments, one per bout.
  # Y-axis: absent (top) → uncertain → present (bottom), mirroring the
  # TOF signal (lower distance = female closer to sensor = present).
  output$bout_timeline <- renderPlotly({
    pal   <- plot_colors()
    bouts <- current_bouts() |>
      filter(!is.na(state_corrected)) |>
      mutate(
        state_chr  = as.character(state_corrected),
        # Numeric y so plotly doesn't struggle with categorical segments
        state_y    = case_when(state_chr == "present"   ~ 0,
                               state_chr == "uncertain" ~ 1,
                               state_chr == "absent"    ~ 2,
                               TRUE ~ NA_real_),
        hover_text = paste0(str_to_title(state_chr), "<br>",
                            format(bout_start, "%H:%M"), " – ",
                            format(bout_end,   "%H:%M"), "<br>",
                            round(bout_duration / 60, 1), " min")
      )

    req(nrow(bouts) > 0)

    p <- plot_ly() |>
      layout(paper_bgcolor = pal$bg, plot_bgcolor = pal$bg,
             font  = list(color = pal$text),
             xaxis = list(title = "Time", color = pal$text, gridcolor = pal$grid),
             yaxis = list(title = "",
                          tickmode = "array",
                          tickvals = c(0, 1, 2),
                          ticktext = c("Present", "Uncertain", "Absent"),
                          range = c(-0.5, 2.5),
                          color = pal$text),
             showlegend = FALSE)

    for (st in c("present", "uncertain", "absent")) {
      b <- filter(bouts, state_chr == st)
      if (nrow(b) == 0) next
      y_val <- unique(b$state_y)
      p <- p |> add_segments(data = b,
                              x = ~bout_start, xend = ~bout_end,
                              y = y_val, yend = y_val,
                              line = list(color = STATE_COLORS[[st]], width = 20),
                              text = ~hover_text, hoverinfo = "text")
    }

    # Dashed vertical lines at state transitions
    transition_ms <- as.numeric(sort(unique(bouts$bout_start[-1]))) * 1000
    if (length(transition_ms) > 0) {
      shapes <- lapply(transition_ms, function(t)
        list(type = "line", x0 = t, x1 = t, y0 = -0.5, y1 = 2.5,
             xref = "x", yref = "y",
             line = list(color = "rgba(128,128,128,0.35)", dash = "dash", width = 1)))
      p <- p |> layout(shapes = shapes)
    }
    p
  })

  # ---------------------------------------------------------------------------
  # TAB 5 — Deployment Summary
  # ---------------------------------------------------------------------------

  output$day_table <- renderDT({
    ds <- current_day_summary() |> mutate(on_nest_h = round(on_nest_sec / 3600, 2))
    has_sun <- "pct_active_day_on_nest" %in% names(ds)
    if (has_sun) {
      ds |> mutate(sunrise_local = format(sunrise, "%H:%M"), sunset_local = format(sunset, "%H:%M")) |>
        select(date, sunrise_local, sunset_local, active_day_hr, pct_active_day_on_nest,
               pct_day_on_nest, on_nest_h, n_off_bouts, mean_off_bout_min) |>
        datatable(rownames = FALSE, options = list(pageLength = 31, scrollX = TRUE),
                  colnames = c("Date","Sunrise","Sunset","Active day (h)","% active day on-nest",
                               "% 24-h on-nest","On-nest (h)","Off-bouts (n)","Mean off-bout (min)"))
    } else {
      ds |> mutate(off_nest_h = round(off_nest_sec/3600,2), uncertain_h = round(uncertain_sec/3600,2)) |>
        select(date, pct_day_on_nest, on_nest_h, off_nest_h, uncertain_h, n_off_bouts, mean_off_bout_min) |>
        datatable(rownames = FALSE, options = list(pageLength = 31, scrollX = TRUE),
                  colnames = c("Date","% 24-h on-nest","On-nest (h)","Off-nest (h)",
                               "Uncertain (h)","Off-bouts (n)","Mean off-bout (min)"))
    }
  })

  output$device_table <- renderDT({
    dep <- current_dep(); req(dep)
    dev <- dep$device_summary |>
      mutate(first_timestamp = format(first_timestamp, "%Y-%m-%d %H:%M"),
             last_timestamp  = format(last_timestamp,  "%Y-%m-%d %H:%M")) |>
      mutate(across(everything(), as.character)) |>
      tidyr::pivot_longer(everything(), names_to = "Metric", values_to = "Value") |>
      mutate(Metric = str_replace_all(Metric, "_", " ") |> str_to_sentence())
    datatable(dev, rownames = FALSE, options = list(dom = "t", pageLength = 50, scrollX = TRUE))
  })

  # ---------------------------------------------------------------------------
  # Export
  # ---------------------------------------------------------------------------

  # Core export — trim applied here so outputs only contain deployment window
  save_deployment <- function(dep, min_bout_sec = 60) {
    tof_trimmed <- trim_df(dep$tof, dep)
    bout_res    <- recompute_bouts(tof_trimmed, min_bout_sec)
    bouts       <- bout_res$bouts
    summaries   <- recompute_summaries(tof = bout_res$tof, bouts = bouts,
                                        meta = dep$meta, log_clean = NULL,
                                        typical_interval = dep$typical_interval,
                                        lat = dep$lat, lon = dep$lon)
    tof_out <- bout_res$tof |>
      left_join(bouts |> select(state_run, bout_id), by = "state_run") |>
      select(-state_run)

    folder <- dep$folder
    write_csv(tof_out,                     file.path(folder, "tof_processed.csv"))
    write_csv(bouts |> select(-state_run), file.path(folder, "bout_summary.csv"))
    write_csv(summaries$day_summary,       file.path(folder, "day_summary.csv"))
    write_csv(summaries$device_summary,    file.path(folder, "device_summary.csv"))
    write_csv(trim_df(dep$dht, dep),       file.path(folder, "dht_processed.csv"))
    folder
  }

  observeEvent(input$export_current, {
    dep_id <- req(input$selected_dep)
    tryCatch({
      folder <- save_deployment(rv$deployments[[dep_id]], isolate(input$min_bout))
      showNotification(paste0("Saved to: ", folder), type = "message", duration = 6)
    }, error = function(e) showNotification(paste("Export failed:", conditionMessage(e)), type = "error"))
  })

  observeEvent(input$export_all, {
    n_saved <- 0L
    walk(names(rv$deployments), function(id) {
      tryCatch({
        save_deployment(rv$deployments[[id]], isolate(input$min_bout)); n_saved <<- n_saved + 1L
      }, error = function(e)
        showNotification(paste("Error exporting", id, ":", conditionMessage(e)), type = "error"))
    })
    showNotification(paste0(n_saved, " deployment(s) saved."), type = "message", duration = 6)
  })

  # Download as ZIP — trim applied, streams to browser downloads folder
  output$download_current <- downloadHandler(
    filename = function() paste0(req(input$selected_dep), "_", format(Sys.Date(), "%Y%m%d"), ".zip"),
    content  = function(file) {
      dep_id <- req(input$selected_dep)
      dep    <- rv$deployments[[dep_id]]

      tof_trimmed <- trim_df(dep$tof, dep)
      min_bout    <- isolate(input$min_bout)
      bout_res    <- recompute_bouts(tof_trimmed, min_bout)
      bouts       <- bout_res$bouts
      summaries   <- recompute_summaries(tof = bout_res$tof, bouts = bouts,
                                          meta = dep$meta, log_clean = NULL,
                                          typical_interval = dep$typical_interval,
                                          lat = dep$lat, lon = dep$lon)
      tof_out <- bout_res$tof |>
        left_join(bouts |> select(state_run, bout_id), by = "state_run") |>
        select(-state_run)

      tmp <- file.path(tempdir(), paste0(dep_id, "_export"))
      dir.create(tmp, showWarnings = FALSE, recursive = TRUE)

      write_csv(tof_out,                     file.path(tmp, "tof_processed.csv"))
      write_csv(bouts |> select(-state_run), file.path(tmp, "bout_summary.csv"))
      write_csv(summaries$day_summary,       file.path(tmp, "day_summary.csv"))
      write_csv(summaries$device_summary,    file.path(tmp, "device_summary.csv"))
      write_csv(trim_df(dep$dht, dep),       file.path(tmp, "dht_processed.csv"))

      meta_src <- file.path(dep$folder, "METADATA.TXT")
      if (file.exists(meta_src))
        file.copy(meta_src, file.path(tmp, "METADATA.TXT"), overwrite = TRUE)

      zip::zipr(file, files = list.files(tmp, full.names = TRUE))
    }
  )

}

shinyApp(ui, server)
