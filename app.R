# Training Outreach Dashboard ---------------------------------------------
#
# Where are instructor-led trainings being delivered, and how is each course
# spread across the 10 outreach regions?
#
# Synthetic-data version of a 2023 project. See README.md.

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(bsicons)
  library(shinyWidgets)
  library(leaflet)
  library(sf)
  library(plotly)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(readr)
  library(htmltools)
  library(RColorBrewer)
})

source("R/clean_schedule.R")

# Data --------------------------------------------------------------------

state_regions <- read_csv("data/state_regions.csv", show_col_types = FALSE)
schedule      <- read_schedule("data/training_schedule_raw.csv", state_regions)
catalogue     <- read_course_catalogue("data/training_schedule_raw.csv")

states_sf  <- st_read("data/us_states.geojson", quiet = TRUE)   # AK/HI moved into insets
regions_sf <- st_read("data/regions.geojson", quiet = TRUE)

region_levels  <- paste("Region", 1:10)
course_levels  <- catalogue$course
# Same palette as the 2023 version: Pastel1, stretched to the number of courses
course_colors  <- setNames(colorRampPalette(brewer.pal(9, "Pastel1"))(length(course_levels)),
                           course_levels)
course_choices <- split(catalogue$course,
                        factor(catalogue$category, levels = unique(catalogue$category)))
status_choices <- c("Scheduled", "Approved", "Completed")

map_blues <- brewer.pal(9, "Blues")[3:9]  # skip the palest blues so 1 training is visible
no_training_color <- "#e3e7ec"
vb_layout <- showcase_left_center(width = 0.25, max_height = "64px")

# UI ----------------------------------------------------------------------

theme <- bs_theme(
  version = 5,
  primary = "#1F4E79",
  base_font = font_collection("system-ui", "-apple-system", "Segoe UI", "Roboto",
                              "Helvetica Neue", "Arial", "sans-serif")
)

ui <- page_sidebar(
  title = "Training Outreach Dashboard",
  theme = theme,
  fillable = FALSE,

  sidebar = sidebar(
    width = 320,
    pickerInput(
      "courses", "Courses",
      choices  = course_choices,
      selected = catalogue$course,
      multiple = TRUE,
      options  = pickerOptions(actionsBox = TRUE, liveSearch = TRUE,
                               selectedTextFormat = "count > 2",
                               countSelectedText = "{0} courses selected")
    ),
    checkboxGroupInput("status", "Status", choices = status_choices, selected = status_choices),
    hr(),
    p(class = "small text-muted",
      "The map shows how many trainings each state hosts. Dark outlines mark the 10 outreach regions;",
      "Alaska, Hawaii and Puerto Rico are shown as insets."),
    p(class = "small text-muted",
      strong("Synthetic data."), "A re-creation of a 2023 project. All courses, schedules and notes are fictional.")
  ),

  tags$head(tags$style(HTML("
    .leaflet-container { background: #f4f6f9; }
    .region-label { font-size: 11px; font-weight: 600; color: #1F4E79;
                    text-shadow: 0 0 3px #fff, 0 0 3px #fff; }
  "))),

  # Stack the rows with consistent spacing between them
  div(
    class = "d-flex flex-column gap-4",

    layout_columns(
      fill = FALSE,
      col_widths = c(3, 3, 3, 3),
      gap = "1.5rem",
      value_box("Trainings", textOutput("vb_trainings"), showcase = bs_icon("calendar-check"),
                theme = "primary", showcase_layout = vb_layout),
      value_box("States reached", textOutput("vb_states"), showcase = bs_icon("geo-alt"),
                showcase_layout = vb_layout),
      value_box("Courses delivered", textOutput("vb_courses"), showcase = bs_icon("journal-text"),
                showcase_layout = vb_layout),
      value_box("Regions covered", textOutput("vb_regions"), showcase = bs_icon("diagram-3"),
                showcase_layout = vb_layout)
    ),

    card(
      full_screen = TRUE,
      card_header("Trainings by state"),
      leafletOutput("map", height = 520)
    ),

    card(
      full_screen = TRUE,
      card_header("Trainings by region and course"),
      plotlyOutput("bar", height = 440)
    )
  )
)

# Server ------------------------------------------------------------------

# Draw (or redraw) the state choropleth on a map or map proxy
draw_states <- function(map, by_state) {
  st <- states_sf |> left_join(by_state, by = "state_abbr")
  max_n <- max(c(st$total, 1), na.rm = TRUE)
  pal <- colorNumeric(map_blues, domain = c(1, max(max_n, 2)), na.color = no_training_color)

  labels <- sprintf(
    "<strong>%s</strong> &middot; Region %s<br/>Trainings: %s%s",
    st$state_name, st$region,
    ifelse(is.na(st$total), 0, st$total),
    ifelse(is.na(st$detail), "", paste0("<br/><span style='color:#555'>", st$detail, "</span>"))
  ) |> lapply(HTML)

  map |>
    clearGroup("states") |>
    clearControls() |>
    addPolygons(
      data = st, group = "states",
      fillColor = ~pal(total), fillOpacity = 0.9,
      color = "#ffffff", weight = 0.8,
      label = labels,
      labelOptions = labelOptions(textsize = "13px", direction = "auto",
                                  style = list(padding = "4px 8px")),
      highlightOptions = highlightOptions(weight = 2, color = "#333333"),
      options = pathOptions(pane = "states")
    ) |>
    addLegend("bottomright", pal = pal, values = c(1, max(max_n, 2)), title = "Trainings",
              opacity = 0.9, bins = min(max(max_n, 2), 5)) |>
    addLegend("bottomright", colors = no_training_color, labels = "None", opacity = 0.9)
}

server <- function(input, output, session) {

  filtered <- reactive({
    schedule |> filter(course %in% input$courses, status %in% input$status)
  })

  by_state <- reactive({
    filtered() |>
      group_by(state_abbr, course_code) |>
      summarise(n = sum(n_trainings), .groups = "drop") |>
      arrange(state_abbr, course_code) |>
      group_by(state_abbr) |>
      summarise(total  = sum(n),
                detail = paste0(course_code, " &times; ", n, collapse = "<br/>"),
                .groups = "drop")
  })

  # Value boxes
  output$vb_trainings <- renderText(sum(filtered()$n_trainings))
  output$vb_states    <- renderText(n_distinct(filtered()$state_abbr))
  output$vb_courses   <- renderText(n_distinct(filtered()$course))
  output$vb_regions   <- renderText(paste(n_distinct(filtered()$region), "of 10"))

  # Map: no basemap tiles, so no external tile API or key is needed
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(attributionControl = FALSE, zoomSnap = 0.25, minZoom = 3)) |>
      addMapPane("states", zIndex = 410) |>
      addMapPane("regions", zIndex = 420) |>
      fitBounds(lng1 = -125.5, lat1 = 21, lng2 = -66.5, lat2 = 49.6) |>
      addPolygons(data = regions_sf, fill = FALSE, color = "#1F4E79", weight = 1.6, opacity = 0.85,
                  options = pathOptions(pane = "regions", interactive = FALSE)) |>
      addLabelOnlyMarkers(
        lng = regions_sf$label_lng, lat = regions_sf$label_lat,
        label = paste("Region", regions_sf$region),
        labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, direction = "center",
                                    className = "region-label")
      ) |>
      draw_states(isolate(by_state()))
  })

  # Update the choropleth in place when filters change (keeps zoom/position)
  observeEvent(by_state(), {
    leafletProxy("map") |> draw_states(by_state())
  }, ignoreInit = TRUE)

  # Stacked bar chart by region
  output$bar <- renderPlotly({
    d <- filtered()
    validate(need(nrow(d) > 0, "No trainings match the current filters."))

    plot_data <- d |>
      group_by(region, course) |>
      summarise(n = sum(n_trainings), .groups = "drop") |>
      mutate(region = factor(paste("Region", region), levels = region_levels),
             course = droplevels(factor(course, levels = course_levels)))

    totals <- plot_data |>
      group_by(region) |>
      summarise(total = sum(n), .groups = "drop")

    plot_ly(plot_data, x = ~region, y = ~n, color = ~course, colors = course_colors,
            type = "bar", hovertemplate = "%{x}<br>%{y} training(s)<extra>%{fullData.name}</extra>") |>
      add_text(data = totals, x = ~region, y = ~total, text = ~total, inherit = FALSE,
               textposition = "top center", showlegend = FALSE, hoverinfo = "none",
               textfont = list(color = "#1f2933")) |>
      layout(
        barmode = "stack",
        xaxis = list(title = "", categoryorder = "array", categoryarray = region_levels),
        yaxis = list(title = "Trainings", rangemode = "tozero"),
        legend = list(font = list(size = 11)),
        margin = list(t = 10)
      ) |>
      config(displayModeBar = FALSE)
  })
}

shinyApp(ui, server)
