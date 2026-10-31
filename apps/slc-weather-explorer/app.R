# SLC Airport & Weather Station Explorer
# 10.20.2026 map and availability heatmaps
#
# Necessary runtime packages:
# bslib, plotly, RColorBrewer, shiny
#
# App data: data/weather_app_data.rds and data/airport_index.rds.

# --------------------------------------------------
# App-level display constants
# --------------------------------------------------

floating_panel_top <- 33
floating_panel_side <- 7
side_col_width <- 3
main_col_width <- 9

note_style <- paste(
  "font-size: 0.8rem;",
  "margin-top: 0;",
  "margin-bottom: 0;",
  "color: var(--bs-secondary-color);"
)

# The five reporting colors use alternate YlOrRd swatches.
heatmap_colors <- stats::setNames(
  RColorBrewer::brewer.pal(9L, "YlOrRd")[
    base::c(1L, 3L, 5L, 7L, 9L)
  ],
  base::c("zero", "low", "mid", "high", "full")
)

make_heatmap_key <- function(label, color) {
  shiny::tags$div(
    style = base::paste(
      "display: flex;",
      "align-items: flex-start;",
      "gap: 0.45rem;",
      "margin-bottom: 0.45rem;"
    ),
    shiny::tags$span(
      `aria-hidden` = "true",
      style = base::paste0(
        "display: inline-block;",
        "flex: 0 0 0.9rem;",
        "height: 0.9rem;",
        "margin-top: 0.1rem;",
        "background-color: ", color, ";",
        "border: 1px solid var(--bs-secondary-color);"
      )
    ),
    shiny::tags$span(label)
  )
}

make_heatmap_legend <- function(time_scope) {
  if (base::identical(time_scope, "all")) {
    legend_title <- "GHCNh element reporting"
    legend_labels <- base::c(
      "No reported values",
      "At least one reported value"
    )
    legend_colors <- base::unname(
      heatmap_colors[base::c("zero", "full")]
    )
  } else if (base::identical(time_scope, "single")) {
    legend_title <- "Days with reported values (%)"
    legend_labels <- base::c(
      "0%",
      "Above 0% to 33 1/3%",
      "Above 33 1/3% to 66 2/3%",
      "Above 66 2/3% to below 100%",
      "100%"
    )
    legend_colors <- base::unname(heatmap_colors)
  } else {
    base::stop("Unknown time scope.", call. = FALSE)
  }

  legend_items <- base::Map(
    make_heatmap_key,
    legend_labels,
    legend_colors
  )

  shiny::tags$aside(
    `aria-label` = "Heatmap legend",
    style = base::paste(
      "flex: 0 0 215px;",
      "width: 215px;",
      "align-self: flex-start;",
      "font-size: 0.82rem;",
      "line-height: 1.25;"
    ),
    shiny::tags$div(
      legend_title,
      style = "font-weight: 600; margin-bottom: 0.5rem;"
    ),
    shiny::tagList(legend_items),
    if (base::identical(time_scope, "all")) {
      shiny::tags$div(
        style = base::paste(
          "border-top: 1px solid var(--bs-border-color);",
          "padding-top: 0.45rem;",
          "margin-top: 0.35rem;"
        ),
        make_heatmap_key(
          "No service to/from SLC in this period",
          "#E5E7EB"
        )
      )
    }
  )
}

make_heatmap_display <- function(
  output_id,
  plot_width,
  time_scope
) {
  plot_style <- if (base::identical(
    time_scope, "single"
  )) {
    "flex: 0 1 420px; min-width: 0; overflow-x: auto;"
  } else {
    "flex: 1 1 0; min-width: 0; overflow-x: auto;"
  }

  shiny::div(
    style = base::paste(
      "display: flex;",
      "align-items: flex-start;",
      "gap: 0.85rem;",
      "width: 100%;"
    ),
    shiny::div(
      class = "weather-scroll-area",
      style = plot_style,
      plotly::plotlyOutput(
        outputId = output_id,
        width = plot_width,
        height = "1100px"
      )
    ),
    make_heatmap_legend(time_scope)
  )
}

make_scroll_note <- function(output_id, time_level) {
  message <- if (base::identical(time_level, "month")) {
    "Scroll horizontally to inspect individual months."
  } else if (base::identical(time_level, "year")) {
    "Scroll horizontally to inspect individual years."
  } else {
    base::stop("Unknown time level.", call. = FALSE)
  }

  shiny::tags$p(
    message,
    class = "weather-scroll-note",
    `data-plot-id` = output_id,
    style = base::paste(note_style, "display: none;")
  )
}

# --------------------------------------------------
# Prepared-data validation
# --------------------------------------------------

check_app_data <- function(app_data) {
  required_names <- c(
    "schema_version",
    "service_months",
    "latest_airports",
    "airport_periods",
    "station_periods",
    "element_periods",
    "metadata"
  )

  missing_names <- base::setdiff(
    required_names,
    base::names(app_data)
  )

  if (base::length(missing_names) > 0L) {
    base::stop(
      base::paste0(
        "Prepared app data is missing: ",
        base::paste(missing_names, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }

  if (!base::identical(
    app_data$schema_version,
    4L
  )) {
    base::stop(
      "Prepared app data must use schema version 4.",
      call. = FALSE
    )
  }

  service_months <- app_data$service_months
  latest_airports <- app_data$latest_airports
  airport_periods <- app_data$airport_periods
  metadata <- app_data$metadata

  if (!base::is.data.frame(service_months) ||
    !base::is.data.frame(latest_airports) ||
    !base::is.data.frame(airport_periods) ||
    !base::is.list(metadata)) {
    base::stop(
      "Prepared app data has an invalid deployment structure.",
      call. = FALSE
    )
  }

  service_names <- c(
    "service_month_id",
    "ServiceMonth",
    "AirportID",
    "AirportCode",
    "AirportName",
    "AirportCity"
  )

  latest_names <- c(
    "AirportID",
    "LatestAirportCode",
    "LatestAirportName"
  )

  period_names <- c(
    "airport_period_id",
    "service_month_id",
    "year",
    "month",
    "service_month",
    "airport_id",
    "airport_seq_id",
    "airport_code",
    "airport_name",
    "airport_city",
    "airport_latitude",
    "airport_longitude",
    "station_id",
    "station_name",
    "station_icao",
    "station_latitude",
    "station_longitude",
    "station_elevation",
    "screen_distance_km",
    "period_start",
    "period_end"
  )

  metadata_names <- c(
    "first_service_month",
    "last_service_month"
  )

  if (!base::all(
    service_names %in% base::names(service_months)
  ) ||
    !base::all(
      latest_names %in% base::names(latest_airports)
    ) ||
    !base::all(
      period_names %in% base::names(airport_periods)
    ) ||
    !base::all(
      metadata_names %in% base::names(metadata)
    )) {
    base::stop(
      "Prepared app data lacks required airport fields.",
      call. = FALSE
    )
  }

  if (base::nrow(service_months) == 0L ||
    base::anyNA(service_months$ServiceMonth) ||
    base::anyNA(service_months$AirportID) ||
    base::anyNA(service_months$AirportCode) ||
    base::anyNA(service_months$AirportName) ||
    base::any(!base::nzchar(
      base::trimws(service_months$AirportName)
    )) ||
    base::anyNA(service_months$AirportCity) ||
    !base::inherits(service_months$ServiceMonth, "Date")) {
    base::stop(
      "Prepared service-month data is invalid.",
      call. = FALSE
    )
  }

  if (base::nrow(latest_airports) == 0L ||
    base::anyNA(latest_airports$AirportID) ||
    base::anyNA(latest_airports$LatestAirportCode) ||
    base::anyNA(latest_airports$LatestAirportName) ||
    base::any(!base::nzchar(
      base::trimws(
        latest_airports$LatestAirportCode
      )
    )) ||
    base::any(!base::nzchar(
      base::trimws(
        latest_airports$LatestAirportName
      )
    ))) {
    base::stop(
      "Prepared latest-airport data is invalid.",
      call. = FALSE
    )
  }

  if (base::anyDuplicated(
    latest_airports$AirportID
  ) > 0L) {
    base::stop(
      "Prepared latest-airport data has duplicate AirportIDs.",
      call. = FALSE
    )
  }

  if (!base::setequal(
    latest_airports$AirportID,
    base::unique(
      service_months$AirportID
    )
  )) {
    base::stop(
      paste(
        "Prepared latest-airport and service-airport",
        "populations disagree."
      ),
      call. = FALSE
    )
  }

  if (base::nrow(airport_periods) == 0L ||
    base::anyNA(airport_periods$service_month) ||
    base::anyNA(airport_periods$airport_id) ||
    base::anyNA(airport_periods$airport_seq_id) ||
    base::anyNA(airport_periods$airport_code) ||
    base::anyNA(airport_periods$airport_name) ||
    base::anyNA(airport_periods$airport_city) ||
    base::anyNA(airport_periods$airport_latitude) ||
    base::anyNA(airport_periods$airport_longitude) ||
    base::anyNA(airport_periods$station_id) ||
    base::anyNA(airport_periods$station_name) ||
    base::anyNA(airport_periods$station_latitude) ||
    base::anyNA(airport_periods$station_longitude) ||
    base::anyNA(airport_periods$period_start) ||
    base::anyNA(airport_periods$period_end) ||
    !base::inherits(airport_periods$service_month, "Date") ||
    !base::inherits(airport_periods$period_start, "Date") ||
    !base::inherits(airport_periods$period_end, "Date") ||
    base::any(
      airport_periods$period_start >
        airport_periods$period_end
    )) {
    base::stop(
      "Prepared airport-period map data are invalid.",
      call. = FALSE
    )
  }

  first_month <- base::min(service_months$ServiceMonth)
  last_month <- base::max(service_months$ServiceMonth)

  if (!base::identical(
    first_month,
    metadata$first_service_month
  ) || !base::identical(
    last_month,
    metadata$last_service_month
  )) {
    base::stop(
      "Prepared app-data metadata disagrees with the service range.",
      call. = FALSE
    )
  }

  base::invisible(TRUE)
}

# --------------------------------------------------
# Year-level reporting-availability heatmap
# --------------------------------------------------

make_year_heatmap <- function(
  index_data,
  airport_id,
  airport_code,
  years,
  source_id = "weather_availability"
) {
  families <- index_data$elements
  n_families <- base::length(families)
  n_years <- base::length(years)

  status <- base::matrix(
    0L,
    nrow = n_families,
    ncol = n_years
  )

  rows <- index_data$family_years
  rows <- rows[
    rows$airport_id == airport_id, ,
    drop = FALSE
  ]

  if (base::nrow(rows) > 0L) {
    family_row <- base::match(rows$element, families)
    year_col <- base::match(rows$year, years)

    keys <- base::paste(
      family_row,
      year_col,
      sep = "|"
    )

    if (base::anyNA(family_row) ||
      base::anyNA(year_col) ||
      base::anyDuplicated(keys) > 0L ||
      base::any(rows$n_applicable_days <= 0L) ||
      base::any(rows$n_days_with_values < 0L) ||
      base::any(
        rows$n_days_with_values > rows$n_applicable_days
      )) {
      base::stop(
        "Invalid airport-year reporting records.",
        call. = FALSE
      )
    }

    status[base::cbind(family_row, year_col)] <-
      1L + base::as.integer(
        rows$n_days_with_values > 0L
      )
  }

  status_labels <- base::c(
    "No service to/from SLC in this period.",
    base::paste0(
      "Service to/from SLC in this period; ",
      "no reported values for this GHCNh element."
    ),
    base::paste0(
      "Service to/from SLC in this period; ",
      "at least one reported value for this GHCNh element."
    )
  )

  hover <- base::matrix(
    base::paste0(
      airport_code,
      "<br>",
      base::rep(families, times = n_years),
      "<br>",
      base::rep(years, each = n_families),
      "<br>",
      status_labels[base::as.vector(status) + 1L],
      "<br><i>Click for details</i>"
    ),
    nrow = n_families,
    ncol = n_years
  )

  color_scale <- base::list(
    base::c(0, "#E5E7EB"),
    base::c(0.249, "#E5E7EB"),
    base::c(0.25, heatmap_colors[["zero"]]),
    base::c(0.749, heatmap_colors[["zero"]]),
    base::c(0.75, heatmap_colors[["full"]]),
    base::c(1, heatmap_colors[["full"]])
  )

  plotly::plot_ly(
    x = base::as.character(years),
    y = families,
    z = status,
    text = hover,
    type = "heatmap",
    colorscale = color_scale,
    zmin = 0,
    zmax = 2,
    showscale = FALSE,
    hoverinfo = "text",
    xgap = 1,
    ygap = 1,
    source = source_id
  ) |>
    plotly::layout(
      margin = base::list(
        l = 80,
        r = 20,
        t = 20,
        b = 90
      ),
      xaxis = base::list(
        title = "Year",
        type = "category",
        tickmode = "array",
        tickvals = base::as.character(years),
        ticktext = base::as.character(years),
        tickangle = -45,
        fixedrange = TRUE
      ),
      yaxis = base::list(
        title = "",
        type = "category",
        autorange = "reversed",
        fixedrange = TRUE
      )
    )
}

# --------------------------------------------------
# Month-level reporting-availability heatmap
# --------------------------------------------------

make_month_heatmap <- function(
  index_data,
  airport_id,
  airport_code,
  service_months,
  source_id = "weather_availability"
) {
  families <- index_data$elements
  n_families <- base::length(families)
  n_months <- base::length(service_months)

  month_labels <- base::format(
    service_months,
    "%Y-%m"
  )

  status <- base::matrix(
    0L,
    nrow = n_families,
    ncol = n_months
  )

  rows <- index_data$family_months
  rows <- rows[
    rows$airport_id == airport_id, ,
    drop = FALSE
  ]

  if (base::nrow(rows) > 0L) {
    family_row <- base::match(
      rows$element,
      families
    )

    month_col <- base::match(
      rows$service_month,
      service_months
    )

    keys <- base::paste(
      family_row,
      month_col,
      sep = "|"
    )

    if (base::anyNA(family_row) ||
      base::anyNA(month_col) ||
      base::anyDuplicated(keys) > 0L ||
      base::any(rows$n_applicable_days <= 0L) ||
      base::any(rows$n_days_with_values < 0L) ||
      base::any(
        rows$n_days_with_values > rows$n_applicable_days
      )) {
      base::stop(
        "Invalid airport-month reporting records.",
        call. = FALSE
      )
    }

    status[base::cbind(family_row, month_col)] <-
      1L + base::as.integer(
        rows$n_days_with_values > 0L
      )
  }

  status_labels <- base::c(
    "No service to/from SLC in this period.",
    base::paste0(
      "Service to/from SLC in this period; ",
      "no reported values for this GHCNh element."
    ),
    base::paste0(
      "Service to/from SLC in this period; ",
      "at least one reported value for this GHCNh element."
    )
  )

  month_names <- base::format(
    service_months,
    "%B %Y"
  )

  hover <- base::matrix(
    base::paste0(
      airport_code,
      "<br>",
      base::rep(families, times = n_months),
      "<br>",
      base::rep(month_names, each = n_families),
      "<br>",
      status_labels[base::as.vector(status) + 1L],
      "<br><i>Click for details</i>"
    ),
    nrow = n_families,
    ncol = n_months
  )

  color_scale <- base::list(
    base::c(0, "#E5E7EB"),
    base::c(0.249, "#E5E7EB"),
    base::c(0.25, heatmap_colors[["zero"]]),
    base::c(0.749, heatmap_colors[["zero"]]),
    base::c(0.75, heatmap_colors[["full"]]),
    base::c(1, heatmap_colors[["full"]])
  )

  plotly::plot_ly(
    x = month_labels,
    y = families,
    z = status,
    text = hover,
    type = "heatmap",
    colorscale = color_scale,
    zmin = 0,
    zmax = 2,
    showscale = FALSE,
    hoverinfo = "text",
    xgap = 1,
    ygap = 1,
    source = source_id
  ) |>
    plotly::layout(
      margin = base::list(
        l = 80,
        r = 20,
        t = 20,
        b = 90
      ),
      xaxis = base::list(
        title = "Service month",
        type = "category",
        tickmode = "array",
        tickvals = month_labels,
        ticktext = base::format(service_months, "%b %Y"),
        tickangle = -45,
        fixedrange = TRUE
      ),
      yaxis = base::list(
        title = "",
        type = "category",
        autorange = "reversed",
        fixedrange = TRUE
      )
    )
}

# --------------------------------------------------
# Single-period percentage-availability heatmap
# --------------------------------------------------

make_single_heatmap <- function(
  rows,
  families,
  airport_code,
  period_label,
  source_id = "weather_availability"
) {
  n_families <- base::length(families)
  family_row <- base::match(families, rows$element)

  if (base::nrow(rows) != n_families ||
    base::anyNA(family_row) ||
    base::anyDuplicated(rows$element) > 0L ||
    base::anyNA(rows$n_days_with_values) ||
    base::anyNA(rows$n_applicable_days) ||
    base::any(rows$n_applicable_days <= 0L) ||
    base::any(rows$n_days_with_values < 0L) ||
    base::any(
      rows$n_days_with_values > rows$n_applicable_days
    ) ||
    base::length(base::unique(rows$n_applicable_days)) != 1L) {
    base::stop(
      "Invalid single-period reporting records.",
      call. = FALSE
    )
  }

  rows <- rows[family_row, , drop = FALSE]
  reported <- rows$n_days_with_values
  applicable <- rows$n_applicable_days

  # Endpoints are distinct; the open interior is divided into thirds.
  category <- base::integer(n_families)
  category[reported == 0L] <- 1L
  category[reported > 0L & 3L * reported <= applicable] <- 2L
  category[
    3L * reported > applicable &
      3L * reported <= 2L * applicable
  ] <- 3L
  category[
    3L * reported > 2L * applicable &
      reported < applicable
  ] <- 4L
  category[reported == applicable] <- 5L

  if (base::any(category == 0L)) {
    base::stop(
      "Reporting percentages have unclassified values.",
      call. = FALSE
    )
  }

  reporting_pct <- 100 * reported / applicable

  hover <- base::matrix(
    base::paste0(
      airport_code,
      "<br>",
      families,
      "<br>",
      period_label,
      "<br>",
      base::sprintf(
        "Days with reported values: %.2f%% (%d of %d days in service periods)",
        reporting_pct,
        reported,
        applicable
      ),
      "<br><i>Click for details</i>"
    ),
    nrow = n_families,
    ncol = 1L
  )

  color_scale <- base::list(
    base::c(0, "#E5E7EB"),
    base::c(0.0999, "#E5E7EB"),
    base::c(0.1, heatmap_colors[["zero"]]),
    base::c(0.2999, heatmap_colors[["zero"]]),
    base::c(0.3, heatmap_colors[["low"]]),
    base::c(0.4999, heatmap_colors[["low"]]),
    base::c(0.5, heatmap_colors[["mid"]]),
    base::c(0.6999, heatmap_colors[["mid"]]),
    base::c(0.7, heatmap_colors[["high"]]),
    base::c(0.8999, heatmap_colors[["high"]]),
    base::c(0.9, heatmap_colors[["full"]]),
    base::c(1, heatmap_colors[["full"]])
  )

  plotly::plot_ly(
    x = period_label,
    y = families,
    z = base::matrix(category, ncol = 1L),
    text = hover,
    type = "heatmap",
    colorscale = color_scale,
    zmin = 0,
    zmax = 5,
    showscale = FALSE,
    hoverinfo = "text",
    xgap = 1,
    ygap = 1,
    source = source_id
  ) |>
    plotly::layout(
      margin = base::list(
        l = 80,
        r = 20,
        t = 20,
        b = 90
      ),
      xaxis = base::list(
        title = "",
        type = "category",
        fixedrange = TRUE
      ),
      yaxis = base::list(
        title = "",
        type = "category",
        autorange = "reversed",
        fixedrange = TRUE
      )
    )
}

# --------------------------------------------------
# Multi-airport availability viewer
# --------------------------------------------------

make_multi_accordion <- function(
  airport_points,
  time_level,
  time_scope
) {
  airport_ids <- airport_points$airport_id

  plot_width <- if (base::identical(time_scope, "single")) {
    "420px"
  } else if (base::identical(time_level, "month")) {
    month_plot_width
  } else {
    year_plot_width
  }

  panels <- base::lapply(
    base::seq_along(airport_ids),
    function(i) {
      airport_id <- airport_ids[[i]]

      panel_title <- base::paste0(
        airport_points$airport_code[[i]],
        " - ",
        airport_points$airport_name[[i]]
      )

      plot_id <- base::paste0("multi_plot_", airport_id)

      panel_body <- shiny::tagList(
        if (base::identical(time_scope, "all")) {
          make_scroll_note(
            output_id = plot_id,
            time_level = time_level
          )
        },
        make_heatmap_display(
          output_id = plot_id,
          plot_width = plot_width,
          time_scope = time_scope
        )
      )

      bslib::accordion_panel(
        title = panel_title,
        panel_body,
        value = base::paste0("airport_", airport_id)
      )
    }
  )

  base::do.call(
    bslib::accordion,
    base::c(
      panels,
      base::list(
        id = "multi_accordion",
        open = FALSE,
        multiple = TRUE
      )
    )
  )
}

# --------------------------------------------------
# Time-selection helpers
# --------------------------------------------------

get_service_month <- function(month_id, service_months) {
  service_months[base::as.integer(month_id)]
}

# --------------------------------------------------
# Airport-selection and map helpers
# --------------------------------------------------

make_airport_choices <- function(latest_airports) {
  labels <- base::paste0(
    latest_airports$LatestAirportCode,
    " - ",
    latest_airports$LatestAirportName
  )

  if (base::anyDuplicated(labels) > 0L) {
    base::stop(
      "Latest airport display labels are ambiguous.",
      call. = FALSE
    )
  }

  display_order <- base::order(
    labels
  )

  stats::setNames(
    base::as.character(
      latest_airports$AirportID[display_order]
    ),
    labels[display_order]
  )
}

make_airport_points <- function(periods) {
  columns <- c(
    "airport_id",
    "airport_seq_id",
    "airport_code",
    "airport_name",
    "airport_city",
    "airport_latitude",
    "airport_longitude",
    "service_month"
  )

  states <- base::unique(
    periods[columns]
  )

  states <- states[
    base::order(
      states$airport_id,
      states$service_month,
      states$airport_seq_id
    ), ,
    drop = FALSE
  ]

  points <- states[
    !base::duplicated(
      states$airport_id,
      fromLast = TRUE
    ), ,
    drop = FALSE
  ]

  points <- points[
    base::order(
      points$airport_code,
      points$airport_name,
      points$airport_id
    ), ,
    drop = FALSE
  ]

  points$point_key <- base::paste0(
    "airport_",
    points$airport_id
  )

  points$hover <- base::paste0(
    "<b>",
    points$airport_code,
    " - ",
    points$airport_name,
    "</b><br>",
    points$airport_city,
    "<br><i>Click for details</i>"
  )

  points
}

make_station_points <- function(periods) {
  columns <- c(
    "station_id",
    "station_name",
    "station_icao",
    "station_latitude",
    "station_longitude",
    "station_elevation"
  )

  points <- base::unique(
    periods[columns]
  )

  points <- points[
    base::order(
      points$station_id
    ), ,
    drop = FALSE
  ]

  points <- points[
    !base::duplicated(points$station_id), ,
    drop = FALSE
  ]

  points$point_key <- base::paste0(
    "station_",
    points$station_id
  )

  points$hover <- base::paste0(
    "<b>",
    points$station_name,
    "</b><br>",
    points$station_id,
    "<br><i>Click for details</i>"
  )

  points
}

month_gap <- function(month, prev_month) {
  12L * (
    base::as.integer(base::format(month, "%Y")) -
      base::as.integer(base::format(prev_month, "%Y"))
  ) +
    base::as.integer(base::format(month, "%m")) -
    base::as.integer(base::format(prev_month, "%m"))
}

format_month_range <- function(start_month, end_month) {
  if (base::identical(
    start_month,
    end_month
  )) {
    return(
      base::format(
        start_month,
        "%B %Y"
      )
    )
  }

  base::paste(
    base::format(start_month, "%B %Y"),
    base::format(end_month, "%B %Y"),
    sep = " - "
  )
}

format_date_range <- function(start_date, end_date) {
  start_day <- base::as.integer(
    base::format(start_date, "%d")
  )
  end_day <- base::as.integer(
    base::format(end_date, "%d")
  )

  same_month <- base::format(
    start_date,
    "%Y-%m"
  ) == base::format(
    end_date,
    "%Y-%m"
  )

  same_year <- base::format(
    start_date,
    "%Y"
  ) == base::format(
    end_date,
    "%Y"
  )

  if (start_date == end_date) {
    return(
      base::paste(
        base::format(start_date, "%B"),
        base::paste0(start_day, ","),
        base::format(start_date, "%Y")
      )
    )
  }

  if (same_month) {
    return(
      base::paste0(
        base::format(start_date, "%B"),
        " ",
        start_day,
        "-",
        end_day,
        ", ",
        base::format(end_date, "%Y")
      )
    )
  }

  if (same_year) {
    return(
      base::paste0(
        base::format(start_date, "%B"),
        " ",
        start_day,
        "-",
        base::format(end_date, "%B"),
        " ",
        end_day,
        ", ",
        base::format(end_date, "%Y")
      )
    )
  }

  base::paste0(
    base::format(start_date, "%B"),
    " ",
    start_day,
    ", ",
    base::format(start_date, "%Y"),
    "-",
    base::format(end_date, "%B"),
    " ",
    end_day,
    ", ",
    base::format(end_date, "%Y")
  )
}

make_month_ranges <- function(months) {
  months <- base::sort(
    base::unique(months)
  )

  if (base::length(months) == 0L) {
    return(
      base::data.frame(
        span_start = base::as.Date(character(0)),
        span_end = base::as.Date(character(0))
      )
    )
  }

  prev_months <- c(
    base::as.Date(NA),
    months[-base::length(months)]
  )

  new_period <- base::is.na(prev_months) |
    month_gap(months, prev_months) > 1L

  period_id <- base::cumsum(new_period)
  period_rows <- base::split(
    base::seq_along(months),
    period_id
  )

  starts <- base::as.Date(
    base::vapply(
      period_rows,
      function(rows) {
        base::as.character(months[rows[[1L]]])
      },
      character(1)
    )
  )

  ends <- base::as.Date(
    base::vapply(
      period_rows,
      function(rows) {
        base::as.character(
          months[rows[[base::length(rows)]]]
        )
      },
      character(1)
    )
  )

  base::data.frame(
    span_start = starts,
    span_end = ends
  )
}

make_daily_ranges <- function(starts, ends) {
  if (base::length(starts) == 0L) {
    return(
      base::data.frame(
        span_start = base::as.Date(character(0)),
        span_end = base::as.Date(character(0))
      )
    )
  }

  display_order <- base::order(
    starts,
    ends
  )

  starts <- starts[display_order]
  ends <- ends[display_order]

  range_starts <- base::as.Date(character(0))
  range_ends <- base::as.Date(character(0))

  current_start <- starts[1L]
  current_end <- ends[1L]

  if (base::length(starts) > 1L) {
    for (i in 2L:base::length(starts)) {
      next_start <- starts[i]
      next_end <- ends[i]

      if (next_start <= current_end + 1L) {
        current_end <- base::max(
          current_end,
          next_end
        )
      } else {
        range_starts <- c(
          range_starts,
          current_start
        )
        range_ends <- c(
          range_ends,
          current_end
        )

        current_start <- next_start
        current_end <- next_end
      }
    }
  }

  range_starts <- c(
    range_starts,
    current_start
  )
  range_ends <- c(
    range_ends,
    current_end
  )

  base::data.frame(
    span_start = range_starts,
    span_end = range_ends
  )
}

make_period_ranges <- function(periods, time_level) {
  if (base::identical(time_level, "month")) {
    return(
      make_daily_ranges(
        starts = periods$period_start,
        ends = periods$period_end
      )
    )
  }

  make_month_ranges(
    periods$service_month
  )
}

make_period_ui <- function(periods, time_level) {
  ranges <- make_period_ranges(
    periods = periods,
    time_level = time_level
  )

  if (base::nrow(ranges) == 0L) {
    return(NULL)
  }

  if (base::identical(time_level, "month")) {
    period_text <- base::vapply(
      base::seq_len(base::nrow(ranges)),
      function(i) {
        format_date_range(
          ranges$span_start[i],
          ranges$span_end[i]
        )
      },
      character(1)
    )
  } else {
    period_text <- base::vapply(
      base::seq_len(base::nrow(ranges)),
      function(i) {
        format_month_range(
          ranges$span_start[i],
          ranges$span_end[i]
        )
      },
      character(1)
    )
  }

  period_label <- if (base::length(period_text) == 1L) {
    "Service period:"
  } else {
    "Service periods:"
  }

  shiny::tagList(
    shiny::tags$p(
      shiny::tags$strong(period_label),
      style = "margin-top: 0.35rem;"
    ),
    base::lapply(
      period_text,
      shiny::tags$p
    )
  )
}

format_elevation <- function(value) {
  if (base::is.na(value)) {
    return(NA_character_)
  }

  base::paste0(
    base::formatC(
      value,
      format = "f",
      digits = 1,
      big.mark = ","
    ),
    " m"
  )
}

clean_detail_values <- function(values) {
  values <- base::as.character(values)
  keep <- !base::is.na(values) &
    base::nzchar(base::trimws(values))

  base::unique(values[keep])
}

make_detail_field <- function(label, value) {
  if (base::length(value) == 0L ||
    base::is.null(value) ||
    base::is.na(value) ||
    !base::nzchar(
      base::as.character(value)
    )) {
    return(NULL)
  }

  shiny::tags$p(
    shiny::tags$strong(
      base::paste0(label, ":")
    ),
    shiny::tags$br(),
    base::as.character(value)
  )
}

make_detail_values <- function(label, values) {
  values <- clean_detail_values(values)

  if (base::length(values) == 0L) {
    return(NULL)
  }

  value_ui <- base::lapply(
    base::seq_along(values),
    function(i) {
      if (i == 1L) {
        return(values[[i]])
      }

      shiny::tagList(
        shiny::tags$br(),
        values[[i]]
      )
    }
  )

  shiny::tags$p(
    shiny::tags$strong(
      base::paste0(label, ":")
    ),
    shiny::tags$br(),
    shiny::tagList(value_ui)
  )
}

make_airport_identity_ui <- function(periods) {
  periods <- periods[
    base::order(
      periods$service_month,
      periods$airport_seq_id
    ), ,
    drop = FALSE
  ]

  airport_names <- clean_detail_values(
    periods$airport_name
  )
  airport_codes <- clean_detail_values(
    periods$airport_code
  )
  airport_cities <- clean_detail_values(
    periods$airport_city
  )

  name_label <- if (base::length(airport_names) == 1L) {
    "Airport name during selected service"
  } else {
    "Airport names during selected service"
  }

  code_label <- if (base::length(airport_codes) == 1L) {
    "Airport code during selected service"
  } else {
    "Airport codes during selected service"
  }

  city_label <- if (base::length(airport_cities) == 1L) {
    "Airport city"
  } else {
    "Airport cities"
  }

  shiny::tagList(
    make_detail_values(
      name_label,
      airport_names
    ),
    make_detail_values(
      code_label,
      airport_codes
    ),
    make_detail_values(
      city_label,
      airport_cities
    )
  )
}

make_station_usage_ui <- function(
  periods,
  time_level
) {
  station_ids <- base::unique(
    periods$station_id
  )

  first_dates <- base::vapply(
    station_ids,
    function(station_id) {
      station_periods <- periods[
        periods$station_id == station_id, ,
        drop = FALSE
      ]

      base::as.character(
        base::min(station_periods$period_start)
      )
    },
    character(1)
  )

  station_ids <- station_ids[
    base::order(first_dates, station_ids)
  ]

  label <- if (base::length(station_ids) == 1L) {
    "Weather station used during selected service:"
  } else {
    "Weather stations used during selected service:"
  }

  items <- base::lapply(
    base::seq_along(station_ids),
    function(i) {
      station_periods <- periods[
        periods$station_id == station_ids[[i]], ,
        drop = FALSE
      ]

      station <- station_periods[
        1L, ,
        drop = FALSE
      ]

      block_style <- if (i == 1L) {
        "margin-top: 0.25rem;"
      } else {
        paste(
          "margin-top: 0.6rem;",
          "padding-top: 0.6rem;",
          "border-top: 1px solid var(--bs-border-color);"
        )
      }

      shiny::tags$div(
        style = block_style,
        make_detail_field(
          "Weather station name",
          station$station_name[[1L]]
        ),
        make_detail_field(
          "GHCNh ID",
          station$station_id[[1L]]
        ),
        make_detail_field(
          "ICAO code",
          station$station_icao[[1L]]
        ),
        make_detail_field(
          "Elevation",
          format_elevation(
            station$station_elevation[[1L]]
          )
        ),
        make_period_ui(
          periods = station_periods,
          time_level = time_level
        )
      )
    }
  )

  shiny::tagList(
    shiny::tags$h6(label),
    items
  )
}

make_airport_usage_ui <- function(
  periods,
  time_level
) {
  airport_ids <- base::unique(
    periods$airport_id
  )

  first_dates <- base::vapply(
    airport_ids,
    function(airport_id) {
      airport_periods <- periods[
        periods$airport_id == airport_id, ,
        drop = FALSE
      ]

      base::as.character(
        base::min(airport_periods$period_start)
      )
    },
    character(1)
  )

  latest_codes <- base::vapply(
    airport_ids,
    function(airport_id) {
      airport_periods <- periods[
        periods$airport_id == airport_id, ,
        drop = FALSE
      ]

      airport_periods <- airport_periods[
        base::order(
          airport_periods$service_month,
          airport_periods$airport_seq_id
        ), ,
        drop = FALSE
      ]

      airport_periods$airport_code[[
        base::nrow(airport_periods)
      ]]
    },
    character(1)
  )

  display_order <- base::order(
    latest_codes,
    first_dates,
    airport_ids
  )

  airport_ids <- airport_ids[display_order]

  items <- base::lapply(
    base::seq_along(airport_ids),
    function(i) {
      airport_periods <- periods[
        periods$airport_id == airport_ids[[i]], ,
        drop = FALSE
      ]

      block_style <- if (i == 1L) {
        "margin-top: 0.25rem;"
      } else {
        paste(
          "margin-top: 0.6rem;",
          "padding-top: 0.6rem;",
          "border-top: 1px solid var(--bs-border-color);"
        )
      }

      shiny::tags$div(
        style = block_style,
        make_airport_identity_ui(
          airport_periods
        ),
        make_period_ui(
          periods = airport_periods,
          time_level = time_level
        )
      )
    }
  )

  shiny::tagList(
    shiny::tags$h6(
      if (base::length(airport_ids) == 1L) {
        "Used for the following airport:"
      } else {
        "Used for the following airports:"
      }
    ),
    items
  )
}

make_airport_detail_ui <- function(
  periods,
  time_level
) {
  shiny::tagList(
    make_airport_identity_ui(
      periods
    ),
    shiny::tags$hr(),
    make_station_usage_ui(
      periods = periods,
      time_level = time_level
    )
  )
}

make_station_detail_ui <- function(
  point,
  periods,
  time_level
) {
  shiny::tagList(
    make_detail_field(
      "Weather station name",
      point$station_name[[1L]]
    ),
    make_detail_field(
      "GHCNh ID",
      point$station_id[[1L]]
    ),
    make_detail_field(
      "ICAO code",
      point$station_icao[[1L]]
    ),
    make_detail_field(
      "Elevation",
      format_elevation(
        point$station_elevation[[1L]]
      )
    ),
    shiny::tags$hr(),
    make_airport_usage_ui(
      periods = periods,
      time_level = time_level
    )
  )
}


make_credits <- function() {
  shiny::HTML(
    base::paste0(
      "<p>",
      "The SLC Airport &amp; Weather Station Explorer was developed by ",
      "<a href=\"https://orcid.org/0009-0009-4396-9878\" ",
      "target=\"_blank\" rel=\"noopener noreferrer\">",
      "Aleksey Kulyukin</a> at ",
      "<a href=\"https://www.usu.edu/\" ",
      "target=\"_blank\" rel=\"noopener noreferrer\">",
      "Utah State University</a>'s ",
      "<a href=\"https://artsci.usu.edu/math-stats/\" ",
      "target=\"_blank\" rel=\"noopener noreferrer\">",
      "Department of Mathematics &amp; Statistics</a>. ",
      "This version was prepared for the R Govys talk ",
      "<em>Using R to Track Flights and Weather Across Salt Lake City’s ",
      "Flight Network</em>. ",
      "It builds on the Airport and Weather Station Spatial Explorer ",
      "developed for the <em>CHANCE</em> article ",
      "<em>Tracking and Mapping Change in Salt Lake City's Flight ",
      "Network, 1987–2026</em> by Kulyukin et al. ",
      "The development of that original application was led by ",
      "<a href=\"https://orcid.org/0009-0004-9019-1097\" ",
      "target=\"_blank\" rel=\"noopener noreferrer\">",
      "Cameron Swapp</a>.",
      "</p>"
    )
  )
}

make_time_css <- function() {
  shiny::tags$style(
    shiny::HTML(
      base::paste0(
        "#time_level {",
        "  margin-bottom: 0.25rem;",
        "}",
        ".floating-panel .card-body {",
        "  padding-top: 0.4rem;",
        "  padding-bottom: 0.4rem;",
        "}",
        ".floating-panel .shiny-input-container {",
        "  margin-bottom: 0;",
        "}",
        ".floating-panel p {",
        "  margin-bottom: 0;",
        "}",
        "#shiny-modal .modal-dialog {",
        "  width: 96vw;",
        "  max-width: 96vw;",
        "  margin: 1rem auto;",
        "}",
        "#shiny-modal .modal-body {",
        "  padding-top: 0.25rem;",
        "  max-height: calc(100vh - 175px);",
        "  overflow-y: auto;",
        "}"
      )
    )
  )
}

make_month_slider_js <- function(month_full_labels) {
  labels <- base::paste(
    base::shQuote(month_full_labels),
    collapse = ", "
  )

  shiny::tags$script(
    shiny::HTML(
      base::paste0(
        "$(document).on('shiny:connected', function() {",
        "  var monthLabels = [",
        labels,
        "];",
        "  function monthText(num) {",
        "    var index = Math.round(Number(num)) - 1;",
        "    return monthLabels[index] || String(num);",
        "  }",
        "  function updateSlider() {",
        "    var slider = $('#month_index').data('ionRangeSlider');",
        "    if (!slider) {",
        "      window.setTimeout(updateSlider, 100);",
        "      return;",
        "    }",
        "    slider.update({",
        "      prettify_enabled: true,",
        "      prettify: monthText",
        "    });",
        "  }",
        "  updateSlider();",
        "});"
      )
    )
  )
}

make_scroll_note_js <- function() {
  shiny::tags$script(
    shiny::HTML(
      base::paste(
        base::c(
          "(function() {",
          "  'use strict';",
          "  var pending = false;",
          "  var watched = new Set();",
          "  var resizeObs = window.ResizeObserver ?",
          "    new ResizeObserver(queueCheck) : null;",
          "",
          "  function updateNotes() {",
          "    document.querySelectorAll('.weather-scroll-note')",
          "      .forEach(function(note) {",
          "        var id = note.getAttribute('data-plot-id');",
          "        var plot = document.getElementById(id);",
          "        var area = plot && plot.closest('.weather-scroll-area');",
          "        var needsScroll = area && area.clientWidth > 0 &&",
          "          area.scrollWidth > area.clientWidth + 1;",
          "        note.style.display = needsScroll ? '' : 'none';",
          "      });",
          "  }",
          "",
          "  function queueCheck() {",
          "    if (pending) return;",
          "    pending = true;",
          "    window.requestAnimationFrame(function() {",
          "      pending = false;",
          "      updateNotes();",
          "    });",
          "  }",
          "",
          "  function scanAreas() {",
          "    if (resizeObs) {",
          "      watched.forEach(function(area) {",
          "        if (document.contains(area)) return;",
          "        resizeObs.unobserve(area);",
          "        watched.delete(area);",
          "      });",
          "      document.querySelectorAll('.weather-scroll-area')",
          "        .forEach(function(area) {",
          "          if (watched.has(area)) return;",
          "          watched.add(area);",
          "          resizeObs.observe(area);",
          "        });",
          "    }",
          "    queueCheck();",
          "  }",
          "",
          "  function start() {",
          "    scanAreas();",
          "    var mutationObs = new MutationObserver(function(records) {",
          "      var selector = '.weather-scroll-area, ' +",
          "        '.weather-scroll-note';",
          "      for (var i = 0; i < records.length; i++) {",
          "        var groups = [",
          "          records[i].addedNodes,",
          "          records[i].removedNodes",
          "        ];",
          "        for (var j = 0; j < groups.length; j++) {",
          "          for (var k = 0; k < groups[j].length; k++) {",
          "            var node = groups[j][k];",
          "            if (node.nodeType !== 1) continue;",
          "            var matching = node.matches(selector) ||",
          "              node.querySelector(selector);",
          "            if (matching) {",
          "              scanAreas();",
          "              return;",
          "            }",
          "          }",
          "        }",
          "      }",
          "    });",
          "    mutationObs.observe(document.body, {",
          "      childList: true,",
          "      subtree: true",
          "    });",
          "    window.addEventListener('resize', queueCheck);",
          "    $(document).on('shown.bs.modal', scanAreas);",
          "    $(document).on('shown.bs.collapse', scanAreas);",
          "    $(document).on('shiny:value', scanAreas);",
          "  }",
          "",
          "  if (document.readyState === 'loading') {",
          "    document.addEventListener('DOMContentLoaded', start);",
          "  } else {",
          "    start();",
          "  }",
          "})();"
        ),
        collapse = "\n"
      )
    )
  )
}

# --------------------------------------------------
# Load precomputed app data
# --------------------------------------------------

app_data <- base::readRDS(
  base::file.path(
    "data",
    "weather_app_data.rds"
  )
)

check_app_data(app_data)

index_data <- base::readRDS(
  base::file.path("data", "airport_index.rds")
)

if (!base::is.list(index_data) ||
  !base::identical(index_data$schema_version, 2L) ||
  !base::identical(
    index_data$elements,
    app_data$metadata$elements
  ) ||
  !base::is.data.frame(index_data$airport_months) ||
  !base::is.data.frame(index_data$family_months) ||
  !base::is.data.frame(index_data$family_years)) {
  base::stop(
    "Airport reporting index is incompatible.",
    call. = FALSE
  )
}

app_month_keys <- base::sort(base::unique(
  base::paste(
    app_data$service_months$AirportID,
    app_data$service_months$ServiceMonth,
    sep = "|"
  )
))

index_month_keys <- base::sort(
  base::paste(
    index_data$airport_months$airport_id,
    index_data$airport_months$service_month,
    sep = "|"
  )
)

if (!base::identical(
  app_month_keys,
  index_month_keys
)) {
  base::stop(
    "Reporting-index and app service months disagree.",
    call. = FALSE
  )
}


service_months <- base::sort(
  base::unique(app_data$service_months$ServiceMonth)
)

service_month_min <- app_data$metadata$first_service_month
service_month_max <- app_data$metadata$last_service_month

years <- base::sort(
  base::unique(
    base::as.integer(
      base::format(service_months, "%Y")
    )
  )
)

year_min <- base::min(years)
year_max <- base::max(years)

# Reserve room for every angled tick; scroll within the plot area.
year_plot_width <- base::paste0(
  100L + 20L * base::length(years),
  "px"
)

month_plot_width <- base::paste0(
  100L + 20L * base::length(service_months),
  "px"
)

month_full_labels <- base::format(
  service_months,
  "%B %Y"
)

airport_choices <- make_airport_choices(
  app_data$latest_airports
)

slc_rows <- app_data$latest_airports$LatestAirportCode == "SLC"

if (base::sum(slc_rows) != 1L) {
  base::stop(
    "Exactly one SLC airport entity is required.",
    call. = FALSE
  )
}

slc_airport_id <- app_data$latest_airports$AirportID[slc_rows]

app_title <- base::paste0(
  "SLC Airport & Weather Station Explorer: ",
  base::format(service_month_min, "%B %Y"),
  " - ",
  base::format(service_month_max, "%B %Y")
)

# --------------------------------------------------
# User interface
# --------------------------------------------------

ui <- bslib::page_fluid(
  make_time_css(),
  make_month_slider_js(month_full_labels),
  make_scroll_note_js(),
  shiny::div(
    style = paste(
      "margin-top: 12px;",
      "margin-bottom: 10px;",
      "text-align: center;"
    ),
    shiny::h2(
      app_title,
      style = "margin-bottom: 0;"
    )
  ),
  bslib::card(
    fill = FALSE,
    max_height = "150px",
    style = paste(
      "position: sticky;",
      "top: 0;",
      "z-index: 1000;",
      "background-color: var(--bs-body-bg);"
    ),
    bslib::card_header(
      "Time Selection",
      class = "py-1"
    ),
    bslib::card_body(
      class = "py-1",
      bslib::layout_columns(
        col_widths = c(
          side_col_width,
          main_col_width
        ),
        shiny::div(
          shiny::radioButtons(
            inputId = "time_level",
            label = "Time unit",
            choices = c(
              "Year" = "year",
              "Month" = "month"
            ),
            selected = "year",
            inline = TRUE
          ),
          shiny::radioButtons(
            inputId = "time_scope",
            label = "Coverage",
            choices = c(
              "All" = "all",
              "Single" = "single"
            ),
            selected = "all",
            inline = TRUE
          )
        ),
        shiny::div(
          shiny::conditionalPanel(
            condition = paste(
              "input.time_level == 'year'",
              "&& input.time_scope == 'single'"
            ),
            shiny::sliderInput(
              inputId = "year",
              label = NULL,
              min = year_min,
              max = year_max,
              step = 1,
              value = year_min,
              ticks = FALSE,
              sep = "",
              width = "100%"
            ),
            shiny::div(
              paste(
                "Use the slider to select a year. The map and ",
                "weather data displays will update."
              ),
              style = paste(
                "font-size: 0.8rem;",
                "margin-top: -0.35rem;",
                "margin-bottom: 0;",
                "color: var(--bs-secondary-color);"
              )
            )
          ),
          shiny::conditionalPanel(
            condition = paste(
              "input.time_level == 'month'",
              "&& input.time_scope == 'single'"
            ),
            shiny::sliderInput(
              inputId = "month_index",
              label = NULL,
              min = 1,
              max = base::length(service_months),
              value = 1,
              step = 1,
              ticks = FALSE,
              sep = "",
              width = "100%"
            ),
            shiny::div(
              paste(
                "Use the slider to select a month. The map and ",
                "weather data displays will update."
              ),
              style = paste(
                "font-size: 0.8rem;",
                "margin-top: -0.35rem;",
                "margin-bottom: 0;",
                "color: var(--bs-secondary-color);"
              )
            )
          )
        )
      )
    )
  ),
  bslib::layout_columns(
    col_widths = c(
      side_col_width,
      main_col_width
    ),
    style = "align-items: start;",
    bslib::card(
      style = paste(
        "position: sticky;",
        "top: 160px;",
        "max-height: calc(100vh - 180px);",
        "overflow-y: auto;"
      ),
      bslib::card_header(
        "Airport Selection"
      ),
      shiny::checkboxInput(
        inputId = "all_airports",
        label = "All Airports",
        value = TRUE
      ),
      shiny::conditionalPanel(
        condition = "!input.all_airports",
        shiny::div(
          paste(
            "Select one or more airports; to reset, click",
            "the button below."
          ),
          style = note_style
        ),
        shiny::actionButton(
          inputId = "clear_airports",
          label = "Clear selection",
          class = "btn-sm",
          style = paste(
            "margin-top: 0;",
            "margin-bottom: 0;"
          )
        ),
        shiny::div(
          style = paste(
            "max-height: calc(100vh - 300px);",
            "overflow-y: auto;",
            "padding-left: 0.15rem;",
            "padding-right: 0.15rem;",
            "padding-top: 0.25rem;"
          ),
          shiny::checkboxGroupInput(
            inputId = "airports",
            label = NULL,
            choices = airport_choices,
            selected = character(0),
            width = "100%"
          )
        )
      )
    ),
    bslib::card(
      style = paste(
        "position: sticky;",
        "top: 160px;",
        "max-height: calc(100vh - 180px);",
        "overflow-y: auto;"
      ),
      shiny::div(
        style = "position: relative;",
        plotly::plotlyOutput(
          outputId = "map",
          height = "600px"
        ),
        shiny::absolutePanel(
          top = floating_panel_top,
          right = floating_panel_side,
          width = 360,
          draggable = TRUE,
          style = paste(
            "max-height: 420px;",
            "overflow-y: auto;",
            "background-color: rgba(255, 255, 255, 0.96);"
          ),
          bslib::card(
            class = "floating-panel",
            bslib::card_header(
              shiny::div(
                style = paste(
                  "display: flex;",
                  "justify-content: space-between;",
                  "align-items: center;",
                  "width: 100%;",
                  "gap: 1rem;"
                ),
                shiny::uiOutput(
                  outputId = "selected_point_title",
                  inline = TRUE
                ),
                shiny::uiOutput(
                  outputId = "clear_point_ui",
                  inline = TRUE
                )
              )
            ),
            shiny::uiOutput(
              outputId = "selected_point_info"
            )
          )
        )
      )
    )
  ),
  shiny::uiOutput("availability_ui"),
  shiny::uiOutput("reporting_drawer_ui"),
  shiny::div(
    style = paste(
      "margin-top: 1rem;",
      "margin-bottom: 1rem;",
      "padding-top: 0.75rem;",
      "border-top: 1px solid var(--bs-border-color);",
      "font-size: 0.85rem;",
      "line-height: 1.35;",
      "color: var(--bs-secondary-color);"
    ),
    make_credits()
  )
)

# --------------------------------------------------
# Server logic
# --------------------------------------------------

server <- function(input, output, session) {
  selected_point <- shiny::reactiveVal(NULL)
  viewer_spec <- shiny::reactiveVal(NULL)
  availability_loader <- shiny::reactiveVal(NULL)
  availability_detail <- shiny::reactiveVal(NULL)
  availability_month <- shiny::reactiveVal(NULL)
  availability_day <- shiny::reactiveVal(NULL)

  all_years_selected <- shiny::reactive({
    base::identical(input$time_level, "year") &&
      base::identical(input$time_scope, "all")
  })

  one_year_selected <- shiny::reactive({
    base::identical(input$time_level, "year") &&
      base::identical(input$time_scope, "single")
  })

  all_months_selected <- shiny::reactive({
    base::identical(input$time_level, "month") &&
      base::identical(input$time_scope, "all")
  })

  one_month_selected <- shiny::reactive({
    base::identical(input$time_level, "month") &&
      base::identical(input$time_scope, "single")
  })

  selected_month <- shiny::reactive({
    shiny::req(input$month_index)

    get_service_month(
      month_id = input$month_index,
      service_months = service_months
    )
  })

  shiny::observeEvent(input$clear_airports, {
    shiny::updateCheckboxGroupInput(
      session = session,
      inputId = "airports",
      selected = character(0)
    )
  })

  selected_airport_ids <- shiny::reactive({
    if (base::isTRUE(input$all_airports)) {
      return(NULL)
    }

    selected_ids <- input$airports

    if (base::is.null(selected_ids) ||
      base::length(selected_ids) == 0L) {
      return(integer(0))
    }

    base::as.integer(selected_ids)
  })

  airport_count_label <- shiny::reactive({
    airport_ids <- effective_airports()$airport_id

    includes_slc <- slc_airport_id %in% airport_ids

    other_count <- base::length(airport_ids) -
      base::as.integer(includes_slc)

    base::paste0(
      other_count,
      if (other_count == 1L) " airport" else " airports",
      " with service to/from SLC",
      if (includes_slc) ", in addition to SLC" else ""
    )
  })

  single_rows <- shiny::reactive({
    airport_ids <- effective_airports()$airport_id

    shiny::req(
      !base::is.null(airport_ids),
      base::length(airport_ids) == 1L,
      base::identical(input$time_scope, "single")
    )

    if (base::identical(input$time_level, "year")) {
      shiny::req(input$year)
      rows <- index_data$family_years
      return(rows[
        rows$airport_id == airport_ids[[1L]] &
          rows$year == base::as.integer(input$year), ,
        drop = FALSE
      ])
    }

    if (base::identical(input$time_level, "month")) {
      month <- selected_month()
      rows <- index_data$family_months
      return(rows[
        rows$airport_id == airport_ids[[1L]] &
          rows$service_month == month, ,
        drop = FALSE
      ])
    }

    base::stop("Unknown time level.", call. = FALSE)
  })


  output$availability_ui <- shiny::renderUI({
    selected_ids <- selected_airport_ids()

    selected_count <- if (base::is.null(selected_ids)) {
      base::nrow(app_data$latest_airports)
    } else {
      base::length(selected_ids)
    }

    airport_points <- effective_airports()
    airport_count <- base::nrow(airport_points)
    missing_count <- selected_count - airport_count

    period_label <- if (one_year_selected()) {
      base::as.character(input$year)
    } else if (one_month_selected()) {
      base::format(selected_month(), "%B %Y")
    } else {
      "the selected period"
    }

    warning_ui <- if (!base::isTRUE(input$all_airports) &&
      selected_count > 1L &&
      missing_count > 0L) {
      missing_ids <- selected_ids[
        !(selected_ids %in% airport_points$airport_id)
      ]

      choice_index <- base::match(
        base::as.character(missing_ids),
        base::unname(airport_choices)
      )

      if (base::anyNA(choice_index)) {
        base::stop(
          "An unavailable airport has no selection label.",
          call. = FALSE
        )
      }

      missing_labels <- base::names(
        airport_choices
      )[choice_index]

      shiny::div(
        class = "alert alert-warning mt-2 mb-2",
        role = "status",
        shiny::tags$p(
          base::paste0(
            "Of the ", selected_count,
            " selected airports, ", missing_count,
            if (missing_count == 1L) " has" else " have",
            " no service to/from SLC in ",
            period_label, ":"
          ),
          class = "mb-1"
        ),
        shiny::tags$ul(
          base::lapply(missing_labels, shiny::tags$li),
          class = "mb-1"
        ),
        shiny::tags$p(
          base::paste(
            if (missing_count == 1L) {
              "This airport remains selected but is not"
            } else {
              "These airports remain selected but are not"
            },
            "shown on the map or in the heatmaps."
          ),
          class = "mb-0"
        )
      )
    } else {
      NULL
    }

    if (selected_count == 0L) {
      return(bslib::card(
        fill = FALSE,
        class = "mt-3",
        bslib::card_header("GHCNh element availability"),
        shiny::tags$p(
          "Select at least one airport to view GHCNh element availability."
        )
      ))
    }

    if (airport_count == 0L) {
      return(shiny::tagList(
        warning_ui,
        bslib::card(
          fill = FALSE,
          class = "mt-3",
          bslib::card_header("GHCNh element availability"),
          shiny::tags$p(
            base::paste0(
              "No selected airports have service to/from SLC in ",
              period_label, "."
            )
          )
        )
      ))
    }

    if (airport_count > 1L) {
      return(shiny::tagList(
        warning_ui,
        bslib::card(
          fill = FALSE,
          class = "mt-3",
          bslib::card_header("GHCNh element availability"),
          shiny::tags$p(
            base::paste0(
              airport_count_label(),
              ". Click the button below to see each airport's heatmap."
            )
          ),
          shiny::actionButton(
            inputId = "open_multi_viewer",
            label = "Open airport availability viewer"
          )
        )
      ))
    }

    year_view <- base::identical(input$time_level, "year")
    month_view <- base::identical(input$time_level, "month")
    all_view <- base::identical(input$time_scope, "all")
    single_view <- base::identical(input$time_scope, "single")

    if (!(year_view || month_view) ||
      !(all_view || single_view)) {
      return(NULL)
    }

    airport <- airport_points[1L, , drop = FALSE]

    period_name <- if (month_view) "month" else "year"

    plot_width <- if (single_view) {
      "420px"
    } else if (month_view) {
      month_plot_width
    } else {
      year_plot_width
    }

    scroll_note <- if (all_view) {
      make_scroll_note(
        output_id = "availability_plot",
        time_level = input$time_level
      )
    } else {
      NULL
    }

    reporting_note <- base::paste(
      "Hover to see element availability;",
      "click for reporting details."
    )

    detail_ui <- make_heatmap_display(
      output_id = "availability_plot",
      plot_width = plot_width,
      time_scope = input$time_scope
    )

    shiny::tagList(
      warning_ui,
      bslib::card(
        fill = FALSE,
        class = "mt-3",
        bslib::card_header(
          base::paste0(
            airport$airport_code[[1L]],
            " - GHCNh element availability by ",
            period_name
          )
        ),
        shiny::tags$p(reporting_note, style = note_style),
        scroll_note,
        detail_ui
      )
    )
  })

  output$availability_plot <- plotly::renderPlotly({
    airport_ids <- effective_airports()$airport_id

    shiny::req(
      !base::is.null(airport_ids),
      base::length(airport_ids) == 1L
    )

    airport_code <- effective_airports()$airport_code[[1L]]

    if (base::identical(input$time_scope, "single")) {
      rows <- single_rows()
      shiny::req(base::nrow(rows) > 0L)

      period_label <- if (base::identical(
        input$time_level, "year"
      )) {
        base::as.character(input$year)
      } else {
        base::format(selected_month(), "%B %Y")
      }

      p <- make_single_heatmap(
        rows = rows,
        families = index_data$elements,
        airport_code = airport_code,
        period_label = period_label
      )

      return(plotly::event_register(
        p,
        "plotly_click"
      ))
    }

    shiny::req(base::identical(input$time_scope, "all"))

    if (base::identical(input$time_level, "month")) {
      p <- make_month_heatmap(
        index_data = index_data,
        airport_id = airport_ids[[1L]],
        airport_code = airport_code,
        service_months = service_months
      )

      return(plotly::event_register(
        p,
        "plotly_click"
      ))
    }

    shiny::req(base::identical(input$time_level, "year"))

    p <- make_year_heatmap(
      index_data = index_data,
      airport_id = airport_ids[[1L]],
      airport_code = airport_code,
      years = years
    )

    plotly::event_register(p, "plotly_click")
  })

  # --------------------------------------------------
  # Reporting clicks: inline and multi-airport heatmaps
  # --------------------------------------------------

  # Preserve the lazy, validated schema-5 loader for daily/hourly data.
  reporting_loader <- function() {
    loader <- availability_loader()

    if (!base::is.null(loader)) {
      return(loader)
    }

    if (!base::exists(
      "new_hourly_reporting_loader",
      mode = "function",
      inherits = TRUE
    )) {
      base::stop(
        "Missing R/hourly_reporting_loader.R.",
        call. = FALSE
      )
    }

    deployed_dir <- base::file.path(
      "data", "hourly_reporting"
    )

    staging_dir <- base::file.path(
      "..", "..", "..",
      "data_derived",
      "slc_weather_explorer",
      "schema5_staging",
      base::paste0("build_", index_data$hourly_build_id)
    )

    stage_dir <- if (base::file.exists(
      base::file.path(deployed_dir, "manifest.rds")
    )) {
      deployed_dir
    } else {
      staging_dir
    }

    loader <- new_hourly_reporting_loader(
      app_data = app_data,
      index_data = index_data,
      stage_dir = stage_dir
    )

    availability_loader(loader)
    loader
  }

  load_reporting_month <- function(airport_id, month, family) {
    service_rows <- index_data$airport_months
    service_rows <- service_rows[
      service_rows$airport_id == airport_id &
        service_rows$service_month == month, ,
      drop = FALSE
    ]

    # No service must not be confused with zero family reporting.
    if (base::nrow(service_rows) == 0L) {
      return(base::list(
        ok = TRUE,
        data = base::list(applicable = FALSE)
      ))
    }

    if (base::nrow(service_rows) != 1L) {
      base::stop(
        "The airport-month index has duplicate service records.",
        call. = FALSE
      )
    }

    family_rows <- index_data$family_months
    family_rows <- family_rows[
      family_rows$airport_id == airport_id &
        family_rows$service_month == month &
        family_rows$element == family, ,
      drop = FALSE
    ]

    if (base::nrow(family_rows) != 1L) {
      base::stop(
        "The service month lacks one unique family index record.",
        call. = FALSE
      )
    }

    base::tryCatch(
      {
        loader <- reporting_loader()
        base::list(
          ok = TRUE,
          data = shiny::withProgress(
            loader$month(
              airport_id = airport_id,
              service_month = month,
              element = family
            ),
            message = "Loading daily reporting..."
          )
        )
      },
      error = function(e) {
        base::list(
          ok = FALSE,
          message = base::conditionMessage(e)
        )
      }
    )
  }


  # Explain historical station changes only when distinct
  # GHCNh stations were assigned during the selected month.
  month_station_ui <- function(airport_id, month) {
    periods <- app_data$airport_periods
    periods <- periods[
      periods$airport_id == airport_id &
        periods$service_month == month, ,
      drop = FALSE
    ]

    if (base::nrow(periods) == 0L) {
      base::stop(
        "Applicable service month has no mapped station periods.",
        call. = FALSE
      )
    }

    station_ids <- base::unique(periods$station_id)

    if (base::length(station_ids) <= 1L) {
      return(NULL)
    }

    shiny::tags$details(
      class = "mt-3",
      shiny::tags$summary(
        "Historical weather station changes",
        style = "display: list-item; cursor: pointer;"
      ),
      shiny::tags$p(
        base::paste(
          "Different weather stations were used during this month.",
          "The dates show when each station was used,",
          "not whether it reported observations."
        ),
        style = note_style
      ),
      make_station_usage_ui(
        periods = periods,
        time_level = "month"
      )
    )
  }

  # The index is dense for each service-eligible airport-month.
  # Keep months without service visible as not applicable.
  year_month_rows <- function(airport_id, family, year) {
    first_month <- base::as.Date(
      base::sprintf("%04d-01-01", year)
    )
    months <- base::seq(
      first_month,
      by = "month",
      length.out = 12L
    )

    service_rows <- index_data$airport_months
    service_rows <- service_rows[
      service_rows$airport_id == airport_id &
        service_rows$service_month %in% months, ,
      drop = FALSE
    ]

    family_rows <- index_data$family_months
    family_rows <- family_rows[
      family_rows$airport_id == airport_id &
        family_rows$element == family &
        family_rows$service_month %in% months, ,
      drop = FALSE
    ]

    if (!base::setequal(
      service_rows$service_month,
      family_rows$service_month
    ) || base::anyDuplicated(
      family_rows$service_month
    ) > 0L) {
      base::stop(
        "Airport and family month index populations disagree.",
        call. = FALSE
      )
    }

    match_rows <- base::match(
      months,
      family_rows$service_month
    )
    applicable <- !base::is.na(match_rows)

    rows <- base::data.frame(
      service_month = months,
      applicable = applicable,
      n_applicable_days = base::rep(NA_integer_, 12L),
      n_days_with_values = base::rep(NA_integer_, 12L),
      report_day_pct = base::rep(NA_real_, 12L)
    )

    if (base::any(applicable)) {
      indexes <- match_rows[applicable]
      rows$n_applicable_days[applicable] <-
        family_rows$n_applicable_days[indexes]
      rows$n_days_with_values[applicable] <-
        family_rows$n_days_with_values[indexes]
      rows$report_day_pct[applicable] <-
        family_rows$report_day_pct[indexes]
    }

    rows
  }

  handle_report_click <- function(
    click, airport_id, airport_code,
    time_level, time_scope,
    selected_year, selected_service_month
  ) {
    if (base::is.null(click) ||
      !base::is.data.frame(click) ||
      base::nrow(click) == 0L ||
      !base::all(base::c("x", "y") %in% base::names(click))) {
      return(NULL)
    }

    family <- base::as.character(click$y[[1L]])

    if (base::is.na(family) ||
      !(family %in% index_data$elements)) {
      shiny::showNotification(
        "The clicked GHCNh element was not recognized.",
        type = "warning"
      )
      return(NULL)
    }

    if (base::identical(time_level, "year")) {
      if (base::identical(time_scope, "all")) {
        year_index <- base::match(
          base::as.character(click$x[[1L]]),
          base::as.character(years)
        )
        year <- years[year_index]
      } else if (base::identical(time_scope, "single")) {
        year <- selected_year
      } else {
        return(NULL)
      }

      if (base::length(year) != 1L ||
        base::is.na(year) || !(year %in% years)) {
        shiny::showNotification(
          "The clicked service year was not recognized.",
          type = "warning"
        )
        return(NULL)
      }

      availability_day(NULL)
      availability_month(NULL)
      availability_detail(base::list(
        origin = "year",
        airport_id = airport_id,
        airport_code = airport_code,
        family = family,
        year = base::as.integer(year)
      ))
      return(NULL)
    }

    if (!base::identical(time_level, "month")) {
      return(NULL)
    }

    if (base::identical(time_scope, "all")) {
      month_index <- base::match(
        base::as.character(click$x[[1L]]),
        base::format(service_months, "%Y-%m")
      )
      month <- service_months[month_index]
    } else if (base::identical(time_scope, "single")) {
      month <- selected_service_month
    } else {
      return(NULL)
    }

    if (base::length(month) != 1L || base::is.na(month)) {
      shiny::showNotification(
        "The clicked service month was not recognized.",
        type = "warning"
      )
      return(NULL)
    }

    result <- load_reporting_month(airport_id, month, family)

    availability_day(NULL)
    availability_month(NULL)
    availability_detail(base::list(
      origin = "month",
      airport_id = airport_id,
      airport_code = airport_code,
      family = family,
      month = month,
      result = result
    ))
  }

  shiny::observeEvent(
    input[["plotly_click-weather_availability"]],
    {
      airports <- effective_airports()

      if (base::nrow(airports) != 1L) {
        return(NULL)
      }

      click <- plotly::event_data(
        "plotly_click",
        source = "weather_availability",
        priority = "event"
      )

      handle_report_click(
        click = click,
        airport_id = airports$airport_id[[1L]],
        airport_code = airports$airport_code[[1L]],
        time_level = input$time_level,
        time_scope = input$time_scope,
        selected_year = if (one_year_selected()) {
          base::as.integer(input$year)
        } else {
          NA_integer_
        },
        selected_service_month = if (one_month_selected()) {
          selected_month()
        } else {
          base::as.Date(NA)
        }
      )
    }
  )

  # A yearly cell first opens monthly availability in the drawer.
  # Only service-eligible months can lead to daily reporting.
  shiny::observeEvent(input$reporting_month_click, {
    detail <- availability_detail()

    if (base::is.null(detail) ||
      !base::identical(detail$origin, "year") ||
      !base::is.null(availability_month())) {
      return(NULL)
    }

    month_text <- base::as.character(
      input$reporting_month_click
    )

    if (base::length(month_text) != 1L ||
      base::is.na(month_text)) {
      return(NULL)
    }

    rows <- year_month_rows(
      detail$airport_id,
      detail$family,
      detail$year
    )
    month_index <- base::match(
      month_text,
      base::format(rows$service_month, "%Y-%m")
    )

    if (base::is.na(month_index) ||
      !base::isTRUE(rows$applicable[[month_index]])) {
      return(NULL)
    }

    month <- rows$service_month[month_index]
    result <- load_reporting_month(
      detail$airport_id, month, detail$family
    )

    availability_day(NULL)
    availability_month(base::list(
      month = month,
      result = result
    ))
  })

  # A daily-table date sends this input from the browser.
  shiny::observeEvent(input$reporting_day_click, {
    detail <- availability_detail()

    if (base::is.null(detail) ||
      !base::identical(detail$origin, "month") ||
      !base::isTRUE(detail$result$ok) ||
      !base::isTRUE(detail$result$data$applicable) ||
      !base::is.null(availability_day())) {
      return(NULL)
    }

    day_text <- base::as.character(
      input$reporting_day_click
    )

    days <- detail$result$data$days

    if (base::length(day_text) != 1L ||
      base::is.na(day_text)) {
      return(NULL)
    }

    day_index <- base::match(
      day_text,
      base::as.character(days$local_date)
    )

    if (base::is.na(day_index)) {
      return(NULL)
    }

    local_date <- days$local_date[day_index]

    result <- base::tryCatch(
      {
        loader <- availability_loader()

        if (base::is.null(loader)) {
          base::stop(
            "The hourly reporting loader is not initialized.",
            call. = FALSE
          )
        }

        hourly <- shiny::withProgress(
          loader$day(
            airport_id = detail$airport_id,
            local_date = local_date,
            element = detail$family
          ),
          message = "Loading hourly reporting..."
        )

        # Reconcile the hourly result against the
        # daily row selected in the drawer.
        if (!base::isTRUE(hourly$applicable) ||
          hourly$n_station_records !=
            days$n_station_records[[day_index]] ||
          hourly$n_value_reports !=
            days$n_value_reports[[day_index]]) {
          base::stop(
            "Hourly reporting does not reconcile ",
            "with the selected daily totals.",
            call. = FALSE
          )
        }

        base::list(
          ok = TRUE,
          data = hourly
        )
      },
      error = function(e) {
        base::list(
          ok = FALSE,
          message = base::conditionMessage(e)
        )
      }
    )

    availability_day(base::list(
      local_date = local_date,
      result = result
    ))
  })

  shiny::observeEvent(input$back_to_reporting_days, {
    availability_day(NULL)
  })

  shiny::observeEvent(input$back_to_reporting_months, {
    detail <- availability_detail()

    if (!base::is.null(detail) &&
      base::identical(detail$origin, "year")) {
      availability_day(NULL)
      availability_month(NULL)
    }
  })

  shiny::observeEvent(input$hide_reporting_drawer, {
    availability_day(NULL)
    availability_month(NULL)
    availability_detail(NULL)
  })

  output$reporting_drawer_ui <- shiny::renderUI({
    detail <- availability_detail()

    if (base::is.null(detail)) {
      return(NULL)
    }

    month_detail <- availability_month()
    day_detail <- availability_day()
    year_origin <- base::identical(detail$origin, "year")

    if (year_origin && base::is.null(month_detail)) {
      # Annual heatmap cells lead to all twelve calendar months.
      months <- year_month_rows(
        detail$airport_id,
        detail$family,
        detail$year
      )

      month_rows <- base::lapply(
        base::seq_len(base::nrow(months)),
        function(i) {
          month_text <- base::format(
            months$service_month[i], "%B %Y"
          )

          if (base::isTRUE(months$applicable[[i]])) {
            month_key <- base::format(
              months$service_month[i], "%Y-%m"
            )
            month_label <- shiny::tags$button(
              type = "button",
              class = "btn btn-link btn-sm p-0",
              onclick = base::paste0(
                "Shiny.setInputValue(",
                "'reporting_month_click', '",
                month_key,
                "', {priority: 'event'}); return false;"
              ),
              month_text
            )
            applicable_days <- base::as.character(
              months$n_applicable_days[[i]]
            )
            reporting_days <- base::as.character(
              months$n_days_with_values[[i]]
            )
            percentage <- base::sprintf(
              "%.2f%%", months$report_day_pct[[i]]
            )
          } else {
            month_label <- month_text
            applicable_days <- "Not applicable"
            reporting_days <- "Not applicable"
            percentage <- "Not applicable"
          }

          shiny::tags$tr(
            shiny::tags$td(month_label),
            shiny::tags$td(applicable_days),
            shiny::tags$td(reporting_days),
            shiny::tags$td(percentage)
          )
        }
      )

      body <- shiny::tagList(
        shiny::tags$p(
          base::paste(
            "Click a month with service to/from SLC to view daily reporting.",
            "Other months are marked Not applicable."
          ),
          style = note_style
        ),
        shiny::tags$table(
          class = "table table-sm table-striped",
          shiny::tags$thead(
            shiny::tags$tr(
              shiny::tags$th("Month"),
              shiny::tags$th("Days in service periods"),
              shiny::tags$th("Days with reported values"),
              shiny::tags$th("Days with reported values (%)")
            )
          ),
          shiny::tags$tbody(month_rows)
        )
      )

      if (!base::any(months$applicable)) {
        body <- shiny::tags$p(
          base::sprintf(
            "No service to/from SLC in %d.",
            detail$year
          )
        )
      }

      displayed_period <- base::as.character(detail$year)
    } else if (base::is.null(day_detail)) {
      # Daily reporting is terminal when entered from a yearly view.
      result <- if (year_origin) {
        month_detail$result
      } else {
        detail$result
      }
      month <- if (year_origin) {
        month_detail$month
      } else {
        detail$month
      }

      if (!base::isTRUE(result$ok)) {
        body <- shiny::tags$p(
          base::paste(
            "Daily reporting could not be loaded:",
            result$message
          ),
          style = "color: var(--bs-danger);"
        )
      } else if (!base::isTRUE(result$data$applicable)) {
        body <- shiny::tags$p(
          "No service to/from SLC in the selected month."
        )
      } else {
        days <- result$data$days

        day_rows <- base::lapply(
          base::seq_len(base::nrow(days)),
          function(i) {
            date_text <- base::format(
              days$local_date[i],
              "%Y-%m-%d"
            )

            # A yearly drill-down ends at daily totals.
            day_button <- if (year_origin) {
              date_text
            } else {
              shiny::tags$button(
                type = "button",
                class = "btn btn-link btn-sm p-0",
                onclick = base::paste0(
                  "Shiny.setInputValue(",
                  "'reporting_day_click', '",
                  date_text,
                  "', {priority: 'event'}); return false;"
                ),
                date_text
              )
            }

            shiny::tags$tr(
              shiny::tags$td(day_button),
              shiny::tags$td(
                base::as.character(
                  days$n_station_records[[i]]
                )
              ),
              shiny::tags$td(
                base::as.character(
                  days$n_value_reports[[i]]
                )
              ),
              shiny::tags$td(
                if (base::isTRUE(
                  days$family_reported[[i]]
                )) {
                  "Yes"
                } else {
                  "No"
                }
              )
            )
          }
        )

        body <- shiny::tagList(
          shiny::tags$p(
            base::sprintf(
              base::paste0(
                "Days with reported values for %s: ",
                "%d of %d days in service periods (%.2f%%)."
              ),
              detail$family,
              result$data$n_days_with_values,
              result$data$n_applicable_days,
              result$data$report_day_pct
            )
          ),
          shiny::tags$p(
            base::paste(
              if (year_origin) {
                "This yearly drill-down ends at daily reporting."
              } else {
                "Click a date to inspect hourly reporting."
              },
              "Station records count all observations;",
              "records with reported values count only those",
              "containing a value for the selected GHCNh element."
            ),
            style = note_style
          ),
          shiny::tags$table(
            class = "table table-sm table-striped",
            shiny::tags$thead(
              shiny::tags$tr(
                shiny::tags$th("Local date"),
                shiny::tags$th("Station records"),
                shiny::tags$th("Records with reported values"),
                shiny::tags$th("Value reported?")
              )
            ),
            shiny::tags$tbody(day_rows)
          ),
          month_station_ui(
            airport_id = detail$airport_id,
            month = month
          )
        )
      }

      displayed_period <- base::format(month, "%B %Y")
    } else {
      # Hourly view: a single airport-local date.
      result <- day_detail$result

      displayed_period <- base::format(
        day_detail$local_date,
        "%B %d, %Y"
      )

      if (!base::isTRUE(result$ok)) {
        body <- shiny::tags$p(
          base::paste(
            "Hourly reporting could not be loaded:",
            result$message
          ),
          style = "color: var(--bs-danger);"
        )
      } else if (!base::isTRUE(result$data$applicable)) {
        body <- shiny::tags$p(
          "The selected date is outside the airport's service periods."
        )
      } else {
        day_data <- result$data
        hours <- day_data$hours

        station_labels <- base::paste0(
          day_data$stations$station_name,
          " (",
          day_data$stations$station_id,
          ")"
        )

        station_ui <- make_detail_values(
          if (base::length(clean_detail_values(station_labels)) == 1L) {
            "Weather station used for this date"
          } else {
            "Weather stations used for this date"
          },
          station_labels
        )

        if (base::nrow(hours) == 0L) {
          hourly_table <- shiny::tags$p(
            "No station records were reported for this date."
          )
        } else {
          hour_rows <- base::lapply(
            base::seq_len(base::nrow(hours)),
            function(i) {
              utc_label <- base::format(
                hours$hour_start_utc[i],
                "%Y-%m-%d %H:%M UTC",
                tz = "UTC"
              )

              shiny::tags$tr(
                shiny::tags$td(
                  hours$local_hour[[i]]
                ),
                shiny::tags$td(utc_label),
                shiny::tags$td(
                  base::as.character(
                    hours$n_station_records[[i]]
                  )
                ),
                shiny::tags$td(
                  base::as.character(
                    hours$n_value_reports[[i]]
                  )
                )
              )
            }
          )

          hourly_table <- shiny::tags$table(
            class = "table table-sm table-striped",
            shiny::tags$thead(
              shiny::tags$tr(
                shiny::tags$th("Local hour"),
                shiny::tags$th("UTC hour start"),
                shiny::tags$th("Station records"),
                shiny::tags$th("Records with reported values")
              )
            ),
            shiny::tags$tbody(hour_rows)
          )
        }

        body <- shiny::tagList(
          shiny::tags$p(
            base::sprintf(
              base::paste0(
                "%s: %s station records; ",
                "%s records with reported values for %s."
              ),
              displayed_period,
              day_data$n_station_records,
              day_data$n_value_reports,
              detail$family
            )
          ),
          station_ui,
          shiny::tags$p(
            base::paste(
              "Local times use",
              day_data$timezone,
              "and include UTC offsets.",
              "Only hours containing station records",
              "appear in the table."
            ),
            style = note_style
          ),
          hourly_table
        )
      }
    }

    shiny::tags$aside(
      role = "region",
      `aria-label` = "Reporting details",
      style = base::paste(
        "position: fixed;",
        "top: 1rem;",
        "right: 1rem;",
        "z-index: 1100;",
        "width: 540px;",
        "max-width: calc(100vw - 2rem);",
        "max-height: calc(100vh - 2rem);",
        "overflow-x: auto;",
        "overflow-y: auto;",
        "padding: 1rem;",
        "background: var(--bs-body-bg);",
        "border: 1px solid var(--bs-border-color);",
        "border-radius: 0.5rem;",
        "box-shadow: 0 0 18px rgba(0,0,0,0.2);"
      ),
      shiny::tags$div(
        style = base::paste(
          "display: flex;",
          "justify-content: space-between;",
          "align-items: center;",
          "gap: 1rem;"
        ),
        shiny::tags$h5(
          if (year_origin && base::is.null(month_detail)) {
            "Monthly reporting details"
          } else if (base::is.null(day_detail)) {
            "Daily reporting details"
          } else {
            "Hourly reporting details"
          }
        ),
        shiny::tags$div(
          style = "display: flex; gap: 0.5rem;",
          if (!base::is.null(day_detail)) {
            shiny::actionButton(
              "back_to_reporting_days",
              "Back to days",
              class = "btn-sm"
            )
          },
          if (year_origin && !base::is.null(month_detail)) {
            shiny::actionButton(
              "back_to_reporting_months",
              "Back to months",
              class = "btn-sm"
            )
          },
          shiny::actionButton(
            "hide_reporting_drawer",
            "Hide",
            class = "btn-sm"
          )
        )
      ),
      shiny::tags$p(
        base::paste(
          detail$airport_code,
          detail$family,
          displayed_period,
          sep = " | "
        )
      ),
      body
    )
  })

  shiny::observeEvent(input$open_multi_viewer, {
    airport_points <- effective_airports()
    airport_ids <- airport_points$airport_id

    shiny::req(base::length(airport_ids) > 1L)

    spec <- base::list(
      airport_ids = airport_ids,
      airport_points = airport_points,
      time_level = input$time_level,
      time_scope = input$time_scope,
      year = if (one_year_selected()) {
        base::as.integer(input$year)
      } else {
        NA_integer_
      },
      month = if (one_month_selected()) {
        selected_month()
      } else {
        base::as.Date(NA)
      }
    )

    viewer_spec(spec)

    period_label <- if (base::identical(spec$time_scope, "all")) {
      if (base::identical(spec$time_level, "year")) {
        "All years"
      } else {
        "All service months"
      }
    } else if (base::identical(spec$time_level, "year")) {
      base::as.character(spec$year)
    } else {
      base::format(spec$month, "%B %Y")
    }

    reporting_note <- base::paste(
      "Hover to see element availability;",
      "click for reporting details."
    )

    shiny::showModal(
      shiny::modalDialog(
        shiny::tags$p(
          base::paste(
            "With many airports in the viewer, Expand All",
            "may take time to render every heatmap.",
            "Open individual airport panels when you",
            "need only a few."
          ),
          class = "text-body-secondary small mt-0 mb-2"
        ),
        shiny::tags$p(
          base::paste0(
            airport_count_label(),
            " | ",
            period_label
          )
        ),
        shiny::tags$p(reporting_note, style = note_style),
        make_multi_accordion(
          airport_points = spec$airport_points,
          time_level = spec$time_level,
          time_scope = spec$time_scope
        ),
        title = shiny::tags$div(
          style = paste(
            "display: flex;",
            "justify-content: space-between;",
            "align-items: center;",
            "gap: 1rem;",
            "width: 100%;"
          ),
          shiny::tags$span("GHCNh element availability by airport"),
          shiny::tags$div(
            style = "display: flex; gap: 0.5rem;",
            shiny::actionButton(
              "expand_multi",
              "Expand All",
              class = "btn-sm"
            ),
            shiny::actionButton(
              "collapse_multi",
              "Collapse All",
              class = "btn-sm"
            )
          )
        ),
        footer = shiny::actionButton(
          "close_multi",
          "Close"
        ),
        size = "xl",
        easyClose = FALSE
      ),
      session = session
    )
  })

  shiny::observeEvent(input$expand_multi, {
    shiny::req(!base::is.null(viewer_spec()))
    bslib::accordion_panel_open(
      id = "multi_accordion",
      values = TRUE,
      session = session
    )
  })

  shiny::observeEvent(input$collapse_multi, {
    shiny::req(!base::is.null(viewer_spec()))
    bslib::accordion_panel_close(
      id = "multi_accordion",
      values = TRUE,
      session = session
    )
  })

  shiny::observeEvent(input$close_multi, {
    shiny::removeModal(session = session)
    viewer_spec(NULL)
    availability_day(NULL)
    availability_month(NULL)
    availability_detail(NULL)
  })

  # Hidden accordion panels suspend their Plotly outputs.
  # Distinct event sources prevent clicks crossing accordion panels.
  for (airport_id in app_data$latest_airports$AirportID) {
    base::local({
      one_id <- airport_id
      output_id <- base::paste0("multi_plot_", one_id)
      plot_source <- base::paste0("weather_multi_", one_id)

      output[[output_id]] <- plotly::renderPlotly({
        spec <- viewer_spec()
        shiny::req(
          !base::is.null(spec),
          one_id %in% spec$airport_ids
        )

        airport_row <- base::match(
          one_id,
          spec$airport_points$airport_id
        )
        shiny::req(!base::is.na(airport_row))

        airport_code <- spec$airport_points$airport_code[[
          airport_row
        ]]

        if (base::identical(spec$time_scope, "single")) {
          if (base::identical(spec$time_level, "year")) {
            rows <- index_data$family_years
            rows <- rows[
              rows$airport_id == one_id &
                rows$year == spec$year, ,
              drop = FALSE
            ]
            period_label <- base::as.character(spec$year)
          } else {
            rows <- index_data$family_months
            rows <- rows[
              rows$airport_id == one_id &
                rows$service_month == spec$month, ,
              drop = FALSE
            ]
            period_label <- base::format(
              spec$month, "%B %Y"
            )
          }

          shiny::req(base::nrow(rows) > 0L)
          p <- make_single_heatmap(
            rows = rows,
            families = index_data$elements,
            airport_code = airport_code,
            period_label = period_label,
            source_id = plot_source
          )
        } else if (base::identical(spec$time_scope, "all")) {
          if (base::identical(spec$time_level, "month")) {
            p <- make_month_heatmap(
              index_data = index_data,
              airport_id = one_id,
              airport_code = airport_code,
              service_months = service_months,
              source_id = plot_source
            )
          } else {
            shiny::req(base::identical(spec$time_level, "year"))
            p <- make_year_heatmap(
              index_data = index_data,
              airport_id = one_id,
              airport_code = airport_code,
              years = years,
              source_id = plot_source
            )
          }
        } else {
          return(NULL)
        }

        plotly::event_register(p, "plotly_click")
      })

      shiny::observeEvent(
        input[[base::paste0("plotly_click-", plot_source)]],
        {
          spec <- viewer_spec()

          if (base::is.null(spec) ||
            !(one_id %in% spec$airport_ids)) {
            return(NULL)
          }

          airport_row <- base::match(
            one_id, spec$airport_points$airport_id
          )
          shiny::req(!base::is.na(airport_row))

          click <- plotly::event_data(
            "plotly_click",
            source = plot_source,
            priority = "event"
          )

          handle_report_click(
            click = click,
            airport_id = one_id,
            airport_code = spec$airport_points$airport_code[[
              airport_row
            ]],
            time_level = spec$time_level,
            time_scope = spec$time_scope,
            selected_year = spec$year,
            selected_service_month = spec$month
          )
        }
      )
    })
  }

  selected_periods <- shiny::reactive({
    periods <- app_data$airport_periods

    if (one_year_selected()) {
      periods <- periods[
        periods$year == base::as.integer(input$year), ,
        drop = FALSE
      ]
    } else if (one_month_selected()) {
      periods <- periods[
        periods$service_month == selected_month(), ,
        drop = FALSE
      ]
    }

    airport_ids <- selected_airport_ids()

    if (!base::is.null(airport_ids)) {
      periods <- periods[
        periods$airport_id %in% airport_ids, ,
        drop = FALSE
      ]
    }

    periods
  })

  # Use the same period-specific airport identities as the map.
  effective_airports <- shiny::reactive({
    periods <- selected_periods()

    if (base::nrow(periods) == 0L) {
      return(periods[
        FALSE,
        base::c(
          "airport_id",
          "airport_code",
          "airport_name"
        ),
        drop = FALSE
      ])
    }

    make_airport_points(periods)
  })

  shiny::observeEvent(
    plotly::event_data(
      "plotly_click",
      source = "weather_map"
    ),
    {
      click <- plotly::event_data(
        "plotly_click",
        source = "weather_map"
      )

      if (base::is.null(click) ||
        !"customdata" %in% base::names(click)) {
        return(NULL)
      }

      point_key <- click$customdata[[1L]]

      if (base::is.null(point_key) ||
        base::is.na(point_key)) {
        return(NULL)
      }

      point_key <- base::as.character(
        point_key
      )

      if (!base::startsWith(point_key, "airport_") &&
        !base::startsWith(point_key, "station_")) {
        return(NULL)
      }

      selected_point(point_key)
    }
  )

  shiny::observeEvent(input$clear_point, {
    selected_point(NULL)
  })

  shiny::observeEvent(
    list(
      input$all_airports,
      input$airports,
      input$time_level,
      input$time_scope,
      input$year,
      input$month_index
    ),
    {
      selected_point(NULL)
      availability_day(NULL)
      availability_month(NULL)
      availability_detail(NULL)
    },
    ignoreInit = TRUE
  )

  output$selected_point_title <- shiny::renderUI({
    key <- selected_point()

    if (base::is.null(key)) {
      return(
        shiny::span("Map Details")
      )
    }

    if (base::startsWith(key, "airport_")) {
      return(
        shiny::span("Selected Airport")
      )
    }

    shiny::span("Selected Weather Station")
  })

  output$clear_point_ui <- shiny::renderUI({
    if (base::is.null(selected_point())) {
      return(NULL)
    }

    shiny::actionButton(
      inputId = "clear_point",
      label = "Clear",
      class = "btn-sm"
    )
  })

  output$selected_point_info <- shiny::renderUI({
    key <- selected_point()

    if (base::is.null(key)) {
      return(
        shiny::tags$p(
          paste(
            "Click an airport or weather station point",
            "to view details."
          )
        )
      )
    }

    periods <- selected_periods()

    if (base::nrow(periods) == 0L) {
      selected_point(NULL)
      return(NULL)
    }

    if (base::startsWith(key, "airport_")) {
      points <- make_airport_points(
        periods
      )

      point <- points[
        points$point_key == key, ,
        drop = FALSE
      ]

      if (base::nrow(point) != 1L) {
        selected_point(NULL)
        return(NULL)
      }

      detail_periods <- periods[
        periods$airport_id ==
          point$airport_id[[1L]], ,
        drop = FALSE
      ]

      return(
        make_airport_detail_ui(
          periods = detail_periods,
          time_level = input$time_level
        )
      )
    }

    points <- make_station_points(
      periods
    )

    point <- points[
      points$point_key == key, ,
      drop = FALSE
    ]

    if (base::nrow(point) != 1L) {
      selected_point(NULL)
      return(NULL)
    }

    detail_periods <- periods[
      periods$station_id ==
        point$station_id[[1L]], ,
      drop = FALSE
    ]

    make_station_detail_ui(
      point = point,
      periods = detail_periods,
      time_level = input$time_level
    )
  })

  output$time_selection_label <- shiny::renderText({
    if (all_years_selected()) {
      return("All years")
    }

    if (one_year_selected()) {
      return(
        base::as.character(input$year)
      )
    }

    if (all_months_selected()) {
      return("All months")
    }

    if (one_month_selected()) {
      return(
        base::format(
          selected_month(),
          "%B %Y"
        )
      )
    }

    ""
  })

  output$airport_selection_label <- shiny::renderText({
    airport_ids <- selected_airport_ids()

    if (base::is.null(airport_ids)) {
      return("All airports")
    }

    if (base::length(airport_ids) == 0L) {
      return("No airports selected")
    }

    choice_values <- base::unname(
      airport_choices
    )

    choice_index <- base::match(
      base::as.character(airport_ids),
      choice_values
    )

    shiny::req(
      !base::anyNA(choice_index)
    )

    selected_labels <- base::names(
      airport_choices
    )[choice_index]

    base::paste(
      selected_labels,
      collapse = ", "
    )
  })

  output$map <- plotly::renderPlotly({
    periods <- selected_periods()

    shiny::validate(
      shiny::need(
        base::nrow(periods) > 0L,
        "No airports to display for this selection."
      )
    )

    station_points <- make_station_points(
      periods
    )

    airport_points <- effective_airports()

    airport_points$marker_symbol <- base::ifelse(
      airport_points$airport_id == slc_airport_id,
      "star",
      "circle"
    )

    airport_points$marker_size <- base::ifelse(
      airport_points$airport_id == slc_airport_id,
      10,
      4
    )

    p <- plotly::plot_ly(
      source = "weather_map"
    )

    p <- p |>
      plotly::add_trace(
        data = station_points,
        type = "scattergeo",
        mode = "markers",
        lon = ~station_longitude,
        lat = ~station_latitude,
        text = ~hover,
        customdata = ~point_key,
        hoverinfo = "text",
        name = "Weather Station",
        showlegend = TRUE,
        marker = list(
          color = "#FED976",
          size = 15,
          opacity = 1,
          line = list(
            color = "black",
            width = 0.5
          )
        ),
        inherit = FALSE
      )

    p <- p |>
      plotly::add_trace(
        data = airport_points,
        type = "scattergeo",
        mode = "markers",
        lon = ~airport_longitude,
        lat = ~airport_latitude,
        text = ~hover,
        customdata = ~point_key,
        hoverinfo = "text",
        name = "Airport",
        showlegend = TRUE,
        marker = list(
          color = "black",
          size = airport_points$marker_size,
          symbol = airport_points$marker_symbol,
          opacity = 1
        ),
        inherit = FALSE
      )

    p <- p |>
      plotly::event_register(
        "plotly_click"
      )

    p |>
      plotly::layout(
        geo = list(
          scope = "usa"
        ),
        legend = list(
          x = 1.02,
          y = 0.5
        )
      )
  })
}

shiny::shinyApp(
  ui = ui,
  server = server
)
