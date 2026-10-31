# SLC Flight Network Explorer
# 09.27.2026 version
#
# Necessary runtime packages:
# bslib, dplyr, ggplot2, plotly, RColorBrewer, sf, shiny, units
#
# slcflights is used only by prepare_data.R to generate the .rds files
# in data/. The deployed app does not import slcflights directly.

# --------------------------------------------------
# App-level display constants
# --------------------------------------------------

reloc_threshold_km <- 9
airport_marker_size <- 8
time_series_height <- 250
time_series_top_margin <- -30
slc_marker_color <- "green"
slc_marker_symbol <- "star"
slc_marker_size <- 8
blue_line_colors <- RColorBrewer::brewer.pal(
  n = 9,
  name = "Blues"
)
route_bidir_color <- blue_line_colors[[4]]
route_monodir_color <- blue_line_colors[[9]]
route_types <- c(
  "bidirectional",
  "from_slc",
  "to_slc"
)
route_colors <- stats::setNames(
  c(
    route_bidir_color,
    route_monodir_color,
    route_monodir_color
  ),
  route_types
)
route_dashes <- stats::setNames(
  c("solid", "dash", "dot"),
  route_types
)
route_css_styles <- stats::setNames(
  c("solid", "dashed", "dotted"),
  route_types
)
route_labels <- stats::setNames(
  c(
    "Both directions",
    "From SLC only",
    "To SLC only"
  ),
  route_types
)
route_line_width <- 0.9
floating_panel_top <- 33
floating_panel_side <- 7
side_col_width <- 3
main_col_width <- 9

# --------------------------------------------------
# Date, distance, and display-format helpers
# --------------------------------------------------

month_gap <- function(month, prev_month) {
  12L * (
    as.integer(format(month, "%Y")) -
      as.integer(format(prev_month, "%Y"))
  ) +
    as.integer(format(month, "%m")) -
    as.integer(format(prev_month, "%m"))
}

distance_km <- function(lat1, lon1, lat2, lon2) {
  from <- sf::st_as_sf(
    data.frame(
      lon = lon1,
      lat = lat1
    ),
    coords = c("lon", "lat"),
    crs = 4326
  )

  to <- sf::st_as_sf(
    data.frame(
      lon = lon2,
      lat = lat2
    ),
    coords = c("lon", "lat"),
    crs = 4326
  )

  sf::st_distance(
    sf::st_geometry(from),
    sf::st_geometry(to),
    by_element = TRUE
  ) |>
    units::set_units(km) |>
    units::drop_units()
}

format_count <- function(x) {
  format(
    x,
    big.mark = ",",
    scientific = FALSE,
    trim = TRUE
  )
}

make_flight_label <- function(from_slc, to_slc) {
  if (from_slc > 0L && to_slc > 0L) {
    return("Flights to/from SLC:")
  }

  if (from_slc > 0L) {
    return("Flights from SLC:")
  }

  "Flights to SLC:"
}

make_hover_flights <- function(from_slc, to_slc) {
  total <- from_slc + to_slc

  paste0(
    make_flight_label(from_slc, to_slc),
    " ",
    format_count(total)
  )
}

make_flight_block <- function(from_slc, to_slc) {
  total <- from_slc + to_slc

  if (from_slc > 0L && to_slc > 0L) {
    return(
      paste0(
        "<b>Flights to/from SLC:</b><br>",
        format_count(total),
        "<br><b>Flights from SLC:</b><br>",
        format_count(from_slc),
        "<br><b>Flights to SLC:</b><br>",
        format_count(to_slc)
      )
    )
  }

  if (from_slc > 0L) {
    return(
      paste0(
        "<b>Flights from SLC:</b><br>",
        format_count(from_slc),
        "<br><b>Flights to SLC:</b><br>",
        "None in this selection"
      )
    )
  }

  paste0(
    "<b>Flights to SLC:</b><br>",
    format_count(to_slc),
    "<br><b>Flights from SLC:</b><br>",
    "None in this selection"
  )
}

format_month_range <- function(start_month, end_month) {
  ifelse(
    start_month == end_month,
    format(start_month, "%B %Y"),
    paste(
      format(start_month, "%B %Y"),
      format(end_month, "%B %Y"),
      sep = " - "
    )
  )
}

format_date_range <- function(start_date, end_date) {
  start_day <- as.integer(format(start_date, "%d"))
  end_day <- as.integer(format(end_date, "%d"))
  same_month <- format(start_date, "%Y-%m") ==
    format(end_date, "%Y-%m")
  same_year <- format(start_date, "%Y") ==
    format(end_date, "%Y")

  if (start_date == end_date) {
    return(
      paste(
        format(start_date, "%B"),
        paste0(start_day, ","),
        format(start_date, "%Y")
      )
    )
  }

  if (same_month) {
    return(
      paste0(
        format(start_date, "%B"),
        " ",
        start_day,
        "-",
        end_day,
        ", ",
        format(end_date, "%Y")
      )
    )
  }

  if (same_year) {
    return(
      paste0(
        format(start_date, "%B"),
        " ",
        start_day,
        "-",
        format(end_date, "%B"),
        " ",
        end_day,
        ", ",
        format(end_date, "%Y")
      )
    )
  }

  paste0(
    format(start_date, "%B"),
    " ",
    start_day,
    ", ",
    format(start_date, "%Y"),
    "-",
    format(end_date, "%B"),
    " ",
    end_day,
    ", ",
    format(end_date, "%Y")
  )
}

make_month_range <- function(month_id, n_months, view_size) {
  view_size <- min(view_size, n_months)

  left <- month_id - floor((view_size - 1L) / 2L)
  left <- max(left, 1L)
  left <- min(left, n_months - view_size + 1L)

  right <- left + view_size - 1L

  c(
    left - 0.25,
    right + 0.25
  )
}

get_service_month <- function(month_id, service_months) {
  service_months[as.integer(month_id)]
}

make_daily_block <- function(dates) {
  dates <- sort(unique(dates))

  if (length(dates) == 0L) {
    return("<b>Service periods:</b><br>")
  }

  prev_dates <- c(as.Date(NA), dates[-length(dates)])
  new_period <- is.na(prev_dates) |
    as.integer(dates - prev_dates) > 1L
  period_id <- cumsum(new_period)

  starts <- dates[!duplicated(period_id)]
  ends <- dates[!duplicated(period_id, fromLast = TRUE)]

  period_text <- mapply(
    format_date_range,
    starts,
    ends,
    USE.NAMES = FALSE
  )

  period_label <- if (length(period_text) == 1L) {
    "Service period:"
  } else {
    "Service periods:"
  }

  paste0(
    "<b>",
    period_label,
    "</b><br>",
    paste(period_text, collapse = "<br>")
  )
}

collapse_display <- function(x) {
  paste(sort(unique(x)), collapse = "<br>")
}

collapse_inline <- function(x) {
  paste(sort(unique(x)), collapse = ", ")
}

make_name_block <- function(names, n_names) {
  label <- if (n_names == 1L) {
    "Airport name during selected service:"
  } else {
    "Airport names during selected service:"
  }

  paste0(
    "<br><b>",
    label,
    "</b><br>",
    names
  )
}

make_code_block <- function(codes, n_codes) {
  label <- if (n_codes == 1L) {
    "Airport code during selected service:"
  } else {
    "Airport codes during selected service:"
  }

  paste0(
    "<br><b>",
    label,
    "</b><br>",
    codes
  )
}

make_current_identity_block <- function(
    current_name,
    current_code,
    current_is_closed
) {
  if (current_is_closed == 1L) {
    return(
      paste0(
        "<b>Final recorded airport name:</b><br>",
        current_name,
        "<br><b>Final recorded airport code:</b><br>",
        current_code
      )
    )
  }
  
  paste0(
    "<b>Current airport name:</b><br>",
    current_name,
    "<br><b>Current airport code:</b><br>",
    current_code
  )
}

make_status_block <- function(is_closed, thru_date) {
  if (is.na(is_closed)) {
    return("")
  }

  status_label <- paste0(
    "<br><b>",
    "Airport status at end of selected service:",
    "</b><br>"
  )

  if (is_closed == 1) {
    if (is.na(thru_date)) {
      return(
        paste0(
          status_label,
          "Closed"
        )
      )
    }

    return(
      paste0(
        status_label,
        "Closed as of ",
        format(thru_date, "%B %d, %Y")
      )
    )
  }

  paste0(
    status_label,
    "Open"
  )
}

make_time_label <- function(
  time_level,
  time_scope,
  year,
  month_id,
  year_min,
  year_max,
  service_months
) {
  if (identical(time_level, "month")) {
    if (identical(time_scope, "all")) {
      return(
        paste(
          format(min(service_months), "%B %Y"),
          format(max(service_months), "%B %Y"),
          sep = " - "
        )
      )
    }

    month <- get_service_month(
      month_id = month_id,
      service_months = service_months
    )

    return(format(month, "%B %Y"))
  }

  if (identical(time_scope, "all")) {
    return(
      paste0(
        year_min,
        "-",
        year_max
      )
    )
  }

  as.character(year)
}

make_legend_title <- function(
  input,
  year_min,
  year_max,
  service_months
) {
  paste0(
    "Flights to/from SLC<br>",
    make_time_label(
      time_level = input$time_level,
      time_scope = input$time_scope,
      year = input$year,
      month_id = input$month_index,
      year_min = year_min,
      year_max = year_max,
      service_months = service_months
    )
  )
}

make_credits <- function() {
  shiny::HTML(
    paste0(
      "<p>",
      "The SLC Flight Network Explorer was developed by ",
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
      "It builds on the SLC Flight Network Explorer developed by ",
      "Kulyukin et al. for the <em>CHANCE</em> article ",
      "<em>Tracking and Mapping Change in Salt Lake City's Flight Network, ",
      "1987–2026.</em>",
      "</p>"
    )
  )
}

make_airline_css <- function() {
  shiny::tags$style(
    shiny::HTML(
      paste0(
        "#airlines .form-check {",
        "  margin-bottom: 0.15rem;",
        "}",
        "#airlines .form-check-label {",
        "  font-size: 0.82rem;",
        "  line-height: 1.15;",
        "}",
        "#airlines .checkbox {",
        "  margin-top: 0.1rem;",
        "  margin-bottom: 0.1rem;",
        "}",
        "#airlines input[type='checkbox'] {",
        "  margin-right: 0.25rem;",
        "}",
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
        "}"
      )
    )
  )
}

make_month_slider_js <- function(month_full_labels) {
  labels <- paste(shQuote(month_full_labels), collapse = ", ")

  shiny::tags$script(
    shiny::HTML(
      paste0(
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

make_route_key <- function(route_type) {
  shiny::div(
    style = paste(
      "display: flex;",
      "align-items: center;",
      "gap: 0.45rem;",
      "margin-bottom: 0.2rem;"
    ),
    shiny::span(
      style = paste0(
        "display: inline-block;",
        "width: 2.25rem;",
        "flex: 0 0 2.25rem;",
        "border-top: 2px ",
        route_css_styles[[route_type]],
        " ",
        route_colors[[route_type]],
        ";"
      )
    ),
    shiny::span(route_labels[[route_type]])
  )
}

make_route_legend <- function() {
  shiny::div(
    style = paste(
      "margin-top: 0.35rem;",
      "font-size: 0.82rem;",
      "line-height: 1.15;"
    ),
    shiny::div(
      "Direction",
      style = paste(
        "font-weight: 600;",
        "margin-bottom: 0.25rem;"
      )
    ),
    shiny::tagList(
      lapply(route_types, make_route_key)
    )
  )
}

# --------------------------------------------------
# Flight-count binning and color helpers
# --------------------------------------------------

nice_width <- function(x) {
  power <- 10^floor(log10(x))
  scaled <- x / power

  nice_scaled <- dplyr::case_when(
    scaled <= 1 ~ 1,
    scaled <= 2 ~ 2,
    scaled <= 5 ~ 5,
    TRUE ~ 10
  )

  nice_scaled * power
}

count_labels <- function(breaks) {
  # Use left-closed bins and make the final bin right-closed so the
  # maximum count is always included in the legend's last class.
  labels <- paste0(
    "[",
    format_count(breaks[-length(breaks)]),
    ", ",
    format_count(breaks[-1]),
    ")"
  )

  labels[length(labels)] <- paste0(
    "[",
    format_count(breaks[length(breaks) - 1]),
    ", ",
    format_count(breaks[length(breaks)]),
    "]"
  )

  labels
}

make_flight_breaks <- function(x, n_bins = 5L) {
  max_count <- as.integer(max(x))

  if (max_count < 5L) {
    bin_width <- 1L
  } else {
    bin_width <- nice_width(max_count / n_bins)
  }

  seq(
    from = 0,
    to = bin_width * n_bins,
    by = bin_width
  )
}

make_flight_classes <- function(x, breaks) {
  # Internally extend the final break by one count. The displayed label
  # remains mathematically correct because count_labels() marks only the
  # final bin as right-closed.
  breaks2 <- breaks
  breaks2[length(breaks2)] <- breaks2[length(breaks2)] + 1

  labels <- count_labels(breaks)

  cut(
    x,
    breaks = breaks2,
    include.lowest = TRUE,
    right = FALSE,
    labels = labels
  )
}

make_flight_colors <- function(n) {
  base_cols <- RColorBrewer::brewer.pal(
    9,
    "YlOrRd"
  )[c(1, 3, 5, 7, 9)]

  grDevices::colorRampPalette(base_cols)(n)
}

# --------------------------------------------------
# Empty-map, route-line, and modal-list helpers
# --------------------------------------------------

empty_airports <- function() {
  data.frame(
    PointKey = character(),
    Longitude = numeric(),
    Latitude = numeric(),
    Hover = character(),
    FlightCount = integer(),
    FlightClass = factor(character()),
    Details = character()
  )
}

make_route_lines <- function(df, slc_point) {
  # Plotly draws one line trace by concatenating lon/lat pairs separated
  # by NA rows. Each displayed airport receives one route from SLC.
  if (nrow(df) == 0L) {
    return(
      data.frame(
        Longitude = numeric(),
        Latitude = numeric()
      )
    )
  }

  route_rows <- lapply(seq_len(nrow(df)), function(i) {
    data.frame(
      Longitude = c(
        slc_point$Longitude[[1]],
        df$Longitude[[i]],
        NA_real_
      ),
      Latitude = c(
        slc_point$Latitude[[1]],
        df$Latitude[[i]],
        NA_real_
      )
    )
  })

  do.call(rbind, route_rows)
}

make_list_modal <- function(title, items) {
  if (length(items) == 0L) {
    list_body <- shiny::tags$p("No records to display.")
  } else {
    list_body <- shiny::tags$ul(
      style = paste(
        "margin-bottom: 0;",
        "padding-left: 1.25rem;"
      ),
      lapply(items, shiny::tags$li)
    )
  }

  shiny::modalDialog(
    title = title,
    shiny::div(
      style = paste(
        "max-height: 60vh;",
        "overflow-y: auto;"
      ),
      list_body
    ),
    footer = shiny::div(
      style = paste(
        "display: flex;",
        "justify-content: center;",
        "width: 100%;"
      ),
      shiny::actionButton(
        inputId = "close_time_list",
        label = "Close"
      )
    ),
    easyClose = TRUE
  )
}

make_airport_items <- function(df) {
  if (nrow(df) == 0L) {
    return(character(0))
  }

  make_latest_points(df) |>
    dplyr::arrange(MarkerCode, MarkerName) |>
    dplyr::transmute(
      item = paste0(MarkerCode, " - ", MarkerName)
    ) |>
    dplyr::pull(item)
}

make_airline_items <- function(df) {
  if (nrow(df) == 0L) {
    return(character(0))
  }

  df |>
    dplyr::distinct(Reporting_Airline_Name) |>
    dplyr::arrange(Reporting_Airline_Name) |>
    dplyr::pull(Reporting_Airline_Name)
}

# --------------------------------------------------
# Airline controls
# --------------------------------------------------

make_airline_lookup <- function(network_data) {
  network_data |>
    dplyr::distinct(Reporting_Airline_Name) |>
    dplyr::arrange(Reporting_Airline_Name) |>
    dplyr::mutate(
      AirlineKey = paste0("airline_", dplyr::row_number())
    )
}

make_airline_choices <- function(airline_lookup) {
  stats::setNames(
    airline_lookup$AirlineKey,
    airline_lookup$Reporting_Airline_Name
  )
}

# --------------------------------------------------
# Airport history summaries for the details panel
# --------------------------------------------------

make_periods <- function(df) {
  # Split service into monthly periods for Year mode whenever an airport
  # has a gap of more than one calendar month in the current selection.
  df |>
    dplyr::distinct(
      AirportID,
      ServiceMonth
    ) |>
    dplyr::arrange(
      AirportID,
      ServiceMonth
    ) |>
    dplyr::group_by(AirportID) |>
    dplyr::mutate(
      prev_month = dplyr::lag(ServiceMonth),
      gap = month_gap(ServiceMonth, prev_month),
      new_period = is.na(prev_month) | gap > 1L,
      period_id = cumsum(new_period)
    ) |>
    dplyr::group_by(
      AirportID,
      period_id
    ) |>
    dplyr::summarize(
      start_month = min(ServiceMonth),
      end_month = max(ServiceMonth),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      period_text = format_month_range(
        start_month,
        end_month
      )
    ) |>
    dplyr::group_by(AirportID) |>
    dplyr::summarize(
      n_periods = dplyr::n(),
      service_periods = paste(
        period_text,
        collapse = "<br>"
      ),
      .groups = "drop"
    )
}

make_change_summary <- function(df) {
  # Summarize airport history within the current selection. Small
  # coordinate changes are treated as jitter; only shifts at or above
  # reloc_threshold_km are reported as location changes.
  changes <- df |>
    dplyr::distinct(
      AirportID,
      AirportSeqID,
      AirportName,
      AirportCode,
      AirportCity,
      Latitude,
      Longitude,
      AirportStartDate,
      AirportIsClosed,
      AirportIsLatest
    ) |>
    dplyr::arrange(
      AirportID,
      AirportStartDate,
      AirportSeqID
    ) |>
    dplyr::group_by(AirportID) |>
    dplyr::mutate(
      prev_name = dplyr::lag(AirportName),
      prev_code = dplyr::lag(AirportCode),
      prev_lat = dplyr::lag(Latitude),
      prev_lon = dplyr::lag(Longitude),
      prev_closed = dplyr::lag(AirportIsClosed),
      shifted = !is.na(prev_lat) &
        ((Latitude != prev_lat) | (Longitude != prev_lon)),
      name_changed = !is.na(prev_name) &
        AirportName != prev_name,
      code_changed = !is.na(prev_code) &
        AirportCode != prev_code,
      closed_changed = !is.na(prev_closed) &
        AirportIsClosed != prev_closed
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      shift_km = 0
    )

  shifted_rows <- which(changes$shifted)

  if (length(shifted_rows) > 0L) {
    changes$shift_km[shifted_rows] <- distance_km(
      lat1 = changes$prev_lat[shifted_rows],
      lon1 = changes$prev_lon[shifted_rows],
      lat2 = changes$Latitude[shifted_rows],
      lon2 = changes$Longitude[shifted_rows]
    )
  }

  changes |>
    dplyr::mutate(
      large_shift = shift_km >= reloc_threshold_km
    ) |>
    dplyr::group_by(AirportID) |>
    dplyr::summarize(
      n_large_shifts = sum(large_shift),
      n_name_changes = sum(name_changed),
      n_code_changes = sum(code_changed),
      n_closed_changes = sum(closed_changed),
      .groups = "drop"
    )
}

make_latest_points <- function(df) {
  # Display one map point per airport. The latest record avoids plotting
  # multiple points for small coordinate jitter or historical sequences.
  df |>
    dplyr::arrange(
      AirportID,
      ServiceMonth,
      AirportStartDate,
      AirportSeqID
    ) |>
    dplyr::group_by(AirportID) |>
    dplyr::slice_tail(n = 1) |>
    dplyr::ungroup() |>
    dplyr::transmute(
      AirportID,
      MarkerName = AirportName,
      MarkerCode = AirportCode,
      Latitude,
      Longitude,
      AirportIsClosed,
      AirportThruDate
    )
}

make_name_history <- function(df) {
  # Preserve airport names represented by the selected service.
  # Current identity is joined separately from unfiltered app data.
  df |>
    dplyr::distinct(
      AirportID,
      AirportSeqID,
      AirportName,
      AirportStartDate
    ) |>
    dplyr::arrange(
      AirportID,
      AirportStartDate,
      AirportSeqID
    ) |>
    dplyr::group_by(AirportID) |>
    dplyr::summarize(
      service_airport_names = paste(
        unique(AirportName),
        collapse = "<br>"
      ),
      n_service_airport_names = dplyr::n_distinct(AirportName),
      .groups = "drop"
    )
}

make_notes <- function(
    n_large,
    n_names,
    name_changed_after_last_slc_service,
    current_code,
    current_code_replacement_name
) {
  notes <- character(0)
  
  if (n_large > 0) {
    notes <- c(
      notes,
      "Location changed during selected service."
    )
  }
  
  if (n_names > 0) {
    notes <- c(
      notes,
      "Airport name changed during selected service."
    )
  }
  
  if (isTRUE(name_changed_after_last_slc_service)) {
    notes <- c(
      notes,
      paste(
        "Airport name changed after its last recorded",
        "service to/from SLC."
      )
    )
  }
  
  if (
    !is.na(current_code_replacement_name) &&
    nzchar(current_code_replacement_name)
  ) {
    notes <- c(
      notes,
      paste0(
        "The ",
        current_code,
        " code is now associated with ",
        current_code_replacement_name,
        "."
      )
    )
  }
  
  if (length(notes) == 0L) {
    return("")
  }
  
  paste0(
    "<br><b>Airport history notes:</b><br>",
    paste(notes, collapse = "<br>")
  )
}

# --------------------------------------------------
# Time-series plotting helper
# --------------------------------------------------

make_time_plot <- function(
  df, title, y_label, y_top, y_breaks,
  years, year_min, year_max, year, source
) {
  # Convert the ggplot object to Plotly for hover text and click events.
  # The red year line is shown only when the app is in single-year mode.
  p <- ggplot2::ggplot(
    df,
    ggplot2::aes(x = Year, y = Count)
  ) +
    ggplot2::geom_point() +
    ggplot2::geom_line(
      ggplot2::aes(group = SegmentID),
      linewidth = 0.5
    ) +
    ggplot2::scale_x_continuous(
      breaks = years,
      limits = c(year_min, year_max),
      minor_breaks = NULL
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, y_top),
      breaks = y_breaks
    ) +
    ggplot2::ylab(y_label) +
    ggplot2::labs(
      title = title
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      axis.text.x = ggplot2::element_text(
        angle = 45,
        hjust = 1
      )
    )

  if (!is.null(year)) {
    p <- p +
      ggplot2::geom_vline(
        xintercept = year,
        color = "red"
      )
  }

  plotly::ggplotly(
    p,
    tooltip = NULL,
    source = source
  ) |>
    plotly::style(
      text = df$Hover,
      hoverinfo = "text",
      traces = 1
    ) |>
    plotly::event_register("plotly_click") |>
    plotly::layout(
      showlegend = FALSE
    )
}

make_month_plot <- function(
  df, title, y_label, y_top, y_breaks,
  month_ids, month_labels, month_id, month_range, source
) {
  p <- ggplot2::ggplot(
    df,
    ggplot2::aes(x = MonthIndex, y = Count)
  ) +
    ggplot2::geom_point() +
    ggplot2::geom_line(
      ggplot2::aes(group = SegmentID),
      linewidth = 0.5
    ) +
    ggplot2::scale_x_continuous(
      breaks = month_ids,
      labels = month_labels,
      limits = range(month_ids),
      expand = ggplot2::expansion(
        mult = 0,
        add = 0.25
      ),
      minor_breaks = NULL
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, y_top),
      breaks = y_breaks
    ) +
    ggplot2::xlab("Month") +
    ggplot2::ylab(y_label) +
    ggplot2::labs(
      title = title
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      axis.text.x = ggplot2::element_text(
        angle = 45,
        hjust = 1
      )
    )

  if (!is.null(month_id)) {
    p <- p +
      ggplot2::geom_vline(
        xintercept = month_id,
        color = "red"
      )
  }

  plotly::ggplotly(
    p,
    tooltip = NULL,
    source = source
  ) |>
    plotly::style(
      text = df$Hover,
      hoverinfo = "text",
      traces = 1
    ) |>
    plotly::event_register("plotly_click") |>
    plotly::layout(
      showlegend = FALSE,
      dragmode = "pan",
      margin = list(
        b = 70
      ),
      xaxis = list(
        title = list(
          text = "Month"
        ),
        tickmode = "array",
        tickvals = month_ids,
        ticktext = month_labels,
        tickangle = -45,
        range = month_range,
        fixedrange = FALSE,
        automargin = TRUE
      ),
      yaxis = list(
        title = list(
          text = y_label
        ),
        automargin = TRUE
      )
    )
}

# --------------------------------------------------
# Load precomputed app data
# --------------------------------------------------

slc_point <- readRDS("data/slc_point.rds")
all_data <- readRDS("data/all_data.rds")
current_airports <- readRDS("data/current_airports.rds")
airline_lookup <- make_airline_lookup(all_data)

years <- sort(unique(all_data$Year))
year_min <- min(years)
year_max <- max(years)

service_months <- sort(unique(all_data$ServiceMonth))
service_month_min <- min(service_months)
service_month_max <- max(service_months)
month_ids <- seq_along(service_months)
month_labels <- format(service_months, "%b %Y")
month_full_labels <- format(service_months, "%B %Y")
month_frame <- data.frame(
  ServiceMonth = service_months,
  MonthIndex = month_ids
)

month_view_size <- length(years)

app_title <- paste0(
  "SLC Flight Network Explorer: ",
  format(service_month_min, "%B %Y"),
  " - ",
  format(service_month_max, "%B %Y")
)

airline_choices <- make_airline_choices(airline_lookup)

# --------------------------------------------------
# User interface
# --------------------------------------------------

ui <- bslib::page_fluid(
  make_airline_css(),
  make_month_slider_js(month_full_labels),
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
    # Sticky positioning freezes this bar at the top.
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
        col_widths = c(side_col_width, main_col_width),
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
              "Use the slider to select a year. The map updates to that ",
              "year, and the selected year is marked by a red vertical ",
              "line in both time-series plots.",
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
              max = length(service_months),
              value = 1,
              step = 1,
              ticks = FALSE,
              sep = "",
              width = "100%"
            ),
            shiny::div(
              "Use the slider to select a month. The map updates to that ",
              "month, and the monthly time-series ",
              "view centers on the selected month, which is marked by a ",
              "red vertical line in both time-series plots.",
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
    col_widths = c(side_col_width, main_col_width),
    # Allow child columns to stick and handle overflow.
    style = "align-items: start;",
    bslib::card(
      # Sticky left column with its own vertical scroll.
      style = paste(
        "position: sticky;",
        "top: 160px;",
        "max-height: calc(100vh - 180px);",
        "overflow-y: auto;"
      ),
      bslib::card_header("Airline Selection"),
      shiny::checkboxInput(
        inputId = "all_airlines",
        label = "All Airlines",
        value = TRUE
      ),
      shiny::conditionalPanel(
        condition = "!input.all_airlines",
        shiny::div(
          "Select one or more airlines; to reset, click button below.",
          style = paste(
            "font-size: 0.8rem;",
            "margin-top: 0;",
            "margin-bottom: 0;",
            "color: var(--bs-secondary-color);"
          )
        ),
        shiny::actionButton(
          inputId = "clear_airlines",
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
            "padding-top: 0;"
          ),
          shiny::checkboxGroupInput(
            inputId = "airlines",
            label = NULL,
            choices = airline_choices,
            selected = character(0),
            width = "100%"
          )
        )
      )
    ),
    bslib::card(
      # Sticky right column with its own vertical scroll.
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
          left = floating_panel_side,
          width = 220,
          draggable = TRUE,
          style = paste(
            "background-color: rgba(255, 255, 255, 0.96);"
          ),
          bslib::card(
            class = "floating-panel",
            bslib::card_header("Route Lines"),
            shiny::checkboxInput(
              inputId = "show_routes",
              label = "Show route lines",
              value = FALSE
            ),
            shiny::conditionalPanel(
              condition = "input.show_routes",
              make_route_legend()
            )
          )
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
                shiny::span("Selected Airport"),
                shiny::uiOutput(
                  outputId = "clear_airport_ui",
                  inline = TRUE
                )
              )
            ),
            shiny::uiOutput("selected_airport")
          )
        )
      ),
      shiny::conditionalPanel(
        condition = "input.time_level == 'year'",
        shiny::div(
          style = paste0(
            "height: ",
            time_series_height,
            "px;",
            "margin-top: ",
            time_series_top_margin,
            "px;"
          ),
          plotly::plotlyOutput(
            outputId = "airport_time_series",
            height = paste0(time_series_height, "px")
          )
        ),
        shiny::div(
          style = paste0(
            "height: ",
            time_series_height,
            "px;"
          ),
          plotly::plotlyOutput(
            outputId = "airline_time_series",
            height = paste0(time_series_height, "px")
          )
        )
      ),
      shiny::conditionalPanel(
        condition = "input.time_level == 'month'",
        shiny::div(
          style = paste0(
            "height: ",
            time_series_height,
            "px;",
            "margin-top: ",
            time_series_top_margin,
            "px;"
          ),
          plotly::plotlyOutput(
            outputId = "airport_month_series",
            height = paste0(time_series_height, "px")
          )
        ),
        shiny::div(
          style = paste0(
            "height: ",
            time_series_height,
            "px;"
          ),
          plotly::plotlyOutput(
            outputId = "airline_month_series",
            height = paste0(time_series_height, "px")
          )
        )
      )
    )
  ),
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

  all_years_selected <- shiny::reactive({
    identical(input$time_level, "year") &&
      identical(input$time_scope, "all")
  })

  one_year_selected <- shiny::reactive({
    identical(input$time_level, "year") &&
      identical(input$time_scope, "single")
  })

  all_months_selected <- shiny::reactive({
    identical(input$time_level, "month") &&
      identical(input$time_scope, "all")
  })

  one_month_selected <- shiny::reactive({
    identical(input$time_level, "month") &&
      identical(input$time_scope, "single")
  })

  shiny::observeEvent(
    plotly::event_data(
      "plotly_click",
      source = "network_map"
    ),
    {
      click <- plotly::event_data(
        "plotly_click",
        source = "network_map"
      )

      if (is.null(click)) {
        return(NULL)
      }

      # Only connected-airport markers carry PointKey customdata. Other
      # map layers, including SLC and route lines, are ignored.
      if (!"customdata" %in% names(click)) {
        return(NULL)
      }

      point_key <- click$customdata[[1]]

      if (is.null(point_key) || is.na(point_key)) {
        return(NULL)
      }

      point_key <- as.character(point_key)

      if (!startsWith(point_key, "point_")) {
        return(NULL)
      }

      selected_point(point_key)
    }
  )

  shiny::observeEvent(input$clear_airport, {
    selected_point(NULL)
  })

  shiny::observeEvent(input$clear_airlines, {
    shiny::updateCheckboxGroupInput(
      session = session,
      inputId = "airlines",
      selected = character(0)
    )
  })

  shiny::observeEvent(input$close_time_list, {
    shiny::removeModal()
  })

  shiny::observeEvent(
    plotly::event_data(
      "plotly_click",
      source = "airport_time_series",
      priority = "event"
    ),
    {
      click <- plotly::event_data(
        "plotly_click",
        source = "airport_time_series",
        priority = "event"
      )

      if (is.null(click) || is.null(click$x)) {
        return(NULL)
      }

      if (!identical(input$time_level, "year")) {
        return(NULL)
      }

      clicked_year <- as.integer(round(click$x[[1]]))

      if (!clicked_year %in% years) {
        return(NULL)
      }

      df <- flight_data() |>
        dplyr::filter(Year == clicked_year)

      items <- make_airport_items(df)

      shiny::showModal(
        make_list_modal(
          title = paste("Connected airports in", clicked_year),
          items = items
        )
      )
    }
  )

  shiny::observeEvent(
    plotly::event_data(
      "plotly_click",
      source = "airline_time_series",
      priority = "event"
    ),
    {
      click <- plotly::event_data(
        "plotly_click",
        source = "airline_time_series",
        priority = "event"
      )

      if (is.null(click) || is.null(click$x)) {
        return(NULL)
      }

      if (!identical(input$time_level, "year")) {
        return(NULL)
      }

      clicked_year <- as.integer(round(click$x[[1]]))

      if (!clicked_year %in% years) {
        return(NULL)
      }

      df <- flight_data() |>
        dplyr::filter(Year == clicked_year)

      items <- make_airline_items(df)

      shiny::showModal(
        make_list_modal(
          title = paste("Airlines in", clicked_year),
          items = items
        )
      )
    }
  )

  shiny::observeEvent(
    plotly::event_data(
      "plotly_click",
      source = "airport_month_series",
      priority = "event"
    ),
    {
      click <- plotly::event_data(
        "plotly_click",
        source = "airport_month_series",
        priority = "event"
      )

      if (is.null(click) || is.null(click$x)) {
        return(NULL)
      }

      if (!identical(input$time_level, "month")) {
        return(NULL)
      }

      clicked_id <- as.integer(round(click$x[[1]]))

      if (!clicked_id %in% month_ids) {
        return(NULL)
      }

      clicked_month <- get_service_month(
        month_id = clicked_id,
        service_months = service_months
      )

      df <- flight_data() |>
        dplyr::filter(ServiceMonth == clicked_month)

      items <- make_airport_items(df)

      shiny::showModal(
        make_list_modal(
          title = paste(
            "Connected airports in",
            format(clicked_month, "%B %Y")
          ),
          items = items
        )
      )
    }
  )

  shiny::observeEvent(
    plotly::event_data(
      "plotly_click",
      source = "airline_month_series",
      priority = "event"
    ),
    {
      click <- plotly::event_data(
        "plotly_click",
        source = "airline_month_series",
        priority = "event"
      )

      if (is.null(click) || is.null(click$x)) {
        return(NULL)
      }

      if (!identical(input$time_level, "month")) {
        return(NULL)
      }

      clicked_id <- as.integer(round(click$x[[1]]))

      if (!clicked_id %in% month_ids) {
        return(NULL)
      }

      clicked_month <- get_service_month(
        month_id = clicked_id,
        service_months = service_months
      )

      df <- flight_data() |>
        dplyr::filter(ServiceMonth == clicked_month)

      items <- make_airline_items(df)

      shiny::showModal(
        make_list_modal(
          title = paste(
            "Airlines in",
            format(clicked_month, "%B %Y")
          ),
          items = items
        )
      )
    }
  )

  shiny::observeEvent(
    list(
      input$all_airlines,
      input$airlines,
      input$time_level,
      input$time_scope,
      input$year,
      input$month_index
    ),
    {
      selected_point(NULL)
    },
    ignoreInit = TRUE
  )

  output$clear_airport_ui <- shiny::renderUI({
    if (is.null(selected_point())) {
      return(NULL)
    }

    shiny::actionButton(
      inputId = "clear_airport",
      label = "Clear",
      class = "btn-sm"
    )
  })

  selected_airlines <- shiny::reactive({
    if (isTRUE(input$all_airlines)) {
      return(NULL)
    }

    selected_keys <- input$airlines

    if (is.null(selected_keys) || length(selected_keys) == 0L) {
      return(character(0))
    }

    airline_lookup |>
      dplyr::filter(AirlineKey %in% selected_keys) |>
      dplyr::pull(Reporting_Airline_Name)
  })

  selected_month <- shiny::reactive({
    shiny::req(input$month_index)

    get_service_month(
      month_id = input$month_index,
      service_months = service_months
    )
  })

  visible_months <- shiny::reactive({
    month_id <- 1L

    if (one_month_selected()) {
      month_id <- as.integer(input$month_index)
    }

    make_month_range(
      month_id = month_id,
      n_months = length(service_months),
      view_size = month_view_size
    )
  })

  flight_data <- shiny::reactive({
    df <- all_data
    airlines <- selected_airlines()

    if (!is.null(airlines)) {
      if (length(airlines) == 0L) {
        return(df[0, ])
      }

      df <- df |>
        dplyr::filter(Reporting_Airline_Name %in% airlines)
    }

    df
  })

  map_data <- shiny::reactive({
    df <- flight_data()

    if (identical(input$time_level, "month")) {
      if (one_month_selected()) {
        return(
          df |>
            dplyr::filter(
              ServiceMonth == selected_month()
            )
        )
      }

      return(df)
    }

    if (one_year_selected()) {
      return(
        df |>
          dplyr::filter(
            Year == input$year
          )
      )
    }

    df
  })

  unique_airports <- shiny::reactive({
    # df is the current map view.
    df <- map_data()

    if (nrow(df) == 0L) {
      return(empty_airports())
    }

    periods <- make_periods(df)
    changes <- make_change_summary(df)
    latest_points <- make_latest_points(df)
    name_history <- make_name_history(df)

    out <- df |>
      dplyr::group_by(AirportID) |>
      dplyr::summarize(
        airport_cities = collapse_display(AirportCity),
        n_airport_cities = dplyr::n_distinct(AirportCity),
        airport_codes = collapse_inline(AirportCode),
        n_airport_codes = dplyr::n_distinct(AirportCode),
        airlines = collapse_display(Reporting_Airline_Name),
        FlightsFromSLC = sum(Flights[Direction == "from_slc"]),
        FlightsToSLC = sum(Flights[Direction == "to_slc"]),
        FlightCount = FlightsFromSLC + FlightsToSLC,
        ServiceDates = list(
          sort(
            unique(
              do.call(c, ServiceDates)
            )
          )
        ),
        .groups = "drop"
      ) |>
      dplyr::left_join(periods, by = "AirportID") |>
      dplyr::left_join(changes, by = "AirportID") |>
      dplyr::left_join(latest_points, by = "AirportID") |>
      dplyr::left_join(name_history, by = "AirportID") |>
      dplyr::left_join(current_airports, by = "AirportID") |>
      dplyr::arrange(MarkerCode, MarkerName)

    # Binning follows the displayed map selection. The current view may
    # contain the full period, one year, or one month.
    bin_counts <- df |>
      dplyr::group_by(AirportID) |>
      dplyr::summarize(
        FlightCount = sum(Flights),
        .groups = "drop"
      )

    flight_breaks <- make_flight_breaks(bin_counts$FlightCount)

    show_status <- all_years_selected() || all_months_selected()

    show_daily <- identical(input$time_level, "month")

    # Keep hover text short. Full airport history is shown only after a
    # user clicks a point.
    out |>
      dplyr::mutate(
        PointKey = paste0("point_", dplyr::row_number()),
        FlightClass = make_flight_classes(
          FlightCount,
          flight_breaks
        ),
        RouteType = dplyr::case_when(
          FlightsFromSLC > 0L & FlightsToSLC > 0L ~
            "bidirectional",
          FlightsFromSLC > 0L ~ "from_slc",
          FlightsToSLC > 0L ~ "to_slc"
        ),
        period_label = dplyr::if_else(
          n_periods == 1L,
          "Service period:",
          "Service periods:"
        ),
        service_block = if (show_daily) {
          mapply(
            make_daily_block,
            ServiceDates,
            USE.NAMES = FALSE
          )
        } else {
          paste0(
            "<b>",
            period_label,
            "</b><br>",
            service_periods
          )
        },
        airport_status = if (show_status) {
          mapply(
            make_status_block,
            AirportIsClosed,
            AirportThruDate,
            USE.NAMES = FALSE
          )
        } else {
          ""
        },
        current_identity = mapply(
          make_current_identity_block,
          CurrentAirportName,
          CurrentAirportCode,
          CurrentAirportIsClosed,
          USE.NAMES = FALSE
        ),
        airport_name_block = mapply(
          make_name_block,
          service_airport_names,
          n_service_airport_names,
          USE.NAMES = FALSE
        ),
        airport_city_label = dplyr::if_else(
          n_airport_cities == 1L,
          "Airport city:",
          "Airport cities:"
        ),
        airport_code_block = mapply(
          make_code_block,
          airport_codes,
          n_airport_codes,
          USE.NAMES = FALSE
        ),
        airport_notes = mapply(
          make_notes,
          n_large_shifts,
          n_name_changes,
          CurrentNameChangedAfterLastSLCService,
          CurrentAirportCode,
          CurrentCodeReplacementAirportName,
          USE.NAMES = FALSE
        ),
        flight_hover = mapply(
          make_hover_flights,
          FlightsFromSLC,
          FlightsToSLC,
          USE.NAMES = FALSE
        ),
        flight_block = mapply(
          make_flight_block,
          FlightsFromSLC,
          FlightsToSLC,
          USE.NAMES = FALSE
        ),
        Hover = paste0(
          "<b>", MarkerName, "</b><br>",
          MarkerCode, "<br>",
          flight_hover,
          "<br>",
          "Click for details"
        ),
        Details = paste0(
          current_identity,
          airport_name_block,
          airport_code_block,
          airport_status,
          "<br><b>",
          airport_city_label,
          "</b><br>",
          airport_cities,
          "<br>",
          flight_block,
          "<br><b>Serviced by:</b><br>",
          airlines,
          "<br>",
          service_block,
          airport_notes
        )
      )
  })

  airport_time_df <- shiny::reactive({
    flight_data() |>
      dplyr::group_by(Year) |>
      dplyr::summarize(
        Count = dplyr::n_distinct(AirportID),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        Year = as.numeric(Year),
        Hover = paste0(
          "Year: ",
          Year,
          "<br>Connected airports: ",
          Count,
          "<br>Click to view airport list"
        )
      ) |>
      dplyr::arrange(Year) |>
      dplyr::mutate(
        year_gap = Year - dplyr::lag(Year),
        new_segment = is.na(year_gap) | year_gap > 1,
        SegmentID = cumsum(new_segment)
      )
  })

  airline_time_df <- shiny::reactive({
    flight_data() |>
      dplyr::group_by(Year) |>
      dplyr::summarize(
        Count = dplyr::n_distinct(Reporting_Airline_Name),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        Year = as.numeric(Year),
        Hover = paste0(
          "Year: ",
          Year,
          "<br>Airlines: ",
          Count,
          "<br>Click to view airline list"
        )
      ) |>
      dplyr::arrange(Year) |>
      dplyr::mutate(
        year_gap = Year - dplyr::lag(Year),
        new_segment = is.na(year_gap) | year_gap > 1,
        SegmentID = cumsum(new_segment)
      )
  })

  airport_month_df <- shiny::reactive({
    month_counts <- flight_data() |>
      dplyr::group_by(ServiceMonth) |>
      dplyr::summarize(
        Count = dplyr::n_distinct(AirportID),
        .groups = "drop"
      )

    month_frame |>
      dplyr::left_join(
        month_counts,
        by = "ServiceMonth"
      ) |>
      dplyr::arrange(MonthIndex) |>
      dplyr::mutate(
        has_data = !is.na(Count),
        new_segment = has_data &
          !dplyr::lag(
            has_data,
            default = FALSE
          ),
        SegmentID = cumsum(new_segment),
        Hover = dplyr::if_else(
          has_data,
          paste0(
            "Month: ",
            format(ServiceMonth, "%B %Y"),
            "<br>Connected airports: ",
            Count,
            "<br>Click to view airport list"
          ),
          ""
        )
      ) |>
      dplyr::filter(has_data)
  })

  airline_month_df <- shiny::reactive({
    month_counts <- flight_data() |>
      dplyr::group_by(ServiceMonth) |>
      dplyr::summarize(
        Count = dplyr::n_distinct(
          Reporting_Airline_Name
        ),
        .groups = "drop"
      )

    month_frame |>
      dplyr::left_join(
        month_counts,
        by = "ServiceMonth"
      ) |>
      dplyr::arrange(MonthIndex) |>
      dplyr::mutate(
        has_data = !is.na(Count),
        new_segment = has_data &
          !dplyr::lag(
            has_data,
            default = FALSE
          ),
        SegmentID = cumsum(new_segment),
        Hover = dplyr::if_else(
          has_data,
          paste0(
            "Month: ",
            format(ServiceMonth, "%B %Y"),
            "<br>Airlines: ",
            Count,
            "<br>Click to view airline list"
          ),
          ""
        )
      ) |>
      dplyr::filter(has_data)
  })

  output$airport_time_series <- plotly::renderPlotly({
    df <- airport_time_df()

    shiny::validate(
      shiny::need(
        nrow(df) > 0,
        "No airport time-series data to display for this selection."
      )
    )

    selected_year <- NULL

    if (one_year_selected()) {
      selected_year <- input$year
    }

    make_time_plot(
      df = df,
      title = "Connected Airports by Year",
      y_label = "Number of Connected Airports",
      y_top = 140,
      y_breaks = seq(0, 140, 20),
      years = years,
      year_min = year_min,
      year_max = year_max,
      year = selected_year,
      source = "airport_time_series"
    )
  })

  output$airline_time_series <- plotly::renderPlotly({
    df <- airline_time_df()

    shiny::validate(
      shiny::need(
        nrow(df) > 0,
        "No airline time-series data to display for this selection."
      )
    )

    selected_year <- NULL

    if (one_year_selected()) {
      selected_year <- input$year
    }

    make_time_plot(
      df = df,
      title = "Airlines by Year",
      y_label = "Number of Airlines",
      y_top = 20,
      y_breaks = seq(0, 20, 5),
      years = years,
      year_min = year_min,
      year_max = year_max,
      year = selected_year,
      source = "airline_time_series"
    )
  })

  output$airport_month_series <- plotly::renderPlotly({
    df <- airport_month_df()

    shiny::validate(
      shiny::need(
        nrow(df) > 0,
        "No airport time-series data to display for this selection."
      )
    )

    selected_id <- NULL

    if (one_month_selected()) {
      selected_id <- as.integer(input$month_index)
    }

    make_month_plot(
      df = df,
      title = "Connected Airports by Month",
      y_label = "Number of Connected Airports",
      y_top = 140,
      y_breaks = seq(0, 140, 20),
      month_ids = month_ids,
      month_labels = month_labels,
      month_id = selected_id,
      month_range = visible_months(),
      source = "airport_month_series"
    )
  })

  output$airline_month_series <- plotly::renderPlotly({
    df <- airline_month_df()

    shiny::validate(
      shiny::need(
        nrow(df) > 0,
        "No airline time-series data to display for this selection."
      )
    )

    selected_id <- NULL

    if (one_month_selected()) {
      selected_id <- as.integer(input$month_index)
    }

    make_month_plot(
      df = df,
      title = "Airlines by Month",
      y_label = "Number of Airlines",
      y_top = 20,
      y_breaks = seq(0, 20, 5),
      month_ids = month_ids,
      month_labels = month_labels,
      month_id = selected_id,
      month_range = visible_months(),
      source = "airline_month_series"
    )
  })

  shiny::outputOptions(
    output,
    "airport_month_series",
    suspendWhenHidden = FALSE
  )

  shiny::outputOptions(
    output,
    "airline_month_series",
    suspendWhenHidden = FALSE
  )

  output$map <- plotly::renderPlotly({
    df <- unique_airports()

    shiny::validate(
      shiny::need(
        nrow(df) > 0,
        "No airports to display for this selection."
      ),
      shiny::need(
        all(c("Longitude", "Latitude", "Hover") %in% names(df)),
        "Map data are missing Longitude, Latitude, or Hover."
      )
    )

    plot_df <- df |>
      dplyr::select(
        PointKey,
        Longitude,
        Latitude,
        Hover,
        FlightCount,
        FlightClass,
        RouteType
      )

    flight_levels <- levels(plot_df$FlightClass)
    flight_cols <- make_flight_colors(length(flight_levels))
    color_map <- stats::setNames(flight_cols, flight_levels)

    hover_text_cols <- stats::setNames(
      c("black", "black", "white", "white", "white"),
      flight_levels
    )

    p <- plotly::plot_ly(source = "network_map")

    # Route lines are optional and non-interactive. They are drawn first
    # so airport markers and the SLC hub remain visually on top.
    #
    # Route color and style encode direction. Light blue solid lines show
    # service in both directions. Dark blue dashed lines show service from
    # SLC only, and dark blue dotted lines show service to SLC only.
    if (isTRUE(input$show_routes)) {
      for (level in flight_levels) {
        for (route_type in route_types) {
          class_df <- plot_df |>
            dplyr::filter(
              FlightClass == level,
              RouteType == route_type
            )

          if (nrow(class_df) == 0L) {
            next
          }

          route_df <- make_route_lines(class_df, slc_point)

          p <- p |>
            plotly::add_trace(
              data = route_df,
              type = "scattergeo",
              mode = "lines",
              lon = ~Longitude,
              lat = ~Latitude,
              hoverinfo = "skip",
              name = level,
              legendgroup = level,
              showlegend = FALSE,
              line = list(
                color = route_colors[[route_type]],
                dash = route_dashes[[route_type]],
                width = route_line_width
              ),
              inherit = FALSE
            )
        }
      }
    }

    # Add one marker trace per flight-count class. Empty traces keep all
    # five legend classes visible even when a class is unused.
    for (level in flight_levels) {
      class_df <- plot_df |>
        dplyr::filter(FlightClass == level)

      if (nrow(class_df) == 0L) {
        class_df <- data.frame(
          PointKey = NA_character_,
          Longitude = NA_real_,
          Latitude = NA_real_,
          Hover = "",
          FlightCount = NA_integer_,
          FlightClass = factor(level, levels = flight_levels)
        )

        p <- p |>
          plotly::add_trace(
            data = class_df,
            type = "scattergeo",
            mode = "markers",
            lon = ~Longitude,
            lat = ~Latitude,
            text = ~Hover,
            customdata = ~PointKey,
            hoverinfo = "skip",
            name = level,
            legendgroup = level,
            showlegend = TRUE,
            marker = list(
              color = color_map[[level]],
              size = airport_marker_size,
              line = list(
                color = "grey30",
                width = 0.4
              )
            ),
            inherit = FALSE
          )
      } else {
        p <- p |>
          plotly::add_trace(
            data = class_df,
            type = "scattergeo",
            mode = "markers",
            lon = ~Longitude,
            lat = ~Latitude,
            text = ~Hover,
            customdata = ~PointKey,
            hoverinfo = "text",
            name = level,
            legendgroup = level,
            showlegend = TRUE,
            marker = list(
              color = color_map[[level]],
              size = airport_marker_size,
              line = list(
                color = "grey30",
                width = 0.4
              )
            ),
            hoverlabel = list(
              bgcolor = color_map[[level]],
              font = list(
                color = hover_text_cols[[level]]
              )
            ),
            inherit = FALSE
          )
      }
    }

    # Add SLC as a hoverable reference hub. It has no customdata, so the
    # click observer ignores it and does not open the airport panel.
    p <- p |>
      plotly::add_trace(
        data = slc_point,
        type = "scattergeo",
        mode = "markers",
        lon = ~Longitude,
        lat = ~Latitude,
        text = ~Hover,
        hoverinfo = "text",
        name = "SLC",
        showlegend = FALSE,
        marker = list(
          color = slc_marker_color,
          size = slc_marker_size,
          symbol = slc_marker_symbol,
          line = list(
            color = "grey30",
            width = 0.8
          )
        ),
        hoverlabel = list(
          bgcolor = slc_marker_color,
          font = list(
            color = "white"
          )
        ),
        inherit = FALSE
      )

    p |>
      plotly::layout(
        geo = list(
          scope = "usa"
        ),
        legend = list(
          title = list(
            text = make_legend_title(
              input = input,
              year_min = year_min,
              year_max = year_max,
              service_months = service_months
            )
          ),
          x = 1.02,
          y = 0.5
        )
      )
  })

  output$selected_airport <- shiny::renderUI({
    key <- selected_point()

    if (is.null(key)) {
      return(shiny::tags$p("Click an airport point to view details."))
    }

    df <- unique_airports()

    selected <- df |>
      dplyr::filter(PointKey == key)

    if (nrow(selected) != 1L) {
      selected_point(NULL)
      return(shiny::tags$p("Click an airport point to view details."))
    }

    shiny::HTML(selected$Details[[1]])
  })
}

shiny::shinyApp(ui = ui, server = server)
