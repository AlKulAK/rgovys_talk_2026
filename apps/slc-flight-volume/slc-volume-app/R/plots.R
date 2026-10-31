# --------------------------------------------------
# Date-level plotting helpers
# --------------------------------------------------

avg_line_color <- "#08306B"
threshold_line_color <- "#6BAED6"

make_date_axis <- function(dates, break_years) {
  if (length(dates) == 0L) {
    stop("Cannot build a date axis from an empty date vector.")
  }

  if (anyNA(dates)) {
    stop(
      "Cannot build a date axis when `FlightDate` contains missing values."
    )
  }

  date_range <- range(dates)
  first_year <- as.integer(format(date_range[1], "%Y"))
  last_year <- as.integer(format(date_range[2], "%Y"))

  axis_limits <- as.Date(
    paste0(
      c(first_year, last_year + 1L),
      "-01-01"
    )
  )

  break_year_seq <- seq(
    from = first_year,
    to = last_year + 1L,
    by = break_years
  )

  axis_breaks <- as.Date(
    paste0(
      break_year_seq,
      "-01-01"
    )
  )

  list(
    limits = axis_limits,
    breaks = axis_breaks
  )
}

plot_main_date_level <- function(plot_df) {
  date_axis <- make_date_axis(
    dates = plot_df$FlightDate,
    break_years = 10L
  )
  plot_df <- plot_df |>
    dplyr::mutate(
      hover_text = paste0(
        "Date: ", FlightDate,
        "<br>Category: ", category,
        "<br>Main flights that date: ", daily_flights,
        "<br>Average main flights/day in ", Year, ": ",
        round(avg_daily_year, 1),
        "<br>This date vs. same-year average day: ",
        round(pct_of_avg_day, 1),
        "%"
      )
    )

  p <- ggplot2::ggplot(
    plot_df,
    ggplot2::aes(
      x = FlightDate,
      y = pct_of_avg_day,
      text = hover_text
    )
  ) +
    ggplot2::geom_hline(
      yintercept = 100,
      linetype = 5,
      color = avg_line_color
    ) +
    ggplot2::geom_point(
      alpha = 0.75,
      size = 0.95
    ) +
    ggplot2::facet_wrap(
      ~category,
      nrow = 4,
      ncol = 4,
      scales = "fixed"
    ) +
    ggplot2::scale_x_date(
      limits = date_axis$limits,
      breaks = date_axis$breaks,
      date_labels = "%Y"
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, 200),
      breaks = seq(0, 200, by = 50)
    ) +
    ggplot2::labs(
      x = NULL,
      y = "Main daily flights (% of same-year average day)"
    ) +
    ggplot2::theme(
      legend.position = "none",
      panel.spacing.y = grid::unit(0.75, "lines"),
      plot.title = ggplot2::element_blank()
    )

  plotly::ggplotly(
    p,
    tooltip = "text"
  ) |>
    plotly::layout(
      margin = list(
        t = 20,
        b = 20
      )
    )
}

plot_div_date_outliers <- function(plot_df, threshold = 750) {
  date_axis <- make_date_axis(
    dates = plot_df$FlightDate,
    break_years = 5L
  )
  plot_df <- plot_df |>
    dplyr::mutate(
      is_outlier = pct_of_avg_day > threshold,
      hover_text = paste0(
        "Date: ", FlightDate,
        "<br>Category: ", category,
        "<br>Diverted flights that date: ", daily_flights,
        "<br>Average diverted flights/day in ", Year, ": ",
        round(avg_daily_year, 1),
        "<br>This date vs. same-year average day: ",
        round(pct_of_avg_day, 1),
        "%"
      )
    )

  p <- ggplot2::ggplot(
    plot_df,
    ggplot2::aes(
      x = FlightDate,
      y = pct_of_avg_day,
      text = hover_text
    )
  ) +
    ggplot2::geom_hline(
      yintercept = 100,
      linetype = 5,
      color = avg_line_color
    ) +
    ggplot2::geom_hline(
      yintercept = threshold,
      linetype = 2,
      color = threshold_line_color
    ) +
    ggplot2::geom_point(
      data = plot_df |>
        dplyr::filter(!is_outlier),
      alpha = 0.75,
      size = 0.95
    ) +
    ggplot2::geom_point(
      data = plot_df |>
        dplyr::filter(is_outlier),
      color = "red",
      alpha = 0.9,
      size = 0.95
    ) +
    ggplot2::facet_wrap(
      ~category,
      nrow = 4,
      ncol = 4,
      scales = "fixed"
    ) +
    ggplot2::scale_x_date(
      limits = date_axis$limits,
      breaks = date_axis$breaks,
      date_labels = "%Y"
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, 1000),
      breaks = seq(0, 1000, by = 250)
    ) +
    ggplot2::labs(
      x = NULL,
      y = "Diverted daily flights (% of same-year average day)"
    ) +
    ggplot2::theme(
      legend.position = "none",
      panel.spacing.y = grid::unit(0.75, "lines"),
      plot.title = ggplot2::element_blank()
    )

  plotly::ggplotly(
    p,
    tooltip = "text"
  ) |>
    plotly::layout(
      margin = list(
        t = 20,
        b = 20
      )
    )
}
