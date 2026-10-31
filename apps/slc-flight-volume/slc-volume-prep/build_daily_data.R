# Deployment data preparator for SLC Flight Volume Explorer
# 06.20.2026 version
#
# This script must be run from /slc-flight-volume/slc-volume-prep.
# Necessary runtime packages: dplyr, lubridate, slcflights
#
# This script is intended to be run locally, not deployed to shinyapps.io.
# It uses the slcflights package to (re)generate the .rds files consumed by the
# app.

# --------------------------------------------------
# Holiday calendar helpers
# --------------------------------------------------

nth_weekday_of_month <- function(year, month, weekday, n) {
  first_day <- as.Date(sprintf("%04d-%02d-01", year, month))
  first_wday <- lubridate::wday(first_day, week_start = 1)

  first_day + ((weekday - first_wday) %% 7) + 7L * (n - 1L)
}

last_weekday_of_month <- function(year, month, weekday) {
  if (month == 12L) {
    next_month <- as.Date(sprintf("%04d-01-01", year + 1L))
  } else {
    next_month <- as.Date(sprintf("%04d-%02d-01", year, month + 1L))
  }

  last_day <- next_month - 1L
  last_wday <- lubridate::wday(last_day, week_start = 1)

  last_day - ((last_wday - weekday) %% 7)
}

holiday_levels <- function() {
  c(
    "Monday",
    "Tuesday",
    "Wednesday",
    "Thursday",
    "Friday",
    "Saturday",
    "Sunday",
    "New Year's Day",
    "Martin Luther King Jr. Day",
    "Presidents' Day",
    "Memorial Day",
    "Independence Day",
    "Labor Day",
    "Veterans Day",
    "Thanksgiving Day",
    "Christmas Day"
  )
}

make_holiday_table <- function(years) {
  years <- sort(unique(as.integer(years)))

  holiday_names <- c(
    "New Year's Day",
    "Martin Luther King Jr. Day",
    "Presidents' Day",
    "Memorial Day",
    "Independence Day",
    "Labor Day",
    "Veterans Day",
    "Thanksgiving Day",
    "Christmas Day"
  )

  holiday_dates <- c(
    as.Date(sprintf("%04d-01-01", years)),
    nth_weekday_of_month(years, 1L, 1L, 3L),
    nth_weekday_of_month(years, 2L, 1L, 3L),
    last_weekday_of_month(years, 5L, 1L),
    as.Date(sprintf("%04d-07-04", years)),
    nth_weekday_of_month(years, 9L, 1L, 1L),
    as.Date(sprintf("%04d-11-11", years)),
    nth_weekday_of_month(years, 11L, 4L, 4L),
    as.Date(sprintf("%04d-12-25", years))
  )

  data.frame(
    Year = rep(years, times = length(holiday_names)),
    FlightDate = holiday_dates,
    category = factor(
      rep(holiday_names, each = length(years)),
      levels = holiday_levels()
    )
  )
}

# --------------------------------------------------
# Daily flight-volume data builder
# --------------------------------------------------

make_day_type_daily_data <- function(type = c("main", "div"),
                                     years = NULL) {
  type <- match.arg(type)

  if (is.null(years)) {
    years <- sort(unique(slcflights::available_years(type)))
  } else {
    years <- sort(unique(as.integer(years)))
  }

  open_fun <- switch(type,
    main = slcflights::open_main,
    div = slcflights::open_div
  )

  # Main records are scheduled SLC origin/destination flights.
  # Diversion-only records are flights where SLC appears as a
  # diversion airport.
  daily <- open_fun(years) |>
    dplyr::select(Year, FlightDate, Flights) |>
    dplyr::group_by(Year, FlightDate) |>
    dplyr::summarise(
      daily_flights = sum(Flights),
      .groups = "drop"
    ) |>
    dplyr::collect() |>
    dplyr::mutate(
      FlightDate = as.Date(FlightDate),
      category = factor(
        lubridate::wday(FlightDate, week_start = 1),
        levels = 1:7,
        labels = c(
          "Monday",
          "Tuesday",
          "Wednesday",
          "Thursday",
          "Friday",
          "Saturday",
          "Sunday"
        )
      )
    )

  # Same-year normalization keeps the percentage comparable across
  # years with different baseline flight volumes.
  year_avg <- daily |>
    dplyr::group_by(Year) |>
    dplyr::summarise(
      avg_daily_year = mean(daily_flights),
      .groups = "drop"
    )

  weekday_dates <- daily |>
    dplyr::transmute(
      Year,
      FlightDate,
      category,
      daily_flights
    )

  # Holiday dates are intentionally added in addition to their weekday
  # rows.
  holiday_dates <- daily |>
    dplyr::select(-category) |>
    dplyr::inner_join(
      make_holiday_table(years),
      by = c("Year", "FlightDate")
    ) |>
    dplyr::transmute(
      Year,
      FlightDate,
      category,
      daily_flights
    )

  dplyr::bind_rows(weekday_dates, holiday_dates) |>
    dplyr::left_join(year_avg, by = "Year") |>
    dplyr::mutate(
      pct_of_avg_day = 100 * daily_flights / avg_daily_year,
      category = factor(
        as.character(category),
        levels = holiday_levels()
      )
    )
}

# --------------------------------------------------
# Build and write deployed app data
# --------------------------------------------------

out_dir <- file.path("..", "slc-volume-app", "data")

dir.create(
  path = out_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

main_daily <- make_day_type_daily_data(type = "main")
div_daily <- make_day_type_daily_data(type = "div")

saveRDS(
  object = main_daily,
  file = file.path(out_dir, "main_daily.rds")
)

saveRDS(
  object = div_daily,
  file = file.path(out_dir, "div_daily.rds")
)

# --------------------------------------------------
# Build report
# --------------------------------------------------

message("Wrote derived app data:")
message("  ", file.path(out_dir, "main_daily.rds"))
message("  ", file.path(out_dir, "div_daily.rds"))

message("Rows:")
message("  main_daily: ", nrow(main_daily))
message("  div_daily:  ", nrow(div_daily))

message("Date ranges:")
message(
  "  main_daily: ",
  min(main_daily$FlightDate),
  " to ",
  max(main_daily$FlightDate)
)
message(
  "  div_daily:  ",
  min(div_daily$FlightDate),
  " to ",
  max(div_daily$FlightDate)
)
