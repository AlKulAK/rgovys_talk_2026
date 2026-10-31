# Prepare diversion-only BTS flight events for weather linkage.
# 2026-09-03

check_div_years <- function(years) {
  if (!is.numeric(years) ||
    length(years) == 0L ||
    anyNA(years) ||
    any(!is.finite(years)) ||
    any(years != floor(years))) {
    stop(
      "`years` must contain finite whole-number years."
    )
  }

  years <- sort(
    unique(
      as.integer(years)
    )
  )

  avail_years <- slcflights::available_years(
    type = "div"
  )

  missing_years <- setdiff(
    years,
    avail_years
  )

  if (length(missing_years) > 0L) {
    stop(
      "At least one requested diversion year is unavailable."
    )
  }

  years
}


normalize_bts_id <- function(
  x,
  field
) {
  if (!is.character(x) &&
    !is.numeric(x)) {
    stop(
      paste0(
        "`",
        field,
        "` must be character or numeric."
      )
    )
  }

  if (is.character(x)) {
    bad_format <- !is.na(x) &
      !grepl(
        "^[0-9]+$",
        x
      )

    if (any(bad_format)) {
      stop(
        paste0(
          "`",
          field,
          "` contains a non-integer identifier."
        )
      )
    }

    value <- rep(
      NA_real_,
      length(x)
    )

    present <- !is.na(x)

    value[present] <- as.numeric(
      x[present]
    )
  } else {
    value <- as.numeric(x)
  }

  bad_value <- !is.na(value) &
    (
      !is.finite(value) |
        value != floor(value) |
        value < 0 |
        value > .Machine$integer.max
    )

  if (any(bad_value)) {
    stop(
      paste0(
        "`",
        field,
        "` contains an invalid integer identifier."
      )
    )
  }

  as.integer(value)
}


normalize_whole_num <- function(
  x,
  field
) {
  if (!is.character(x) &&
    !is.numeric(x)) {
    stop(
      paste0(
        "`",
        field,
        "` must be character or numeric."
      )
    )
  }

  if (is.character(x)) {
    bad_format <- !is.na(x) &
      !grepl(
        "^[0-9]+(\\.0+)?$",
        x
      )

    if (any(bad_format)) {
      stop(
        paste0(
          "`",
          field,
          "` contains a non-whole-number value."
        )
      )
    }

    value <- rep(
      NA_real_,
      length(x)
    )

    present <- !is.na(x)

    value[present] <- as.numeric(
      x[present]
    )
  } else {
    value <- as.numeric(x)
  }

  bad_value <- !is.na(value) &
    (
      !is.finite(value) |
        value != floor(value) |
        value < 0 |
        value > .Machine$integer.max
    )

  if (any(bad_value)) {
    stop(
      paste0(
        "`",
        field,
        "` contains an invalid whole-number value."
      )
    )
  }

  as.integer(value)
}


parse_bts_flag <- function(
  x,
  field
) {
  value <- normalize_whole_num(
    x,
    field
  )

  if (anyNA(value) ||
    any(!value %in% c(0L, 1L))) {
    stop(
      paste0(
        "`",
        field,
        "` must contain only 0 and 1."
      )
    )
  }

  value == 1L
}


parse_div_clock <- function(
  x,
  field
) {
  if (!is.character(x) ||
    anyNA(x)) {
    stop(
      paste0(
        "`",
        field,
        "` must contain non-missing character times."
      )
    )
  }

  format_ok <- grepl(
    "^[0-9]{4}$",
    x
  )

  if (any(!format_ok)) {
    stop(
      paste0(
        "At least one `",
        field,
        "` value is not four-digit hhmm."
      )
    )
  }

  time_num <- as.integer(x)
  hour <- time_num %/% 100L
  minute <- time_num %% 100L

  invalid_time <- hour > 24L |
    minute > 59L |
    (
      hour == 24L &
        minute != 0L
    )

  if (any(invalid_time)) {
    stop(
      paste0(
        "At least one `",
        field,
        "` value is not a valid BTS local time."
      )
    )
  }

  tibble::tibble(
    clock = dplyr::if_else(
      x == "2400",
      "0000",
      x
    ),
    raw_min = hour * 60L + minute,
    day_add = as.integer(
      x == "2400"
    )
  )
}


resolve_div_clock <- function(
  date,
  clock,
  timezone
) {
  if (!inherits(date, "Date") ||
    anyNA(date)) {
    stop(
      "`date` must contain non-missing Date values."
    )
  }

  if (!is.character(clock) ||
    anyNA(clock)) {
    stop(
      "`clock` must contain non-missing character times."
    )
  }

  if (!is.character(timezone) ||
    length(timezone) != 1L ||
    is.na(timezone) ||
    !nzchar(timezone)) {
    stop(
      "`timezone` must be one non-empty character value."
    )
  }

  if (length(date) != length(clock)) {
    stop(
      "`date` and `clock` must have the same length."
    )
  }

  clock_text <- paste0(
    as.character(date),
    " ",
    substr(
      clock,
      1L,
      2L
    ),
    ":",
    substr(
      clock,
      3L,
      4L
    ),
    ":00"
  )

  # Parse civil fields before assigning a local DST occurrence.
  clock_time <- lubridate::ymd_hms(
    clock_text,
    tz = "UTC"
  )

  if (anyNA(clock_time)) {
    stop(
      "At least one local event time failed to parse."
    )
  }

  pre_local <- lubridate::force_tz(
    clock_time,
    tzone = timezone,
    roll_dst = c(
      "pre",
      "pre"
    )
  )

  post_local <- lubridate::force_tz(
    clock_time,
    tzone = timezone,
    roll_dst = c(
      "post",
      "post"
    )
  )

  event_local <- lubridate::force_tz(
    clock_time,
    tzone = timezone,
    roll_dst = c(
      "NA",
      "NA"
    )
  )

  if (anyNA(pre_local) ||
    anyNA(post_local)) {
    stop(
      "A local event time failed during DST checking."
    )
  }

  clock_key <- paste(
    as.character(date),
    clock
  )

  pre_key <- format(
    pre_local,
    format = "%Y-%m-%d %H%M",
    tz = timezone
  )

  post_key <- format(
    post_local,
    format = "%Y-%m-%d %H%M",
    tz = timezone
  )

  dst_problem <- is.na(
    event_local
  )

  dst_ambiguous <- dst_problem &
    pre_key == clock_key &
    post_key == clock_key

  dst_nonexistent <- dst_problem &
    !dst_ambiguous

  time_status <- dplyr::case_when(
    dst_ambiguous ~ "ambiguous",
    dst_nonexistent ~ "nonexistent",
    TRUE ~ "valid"
  )

  event_utc <- lubridate::with_tz(
    event_local,
    tzone = "UTC"
  )

  tibble::tibble(
    event_utc = event_utc,
    time_status = time_status
  )
}


lookup_airport_tz <- function(
  latitude,
  longitude
) {
  if (!is.numeric(latitude) ||
    !is.numeric(longitude)) {
    stop(
      "Airport latitude and longitude must be numeric."
    )
  }

  if (length(latitude) !=
    length(longitude)) {
    stop(
      "Airport coordinates must have the same length."
    )
  }

  bad_coord <- is.na(latitude) |
    is.na(longitude) |
    !is.finite(latitude) |
    !is.finite(longitude) |
    latitude < -90 |
    latitude > 90 |
    longitude < -180 |
    longitude > 180

  if (any(bad_coord)) {
    stop(
      "At least one airport coordinate is invalid."
    )
  }

  coord_tbl <- tibble::tibble(
    latitude = latitude,
    longitude = longitude
  )

  lookup_tbl <- coord_tbl |>
    dplyr::distinct(
      latitude,
      longitude
    )

  lookup_tbl$timezone <- lutz::tz_lookup_coords(
    lat = lookup_tbl$latitude,
    lon = lookup_tbl$longitude,
    method = "accurate"
  )

  if (anyNA(lookup_tbl$timezone) ||
    any(!nzchar(lookup_tbl$timezone))) {
    stop(
      "At least one airport time zone could not be resolved."
    )
  }

  if (any(
    grepl(
      ";",
      lookup_tbl$timezone,
      fixed = TRUE
    )
  )) {
    stop(
      "At least one airport has an ambiguous time-zone lookup."
    )
  }

  unknown_tz <- setdiff(
    unique(
      lookup_tbl$timezone
    ),
    OlsonNames()
  )

  if (length(unknown_tz) > 0L) {
    print(
      unknown_tz
    )

    stop(
      "At least one resolved time zone is unknown to R."
    )
  }

  coord_tbl |>
    dplyr::left_join(
      lookup_tbl,
      by = c(
        "latitude",
        "longitude"
      ),
      relationship = "many-to-one"
    ) |>
    dplyr::pull(
      timezone
    )
}


resolve_local_utc <- function(
  date,
  clock,
  timezone,
  field
) {
  if (!inherits(date, "Date")) {
    stop(
      "`date` must be Date."
    )
  }

  if (!is.character(clock) ||
    !is.character(timezone)) {
    stop(
      "`clock` and `timezone` must be character."
    )
  }

  input_n <- length(date)

  if (length(clock) != input_n ||
    length(timezone) != input_n) {
    stop(
      "Local-time inputs must have the same length."
    )
  }

  event_date <- rep(
    as.Date(NA),
    input_n
  )

  event_clock <- rep(
    NA_character_,
    input_n
  )

  event_utc <- as.POSIXct(
    rep(
      NA_real_,
      input_n
    ),
    origin = "1970-01-01",
    tz = "UTC"
  )

  time_status <- rep(
    "source_unresolved",
    input_n
  )

  has_clock <- !is.na(
    clock
  )

  day_add <- rep(
    NA_integer_,
    input_n
  )

  if (any(has_clock)) {
    parsed <- parse_div_clock(
      clock[has_clock],
      field
    )

    event_clock[has_clock] <-
      parsed$clock

    day_add[has_clock] <-
      parsed$day_add
  }

  ready <- !is.na(date) &
    has_clock &
    !is.na(timezone) &
    nzchar(timezone)

  if (any(ready)) {
    groups <- split(
      which(ready),
      timezone[ready]
    )

    for (zone in names(groups)) {
      row_id <- groups[[zone]]

      local_date <- date[row_id] +
        day_add[row_id]

      resolved <- resolve_div_clock(
        date = local_date,
        clock = event_clock[row_id],
        timezone = zone
      )

      event_date[row_id] <- local_date
      event_utc[row_id] <- resolved$event_utc
      time_status[row_id] <- resolved$time_status
    }
  }

  tibble::tibble(
    event_date = event_date,
    event_clock = event_clock,
    event_utc = event_utc,
    time_status = time_status
  )
}


utc_local_parts <- function(
  event_utc,
  timezone
) {
  if (!inherits(
    event_utc,
    "POSIXct"
  )) {
    stop(
      "`event_utc` must be POSIXct."
    )
  }

  if (!is.character(timezone)) {
    stop(
      "`timezone` must be character."
    )
  }

  if (length(event_utc) !=
    length(timezone)) {
    stop(
      "UTC times and time zones must have the same length."
    )
  }

  input_n <- length(event_utc)

  event_date <- rep(
    as.Date(NA),
    input_n
  )

  event_clock <- rep(
    NA_character_,
    input_n
  )

  ready <- !is.na(event_utc) &
    !is.na(timezone) &
    nzchar(timezone)

  if (any(ready)) {
    groups <- split(
      which(ready),
      timezone[ready]
    )

    for (zone in names(groups)) {
      row_id <- groups[[zone]]

      local_time <- lubridate::with_tz(
        event_utc[row_id],
        tzone = zone
      )

      event_date[row_id] <- as.Date(
        local_time,
        tz = zone
      )

      event_clock[row_id] <- format(
        local_time,
        format = "%H%M",
        tz = zone
      )
    }
  }

  tibble::tibble(
    event_date = event_date,
    event_clock = event_clock
  )
}


resolve_after_utc <- function(
  anchor_utc,
  clock,
  timezone,
  field
) {
  if (!inherits(
    anchor_utc,
    "POSIXct"
  )) {
    stop(
      "`anchor_utc` must be POSIXct."
    )
  }

  if (!is.character(clock) ||
    !is.character(timezone)) {
    stop(
      "`clock` and `timezone` must be character."
    )
  }

  input_n <- length(anchor_utc)

  if (length(clock) != input_n ||
    length(timezone) != input_n) {
    stop(
      "Event-time inputs must have the same length."
    )
  }

  event_date <- rep(
    as.Date(NA),
    input_n
  )

  event_clock <- rep(
    NA_character_,
    input_n
  )

  event_utc <- as.POSIXct(
    rep(
      NA_real_,
      input_n
    ),
    origin = "1970-01-01",
    tz = "UTC"
  )

  time_status <- rep(
    "source_unresolved",
    input_n
  )

  has_clock <- !is.na(
    clock
  )

  day_add <- rep(
    NA_integer_,
    input_n
  )

  if (any(has_clock)) {
    parsed <- parse_div_clock(
      clock[has_clock],
      field
    )

    event_clock[has_clock] <-
      parsed$clock

    day_add[has_clock] <-
      parsed$day_add
  }

  ready <- !is.na(anchor_utc) &
    has_clock &
    !is.na(timezone) &
    nzchar(timezone)

  if (any(ready)) {
    groups <- split(
      which(ready),
      timezone[ready]
    )

    for (zone in names(groups)) {
      row_id <- groups[[zone]]

      anchor_local <- lubridate::with_tz(
        anchor_utc[row_id],
        tzone = zone
      )

      base_date <- as.Date(
        anchor_local,
        tz = zone
      )

      local_date <- base_date +
        day_add[row_id]

      resolved <- resolve_div_clock(
        date = local_date,
        clock = event_clock[row_id],
        timezone = zone
      )

      before_anchor <-
        resolved$time_status == "valid" &
          resolved$event_utc <
            anchor_utc[row_id]

      if (any(before_anchor)) {
        local_date[before_anchor] <-
          local_date[before_anchor] + 1L

        resolved_next <- resolve_div_clock(
          date = local_date[before_anchor],
          clock = event_clock[row_id][before_anchor],
          timezone = zone
        )

        resolved$event_utc[before_anchor] <-
          resolved_next$event_utc

        resolved$time_status[before_anchor] <-
          resolved_next$time_status
      }

      still_before <-
        resolved$time_status == "valid" &
          resolved$event_utc <
            anchor_utc[row_id]

      if (any(still_before)) {
        stop(
          "An event remained earlier than its temporal anchor."
        )
      }

      event_date[row_id] <- local_date
      event_utc[row_id] <- resolved$event_utc
      time_status[row_id] <- resolved$time_status
    }
  }

  tibble::tibble(
    event_date = event_date,
    event_clock = event_clock,
    event_utc = event_utc,
    time_status = time_status
  )
}


resolve_actual_dep <- function(
    date,
    sched_clock,
    clock,
    delay,
    timezone) {
  
  if (!inherits(
    date,
    "Date"
  ) ||
  anyNA(date)) {
    stop(
      "`date` must contain non-missing Date values."
    )
  }
  
  if (!is.character(sched_clock) ||
      anyNA(sched_clock) ||
      !is.character(clock) ||
      !is.numeric(delay) ||
      !is.character(timezone)) {
    stop(
      "Actual-departure inputs have unexpected types."
    )
  }
  
  input_n <- length(date)
  
  if (length(sched_clock) != input_n ||
      length(clock) != input_n ||
      length(delay) != input_n ||
      length(timezone) != input_n) {
    stop(
      "Actual-departure inputs must have the same length."
    )
  }
  
  if (anyNA(timezone) ||
      any(!nzchar(timezone))) {
    stop(
      "`timezone` must contain non-missing values."
    )
  }
  
  sched <- parse_div_clock(
    sched_clock,
    "CRSDepTime"
  )
  
  event_date <- rep(
    as.Date(NA),
    input_n
  )
  
  event_clock <- rep(
    NA_character_,
    input_n
  )
  
  event_utc <- as.POSIXct(
    rep(
      NA_real_,
      input_n
    ),
    origin = "1970-01-01",
    tz = "UTC"
  )
  
  time_status <- rep(
    "source_unresolved",
    input_n
  )
  
  has_clock <- !is.na(
    clock
  )
  
  dep_raw <- rep(
    NA_integer_,
    input_n
  )
  
  if (any(has_clock)) {
    parsed <- parse_div_clock(
      clock[has_clock],
      "DepTime"
    )
    
    event_clock[has_clock] <-
      parsed$clock
    
    dep_raw[has_clock] <-
      parsed$raw_min
  }
  
  delay_ok <- !is.na(delay) &
    is.finite(delay)
  
  nonwhole_delay <- has_clock &
    delay_ok &
    delay != floor(delay)
  
  if (any(nonwhole_delay)) {
    stop(
      "`DepDelay` must contain whole minutes."
    )
  }
  
  ready <- has_clock &
    delay_ok
  
  day_raw <- rep(
    NA_real_,
    input_n
  )
  
  day_raw[ready] <- (
    delay[ready] +
      sched$raw_min[ready] -
      dep_raw[ready]
  ) / 1440
  
  source_match <- ready &
    day_raw == floor(day_raw)
  
  if (any(source_match)) {
    day_shift <- as.integer(
      day_raw[source_match]
    )
    
    resolved <- resolve_local_utc(
      date =
        date[source_match] +
        day_shift,
      clock = clock[source_match],
      timezone = timezone[source_match],
      field = "DepTime"
    )
    
    event_date[source_match] <-
      resolved$event_date
    
    event_clock[source_match] <-
      resolved$event_clock
    
    event_utc[source_match] <-
      resolved$event_utc
    
    time_status[source_match] <-
      resolved$time_status
  }
  
  tibble::tibble(
    event_date = event_date,
    event_clock = event_clock,
    event_utc = event_utc,
    time_status = time_status
  )
}


# Normalize main-table flights to flight events.
prepare_main_events <- function(flights) {
  if (!is.data.frame(flights)) {
    stop(
      "`flights` must be a data frame."
    )
  }
  
  if (nrow(flights) == 0L) {
    stop(
      "`flights` contains no flight rows."
    )
  }
  
  stop_suffix <- c(
    "Airport",
    "AirportID",
    "AirportSeqID",
    "Latitude",
    "Longitude",
    "WheelsOn",
    "WheelsOff"
  )
  
  stop_cols <- unlist(
    lapply(
      1:5,
      function(i) {
        paste0(
          "Div",
          i,
          stop_suffix
        )
      }
    ),
    use.names = FALSE
  )
  
  required_cols <- c(
    "Year",
    "FlightDate",
    "CRSDepTime",
    "DepTime",
    "DepDelay",
    "WheelsOff",
    "CRSArrTime",
    "WheelsOn",
    "ArrTime",
    "Cancelled",
    "Diverted",
    "DivAirportLandings",
    "DivReachedDest",
    "Reporting_Airline",
    "Flight_Number_Reporting_Airline",
    "OriginAirportID",
    "OriginAirportSeqID",
    "Origin",
    "OriginLatitude",
    "OriginLongitude",
    "DestAirportID",
    "DestAirportSeqID",
    "Dest",
    "DestLatitude",
    "DestLongitude",
    stop_cols
  )
  
  missing_cols <- setdiff(
    required_cols,
    names(flights)
  )
  
  if (length(missing_cols) > 0L) {
    print(
      missing_cols
    )
    
    stop(
      "Main flight data lack required event fields."
    )
  }
  
  if (inherits(
    flights$FlightDate,
    "POSIXct"
  )) {
    if (anyNA(
      flights$FlightDate
    )) {
      stop(
        "`FlightDate` contains a missing value."
      )
    }
    
    utc_clock <- format(
      flights$FlightDate,
      format = "%H%M%S",
      tz = "UTC"
    )
    
    if (any(
      utc_clock != "000000"
    )) {
      stop(
        paste(
          "A POSIXct `FlightDate` is not",
          "represented at midnight UTC."
        )
      )
    }
    
    flights$FlightDate <- as.Date(
      flights$FlightDate,
      tz = "UTC"
    )
  }
  
  if (!inherits(
    flights$FlightDate,
    "Date"
  ) ||
  anyNA(
    flights$FlightDate
  )) {
    stop(
      "`FlightDate` must contain non-missing dates."
    )
  }
  
  if (!is.numeric(flights$Year) ||
      anyNA(flights$Year) ||
      any(
        flights$Year !=
        floor(flights$Year)
      )) {
    stop(
      "`Year` contains an invalid value."
    )
  }
  
  flight_year <- lubridate::year(
    flights$FlightDate
  )
  
  if (any(
    flight_year !=
    flights$Year
  )) {
    stop(
      "At least one BTS year disagrees with `FlightDate`."
    )
  }
  
  cancelled <- parse_bts_flag(
    flights$Cancelled,
    "Cancelled"
  )
  
  diverted <- parse_bts_flag(
    flights$Diverted,
    "Diverted"
  )
  
  if (any(
    cancelled &
    diverted
  )) {
    stop(
      "A main flight is both cancelled and diverted."
    )
  }
  
  landing_code <- normalize_whole_num(
    flights$DivAirportLandings,
    "DivAirportLandings"
  )
  
  reached_value <- normalize_whole_num(
    flights$DivReachedDest,
    "DivReachedDest"
  )
  
  bad_reached <- !is.na(
    reached_value
  ) &
    !reached_value %in%
    c(
      0L,
      1L
    )
  
  if (any(bad_reached)) {
    stop(
      "`DivReachedDest` contains an invalid value."
    )
  }
  
  air_return <- cancelled &
    !is.na(
      landing_code
    ) &
    landing_code == 9L
  
  detailed_div <- diverted &
    !is.na(
      landing_code
    ) &
    landing_code %in%
    1:5
  
  historic_div <- diverted &
    is.na(
      landing_code
    )
  
  bad_return <- !is.na(
    landing_code
  ) &
    landing_code == 9L &
    !cancelled
  
  if (any(bad_return)) {
    stop(
      paste(
        "`DivAirportLandings == 9` occurs",
        "on a non-cancelled flight."
      )
    )
  }
  
  bad_div_code <- diverted &
    !is.na(
      landing_code
    ) &
    !landing_code %in%
    1:5
  
  if (any(bad_div_code)) {
    stop(
      paste(
        "A diverted main flight has an",
        "unexpected landing code."
      )
    )
  }
  
  bad_normal_code <- !cancelled &
    !diverted &
    !is.na(
      landing_code
    ) &
    landing_code != 0L
  
  if (any(bad_normal_code)) {
    stop(
      paste(
        "A non-cancelled, non-diverted flight",
        "has an unexpected landing code."
      )
    )
  }
  
  bad_cancel_code <- cancelled &
    !air_return &
    !is.na(
      landing_code
    ) &
    landing_code != 0L
  
  if (any(bad_cancel_code)) {
    stop(
      paste(
        "A cancelled flight has an",
        "unexpected landing code."
      )
    )
  }
  
  if (any(
    detailed_div &
    is.na(
      reached_value
    )
  )) {
    stop(
      paste(
        "A detailed diversion lacks",
        "`DivReachedDest`."
      )
    )
  }
  
  reached_dest <- rep(
    FALSE,
    nrow(flights)
  )
  
  reached_dest[detailed_div] <-
    reached_value[detailed_div] == 1L
  
  required_clock <- c(
    "CRSDepTime",
    "CRSArrTime"
  )
  
  if (any(
    !vapply(
      flights[required_clock],
      is.character,
      logical(1)
    )
  ) ||
  anyNA(
    flights[required_clock]
  )) {
    stop(
      "Scheduled departure and arrival clocks are required."
    )
  }
  
  actual_clock <- c(
    "DepTime",
    "WheelsOff",
    "WheelsOn",
    "ArrTime"
  )
  
  if (any(
    !vapply(
      flights[actual_clock],
      is.character,
      logical(1)
    )
  )) {
    stop(
      "Actual BTS event clocks must be character."
    )
  }
  
  if (!is.numeric(
    flights$DepDelay
  )) {
    stop(
      "`DepDelay` must be numeric."
    )
  }
  
  bad_delay <- !is.na(
    flights$DepDelay
  ) &
    !is.finite(
      flights$DepDelay
    )
  
  if (any(bad_delay)) {
    stop(
      "`DepDelay` contains a non-finite value."
    )
  }
  
  if (!is.character(flights$Origin) ||
      anyNA(flights$Origin) ||
      any(!nzchar(flights$Origin)) ||
      !is.character(flights$Dest) ||
      anyNA(flights$Dest) ||
      any(!nzchar(flights$Dest))) {
    stop(
      "Scheduled airport codes must be non-missing character."
    )
  }
  
  origin_id <- normalize_bts_id(
    flights$OriginAirportID,
    "OriginAirportID"
  )
  
  origin_seq <- normalize_bts_id(
    flights$OriginAirportSeqID,
    "OriginAirportSeqID"
  )
  
  dest_id <- normalize_bts_id(
    flights$DestAirportID,
    "DestAirportID"
  )
  
  dest_seq <- normalize_bts_id(
    flights$DestAirportSeqID,
    "DestAirportSeqID"
  )
  
  bad_endpoint <- is.na(origin_id) |
    origin_id <= 0L |
    is.na(origin_seq) |
    origin_seq <= 0L |
    is.na(dest_id) |
    dest_id <= 0L |
    is.na(dest_seq) |
    dest_seq <= 0L
  
  if (any(bad_endpoint)) {
    stop(
      "A scheduled endpoint lacks a valid BTS airport identifier."
    )
  }
  
  slc_id <- 14869L
  
  slc_origin <- origin_id == slc_id
  slc_dest <- dest_id == slc_id
  
  if (any(
    slc_origin ==
    slc_dest
  )) {
    stop(
      "A main flight does not define exactly one SLC endpoint."
    )
  }
  
  stop_num <- 1:5
  
  stop_ids <- lapply(
    stop_num,
    function(i) {
      field <- paste0(
        "Div",
        i,
        "AirportID"
      )
      
      normalize_bts_id(
        flights[[field]],
        field
      )
    }
  )
  
  stop_id_mat <- do.call(
    cbind,
    stop_ids
  )
  
  stop_present <- !is.na(
    stop_id_mat
  )
  
  gap_present <-
    stop_present[, 2:5, drop = FALSE] &
    !stop_present[, 1:4, drop = FALSE]
  
  if (any(gap_present)) {
    stop(
      "A main-flight diversion sequence contains a gap."
    )
  }
  
  observed_stop_n <- as.integer(
    rowSums(
      stop_present
    )
  )
  
  known_stops <- air_return |
    detailed_div
  
  bad_detail_n <- detailed_div &
    observed_stop_n !=
    landing_code
  
  if (any(bad_detail_n)) {
    stop(
      paste(
        "Detailed diversion fields disagree with",
        "`DivAirportLandings`."
      )
    )
  }
  
  bad_return_n <- air_return &
    (
      observed_stop_n < 1L |
        observed_stop_n > 5L
    )
  
  if (any(bad_return_n)) {
    stop(
      "An air-return cancellation has an invalid stop count."
    )
  }
  
  later_stop_cols <- unlist(
    lapply(
      2:5,
      function(i) {
        paste0(
          "Div",
          i,
          stop_suffix
        )
      }
    ),
    use.names = FALSE
  )
  
  later_stop_empty <- rowSums(
    !is.na(
      flights[later_stop_cols]
    )
  ) == 0L
  
  # These rows disagree on the airport of the reported landing.
  source_stop_conflict <- !cancelled &
    !diverted &
    is.na(landing_code) &
    is.na(reached_value) &
    observed_stop_n == 1L &
    !is.na(flights$Div1Airport) &
    !is.na(flights$Div1AirportSeqID) &
    !is.na(flights$Div1Latitude) &
    !is.na(flights$Div1Longitude) &
    !is.na(flights$Div1WheelsOn) &
    !is.na(flights$WheelsOn) &
    !is.na(flights$ArrTime) &
    flights$Div1WheelsOn ==
    flights$WheelsOn &
    is.na(flights$Div1WheelsOff) &
    stop_id_mat[, 1L] !=
    origin_id &
    stop_id_mat[, 1L] !=
    dest_id &
    later_stop_empty
  
  unknown_stops <- !known_stops &
    observed_stop_n > 0L &
    !source_stop_conflict
  
  if (any(unknown_stops)) {
    stop(
      paste(
        "Diversion-stop fields are populated without",
        "a supported diversion structure."
      )
    )
  }
  
  air_rows <- which(
    air_return
  )
  
  if (length(air_rows) > 0L) {
    final_air_id <- vapply(
      air_rows,
      function(i) {
        stop_id_mat[
          i,
          observed_stop_n[[i]]
        ]
      },
      integer(1)
    )
    
    if (any(
      final_air_id !=
      origin_id[air_rows]
    )) {
      stop(
        paste(
          "An air-return cancellation does not",
          "terminate at its scheduled origin."
        )
      )
    }
  }
  
  if (any(
    cancelled &
    is.na(
      flights$DepTime
    ) &
    !is.na(
      flights$WheelsOff
    )
  )) {
    stop(
      paste(
        "A cancelled flight reports WheelsOff",
        "without DepTime."
      )
    )
  }
  
  if (any(
    cancelled &
    (
      !is.na(
        flights$WheelsOn
      ) |
      !is.na(
        flights$ArrTime
      )
    )
  )) {
    stop(
      paste(
        "A cancelled flight reports a scheduled-",
        "destination actual event."
      )
    )
  }
  
  not_reached <- detailed_div &
    !reached_dest
  
  if (any(
    not_reached &
    (
      !is.na(
        flights$WheelsOn
      ) |
      !is.na(
        flights$ArrTime
      )
    )
  )) {
    stop(
      paste(
        "A detailed diversion that did not reach",
        "its destination reports destination events."
      )
    )
  }
  
  origin_tz <- lookup_airport_tz(
    flights$OriginLatitude,
    flights$OriginLongitude
  )
  
  dest_tz <- lookup_airport_tz(
    flights$DestLatitude,
    flights$DestLongitude
  )
  
  if (any(
    slc_origin &
    origin_tz != "America/Denver"
  ) ||
  any(
    slc_dest &
    dest_tz != "America/Denver"
  )) {
    stop(
      "An SLC endpoint has an unexpected time zone."
    )
  }
  
  origin_sched <- resolve_local_utc(
    date = flights$FlightDate,
    clock = flights$CRSDepTime,
    timezone = origin_tz,
    field = "CRSDepTime"
  )
  
  dep_actual <- resolve_actual_dep(
    date = flights$FlightDate,
    sched_clock = flights$CRSDepTime,
    clock = flights$DepTime,
    delay = flights$DepDelay,
    timezone = origin_tz
  )
  
  origin_off <- resolve_after_utc(
    anchor_utc = dep_actual$event_utc,
    clock = flights$WheelsOff,
    timezone = origin_tz,
    field = "WheelsOff"
  )
  
  dest_sched <- resolve_after_utc(
    anchor_utc = origin_sched$event_utc,
    clock = flights$CRSArrTime,
    timezone = dest_tz,
    field = "CRSArrTime"
  )
  
  dest_status <- dplyr::case_when(
    origin_sched$time_status != "valid" ~
      origin_sched$time_status,
    TRUE ~ dest_sched$time_status
  )
  
  flights <- flights |>
    dplyr::mutate(
      flight_row_id =
        dplyr::row_number()
    )
  
  flight_cols <- setdiff(
    names(flights),
    "flight_row_id"
  )
  
  stops <- NULL
  
  if (any(known_stops)) {
    stop_rows <- which(
      known_stops
    )
    
    stop_count <- observed_stop_n[
      stop_rows
    ]
    
    stop_reached <- reached_dest[
      stop_rows
    ]
    
    stop_input <- flights[
      stop_rows,
      ,
      drop = FALSE
    ] |>
      dplyr::mutate(
        div_row_id =
          flight_row_id
      )
    
    stops <- build_div_stops(
      flights = stop_input,
      dep_anchor_utc =
        origin_off$event_utc[
          stop_rows
        ],
      reached_dest =
        stop_reached,
      stop_count =
        stop_count
    ) |>
      dplyr::rename(
        flight_row_id =
          div_row_id
      )
  }
  
  dest_anchor_utc <-
    dep_actual$event_utc
  
  if (!is.null(stops)) {
    final_stops <- stops |>
      dplyr::group_by(
        flight_row_id
      ) |>
      dplyr::filter(
        div_number == max(
          div_number
        )
      ) |>
      dplyr::ungroup() |>
      dplyr::arrange(
        flight_row_id
      )
    
    if (anyDuplicated(
      final_stops$flight_row_id
    ) > 0L) {
      stop(
        "Final main-table diversion stops are not unique."
      )
    }
    
    final_rows <- match(
      final_stops$flight_row_id,
      flights$flight_row_id
    )
    
    if (anyNA(final_rows)) {
      stop(
        "A final diversion stop does not match its flight."
      )
    }
    
    use_final <- final_stops$
      departure_applicable
    
    dest_anchor_utc[
      final_rows[use_final]
    ] <- final_stops$
      departure_utc[use_final]
  }
  
  normal_dest <- !cancelled &
    !diverted
  
  detail_dest <- detailed_div &
    reached_dest
  
  historic_on <- historic_div &
    !is.na(
      flights$WheelsOn
    )
  
  historic_arr <- historic_div &
    !is.na(
      flights$ArrTime
    )
  
  dest_on_app <- normal_dest |
    detail_dest |
    historic_on
  
  dest_arr_app <- normal_dest |
    detail_dest |
    historic_arr
  
  dest_on_clock <- flights$WheelsOn
  dest_arr_clock <- flights$ArrTime
  
  dest_on_clock[!dest_on_app] <-
    NA_character_
  
  dest_arr_clock[!dest_arr_app] <-
    NA_character_
  
  dest_on <- resolve_after_utc(
    anchor_utc = dest_anchor_utc,
    clock = dest_on_clock,
    timezone = dest_tz,
    field = "WheelsOn"
  )
  
  dest_arr <- resolve_after_utc(
    anchor_utc = dest_anchor_utc,
    clock = dest_arr_clock,
    timezone = dest_tz,
    field = "ArrTime"
  )
  
  # Contradictory ordinary-flight clocks cannot establish event dates.
  air_conflict <- normal_dest &
    origin_off$time_status == "valid" &
    dest_on$time_status == "valid" &
    dest_on$event_utc <
    origin_off$event_utc
  
  taxi_conflict <- normal_dest &
    dest_on$time_status == "valid" &
    dest_arr$time_status == "valid" &
    dest_arr$event_utc <
    dest_on$event_utc
  
  off_unresolved <- air_conflict
  
  on_unresolved <- air_conflict |
    taxi_conflict |
    source_stop_conflict
  
  arr_unresolved <- taxi_conflict |
    source_stop_conflict
  
  unresolved_date <- as.Date(NA)
  
  unresolved_utc <- as.POSIXct(
    NA_real_,
    origin = "1970-01-01",
    tz = "UTC"
  )
  
  origin_off$event_date[
    off_unresolved
  ] <- unresolved_date
  
  origin_off$event_utc[
    off_unresolved
  ] <- unresolved_utc
  
  origin_off$time_status[
    off_unresolved
  ] <- "source_unresolved"
  
  dest_on$event_date[
    on_unresolved
  ] <- unresolved_date
  
  dest_on$event_utc[
    on_unresolved
  ] <- unresolved_utc
  
  dest_on$time_status[
    on_unresolved
  ] <- "source_unresolved"
  
  dest_arr$event_date[
    arr_unresolved
  ] <- unresolved_date
  
  dest_arr$event_utc[
    arr_unresolved
  ] <- unresolved_utc
  
  dest_arr$time_status[
    arr_unresolved
  ] <- "source_unresolved"
  
  origin_dep_app <- !cancelled |
    !is.na(
      flights$DepTime
    )
  
  origin_off_app <- !cancelled |
    !is.na(
      flights$WheelsOff
    )
  
  origin_sched_events <- flights |>
    dplyr::mutate(
      airport_role = "origin",
      event_type = "gate_departure",
      event_time_basis = "scheduled",
      div_number = NA_integer_,
      event_source_field = "CRSDepTime",
      event_airport_id = origin_id,
      event_airport_seq_id = origin_seq,
      event_airport_code = Origin,
      event_timezone = origin_tz,
      event_date =
        origin_sched$event_date,
      event_clock =
        origin_sched$event_clock,
      event_utc =
        origin_sched$event_utc,
      event_time_status =
        origin_sched$time_status,
      is_slc_diversion = FALSE
    )
  
  dep_rows <- which(
    origin_dep_app
  )
  
  origin_dep_events <- flights[
    dep_rows,
    ,
    drop = FALSE
  ] |>
    dplyr::mutate(
      airport_role = "origin",
      event_type = "gate_departure",
      event_time_basis = "actual",
      div_number = NA_integer_,
      event_source_field = "DepTime",
      event_airport_id =
        origin_id[dep_rows],
      event_airport_seq_id =
        origin_seq[dep_rows],
      event_airport_code = Origin,
      event_timezone =
        origin_tz[dep_rows],
      event_date =
        dep_actual$event_date[
          dep_rows
        ],
      event_clock =
        dep_actual$event_clock[
          dep_rows
        ],
      event_utc =
        dep_actual$event_utc[
          dep_rows
        ],
      event_time_status =
        dep_actual$time_status[
          dep_rows
        ],
      is_slc_diversion = FALSE
    )
  
  off_rows <- which(
    origin_off_app
  )
  
  origin_off_events <- flights[
    off_rows,
    ,
    drop = FALSE
  ] |>
    dplyr::mutate(
      airport_role = "origin",
      event_type = "wheels_off",
      event_time_basis = "actual",
      div_number = NA_integer_,
      event_source_field = "WheelsOff",
      event_airport_id =
        origin_id[off_rows],
      event_airport_seq_id =
        origin_seq[off_rows],
      event_airport_code = Origin,
      event_timezone =
        origin_tz[off_rows],
      event_date =
        origin_off$event_date[
          off_rows
        ],
      event_clock =
        origin_off$event_clock[
          off_rows
        ],
      event_utc =
        origin_off$event_utc[
          off_rows
        ],
      event_time_status =
        origin_off$time_status[
          off_rows
        ],
      is_slc_diversion = FALSE
    )
  
  dest_sched_events <- flights |>
    dplyr::mutate(
      airport_role =
        "scheduled_destination",
      event_type = "gate_arrival",
      event_time_basis = "scheduled",
      div_number = NA_integer_,
      event_source_field = "CRSArrTime",
      event_airport_id = dest_id,
      event_airport_seq_id = dest_seq,
      event_airport_code = Dest,
      event_timezone = dest_tz,
      event_date =
        dest_sched$event_date,
      event_clock =
        dest_sched$event_clock,
      event_utc =
        dest_sched$event_utc,
      event_time_status = dest_status,
      is_slc_diversion = FALSE
    )
  
  div_on_events <- NULL
  div_off_events <- NULL
  
  if (!is.null(stops)) {
    stop_events <- flights |>
      dplyr::inner_join(
        stops,
        by = "flight_row_id",
        relationship = "one-to-many"
      )
    
    div_on_events <- stop_events |>
      dplyr::mutate(
        airport_role = "diversion",
        event_type = "wheels_on",
        event_time_basis = "actual",
        event_source_field = paste0(
          "Div",
          div_number,
          "WheelsOn"
        ),
        event_airport_id =
          div_airport_id,
        event_airport_seq_id =
          div_airport_seq_id,
        event_airport_code =
          div_airport_code,
        event_timezone =
          div_timezone,
        event_date = arrival_date,
        event_clock = arrival_clock,
        event_utc = arrival_utc,
        event_time_status =
          arrival_status,
        is_slc_diversion =
          div_airport_id == slc_id
      )
    
    div_off_events <- stop_events |>
      dplyr::filter(
        departure_applicable
      ) |>
      dplyr::mutate(
        airport_role = "diversion",
        event_type = "wheels_off",
        event_time_basis = "actual",
        event_source_field = paste0(
          "Div",
          div_number,
          "WheelsOff"
        ),
        event_airport_id =
          div_airport_id,
        event_airport_seq_id =
          div_airport_seq_id,
        event_airport_code =
          div_airport_code,
        event_timezone =
          div_timezone,
        event_date = departure_date,
        event_clock = departure_clock,
        event_utc = departure_utc,
        event_time_status =
          departure_status,
        is_slc_diversion =
          div_airport_id == slc_id
      )
  }
  
  on_rows <- which(
    dest_on_app
  )
  
  dest_on_events <- flights[
    on_rows,
    ,
    drop = FALSE
  ] |>
    dplyr::mutate(
      airport_role =
        "scheduled_destination",
      event_type = "wheels_on",
      event_time_basis = "actual",
      div_number = NA_integer_,
      event_source_field = "WheelsOn",
      event_airport_id =
        dest_id[on_rows],
      event_airport_seq_id =
        dest_seq[on_rows],
      event_airport_code = Dest,
      event_timezone =
        dest_tz[on_rows],
      event_date =
        dest_on$event_date[
          on_rows
        ],
      event_clock =
        dest_on$event_clock[
          on_rows
        ],
      event_utc =
        dest_on$event_utc[
          on_rows
        ],
      event_time_status =
        dest_on$time_status[
          on_rows
        ],
      is_slc_diversion = FALSE
    )
  
  arr_rows <- which(
    dest_arr_app
  )
  
  dest_arr_events <- flights[
    arr_rows,
    ,
    drop = FALSE
  ] |>
    dplyr::mutate(
      airport_role =
        "scheduled_destination",
      event_type = "gate_arrival",
      event_time_basis = "actual",
      div_number = NA_integer_,
      event_source_field = "ArrTime",
      event_airport_id =
        dest_id[arr_rows],
      event_airport_seq_id =
        dest_seq[arr_rows],
      event_airport_code = Dest,
      event_timezone =
        dest_tz[arr_rows],
      event_date =
        dest_arr$event_date[
          arr_rows
        ],
      event_clock =
        dest_arr$event_clock[
          arr_rows
        ],
      event_utc =
        dest_arr$event_utc[
          arr_rows
        ],
      event_time_status =
        dest_arr$time_status[
          arr_rows
        ],
      is_slc_diversion = FALSE
    )
  
  events <- dplyr::bind_rows(
    origin_sched_events,
    origin_dep_events,
    origin_off_events,
    dest_sched_events,
    div_on_events,
    div_off_events,
    dest_on_events,
    dest_arr_events
  )
  
  stop_event_n <- 0L
  
  if (!is.null(stops)) {
    stop_event_n <- nrow(
      stops
    ) +
      sum(
        stops$departure_applicable
      )
  }
  
  expected_n <-
    2L * nrow(flights) +
    sum(origin_dep_app) +
    sum(origin_off_app) +
    stop_event_n +
    sum(dest_on_app) +
    sum(dest_arr_app)
  
  if (nrow(events) != expected_n) {
    stop(
      "Main-event preparation changed the event grain."
    )
  }
  
  core_cols <- c(
    "flight_row_id",
    "airport_role",
    "event_type",
    "event_time_basis",
    "div_number",
    "event_source_field",
    "event_airport_id",
    "event_airport_seq_id",
    "event_airport_code",
    "event_timezone",
    "event_date",
    "event_clock",
    "event_utc",
    "event_time_status",
    "is_slc_diversion"
  )
  
  events <- events |>
    dplyr::select(
      dplyr::all_of(
        core_cols
      ),
      dplyr::all_of(
        flight_cols
      )
    )
  
  # Output order is structural only. Chronology must use event_utc.
  role_order <- dplyr::case_when(
    events$airport_role == "origin" ~ 1L,
    events$airport_role == "diversion" ~ 2L,
    TRUE ~ 3L
  )
  
  basis_order <- dplyr::case_when(
    events$event_time_basis == "scheduled" ~ 1L,
    TRUE ~ 2L
  )
  
  type_order <- dplyr::case_when(
    events$event_type == "gate_departure" ~ 1L,
    events$event_type == "wheels_on" ~ 2L,
    events$event_type == "wheels_off" ~ 3L,
    TRUE ~ 4L
  )
  
  events |>
    dplyr::mutate(
      .role_order = role_order,
      .basis_order = basis_order,
      .type_order = type_order,
      .div_order = dplyr::coalesce(
        div_number,
        0L
      )
    ) |>
    dplyr::arrange(
      FlightDate,
      flight_row_id,
      .role_order,
      .div_order,
      .basis_order,
      .type_order
    ) |>
    dplyr::select(
      -.role_order,
      -.basis_order,
      -.type_order,
      -.div_order
    )
}


build_div_stops <- function(
  flights,
  dep_anchor_utc,
  reached_dest,
  stop_count
) {
  if (!is.data.frame(flights)) {
    stop(
      "`flights` must be a data frame."
    )
  }

  if (!inherits(
    dep_anchor_utc,
    "POSIXct"
  )) {
    stop(
      "`dep_anchor_utc` must be POSIXct."
    )
  }

  if (!is.logical(reached_dest) ||
    anyNA(reached_dest)) {
    stop(
      "`reached_dest` must be non-missing logical."
    )
  }

  if (!is.integer(stop_count) ||
    anyNA(stop_count)) {
    stop(
      "`stop_count` must be non-missing integer."
    )
  }

  input_n <- nrow(flights)

  if (length(dep_anchor_utc) != input_n ||
    length(reached_dest) != input_n ||
    length(stop_count) != input_n) {
    stop(
      "Diversion path inputs must match the flight rows."
    )
  }

  if (!"div_row_id" %in%
    names(flights)) {
    stop(
      "`flights` lacks `div_row_id`."
    )
  }

  div_num <- 1:5

  airport_ids <- lapply(
    div_num,
    function(i) {
      field <- paste0(
        "Div",
        i,
        "AirportID"
      )

      normalize_bts_id(
        flights[[field]],
        field
      )
    }
  )

  present <- do.call(
    cbind,
    lapply(
      airport_ids,
      function(x) {
        !is.na(x)
      }
    )
  )

  gap_present <-
    present[, 2:5, drop = FALSE] &
      !present[, 1:4, drop = FALSE]

  if (any(gap_present)) {
    stop(
      "A diversion-stop sequence contains a gap."
    )
  }

  observed_count <- rowSums(
    present
  )

  if (any(
    observed_count !=
      stop_count
  )) {
    stop(
      paste(
        "Diversion-stop fields disagree with",
        "`DivAirportLandings`."
      )
    )
  }

  if (any(
    stop_count < 1L |
      stop_count > 5L
  )) {
    stop(
      "`DivAirportLandings` must be between 1 and 5."
    )
  }

  anchor_utc <- dep_anchor_utc

  stop_list <- vector(
    "list",
    length(div_num)
  )

  # Each stop depends on the preceding actual departure timestamp.
  for (i in div_num) {
    prefix <- paste0(
      "Div",
      i
    )

    airport_id <- airport_ids[[i]]

    seq_field <- paste0(
      prefix,
      "AirportSeqID"
    )

    airport_seq_id <- normalize_bts_id(
      flights[[seq_field]],
      seq_field
    )

    airport_code <- flights[[
      paste0(
        prefix,
        "Airport"
      )
    ]]

    latitude <- flights[[
      paste0(
        prefix,
        "Latitude"
      )
    ]]

    longitude <- flights[[
      paste0(
        prefix,
        "Longitude"
      )
    ]]

    wheels_on <- flights[[
      paste0(
        prefix,
        "WheelsOn"
      )
    ]]

    wheels_off <- flights[[
      paste0(
        prefix,
        "WheelsOff"
      )
    ]]

    has_stop <- !is.na(
      airport_id
    )

    if (!is.character(airport_code) ||
      !is.character(wheels_on) ||
      !is.character(wheels_off) ||
      !is.numeric(latitude) ||
      !is.numeric(longitude)) {
      stop(
        "A diversion-stop field has an unexpected type."
      )
    }

    bad_identity <- has_stop &
      (
        airport_id <= 0L |
          is.na(airport_seq_id) |
          airport_seq_id <= 0L |
          is.na(airport_code) |
          !nzchar(airport_code) |
          is.na(latitude) |
          is.na(longitude)
      )

    if (any(bad_identity)) {
      stop(
        "A diversion stop lacks required airport metadata."
      )
    }

    orphan_field <- !has_stop &
      (
        !is.na(airport_seq_id) |
          !is.na(airport_code) |
          !is.na(latitude) |
          !is.na(longitude) |
          !is.na(wheels_on) |
          !is.na(wheels_off)
      )

    if (any(orphan_field)) {
      stop(
        paste(
          "Diversion-stop metadata exists without",
          "a diversion AirportID."
        )
      )
    }

    timezone <- rep(
      NA_character_,
      input_n
    )

    if (any(has_stop)) {
      timezone[has_stop] <- lookup_airport_tz(
        latitude[has_stop],
        longitude[has_stop]
      )
    }

    slc_stop <- has_stop &
      airport_id == 14869L

    if (any(
      slc_stop &
        timezone != "America/Denver"
    )) {
      stop(
        "An SLC diversion stop has an unexpected time zone."
      )
    }

    is_final <- has_stop &
      stop_count == i

    # Final WheelsOff is inapplicable if the destination was not reached.
    dep_applicable <- has_stop &
      (
        !is_final |
          reached_dest
      )

    bad_final_off <- is_final &
      !reached_dest &
      !is.na(wheels_off)

    if (any(bad_final_off)) {
      stop(
        paste(
          "A flight that did not reach its scheduled",
          "destination reports a final diversion WheelsOff."
        )
      )
    }

    arrival <- resolve_after_utc(
      anchor_utc = anchor_utc,
      clock = wheels_on,
      timezone = timezone,
      field = paste0(
        prefix,
        "WheelsOn"
      )
    )

    dep_clock <- wheels_off
    dep_clock[!dep_applicable] <- NA_character_

    departure <- resolve_after_utc(
      anchor_utc = arrival$event_utc,
      clock = dep_clock,
      timezone = timezone,
      field = paste0(
        prefix,
        "WheelsOff"
      )
    )

    stop_list[[i]] <- tibble::tibble(
      div_row_id = flights$div_row_id,
      div_number = i,
      div_airport_id = airport_id,
      div_airport_seq_id = airport_seq_id,
      div_airport_code = airport_code,
      div_latitude = latitude,
      div_longitude = longitude,
      div_timezone = timezone,
      div_wheels_on = wheels_on,
      div_wheels_off = wheels_off,
      departure_applicable = dep_applicable,
      arrival_date = arrival$event_date,
      arrival_clock = arrival$event_clock,
      arrival_utc = arrival$event_utc,
      arrival_status = arrival$time_status,
      departure_date = departure$event_date,
      departure_clock = departure$event_clock,
      departure_utc = departure$event_utc,
      departure_status = departure$time_status
    ) |>
      dplyr::filter(
        !is.na(div_airport_id)
      )

    update_anchor <- has_stop &
      dep_applicable

    anchor_utc[update_anchor] <-
      departure$event_utc[update_anchor]
  }

  dplyr::bind_rows(
    stop_list
  )
}


prepare_div_events <- function(
  years = slcflights::available_years(
    type = "div"
  )
) {
  years <- check_div_years(
    years
  )

  flights <- slcflights::read_div(
    years
  )

  if (nrow(flights) == 0L) {
    stop(
      "No diversion-only flight rows were found."
    )
  }

  required_cols <- c(
    "Year",
    "FlightDate",
    "CRSDepTime",
    "DepTime",
    "DepDelay",
    "WheelsOff",
    "CRSArrTime",
    "CRSElapsedTime",
    "Diverted",
    "DivAirportLandings",
    "DivReachedDest",
    "WheelsOn",
    "ArrTime",
    "OriginAirportID",
    "OriginAirportSeqID",
    "Origin",
    "OriginLatitude",
    "OriginLongitude",
    "DestAirportID",
    "DestAirportSeqID",
    "Dest",
    "DestLatitude",
    "DestLongitude"
  )

  stop_suffix <- c(
    "Airport",
    "AirportID",
    "AirportSeqID",
    "Latitude",
    "Longitude",
    "WheelsOn",
    "WheelsOff"
  )

  stop_cols <- unlist(
    lapply(
      1:5,
      function(i) {
        paste0(
          "Div",
          i,
          stop_suffix
        )
      }
    ),
    use.names = FALSE
  )

  required_cols <- c(
    required_cols,
    stop_cols
  )

  missing_cols <- setdiff(
    required_cols,
    names(flights)
  )

  if (length(missing_cols) > 0L) {
    print(
      missing_cols
    )

    stop(
      "Diversion data lack required fields."
    )
  }

  if (!inherits(
    flights$FlightDate,
    "Date"
  ) ||
    anyNA(
      flights$FlightDate
    )) {
    stop(
      "`FlightDate` must contain non-missing Date values."
    )
  }

  if (!is.numeric(flights$Year) ||
    anyNA(flights$Year) ||
    any(
      flights$Year !=
        floor(flights$Year)
    )) {
    stop(
      "`Year` contains an invalid value."
    )
  }

  flight_year <- lubridate::year(
    flights$FlightDate
  )

  if (any(
    flight_year !=
      flights$Year
  )) {
    stop(
      "At least one BTS year disagrees with `FlightDate`."
    )
  }

  diverted <- parse_bts_flag(
    flights$Diverted,
    "Diverted"
  )

  if (any(!diverted)) {
    stop(
      "A diversion-only row does not have `Diverted == 1`."
    )
  }

  reached_dest <- parse_bts_flag(
    flights$DivReachedDest,
    "DivReachedDest"
  )

  stop_count <- normalize_whole_num(
    flights$DivAirportLandings,
    "DivAirportLandings"
  )

  if (anyNA(stop_count)) {
    stop(
      "`DivAirportLandings` contains a missing value."
    )
  }

  if (!is.numeric(
    flights$CRSElapsedTime
  )) {
    stop(
      "`CRSElapsedTime` must be numeric."
    )
  }

  if (!is.numeric(
    flights$DepDelay
  )) {
    stop(
      "`DepDelay` must be numeric."
    )
  }

  bad_delay <- !is.na(
    flights$DepDelay
  ) &
    !is.finite(
      flights$DepDelay
    )

  if (any(bad_delay)) {
    stop(
      "`DepDelay` contains a non-finite value."
    )
  }

  required_clock <- c(
    "CRSDepTime",
    "CRSArrTime"
  )

  if (any(
    !vapply(
      flights[required_clock],
      is.character,
      logical(1)
    )
  ) ||
    anyNA(
      flights[required_clock]
    )) {
    stop(
      "Scheduled departure and arrival clocks are required."
    )
  }

  actual_clock <- c(
    "DepTime",
    "WheelsOff",
    "WheelsOn",
    "ArrTime"
  )

  if (any(
    !vapply(
      flights[actual_clock],
      is.character,
      logical(1)
    )
  )) {
    stop(
      "Actual BTS event clocks must be character."
    )
  }

  if (!is.character(flights$Origin) ||
    anyNA(flights$Origin) ||
    any(!nzchar(flights$Origin)) ||
    !is.character(flights$Dest) ||
    anyNA(flights$Dest) ||
    any(!nzchar(flights$Dest))) {
    stop(
      "Scheduled airport codes must be non-missing character."
    )
  }

  origin_id <- normalize_bts_id(
    flights$OriginAirportID,
    "OriginAirportID"
  )

  origin_seq <- normalize_bts_id(
    flights$OriginAirportSeqID,
    "OriginAirportSeqID"
  )

  dest_id <- normalize_bts_id(
    flights$DestAirportID,
    "DestAirportID"
  )

  dest_seq <- normalize_bts_id(
    flights$DestAirportSeqID,
    "DestAirportSeqID"
  )

  bad_endpoint <- is.na(origin_id) |
    origin_id <= 0L |
    is.na(origin_seq) |
    origin_seq <= 0L |
    is.na(dest_id) |
    dest_id <= 0L |
    is.na(dest_seq) |
    dest_seq <= 0L

  if (any(bad_endpoint)) {
    stop(
      "A scheduled endpoint lacks a valid BTS airport identifier."
    )
  }

  slc_id <- 14869L

  if (any(
    origin_id == slc_id |
      dest_id == slc_id
  )) {
    stop(
      paste(
        "A diversion-only flight has SLC as a",
        "scheduled endpoint."
      )
    )
  }

  # BTS omits destination arrival fields when the destination is not reached.
  bad_unreached_dest <- !reached_dest &
    (
      !is.na(flights$WheelsOn) |
        !is.na(flights$ArrTime)
    )

  if (any(bad_unreached_dest)) {
    stop(
      paste(
        "A flight that did not reach its scheduled destination",
        "reports destination arrival fields."
      )
    )
  }

  origin_tz <- lookup_airport_tz(
    flights$OriginLatitude,
    flights$OriginLongitude
  )

  dest_tz <- lookup_airport_tz(
    flights$DestLatitude,
    flights$DestLongitude
  )

  origin_sched <- resolve_local_utc(
    date = flights$FlightDate,
    clock = flights$CRSDepTime,
    timezone = origin_tz,
    field = "CRSDepTime"
  )
  
  dep_actual <- resolve_actual_dep(
    date = flights$FlightDate,
    sched_clock = flights$CRSDepTime,
    clock = flights$DepTime,
    delay = flights$DepDelay,
    timezone = origin_tz
  )
  
  origin_off <- resolve_after_utc(
    anchor_utc = dep_actual$event_utc,
    clock = flights$WheelsOff,
    timezone = origin_tz,
    field = "WheelsOff"
  )

  # CRSArrTime is the source scheduled destination event clock.
  dest_sched <- resolve_after_utc(
    anchor_utc = origin_sched$event_utc,
    clock = flights$CRSArrTime,
    timezone = dest_tz,
    field = "CRSArrTime"
  )

  dest_status <- dplyr::case_when(
    origin_sched$time_status != "valid" ~
      origin_sched$time_status,
    TRUE ~ dest_sched$time_status
  )

  flights <- flights |>
    dplyr::mutate(
      div_row_id = dplyr::row_number()
    )

  flight_cols <- setdiff(
    names(flights),
    "div_row_id"
  )

  # Operational diversion chronology begins at actual WheelsOff.
  stops <- build_div_stops(
    flights = flights,
    dep_anchor_utc = origin_off$event_utc,
    reached_dest = reached_dest,
    stop_count = stop_count
  )

  slc_hits <- tabulate(
    stops$div_row_id[
      stops$div_airport_id ==
        slc_id
    ],
    nbins = nrow(flights)
  )

  if (any(slc_hits == 0L)) {
    stop(
      "A diversion-only row contains no SLC diversion stop."
    )
  }

  final_stops <- stops |>
    dplyr::group_by(
      div_row_id
    ) |>
    dplyr::filter(
      div_number == max(
        div_number
      )
    ) |>
    dplyr::ungroup()

  if (nrow(final_stops) != nrow(flights) ||
    anyDuplicated(
      final_stops$div_row_id
    ) > 0L) {
    stop(
      "Final diversion stops are not one-to-one with flights."
    )
  }

  final_stops <- final_stops |>
    dplyr::arrange(
      div_row_id
    )

  if (!identical(
    final_stops$div_row_id,
    flights$div_row_id
  )) {
    stop(
      "Final diversion stops do not align with flight rows."
    )
  }

  final_dep_utc <- final_stops$departure_utc

  # Destination WheelsOn and ArrTime remain independent source events.
  dest_on <- resolve_after_utc(
    anchor_utc = final_dep_utc,
    clock = flights$WheelsOn,
    timezone = dest_tz,
    field = "WheelsOn"
  )

  dest_arr <- resolve_after_utc(
    anchor_utc = final_dep_utc,
    clock = flights$ArrTime,
    timezone = dest_tz,
    field = "ArrTime"
  )

  both_dest_valid <- reached_dest &
    dest_on$time_status == "valid" &
    dest_arr$time_status == "valid"

  if (any(
    both_dest_valid &
      dest_arr$event_utc <
        dest_on$event_utc
  )) {
    stop(
      "A destination gate arrival precedes destination WheelsOn."
    )
  }

  origin_sched_events <- flights |>
    dplyr::mutate(
      airport_role = "origin",
      event_type = "gate_departure",
      event_time_basis = "scheduled",
      div_number = NA_integer_,
      event_source_field = "CRSDepTime",
      event_airport_id = origin_id,
      event_airport_seq_id = origin_seq,
      event_airport_code = Origin,
      event_timezone = origin_tz,
      event_date = origin_sched$event_date,
      event_clock = origin_sched$event_clock,
      event_utc = origin_sched$event_utc,
      event_time_status =
        origin_sched$time_status,
      is_slc_diversion = FALSE
    )

  origin_dep_events <- flights |>
    dplyr::mutate(
      airport_role = "origin",
      event_type = "gate_departure",
      event_time_basis = "actual",
      div_number = NA_integer_,
      event_source_field = "DepTime",
      event_airport_id = origin_id,
      event_airport_seq_id = origin_seq,
      event_airport_code = Origin,
      event_timezone = origin_tz,
      event_date = dep_actual$event_date,
      event_clock = dep_actual$event_clock,
      event_utc = dep_actual$event_utc,
      event_time_status =
        dep_actual$time_status,
      is_slc_diversion = FALSE
    )

  origin_off_events <- flights |>
    dplyr::mutate(
      airport_role = "origin",
      event_type = "wheels_off",
      event_time_basis = "actual",
      div_number = NA_integer_,
      event_source_field = "WheelsOff",
      event_airport_id = origin_id,
      event_airport_seq_id = origin_seq,
      event_airport_code = Origin,
      event_timezone = origin_tz,
      event_date = origin_off$event_date,
      event_clock = origin_off$event_clock,
      event_utc = origin_off$event_utc,
      event_time_status =
        origin_off$time_status,
      is_slc_diversion = FALSE
    )

  dest_sched_events <- flights |>
    dplyr::mutate(
      airport_role = "scheduled_destination",
      event_type = "gate_arrival",
      event_time_basis = "scheduled",
      div_number = NA_integer_,
      event_source_field = "CRSArrTime",
      event_airport_id = dest_id,
      event_airport_seq_id = dest_seq,
      event_airport_code = Dest,
      event_timezone = dest_tz,
      event_date = dest_sched$event_date,
      event_clock = dest_sched$event_clock,
      event_utc = dest_sched$event_utc,
      event_time_status = dest_status,
      is_slc_diversion = FALSE
    )

  stop_events <- flights |>
    dplyr::left_join(
      stops,
      by = "div_row_id",
      relationship = "one-to-many"
    )

  div_on_events <- stop_events |>
    dplyr::mutate(
      airport_role = "diversion",
      event_type = "wheels_on",
      event_time_basis = "actual",
      event_source_field = paste0(
        "Div",
        div_number,
        "WheelsOn"
      ),
      event_airport_id = div_airport_id,
      event_airport_seq_id =
        div_airport_seq_id,
      event_airport_code =
        div_airport_code,
      event_timezone = div_timezone,
      event_date = arrival_date,
      event_clock = arrival_clock,
      event_utc = arrival_utc,
      event_time_status = arrival_status,
      is_slc_diversion =
        div_airport_id == slc_id
    )

  div_off_events <- stop_events |>
    dplyr::filter(
      departure_applicable
    ) |>
    dplyr::mutate(
      airport_role = "diversion",
      event_type = "wheels_off",
      event_time_basis = "actual",
      event_source_field = paste0(
        "Div",
        div_number,
        "WheelsOff"
      ),
      event_airport_id = div_airport_id,
      event_airport_seq_id =
        div_airport_seq_id,
      event_airport_code =
        div_airport_code,
      event_timezone = div_timezone,
      event_date = departure_date,
      event_clock = departure_clock,
      event_utc = departure_utc,
      event_time_status = departure_status,
      is_slc_diversion =
        div_airport_id == slc_id
    )

  reached_id <- which(
    reached_dest
  )

  dest_on_events <- flights[
    reached_id,
  ] |>
    dplyr::mutate(
      airport_role = "scheduled_destination",
      event_type = "wheels_on",
      event_time_basis = "actual",
      div_number = NA_integer_,
      event_source_field = "WheelsOn",
      event_airport_id =
        dest_id[reached_id],
      event_airport_seq_id =
        dest_seq[reached_id],
      event_airport_code = Dest,
      event_timezone =
        dest_tz[reached_id],
      event_date =
        dest_on$event_date[reached_id],
      event_clock =
        dest_on$event_clock[reached_id],
      event_utc =
        dest_on$event_utc[reached_id],
      event_time_status =
        dest_on$time_status[reached_id],
      is_slc_diversion = FALSE
    )

  dest_arr_events <- flights[
    reached_id,
  ] |>
    dplyr::mutate(
      airport_role = "scheduled_destination",
      event_type = "gate_arrival",
      event_time_basis = "actual",
      div_number = NA_integer_,
      event_source_field = "ArrTime",
      event_airport_id =
        dest_id[reached_id],
      event_airport_seq_id =
        dest_seq[reached_id],
      event_airport_code = Dest,
      event_timezone =
        dest_tz[reached_id],
      event_date =
        dest_arr$event_date[reached_id],
      event_clock =
        dest_arr$event_clock[reached_id],
      event_utc =
        dest_arr$event_utc[reached_id],
      event_time_status =
        dest_arr$time_status[reached_id],
      is_slc_diversion = FALSE
    )

  events <- dplyr::bind_rows(
    origin_sched_events,
    origin_dep_events,
    origin_off_events,
    dest_sched_events,
    div_on_events,
    div_off_events,
    dest_on_events,
    dest_arr_events
  )

  expected_n <- 4L * nrow(flights) +
    nrow(stops) +
    sum(stops$departure_applicable) +
    2L * sum(reached_dest)

  if (nrow(events) != expected_n) {
    stop(
      "Diversion-event preparation changed the event grain."
    )
  }

  core_cols <- c(
    "div_row_id",
    "airport_role",
    "event_type",
    "event_time_basis",
    "div_number",
    "event_source_field",
    "event_airport_id",
    "event_airport_seq_id",
    "event_airport_code",
    "event_timezone",
    "event_date",
    "event_clock",
    "event_utc",
    "event_time_status",
    "is_slc_diversion"
  )

  events <- events |>
    dplyr::select(
      dplyr::all_of(
        core_cols
      ),
      dplyr::all_of(
        flight_cols
      )
    )

  # Output order is structural only. Chronology must use event_utc.
  role_order <- dplyr::case_when(
    events$airport_role == "origin" ~ 1L,
    events$airport_role == "diversion" ~ 2L,
    TRUE ~ 3L
  )

  basis_order <- dplyr::case_when(
    events$event_time_basis == "scheduled" ~ 1L,
    TRUE ~ 2L
  )

  type_order <- dplyr::case_when(
    events$event_type == "gate_departure" ~ 1L,
    events$event_type == "wheels_on" ~ 2L,
    events$event_type == "wheels_off" ~ 3L,
    TRUE ~ 4L
  )

  events |>
    dplyr::mutate(
      .role_order = role_order,
      .basis_order = basis_order,
      .type_order = type_order,
      .div_order = dplyr::coalesce(
        div_number,
        0L
      )
    ) |>
    dplyr::arrange(
      FlightDate,
      div_row_id,
      .role_order,
      .div_order,
      .basis_order,
      .type_order
    ) |>
    dplyr::select(
      -.role_order,
      -.basis_order,
      -.type_order,
      -.div_order
    )
}

prepare_div_intervals <- function(events) {
  if (!is.data.frame(events)) {
    stop(
      "`events` must be a data frame."
    )
  }

  if (nrow(events) == 0L) {
    stop(
      "`events` contains no diversion events."
    )
  }

  required_cols <- c(
    "div_row_id",
    "FlightDate",
    "airport_role",
    "event_type",
    "event_time_basis",
    "div_number",
    "event_source_field",
    "event_airport_id",
    "event_airport_seq_id",
    "event_airport_code",
    "event_timezone",
    "event_date",
    "event_clock",
    "event_utc",
    "event_time_status"
  )

  missing_cols <- setdiff(
    required_cols,
    names(events)
  )

  if (length(missing_cols) > 0L) {
    print(
      missing_cols
    )

    stop(
      "`events` lacks required diversion-event fields."
    )
  }

  if (!is.integer(events$div_row_id) ||
    anyNA(events$div_row_id) ||
    any(events$div_row_id <= 0L)) {
    stop(
      "`div_row_id` must contain positive integer values."
    )
  }

  if (!inherits(
    events$FlightDate,
    "Date"
  ) ||
    anyNA(
      events$FlightDate
    )) {
    stop(
      "`FlightDate` must contain non-missing Date values."
    )
  }

  if (!is.character(
    events$airport_role
  ) ||
    anyNA(
      events$airport_role
    )) {
    stop(
      "`airport_role` must be non-missing character."
    )
  }

  valid_roles <- c(
    "origin",
    "scheduled_destination",
    "diversion"
  )

  if (any(
    !events$airport_role %in%
      valid_roles
  )) {
    stop(
      "`airport_role` contains an unknown value."
    )
  }

  if (!is.character(
    events$event_type
  ) ||
    anyNA(
      events$event_type
    )) {
    stop(
      "`event_type` must be non-missing character."
    )
  }

  valid_types <- c(
    "gate_departure",
    "gate_arrival",
    "wheels_off",
    "wheels_on"
  )

  if (any(
    !events$event_type %in%
      valid_types
  )) {
    stop(
      "`event_type` contains an unknown value."
    )
  }

  if (!is.character(
    events$event_time_basis
  ) ||
    anyNA(
      events$event_time_basis
    )) {
    stop(
      "`event_time_basis` must be non-missing character."
    )
  }

  valid_basis <- c(
    "scheduled",
    "actual"
  )

  if (any(
    !events$event_time_basis %in%
      valid_basis
  )) {
    stop(
      "`event_time_basis` contains an unknown value."
    )
  }

  if (!is.integer(
    events$div_number
  )) {
    stop(
      "`div_number` must be integer."
    )
  }

  if (!is.character(
    events$event_source_field
  ) ||
    anyNA(
      events$event_source_field
    ) ||
    any(
      !nzchar(
        events$event_source_field
      )
    )) {
    stop(
      "`event_source_field` must be non-missing character."
    )
  }

  if (!inherits(
    events$event_date,
    "Date"
  )) {
    stop(
      "`event_date` must be Date."
    )
  }

  if (!inherits(
    events$event_utc,
    "POSIXct"
  )) {
    stop(
      "`event_utc` must be POSIXct."
    )
  }

  if (!is.character(
    events$event_time_status
  ) ||
    anyNA(
      events$event_time_status
    )) {
    stop(
      "`event_time_status` must be non-missing character."
    )
  }

  valid_status <- c(
    "valid",
    "ambiguous",
    "nonexistent",
    "source_unresolved"
  )

  if (any(
    !events$event_time_status %in%
      valid_status
  )) {
    stop(
      "`event_time_status` contains an unknown value."
    )
  }

  actual <- events |>
    dplyr::filter(
      event_time_basis == "actual"
    )

  if (nrow(actual) == 0L) {
    stop(
      "`events` contains no actual diversion events."
    )
  }

  valid_time <- actual$event_time_status ==
    "valid"

  if (any(
    valid_time &
      is.na(actual$event_utc)
  )) {
    stop(
      "A valid actual event lacks its UTC instant."
    )
  }

  if (any(
    !valid_time &
      !is.na(actual$event_utc)
  )) {
    stop(
      "An unresolved actual event has a UTC instant."
    )
  }

  event_key <- actual |>
    dplyr::count(
      div_row_id,
      event_source_field,
      name = "n"
    ) |>
    dplyr::filter(
      n != 1L
    )

  if (nrow(event_key) > 0L) {
    print(
      event_key
    )

    stop(
      "An actual BTS event source is not unique within a flight."
    )
  }

  flight_tbl <- events |>
    dplyr::distinct(
      div_row_id,
      FlightDate
    )

  if (anyDuplicated(
    flight_tbl$div_row_id
  ) > 0L) {
    stop(
      "`FlightDate` is not constant within a diversion flight."
    )
  }

  div_on <- actual |>
    dplyr::filter(
      airport_role == "diversion",
      event_type == "wheels_on"
    )

  if (nrow(div_on) == 0L) {
    stop(
      "No diversion landing events are available."
    )
  }

  if (anyNA(div_on$div_number) ||
    any(
      div_on$div_number < 1L |
        div_on$div_number > 5L
    )) {
    stop(
      "A diversion landing has an invalid diversion number."
    )
  }

  expected_on <- paste0(
    "Div",
    div_on$div_number,
    "WheelsOn"
  )

  if (any(
    div_on$event_source_field !=
      expected_on
  )) {
    stop(
      "A diversion landing has the wrong BTS source field."
    )
  }

  div_off <- actual |>
    dplyr::filter(
      airport_role == "diversion",
      event_type == "wheels_off"
    )

  if (nrow(div_off) > 0L) {
    if (anyNA(div_off$div_number) ||
      any(
        div_off$div_number < 1L |
          div_off$div_number > 5L
      )) {
      stop(
        "A diversion departure has an invalid diversion number."
      )
    }

    expected_off <- paste0(
      "Div",
      div_off$div_number,
      "WheelsOff"
    )

    if (any(
      div_off$event_source_field !=
        expected_off
    )) {
      stop(
        "A diversion departure has the wrong BTS source field."
      )
    }
  }

  stop_tbl <- div_on |>
    dplyr::select(
      div_row_id,
      div_number
    )

  if (anyDuplicated(
    stop_tbl
  ) > 0L) {
    stop(
      "A diversion flight contains a duplicate landing number."
    )
  }

  stop_sum <- stop_tbl |>
    dplyr::group_by(
      div_row_id
    ) |>
    dplyr::summarise(
      stop_count = dplyr::n(),
      max_div = max(
        div_number
      ),
      .groups = "drop"
    )

  if (any(
    stop_sum$stop_count !=
      stop_sum$max_div
  )) {
    stop(
      "A diversion landing sequence contains a gap."
    )
  }

  missing_stop <- setdiff(
    flight_tbl$div_row_id,
    stop_sum$div_row_id
  )

  if (length(missing_stop) > 0L) {
    stop(
      "At least one diversion flight lacks a landing event."
    )
  }

  dest_on <- actual |>
    dplyr::filter(
      airport_role == "scheduled_destination",
      event_type == "wheels_on"
    ) |>
    dplyr::select(
      div_row_id
    )

  dest_arr <- actual |>
    dplyr::filter(
      airport_role == "scheduled_destination",
      event_type == "gate_arrival"
    ) |>
    dplyr::select(
      div_row_id
    )

  if (anyDuplicated(
    dest_on$div_row_id
  ) > 0L ||
    anyDuplicated(
      dest_arr$div_row_id
    ) > 0L) {
    stop(
      "A flight has duplicate actual destination events."
    )
  }

  if (!setequal(
    dest_on$div_row_id,
    dest_arr$div_row_id
  )) {
    stop(
      paste(
        "Destination WheelsOn and gate-arrival",
        "applicability disagree."
      )
    )
  }

  reached_tbl <- flight_tbl |>
    dplyr::transmute(
      div_row_id,
      reached_dest =
        div_row_id %in%
          dest_on$div_row_id
    )

  stop_info <- stop_tbl |>
    dplyr::left_join(
      stop_sum,
      by = "div_row_id",
      relationship = "many-to-one",
      na_matches = "never"
    ) |>
    dplyr::left_join(
      reached_tbl,
      by = "div_row_id",
      relationship = "many-to-one",
      na_matches = "never"
    )

  origin_spec <- stop_sum |>
    dplyr::transmute(
      div_row_id,
      interval_type = "airborne",
      interval_stage =
        "origin_to_diversion",
      interval_number = 1L,
      start_source_field = "WheelsOff",
      end_source_field =
        "Div1WheelsOn",
      path_order = 1L
    )

  ground_spec <- stop_info |>
    dplyr::filter(
      div_number < max_div |
        reached_dest
    ) |>
    dplyr::transmute(
      div_row_id,
      interval_type =
        "diversion_ground",
      interval_stage =
        "diversion_ground",
      interval_number = div_number,
      start_source_field = paste0(
        "Div",
        div_number,
        "WheelsOn"
      ),
      end_source_field = paste0(
        "Div",
        div_number,
        "WheelsOff"
      ),
      path_order = 2L * div_number
    )

  cont_spec <- stop_info |>
    dplyr::filter(
      div_number < max_div
    ) |>
    dplyr::transmute(
      div_row_id,
      interval_type = "airborne",
      interval_stage =
        "diversion_to_diversion",
      interval_number =
        div_number + 1L,
      start_source_field = paste0(
        "Div",
        div_number,
        "WheelsOff"
      ),
      end_source_field = paste0(
        "Div",
        div_number + 1L,
        "WheelsOn"
      ),
      path_order =
        2L * div_number + 1L
    )

  final_spec <- stop_sum |>
    dplyr::left_join(
      reached_tbl,
      by = "div_row_id",
      relationship = "one-to-one",
      na_matches = "never"
    ) |>
    dplyr::filter(
      reached_dest
    ) |>
    dplyr::transmute(
      div_row_id,
      interval_type = "airborne",
      interval_stage =
        "diversion_to_destination",
      interval_number = max_div + 1L,
      start_source_field = paste0(
        "Div",
        max_div,
        "WheelsOff"
      ),
      end_source_field = "WheelsOn",
      path_order = 2L * max_div + 1L
    )

  spec <- dplyr::bind_rows(
    origin_spec,
    ground_spec,
    cont_spec,
    final_spec
  ) |>
    dplyr::left_join(
      flight_tbl,
      by = "div_row_id",
      relationship = "many-to-one",
      na_matches = "never"
    )

  interval_key <- spec |>
    dplyr::count(
      div_row_id,
      interval_type,
      interval_number,
      name = "n"
    ) |>
    dplyr::filter(
      n != 1L
    )

  if (nrow(interval_key) > 0L) {
    print(
      interval_key
    )

    stop(
      "An operational interval key is not unique."
    )
  }

  endpoint <- actual |>
    dplyr::transmute(
      div_row_id,
      source_field = event_source_field,
      event_type,
      airport_role,
      div_number,
      airport_id = event_airport_id,
      airport_seq_id =
        event_airport_seq_id,
      airport_code =
        event_airport_code,
      timezone = event_timezone,
      event_date,
      event_clock,
      event_utc,
      time_status =
        event_time_status
    )

  start_tbl <- endpoint |>
    dplyr::rename(
      start_source_field =
        source_field,
      start_event_type =
        event_type,
      start_airport_role =
        airport_role,
      start_div_number =
        div_number,
      start_airport_id =
        airport_id,
      start_airport_seq_id =
        airport_seq_id,
      start_airport_code =
        airport_code,
      start_timezone =
        timezone,
      start_date =
        event_date,
      start_clock =
        event_clock,
      start_utc =
        event_utc,
      start_time_status =
        time_status
    )

  end_tbl <- endpoint |>
    dplyr::rename(
      end_source_field =
        source_field,
      end_event_type =
        event_type,
      end_airport_role =
        airport_role,
      end_div_number =
        div_number,
      end_airport_id =
        airport_id,
      end_airport_seq_id =
        airport_seq_id,
      end_airport_code =
        airport_code,
      end_timezone =
        timezone,
      end_date =
        event_date,
      end_clock =
        event_clock,
      end_utc =
        event_utc,
      end_time_status =
        time_status
    )

  joined <- spec |>
    dplyr::left_join(
      start_tbl,
      by = c(
        "div_row_id",
        "start_source_field"
      ),
      relationship = "many-to-one",
      na_matches = "never"
    ) |>
    dplyr::left_join(
      end_tbl,
      by = c(
        "div_row_id",
        "end_source_field"
      ),
      relationship = "many-to-one",
      na_matches = "never"
    )

  if (nrow(joined) != nrow(spec)) {
    stop(
      "Endpoint joining changed the interval candidate count."
    )
  }

  missing_endpoint <-
    is.na(joined$start_event_type) |
      is.na(joined$end_event_type)

  if (any(missing_endpoint)) {
    print(
      joined |>
        dplyr::filter(
          missing_endpoint
        ) |>
        dplyr::select(
          div_row_id,
          interval_type,
          interval_stage,
          interval_number,
          start_source_field,
          end_source_field
        )
    )

    stop(
      "An applicable interval lacks a represented endpoint."
    )
  }

  is_origin <- joined$interval_stage ==
    "origin_to_diversion"

  bad_origin <- is_origin &
    (
      joined$start_airport_role != "origin" |
        joined$start_event_type != "wheels_off" |
        joined$end_airport_role != "diversion" |
        joined$end_event_type != "wheels_on" |
        joined$end_div_number != 1L
    )

  is_ground <- joined$interval_stage ==
    "diversion_ground"

  bad_ground <- is_ground &
    (
      joined$start_airport_role != "diversion" |
        joined$end_airport_role != "diversion" |
        joined$start_event_type != "wheels_on" |
        joined$end_event_type != "wheels_off" |
        joined$start_div_number !=
          joined$end_div_number |
        joined$start_airport_id !=
          joined$end_airport_id
    )

  is_cont <- joined$interval_stage ==
    "diversion_to_diversion"

  bad_cont <- is_cont &
    (
      joined$start_airport_role != "diversion" |
        joined$end_airport_role != "diversion" |
        joined$start_event_type != "wheels_off" |
        joined$end_event_type != "wheels_on" |
        joined$end_div_number !=
          joined$start_div_number + 1L
    )

  is_final <- joined$interval_stage ==
    "diversion_to_destination"

  bad_final <- is_final &
    (
      joined$start_airport_role != "diversion" |
        joined$start_event_type != "wheels_off" |
        joined$end_airport_role !=
          "scheduled_destination" |
        joined$end_event_type != "wheels_on"
    )

  bad_semantics <- bad_origin |
    bad_ground |
    bad_cont |
    bad_final

  if (any(bad_semantics)) {
    stop(
      "An interval has inconsistent endpoint semantics."
    )
  }

  valid_interval <-
    joined$start_time_status == "valid" &
      joined$end_time_status == "valid"

  result <- joined |>
    dplyr::filter(
      valid_interval
    ) |>
    dplyr::mutate(
      interval_duration_min = as.numeric(
        difftime(
          end_utc,
          start_utc,
          units = "mins"
        )
      )
    )

  bad_duration <-
    !is.finite(
      result$interval_duration_min
    ) |
      result$interval_duration_min < 0

  if (any(bad_duration)) {
    stop(
      "An operational interval has an invalid duration."
    )
  }

  result |>
    dplyr::arrange(
      FlightDate,
      div_row_id,
      path_order
    ) |>
    dplyr::mutate(
      div_interval_id =
        dplyr::row_number()
    ) |>
    dplyr::relocate(
      div_interval_id,
      div_row_id,
      FlightDate,
      interval_type,
      interval_stage,
      interval_number,
      start_source_field,
      end_source_field
    ) |>
    dplyr::select(
      -path_order
    )
}
