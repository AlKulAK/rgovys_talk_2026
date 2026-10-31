# Link diversion-only BTS events to airport GHCNh weather.
# 2026-09-04

check_div_weather_events <- function(events) {
  if (!is.data.frame(events)) {
    stop("`events` must be a data frame.")
  }
  
  if (nrow(events) == 0L) {
    stop("`events` contains no diversion events.")
  }
  
  required_cols <- c(
    "div_row_id",
    "event_airport_id",
    "event_airport_seq_id",
    "event_airport_code",
    "event_date",
    "event_utc",
    "event_time_status"
  )
  
  missing_cols <- setdiff(
    required_cols,
    names(events)
  )
  
  if (length(missing_cols) > 0L) {
    print(missing_cols)
    
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
  
  if (!is.integer(events$event_airport_id) ||
      anyNA(events$event_airport_id) ||
      any(events$event_airport_id <= 0L)) {
    stop(
      "`event_airport_id` must contain positive integers."
    )
  }
  
  if (!is.integer(
    events$event_airport_seq_id
  ) ||
  anyNA(
    events$event_airport_seq_id
  ) ||
  any(
    events$event_airport_seq_id <= 0L
  )) {
    stop(
      paste(
        "`event_airport_seq_id` must contain",
        "positive integers."
      )
    )
  }
  
  if (!is.character(
    events$event_airport_code
  ) ||
  anyNA(
    events$event_airport_code
  ) ||
  any(
    !nzchar(
      events$event_airport_code
    )
  )) {
    stop(
      paste(
        "`event_airport_code` must contain",
        "non-missing character values."
      )
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
  
  valid_time <- events$event_time_status ==
    "valid"
  
  if (any(
    valid_time &
    is.na(events$event_date)
  )) {
    stop(
      "A valid diversion event lacks its local date."
    )
  }
  
  if (any(
    valid_time &
    is.na(events$event_utc)
  )) {
    stop(
      "A valid diversion event lacks its UTC instant."
    )
  }
  
  if (any(
    !valid_time &
    !is.na(events$event_utc)
  )) {
    stop(
      "An unresolved diversion event has a UTC instant."
    )
  }
  
  invisible(events)
}


build_div_weather_manifest <- function(
    events,
    meta_dir = file.path(
      "data_raw",
      "ghcnh_metadata"
    )) {
  
  check_div_weather_events(
    events
  )
  
  req_tbl <- events |>
    dplyr::filter(
      !is.na(event_date)
    ) |>
    dplyr::distinct(
      div_row_id,
      event_date,
      event_airport_id,
      event_airport_seq_id,
      event_airport_code
    ) |>
    dplyr::transmute(
      event_date,
      AirportID = event_airport_id,
      AirportSeqID = event_airport_seq_id,
      AirportCode = event_airport_code
    )
  
  if (nrow(req_tbl) == 0L) {
    stop(
      "No dated diversion airport requirement is available."
    )
  }
  
  service_tbl <- build_req_service_tbl(
    req_tbl
  )
  
  req_years <- sort(
    unique(
      as.integer(
        format(
          req_tbl$event_date,
          "%Y"
        )
      )
    )
  )
  
  build_ghcnh_manifest(
    meta_dir = meta_dir,
    years = req_years,
    service_tbl = service_tbl
  )
}


prepare_div_weather_input <- function(
    events,
    manifest) {
  
  check_div_weather_events(
    events
  )
  
  if (!is.data.frame(manifest)) {
    stop(
      "`manifest` must be a data frame."
    )
  }
  
  if ("div_weather_row_id" %in%
      names(events)) {
    stop(
      "`events` already contains `div_weather_row_id`."
    )
  }
  
  input <- events |>
    dplyr::mutate(
      div_weather_row_id =
        dplyr::row_number(),
      other_airport_id =
        event_airport_id,
      other_airport_seq_id =
        event_airport_seq_id,
      other_airport_code =
        event_airport_code,
      other_sched_date =
        event_date,
      other_sched_utc =
        event_utc,
      other_time_status =
        dplyr::if_else(
          event_time_status == "valid",
          "valid",
          "schedule_unresolved"
        ),
      other_time_reason =
        event_time_status
    )
  
  dated <- !is.na(
    input$other_sched_date
  )
  
  if (!any(dated)) {
    stop(
      "No diversion event has a dated airport requirement."
    )
  }
  
  assigned <- assign_other_stations(
    events = input[dated, ],
    manifest = manifest
  )
  
  map_cols <- c(
    "div_weather_row_id",
    "other_map_status",
    "other_manifest_id",
    "other_station_id",
    "other_station_name",
    "other_station_icao",
    "other_map_start",
    "other_map_end",
    "other_selection_basis",
    "other_selection_note"
  )
  
  map_tbl <- assigned |>
    dplyr::select(
      dplyr::all_of(
        map_cols
      )
    )
  
  result <- input |>
    dplyr::left_join(
      map_tbl,
      by = "div_weather_row_id",
      relationship = "one-to-one",
      na_matches = "never"
    ) |>
    dplyr::mutate(
      other_map_status =
        dplyr::if_else(
          is.na(other_sched_date),
          "date_unresolved",
          other_map_status
        )
    )
  
  if (!identical(
    result$div_weather_row_id,
    seq_len(nrow(events))
  )) {
    stop(
      "Diversion station assignment changed event order."
    )
  }
  
  dated_missing <- dated &
    is.na(result$other_station_id)
  
  if (any(dated_missing)) {
    stop(
      "A dated diversion event lacks a station assignment."
    )
  }
  
  undated_station <- !dated &
    !is.na(result$other_station_id)
  
  if (any(undated_station)) {
    stop(
      "An undated diversion event received a station."
    )
  }
  
  if (nrow(result) != nrow(events)) {
    stop(
      "Diversion station assignment changed the event count."
    )
  }
  
  result
}


match_div_weather <- function(
    events,
    manifest,
    data_dir = file.path(
      "data_raw",
      "ghcnh"
    ),
    max_dist_min = 60) {
  
  prepared <- prepare_div_weather_input(
    events = events,
    manifest = manifest
  )
  
  valid_time <- prepared$other_time_status ==
    "valid"
  
  if (!any(valid_time)) {
    stop(
      "No diversion event has a valid weather-linkage time."
    )
  }
  
  match_input <- prepared[
    valid_time,
  ]
  
  matched <- match_other_weather(
    events = match_input,
    data_dir = data_dir,
    max_dist_min = max_dist_min
  )
  
  match_cols <- c(
    "div_weather_row_id",
    "other_weather_station_id",
    "other_weather_raw_time",
    "other_weather_obs_time",
    "other_weather_dist_min",
    "other_weather_match_side",
    "other_weather_match_status",
    "other_weather_aviation_class"
  )
  
  missing_match <- setdiff(
    match_cols,
    names(matched)
  )
  
  if (length(missing_match) > 0L) {
    print(missing_match)
    
    stop(
      "Network weather matching lacks required output fields."
    )
  }
  
  match_tbl <- matched |>
    dplyr::select(
      dplyr::all_of(
        match_cols
      )
    )
  
  result <- prepared |>
    dplyr::left_join(
      match_tbl,
      by = "div_weather_row_id",
      relationship = "one-to-one",
      na_matches = "never"
    ) |>
    dplyr::mutate(
      other_weather_match_status =
        dplyr::if_else(
          other_time_status != "valid",
          "time_unresolved",
          other_weather_match_status
        )
    )
  
  valid_missing <- valid_time &
    is.na(
      result$other_weather_match_status
    )
  
  if (any(valid_missing)) {
    stop(
      "A valid diversion event lacks a weather-match status."
    )
  }
  
  unresolved_obs <- !valid_time &
    !is.na(
      result$other_weather_obs_time
    )
  
  if (any(unresolved_obs)) {
    stop(
      "An unresolved diversion event received weather."
    )
  }
  
  if (!identical(
    result$div_weather_row_id,
    seq_len(nrow(events))
  )) {
    stop(
      "Diversion weather matching changed event order."
    )
  }
  
  if (nrow(result) != nrow(events)) {
    stop(
      "Diversion weather matching changed the event count."
    )
  }
  
  result
}


attach_div_weather <- function(
    events,
    manifest,
    elements,
    data_dir = file.path(
      "data_raw",
      "ghcnh"
    ),
    max_dist_min = 60) {
  
  matched <- match_div_weather(
    events = events,
    manifest = manifest,
    data_dir = data_dir,
    max_dist_min = max_dist_min
  )
  
  attached <- attach_other_weather(
    events = matched,
    elements = elements,
    data_dir = data_dir
  )
  
  result <- attached |>
    dplyr::rename(
      event_map_status =
        other_map_status,
      event_manifest_id =
        other_manifest_id,
      event_station_id =
        other_station_id,
      event_station_name =
        other_station_name,
      event_station_icao =
        other_station_icao,
      event_map_start =
        other_map_start,
      event_map_end =
        other_map_end,
      event_selection_basis =
        other_selection_basis,
      event_selection_note =
        other_selection_note
    ) |>
    dplyr::rename_with(
      .fn = function(x) {
        sub(
          "^other_weather_",
          "event_weather_",
          x
        )
      },
      .cols = dplyr::starts_with(
        "other_weather_"
      )
    ) |>
    dplyr::select(
      -div_weather_row_id,
      -other_airport_id,
      -other_airport_seq_id,
      -other_airport_code,
      -other_sched_date,
      -other_sched_utc,
      -other_time_status,
      -other_time_reason
    )
  
  other_cols <- grep(
    "^other_",
    names(result),
    value = TRUE
  )
  
  if (length(other_cols) > 0L) {
    print(other_cols)
    
    stop(
      "Diversion weather output retains adapter fields."
    )
  }
  
  if (nrow(result) != nrow(events)) {
    stop(
      "Diversion weather attachment changed the event count."
    )
  }
  
  result
}


build_div_weather_context <- function(
    events,
    meta_dir = file.path(
      "data_raw",
      "ghcnh_metadata"
    )) {
  
  check_div_weather_events(
    events
  )
  
  required_cols <- c(
    "FlightDate",
    "airport_role",
    "event_type",
    "event_time_basis",
    "div_number",
    "event_source_field",
    "event_timezone"
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
      "`events` lacks fields required for weather context."
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
  
  if (!is.character(events$airport_role) ||
      anyNA(events$airport_role) ||
      !is.character(events$event_type) ||
      anyNA(events$event_type) ||
      !is.character(events$event_time_basis) ||
      anyNA(events$event_time_basis) ||
      !is.character(events$event_source_field) ||
      anyNA(events$event_source_field) ||
      !is.character(events$event_timezone) ||
      anyNA(events$event_timezone) ||
      any(!nzchar(events$event_timezone))) {
    stop(
      "Diversion weather-context fields have unexpected types."
    )
  }
  
  if (!is.integer(events$div_number)) {
    stop(
      "`div_number` must be integer."
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
  
  start_tbl <- events |>
    dplyr::filter(
      airport_role == "origin",
      event_type == "gate_departure",
      event_time_basis == "scheduled",
      event_source_field == "CRSDepTime"
    ) |>
    dplyr::select(
      div_row_id,
      operational_start_utc = event_utc,
      operational_start_status = event_time_status
    )
  
  if (nrow(start_tbl) != nrow(flight_tbl) ||
      anyDuplicated(start_tbl$div_row_id) > 0L) {
    stop(
      "Scheduled origin departure is not one-to-one with flights."
    )
  }
  
  if (any(
    start_tbl$operational_start_status != "valid"
  ) ||
  anyNA(
    start_tbl$operational_start_utc
  )) {
    stop(
      "A flight lacks a valid scheduled origin departure."
    )
  }
  
  sched_end_tbl <- events |>
    dplyr::filter(
      airport_role == "scheduled_destination",
      event_type == "gate_arrival",
      event_time_basis == "scheduled",
      event_source_field == "CRSArrTime"
    ) |>
    dplyr::select(
      div_row_id,
      scheduled_end_utc = event_utc,
      scheduled_end_status = event_time_status
    )
  
  if (nrow(sched_end_tbl) != nrow(flight_tbl) ||
      anyDuplicated(sched_end_tbl$div_row_id) > 0L) {
    stop(
      "Scheduled destination arrival is not one-to-one with flights."
    )
  }
  
  if (any(
    sched_end_tbl$scheduled_end_status != "valid"
  ) ||
  anyNA(
    sched_end_tbl$scheduled_end_utc
  )) {
    stop(
      "A flight lacks a valid scheduled destination arrival."
    )
  }
  
  end_tbl <- events |>
    dplyr::filter(
      airport_role == "scheduled_destination",
      event_type == "gate_arrival",
      event_time_basis == "actual",
      event_source_field == "ArrTime"
    ) |>
    dplyr::select(
      div_row_id,
      operational_end_utc = event_utc,
      operational_end_status = event_time_status
    )
  
  if (anyDuplicated(
    end_tbl$div_row_id
  ) > 0L) {
    stop(
      "Actual destination arrival is not unique within a flight."
    )
  }
  
  last_actual <- events |>
    dplyr::filter(
      event_time_basis == "actual",
      event_time_status == "valid",
      !is.na(event_utc)
    ) |>
    dplyr::group_by(
      div_row_id
    ) |>
    dplyr::summarise(
      last_actual_utc = max(
        event_utc
      ),
      .groups = "drop"
    )
  
  flight_bounds <- flight_tbl |>
    dplyr::left_join(
      start_tbl,
      by = "div_row_id",
      relationship = "one-to-one",
      na_matches = "never"
    ) |>
    dplyr::left_join(
      sched_end_tbl,
      by = "div_row_id",
      relationship = "one-to-one",
      na_matches = "never"
    ) |>
    dplyr::left_join(
      end_tbl,
      by = "div_row_id",
      relationship = "one-to-one",
      na_matches = "never"
    ) |>
    dplyr::left_join(
      last_actual,
      by = "div_row_id",
      relationship = "one-to-one",
      na_matches = "never"
    ) |>
    dplyr::mutate(
      operational_end_status = dplyr::coalesce(
        operational_end_status,
        "not_reached"
      ),
      context_anchor_utc = dplyr::case_when(
        operational_end_status == "valid" ~
          operational_end_utc,
        !is.na(last_actual_utc) &
          last_actual_utc >
          scheduled_end_utc ~
          last_actual_utc,
        TRUE ~
          scheduled_end_utc
      ),
      context_end_basis = dplyr::case_when(
        operational_end_status == "valid" ~
          "actual_arrival",
        !is.na(last_actual_utc) &
          last_actual_utc >
          scheduled_end_utc ~
          "last_valid_actual_event",
        TRUE ~
          "scheduled_arrival"
      )
    )
  
  if (any(
    flight_bounds$scheduled_end_utc <
    flight_bounds$operational_start_utc
  )) {
    stop(
      "Scheduled arrival precedes scheduled departure."
    )
  }
  
  if (anyNA(
    flight_bounds$context_anchor_utc
  )) {
    stop(
      "A flight lacks a UTC endpoint for weather context."
    )
  }
  
  if (any(
    flight_bounds$context_anchor_utc <
    flight_bounds$operational_start_utc
  )) {
    stop(
      "A weather-context endpoint precedes scheduled departure."
    )
  }
  
  location_cols <- c(
    "div_row_id",
    "airport_role",
    "div_number",
    "event_airport_id",
    "event_airport_seq_id",
    "event_airport_code",
    "event_timezone"
  )
  
  locations <- events |>
    dplyr::select(
      dplyr::all_of(
        location_cols
      )
    ) |>
    dplyr::distinct()
  
  location_key <- locations |>
    dplyr::count(
      div_row_id,
      airport_role,
      div_number,
      name = "n"
    ) |>
    dplyr::filter(
      n != 1L
    )
  
  if (nrow(location_key) > 0L) {
    print(
      location_key
    )
    
    stop(
      "A flight-location key maps to multiple airport identities."
    )
  }
  
  locations <- locations |>
    dplyr::left_join(
      flight_bounds,
      by = "div_row_id",
      relationship = "many-to-one",
      na_matches = "never"
    )
  
  start_local <- utc_local_parts(
    event_utc = locations$operational_start_utc,
    timezone = locations$event_timezone
  )
  
  end_local <- utc_local_parts(
    event_utc = locations$context_anchor_utc,
    timezone = locations$event_timezone
  )
  
  role_order <- dplyr::case_when(
    locations$airport_role == "origin" ~ 1L,
    locations$airport_role == "diversion" ~ 2L,
    locations$airport_role ==
      "scheduled_destination" ~ 3L,
    TRUE ~ 4L
  )
  
  if (any(role_order == 4L)) {
    stop(
      "A flight location has an unknown airport role."
    )
  }
  
  locations <- locations |>
    dplyr::mutate(
      context_start_date = start_local$event_date,
      context_end_date = end_local$event_date,
      .role_order = role_order,
      .div_order = dplyr::coalesce(
        div_number,
        0L
      )
    ) |>
    dplyr::arrange(
      FlightDate,
      div_row_id,
      .role_order,
      .div_order
    ) |>
    dplyr::mutate(
      div_location_id = dplyr::row_number()
    ) |>
    dplyr::select(
      -.role_order,
      -.div_order
    ) |>
    dplyr::relocate(
      div_location_id
    )
  
  context_n <- as.integer(
    locations$context_end_date -
      locations$context_start_date
  ) + 1L
  
  if (anyNA(context_n) ||
      any(context_n < 1L)) {
    stop(
      "A flight location has an invalid local context-day span."
    )
  }
  
  requirements <- locations |>
    dplyr::mutate(
      context_n = context_n
    ) |>
    tidyr::uncount(
      weights = context_n,
      .id = "context_day_number"
    ) |>
    dplyr::mutate(
      context_date = context_start_date +
        context_day_number - 1L,
      div_context_id = dplyr::row_number(),
      other_airport_id = event_airport_id,
      other_airport_seq_id = event_airport_seq_id,
      other_airport_code = event_airport_code,
      other_sched_date = context_date,
      other_sched_utc = as.POSIXct(
        NA_real_,
        origin = "1970-01-01",
        tz = "UTC"
      ),
      other_time_status = "schedule_unresolved",
      other_time_reason = "context_date"
    ) |>
    dplyr::relocate(
      div_context_id,
      .after = div_location_id
    )
  
  req_tbl <- requirements |>
    dplyr::transmute(
      event_date = context_date,
      AirportID = event_airport_id,
      AirportSeqID = event_airport_seq_id,
      AirportCode = event_airport_code
    ) |>
    dplyr::distinct()
  
  service_tbl <- build_req_service_tbl(
    req_tbl
  )
  
  req_years <- sort(
    unique(
      as.integer(
        format(
          req_tbl$event_date,
          "%Y"
        )
      )
    )
  )
  
  manifest_obj <- build_ghcnh_manifest(
    meta_dir = meta_dir,
    years = req_years,
    service_tbl = service_tbl
  )
  
  assigned <- assign_other_stations(
    events = requirements,
    manifest = manifest_obj$manifest
  )
  
  if (nrow(assigned) !=
      nrow(requirements)) {
    stop(
      "Station assignment changed the context requirement count."
    )
  }
  
  if (any(
    assigned$other_map_status != "mapped"
  ) ||
  anyNA(
    assigned$other_station_id
  )) {
    stop(
      "A context day lacks a station mapping."
    )
  }
  
  day_start <- resolve_local_utc(
    date = assigned$context_date,
    clock = rep(
      "0000",
      nrow(assigned)
    ),
    timezone = assigned$event_timezone,
    field = "context_start"
  )
  
  day_end <- resolve_local_utc(
    date = assigned$context_date + 1L,
    clock = rep(
      "0000",
      nrow(assigned)
    ),
    timezone = assigned$event_timezone,
    field = "context_end"
  )
  
  if (any(
    day_start$time_status != "valid"
  ) ||
  any(
    day_end$time_status != "valid"
  )) {
    stop(
      "A local weather-context day boundary is unresolved."
    )
  }
  
  requirements <- assigned |>
    dplyr::mutate(
      series_start_utc = day_start$event_utc,
      series_end_utc = day_end$event_utc
    ) |>
    dplyr::rename(
      station_id = other_station_id,
      station_name = other_station_name,
      station_icao = other_station_icao,
      manifest_id = other_manifest_id,
      map_start = other_map_start,
      map_end = other_map_end,
      selection_basis = other_selection_basis,
      selection_note = other_selection_note
    ) |>
    dplyr::select(
      -other_airport_id,
      -other_airport_seq_id,
      -other_airport_code,
      -other_sched_date,
      -other_sched_utc,
      -other_time_status,
      -other_time_reason,
      -other_map_status
    )
  
  series_key <- requirements |>
    dplyr::distinct(
      station_id,
      event_timezone,
      context_date,
      series_start_utc,
      series_end_utc
    ) |>
    dplyr::arrange(
      station_id,
      event_timezone,
      context_date
    ) |>
    dplyr::mutate(
      weather_series_id = dplyr::row_number()
    ) |>
    dplyr::relocate(
      weather_series_id
    )
  
  requirements <- requirements |>
    dplyr::left_join(
      series_key,
      by = c(
        "station_id",
        "event_timezone",
        "context_date",
        "series_start_utc",
        "series_end_utc"
      ),
      relationship = "many-to-one",
      na_matches = "never"
    )
  
  if (anyNA(
    requirements$weather_series_id
  )) {
    stop(
      "A context requirement lacks a weather-series key."
    )
  }
  
  year_start <- lubridate::year(
    series_key$series_start_utc
  )
  
  year_end <- lubridate::year(
    series_key$series_end_utc -
      lubridate::seconds(1)
  )
  
  year_n <- year_end -
    year_start + 1L
  
  if (anyNA(year_n) ||
      any(year_n < 1L)) {
    stop(
      "A weather series has an invalid GHCNh year span."
    )
  }
  
  series_years <- series_key |>
    dplyr::mutate(
      year_start = year_start,
      year_n = year_n
    ) |>
    tidyr::uncount(
      weights = year_n,
      .id = "year_number"
    ) |>
    dplyr::transmute(
      weather_series_id,
      station_id,
      year = as.integer(
        year_start +
          year_number - 1L
      )
    )
  
  download_plan <- series_years |>
    dplyr::distinct(
      station_id,
      year
    ) |>
    dplyr::arrange(
      station_id,
      year
    )
  
  markers <- events |>
    dplyr::left_join(
      locations |>
        dplyr::select(
          div_location_id,
          div_row_id,
          airport_role,
          div_number
        ),
      by = c(
        "div_row_id",
        "airport_role",
        "div_number"
      ),
      relationship = "many-to-one"
    ) |>
    dplyr::relocate(
      div_location_id,
      .after = div_row_id
    )
  
  if (anyNA(
    markers$div_location_id
  )) {
    stop(
      "A diversion event lacks its flight-location key."
    )
  }
  
  list(
    locations = locations,
    requirements = requirements,
    series_key = series_key,
    series_years = series_years,
    markers = markers,
    manifest = manifest_obj$manifest,
    download_plan = download_plan,
    station_year_use =
      manifest_obj$station_year_use,
    metadata_provenance =
      manifest_obj$metadata_provenance
  )
}


build_div_weather_series <- function(
    context,
    data_dir = file.path(
      "data_raw",
      "ghcnh"
    )) {
  
  if (!is.list(context)) {
    stop(
      "`context` must be a diversion weather-context list."
    )
  }
  
  required_names <- c(
    "series_key",
    "series_years",
    "download_plan"
  )
  
  missing_names <- setdiff(
    required_names,
    names(context)
  )
  
  if (length(missing_names) > 0L) {
    print(
      missing_names
    )
    
    stop(
      "`context` lacks required weather-series objects."
    )
  }
  
  series_key <- context$series_key
  series_years <- context$series_years
  plan <- context$download_plan
  
  if (!is.data.frame(series_key) ||
      nrow(series_key) == 0L) {
    stop(
      "`series_key` must be a non-empty data frame."
    )
  }
  
  if (!is.data.frame(series_years) ||
      nrow(series_years) == 0L) {
    stop(
      "`series_years` must be a non-empty data frame."
    )
  }
  
  if (!is.data.frame(plan) ||
      nrow(plan) == 0L) {
    stop(
      "`download_plan` must be a non-empty data frame."
    )
  }
  
  key_cols <- c(
    "weather_series_id",
    "station_id",
    "event_timezone",
    "context_date",
    "series_start_utc",
    "series_end_utc"
  )
  
  missing_key <- setdiff(
    key_cols,
    names(series_key)
  )
  
  if (length(missing_key) > 0L) {
    print(
      missing_key
    )
    
    stop(
      "`series_key` lacks required fields."
    )
  }
  
  if (!is.integer(
    series_key$weather_series_id
  ) ||
  anyNA(
    series_key$weather_series_id
  ) ||
  any(
    series_key$weather_series_id <= 0L
  ) ||
  anyDuplicated(
    series_key$weather_series_id
  ) > 0L) {
    stop(
      "`weather_series_id` must be a positive unique integer."
    )
  }
  
  if (!is.character(
    series_key$station_id
  ) ||
  anyNA(
    series_key$station_id
  ) ||
  any(
    !nzchar(
      series_key$station_id
    )
  )) {
    stop(
      "`station_id` must contain non-missing character values."
    )
  }
  
  if (!is.character(
    series_key$event_timezone
  ) ||
  anyNA(
    series_key$event_timezone
  ) ||
  any(
    !nzchar(
      series_key$event_timezone
    )
  )) {
    stop(
      "`event_timezone` must contain valid character values."
    )
  }
  
  if (!inherits(
    series_key$context_date,
    "Date"
  ) ||
  anyNA(
    series_key$context_date
  )) {
    stop(
      "`context_date` must contain non-missing Date values."
    )
  }
  
  if (!inherits(
    series_key$series_start_utc,
    "POSIXct"
  ) ||
  !inherits(
    series_key$series_end_utc,
    "POSIXct"
  )) {
    stop(
      "Weather-series bounds must be POSIXct."
    )
  }
  
  if (anyNA(
    series_key$series_start_utc
  ) ||
  anyNA(
    series_key$series_end_utc
  ) ||
  any(
    series_key$series_end_utc <=
    series_key$series_start_utc
  )) {
    stop(
      "A weather series has invalid UTC bounds."
    )
  }
  
  year_cols <- c(
    "weather_series_id",
    "station_id",
    "year"
  )
  
  missing_year <- setdiff(
    year_cols,
    names(series_years)
  )
  
  if (length(missing_year) > 0L) {
    print(
      missing_year
    )
    
    stop(
      "`series_years` lacks required fields."
    )
  }
  
  plan_cols <- c(
    "station_id",
    "year"
  )
  
  missing_plan <- setdiff(
    plan_cols,
    names(plan)
  )
  
  if (length(missing_plan) > 0L) {
    print(
      missing_plan
    )
    
    stop(
      "`download_plan` lacks required fields."
    )
  }
  
  if (!is.numeric(series_years$year) ||
      anyNA(series_years$year) ||
      any(!is.finite(series_years$year)) ||
      any(
        series_years$year !=
        floor(series_years$year)
      )) {
    stop(
      "`series_years$year` must contain whole-number years."
    )
  }
  
  if (!is.numeric(plan$year) ||
      anyNA(plan$year) ||
      any(!is.finite(plan$year)) ||
      any(
        plan$year !=
        floor(plan$year)
      )) {
    stop(
      "`download_plan$year` must contain whole-number years."
    )
  }
  
  series_years <- series_years |>
    dplyr::mutate(
      year = as.integer(
        year
      )
    )
  
  plan <- plan |>
    dplyr::mutate(
      year = as.integer(
        year
      )
    )
  
  if (anyDuplicated(
    series_years[
      c(
        "weather_series_id",
        "year"
      )
    ]
  ) > 0L) {
    stop(
      "A weather series contains a duplicate GHCNh year."
    )
  }
  
  if (anyDuplicated(
    plan[
      c(
        "station_id",
        "year"
      )
    ]
  ) > 0L) {
    stop(
      "The GHCNh download plan contains a duplicate key."
    )
  }
  
  year_check <- series_years |>
    dplyr::left_join(
      series_key |>
        dplyr::select(
          weather_series_id,
          key_station_id = station_id
        ),
      by = "weather_series_id",
      relationship = "many-to-one",
      na_matches = "never"
    )
  
  if (anyNA(
    year_check$key_station_id
  ) ||
  any(
    year_check$station_id !=
    year_check$key_station_id
  )) {
    stop(
      "A weather-series year has the wrong station identity."
    )
  }
  
  expected_plan <- series_years |>
    dplyr::distinct(
      station_id,
      year
    )
  
  expected_key <- paste(
    expected_plan$station_id,
    expected_plan$year,
    sep = "|"
  )
  
  plan_key <- paste(
    plan$station_id,
    plan$year,
    sep = "|"
  )
  
  if (!setequal(
    expected_key,
    plan_key
  )) {
    stop(
      "The download plan disagrees with the series-year needs."
    )
  }
  
  element_list <- lapply(
    seq_len(
      nrow(plan)
    ),
    function(i) {
      list_ghcnh_elements(
        station_id =
          plan$station_id[[i]],
        data_dir = data_dir,
        years = plan$year[[i]]
      )
    }
  )
  
  element_union <- sort(
    unique(
      unlist(
        element_list,
        use.names = FALSE
      )
    )
  )
  
  element_common <- sort(
    Reduce(
      intersect,
      element_list
    )
  )
  
  if (length(element_union) == 0L) {
    stop(
      "No GHCNh measurement family is available."
    )
  }
  
  schema_audit <- tibble::tibble(
    station_id = plan$station_id,
    year = plan$year,
    element_n = lengths(
      element_list
    )
  )
  
  if (!identical(
    element_union,
    element_common
  )) {
    schema_missing <- lapply(
      seq_along(
        element_list
      ),
      function(i) {
        missing_element <- setdiff(
          element_union,
          element_list[[i]]
        )
        
        tibble::tibble(
          station_id =
            plan$station_id[[i]],
          year =
            plan$year[[i]],
          missing_element =
            missing_element
        )
      }
    ) |>
      dplyr::bind_rows()
    
    print(
      schema_missing
    )
    
    stop(
      paste(
        "Required station-year files do not share",
        "one GHCNh measurement-family schema."
      )
    )
  }
  
  elements <- element_union
  
  station_groups <- split(
    plan$year,
    plan$station_id
  )
  
  station_names <- names(
    station_groups
  )
  
  weather_list <- lapply(
    station_names,
    function(station_value) {
      station_years <- sort(
        unique(
          as.integer(
            station_groups[[station_value]]
          )
        )
      )

      station_key <- series_key[
        series_key$station_id ==
          station_value,
      ]
      
      if (nrow(station_key) == 0L) {
        stop(
          "A required station lacks a weather-series window."
        )
      }
      
      station_weather <- build_ghcnh_weather(
        station_id = station_value,
        elements = elements,
        data_dir = data_dir,
        years = station_years
      )
      
      range_start <- min(
        station_key$series_start_utc
      )
      
      range_end <- max(
        station_key$series_end_utc
      )
      
      station_weather <- station_weather |>
        dplyr::filter(
          obs_time >= range_start,
          obs_time < range_end
        )
      
      station_key |>
        dplyr::inner_join(
          station_weather,
          by = dplyr::join_by(
            station_id,
            series_start_utc <= obs_time,
            series_end_utc > obs_time
          ),
          relationship = "one-to-many",
          na_matches = "never"
        )
    }
  )
  
  weather <- dplyr::bind_rows(
    weather_list
  )
  
  if (nrow(weather) > 0L) {
    local_parts <- utc_local_parts(
      event_utc = weather$obs_time,
      timezone = weather$event_timezone
    )
    
    weather <- weather |>
      dplyr::mutate(
        obs_local_date =
          local_parts$event_date,
        obs_local_clock =
          local_parts$event_clock
      )
    
    if (any(
      weather$obs_local_date !=
      weather$context_date
    )) {
      stop(
        "A GHCNh observation falls outside its local context day."
      )
    }
    
    if (anyDuplicated(
      weather[
        c(
          "weather_series_id",
          "obs_time"
        )
      ]
    ) > 0L) {
      stop(
        "A weather series contains a duplicate observation time."
      )
    }
    
    if (any(
      weather$obs_time <
      weather$series_start_utc
    ) ||
    any(
      weather$obs_time >=
      weather$series_end_utc
    )) {
      stop(
        "A GHCNh observation falls outside its series bounds."
      )
    }
  } else {
    weather$obs_local_date <- as.Date(
      character()
    )
    
    weather$obs_local_clock <- character()
  }
  
  weather <- weather |>
    dplyr::relocate(
      weather_series_id,
      station_id,
      context_date,
      event_timezone,
      series_start_utc,
      series_end_utc,
      obs_time,
      obs_local_date,
      obs_local_clock
    ) |>
    dplyr::arrange(
      weather_series_id,
      obs_time
    )
  
  obs_summary <- weather |>
    dplyr::group_by(
      weather_series_id
    ) |>
    dplyr::summarise(
      obs_n = dplyr::n(),
      first_obs_utc = min(
        obs_time
      ),
      last_obs_utc = max(
        obs_time
      ),
      .groups = "drop"
    )
  
  series_summary <- series_key |>
    dplyr::left_join(
      obs_summary,
      by = "weather_series_id",
      relationship = "one-to-one",
      na_matches = "never"
    ) |>
    dplyr::mutate(
      obs_n = dplyr::coalesce(
        obs_n,
        0L
      ),
      series_status = dplyr::if_else(
        obs_n > 0L,
        "observed",
        "no_observations"
      )
    ) |>
    dplyr::arrange(
      weather_series_id
    )
  
  result <- context
  result$elements <- elements
  result$schema_audit <- schema_audit
  result$series_summary <- series_summary
  result$weather <- weather
  
  result
}


build_div_flight_weather <- function(weather) {
  if (!is.list(weather)) {
    stop(
      "`weather` must be a diversion weather-series list."
    )
  }
  
  required_names <- c(
    "requirements",
    "series_summary",
    "weather",
    "markers"
  )
  
  missing_names <- setdiff(
    required_names,
    names(weather)
  )
  
  if (length(missing_names) > 0L) {
    print(
      missing_names
    )
    
    stop(
      "`weather` lacks required diversion-weather objects."
    )
  }
  
  requirements <- weather$requirements
  series_summary <- weather$series_summary
  weather_tbl <- weather$weather
  
  if (!is.data.frame(requirements) ||
      nrow(requirements) == 0L) {
    stop(
      "`requirements` must be a non-empty data frame."
    )
  }
  
  if (!is.data.frame(series_summary) ||
      nrow(series_summary) == 0L) {
    stop(
      "`series_summary` must be a non-empty data frame."
    )
  }
  
  if (!is.data.frame(weather_tbl) ||
      nrow(weather_tbl) == 0L) {
    stop(
      "`weather` contains no GHCNh observations."
    )
  }
  
  req_cols <- c(
    "div_context_id",
    "div_location_id",
    "div_row_id",
    "FlightDate",
    "airport_role",
    "div_number",
    "event_airport_id",
    "event_airport_seq_id",
    "event_airport_code",
    "event_timezone",
    "operational_start_utc",
    "operational_start_status",
    "operational_end_utc",
    "operational_end_status",
    "last_actual_utc",
    "context_anchor_utc",
    "context_end_basis",
    "context_date",
    "weather_series_id",
    "station_id"
  )
  
  missing_req <- setdiff(
    req_cols,
    names(requirements)
  )
  
  if (length(missing_req) > 0L) {
    print(
      missing_req
    )
    
    stop(
      "`requirements` lacks fields for a flight-weather view."
    )
  }
  
  weather_cols <- c(
    "weather_series_id",
    "station_id",
    "event_timezone",
    "context_date",
    "obs_time",
    "obs_local_date",
    "obs_local_clock"
  )
  
  missing_weather <- setdiff(
    weather_cols,
    names(weather_tbl)
  )
  
  if (length(missing_weather) > 0L) {
    print(
      missing_weather
    )
    
    stop(
      "The canonical weather table lacks required fields."
    )
  }
  
  summary_cols <- c(
    "weather_series_id",
    "obs_n",
    "series_status"
  )
  
  missing_summary <- setdiff(
    summary_cols,
    names(series_summary)
  )
  
  if (length(missing_summary) > 0L) {
    print(
      missing_summary
    )
    
    stop(
      "`series_summary` lacks required fields."
    )
  }
  
  if (!is.integer(
    requirements$div_context_id
  ) ||
  anyNA(
    requirements$div_context_id
  ) ||
  any(
    requirements$div_context_id <= 0L
  ) ||
  anyDuplicated(
    requirements$div_context_id
  ) > 0L) {
    stop(
      "`div_context_id` must be a positive unique integer."
    )
  }
  
  if (!is.integer(
    requirements$div_location_id
  ) ||
  anyNA(
    requirements$div_location_id
  ) ||
  any(
    requirements$div_location_id <= 0L
  )) {
    stop(
      "`div_location_id` must contain positive integers."
    )
  }
  
  if (!is.integer(
    requirements$div_row_id
  ) ||
  anyNA(
    requirements$div_row_id
  ) ||
  any(
    requirements$div_row_id <= 0L
  )) {
    stop(
      "`div_row_id` must contain positive integers."
    )
  }
  
  if (!is.integer(
    requirements$weather_series_id
  ) ||
  anyNA(
    requirements$weather_series_id
  ) ||
  any(
    requirements$weather_series_id <= 0L
  )) {
    stop(
      "`weather_series_id` must contain positive integers."
    )
  }
  
  if (!inherits(
    requirements$operational_start_utc,
    "POSIXct"
  ) ||
  anyNA(
    requirements$operational_start_utc
  )) {
    stop(
      "Operational start times must be non-missing POSIXct."
    )
  }
  
  if (!inherits(
    requirements$operational_end_utc,
    "POSIXct"
  )) {
    stop(
      "`operational_end_utc` must be POSIXct."
    )
  }
  
  if (!is.character(
    requirements$operational_end_status
  ) ||
  anyNA(
    requirements$operational_end_status
  )) {
    stop(
      "`operational_end_status` must be non-missing character."
    )
  }
  
  resolved_end <-
    requirements$operational_end_status ==
    "valid"
  
  if (any(
    resolved_end &
    is.na(
      requirements$operational_end_utc
    )
  )) {
    stop(
      "A valid operational endpoint lacks its UTC instant."
    )
  }
  
  if (any(
    !resolved_end &
    !is.na(
      requirements$operational_end_utc
    )
  )) {
    stop(
      "An unresolved operational endpoint has a UTC instant."
    )
  }
  
  req_key <- requirements |>
    dplyr::count(
      div_location_id,
      context_date,
      name = "n"
    ) |>
    dplyr::filter(
      n != 1L
    )
  
  if (nrow(req_key) > 0L) {
    print(
      req_key
    )
    
    stop(
      "A flight location has a duplicate local context day."
    )
  }
  
  series_key <- weather_tbl |>
    dplyr::count(
      weather_series_id,
      obs_time,
      name = "n"
    ) |>
    dplyr::filter(
      n != 1L
    )
  
  if (nrow(series_key) > 0L) {
    print(
      series_key
    )
    
    stop(
      "Canonical weather contains a duplicate series-time key."
    )
  }
  
  req_summary <- requirements |>
    dplyr::select(
      dplyr::all_of(
        req_cols
      )
    ) |>
    dplyr::left_join(
      series_summary |>
        dplyr::select(
          dplyr::all_of(
            summary_cols
          )
        ),
      by = "weather_series_id",
      relationship = "many-to-one",
      na_matches = "never"
    )
  
  if (anyNA(
    req_summary$series_status
  )) {
    stop(
      "A flight-location day lacks its weather-series status."
    )
  }
  
  if (any(
    req_summary$obs_n < 0L
  )) {
    stop(
      "A flight-location day has an invalid observation count."
    )
  }
  
  req_view <- requirements |>
    dplyr::select(
      dplyr::all_of(
        req_cols
      )
    )
  
  flight_weather <- req_view |>
    dplyr::inner_join(
      weather_tbl,
      by = c(
        "weather_series_id",
        "station_id",
        "event_timezone",
        "context_date"
      ),
      relationship = "many-to-many",
      na_matches = "never"
    ) |>
    dplyr::mutate(
      in_operational_window =
        dplyr::case_when(
          operational_end_status == "valid" ~
            obs_time >=
            operational_start_utc &
            obs_time <=
            operational_end_utc,
          TRUE ~ NA
        ),
      window_position =
        dplyr::case_when(
          obs_time <
            operational_start_utc ~
            "pre_departure",
          operational_end_status !=
            "valid" ~
            "end_unresolved",
          obs_time <=
            operational_end_utc ~
            "operational",
          TRUE ~
            "post_arrival"
        )
    )
  
  expected_n <- sum(
    req_summary$obs_n
  )
  
  if (nrow(flight_weather) !=
      expected_n) {
    stop(
      "Flight-weather expansion changed the expected row count."
    )
  }
  
  flight_key <- flight_weather |>
    dplyr::count(
      div_context_id,
      obs_time,
      name = "n"
    ) |>
    dplyr::filter(
      n != 1L
    )
  
  if (nrow(flight_key) > 0L) {
    print(
      flight_key
    )
    
    stop(
      "The flight-weather observation key is not unique."
    )
  }
  
  resolved_row <-
    flight_weather$operational_end_status ==
    "valid"
  
  if (any(
    resolved_row &
    is.na(
      flight_weather$in_operational_window
    )
  )) {
    stop(
      "A resolved flight window has an unresolved indicator."
    )
  }
  
  if (any(
    !resolved_row &
    !is.na(
      flight_weather$in_operational_window
    )
  )) {
    stop(
      "An unresolved flight window received a window indicator."
    )
  }
  
  flight_weather <- flight_weather |>
    dplyr::arrange(
      FlightDate,
      div_row_id,
      div_location_id,
      context_date,
      obs_time
    ) |>
    dplyr::relocate(
      div_context_id,
      div_location_id,
      div_row_id,
      FlightDate,
      airport_role,
      div_number,
      event_airport_id,
      event_airport_seq_id,
      event_airport_code,
      station_id,
      weather_series_id,
      context_date,
      obs_time,
      in_operational_window,
      window_position
    )
  
  req_summary <- req_summary |>
    dplyr::arrange(
      FlightDate,
      div_row_id,
      div_location_id,
      context_date
    )
  
  result <- weather
  result$flight_summary <- req_summary
  result$flight_weather <- flight_weather
  
  result
}


# Adapt flight events to the validated diversion weather engine.
as_div_weather_events <- function(events) {
  if (!is.data.frame(events)) {
    stop(
      "`events` must be a data frame."
    )
  }
  
  if (nrow(events) == 0L) {
    stop(
      "`events` contains no flight events."
    )
  }
  
  has_flight_id <- "flight_row_id" %in%
    names(events)
  
  has_div_id <- "div_row_id" %in%
    names(events)
  
  if (has_flight_id == has_div_id) {
    stop(
      paste(
        "`events` must contain exactly one of",
        "`flight_row_id` and `div_row_id`."
      )
    )
  }
  
  adapted <- events
  
  if (has_flight_id) {
    if (!is.integer(
      adapted$flight_row_id
    ) ||
    anyNA(
      adapted$flight_row_id
    ) ||
    any(
      adapted$flight_row_id <= 0L
    )) {
      stop(
        paste(
          "`flight_row_id` must contain",
          "positive integers."
        )
      )
    }
    
    adapted <- adapted |>
      dplyr::rename(
        div_row_id = flight_row_id
      )
  }
  
  check_div_weather_events(
    adapted
  )
  
  adapted
}


# Build longitudinal weather context for generic flight events.
build_flight_weather_context <- function(
    events,
    meta_dir = file.path(
      "data_raw",
      "ghcnh_metadata"
    )) {
  
  adapted <- as_div_weather_events(
    events
  )
  
  result <- build_div_weather_context(
    events = adapted,
    meta_dir = meta_dir
  )
  
  result$locations <- result$locations |>
    dplyr::rename(
      flight_location_id =
        div_location_id,
      flight_row_id =
        div_row_id
    )
  
  result$requirements <- result$requirements |>
    dplyr::rename(
      flight_context_id =
        div_context_id,
      flight_location_id =
        div_location_id,
      flight_row_id =
        div_row_id
    )
  
  result$markers <- result$markers |>
    dplyr::rename(
      flight_location_id =
        div_location_id,
      flight_row_id =
        div_row_id
    )
  
  result
}


# Build canonical longitudinal weather for generic flight context.
build_flight_weather_series <- function(
    context,
    data_dir = file.path(
      "data_raw",
      "ghcnh"
    )) {
  
  if (!is.list(context)) {
    stop(
      "`context` must be a flight weather-context list."
    )
  }
  
  build_div_weather_series(
    context = context,
    data_dir = data_dir
  )
}


# Expand canonical longitudinal weather to generic flight locations.
build_flight_weather <- function(weather) {
  if (!is.list(weather)) {
    stop(
      "`weather` must be a flight weather-series list."
    )
  }
  
  if (!"requirements" %in%
      names(weather)) {
    stop(
      "`weather` lacks `requirements`."
    )
  }
  
  requirements <- weather$requirements
  
  if (!is.data.frame(requirements)) {
    stop(
      "`requirements` must be a data frame."
    )
  }
  
  generic_ids <- c(
    "flight_context_id",
    "flight_location_id",
    "flight_row_id"
  )
  
  missing_ids <- setdiff(
    generic_ids,
    names(requirements)
  )
  
  if (length(missing_ids) > 0L) {
    print(
      missing_ids
    )
    
    stop(
      "`requirements` lacks generic flight identities."
    )
  }
  
  div_ids <- c(
    "div_context_id",
    "div_location_id",
    "div_row_id"
  )
  
  conflicts <- intersect(
    div_ids,
    names(requirements)
  )
  
  if (length(conflicts) > 0L) {
    print(
      conflicts
    )
    
    stop(
      "`requirements` contains diversion identity fields."
    )
  }
  
  adapted <- weather
  
  adapted$requirements <- requirements |>
    dplyr::rename(
      div_context_id =
        flight_context_id,
      div_location_id =
        flight_location_id,
      div_row_id =
        flight_row_id
    )
  
  result <- build_div_flight_weather(
    adapted
  )
  
  result$requirements <- requirements
  
  result$flight_summary <- result$flight_summary |>
    dplyr::rename(
      flight_context_id =
        div_context_id,
      flight_location_id =
        div_location_id,
      flight_row_id =
        div_row_id
    )
  
  result$flight_weather <- result$flight_weather |>
    dplyr::rename(
      flight_context_id =
        div_context_id,
      flight_location_id =
        div_location_id,
      flight_row_id =
        div_row_id
    )
  
  result
}