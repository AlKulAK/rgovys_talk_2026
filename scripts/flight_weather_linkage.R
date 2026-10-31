# Link prepared flight events to canonical weather observations.

# Resolve GHCNh years required around UTC event times.
resolve_ghcnh_years <- function(
  times,
  margin_min = 0
) {
  if (!inherits(times, "POSIXct")) {
    stop("`times` must be POSIXct.")
  }

  if (!is.numeric(margin_min) ||
    length(margin_min) != 1L ||
    is.na(margin_min) ||
    !is.finite(margin_min) ||
    margin_min < 0) {
    stop(
      "`margin_min` must be one non-negative numeric value."
    )
  }

  times <- times[
    !is.na(times)
  ]

  if (length(times) == 0L) {
    stop("No resolved UTC times are available.")
  }

  margin_sec <- 60 * margin_min

  bound_times <- c(
    times - margin_sec,
    times + margin_sec
  )

  years <- as.integer(
    format(
      bound_times,
      "%Y",
      tz = "UTC"
    )
  )

  if (anyNA(years)) {
    stop("At least one GHCNh year could not be resolved.")
  }

  sort(
    unique(years)
  )
}

match_weather_times <- function(
  flights,
  weather_index,
  max_dist_min = 60
) {
  if (!is.data.frame(flights)) {
    stop("`flights` must be a data frame.")
  }

  if (!is.data.frame(weather_index)) {
    stop("`weather_index` must be a data frame.")
  }

  if (!is.numeric(max_dist_min) ||
    length(max_dist_min) != 1L ||
    is.na(max_dist_min) ||
    !is.finite(max_dist_min) ||
    max_dist_min <= 0) {
    stop("`max_dist_min` must be one positive finite value.")
  }

  if (nrow(flights) == 0L) {
    stop("`flights` contains no flight events.")
  }

  event_cols <- c(
    "sched_event_utc",
    "event_time_status"
  )

  dep_cols <- c(
    "sched_dep_utc",
    "sched_dst_status"
  )

  has_event_time <- all(
    event_cols %in% names(flights)
  )

  has_dep_time <- all(
    dep_cols %in% names(flights)
  )

  if (!has_event_time && !has_dep_time) {
    stop("`flights` lacks required scheduled-time fields.")
  }

  if (has_event_time) {
    sched_utc <- flights$sched_event_utc
    time_status <- flights$event_time_status
  } else {
    sched_utc <- flights$sched_dep_utc
    time_status <- flights$sched_dst_status
  }

  weather_cols <- c(
    "station_id",
    "raw_time",
    "obs_time",
    "aviation_class"
  )

  missing_weather <- setdiff(
    weather_cols,
    names(weather_index)
  )

  if (length(missing_weather) > 0L) {
    stop("`weather_index` lacks required index fields.")
  }

  if (nrow(weather_index) == 0L) {
    stop("`weather_index` contains no observations.")
  }

  if (!inherits(sched_utc, "POSIXct")) {
    stop("The scheduled event UTC field must be POSIXct.")
  }

  if (!is.character(time_status) ||
    anyNA(time_status)) {
    stop("The scheduled event status must be non-missing character.")
  }

  if (!inherits(
    weather_index$obs_time,
    "POSIXct"
  )) {
    stop("`obs_time` must be POSIXct.")
  }

  if (anyNA(weather_index$obs_time)) {
    stop("`weather_index` contains missing observation times.")
  }

  if (anyDuplicated(weather_index$obs_time) > 0L) {
    stop("`weather_index` contains duplicate observation times.")
  }

  if (anyNA(weather_index$station_id) ||
    length(
      unique(
        weather_index$station_id
      )
    ) != 1L) {
    stop("`weather_index` must contain exactly one station.")
  }

  valid_class <- c(
    "direct",
    "merged_only",
    "direct_and_merged"
  )

  if (anyNA(weather_index$aviation_class) ||
    any(
      !weather_index$aviation_class %in%
        valid_class
    )) {
    stop("`weather_index` contains an invalid aviation class.")
  }

  valid_status <- c(
    "valid",
    "ambiguous",
    "nonexistent",
    "schedule_unresolved"
  )

  if (any(!time_status %in% valid_status)) {
    stop("The scheduled event status contains an unknown value.")
  }

  valid_time <- time_status == "valid"

  if (any(
    valid_time &
      is.na(sched_utc)
  )) {
    stop("A valid scheduled event lacks a UTC instant.")
  }

  if (any(
    !valid_time &
      !is.na(sched_utc)
  )) {
    stop("An unresolved scheduled event has a UTC instant.")
  }

  weather <- weather_index |>
    dplyr::arrange(obs_time)

  weather_sec <- as.numeric(
    weather$obs_time
  )

  event_sec <- as.numeric(
    sched_utc
  )

  n_events <- nrow(flights)

  prior_idx <- rep(
    NA_integer_,
    n_events
  )

  valid_id <- which(
    valid_time
  )

  prior_idx[valid_id] <- findInterval(
    event_sec[valid_id],
    weather_sec
  )

  next_idx <- rep(
    NA_integer_,
    n_events
  )

  next_idx[valid_id] <- prior_idx[
    valid_id
  ] + 1L

  prior_sec <- rep(
    NA_real_,
    n_events
  )

  next_sec <- rep(
    NA_real_,
    n_events
  )

  has_prior <- !is.na(prior_idx) &
    prior_idx > 0L

  has_next <- !is.na(next_idx) &
    next_idx <= length(weather_sec)

  prior_sec[has_prior] <- weather_sec[
    prior_idx[has_prior]
  ]

  next_sec[has_next] <- weather_sec[
    next_idx[has_next]
  ]

  prior_dist <- (
    event_sec - prior_sec
  ) / 60

  next_dist <- (
    next_sec - event_sec
  ) / 60

  use_prior <- valid_time &
    !is.na(prior_dist) &
    (
      is.na(next_dist) |
        prior_dist <= next_dist
    )

  use_next <- valid_time &
    !use_prior &
    !is.na(next_dist)

  nearest_idx <- rep(
    NA_integer_,
    n_events
  )

  nearest_idx[use_prior] <- prior_idx[
    use_prior
  ]

  nearest_idx[use_next] <- next_idx[
    use_next
  ]

  nearest_dist <- rep(
    NA_real_,
    n_events
  )

  nearest_dist[use_prior] <- prior_dist[
    use_prior
  ]

  nearest_dist[use_next] <- next_dist[
    use_next
  ]

  nearest_side <- rep(
    NA_character_,
    n_events
  )

  nearest_side[use_prior] <- "prior"
  nearest_side[use_next] <- "next"

  match_ok <- valid_time &
    !is.na(nearest_dist) &
    nearest_dist <= max_dist_min

  match_status <- dplyr::case_when(
    !valid_time ~ "time_unresolved",
    match_ok ~ "matched",
    TRUE ~ "coverage_gap"
  )

  match_side <- rep(
    NA_character_,
    n_events
  )

  match_side[match_ok] <- nearest_side[
    match_ok
  ]

  obs_sec <- rep(
    NA_real_,
    n_events
  )

  obs_sec[match_ok] <- weather_sec[
    nearest_idx[match_ok]
  ]

  weather_obs_time <- as.POSIXct(
    obs_sec,
    origin = "1970-01-01",
    tz = "UTC"
  )

  weather_meta <- weather |>
    dplyr::transmute(
      weather_station_id = station_id,
      weather_raw_time = raw_time,
      weather_obs_time = obs_time,
      weather_aviation_class =
        aviation_class
    )

  result <- flights |>
    dplyr::mutate(
      weather_obs_time = weather_obs_time,
      weather_dist_min = nearest_dist,
      weather_match_side = match_side,
      weather_match_status = match_status
    ) |>
    dplyr::left_join(
      weather_meta,
      by = "weather_obs_time",
      relationship = "many-to-one",
      na_matches = "never"
    )

  bad_match <- (
    result$weather_match_status == "matched" &
      is.na(result$weather_obs_time)
  ) |
    (
      result$weather_match_status != "matched" &
        !is.na(result$weather_obs_time)
    )

  if (any(bad_match)) {
    stop("Weather match status disagrees with observation time.")
  }

  if (nrow(result) != nrow(flights)) {
    stop("Weather matching changed the number of event rows.")
  }

  result
}


attach_weather_values <- function(
  flights,
  station_id,
  elements,
  data_dir = file.path("data_raw", "ghcnh")
) {
  if (!is.data.frame(flights)) {
    stop("`flights` must be a data frame.")
  }

  if (!is.character(station_id) ||
    length(station_id) != 1L ||
    is.na(station_id) ||
    !nzchar(station_id)) {
    stop("`station_id` must be one non-empty character value.")
  }

  if (!is.character(elements) ||
    length(elements) == 0L ||
    anyNA(elements) ||
    any(!nzchar(elements))) {
    stop("`elements` must be a non-empty character vector.")
  }

  required_cols <- c(
    "weather_obs_time",
    "weather_match_status",
    "weather_station_id"
  )

  missing_cols <- setdiff(
    required_cols,
    names(flights)
  )

  if (length(missing_cols) > 0L) {
    stop("`flights` lacks required weather-match fields.")
  }

  matched <- flights$weather_match_status == "matched"

  if (any(
    matched &
      is.na(flights$weather_obs_time)
  )) {
    stop("A matched flight lacks `weather_obs_time`.")
  }

  bad_station <- matched &
    (
      is.na(flights$weather_station_id) |
        flights$weather_station_id != station_id
    )

  if (any(bad_station)) {
    stop("A matched flight uses a different weather station.")
  }

  elements <- unique(elements)

  matched_times <- flights$weather_obs_time[
    matched
  ]

  if (length(matched_times) == 0L) {
    stop("No matched weather observations are available.")
  }

  weather_years <- resolve_ghcnh_years(
    times = matched_times
  )

  avail_elements <- list_ghcnh_elements(
    station_id = station_id,
    data_dir = data_dir,
    years = weather_years
  )

  unknown <- setdiff(
    elements,
    avail_elements
  )

  if (length(unknown) > 0L) {
    print(unknown)
    stop("At least one requested GHCNh element is unavailable.")
  }

  meta_suffix <- c(
    "_Measurement_Code",
    "_Quality_Code",
    "_Report_Type",
    "_Source_Code",
    "_Source_Station_ID"
  )

  family_cols <- as.vector(
    t(
      outer(
        elements,
        c("", meta_suffix),
        paste0
      )
    )
  )

  weather <- build_ghcnh_weather(
    station_id = station_id,
    elements = elements,
    data_dir = data_dir,
    years = weather_years
  )

  payload <- weather |>
    dplyr::select(
      weather_obs_time = obs_time,
      dplyr::all_of(family_cols)
    )

  if (anyDuplicated(payload$weather_obs_time) > 0L) {
    stop("The weather payload has duplicate observation times.")
  }

  result <- flights |>
    dplyr::left_join(
      payload,
      by = "weather_obs_time",
      relationship = "many-to-one",
      na_matches = "never"
    )

  if (nrow(result) != nrow(flights)) {
    stop("Weather attachment changed the number of flight rows.")
  }

  result
}

assign_other_stations <- function(
  events,
  manifest
) {
  if (!is.data.frame(events)) {
    stop("`events` must be a data frame.")
  }

  if (!is.data.frame(manifest)) {
    stop("`manifest` must be a data frame.")
  }

  event_cols <- c(
    "other_airport_id",
    "other_sched_date"
  )

  missing_event <- setdiff(
    event_cols,
    names(events)
  )

  if (length(missing_event) > 0L) {
    stop("`events` lacks required other-endpoint fields.")
  }

  manifest_cols <- c(
    "manifest_id",
    "AirportID",
    "station_id",
    "station_name",
    "station_icao",
    "map_start",
    "map_end",
    "selection_basis",
    "selection_note"
  )

  missing_manifest <- setdiff(
    manifest_cols,
    names(manifest)
  )

  if (length(missing_manifest) > 0L) {
    stop("`manifest` lacks required station-mapping fields.")
  }

  if (!inherits(manifest$map_start, "Date") ||
    !inherits(manifest$map_end, "Date")) {
    stop("Manifest mapping bounds must be Date.")
  }

  if (anyNA(manifest$manifest_id) ||
    anyNA(manifest$AirportID) ||
    anyNA(manifest$station_id) ||
    anyNA(manifest$map_start) ||
    anyNA(manifest$map_end)) {
    stop("The station manifest contains missing mapping fields.")
  }

  if (anyDuplicated(
    manifest$manifest_id
  ) > 0L) {
    stop("The station manifest contains duplicate manifest IDs.")
  }

  if (any(
    manifest$map_start >
      manifest$map_end
  )) {
    stop("A station mapping has invalid date bounds.")
  }

  unknown_airports <- setdiff(
    unique(events$other_airport_id),
    unique(manifest$AirportID)
  )

  if (length(unknown_airports) > 0L) {
    print(unknown_airports)

    stop(
      "At least one other airport is absent from the manifest."
    )
  }

  map_tbl <- manifest |>
    dplyr::arrange(
      AirportID,
      map_start,
      map_end
    ) |>
    dplyr::group_by(
      AirportID
    ) |>
    dplyr::mutate(
      assign_start = dplyr::if_else(
        dplyr::row_number() == 1L,
        as.Date("1900-01-01"),
        map_start
      ),
      assign_end = dplyr::if_else(
        dplyr::row_number() == dplyr::n(),
        as.Date("2100-12-31"),
        map_end
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::transmute(
      other_airport_id = AirportID,
      other_manifest_id = manifest_id,
      other_station_id = station_id,
      other_station_name = station_name,
      other_station_icao = station_icao,
      other_map_start = map_start,
      other_map_end = map_end,
      other_assign_start = assign_start,
      other_assign_end = assign_end,
      other_selection_basis = selection_basis,
      other_selection_note = selection_note
    )

  overlap <- map_tbl |>
    dplyr::group_by(
      other_airport_id
    ) |>
    dplyr::arrange(
      other_assign_start,
      .by_group = TRUE
    ) |>
    dplyr::mutate(
      prior_end = dplyr::lag(
        other_assign_end
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(
      !is.na(prior_end),
      other_assign_start <= prior_end
    )

  if (nrow(overlap) > 0L) {
    stop(
      "At least one endpoint assignment epoch overlaps."
    )
  }

  result <- events |>
    dplyr::left_join(
      map_tbl,
      by = dplyr::join_by(
        other_airport_id,
        other_sched_date >= other_assign_start,
        other_sched_date <= other_assign_end
      ),
      relationship = "many-to-one",
      na_matches = "never"
    )

  known_date <- !is.na(
    result$other_sched_date
  )

  missing_map <- known_date &
    is.na(result$other_station_id)

  if (any(missing_map)) {
    print(
      result |>
        dplyr::filter(
          missing_map
        ) |>
        dplyr::distinct(
          other_airport_id,
          other_airport_code,
          other_sched_date
        ) |>
        dplyr::arrange(
          other_airport_id,
          other_sched_date
        )
    )

    stop(
      "At least one dated endpoint lacks a station mapping."
    )
  }

  unexpected_map <- !known_date &
    !is.na(result$other_station_id)

  if (any(unexpected_map)) {
    stop(
      "An undated endpoint received a station mapping."
    )
  }

  result <- result |>
    dplyr::mutate(
      other_map_status = dplyr::if_else(
        known_date,
        "mapped",
        "date_unresolved"
      )
    ) |>
    dplyr::select(
      -other_assign_start,
      -other_assign_end
    ) |>
    dplyr::relocate(
      other_map_status,
      other_manifest_id,
      other_station_id,
      other_station_name,
      other_station_icao,
      other_map_start,
      other_map_end,
      other_selection_basis,
      other_selection_note,
      .after = other_time_reason
    )

  if (nrow(result) != nrow(events)) {
    stop("Station assignment changed the number of event rows.")
  }

  result
}

match_station_weather <- function(
  events,
  row_ids,
  station_id,
  data_dir,
  max_dist_min
) {
  station_times <- events$other_sched_utc[
    row_ids
  ]

  weather_years <- resolve_ghcnh_years(
    times = station_times,
    margin_min = max_dist_min
  )

  weather_index <- build_ghcnh_index(
    station_id = station_id,
    data_dir = data_dir,
    years = weather_years
  )

  match_input <- tibble::tibble(
    network_row_id = row_ids,
    sched_event_utc =
      events$other_sched_utc[row_ids],
    event_time_status =
      events$other_time_status[row_ids]
  )

  matched <- match_weather_times(
    flights = match_input,
    weather_index = weather_index,
    max_dist_min = max_dist_min
  )

  matched |>
    dplyr::transmute(
      network_row_id,
      other_weather_station_id =
        weather_station_id,
      other_weather_raw_time =
        weather_raw_time,
      other_weather_obs_time =
        weather_obs_time,
      other_weather_dist_min =
        weather_dist_min,
      other_weather_match_side =
        weather_match_side,
      other_weather_match_status =
        weather_match_status,
      other_weather_aviation_class =
        weather_aviation_class
    )
}


match_other_weather <- function(
  events,
  data_dir = file.path("data_raw", "ghcnh"),
  max_dist_min = 60
) {
  if (!is.data.frame(events)) {
    stop("`events` must be a data frame.")
  }

  if (nrow(events) == 0L) {
    stop("`events` contains no flight events.")
  }

  if (!is.character(data_dir) ||
    length(data_dir) != 1L ||
    is.na(data_dir) ||
    !nzchar(data_dir)) {
    stop("`data_dir` must be one non-empty character value.")
  }

  if (!is.numeric(max_dist_min) ||
    length(max_dist_min) != 1L ||
    is.na(max_dist_min) ||
    !is.finite(max_dist_min) ||
    max_dist_min <= 0) {
    stop("`max_dist_min` must be one positive finite value.")
  }

  required_cols <- c(
    "other_station_id",
    "other_sched_utc",
    "other_time_status",
    "other_map_status"
  )

  missing_cols <- setdiff(
    required_cols,
    names(events)
  )

  if (length(missing_cols) > 0L) {
    stop("`events` lacks required network-linkage fields.")
  }

  if ("network_row_id" %in% names(events)) {
    stop("`events` already contains `network_row_id`.")
  }

  output_cols <- c(
    "other_weather_station_id",
    "other_weather_raw_time",
    "other_weather_obs_time",
    "other_weather_dist_min",
    "other_weather_match_side",
    "other_weather_match_status",
    "other_weather_aviation_class"
  )

  if (any(output_cols %in% names(events))) {
    stop("`events` already contains other-weather match fields.")
  }

  if (!inherits(
    events$other_sched_utc,
    "POSIXct"
  )) {
    stop("`other_sched_utc` must be POSIXct.")
  }

  if (!is.character(events$other_time_status) ||
    anyNA(events$other_time_status)) {
    stop("`other_time_status` must be non-missing character.")
  }

  valid_time_status <- c(
    "valid",
    "schedule_unresolved"
  )

  if (any(
    !events$other_time_status %in%
      valid_time_status
  )) {
    stop("`other_time_status` contains an unknown value.")
  }

  if (!is.character(events$other_map_status) ||
    anyNA(events$other_map_status)) {
    stop("`other_map_status` must be non-missing character.")
  }

  valid_map_status <- c(
    "mapped",
    "date_unresolved"
  )

  if (any(
    !events$other_map_status %in%
      valid_map_status
  )) {
    stop("`other_map_status` contains an unknown value.")
  }

  if (!is.character(events$other_station_id)) {
    stop("`other_station_id` must be character.")
  }

  has_station <- !is.na(
    events$other_station_id
  )

  if (any(
    has_station &
      !nzchar(events$other_station_id)
  )) {
    stop("A mapped station ID is empty.")
  }

  valid_time <- events$other_time_status == "valid"

  if (any(
    valid_time &
      is.na(events$other_sched_utc)
  )) {
    stop("A valid endpoint lacks a UTC instant.")
  }

  if (any(
    !valid_time &
      !is.na(events$other_sched_utc)
  )) {
    stop("An unresolved endpoint has a UTC instant.")
  }

  mapped <- events$other_map_status == "mapped"

  if (any(
    mapped &
      !has_station
  )) {
    stop("A mapped endpoint lacks a station ID.")
  }

  if (any(
    !mapped &
      has_station
  )) {
    stop("An unresolved mapping has a station ID.")
  }

  if (any(
    valid_time &
      !mapped
  )) {
    stop("A valid endpoint lacks a station mapping.")
  }

  mapped_id <- which(
    has_station
  )

  if (length(mapped_id) == 0L) {
    stop("No endpoint has a station assignment.")
  }

  row_groups <- split(
    mapped_id,
    events$other_station_id[mapped_id]
  )

  match_list <- lapply(
    names(row_groups),
    function(station_id) {
      match_station_weather(
        events = events,
        row_ids = row_groups[[station_id]],
        station_id = station_id,
        data_dir = data_dir,
        max_dist_min = max_dist_min
      )
    }
  )

  match_tbl <- dplyr::bind_rows(
    match_list
  )

  if (nrow(match_tbl) != length(mapped_id)) {
    stop("Station matching changed the mapped event count.")
  }

  if (anyDuplicated(
    match_tbl$network_row_id
  ) > 0L) {
    stop("Station matching duplicated an event row.")
  }

  result <- events |>
    dplyr::mutate(
      network_row_id = dplyr::row_number()
    ) |>
    dplyr::left_join(
      match_tbl,
      by = "network_row_id",
      relationship = "one-to-one",
      na_matches = "never"
    )

  if (!identical(
    result$network_row_id,
    seq_len(nrow(events))
  )) {
    stop("Network weather matching changed event order.")
  }

  mapped_missing <- !is.na(
    result$other_station_id
  ) &
    is.na(
      result$other_weather_match_status
    )

  if (any(mapped_missing)) {
    stop("A mapped endpoint lacks a weather-match status.")
  }

  result <- result |>
    dplyr::mutate(
      other_weather_match_status =
        dplyr::if_else(
          is.na(other_station_id),
          "time_unresolved",
          other_weather_match_status
        )
    )

  matched <- result$other_weather_match_status ==
    "matched"

  bad_station <- matched &
    (
      is.na(result$other_weather_station_id) |
        result$other_weather_station_id !=
          result$other_station_id
    )

  if (any(bad_station)) {
    stop("A weather match uses the wrong target station.")
  }

  bad_obs <- (
    matched &
      is.na(result$other_weather_obs_time)
  ) |
    (
      !matched &
        !is.na(result$other_weather_obs_time)
    )

  if (any(bad_obs)) {
    stop("Weather match status disagrees with observation time.")
  }

  result <- result |>
    dplyr::select(
      -network_row_id
    )

  if (nrow(result) != nrow(events)) {
    stop("Network weather matching changed the event count.")
  }

  result
}

build_station_payload <- function(
  events,
  station_id,
  elements,
  data_dir
) {
  station_times <- events |>
    dplyr::filter(
      other_weather_match_status == "matched",
      other_weather_station_id == station_id
    ) |>
    dplyr::distinct(
      other_weather_obs_time
    ) |>
    dplyr::arrange(
      other_weather_obs_time
    )

  if (nrow(station_times) == 0L) {
    stop("The station has no matched observation times.")
  }

  attach_input <- station_times |>
    dplyr::transmute(
      weather_obs_time = other_weather_obs_time,
      weather_match_status = "matched",
      weather_station_id = station_id
    )

  attached <- attach_weather_values(
    flights = attach_input,
    station_id = station_id,
    elements = elements,
    data_dir = data_dir
  )

  meta_suffix <- c(
    "_Measurement_Code",
    "_Quality_Code",
    "_Report_Type",
    "_Source_Code",
    "_Source_Station_ID"
  )

  family_cols <- as.vector(
    t(
      outer(
        elements,
        c("", meta_suffix),
        paste0
      )
    )
  )

  payload <- attached |>
    dplyr::select(
      weather_obs_time,
      dplyr::all_of(
        family_cols
      )
    )

  names(payload)[1L] <-
    "other_weather_obs_time"

  names(payload)[-1L] <- paste0(
    "other_weather_",
    family_cols
  )

  payload |>
    dplyr::mutate(
      other_weather_station_id = station_id
    ) |>
    dplyr::relocate(
      other_weather_station_id,
      .before = other_weather_obs_time
    )
}


attach_other_weather <- function(
  events,
  elements,
  data_dir = file.path("data_raw", "ghcnh")
) {
  if (!is.data.frame(events)) {
    stop("`events` must be a data frame.")
  }

  if (nrow(events) == 0L) {
    stop("`events` contains no flight events.")
  }

  if (!is.character(elements) ||
    length(elements) == 0L ||
    anyNA(elements) ||
    any(!nzchar(elements))) {
    stop("`elements` must be a non-empty character vector.")
  }

  if (!is.character(data_dir) ||
    length(data_dir) != 1L ||
    is.na(data_dir) ||
    !nzchar(data_dir)) {
    stop("`data_dir` must be one non-empty character value.")
  }

  required_cols <- c(
    "other_station_id",
    "other_weather_station_id",
    "other_weather_obs_time",
    "other_weather_match_status"
  )

  missing_cols <- setdiff(
    required_cols,
    names(events)
  )

  if (length(missing_cols) > 0L) {
    stop("`events` lacks required weather-match fields.")
  }

  if ("network_row_id" %in% names(events)) {
    stop("`events` already contains `network_row_id`.")
  }

  if (!is.character(
    events$other_station_id
  )) {
    stop("`other_station_id` must be character.")
  }

  if (!is.character(
    events$other_weather_station_id
  )) {
    stop("`other_weather_station_id` must be character.")
  }

  if (!inherits(
    events$other_weather_obs_time,
    "POSIXct"
  )) {
    stop("`other_weather_obs_time` must be POSIXct.")
  }

  status <- events$other_weather_match_status

  valid_status <- c(
    "matched",
    "coverage_gap",
    "time_unresolved"
  )

  if (!is.character(status) ||
    anyNA(status) ||
    any(!status %in% valid_status)) {
    stop("`other_weather_match_status` has an unknown value.")
  }

  matched <- status == "matched"

  if (any(
    matched &
      is.na(events$other_weather_obs_time)
  )) {
    stop("A matched endpoint lacks a weather observation time.")
  }

  if (any(
    !matched &
      !is.na(events$other_weather_obs_time)
  )) {
    stop("An unmatched endpoint has a weather observation time.")
  }

  bad_station <- matched &
    (
      is.na(events$other_weather_station_id) |
        events$other_weather_station_id !=
          events$other_station_id
    )

  if (any(bad_station)) {
    stop("A matched endpoint uses the wrong weather station.")
  }

  elements <- unique(
    elements
  )

  meta_suffix <- c(
    "_Measurement_Code",
    "_Quality_Code",
    "_Report_Type",
    "_Source_Code",
    "_Source_Station_ID"
  )

  family_cols <- as.vector(
    t(
      outer(
        elements,
        c("", meta_suffix),
        paste0
      )
    )
  )

  output_cols <- paste0(
    "other_weather_",
    family_cols
  )

  if (any(
    output_cols %in% names(events)
  )) {
    stop("`events` already contains requested weather fields.")
  }

  station_ids <- sort(
    unique(
      events$other_weather_station_id[
        matched
      ]
    )
  )

  if (length(station_ids) == 0L) {
    stop("No matched weather stations are available.")
  }

  payload_list <- lapply(
    station_ids,
    function(station_id) {
      build_station_payload(
        events = events,
        station_id = station_id,
        elements = elements,
        data_dir = data_dir
      )
    }
  )

  payload_tbl <- dplyr::bind_rows(
    payload_list
  ) |>
    dplyr::mutate(
      payload_present = TRUE
    )

  result <- events |>
    dplyr::mutate(
      network_row_id = dplyr::row_number()
    ) |>
    dplyr::left_join(
      payload_tbl,
      by = c(
        "other_weather_station_id",
        "other_weather_obs_time"
      ),
      relationship = "many-to-one",
      na_matches = "never"
    )

  if (!identical(
    result$network_row_id,
    seq_len(nrow(events))
  )) {
    stop("Weather attachment changed event order.")
  }

  missing_payload <- matched &
    is.na(result$payload_present)

  if (any(missing_payload)) {
    stop("A matched endpoint lacks its weather payload.")
  }

  unexpected_payload <- !matched &
    !is.na(result$payload_present)

  if (any(unexpected_payload)) {
    stop("An unmatched endpoint received a weather payload.")
  }

  result <- result |>
    dplyr::select(
      -network_row_id,
      -payload_present
    )

  if (nrow(result) != nrow(events)) {
    stop("Weather attachment changed the event count.")
  }

  result
}
