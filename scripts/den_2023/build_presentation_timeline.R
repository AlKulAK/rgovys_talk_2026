# Build the DEN-bound diversion chronology for June 21, 2023.

source_files <- file.path(
  "scripts",
  c(
    "diversion_processing.R",
    "GHCNh_station_manifest.R",
    "GHCNh_processing.R",
    "flight_weather_linkage.R",
    "diversion_weather_linkage.R",
    "GHCNh_downloader.R"
  )
)

missing_files <- source_files[!file.exists(source_files)]
if (length(missing_files) > 0L) {
  stop("Missing R scripts: ", paste(missing_files, collapse = ", "))
}

for (path in source_files) {
  source(path)
}

if (!2023L %in% slcflights::available_years(type = "div")) {
  stop("The slcflights diversion database needs 2023 data.")
}

cache_dir <- tools::R_user_dir("rgovys_talk_2026", "cache")
meta_dir <- file.path(cache_dir, "ghcnh_metadata")
data_dir <- file.path(cache_dir, "ghcnh")
meta_file <- file.path(meta_dir, "ghcnh_metadata_manifest.csv")

if (!file.exists(meta_file)) {
  download_ghcnh_meta(meta_dir = meta_dir)
}

flight_events <- prepare_div_events(years = 2023L) |>
  dplyr::filter(
    FlightDate == as.Date("2023-06-21"),
    DestAirportID == 11292L
  )

n_flights <- dplyr::n_distinct(flight_events$div_row_id)
if (n_flights != 19L) {
  stop("Expected 19 DEN-bound flights diverted through SLC.")
}

if (anyNA(flight_events$event_utc) ||
  any(flight_events$event_time_status != "valid")) {
  stop("A selected flight event has an invalid UTC timestamp.")
}

weather_context <- build_flight_weather_context(
  flight_events,
  meta_dir = meta_dir
)

download_plan <- weather_context$download_plan
plan_by_year <- split(download_plan$station_id, download_plan$year)

noaa_files <- dplyr::bind_rows(lapply(
  names(plan_by_year),
  function(year) {
    download_ghcnh(
      station_id = plan_by_year[[year]],
      years = as.integer(year),
      data_dir = data_dir,
      progress = TRUE,
      refresh = FALSE
    )
  }
))

weather_data <- build_flight_weather_series(
  weather_context,
  data_dir = data_dir
)

station_map <- weather_data$requirements |>
  dplyr::select(
    weather_series_id,
    station_id,
    context_date,
    event_timezone,
    event_airport_id,
    event_airport_code,
    station_icao
  ) |>
  dplyr::distinct()

if (anyDuplicated(station_map$weather_series_id) > 0L) {
  stop("A weather series has multiple airport assignments.")
}

weather_obs <- weather_data$weather |>
  dplyr::inner_join(
    station_map,
    by = c(
      "weather_series_id",
      "station_id",
      "context_date",
      "event_timezone"
    ),
    relationship = "many-to-one",
    na_matches = "never"
  )

if (nrow(weather_obs) != nrow(weather_data$weather) ||
  !setequal(
    weather_obs$event_airport_id,
    flight_events$event_airport_id
  )) {
  stop("The weather records do not cover all case airports.")
}

weather_elements <- sort(setdiff(weather_data$elements, "REM"))

attr_suffix <- c(
  measurement_code = "_Measurement_Code",
  quality_code = "_Quality_Code",
  report_type = "_Report_Type",
  source_code = "_Source_Code",
  source_station_id = "_Source_Station_ID"
)

family_cols <- as.vector(t(outer(
  weather_data$elements,
  c("", unname(attr_suffix)),
  paste0
)))

if (!"REM" %in% weather_data$elements ||
  length(weather_elements) == 0L ||
  length(setdiff(family_cols, names(weather_obs))) > 0L) {
  stop("The GHCNh observation families are incomplete.")
}

weather_unit <- function(element) {
  if (element %in% c(
    "temperature", "dew_point_temperature", "wet_bulb_temperature"
  )) {
    return("C")
  }
  if (element %in% c("wind_speed", "wind_gust")) {
    return("m/s")
  }
  if (element == "wind_direction") {
    return("degrees")
  }
  if (element == "visibility") {
    return("km")
  }
  if (element == "relative_humidity") {
    return("percent")
  }
  if (element %in% c(
    "altimeter", "sea_level_pressure", "station_level_pressure",
    "pressure_3hr_change"
  )) {
    return("hPa")
  }
  if (element == "snow_depth" ||
    grepl("^precipitation($|_)", element)) {
    return("mm")
  }
  if (element %in% c("ceiling_height", "sky_condition_baseht") ||
    grepl("^sky_cover_(layer|summation)_baseht_[1-4]$", element)) {
    return("m")
  }
  NA_character_
}

weather_meaning <- function(element, value, code) {
  if (is.na(value)) {
    return(NA_character_)
  }
  if (element == "ceiling_height" &&
    as.character(value) == "22000") {
    return("unlimited_ceiling")
  }
  if (element == "wind_direction" &&
    as.character(value) == "999") {
    if (identical(as.character(code), "C")) {
      return("calm_wind")
    }
    if (identical(as.character(code), "V")) {
      return("variable_wind")
    }
    stop("Unsupported measurement code for wind direction 999.")
  }
  if (grepl("^precipitation($|_)", element) &&
    !is.na(code) && as.character(code) == "T") {
    return("trace_precipitation")
  }
  NA_character_
}

weather_family <- function(element, i) {
  value <- weather_obs[[element]][[i]]
  attrs <- lapply(attr_suffix, function(suffix) {
    weather_obs[[paste0(element, suffix)]][[i]]
  })
  names(attrs) <- names(attr_suffix)

  c(
    list(value = value, unit = weather_unit(element)),
    attrs,
    list(interpretation = weather_meaning(
      element, value, attrs$measurement_code
    ))
  )
}

format_utc <- function(x) {
  format(x, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

format_mdt <- function(x) {
  format(x, "%Y-%m-%d %H:%M:%S", tz = "America/Denver")
}

flight_record <- function(i) {
  clock <- flight_events$event_clock[[i]]

  list(
    flight = list(
      flight_date = as.character(flight_events$FlightDate[[i]]),
      airline = flight_events$Reporting_Airline[[i]],
      flight_number = flight_events$Flight_Number_Reporting_Airline[[i]],
      origin = list(
        airport_id = flight_events$OriginAirportID[[i]],
        airport_seq_id = flight_events$OriginAirportSeqID[[i]],
        code = flight_events$Origin[[i]]
      ),
      destination = list(
        airport_id = flight_events$DestAirportID[[i]],
        airport_seq_id = flight_events$DestAirportSeqID[[i]],
        code = flight_events$Dest[[i]]
      ),
      diversion_airport_landings =
        flight_events$DivAirportLandings[[i]],
      reached_destination = flight_events$DivReachedDest[[i]]
    ),
    movement = list(
      airport_role = flight_events$airport_role[[i]],
      event_type = flight_events$event_type[[i]],
      time_basis = flight_events$event_time_basis[[i]],
      source_field = flight_events$event_source_field[[i]],
      time_status = flight_events$event_time_status[[i]],
      diversion_number = flight_events$div_number[[i]],
      is_slc_diversion = flight_events$is_slc_diversion[[i]],
      airport_id = flight_events$event_airport_id[[i]],
      airport_seq_id = flight_events$event_airport_seq_id[[i]],
      airport_code = flight_events$event_airport_code[[i]],
      local_date = as.character(flight_events$event_date[[i]]),
      local_time = paste0(
        substr(clock, 1L, 2L), ":",
        substr(clock, 3L, 4L)
      ),
      local_clock_source = clock,
      local_timezone = flight_events$event_timezone[[i]]
    )
  )
}

weather_record <- function(i) {
  elements <- lapply(weather_elements, weather_family, i = i)
  names(elements) <- weather_elements

  observed_n <- sum(vapply(weather_elements, function(element) {
    !is.na(weather_obs[[element]][[i]])
  }, logical(1)))

  list(
    airport = list(
      airport_id = weather_obs$event_airport_id[[i]],
      code = weather_obs$event_airport_code[[i]]
    ),
    station = list(
      ghcnh_id = weather_obs$station_id[[i]],
      icao = weather_obs$station_icao[[i]],
      name = weather_obs$station_name[[i]],
      latitude = weather_obs$latitude[[i]],
      longitude = weather_obs$longitude[[i]],
      elevation = weather_obs$elevation[[i]]
    ),
    observation = list(
      weather_series_id = weather_obs$weather_series_id[[i]],
      context_date = as.character(weather_obs$context_date[[i]]),
      local_date = as.character(weather_obs$obs_local_date[[i]]),
      local_clock = weather_obs$obs_local_clock[[i]],
      local_timezone = weather_obs$event_timezone[[i]],
      ghcnh_raw_time = weather_obs$raw_time[[i]],
      aviation_class = weather_obs$aviation_class[[i]],
      ghcnh_year = weather_obs$year[[i]],
      ghcnh_month = weather_obs$month[[i]],
      ghcnh_day = weather_obs$day[[i]],
      ghcnh_hour = weather_obs$hour[[i]],
      ghcnh_minute = weather_obs$minute[[i]],
      element_count = length(weather_elements),
      reported_element_count = observed_n
    ),
    elements = elements,
    remarks = weather_family("REM", i)
  )
}

chronology <- dplyr::bind_rows(
  tibble::tibble(
    timestamp_utc = flight_events$event_utc,
    record_type = "flight_event",
    source_row = seq_len(nrow(flight_events)),
    record_order = 1L,
    airport = flight_events$event_airport_code
  ),
  tibble::tibble(
    timestamp_utc = weather_obs$obs_time,
    record_type = "weather_observation",
    source_row = seq_len(nrow(weather_obs)),
    record_order = 2L,
    airport = weather_obs$event_airport_code
  )
) |>
  dplyr::arrange(
    timestamp_utc,
    record_order,
    airport,
    source_row
  )

records <- lapply(seq_len(nrow(chronology)), function(i) {
  row_id <- chronology$source_row[[i]]
  record_type <- chronology$record_type[[i]]

  details <- if (record_type == "flight_event") {
    flight_record(row_id)
  } else {
    weather_record(row_id)
  }

  c(
    list(
      record_type = record_type,
      timestamp_utc = format_utc(chronology$timestamp_utc[[i]]),
      timestamp_mdt = format_mdt(chronology$timestamp_utc[[i]])
    ),
    details
  )
})

noaa_sources <- lapply(seq_len(nrow(noaa_files)), function(i) {
  list(
    station_id = noaa_files$station_id[[i]],
    year = noaa_files$year[[i]],
    url = noaa_files$url[[i]],
    size_bytes = noaa_files$size_bytes[[i]],
    md5 = noaa_files$md5[[i]]
  )
})

script_md5 <- unname(tools::md5sum(source_files))
script_sources <- lapply(seq_along(source_files), function(i) {
  list(path = source_files[[i]], md5 = script_md5[[i]])
})

output <- list(
  metadata = list(
    title = "DEN-bound flights diverted through SLC: complete chronology",
    case_date_denver = "2023-06-21",
    ordering = "timestamp_utc",
    display_timezone = "America/Denver",
    weather_timestamp_basis = "GHCNh observation time",
    flight_count = n_flights,
    flight_event_count = nrow(flight_events),
    actual_flight_event_count = sum(
      flight_events$event_time_basis == "actual"
    ),
    scheduled_flight_event_count = sum(
      flight_events$event_time_basis == "scheduled"
    ),
    weather_observation_count = nrow(weather_obs),
    ghcnh_element_count = length(weather_elements),
    airport_count = length(unique(c(
      flight_events$event_airport_id,
      weather_obs$event_airport_id
    ))),
    record_count = nrow(chronology),
    source = list(
      bts = list(
        dataset = "BTS diversion-only On-Time Performance",
        package = "slcflights",
        package_version = as.character(
          utils::packageVersion("slcflights")
        ),
        package_url = "https://github.com/AlKulAK/slcflights"
      ),
      ghcnh = list(
        provider = "NOAA National Centers for Environmental Information",
        metadata_manifest_md5 = unname(tools::md5sum(meta_file)),
        station_year_files = noaa_sources
      ),
      r_source_files = script_sources
    )
  ),
  events = records
)

output_path <- file.path(
  "scripts",
  "den_2023",
  "den_2023_full_chronology.json"
)

dir.create(
  dirname(output_path),
  recursive = TRUE,
  showWarnings = FALSE
)

jsonlite::write_json(
  output,
  path = output_path,
  pretty = TRUE,
  auto_unbox = TRUE,
  na = "null",
  null = "null",
  digits = NA
)

message(
  "Wrote ",
  output_path,
  " (",
  nrow(chronology),
  " entries)."
)
