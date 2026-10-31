# Deployment data preparator for SLC Flight Network Explorer
# 09.27.2026 version
#
# Necessary runtime packages:
# dplyr, slcflights
#
# This script is intended to be run locally, not deployed to shinyapps.io.
# It uses the slcflights package to (re)generate the .rds files consumed by the
# app.

get_slc_airport_id <- function() {
  coords <- slcflights::read_coords()

  slc_ids <- coords |>
    dplyr::filter(AIRPORT == "SLC") |>
    dplyr::distinct(AIRPORT_ID) |>
    dplyr::pull(AIRPORT_ID)

  if (length(slc_ids) != 1L) {
    stop(
      "Expected exactly one AIRPORT_ID for SLC in read_coords().",
      call. = FALSE
    )
  }

  slc_ids
}

get_slc_point <- function(slc_airport_id) {
  coords <- slcflights::read_coords() |>
    dplyr::filter(
      AIRPORT_ID == slc_airport_id,
      AIRPORT == "SLC"
    ) |>
    dplyr::arrange(
      AIRPORT_START_DATE,
      AIRPORT_SEQ_ID
    ) |>
    dplyr::slice_tail(n = 1) |>
    dplyr::transmute(
      AirportCode = AIRPORT,
      AirportName = DISPLAY_AIRPORT_NAME,
      AirportCity = DISPLAY_AIRPORT_CITY_NAME_FULL,
      Latitude = LATITUDE,
      Longitude = LONGITUDE,
      Hover = paste0(
        "<b>", DISPLAY_AIRPORT_NAME, "</b><br>",
        AIRPORT,
        "<br>",
        DISPLAY_AIRPORT_CITY_NAME_FULL
      )
    )

  if (nrow(coords) != 1L) {
    stop(
      "Expected exactly one latest coordinate record for SLC.",
      call. = FALSE
    )
  }

  coords
}

parse_bts_date <- function(x) {
  if (inherits(x, "Date")) {
    return(x)
  }

  as.Date(
    strptime(
      x,
      format = "%m/%d/%Y %I:%M:%S %p",
      tz = "UTC"
    )
  )
}

get_full_coords_path <- function() {
  test_root <- Sys.getenv("SLCFLIGHTS_TEST_CACHE_ROOT", unset = "")

  cache_root <- if (nzchar(test_root)) {
    path.expand(test_root)
  } else {
    tools::R_user_dir("slcflights", which = "cache")
  }

  path <- file.path(
    cache_root,
    "raw",
    "bts_coords",
    "T_MASTER_CORD.csv"
  )

  if (!file.exists(path)) {
    stop(
      paste(
        "Full BTS Master Coordinate CSV was not found in the",
        "slcflights raw cache."
      ),
      call. = FALSE
    )
  }

  path
}

make_service_month <- function(year, month) {
  as.Date(sprintf("%04d-%02d-01", year, month))
}

build_slc_network_data <- function(slc_airport_id) {
  airport_names <- slcflights::read_coords() |>
    dplyr::transmute(
      AirportID = AIRPORT_ID,
      AirportSeqID = AIRPORT_SEQ_ID,
      AirportName = DISPLAY_AIRPORT_NAME
    )

  raw <- slcflights::open_main() |>
    dplyr::select(
      Year,
      Month,
      FlightDate,
      DOT_ID_Reporting_Airline,
      Reporting_Airline,
      Reporting_Airline_Lookup_Code,
      Reporting_Airline_Name,
      OriginAirportID,
      OriginAirportSeqID,
      Origin,
      OriginCityName,
      OriginLatitude,
      OriginLongitude,
      OriginAirportStartDate,
      OriginAirportThruDate,
      OriginAirportIsClosed,
      OriginAirportIsLatest,
      DestAirportID,
      DestAirportSeqID,
      Dest,
      DestCityName,
      DestLatitude,
      DestLongitude,
      DestAirportStartDate,
      DestAirportThruDate,
      DestAirportIsClosed,
      DestAirportIsLatest
    ) |>
    dplyr::filter(
      OriginAirportID == slc_airport_id |
        DestAirportID == slc_airport_id
    ) |>
    dplyr::collect() |>
    dplyr::mutate(
      FlightDate = as.Date(FlightDate)
    )

  if (anyNA(raw$FlightDate)) {
    stop(
      "FlightDate contains missing or unparseable values.",
      call. = FALSE
    )
  }

  date_year <- as.integer(format(raw$FlightDate, "%Y"))
  date_month <- as.integer(format(raw$FlightDate, "%m"))

  if (any(date_year != raw$Year | date_month != raw$Month)) {
    stop(
      "FlightDate is inconsistent with Year or Month.",
      call. = FALSE
    )
  }

  raw |>
    dplyr::mutate(
      Direction = dplyr::if_else(
        OriginAirportID == slc_airport_id,
        "from_slc",
        "to_slc"
      ),
      AirportID = dplyr::if_else(
        OriginAirportID == slc_airport_id,
        DestAirportID,
        OriginAirportID
      ),
      AirportSeqID = dplyr::if_else(
        OriginAirportID == slc_airport_id,
        DestAirportSeqID,
        OriginAirportSeqID
      ),
      AirportCode = dplyr::if_else(
        OriginAirportID == slc_airport_id,
        Dest,
        Origin
      ),
      AirportCity = dplyr::if_else(
        OriginAirportID == slc_airport_id,
        DestCityName,
        OriginCityName
      ),
      Latitude = dplyr::if_else(
        OriginAirportID == slc_airport_id,
        DestLatitude,
        OriginLatitude
      ),
      Longitude = dplyr::if_else(
        OriginAirportID == slc_airport_id,
        DestLongitude,
        OriginLongitude
      ),
      AirportStartDate = dplyr::if_else(
        OriginAirportID == slc_airport_id,
        DestAirportStartDate,
        OriginAirportStartDate
      ),
      AirportThruDate = dplyr::if_else(
        OriginAirportID == slc_airport_id,
        DestAirportThruDate,
        OriginAirportThruDate
      ),
      AirportIsClosed = dplyr::if_else(
        OriginAirportID == slc_airport_id,
        DestAirportIsClosed,
        OriginAirportIsClosed
      ),
      AirportIsLatest = dplyr::if_else(
        OriginAirportID == slc_airport_id,
        DestAirportIsLatest,
        OriginAirportIsLatest
      ),
      ServiceMonth = make_service_month(Year, Month)
    ) |>
    dplyr::left_join(
      airport_names,
      by = c("AirportID", "AirportSeqID")
    ) |>
    dplyr::filter(
      AirportCode != "SLC",
      !is.na(AirportID),
      !is.na(AirportSeqID),
      !is.na(AirportCode),
      !is.na(AirportName),
      !is.na(AirportCity),
      !is.na(Reporting_Airline),
      !is.na(Reporting_Airline_Name),
      !is.na(Direction),
      !is.na(ServiceMonth),
      !is.na(Latitude),
      !is.na(Longitude),
      Longitude >= -180,
      Longitude <= 180,
      Latitude >= -90,
      Latitude <= 90
    ) |>
    dplyr::group_by(
      Year,
      Month,
      ServiceMonth,
      Direction,
      DOT_ID_Reporting_Airline,
      Reporting_Airline,
      Reporting_Airline_Lookup_Code,
      Reporting_Airline_Name,
      AirportID,
      AirportSeqID,
      AirportCode,
      AirportName,
      AirportCity,
      Latitude,
      Longitude,
      AirportStartDate,
      AirportThruDate,
      AirportIsClosed,
      AirportIsLatest
    ) |>
    dplyr::summarize(
      FirstFlightDate = min(FlightDate),
      LastFlightDate = max(FlightDate),
      ServiceDates = list(sort(unique(FlightDate))),
      Flights = dplyr::n(),
      .groups = "drop"
    )
}

build_current_airports <- function(network_data) {
  network_ids <- network_data |>
    dplyr::distinct(AirportID)

  full_coords <- utils::read.csv(
    get_full_coords_path(),
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = c("", "NA")
  )

  required_cols <- c(
    "AIRPORT_ID",
    "AIRPORT_SEQ_ID",
    "AIRPORT",
    "DISPLAY_AIRPORT_NAME",
    "AIRPORT_START_DATE",
    "AIRPORT_IS_CLOSED",
    "AIRPORT_IS_LATEST",
    "AIRPORT_THRU_DATE"
  )

  missing_cols <- setdiff(required_cols, names(full_coords))

  if (length(missing_cols) > 0L) {
    stop(
      paste0(
        "Full BTS Master Coordinate CSV is missing required columns: ",
        paste(missing_cols, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }

  coords <- full_coords |>
    dplyr::transmute(
      AIRPORT_ID = as.integer(AIRPORT_ID),
      AIRPORT_SEQ_ID = as.numeric(AIRPORT_SEQ_ID),
      AIRPORT = as.character(AIRPORT),
      DISPLAY_AIRPORT_NAME = as.character(DISPLAY_AIRPORT_NAME),
      AIRPORT_START_DATE = parse_bts_date(AIRPORT_START_DATE),
      AIRPORT_IS_CLOSED = as.integer(AIRPORT_IS_CLOSED),
      AIRPORT_IS_LATEST = as.integer(AIRPORT_IS_LATEST),
      AIRPORT_THRU_DATE = parse_bts_date(AIRPORT_THRU_DATE)
    ) |>
    dplyr::semi_join(
      network_ids,
      by = c("AIRPORT_ID" = "AirportID")
    )

  current <- coords |>
    dplyr::filter(AIRPORT_IS_LATEST == 1L) |>
    dplyr::distinct(
      AIRPORT_ID,
      AIRPORT_SEQ_ID,
      AIRPORT,
      DISPLAY_AIRPORT_NAME,
      AIRPORT_IS_CLOSED,
      AIRPORT_THRU_DATE
    )

  duplicate_ids <- current |>
    dplyr::count(
      AIRPORT_ID,
      name = "n_current"
    ) |>
    dplyr::filter(n_current > 1L)

  if (nrow(duplicate_ids) > 0L) {
    stop(
      paste(
        "Current airport metadata has multiple latest rows",
        "for an AIRPORT_ID."
      ),
      call. = FALSE
    )
  }

  missing_ids <- network_ids |>
    dplyr::anti_join(
      current,
      by = c("AirportID" = "AIRPORT_ID")
    )

  if (nrow(missing_ids) > 0L) {
    stop(
      paste(
        "Full BTS Master Coordinate metadata is missing a latest row",
        "for an AIRPORT_ID."
      ),
      call. = FALSE
    )
  }

  if (
    anyNA(current$DISPLAY_AIRPORT_NAME) ||
      anyNA(current$AIRPORT) ||
      anyNA(current$AIRPORT_IS_CLOSED)
  ) {
    stop(
      "Current airport name, code, or closure status is missing.",
      call. = FALSE
    )
  }

  name_episodes <- coords |>
    dplyr::arrange(
      AIRPORT_ID,
      AIRPORT_SEQ_ID,
      AIRPORT_START_DATE
    ) |>
    dplyr::group_by(AIRPORT_ID) |>
    dplyr::mutate(
      new_name_episode =
        dplyr::row_number() == 1L |
          dplyr::coalesce(
            DISPLAY_AIRPORT_NAME !=
              dplyr::lag(DISPLAY_AIRPORT_NAME),
            TRUE
          ),
      name_episode = cumsum(new_name_episode)
    ) |>
    dplyr::ungroup()

  current_name_episode_start <- name_episodes |>
    dplyr::group_by(AIRPORT_ID) |>
    dplyr::filter(name_episode == max(name_episode)) |>
    dplyr::summarize(
      CurrentNameEpisodeStartDate = if (
        all(is.na(AIRPORT_START_DATE))
      ) {
        as.Date(NA)
      } else {
        min(AIRPORT_START_DATE, na.rm = TRUE)
      },
      .groups = "drop"
    ) |>
    dplyr::rename(AirportID = AIRPORT_ID)

  last_slc_service <- network_data |>
    dplyr::group_by(AirportID) |>
    dplyr::summarize(
      LastSLCServiceDate = max(LastFlightDate),
      .groups = "drop"
    )

  service_names <- network_data |>
    dplyr::distinct(
      AirportID,
      AirportName
    ) |>
    dplyr::mutate(
      CurrentNameInSLCService = TRUE
    )

  current_prepared <- current |>
    dplyr::transmute(
      AirportID = AIRPORT_ID,
      CurrentAirportName = DISPLAY_AIRPORT_NAME,
      CurrentAirportCode = AIRPORT,
      CurrentAirportIsClosed = AIRPORT_IS_CLOSED,
      CurrentAirportThruDate = AIRPORT_THRU_DATE
    ) |>
    dplyr::left_join(
      current_name_episode_start,
      by = "AirportID"
    ) |>
    dplyr::left_join(
      last_slc_service,
      by = "AirportID"
    ) |>
    dplyr::left_join(
      service_names,
      by = c(
        "AirportID",
        "CurrentAirportName" = "AirportName"
      )
    ) |>
    dplyr::mutate(
      CurrentNameInSLCService = dplyr::coalesce(
        CurrentNameInSLCService,
        FALSE
      )
    )

  unresolved_name_changes <- current_prepared |>
    dplyr::filter(
      !CurrentNameInSLCService &
        (
          is.na(CurrentNameEpisodeStartDate) |
            is.na(LastSLCServiceDate) |
            CurrentNameEpisodeStartDate <= LastSLCServiceDate
        )
    )

  if (nrow(unresolved_name_changes) > 0L) {
    stop(
      paste(
        "A current airport name is absent from SLC-service history",
        "but cannot be classified as a post-service name change."
      ),
      call. = FALSE
    )
  }

  current_prepared <- current_prepared |>
    dplyr::mutate(
      CurrentNameChangedAfterLastSLCService =
        !CurrentNameInSLCService &
          CurrentNameEpisodeStartDate > LastSLCServiceDate
    )

  duplicate_open_codes <- current_prepared |>
    dplyr::filter(CurrentAirportIsClosed == 0L) |>
    dplyr::count(
      CurrentAirportCode,
      name = "n_open_airports"
    ) |>
    dplyr::filter(n_open_airports > 1L)

  if (nrow(duplicate_open_codes) > 0L) {
    stop(
      paste(
        "A current airport code is associated with multiple",
        "open AIRPORT_ID values."
      ),
      call. = FALSE
    )
  }

  open_code_lookup <- current_prepared |>
    dplyr::filter(CurrentAirportIsClosed == 0L) |>
    dplyr::transmute(
      CurrentAirportCode,
      CurrentCodeReplacementAirportName = CurrentAirportName
    )

  closed_replacements <- current_prepared |>
    dplyr::filter(CurrentAirportIsClosed == 1L) |>
    dplyr::select(
      AirportID,
      CurrentAirportCode
    ) |>
    dplyr::left_join(
      open_code_lookup,
      by = "CurrentAirportCode"
    ) |>
    dplyr::select(
      AirportID,
      CurrentCodeReplacementAirportName
    )

  current_prepared |>
    dplyr::left_join(
      closed_replacements,
      by = "AirportID"
    ) |>
    dplyr::transmute(
      AirportID,
      CurrentAirportName,
      CurrentAirportCode,
      CurrentAirportIsClosed,
      CurrentAirportThruDate,
      CurrentNameChangedAfterLastSLCService,
      CurrentCodeReplacementAirportName
    ) |>
    dplyr::arrange(AirportID)
}

app_data_dir <- "../slc-network-app/data"

dir.create(
  app_data_dir,
  showWarnings = FALSE,
  recursive = TRUE
)

slc_airport_id <- get_slc_airport_id()
slc_point <- get_slc_point(slc_airport_id)
all_data <- build_slc_network_data(slc_airport_id)
current_airports <- build_current_airports(all_data)

saveRDS(
  all_data,
  file = file.path(app_data_dir, "all_data.rds")
)

saveRDS(
  slc_point,
  file = file.path(app_data_dir, "slc_point.rds")
)

saveRDS(
  current_airports,
  file = file.path(app_data_dir, "current_airports.rds")
)

message("Prepared SLC app data.")
message("Rows in all_data: ", nrow(all_data))
message("Rows in current_airports: ", nrow(current_airports))
message(
  "Date range: ",
  format(min(all_data$ServiceMonth), "%B %Y"),
  " - ",
  format(max(all_data$ServiceMonth), "%B %Y")
)
