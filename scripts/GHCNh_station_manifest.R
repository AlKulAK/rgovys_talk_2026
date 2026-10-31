# Build the SLC-network GHCNh station manifest.
# 2026-08-29

make_service_month <- function(year, month) {
  as.Date(
    sprintf(
      "%04d-%02d-01",
      year,
      month
    )
  )
}


get_month_end <- function(x) {
  next_month <- as.Date(
    format(
      x + 32,
      "%Y-%m-01"
    )
  )
  
  next_month - 1L
}

get_slc_id <- function() {
  coords <- slcflights::read_coords()
  
  slc_ids <- coords |>
    dplyr::filter(
      AIRPORT == "SLC"
    ) |>
    dplyr::distinct(
      AIRPORT_ID
    ) |>
    dplyr::pull(
      AIRPORT_ID
    )
  
  if (length(slc_ids) != 1L) {
    stop(
      "Expected exactly one BTS AirportID for SLC."
    )
  }
  
  slc_ids[[1]]
}

build_service_tbl <- function(
    years = slcflights::available_years(
      type = "main"
    )) {
  
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
    type = "main"
  )
  
  missing_years <- setdiff(
    years,
    avail_years
  )
  
  if (length(missing_years) > 0L) {
    stop(
      "At least one requested BTS year is unavailable."
    )
  }
  
  slc_id <- get_slc_id()
  
  dep_service <- slcflights::open_main(
    years
  ) |>
    dplyr::filter(
      OriginAirportID == slc_id
    ) |>
    dplyr::select(
      Year,
      Month,
      AirportID = DestAirportID,
      AirportSeqID = DestAirportSeqID,
      AirportCode = Dest,
      AirportCity = DestCityName,
      Latitude = DestLatitude,
      Longitude = DestLongitude
    ) |>
    dplyr::group_by(
      Year,
      Month,
      AirportID,
      AirportSeqID,
      AirportCode,
      AirportCity,
      Latitude,
      Longitude
    ) |>
    dplyr::summarise(
      Flights = dplyr::n()
    ) |>
    dplyr::collect() |>
    dplyr::ungroup()
  
  arr_service <- slcflights::open_main(
    years
  ) |>
    dplyr::filter(
      DestAirportID == slc_id
    ) |>
    dplyr::select(
      Year,
      Month,
      AirportID = OriginAirportID,
      AirportSeqID = OriginAirportSeqID,
      AirportCode = Origin,
      AirportCity = OriginCityName,
      Latitude = OriginLatitude,
      Longitude = OriginLongitude
    ) |>
    dplyr::group_by(
      Year,
      Month,
      AirportID,
      AirportSeqID,
      AirportCode,
      AirportCity,
      Latitude,
      Longitude
    ) |>
    dplyr::summarise(
      Flights = dplyr::n()
    ) |>
    dplyr::collect() |>
    dplyr::ungroup()
  
  service_tbl <- dplyr::bind_rows(
    dep_service,
    arr_service
  ) |>
    dplyr::group_by(
      Year,
      Month,
      AirportID,
      AirportSeqID,
      AirportCode,
      AirportCity,
      Latitude,
      Longitude
    ) |>
    dplyr::summarise(
      Flights = sum(Flights),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      ServiceMonth = make_service_month(
        Year,
        Month
      )
    )
  
  if (anyNA(service_tbl$AirportID) ||
      anyNA(service_tbl$AirportSeqID) ||
      anyNA(service_tbl$ServiceMonth) ||
      anyNA(service_tbl$Latitude) ||
      anyNA(service_tbl$Longitude)) {
    stop(
      "An SLC-network service record is incomplete."
    )
  }
  
  if (nrow(service_tbl) == 0L) {
    stop(
      "No SLC-network service records were found."
    )
  }
  
  service_tbl
}

normalize_req_bts_id <- function(x, arg) {
  if (!is.numeric(x) &&
      !is.character(x)) {
    stop(
      paste0(
        "`",
        arg,
        "` must be numeric or character."
      )
    )
  }
  
  if (is.numeric(x)) {
    if (anyNA(x) ||
        any(!is.finite(x)) ||
        any(x != floor(x)) ||
        any(x <= 0) ||
        any(x > .Machine$integer.max)) {
      stop(
        paste0(
          "`",
          arg,
          "` contains an invalid BTS identifier."
        )
      )
    }
    
    return(
      as.integer(x)
    )
  }
  
  if (anyNA(x) ||
      any(!nzchar(x)) ||
      any(!grepl(
        "^[0-9]+$",
        x
      ))) {
    stop(
      paste0(
        "`",
        arg,
        "` contains an invalid BTS identifier."
      )
    )
  }
  
  value <- as.numeric(x)
  
  if (anyNA(value) ||
      any(!is.finite(value)) ||
      any(value <= 0) ||
      any(value > .Machine$integer.max)) {
    stop(
      paste0(
        "`",
        arg,
        "` contains an invalid BTS identifier."
      )
    )
  }
  
  as.integer(value)
}


build_req_service_tbl <- function(req_tbl) {
  if (!is.data.frame(req_tbl) ||
      nrow(req_tbl) == 0L) {
    stop(
      "`req_tbl` must be a non-empty data frame."
    )
  }
  
  required_cols <- c(
    "event_date",
    "AirportID",
    "AirportSeqID",
    "AirportCode"
  )
  
  missing_cols <- setdiff(
    required_cols,
    names(req_tbl)
  )
  
  if (length(missing_cols) > 0L) {
    print(missing_cols)
    
    stop(
      "`req_tbl` lacks required airport fields."
    )
  }
  
  if (!inherits(
    req_tbl$event_date,
    "Date"
  ) ||
  anyNA(req_tbl$event_date)) {
    stop(
      "`event_date` must be non-missing Date."
    )
  }
  
  if (!is.character(req_tbl$AirportCode) ||
      anyNA(req_tbl$AirportCode) ||
      any(!nzchar(req_tbl$AirportCode))) {
    stop(
      "`AirportCode` must be non-missing character."
    )
  }
  
  req_tbl <- req_tbl |>
    dplyr::transmute(
      event_date,
      AirportID = normalize_req_bts_id(
        AirportID,
        "AirportID"
      ),
      AirportSeqID = normalize_req_bts_id(
        AirportSeqID,
        "AirportSeqID"
      ),
      AirportCode
    )
  
  coords <- slcflights::read_coords()
  
  coord_cols <- c(
    "AIRPORT_SEQ_ID",
    "AIRPORT_ID",
    "AIRPORT",
    "DISPLAY_AIRPORT_CITY_NAME_FULL",
    "LATITUDE",
    "LONGITUDE"
  )
  
  missing_coord <- setdiff(
    coord_cols,
    names(coords)
  )
  
  if (length(missing_coord) > 0L) {
    print(missing_coord)
    
    stop(
      "The BTS coordinate table lacks required fields."
    )
  }
  
  coords <- coords |>
    dplyr::filter(
      AIRPORT_SEQ_ID %in%
        req_tbl$AirportSeqID
    )
  
  if (nrow(coords) == 0L) {
    stop(
      "No required AirportSeqID occurs in BTS coordinates."
    )
  }
  
  if (anyNA(coords$AIRPORT_ID) ||
      anyNA(coords$AIRPORT_SEQ_ID) ||
      any(!is.finite(coords$AIRPORT_ID)) ||
      any(!is.finite(coords$AIRPORT_SEQ_ID)) ||
      any(
        coords$AIRPORT_ID !=
        floor(coords$AIRPORT_ID)
      ) ||
      any(
        coords$AIRPORT_SEQ_ID !=
        floor(coords$AIRPORT_SEQ_ID)
      )) {
    stop(
      "A required BTS coordinate identifier is invalid."
    )
  }
  
  coords <- coords |>
    dplyr::transmute(
      AirportID = as.integer(
        AIRPORT_ID
      ),
      AirportSeqID = as.integer(
        AIRPORT_SEQ_ID
      ),
      coord_code = AIRPORT,
      AirportCity =
        DISPLAY_AIRPORT_CITY_NAME_FULL,
      Latitude = LATITUDE,
      Longitude = LONGITUDE
    )
  
  coord_key <- c(
    "AirportID",
    "AirportSeqID"
  )
  
  if (anyDuplicated(
    coords[coord_key]
  ) > 0L) {
    stop(
      "A required BTS airport key has duplicate coordinates."
    )
  }
  
  req_key <- req_tbl |>
    dplyr::distinct(
      AirportID,
      AirportSeqID
    )
  
  missing_key <- req_key |>
    dplyr::anti_join(
      coords,
      by = coord_key
    )
  
  if (nrow(missing_key) > 0L) {
    print(missing_key)
    
    stop(
      "At least one required BTS airport lacks coordinates."
    )
  }
  
  result <- req_tbl |>
    dplyr::left_join(
      coords,
      by = coord_key,
      relationship = "many-to-one"
    )
  
  bad_code <- result$AirportCode !=
    result$coord_code
  
  if (anyNA(bad_code) ||
      any(bad_code)) {
    print(
      result |>
        dplyr::filter(
          is.na(bad_code) |
            bad_code
        ) |>
        dplyr::distinct(
          AirportID,
          AirportSeqID,
          AirportCode,
          coord_code
        )
    )
    
    stop(
      "A required airport code disagrees with BTS coordinates."
    )
  }
  
  if (anyNA(result$Latitude) ||
      anyNA(result$Longitude)) {
    stop(
      "At least one required airport lacks coordinates."
    )
  }
  
  result |>
    dplyr::mutate(
      Year = as.integer(
        format(
          event_date,
          "%Y"
        )
      ),
      Month = as.integer(
        format(
          event_date,
          "%m"
        )
      )
    ) |>
    dplyr::group_by(
      Year,
      Month,
      AirportID,
      AirportSeqID,
      AirportCode,
      AirportCity,
      Latitude,
      Longitude
    ) |>
    dplyr::summarise(
      Flights = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      ServiceMonth = make_service_month(
        Year,
        Month
      )
    ) |>
    dplyr::select(
      Year,
      Month,
      AirportID,
      AirportSeqID,
      AirportCode,
      AirportCity,
      Latitude,
      Longitude,
      Flights,
      ServiceMonth
    ) |>
    dplyr::arrange(
      Year,
      Month,
      AirportID,
      AirportSeqID
    )
}

# Treat NCEI metadata as externally sourced raw inputs.
get_ghcnh_meta_spec <- function() {
  base_url <- paste0(
    "https://",
    "www.ncei.noaa.gov/oa/",
    "global-historical-climatology-network/",
    "hourly/doc/"
  )
  
  file_name <- c(
    "ghcnh-station-list.csv",
    "ghcnh-inventory.txt"
  )
  
  tibble::tibble(
    metadata_type = c(
      "station_list",
      "inventory"
    ),
    file_name = file_name,
    source_url = paste0(
      base_url,
      file_name
    )
  )
}

read_station_meta <- function(path) {
  expected_names <- c(
    "GHCN_ID",
    "LATITUDE",
    "LONGITUDE",
    "ELEVATION",
    "STATE",
    "NAME",
    "GSN",
    "(US)HCN_(US)CRN",
    "WMO_ID",
    "ICAO",
    "ISO_CODE"
  )
  
  station_tbl <- readr::read_csv(
    path,
    col_types = readr::cols(
      GHCN_ID = readr::col_character(),
      LATITUDE = readr::col_double(),
      LONGITUDE = readr::col_double(),
      ELEVATION = readr::col_double(),
      STATE = readr::col_character(),
      NAME = readr::col_character(),
      GSN = readr::col_character(),
      `(US)HCN_(US)CRN` =
        readr::col_character(),
      WMO_ID = readr::col_character(),
      ICAO = readr::col_character(),
      ISO_CODE = readr::col_character()
    ),
    na = c(
      "",
      "-999.9",
      "-999",
      "-999.0"
    ),
    trim_ws = TRUE,
    show_col_types = FALSE,
    progress = FALSE
  )
  
  if (!identical(
    names(station_tbl),
    expected_names
  )) {
    print(
      names(station_tbl)
    )
    
    stop(
      "The GHCNh station list has an unexpected schema."
    )
  }
  
  station_tbl <- station_tbl |>
    dplyr::rename(
      HCN_CRN = `(US)HCN_(US)CRN`
    )
  
  if (anyNA(station_tbl$GHCN_ID)) {
    stop(
      "The GHCNh station list contains a missing station ID."
    )
  }
  
  if (anyDuplicated(
    station_tbl$GHCN_ID
  ) > 0L) {
    stop(
      "The GHCNh station list contains duplicate station IDs."
    )
  }
  
  if (anyNA(station_tbl$ISO_CODE) ||
      any(!grepl(
        "^[A-Z]{2}$",
        station_tbl$ISO_CODE
      ))) {
    stop(
      "The GHCNh station list contains an invalid ISO code."
    )
  }
  
  has_icao <- !is.na(
    station_tbl$ICAO
  )
  
  if (any(
    has_icao &
    !grepl(
      "^[A-Z0-9]{4}$",
      station_tbl$ICAO
    )
  )) {
    stop(
      "The GHCNh station list contains an invalid ICAO code."
    )
  }
  
  station_tbl
}

read_inventory_meta <- function(path) {
  inv_raw <- readr::read_fwf(
    path,
    col_types = readr::cols(
      .default = readr::col_character()
    ),
    trim_ws = TRUE,
    progress = FALSE
  )
  
  expected_names <- c(
    "GHCNh_ID",
    "YEAR",
    toupper(month.abb)
  )
  
  if (ncol(inv_raw) != length(expected_names)) {
    stop(
      "The GHCNh inventory has an unexpected column count."
    )
  }
  
  header <- as.character(
    unlist(
      inv_raw[1L, ],
      use.names = FALSE
    )
  )
  
  if (!identical(
    header,
    expected_names
  )) {
    print(header)
    
    stop(
      "The GHCNh inventory header is unexpected."
    )
  }
  
  names(inv_raw) <- header
  
  inv_tbl <- inv_raw |>
    dplyr::slice(
      -1L
    ) |>
    dplyr::mutate(
      YEAR = as.integer(YEAR),
      dplyr::across(
        JAN:DEC,
        as.integer
      )
    )
  
  month_names <- toupper(
    month.abb
  )
  
  if (anyNA(inv_tbl$GHCNh_ID) ||
      anyNA(inv_tbl$YEAR) ||
      anyNA(inv_tbl[month_names])) {
    stop(
      "The GHCNh inventory contains missing required values."
    )
  }
  
  if (anyDuplicated(
    inv_tbl[
      c(
        "GHCNh_ID",
        "YEAR"
      )
    ]
  ) > 0L) {
    stop(
      "The GHCNh inventory contains duplicate station-years."
    )
  }
  
  if (any(
    as.matrix(
      inv_tbl[month_names]
    ) < 0L
  )) {
    stop(
      "The GHCNh inventory contains a negative monthly count."
    )
  }
  
  inv_tbl
}

# Verify cached metadata against recorded provenance before use.
read_meta_cache <- function(meta_dir) {
  spec <- get_ghcnh_meta_spec()
  
  manifest_path <- file.path(
    meta_dir,
    "ghcnh_metadata_manifest.csv"
  )
  
  if (!file.exists(manifest_path)) {
    stop(
      "The GHCNh metadata cache manifest does not exist."
    )
  }
  
  cache_manifest <- readr::read_csv(
    manifest_path,
    col_types = readr::cols(
      metadata_type = readr::col_character(),
      file_name = readr::col_character(),
      source_url = readr::col_character(),
      retrieved_utc = readr::col_character(),
      size_bytes = readr::col_double(),
      md5 = readr::col_character()
    ),
    show_col_types = FALSE
  )
  
  key_cols <- c(
    "metadata_type",
    "file_name",
    "source_url"
  )
  
  missing_meta <- spec |>
    dplyr::anti_join(
      cache_manifest,
      by = key_cols
    )
  
  extra_meta <- cache_manifest |>
    dplyr::anti_join(
      spec,
      by = key_cols
    )
  
  if (nrow(missing_meta) > 0L ||
      nrow(extra_meta) > 0L) {
    stop(
      "The GHCNh metadata cache manifest is inconsistent."
    )
  }
  
  cache_manifest <- spec |>
    dplyr::left_join(
      cache_manifest,
      by = key_cols,
      relationship = "one-to-one"
    ) |>
    dplyr::mutate(
      path = file.path(
        meta_dir,
        file_name
      )
    )
  
  if (any(!file.exists(
    cache_manifest$path
  ))) {
    stop(
      "At least one cached GHCNh metadata file is missing."
    )
  }
  
  cache_size <- file.info(
    cache_manifest$path
  )$size
  
  cache_md5 <- unname(
    tools::md5sum(
      cache_manifest$path
    )
  )
  
  if (any(cache_size != cache_manifest$size_bytes) ||
      any(cache_md5 != cache_manifest$md5)) {
    stop(
      paste(
        "Cached GHCNh metadata differ from their",
        "recorded provenance. Refresh the metadata."
      )
    )
  }
  
  cache_manifest
}

# Validate both downloads before replacing the local metadata cache.
download_ghcnh_meta <- function(
    meta_dir = file.path(
      "data_raw",
      "ghcnh_metadata"
    )
) {
  if (!dir.exists(meta_dir)) {
    dir.create(
      meta_dir,
      recursive = TRUE
    )
  }
  
  if (!dir.exists(meta_dir)) {
    stop(
      "The GHCNh metadata cache directory could not be created."
    )
  }
  
  spec <- get_ghcnh_meta_spec()
  
  old_manifest_path <- file.path(
    meta_dir,
    "ghcnh_metadata_manifest.csv"
  )
  
  old_file_names <- character()
  
  if (file.exists(old_manifest_path)) {
    old_manifest <- readr::read_csv(
      old_manifest_path,
      col_types = readr::cols(
        .default = readr::col_character()
      ),
      show_col_types = FALSE
    )
    
    if (!"file_name" %in%
        names(old_manifest) ||
        anyNA(old_manifest$file_name) ||
        any(!nzchar(
          old_manifest$file_name
        )) ||
        any(
          basename(
            old_manifest$file_name
          ) != old_manifest$file_name
        )) {
      stop(
        "The existing metadata cache manifest is invalid."
      )
    }
    
    old_file_names <- unique(
      old_manifest$file_name
    )
  }
  
  stage_dir <- tempfile(
    pattern = "ghcnh_meta_",
    tmpdir = meta_dir
  )
  
  dir.create(
    stage_dir
  )
  
  on.exit(
    unlink(
      stage_dir,
      recursive = TRUE
    ),
    add = TRUE
  )
  
  stage_paths <- file.path(
    stage_dir,
    spec$file_name
  )
  
  result <- curl::multi_download(
    urls = spec$source_url,
    destfiles = stage_paths,
    resume = FALSE,
    progress = TRUE,
    multi_timeout = Inf
  )
  
  if (nrow(result) != nrow(spec)) {
    stop(
      paste(
        "GHCNh metadata download results do not match",
        "the request count."
      )
    )
  }
  
  if (anyNA(result$success) ||
      any(!result$success)) {
    print(
      result
    )
    
    stop(
      "At least one GHCNh metadata download failed."
    )
  }
  
  good_status <- result$status_code >= 200L &
    result$status_code < 300L
  
  if (anyNA(good_status) ||
      any(!good_status)) {
    print(
      result
    )
    
    stop(
      paste(
        "At least one GHCNh metadata download returned",
        "a non-success HTTP status."
      )
    )
  }
  
  if (any(!file.exists(stage_paths))) {
    stop(
      "At least one downloaded GHCNh metadata file is missing."
    )
  }
  
  stage_size <- file.info(
    stage_paths
  )$size
  
  if (any(is.na(stage_size)) ||
      any(stage_size <= 0)) {
    stop(
      "At least one downloaded GHCNh metadata file is empty."
    )
  }
  
  station_path <- stage_paths[
    spec$metadata_type == "station_list"
  ]
  
  inventory_path <- stage_paths[
    spec$metadata_type == "inventory"
  ]
  
  read_station_meta(
    station_path
  )
  
  read_inventory_meta(
    inventory_path
  )
  
  retrieved_utc <- format(
    Sys.time(),
    "%Y-%m-%dT%H:%M:%SZ",
    tz = "UTC"
  )
  
  cache_manifest <- spec |>
    dplyr::mutate(
      retrieved_utc = retrieved_utc,
      size_bytes = stage_size,
      md5 = unname(
        tools::md5sum(
          stage_paths
        )
      )
    )
  
  stage_manifest <- file.path(
    stage_dir,
    "ghcnh_metadata_manifest.csv"
  )
  
  readr::write_csv(
    cache_manifest,
    stage_manifest
  )
  
  target_paths <- file.path(
    meta_dir,
    spec$file_name
  )
  
  target_manifest <- file.path(
    meta_dir,
    "ghcnh_metadata_manifest.csv"
  )
  
  copied <- file.copy(
    from = c(
      stage_paths,
      stage_manifest
    ),
    to = c(
      target_paths,
      target_manifest
    ),
    overwrite = TRUE
  )
  
  if (any(!copied)) {
    stop(
      "The downloaded GHCNh metadata could not be cached."
    )
  }
  
  cache_result <- read_meta_cache(
    meta_dir
  )
  
  stale_names <- setdiff(
    old_file_names,
    spec$file_name
  )
  
  if (length(stale_names) > 0L) {
    stale_paths <- file.path(
      meta_dir,
      stale_names
    )
    
    stale_paths <- stale_paths[
      file.exists(stale_paths)
    ]
    
    if (length(stale_paths) > 0L) {
      unlink(
        stale_paths
      )
    }
    
    if (any(file.exists(
      stale_paths
    ))) {
      stop(
        paste(
          "Obsolete GHCNh metadata files",
          "could not be removed."
        )
      )
    }
  }
  
  cache_result
}

read_ghcnh_meta <- function(
    meta_dir = file.path(
      "data_raw",
      "ghcnh_metadata"
    )
) {
  provenance <- read_meta_cache(
    meta_dir = meta_dir
  )
  
  station_path <- provenance |>
    dplyr::filter(
      metadata_type == "station_list"
    ) |>
    dplyr::pull(path)
  
  inventory_path <- provenance |>
    dplyr::filter(
      metadata_type == "inventory"
    ) |>
    dplyr::pull(path)
  
  station_tbl <- read_station_meta(
    station_path
  ) |>
    dplyr::filter(
      grepl(
        "^USW",
        GHCN_ID
      ),
      !is.na(LATITUDE),
      !is.na(LONGITUDE)
    )
  
  inv_tbl <- read_inventory_meta(
    inventory_path
  ) |>
    dplyr::filter(
      grepl(
        "^USW",
        GHCNh_ID
      )
    )
  
  if (anyDuplicated(
    station_tbl$GHCN_ID
  ) > 0L) {
    stop(
      "The filtered GHCNh station list has duplicate IDs."
    )
  }
  
  list(
    stations = station_tbl,
    inventory = inv_tbl,
    provenance = provenance
  )
}

build_airport_tbl <- function(service_tbl) {
  service_bounds <- service_tbl |>
    dplyr::group_by(
      AirportID
    ) |>
    dplyr::summarise(
      service_start = min(ServiceMonth),
      service_end = get_month_end(
        max(ServiceMonth)
      ),
      .groups = "drop"
    )
  
  airport_tbl <- service_tbl |>
    dplyr::arrange(
      AirportID,
      ServiceMonth,
      AirportSeqID
    ) |>
    dplyr::group_by(
      AirportID
    ) |>
    dplyr::slice_tail(
      n = 1L
    ) |>
    dplyr::ungroup() |>
    dplyr::select(
      AirportID,
      AirportCode,
      AirportCity,
      Latitude,
      Longitude
    ) |>
    dplyr::left_join(
      service_bounds,
      by = "AirportID",
      relationship = "one-to-one"
    )
  
  if (nrow(airport_tbl) !=
      dplyr::n_distinct(
        service_tbl$AirportID
      ) ||
      !setequal(
        airport_tbl$AirportID,
        service_tbl$AirportID
      )) {
    stop(
      paste(
        "The airport table does not contain",
        "exactly one row per service AirportID."
      )
    )
  }
  
  airport_tbl
}


find_nearest_station <- function(
    airport_tbl,
    station_tbl
) {
  if (!isTRUE(sf::sf_use_s2())) {
    stop(
      "`sf` must use s2 for geographic distances."
    )
  }
  
  airport_sf <- sf::st_as_sf(
    airport_tbl,
    coords = c(
      "Longitude",
      "Latitude"
    ),
    crs = 4326,
    remove = FALSE
  )
  
  station_sf <- sf::st_as_sf(
    station_tbl,
    coords = c(
      "LONGITUDE",
      "LATITUDE"
    ),
    crs = 4326,
    remove = FALSE
  )
  
  dist_mat <- sf::st_distance(
    airport_sf,
    station_sf
  )
  
  dist_num <- matrix(
    as.numeric(dist_mat),
    nrow = nrow(airport_tbl),
    ncol = nrow(station_tbl)
  )
  
  station_idx <- max.col(
    -dist_num,
    ties.method = "first"
  )
  
  station_dist <- dist_num[
    cbind(
      seq_len(nrow(airport_tbl)),
      station_idx
    )
  ] / 1000
  
  airport_tbl |>
    dplyr::mutate(
      station_id =
        station_tbl$GHCN_ID[station_idx],
      screen_distance_km =
        station_dist,
      selection_basis =
        "nearest_screened",
      selection_note =
        paste(
          "Nearest USW station after",
          "full-network screening."
        )
    )
}

get_station_overrides <- function() {
  tibble::tribble(
    ~AirportID, ~station_id,
    ~selection_basis, ~selection_note,
    16440L, "USW00013958",
    "historical_station_lineage",
    paste(
      "Old Austin/Mueller observing lineage.",
      "Static GHCNh metadata reflect Camp Mabry."
    ),
    10994L, "USW00013880",
    "airport_identity_override",
    paste(
      "Charleston International station selected",
      "over obsolete Charleston NAS record."
    ),
    11233L, "USW00024018",
    "airport_identity_reviewed",
    paste(
      "Cheyenne aviation station CYS.",
      "Station-list ICAO KFEW is inconsistent",
      "with the station identity."
    ),
    11278L, "USW00013743",
    "airport_identity_override",
    "Washington Reagan National airport station.",
    13487L, "USW00014922",
    "airport_identity_override",
    "Minneapolis-St Paul International airport station.",
    13930L, "USW00094846",
    "airport_identity_override",
    "Chicago O'Hare International airport station.",
    14113L, "USW00024156",
    "airport_identity_override",
    "Pocatello Regional airport station.",
    14689L, "USW00023190",
    "airport_identity_override",
    "Santa Barbara Municipal airport station.",
    16218L, "USW00003145",
    "shared_use_airport_station",
    paste(
      "Yuma MCAS/KNYL station selected for",
      "the SLC-service period."
    )
  )
}

get_epoch_specs <- function() {
  tibble::tribble(
    ~AirportID, ~station_id,
    ~map_start, ~map_end,
    ~selection_basis, ~selection_note,
    11292L, "USW00023062",
    "1900-01-01", "1995-02-27",
    "historical_airport_station",
    "Stapleton operational airport epoch.",
    11292L, "USW00003017",
    "1995-02-28", "2100-12-31",
    "historical_airport_station",
    "Denver International operational airport epoch.",
    14794L, "USW00023186",
    "1900-01-01", "2011-01-12",
    "station_lineage_relocation",
    "KSGU at the former St. George airport.",
    14794L, "USW00093198",
    "2011-01-13", "2011-12-14",
    "station_lineage_relocation",
    "Temporary KDXZ stream at the replacement airport.",
    14794L, "USW00023186",
    "2011-12-15", "2100-12-31",
    "station_lineage_relocation",
    paste(
      "KSGU restored at the replacement airport.",
      "Static GHCNh coordinates reflect an older site."
    )
  ) |>
    dplyr::mutate(
      map_start = as.Date(map_start),
      map_end = as.Date(map_end)
    )
}

apply_station_overrides <- function(nearest_tbl) {
  override_tbl <- get_station_overrides()
  
  nearest_tbl |>
    dplyr::left_join(
      override_tbl |>
        dplyr::rename(
          override_station = station_id,
          override_basis = selection_basis,
          override_note = selection_note
        ),
      by = "AirportID",
      relationship = "many-to-one"
    ) |>
    dplyr::mutate(
      station_id = dplyr::coalesce(
        override_station,
        station_id
      ),
      selection_basis = dplyr::coalesce(
        override_basis,
        selection_basis
      ),
      selection_note = dplyr::coalesce(
        override_note,
        selection_note
      )
    ) |>
    dplyr::select(
      -override_station,
      -override_basis,
      -override_note
    )
}

build_manifest_rows <- function(
    airport_tbl,
    nearest_tbl,
    station_tbl
) {
  epoch_ids <- c(
    11292L,
    14794L
  )
  
  base_rows <- nearest_tbl |>
    dplyr::filter(
      !AirportID %in% epoch_ids
    ) |>
    dplyr::transmute(
      AirportID,
      AirportCode,
      AirportCity,
      airport_latitude = Latitude,
      airport_longitude = Longitude,
      service_start,
      service_end,
      station_id,
      map_start = service_start,
      map_end = service_end,
      screen_distance_km,
      selection_basis,
      selection_note
    )
  
  epoch_rows <- get_epoch_specs() |>
    dplyr::left_join(
      airport_tbl |>
        dplyr::select(
          AirportID,
          AirportCode,
          AirportCity,
          airport_latitude = Latitude,
          airport_longitude = Longitude,
          service_start,
          service_end
        ),
      by = "AirportID",
      relationship = "many-to-one"
    ) |>
    dplyr::mutate(
      map_start = pmax(
        map_start,
        service_start
      ),
      map_end = pmin(
        map_end,
        service_end
      )
    ) |>
    dplyr::filter(
      map_start <= map_end
    ) |>
    dplyr::mutate(
      screen_distance_km = NA_real_
    ) |>
    dplyr::select(
      AirportID,
      AirportCode,
      AirportCity,
      airport_latitude,
      airport_longitude,
      service_start,
      service_end,
      station_id,
      map_start,
      map_end,
      screen_distance_km,
      selection_basis,
      selection_note
    )
  
  manifest <- dplyr::bind_rows(
    base_rows,
    epoch_rows
  ) |>
    dplyr::left_join(
      station_tbl |>
        dplyr::transmute(
          station_id = GHCN_ID,
          station_name = NAME,
          station_icao = ICAO,
          station_latitude = LATITUDE,
          station_longitude = LONGITUDE,
          station_elevation = ELEVATION
        ),
      by = "station_id",
      relationship = "many-to-one"
    ) |>
    dplyr::arrange(
      AirportID,
      map_start,
      station_id
    ) |>
    dplyr::mutate(
      manifest_id = dplyr::row_number(),
      .before = 1
    )
  
  if (anyNA(manifest$station_name)) {
    stop(
      "At least one selected station is absent from metadata."
    )
  }
  
  if (dplyr::n_distinct(
    manifest$AirportID
  ) != nrow(airport_tbl) ||
  !setequal(
    manifest$AirportID,
    airport_tbl$AirportID
  )) {
    stop(
      paste(
        "The station manifest does not cover",
        "every airport in the airport table."
      )
    )
  }
  
  overlap <- manifest |>
    dplyr::group_by(
      AirportID
    ) |>
    dplyr::arrange(
      map_start,
      .by_group = TRUE
    ) |>
    dplyr::mutate(
      prior_end = dplyr::lag(
        map_end
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(
      !is.na(prior_end),
      map_start <= prior_end
    )
  
  if (nrow(overlap) > 0L) {
    stop(
      "At least one station mapping epoch overlaps."
    )
  }
  
  manifest
}

build_inventory_long <- function(
    inv_tbl,
    station_ids,
    years
) {
  inv_tbl |>
    dplyr::filter(
      GHCNh_ID %in% station_ids,
      YEAR %in% years
    ) |>
    tidyr::pivot_longer(
      cols = JAN:DEC,
      names_to = "month_name",
      values_to = "obs_count"
    ) |>
    dplyr::mutate(
      Month = match(
        month_name,
        toupper(month.abb)
      )
    ) |>
    dplyr::select(
      station_id = GHCNh_ID,
      Year = YEAR,
      Month,
      obs_count
    )
}

audit_manifest_inventory <- function(
    manifest,
    service_tbl,
    inv_tbl
) {
  service_months <- service_tbl |>
    dplyr::group_by(
      AirportID,
      Year,
      Month,
      ServiceMonth
    ) |>
    dplyr::summarise(
      Flights = sum(Flights),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      MonthEnd = get_month_end(
        ServiceMonth
      )
    )
  
  month_map <- service_months |>
    dplyr::inner_join(
      manifest |>
        dplyr::select(
          manifest_id,
          AirportID,
          station_id,
          map_start,
          map_end
        ),
      by = "AirportID",
      relationship = "many-to-many"
    ) |>
    dplyr::filter(
      map_start <= MonthEnd,
      map_end >= ServiceMonth
    )
  
  mapped_months <- month_map |>
    dplyr::distinct(
      AirportID,
      Year,
      Month
    )
  
  missing_map <- service_months |>
    dplyr::anti_join(
      mapped_months,
      by = c(
        "AirportID",
        "Year",
        "Month"
      )
    )
  
  if (nrow(missing_map) > 0L) {
    stop(
      "At least one service month lacks a station mapping."
    )
  }
  
  inv_long <- build_inventory_long(
    inv_tbl = inv_tbl,
    station_ids = unique(
      manifest$station_id
    ),
    years = unique(
      service_tbl$Year
    )
  )
  
  month_map <- month_map |>
    dplyr::left_join(
      inv_long,
      by = c(
        "station_id",
        "Year",
        "Month"
      ),
      relationship = "many-to-one"
    ) |>
    dplyr::mutate(
      obs_count = dplyr::coalesce(
        obs_count,
        0
      )
    )
  
  coverage_tbl <- month_map |>
    dplyr::group_by(
      manifest_id
    ) |>
    dplyr::summarise(
      service_months = dplyr::n(),
      covered_months = sum(
        obs_count > 0
      ),
      service_flights = sum(
        Flights
      ),
      covered_flights = sum(
        dplyr::if_else(
          obs_count > 0,
          Flights,
          0L
        )
      ),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      inventory_cov_pct =
        100 * covered_months /
        service_months,
      inventory_flight_cov_pct =
        100 * covered_flights /
        service_flights,
      inventory_status = dplyr::if_else(
        covered_months == service_months,
        "full_service_month_coverage",
        "partial_service_month_coverage"
      )
    )
  
  manifest |>
    dplyr::left_join(
      coverage_tbl,
      by = "manifest_id",
      relationship = "one-to-one"
    )
}

build_download_plan <- function(
    manifest,
    service_tbl
) {
  service_years <- service_tbl |>
    dplyr::distinct(
      AirportID,
      Year
    ) |>
    dplyr::mutate(
      year_start = as.Date(
        sprintf(
          "%04d-01-01",
          Year
        )
      ),
      year_end = as.Date(
        sprintf(
          "%04d-12-31",
          Year
        )
      )
    )
  
  station_year_use <- service_years |>
    dplyr::inner_join(
      manifest |>
        dplyr::select(
          AirportID,
          AirportCode,
          station_id,
          map_start,
          map_end
        ),
      by = "AirportID",
      relationship = "many-to-many"
    ) |>
    dplyr::filter(
      map_start <= year_end,
      map_end >= year_start
    ) |>
    dplyr::distinct(
      AirportID,
      AirportCode,
      station_id,
      year = Year
    ) |>
    dplyr::arrange(
      station_id,
      year,
      AirportID
    )
  
  download_plan <- station_year_use |>
    dplyr::distinct(
      station_id,
      year
    ) |>
    dplyr::arrange(
      station_id,
      year
    )
  
  list(
    station_year_use = station_year_use,
    download_plan = download_plan
  )
}

build_ghcnh_manifest <- function(
    meta_dir = file.path(
      "data_raw",
      "ghcnh_metadata"
    ),
    years = slcflights::available_years(
      type = "main"
    ),
    service_tbl = NULL) {
  
  if (is.null(service_tbl)) {
    service_tbl <- build_service_tbl(
      years = years
    )
  }
  
  airport_tbl <- build_airport_tbl(
    service_tbl
  )
  
  weather_meta <- read_ghcnh_meta(
    meta_dir = meta_dir
  )
  
  nearest_tbl <- find_nearest_station(
    airport_tbl = airport_tbl,
    station_tbl = weather_meta$stations
  ) |>
    apply_station_overrides()
  
  manifest <- build_manifest_rows(
    airport_tbl = airport_tbl,
    nearest_tbl = nearest_tbl,
    station_tbl = weather_meta$stations
  )
  
  manifest <- audit_manifest_inventory(
    manifest = manifest,
    service_tbl = service_tbl,
    inv_tbl = weather_meta$inventory
  )
  
  plan <- build_download_plan(
    manifest = manifest,
    service_tbl = service_tbl
  )
  
  list(
    manifest = manifest,
    download_plan = plan$download_plan,
    station_year_use = plan$station_year_use,
    nearest_screen = nearest_tbl,
    metadata_provenance = weather_meta$provenance
  )
}