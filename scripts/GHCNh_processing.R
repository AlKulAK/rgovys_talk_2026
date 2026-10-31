# Build canonical GHCNh aviation and element-level weather data.

# Resolve the station-year files requested for processing.
resolve_ghcnh_files <- function(
    station_id,
    data_dir,
    years = NULL) {
  
  if (!is.character(station_id) ||
      length(station_id) != 1L ||
      is.na(station_id) ||
      !nzchar(station_id)) {
    stop(
      "`station_id` must be one non-empty character value."
    )
  }
  
  if (!is.null(years)) {
    if (!is.numeric(years) ||
        length(years) == 0L) {
      stop(
        "`years` must be NULL or a non-empty numeric vector."
      )
    }
    
    if (anyNA(years) ||
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
  }
  
  station_dir <- file.path(
    data_dir,
    station_id
  )
  
  if (!dir.exists(station_dir)) {
    stop(
      "The GHCNh station directory does not exist."
    )
  }
  
  if (is.null(years)) {
    file_pattern <- sprintf(
      "^GHCNh_%s_[0-9]{4}\\.parquet$",
      station_id
    )
    
    ghcnh_files <- list.files(
      station_dir,
      pattern = file_pattern,
      full.names = TRUE
    )
    
    if (length(ghcnh_files) == 0L) {
      stop(
        "No GHCNh station-year Parquet files were found."
      )
    }
    
    file_years <- as.integer(
      sub(
        "^.*_([0-9]{4})\\.parquet$",
        "\\1",
        basename(
          ghcnh_files
        )
      )
    )
    
    if (anyNA(file_years)) {
      stop(
        "At least one GHCNh file year could not be parsed."
      )
    }
    
    if (anyDuplicated(file_years) > 0L) {
      stop(
        "Duplicate GHCNh station-year files were found."
      )
    }
    
    file_order <- order(
      file_years
    )
    
    ghcnh_files <- ghcnh_files[
      file_order
    ]
    
    file_years <- file_years[
      file_order
    ]
  } else {
    file_years <- years
    
    file_names <- sprintf(
      "GHCNh_%s_%d.parquet",
      station_id,
      file_years
    )
    
    ghcnh_files <- file.path(
      station_dir,
      file_names
    )
    
    file_exists <- file.exists(
      ghcnh_files
    )
    
    if (any(!file_exists)) {
      missing_years <- file_years[
        !file_exists
      ]
      
      stop(
        paste(
          "Missing GHCNh station-year files:",
          paste(
            missing_years,
            collapse = ", "
          )
        )
      )
    }
  }
  
  list(
    files = ghcnh_files,
    years = file_years
  )
}


# Identify structured report metadata used for aviation admission.
get_aviation_report_cols <- function(
    ghcnh_ds) {
  
  all_names <- names(
    ghcnh_ds
  )
  
  measure_vars <- sub(
    "_Measurement_Code$",
    "",
    grep(
      "_Measurement_Code$",
      all_names,
      value = TRUE
    )
  )
  
  measure_vars <- sort(
    unique(
      setdiff(
        measure_vars,
        "REM"
      )
    )
  )
  
  if (length(measure_vars) == 0L) {
    stop(
      "No structured GHCNh measurement families were found."
    )
  }
  
  report_cols <- paste0(
    measure_vars,
    "_Report_Type"
  )
  
  missing_cols <- setdiff(
    report_cols,
    all_names
  )
  
  if (length(missing_cols) > 0L) {
    print(
      missing_cols
    )
    
    stop(
      paste(
        "At least one structured GHCNh family",
        "lacks report-type metadata."
      )
    )
  }
  
  report_cols
}


# Build one source-grounded aviation row per eligible GHCNh timestamp.
make_ghcnh_index <- function(
    ghcnh_ds,
    station_id,
    years) {
  
  direct_types <- c(
    "SAO",
    "SAOSP",
    "FM15",
    "FM16"
  )
  
  merged_types <- c(
    "SYSA",
    "SYMT"
  )
  
  report_cols <- get_aviation_report_cols(
    ghcnh_ds
  )
  
  keep_cols <- c(
    "STATION",
    "Station_name",
    "DATE",
    "Year",
    report_cols
  )
  
  raw_index <- ghcnh_ds |>
    dplyr::select(
      dplyr::all_of(
        keep_cols
      )
    ) |>
    dplyr::collect()
  
  if (nrow(raw_index) == 0L) {
    stop(
      "The resolved GHCNh dataset contains no rows."
    )
  }
  
  report_mat <- as.matrix(
    raw_index[
      report_cols
    ]
  )
  
  direct_mat <- matrix(
    report_mat %in%
      direct_types,
    nrow = nrow(
      report_mat
    ),
    ncol = ncol(
      report_mat
    )
  )
  
  merged_mat <- matrix(
    report_mat %in%
      merged_types,
    nrow = nrow(
      report_mat
    ),
    ncol = ncol(
      report_mat
    )
  )
  
  has_direct <- rowSums(
    direct_mat
  ) > 0L
  
  has_merged <- rowSums(
    merged_mat
  ) > 0L
  
  eligible <- has_direct |
    has_merged
  
  if (!any(eligible)) {
    stop(
      "No eligible aviation weather observations were found."
    )
  }
  
  raw_index <- raw_index[
    eligible,
    ,
    drop = FALSE
  ]
  
  has_direct <- has_direct[
    eligible
  ]
  
  has_merged <- has_merged[
    eligible
  ]
  
  index <- tibble::tibble(
    station_id = raw_index$STATION,
    station_name = raw_index$Station_name,
    raw_time = raw_index$DATE,
    obs_time = lubridate::ymd_hms(
      raw_index$DATE,
      tz = "UTC"
    ),
    year = as.integer(
      raw_index$Year
    ),
    aviation_class = dplyr::case_when(
      has_direct &
        has_merged ~
        "direct_and_merged",
      has_direct ~
        "direct",
      has_merged ~
        "merged_only"
    )
  ) |>
    dplyr::arrange(
      obs_time
    )
  
  if (anyNA(index$station_id) ||
      any(
        index$station_id !=
        station_id
      )) {
    stop(
      paste(
        "A different or missing station",
        "is present in the GHCNh files."
      )
    )
  }
  
  if (anyNA(index$obs_time)) {
    stop(
      "At least one GHCNh timestamp failed to parse."
    )
  }
  
  if (anyNA(index$year)) {
    stop(
      "At least one GHCNh year failed to parse."
    )
  }
  
  if (any(
    index$year !=
    lubridate::year(
      index$obs_time
    )
  )) {
    stop(
      paste(
        "At least one GHCNh year disagrees",
        "with its timestamp."
      )
    )
  }
  
  if (any(
    !index$year %in%
    years
  )) {
    stop(
      paste(
        "At least one GHCNh observation",
        "falls outside the resolved years."
      )
    )
  }
  
  valid_class <- c(
    "direct",
    "merged_only",
    "direct_and_merged"
  )
  
  if (anyNA(index$aviation_class) ||
      any(
        !index$aviation_class %in%
        valid_class
      )) {
    stop(
      "At least one aviation-class value is invalid."
    )
  }
  
  if (anyDuplicated(
    index$raw_time
  ) > 0L) {
    stop(
      paste(
        "Eligible aviation observations",
        "contain duplicate raw timestamps."
      )
    )
  }
  
  if (anyDuplicated(
    index$obs_time
  ) > 0L) {
    stop(
      paste(
        "Eligible aviation observations",
        "contain duplicate observation times."
      )
    )
  }
  
  index
}


build_ghcnh_index <- function(
    station_id,
    data_dir = file.path(
      "data_raw",
      "ghcnh"
    ),
    years = NULL) {
  
  file_spec <- resolve_ghcnh_files(
    station_id = station_id,
    data_dir = data_dir,
    years = years
  )
  
  ghcnh_ds <- arrow::open_dataset(
    file_spec$files,
    format = "parquet",
    unify_schemas = TRUE
  )
  
  make_ghcnh_index(
    ghcnh_ds = ghcnh_ds,
    station_id = station_id,
    years = file_spec$years
  )
}


list_ghcnh_elements <- function(
    station_id,
    data_dir = file.path(
      "data_raw",
      "ghcnh"
    ),
    years = NULL) {
  
  file_spec <- resolve_ghcnh_files(
    station_id = station_id,
    data_dir = data_dir,
    years = years
  )
  
  ghcnh_ds <- arrow::open_dataset(
    file_spec$files,
    format = "parquet",
    unify_schemas = TRUE
  )
  
  all_names <- names(
    ghcnh_ds
  )
  
  measure_vars <- sub(
    "_Measurement_Code$",
    "",
    grep(
      "_Measurement_Code$",
      all_names,
      value = TRUE
    )
  )
  
  measure_vars <- sort(
    unique(
      measure_vars
    )
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
        measure_vars,
        c(
          "",
          meta_suffix
        ),
        paste0
      )
    )
  )
  
  if (!all(
    family_cols %in%
    all_names
  )) {
    stop(
      paste(
        "At least one GHCNh measurement",
        "family is incomplete."
      )
    )
  }
  
  measure_vars
}


extract_ghcnh_element <- function(
    station_id,
    element,
    data_dir = file.path(
      "data_raw",
      "ghcnh"
    ),
    years = NULL) {
  
  if (!is.character(element) ||
      length(element) != 1L ||
      is.na(element) ||
      !nzchar(element)) {
    stop(
      "`element` must be one non-empty character value."
    )
  }
  
  file_spec <- resolve_ghcnh_files(
    station_id = station_id,
    data_dir = data_dir,
    years = years
  )
  
  ghcnh_ds <- arrow::open_dataset(
    file_spec$files,
    format = "parquet",
    unify_schemas = TRUE
  )
  
  all_names <- names(
    ghcnh_ds
  )
  
  meta_suffix <- c(
    "_Measurement_Code",
    "_Quality_Code",
    "_Report_Type",
    "_Source_Code",
    "_Source_Station_ID"
  )
  
  measure_vars <- sub(
    "_Measurement_Code$",
    "",
    grep(
      "_Measurement_Code$",
      all_names,
      value = TRUE
    )
  )
  
  measure_vars <- sort(
    unique(
      measure_vars
    )
  )
  
  if (!element %in%
      measure_vars) {
    stop(
      "`element` is not a GHCNh measurement family."
    )
  }
  
  base_cols <- c(
    "STATION",
    "Station_name",
    "DATE",
    "Year",
    "Month",
    "Day",
    "Hour",
    "Minute",
    "LATITUDE",
    "LONGITUDE",
    "ELEVATION"
  )
  
  family_cols <- c(
    element,
    paste0(
      element,
      meta_suffix
    )
  )
  
  keep_cols <- c(
    base_cols,
    family_cols
  )
  
  missing_cols <- setdiff(
    keep_cols,
    all_names
  )
  
  if (length(
    missing_cols
  ) > 0L) {
    stop(
      "The GHCNh measurement family is incomplete."
    )
  }
  
  value_sym <- rlang::sym(
    element
  )
  
  result <- ghcnh_ds |>
    dplyr::select(
      dplyr::all_of(
        keep_cols
      )
    ) |>
    dplyr::filter(
      !is.na(
        !!value_sym
      )
    ) |>
    dplyr::collect() |>
    dplyr::mutate(
      obs_time = lubridate::ymd_hms(
        DATE,
        tz = "UTC"
      )
    ) |>
    dplyr::relocate(
      obs_time,
      .after = DATE
    ) |>
    dplyr::arrange(
      obs_time
    )
  
  if (nrow(result) == 0L) {
    stop(
      paste(
        "The requested GHCNh element",
        "has no observations."
      )
    )
  }
  
  if (anyNA(
    result$obs_time
  )) {
    stop(
      "At least one GHCNh timestamp failed to parse."
    )
  }
  
  if (anyNA(
    result$STATION
  ) ||
  any(
    result$STATION !=
    station_id
  )) {
    stop(
      paste(
        "A different or missing station",
        "is present in GHCNh."
      )
    )
  }
  
  parsed_year <- lubridate::year(
    result$obs_time
  )
  
  source_year <- as.integer(
    result$Year
  )
  
  if (anyNA(
    source_year
  )) {
    stop(
      "At least one GHCNh year failed to parse."
    )
  }
  
  if (any(
    source_year !=
    parsed_year
  )) {
    stop(
      paste(
        "At least one GHCNh year disagrees",
        "with its timestamp."
      )
    )
  }
  
  if (any(
    !source_year %in%
    file_spec$years
  )) {
    stop(
      paste(
        "At least one GHCNh observation",
        "falls outside the resolved years."
      )
    )
  }
  
  result
}


build_ghcnh_weather <- function(
    station_id,
    elements,
    data_dir = file.path(
      "data_raw",
      "ghcnh"
    ),
    years = NULL) {
  
  if (!is.character(elements) ||
      length(elements) == 0L ||
      anyNA(elements) ||
      any(!nzchar(elements))) {
    stop(
      "`elements` must be a non-empty character vector."
    )
  }
  
  elements <- unique(
    elements
  )
  
  file_spec <- resolve_ghcnh_files(
    station_id = station_id,
    data_dir = data_dir,
    years = years
  )
  
  ghcnh_ds <- arrow::open_dataset(
    file_spec$files,
    format = "parquet",
    unify_schemas = TRUE
  )
  
  all_names <- names(
    ghcnh_ds
  )
  
  meta_suffix <- c(
    "_Measurement_Code",
    "_Quality_Code",
    "_Report_Type",
    "_Source_Code",
    "_Source_Station_ID"
  )
  
  measure_vars <- sub(
    "_Measurement_Code$",
    "",
    grep(
      "_Measurement_Code$",
      all_names,
      value = TRUE
    )
  )
  
  measure_vars <- sort(
    unique(
      measure_vars
    )
  )
  
  unknown <- setdiff(
    elements,
    measure_vars
  )
  
  if (length(unknown) > 0L) {
    print(
      unknown
    )
    
    stop(
      paste(
        "At least one requested GHCNh",
        "element is unavailable."
      )
    )
  }
  
  family_cols <- as.vector(
    t(
      outer(
        elements,
        c(
          "",
          meta_suffix
        ),
        paste0
      )
    )
  )
  
  if (!all(
    family_cols %in%
    all_names
  )) {
    stop(
      paste(
        "At least one requested GHCNh",
        "family is incomplete."
      )
    )
  }
  
  index <- make_ghcnh_index(
    ghcnh_ds = ghcnh_ds,
    station_id = station_id,
    years = file_spec$years
  )
  
  base_cols <- c(
    "STATION",
    "DATE",
    "Month",
    "Day",
    "Hour",
    "Minute",
    "LATITUDE",
    "LONGITUDE",
    "ELEVATION"
  )
  
  keep_cols <- unique(
    c(
      base_cols,
      family_cols
    )
  )
  
  raw_weather <- ghcnh_ds |>
    dplyr::select(
      dplyr::all_of(
        keep_cols
      )
    ) |>
    dplyr::collect()
  
  weather <- index |>
    dplyr::inner_join(
      raw_weather,
      dplyr::join_by(
        station_id == STATION,
        raw_time == DATE
      ),
      relationship = "one-to-one"
    ) |>
    dplyr::rename(
      month = Month,
      day = Day,
      hour = Hour,
      minute = Minute,
      latitude = LATITUDE,
      longitude = LONGITUDE,
      elevation = ELEVATION
    ) |>
    dplyr::select(
      station_id,
      station_name,
      raw_time,
      obs_time,
      year,
      month,
      day,
      hour,
      minute,
      latitude,
      longitude,
      elevation,
      aviation_class,
      dplyr::all_of(
        family_cols
      )
    ) |>
    dplyr::arrange(
      obs_time
    )
  
  if (nrow(weather) !=
      nrow(index)) {
    stop(
      paste(
        "The canonical weather rows do not",
        "match the index rows."
      )
    )
  }
  
  if (anyDuplicated(
    weather$raw_time
  ) > 0L) {
    stop(
      paste(
        "The canonical weather table",
        "has duplicate raw timestamps."
      )
    )
  }
  
  if (anyDuplicated(
    weather$obs_time
  ) > 0L) {
    stop(
      paste(
        "The canonical weather table",
        "has duplicate observation times."
      )
    )
  }
  
  if (any(
    weather$year !=
    lubridate::year(
      weather$obs_time
    )
  )) {
    stop(
      paste(
        "The canonical weather year disagrees",
        "with its observation time."
      )
    )
  }
  
  weather
}