# SLC Airport & Weather Station Explorer
# Schema-5 daily/hourly reporting loader.
#
# Application-specific supporting code for app/R/.
# Shiny automatically sources files in this directory.
#
# The loader reads prepared reporting partitions only.
# It does not modify data or read raw GHCNh observations.

new_hourly_reporting_loader <- function(
  app_data,
  index_data,
  stage_dir = base::file.path("data", "hourly_reporting"),
  max_cached_partitions = 3L,
  check_md5 = TRUE
) {
  required_map <- base::c(
    "airport_id", "service_month", "weather_period_id",
    "station_id", "station_name", "station_icao",
    "airport_timezone", "period_start", "period_end"
  )

  required_month <- base::c(
    "airport_id", "service_month", "element",
    "n_applicable_days", "n_days_with_values"
  )

  if (!base::is.list(app_data) ||
    !base::identical(app_data$schema_version, 4L) ||
    !base::is.data.frame(app_data$airport_periods) ||
    !base::is.data.frame(app_data$station_periods) ||
    !base::is.data.frame(app_data$service_months) ||
    !base::is.data.frame(app_data$latest_airports) ||
    !base::is.list(app_data$metadata) ||
    !base::all(required_map %in%
      base::names(app_data$airport_periods)) ||
    !"weather_period_id" %in%
      base::names(app_data$station_periods) ||
    !base::is.list(index_data) ||
    !base::identical(index_data$schema_version, 2L) ||
    !base::is.data.frame(index_data$family_months) ||
    !base::is.data.frame(index_data$airport_months) ||
    !base::all(required_month %in%
      base::names(index_data$family_months))) {
    base::stop(
      "Incompatible prepared app data or airport index.",
      call. = FALSE
    )
  }

  if (base::anyNA(
    app_data$airport_periods$weather_period_id
  ) ||
    base::anyNA(
      app_data$station_periods$weather_period_id
    ) ||
    !base::inherits(
      app_data$airport_periods$service_month, "Date"
    ) ||
    !base::inherits(
      app_data$airport_periods$period_start, "Date"
    ) ||
    !base::inherits(
      app_data$airport_periods$period_end, "Date"
    ) ||
    !base::inherits(
      index_data$family_months$service_month, "Date"
    )) {
    base::stop(
      "Prepared mapping keys or dates are invalid.",
      call. = FALSE
    )
  }

  max_cached_partitions <- base::as.integer(
    max_cached_partitions
  )

  if (base::length(max_cached_partitions) != 1L ||
    base::is.na(max_cached_partitions) ||
    max_cached_partitions < 1L) {
    base::stop(
      "max_cached_partitions must be a positive integer.",
      call. = FALSE
    )
  }

  manifest_path <- base::file.path(
    stage_dir,
    "manifest.rds"
  )

  if (!base::file.exists(manifest_path)) {
    base::stop(
      "Hourly manifest not found: ",
      manifest_path,
      call. = FALSE
    )
  }

  manifest <- base::readRDS(manifest_path)

  if (!base::is.list(manifest) ||
    !base::identical(manifest$schema_version, 5L) ||
    !base::identical(manifest$base_schema_version, 4L) ||
    !base::identical(manifest$hourly_revision, 2L) ||
    !base::isTRUE(manifest$complete) ||
    !base::identical(
      manifest$build_id,
      index_data$hourly_build_id
    ) ||
    !base::identical(
      manifest$elements,
      index_data$elements
    ) ||
    !base::identical(
      manifest$elements,
      app_data$metadata$elements
    )) {
    base::stop(
      "Hourly manifest, airport index, and app data do not match.",
      call. = FALSE
    )
  }

  parts <- manifest$partitions

  required_parts <- base::c(
    "partition_id", "group_key", "relative_path", "file_md5",
    "bytes", "station_hour_rows", "element_hour_rows",
    "station_day_rows", "element_day_rows", "weather_period_ids"
  )

  if (!base::is.data.frame(parts) ||
    !base::all(required_parts %in% base::names(parts)) ||
    base::nrow(parts) == 0L ||
    !base::identical(
      parts$partition_id,
      base::seq_len(base::nrow(parts))
    ) ||
    !base::is.list(parts$weather_period_ids) ||
    base::anyNA(parts$relative_path) ||
    base::anyNA(parts$file_md5) ||
    base::anyDuplicated(parts$relative_path) > 0L ||
    !base::all(base::grepl(
      "^hourly/part_[0-9]{5}\\.rds$",
      parts$relative_path
    ))) {
    base::stop(
      "The hourly partition manifest is invalid.",
      call. = FALSE
    )
  }

  paths <- base::file.path(
    stage_dir,
    parts$relative_path
  )

  if (!base::all(base::file.exists(paths))) {
    base::stop(
      "One or more hourly partition files are missing.",
      call. = FALSE
    )
  }

  period_ids <- base::unlist(
    parts$weather_period_ids,
    use.names = FALSE
  )

  source_ids <- app_data$station_periods$weather_period_id
  mapped_ids <- app_data$airport_periods$weather_period_id

  if (!base::is.numeric(period_ids) ||
    base::length(period_ids) == 0L ||
    base::anyNA(period_ids) ||
    base::any(period_ids < 1L) ||
    base::anyDuplicated(period_ids) > 0L ||
    !base::setequal(period_ids, source_ids) ||
    !base::setequal(period_ids, mapped_ids)) {
    base::stop(
      "Weather-period IDs disagree across the builds.",
      call. = FALSE
    )
  }

  app_months <- base::unique(base::paste(
    app_data$service_months$AirportID,
    app_data$service_months$ServiceMonth,
    sep = "|"
  ))

  index_months <- base::paste(
    index_data$airport_months$airport_id,
    index_data$airport_months$service_month,
    sep = "|"
  )

  if (!base::setequal(app_months, index_months)) {
    base::stop(
      "App and index airport-month populations disagree.",
      call. = FALSE
    )
  }

  period_partition <- base::integer(
    base::max(period_ids)
  )

  for (k in base::seq_len(base::nrow(parts))) {
    period_partition[
      parts$weather_period_ids[[k]]
    ] <- k
  }

  cache <- base::new.env(
    parent = base::emptyenv()
  )

  cache_order <- base::character(0)

  read_partition <- function(k) {
    key <- base::as.character(k)

    if (base::exists(
      key,
      envir = cache,
      inherits = FALSE
    )) {
      cache_order <<- base::c(
        cache_order[cache_order != key],
        key
      )

      return(
        base::get(
          key,
          envir = cache,
          inherits = FALSE
        )
      )
    }

    path <- paths[[k]]

    if (base::isTRUE(check_md5)) {
      actual_md5 <- base::unname(
        base::as.character(tools::md5sum(path))
      )

      if (!base::identical(
        actual_md5,
        parts$file_md5[[k]]
      )) {
        base::stop(
          "Hourly partition checksum mismatch: ",
          path,
          call. = FALSE
        )
      }
    }

    part <- base::readRDS(path)

    required_tables <- base::c(
      "station_days", "element_days",
      "station_hours", "element_hours"
    )

    expected_rows <- base::c(
      parts$station_day_rows[[k]],
      parts$element_day_rows[[k]],
      parts$station_hour_rows[[k]],
      parts$element_hour_rows[[k]]
    )

    if (!base::is.list(part) ||
      !base::identical(part$schema_version, 5L) ||
      !base::identical(part$build_id, manifest$build_id) ||
      !base::identical(
        part$group_key,
        parts$group_key[[k]]
      ) ||
      !base::identical(
        part$weather_period_ids,
        parts$weather_period_ids[[k]]
      ) ||
      !base::all(
        required_tables %in% base::names(part)
      )) {
      base::stop(
        "Invalid hourly partition identity: ",
        path,
        call. = FALSE
      )
    }

    for (j in base::seq_along(required_tables)) {
      tbl <- part[[required_tables[[j]]]]

      if (!base::is.data.frame(tbl) ||
        base::nrow(tbl) != expected_rows[[j]]) {
        base::stop(
          "Invalid hourly partition table: ",
          path,
          call. = FALSE
        )
      }
    }

    if (base::length(cache_order) >=
      max_cached_partitions) {
      old <- cache_order[[1L]]

      base::rm(
        list = old,
        envir = cache
      )

      cache_order <<- cache_order[-1L]
    }

    base::assign(
      key,
      part,
      envir = cache
    )

    cache_order <<- base::c(
      cache_order,
      key
    )

    part
  }

  validate_keys <- function(airport_id, element) {
    if (base::length(airport_id) != 1L ||
      base::is.na(airport_id) ||
      !(airport_id %in%
        app_data$latest_airports$AirportID)) {
      base::stop(
        "Unknown canonical airport ID.",
        call. = FALSE
      )
    }

    if (base::length(element) != 1L ||
      base::is.na(element) ||
      !(element %in% index_data$elements)) {
      base::stop(
        "Unknown GHCNh measurement family.",
        call. = FALSE
      )
    }

    base::invisible(TRUE)
  }

  month_mappings <- function(airport_id, service_month) {
    map <- app_data$airport_periods

    map <- map[
      map$airport_id == airport_id &
        map$service_month == service_month, ,
      drop = FALSE
    ]

    cols <- base::c(
      "weather_period_id", "station_id", "station_name",
      "station_icao", "airport_timezone",
      "period_start", "period_end"
    )

    base::unique(map[cols])
  }

  active_days <- function(map) {
    pieces <- base::lapply(
      base::seq_len(base::nrow(map)),
      function(j) {
        dates <- base::seq.Date(
          map$period_start[[j]],
          map$period_end[[j]],
          by = "day"
        )

        base::data.frame(
          weather_period_id = base::rep(
            map$weather_period_id[[j]],
            base::length(dates)
          ),
          local_date = dates
        )
      }
    )

    base::unique(
      base::do.call(base::rbind, pieces)
    )
  }

  selected_table <- function(
    map,
    table_name,
    element = NULL
  ) {
    ids <- base::unique(
      map$weather_period_id
    )

    partition_ids <- base::sort(base::unique(
      period_partition[ids]
    ))

    if (base::any(partition_ids < 1L)) {
      base::stop(
        "A mapped weather period has no partition.",
        call. = FALSE
      )
    }

    rows <- base::lapply(
      partition_ids,
      function(k) {
        tbl <- read_partition(k)[[table_name]]

        tbl <- tbl[
          tbl$weather_period_id %in% ids, ,
          drop = FALSE
        ]

        if (!base::is.null(element)) {
          tbl <- tbl[
            tbl$element == element, ,
            drop = FALSE
          ]
        }

        tbl
      }
    )

    base::do.call(
      base::rbind,
      rows
    )
  }

  month <- function(
    airport_id,
    service_month,
    element
  ) {
    validate_keys(airport_id, element)

    if (base::length(service_month) != 1L ||
      !base::inherits(service_month, "Date") ||
      base::is.na(service_month) ||
      base::format(service_month, "%d") != "01") {
      base::stop(
        "service_month must be a first-of-month Date.",
        call. = FALSE
      )
    }

    map <- month_mappings(
      airport_id,
      service_month
    )

    if (base::nrow(map) == 0L) {
      return(base::list(
        applicable = FALSE,
        airport_id = airport_id,
        service_month = service_month,
        element = element,
        stations = map,
        days = base::data.frame(),
        n_applicable_days = 0L,
        n_days_with_values = 0L,
        report_day_pct = NA_real_
      ))
    }

    active <- active_days(map)

    station <- selected_table(
      map,
      "station_days"
    )

    values <- selected_table(
      map,
      "element_days",
      element
    )

    join_cols <- base::c(
      "weather_period_id",
      "local_date"
    )

    station <- base::merge(
      active,
      station,
      by = join_cols,
      all.x = TRUE,
      sort = FALSE
    )

    values <- base::merge(
      active,
      values,
      by = join_cols,
      all.x = TRUE,
      sort = FALSE
    )

    station$n_records[
      base::is.na(station$n_records)
    ] <- 0L

    values$n_value_reports[
      base::is.na(values$n_value_reports)
    ] <- 0L

    dates <- base::sort(
      base::unique(active$local_date)
    )

    day_numbers <- base::as.numeric(dates)

    day_counts <- base::data.frame(
      local_date = dates,
      n_station_records = base::vapply(
        day_numbers,
        function(day_number) {
          base::as.numeric(base::sum(
            station$n_records[
              base::as.numeric(station$local_date) ==
                day_number
            ]
          ))
        },
        numeric(1)
      ),
      n_value_reports = base::vapply(
        day_numbers,
        function(day_number) {
          base::as.numeric(base::sum(
            values$n_value_reports[
              base::as.numeric(values$local_date) ==
                day_number
            ]
          ))
        },
        numeric(1)
      )
    )

    if (base::any(
      day_counts$n_value_reports >
        day_counts$n_station_records
    )) {
      base::stop(
        "Family reports exceed station records on a date.",
        call. = FALSE
      )
    }

    day_counts$station_reported <-
      day_counts$n_station_records > 0

    day_counts$family_reported <-
      day_counts$n_value_reports > 0

    n_applicable <- base::nrow(day_counts)

    n_reported <- base::sum(
      day_counts$family_reported
    )

    expected <- index_data$family_months

    expected <- expected[
      expected$airport_id == airport_id &
        expected$service_month == service_month &
        expected$element == element, ,
      drop = FALSE
    ]

    if (base::nrow(expected) != 1L ||
      expected$n_applicable_days[[1L]] != n_applicable ||
      expected$n_days_with_values[[1L]] != n_reported) {
      base::stop(
        "Daily family reporting does not reconcile with the index.",
        call. = FALSE
      )
    }

    base::list(
      applicable = TRUE,
      airport_id = airport_id,
      service_month = service_month,
      element = element,
      stations = map,
      days = day_counts,
      n_applicable_days = n_applicable,
      n_days_with_values = n_reported,
      report_day_pct = 100 * n_reported / n_applicable
    )
  }

  day <- function(
    airport_id,
    local_date,
    element
  ) {
    validate_keys(airport_id, element)

    if (base::length(local_date) != 1L ||
      !base::inherits(local_date, "Date") ||
      base::is.na(local_date)) {
      base::stop(
        "local_date must be a single Date.",
        call. = FALSE
      )
    }

    service_month <- base::as.Date(
      base::format(local_date, "%Y-%m-01")
    )

    info <- month(
      airport_id,
      service_month,
      element
    )

    if (!base::isTRUE(info$applicable)) {
      return(base::list(
        applicable = FALSE,
        airport_id = airport_id,
        local_date = local_date,
        element = element,
        hours = base::data.frame()
      ))
    }

    matching_day <- info$days[
      info$days$local_date == local_date, ,
      drop = FALSE
    ]

    if (base::nrow(matching_day) == 0L) {
      return(base::list(
        applicable = FALSE,
        airport_id = airport_id,
        local_date = local_date,
        element = element,
        hours = base::data.frame()
      ))
    }

    map <- info$stations

    map <- map[
      map$period_start <= local_date &
        map$period_end >= local_date, ,
      drop = FALSE
    ]

    timezones <- base::unique(
      map$airport_timezone
    )

    if (base::length(timezones) != 1L ||
      base::is.na(timezones[[1L]]) ||
      !timezones[[1L]] %in% base::OlsonNames()) {
      base::stop(
        "Invalid or ambiguous airport time zone.",
        call. = FALSE
      )
    }

    timezone <- timezones[[1L]]

    hours <- selected_table(
      map,
      "station_hours"
    )

    values <- selected_table(
      map,
      "element_hours",
      element
    )

    local_hour_date <- base::as.Date(base::format(
      hours$hour_start_utc,
      "%Y-%m-%d",
      tz = timezone
    ))

    hours <- hours[
      local_hour_date == local_date, ,
      drop = FALSE
    ]

    values$hour_epoch <- base::as.numeric(
      values$hour_start_utc
    )

    hours$hour_epoch <- base::as.numeric(
      hours$hour_start_utc
    )

    values <- values[
      values$hour_epoch %in% hours$hour_epoch, ,
      drop = FALSE
    ]

    joined <- base::merge(
      hours[base::c(
        "weather_period_id",
        "hour_epoch",
        "n_records"
      )],
      values[base::c(
        "weather_period_id",
        "hour_epoch",
        "n_value_reports"
      )],
      by = base::c(
        "weather_period_id",
        "hour_epoch"
      ),
      all.x = TRUE,
      sort = FALSE
    )

    joined$n_value_reports[
      base::is.na(joined$n_value_reports)
    ] <- 0L

    epochs <- base::sort(
      base::unique(joined$hour_epoch)
    )

    hourly <- base::data.frame(
      hour_start_utc = base::as.POSIXct(
        epochs,
        origin = "1970-01-01",
        tz = "UTC"
      ),
      n_station_records = base::vapply(
        epochs,
        function(x) {
          base::as.numeric(base::sum(
            joined$n_records[
              joined$hour_epoch == x
            ]
          ))
        },
        numeric(1)
      ),
      n_value_reports = base::vapply(
        epochs,
        function(x) {
          base::as.numeric(base::sum(
            joined$n_value_reports[
              joined$hour_epoch == x
            ]
          ))
        },
        numeric(1)
      )
    )

    if (base::any(
      hourly$n_value_reports >
        hourly$n_station_records
    )) {
      base::stop(
        "Hourly family reports exceed station records.",
        call. = FALSE
      )
    }

    hourly$local_hour <- base::format(
      hourly$hour_start_utc,
      "%Y-%m-%d %H:%M %Z (%z)",
      tz = timezone
    )

    hourly$timezone <- base::rep(
      timezone,
      base::nrow(hourly)
    )

    if (base::sum(hourly$n_station_records) !=
      matching_day$n_station_records[[1L]] ||
      base::sum(hourly$n_value_reports) !=
        matching_day$n_value_reports[[1L]]) {
      base::stop(
        "Hourly counts do not reconcile with local-day totals.",
        call. = FALSE
      )
    }

    base::list(
      applicable = TRUE,
      airport_id = airport_id,
      local_date = local_date,
      element = element,
      timezone = timezone,
      stations = map,
      hours = hourly,
      n_station_records = base::sum(
        hourly$n_station_records
      ),
      n_value_reports = base::sum(
        hourly$n_value_reports
      )
    )
  }

  base::list(
    build_id = manifest$build_id,
    partition_count = base::nrow(parts),
    month = month,
    day = day
  )
}
