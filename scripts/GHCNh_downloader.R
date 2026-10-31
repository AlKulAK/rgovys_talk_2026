# Download GHCNh station-year Parquet files from NOAA.

# Validate a downloaded station-year before promotion.
check_ghcnh_stage <- function(
    path,
    station_id,
    year) {
  
  file_info <- file.info(path)
  
  if (is.na(file_info$size) ||
      file_info$size <= 0) {
    stop("A staged GHCNh file is missing or empty.")
  }
  
  stage_file <- arrow::ReadableFile$create(
    path
  )
  
  on.exit(
    stage_file$close(),
    add = TRUE
  )
  
  stage_key <- arrow::read_parquet(
    stage_file,
    col_select = c(
      "STATION",
      "Year"
    )
  )
  
  if (nrow(stage_key) == 0L) {
    stop("A staged GHCNh file contains no rows.")
  }
  
  if (anyNA(stage_key$STATION) ||
      any(stage_key$STATION != station_id)) {
    stop(
      paste(
        "A staged GHCNh file contains the wrong",
        "or a missing station ID."
      )
    )
  }
  
  year_value <- as.integer(
    stage_key$Year
  )
  
  if (anyNA(year_value) ||
      any(year_value != year)) {
    stop(
      paste(
        "A staged GHCNh file contains the wrong",
        "or a missing year."
      )
    )
  }
  
  invisible(TRUE)
}


download_ghcnh <- function(
    station_id,
    years,
    data_dir = file.path(
      "data_raw",
      "ghcnh"
    ),
    progress = TRUE,
    refresh = FALSE) {
  
  if (!is.character(station_id) ||
      length(station_id) == 0L) {
    stop(
      "`station_id` must be a non-empty character vector."
    )
  }
  
  if (anyNA(station_id) ||
      any(!nzchar(station_id))) {
    stop(
      paste(
        "`station_id` cannot contain missing",
        "or empty values."
      )
    )
  }
  
  if (!is.numeric(years) ||
      length(years) == 0L) {
    stop(
      "`years` must be a non-empty numeric vector."
    )
  }
  
  if (anyNA(years) ||
      any(!is.finite(years)) ||
      any(years != floor(years))) {
    stop(
      "`years` must contain finite whole-number years."
    )
  }
  
  if (!is.logical(progress) ||
      length(progress) != 1L ||
      is.na(progress)) {
    stop(
      "`progress` must be TRUE or FALSE."
    )
  }
  
  if (!is.logical(refresh) ||
      length(refresh) != 1L ||
      is.na(refresh)) {
    stop(
      "`refresh` must be TRUE or FALSE."
    )
  }
  
  station_id <- unique(
    station_id
  )
  
  years <- unique(
    as.integer(years)
  )
  
  request <- expand.grid(
    station_id = station_id,
    year = years,
    stringsAsFactors = FALSE
  )
  
  request <- request[
    order(
      request$station_id,
      request$year
    ),
  ]
  
  row.names(request) <- NULL
  
  base_url <- paste0(
    "https://www.ncei.noaa.gov/oa/",
    "global-historical-climatology-network/",
    "hourly/access/by-year"
  )
  
  request$file_name <- sprintf(
    "GHCNh_%s_%d.parquet",
    request$station_id,
    request$year
  )
  
  request$url <- sprintf(
    "%s/%d/parquet/%s",
    base_url,
    request$year,
    request$file_name
  )
  
  request$station_dir <- file.path(
    data_dir,
    request$station_id
  )
  
  station_dirs <- unique(
    request$station_dir
  )
  
  for (station_dir in station_dirs) {
    if (!dir.exists(station_dir)) {
      dir.create(
        station_dir,
        recursive = TRUE
      )
    }
  }
  
  if (any(!dir.exists(station_dirs))) {
    stop(
      paste(
        "At least one GHCNh data directory",
        "could not be created."
      )
    )
  }
  
  request$destfile <- file.path(
    request$station_dir,
    request$file_name
  )
  
  request$stagefile <- vapply(
    seq_len(
      nrow(request)
    ),
    function(i) {
      tempfile(
        pattern = paste0(
          ".",
          request$file_name[[i]],
          "_"
        ),
        tmpdir = request$station_dir[[i]],
        fileext = ".part"
      )
    },
    character(1)
  )
  
  on.exit(
    unlink(
      request$stagefile[
        file.exists(
          request$stagefile
        )
      ]
    ),
    add = TRUE
  )
  
  existed <- file.exists(
    request$destfile
  )
  
  needed <- !existed |
    refresh
  
  status <- rep(
    NA_character_,
    nrow(request)
  )
  
  status[existed] <- "existing"
  
  source_modified_at <- as.POSIXct(
    rep(
      NA_real_,
      nrow(request)
    ),
    origin = "1970-01-01",
    tz = "UTC"
  )
  
  retrieved_at <- as.POSIXct(
    rep(
      NA_real_,
      nrow(request)
    ),
    origin = "1970-01-01",
    tz = "UTC"
  )
  
  bad_idx <- integer()
  
  if (any(needed)) {
    needed_idx <- which(
      needed
    )
    
    result <- curl::multi_download(
      urls = request$url[needed],
      destfiles = request$stagefile[needed],
      resume = FALSE,
      progress = progress
    )
    
    if (nrow(result) !=
        length(needed_idx)) {
      stop(
        paste(
          "GHCNh download results do not match",
          "the request count."
        )
      )
    }
    
    ok <- !is.na(result$success) &
      result$success &
      result$status_code %in% c(
        200L,
        206L
      )
    
    good_idx <- needed_idx[ok]
    bad_idx <- needed_idx[!ok]
    
    source_modified_at[good_idx] <-
      result$modified[ok]
    
    retrieved_at[good_idx] <-
      Sys.time()
    
    for (i in good_idx) {
      stage_md5 <- unname(
        tools::md5sum(
          request$stagefile[[i]]
        )
      )
      
      if (is.na(stage_md5)) {
        stop(
          paste(
            "A staged GHCNh checksum",
            "could not be calculated."
          )
        )
      }
      
      if (existed[[i]]) {
        local_md5 <- unname(
          tools::md5sum(
            request$destfile[[i]]
          )
        )
        
        if (is.na(local_md5)) {
          stop(
            paste(
              "An existing GHCNh checksum",
              "could not be calculated."
            )
          )
        }
        
        if (identical(
          stage_md5,
          local_md5
        )) {
          status[[i]] <- "unchanged"
          next
        }
      }
      
      check_ghcnh_stage(
        path = request$stagefile[[i]],
        station_id = request$station_id[[i]],
        year = request$year[[i]]
      )
      
      moved <- file.rename(
        from = request$stagefile[[i]],
        to = request$destfile[[i]]
      )
      
      if (!moved) {
        stop(
          paste(
            "A staged GHCNh file could not",
            "be promoted."
          )
        )
      }
      
      if (existed[[i]]) {
        status[[i]] <- "refreshed"
      } else {
        status[[i]] <- "downloaded"
      }
    }


    if (length(bad_idx) > 0L) {
      print(
        result[
          !ok,
          c(
            "url",
            "success",
            "status_code",
            "error"
          )
        ]
      )
      
      stop(
        "At least one GHCNh download failed."
      )
    }
  }
  
  if (anyNA(status)) {
    stop(
      "At least one GHCNh file lacks a final status."
    )
  }
  
  file_info <- file.info(
    request$destfile
  )
  
  if (anyNA(file_info$size) ||
      any(file_info$size <= 0)) {
    stop(
      "At least one GHCNh file is missing or empty."
    )
  }
  
  md5 <- unname(
    tools::md5sum(
      request$destfile
    )
  )
  
  if (anyNA(md5)) {
    stop(
      paste(
        "At least one GHCNh checksum",
        "could not be calculated."
      )
    )
  }
  
  manifest <- data.frame(
    station_id = request$station_id,
    year = request$year,
    status = status,
    url = request$url,
    path = request$destfile,
    size_bytes = unname(
      file_info$size
    ),
    source_modified_at =
      source_modified_at,
    retrieved_at = retrieved_at,
    md5 = md5,
    stringsAsFactors = FALSE
  )
  
  row.names(manifest) <- NULL
  
  manifest
}