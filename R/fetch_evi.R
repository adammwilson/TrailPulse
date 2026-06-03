# R/fetch_evi.R
# Fetches HLS EVI for a park area via NASA AppEEARS async API.
# HLS = Harmonized Sentinel-2  |  30 m, ~2–3 day revisit.
# Uses HLSS30_VI.020 (pre-computed VI product) for EVI and HLSS30.020 for Fmask.
#
# Workflow (incremental async with persistent cache):
#   1. Fresh RDS cache (< rds_max_age_h)  → return immediately.
#   2. RDS exists but stale               → submit task for (max_date+1 → today).
#   3. No RDS                             → submit full-history task.
#   4. Task pending/running               → return stale cache (if any) and wait.
#   5. Task done                          → download new TIFFs, compute means for
#                                           new dates only, append to RDS, return.
#   6. Task expired/error                 → delete task file, resubmit same window.
#
# Required env vars:
#   EARTHDATA_USER      NASA Earthdata username
#   EARTHDATA_PASSWORD  NASA Earthdata password
#
# HLS coverage: HLSS30_VI.020 starts 2015-11-28; latency ~2–3 days.
# Output: tibble(date, evi, n_pixels) — mean EVI of clear pixels over the park.
# Full GeoTIFF rasters retained in cache_dir/evi_rasters/ for future mapping.

.AE_BASE <- "https://appeears.earthdatacloud.nasa.gov/api"

# ── Authenticate ─────────────────────────────────────────────────────────────
.ae_token <- function() {
  user <- Sys.getenv("EARTHDATA_USER",     unset = "")
  pass <- Sys.getenv("EARTHDATA_PASSWORD", unset = "")
  if (nchar(user) == 0 || nchar(pass) == 0)
    stop("Set EARTHDATA_USER and EARTHDATA_PASSWORD for NASA AppEEARS auth")
  httr2::request(paste0(.AE_BASE, "/login")) |>
    httr2::req_auth_basic(user, pass) |>
    httr2::req_method("POST") |>
    httr2::req_perform() |>
    httr2::resp_body_json() |>
    purrr::pluck("token")
}

# ── Read boundary and build GeoJSON FeatureCollection ─────────────────────────
.boundary_geojson <- function(boundary_file) {
  boundary <- sf::st_read(boundary_file, quiet = TRUE) |>
    sf::st_transform(4326) |>
    sf::st_union() |>
    sf::st_as_sf() |>
    sf::st_cast("MULTIPOLYGON")
  tmp <- tempfile(fileext = ".geojson")
  on.exit(unlink(tmp))
  sf::st_write(boundary, tmp, driver = "GeoJSON", quiet = TRUE, delete_dsn = TRUE)
  jsonlite::fromJSON(tmp, simplifyVector = FALSE)
}

# ── Submit area-extraction task ───────────────────────────────────────────────
.ae_submit_evi <- function(token, boundary_file, park_id, start) {
  geo <- .boundary_geojson(boundary_file)
  task <- list(
    task_type = "area",
    task_name = sprintf("trailpulse_evi_%s", park_id),
    params = list(
      dates = list(list(
        startDate = format(as.Date(start), "%m-%d-%Y"),
        endDate   = format(Sys.Date(),     "%m-%d-%Y")
      )),
      layers = list(
        list(product = "HLSS30_VI.020", layer = "EVI"),
        list(product = "HLSS30.020",    layer = "Fmask")
      ),
      geo    = geo,
      output = list(
        format     = list(type = "geotiff"),
        projection = "geographic"
      )
    )
  )
  httr2::request(paste0(.AE_BASE, "/task")) |>
    httr2::req_auth_bearer_token(token) |>
    httr2::req_body_json(task) |>
    httr2::req_perform() |>
    httr2::resp_body_json() |>
    purrr::pluck("task_id")
}

# ── Check task status ─────────────────────────────────────────────────────────
.ae_status <- function(token, task_id) {
  httr2::request(paste0(.AE_BASE, "/task/", task_id)) |>
    httr2::req_auth_bearer_token(token) |>
    httr2::req_perform() |>
    httr2::resp_body_json() |>
    purrr::pluck("status")
}

# ── Extract date from AppEEARS area TIF filename (doyYYYYDDD) ─────────────────
.parse_doy_date <- function(fname) {
  m <- regmatches(fname, regexpr("doy(\\d{4})(\\d{3})", fname))
  if (length(m) == 0) return(as.Date(NA))
  as.Date(paste0(substr(m, 4, 7), "-", substr(m, 8, 10)), format = "%Y-%j")
}

# ── Download new TIFFs and compute mean EVI for dates >= since ───────────────
# `since` limits which TIFFs get processed (avoids reprocessing all history).
# TIFFs already on disk are never re-downloaded regardless of `since`.
.ae_download_evi <- function(token, task_id, raster_dir,
                             since = as.Date("2000-01-01")) {
  dir.create(raster_dir, showWarnings = FALSE, recursive = TRUE)

  files <- httr2::request(paste0(.AE_BASE, "/bundle/", task_id)) |>
    httr2::req_auth_bearer_token(token) |>
    httr2::req_perform() |>
    httr2::resp_body_json() |>
    purrr::pluck("files")

  # Keep only GeoTIFF data files
  tifs <- purrr::keep(
    files,
    ~grepl("\\.tif$", .x$file_name, ignore.case = TRUE) &&
      !grepl("request|statistics|browse", .x$file_name, ignore.case = TRUE)
  )

  # Download any TIFFs not already on disk
  purrr::walk(tifs, function(f) {
    dest <- file.path(raster_dir, basename(f$file_name))
    if (!file.exists(dest)) {
      resp <- httr2::request(
          paste0(.AE_BASE, "/bundle/", task_id, "/", f$file_id)
        ) |>
        httr2::req_auth_bearer_token(token) |>
        httr2::req_perform()
      writeBin(httr2::resp_body_raw(resp), dest)
    }
  })

  # Pair EVI and Fmask files by date; only process dates >= since
  all_tifs    <- list.files(raster_dir, pattern = "\\.tif$", full.names = TRUE)
  evi_files   <- all_tifs[grepl("EVI",   basename(all_tifs))]
  fmask_files <- all_tifs[grepl("Fmask", basename(all_tifs))]

  evi_dates   <- as.Date(sapply(basename(evi_files),   .parse_doy_date))
  fmask_dates <- as.Date(sapply(basename(fmask_files), .parse_doy_date))

  # Restrict to new dates only
  keep_idx  <- which(!is.na(evi_dates) & evi_dates >= since)
  if (length(keep_idx) == 0) return(tibble::tibble())
  evi_files <- evi_files[keep_idx]
  evi_dates <- evi_dates[keep_idx]

  purrr::map_dfr(seq_along(evi_files), function(i) {
    date  <- evi_dates[i]
    if (is.na(date)) return(tibble::tibble())

    r_evi    <- terra::rast(evi_files[i])
    evi_vals <- as.integer(terra::values(r_evi, na.rm = FALSE)[, 1])

    # EVI fill values: 28672 (HLS standard) and -9999; valid raw range -3000–10000
    valid <- !is.na(evi_vals) &
             evi_vals != 28672L &
             evi_vals != -9999L &
             evi_vals > -3000L &
             evi_vals < 10001L

    # Apply Fmask cloud/shadow mask (bits 1-4; clear = 0)
    fm_idx <- which(fmask_dates == date)
    if (length(fm_idx) == 1) {
      fm_vals <- as.integer(terra::values(terra::rast(fmask_files[fm_idx]),
                                          na.rm = FALSE)[, 1])
      clear   <- !is.na(fm_vals) & (bitwAnd(fm_vals, 30L) == 0L)
      valid   <- valid & clear
    }

    good <- evi_vals[valid] * 0.0001   # apply HLS scale factor
    if (length(good) == 0) return(tibble::tibble())

    tibble::tibble(
      date     = date,
      evi      = mean(good, na.rm = TRUE),
      n_pixels = sum(valid)
    )
  }) |>
    dplyr::filter(!is.na(evi), evi >= -0.2, evi <= 1.0) |>
    dplyr::arrange(date)
}

# ── Public interface ──────────────────────────────────────────────────────────
fetch_evi <- function(
    boundary_file  = PARK_BOUNDARY,
    park_id        = PARK_ID,
    start          = "2015-11-28",   # HLSS30_VI.020 start date
    cache_dir      = "data/cache",
    rds_max_age_h  = 24              # refresh daily (HLS is a daily product)
) {
  if (is.null(boundary_file) || !file.exists(boundary_file)) {
    message("EVI: boundary file not found (", boundary_file %||% "NULL", ") — skipping")
    return(NULL)
  }

  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  rds_file   <- file.path(cache_dir, sprintf("evi_%s.rds",       park_id))
  task_file  <- file.path(cache_dir, sprintf("evi_task_%s.json",  park_id))
  raster_dir <- file.path(cache_dir, "evi_rasters")

  # ── 1. Return fresh cached results ─────────────────────────────────────────
  cached <- if (file.exists(rds_file)) readRDS(rds_file) else NULL
  if (!is.null(cached)) {
    age_h <- as.numeric(difftime(Sys.time(), file.mtime(rds_file), units = "hours"))
    if (age_h < rds_max_age_h) return(cached)
  }

  # ── 2. Determine incremental start date ────────────────────────────────────
  # On first run: fetch full history from `start`.
  # On updates: only request dates after the last observation already cached.
  update_start <- if (!is.null(cached) && nrow(cached) > 0) {
    max(cached$date) + 1L
  } else {
    as.Date(start)
  }

  # If the cache already covers through today, just refresh the mtime and return.
  if (update_start > Sys.Date()) {
    Sys.setFileTime(rds_file, Sys.time())
    return(cached)
  }

  # ── 3. Authenticate ─────────────────────────────────────────────────────────
  token <- tryCatch(.ae_token(), error = function(e) {
    warning("AppEEARS auth failed: ", conditionMessage(e)); NULL
  })
  if (is.null(token)) {
    if (!is.null(cached)) { warning("Using stale EVI cache"); return(cached) }
    return(NULL)
  }

  # ── 4. Load existing task ID or submit a new incremental task ───────────────
  task_id <- NULL
  if (file.exists(task_file)) {
    task_meta <- tryCatch(jsonlite::read_json(task_file), error = function(e) NULL)
    # Only reuse task if it covers the same update window
    if (!is.null(task_meta) &&
        identical(task_meta$update_start, as.character(update_start))) {
      task_id <- task_meta$task_id
    } else {
      unlink(task_file)   # stale task from a different window — discard
    }
  }

  if (is.null(task_id)) {
    task_id <- tryCatch(
      .ae_submit_evi(token, boundary_file, park_id, update_start),
      error = function(e) {
        warning("AppEEARS task submit failed: ", conditionMessage(e)); NULL
      }
    )
    if (!is.null(task_id)) {
      jsonlite::write_json(
        list(task_id     = task_id,
             update_start = as.character(update_start),
             submitted    = as.character(Sys.time())),
        task_file, auto_unbox = TRUE
      )
      message("AppEEARS EVI task submitted: ", task_id,
              " (", update_start, " → today)",
              " — EVI will update after task completes (~5–20 min).")
    }
  }

  if (is.null(task_id)) {
    if (!is.null(cached)) return(cached)
    return(NULL)
  }

  # ── 5. Poll task status ─────────────────────────────────────────────────────
  status <- tryCatch(.ae_status(token, task_id), error = function(e) "unknown")

  if (status %in% c("expired", "deleted", "error")) {
    message("AppEEARS task ", task_id, " status: ", status, " — resubmitting")
    unlink(task_file)
    task_id <- tryCatch(
      .ae_submit_evi(token, boundary_file, park_id, update_start),
      error = function(e) NULL
    )
    if (!is.null(task_id))
      jsonlite::write_json(
        list(task_id      = task_id,
             update_start = as.character(update_start),
             submitted    = as.character(Sys.time())),
        task_file, auto_unbox = TRUE
      )
    status <- "pending"
  }

  if (status != "done") {
    message("AppEEARS task ", task_id, " is ", status,
            " — EVI panel will update on next render after completion")
    if (!is.null(cached)) return(cached)
    return(NULL)
  }

  # ── 6. Download new TIFFs, compute means for new dates only, append ─────────
  new_rows <- tryCatch(
    .ae_download_evi(token, task_id, raster_dir, since = update_start),
    error = function(e) {
      warning("AppEEARS EVI download failed: ", conditionMessage(e)); NULL
    }
  )

  if (is.null(new_rows)) {
    if (!is.null(cached)) return(cached)
    return(NULL)
  }

  # Merge new rows with existing cache, deduplicate, sort
  result <- dplyr::bind_rows(cached, new_rows) |>
    dplyr::distinct(date, .keep_all = TRUE) |>
    dplyr::arrange(date)

  if (nrow(result) == 0) {
    if (!is.null(cached)) return(cached)
    return(NULL)
  }

  saveRDS(result, rds_file)
  unlink(task_file)   # cleared; next render will submit a fresh incremental task
  result
}
