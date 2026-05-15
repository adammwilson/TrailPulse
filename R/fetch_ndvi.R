# R/fetch_ndvi.R
# Fetches HLS NDVI for a park location via NASA AppEEARS async API.
# HLS = Harmonized Landsat 8/9 + Sentinel-2  |  30 m, ~2–3 day revisit.
#
# Workflow (async with persistent cache):
#   1. Fresh RDS cache present  → return immediately.
#   2. No/stale cache           → authenticate, submit AppEEARS point task.
#   3. Task pending/running     → return stale cache (if any) and wait.
#   4. Task done                → download, parse, save RDS, return.
#   5. Task expired/error       → delete task file, resubmit.
#
# Required env vars:
#   EARTHDATA_USER      NASA Earthdata username
#   EARTHDATA_PASSWORD  NASA Earthdata password
#
# HLS coverage: HLSL30 v2.0 starts 2013-04-11; HLSS30 v2.0 starts 2015-11-28.
# Combined revisit is ~2–3 days for most mid-latitude sites.

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

# ── Submit point-extraction task ─────────────────────────────────────────────
.ae_submit <- function(token, lat, lon, start) {
  task <- list(
    task_type = "point",
    task_name = sprintf("trailpulse_ndvi_%.3f_%.3f", lat, lon),
    params = list(
      dates = list(list(
        startDate = format(as.Date(start), "%m-%d-%Y"),
        endDate   = format(Sys.Date(),     "%m-%d-%Y")
      )),
      layers = list(
        # Landsat 8/9 — Red, NIR, Quality
        list(product = "HLSL30.v2.0", layer = "B04"),
        list(product = "HLSL30.v2.0", layer = "B05"),
        list(product = "HLSL30.v2.0", layer = "Fmask"),
        # Sentinel-2 — Red, Narrow NIR, Quality
        list(product = "HLSS30.v2.0", layer = "B04"),
        list(product = "HLSS30.v2.0", layer = "B8A"),
        list(product = "HLSS30.v2.0", layer = "Fmask")
      ),
      coordinates = list(list(
        id        = "park",
        longitude = lon,
        latitude  = lat,
        category  = "park"
      )),
      output = list(
        format     = list(type = "csv"),
        projection = "native"
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

# ── Download bundle CSVs and parse NDVI ──────────────────────────────────────
.ae_download_ndvi <- function(token, task_id) {
  files <- httr2::request(paste0(.AE_BASE, "/bundle/", task_id)) |>
    httr2::req_auth_bearer_token(token) |>
    httr2::req_perform() |>
    httr2::resp_body_json() |>
    purrr::pluck("files")

  # Keep only data CSVs (drop request-summary and stats files)
  data_csvs <- purrr::keep(
    files,
    ~grepl("\\.csv$", .x$file_name, ignore.case = TRUE) &&
      !grepl("request\\.csv$|Statistics\\.csv$", .x$file_name)
  )

  purrr::map_dfr(data_csvs, function(f) {
    # AppEEARS redirects to a presigned S3 URL; httr2 follows transparently.
    txt <- httr2::request(
        paste0(.AE_BASE, "/bundle/", task_id, "/", f$file_id)
      ) |>
      httr2::req_auth_bearer_token(token) |>
      httr2::req_perform() |>
      httr2::resp_body_string()

    df <- readr::read_csv(txt, show_col_types = FALSE)

    is_landsat  <- all(c("B04", "B05", "Fmask") %in% names(df))
    is_sentinel <- all(c("B04", "B8A", "Fmask") %in% names(df))

    if (!is_landsat && !is_sentinel) return(tibble::tibble())

    # Use scalar if/else so only the present NIR column is accessed
    nir_col <- if (is_landsat) df[["B05"]] else df[["B8A"]]
    red_col <- df[["B04"]]

    df |>
      dplyr::mutate(
        .nir   = nir_col,
        date   = as.Date(Date, format = "%m-%d-%Y"),
        sensor = if (is_landsat) "Landsat" else "Sentinel-2",
        # Scale factor (0.0001) cancels in the ratio — use raw integers
        ndvi   = (.nir - B04) / (.nir + B04),
        # Fmask: bits 1-4 = cloud/adj-cloud/shadow/snow  (mask = 30)
        .clear = bitwAnd(as.integer(Fmask), 30L) == 0L
      ) |>
      dplyr::filter(
        !is.na(B04), B04 > 0L, B04 < 10001L,
        !is.na(.nir), .nir > 0L, .nir < 10001L,
        .clear, !is.na(ndvi), ndvi >= -0.2, ndvi <= 1.0
      ) |>
      dplyr::select(date, sensor, ndvi)
  }) |>
    dplyr::arrange(date)
}

# ── Public interface ──────────────────────────────────────────────────────────
fetch_ndvi <- function(
    lat          = PARK_LAT,
    lon          = PARK_LON,
    start        = "2013-04-11",   # HLSL30 v2.0 start date
    cache_dir    = "data/cache",
    rds_max_age_h = 168            # refresh weekly (results change slowly)
) {
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  rds_file  <- file.path(cache_dir, sprintf("ndvi_%.4f_%.4f.rds",       lat, lon))
  task_file <- file.path(cache_dir, sprintf("ndvi_task_%.4f_%.4f.json", lat, lon))

  # ── 1. Return fresh cached results ─────────────────────────────────────────
  if (file.exists(rds_file)) {
    age_h <- as.numeric(difftime(Sys.time(), file.mtime(rds_file), units = "hours"))
    if (age_h < rds_max_age_h) return(readRDS(rds_file))
  }

  # ── 2. Authenticate ─────────────────────────────────────────────────────────
  token <- tryCatch(.ae_token(), error = function(e) {
    warning("AppEEARS auth failed: ", conditionMessage(e)); NULL
  })
  if (is.null(token)) {
    if (file.exists(rds_file)) { warning("Using stale NDVI cache"); return(readRDS(rds_file)) }
    return(NULL)
  }

  # ── 3. Load existing task ID or submit a new task ───────────────────────────
  task_id <- NULL
  if (file.exists(task_file)) {
    task_id <- tryCatch(
      jsonlite::read_json(task_file)$task_id,
      error = function(e) NULL
    )
  }

  if (is.null(task_id)) {
    task_id <- tryCatch(.ae_submit(token, lat, lon, start), error = function(e) {
      warning("AppEEARS task submit failed: ", conditionMessage(e)); NULL
    })
    if (!is.null(task_id)) {
      jsonlite::write_json(
        list(task_id = task_id, submitted = as.character(Sys.time())),
        task_file, auto_unbox = TRUE
      )
      message("AppEEARS task submitted: ", task_id,
              " — NDVI plot will appear after task completes (~5–20 min).")
    }
  }

  if (is.null(task_id)) {
    if (file.exists(rds_file)) return(readRDS(rds_file))
    return(NULL)
  }

  # ── 4. Poll task status ─────────────────────────────────────────────────────
  status <- tryCatch(.ae_status(token, task_id), error = function(e) "unknown")

  if (status %in% c("expired", "deleted", "error")) {
    message("AppEEARS task ", task_id, " status: ", status, " — resubmitting")
    unlink(task_file)
    task_id <- tryCatch(.ae_submit(token, lat, lon, start), error = function(e) NULL)
    if (!is.null(task_id))
      jsonlite::write_json(
        list(task_id = task_id, submitted = as.character(Sys.time())),
        task_file, auto_unbox = TRUE
      )
    status <- "pending"
  }

  if (status != "done") {
    message("AppEEARS task ", task_id, " is ", status,
            " — NDVI panel will populate on next render after completion")
    if (file.exists(rds_file)) return(readRDS(rds_file))
    return(NULL)
  }

  # ── 5. Download, parse, and cache ───────────────────────────────────────────
  result <- tryCatch(.ae_download_ndvi(token, task_id), error = function(e) {
    warning("AppEEARS download failed: ", conditionMessage(e)); NULL
  })

  if (is.null(result) || nrow(result) == 0) {
    if (file.exists(rds_file)) return(readRDS(rds_file))
    return(NULL)
  }

  saveRDS(result, rds_file)
  unlink(task_file)   # Fresh task will be submitted when cache next expires
  result
}
