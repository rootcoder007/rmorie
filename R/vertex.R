# SPDX-License-Identifier: AGPL-3.0-or-later
#
# vertex.R -- Vertex AI Gemini client (small-dep path).
#
# R port of src/morie/vertex.py. Mirrors the lightweight pure-HTTP
# approach: shell out to `gcloud auth print-access-token` to mint an
# access token, then POST to the Vertex AI `:generateContent` REST
# endpoint via `httr2`. No `google-cloud-aiplatform` SDK dependency.
#
# Configure with:
#   - GOOGLE_APPLICATION_CREDENTIALS -- service-account JSON path
#   - GOOGLE_CLOUD_PROJECT (or MORIE_EE_PROJECT) -- project id
#   - VERTEX_LOCATION (optional) -- default us-central1
#   - VERTEX_MODEL    (optional) -- default gemini-2.5-flash

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}

#' Resolve Vertex AI configuration from environment variables
#' @return Named list: project / location / model / token_ttl_s / gcloud_path.
#' @export
morie_vertex_resolve_config <- function() {
  project <- Sys.getenv("GOOGLE_CLOUD_PROJECT", unset = "")
  if (!nzchar(project)) project <- Sys.getenv("MORIE_EE_PROJECT", unset = "")
  if (!nzchar(project)) {
    stop("Vertex requires GOOGLE_CLOUD_PROJECT (or MORIE_EE_PROJECT). ",
         "Set via Sys.setenv(GOOGLE_CLOUD_PROJECT = '<project-id>').")
  }
  gcloud <- Sys.getenv("GCLOUD_PATH", unset = "gcloud")
  candidates <- c(gcloud,
                  "/mnt/nvme/google-cloud-sdk/bin/gcloud",
                  "/opt/google-cloud-sdk/bin/gcloud")
  for (c in candidates) {
    if (file.exists(c) || nzchar(Sys.which(c))) {
      gcloud <- c
      break
    }
  }
  list(
    project = project,
    location = Sys.getenv("VERTEX_LOCATION", unset = "us-central1"),
    model    = Sys.getenv("VERTEX_MODEL",    unset = "gemini-2.5-flash"),
    token_ttl_s = 3300L,
    gcloud_path = gcloud
  )
}

# Process-lifetime token cache.
.morie_vertex_token_cache <- new.env(parent = emptyenv())
.morie_vertex_token_cache$token      <- NULL
.morie_vertex_token_cache$expires_at <- 0

#' Fetch and cache a Google Cloud access token via gcloud
#' @param cfg Config list, or NULL to resolve.
#' @return Character bearer token.
#' @export
morie_vertex_access_token <- function(cfg = NULL) {
  if (is.null(cfg)) cfg <- morie_vertex_resolve_config()
  now <- as.numeric(Sys.time())
  cached <- .morie_vertex_token_cache$token
  if (!is.null(cached) && now < .morie_vertex_token_cache$expires_at) {
    return(cached)
  }
  out <- tryCatch(
    system2(cfg$gcloud_path, c("auth", "print-access-token"),
            stdout = TRUE, stderr = TRUE),
    error = function(e) {
      stop(sprintf("gcloud not found at %s. Set GCLOUD_PATH env var.",
                   cfg$gcloud_path))
    })
  status <- attr(out, "status")
  if (!is.null(status) && status != 0L) {
    stop(sprintf("gcloud print-access-token failed: %s",
                 paste(out, collapse = " ")))
  }
  token <- trimws(paste(out, collapse = ""))
  if (!nzchar(token)) stop("gcloud returned empty access token.")
  .morie_vertex_token_cache$token <- token
  .morie_vertex_token_cache$expires_at <- now + cfg$token_ttl_s
  token
}

#' Send a single-turn prompt to Gemini via Vertex AI
#'
#' R port of `morie.vertex.ask_gemini`. POSTs to the Vertex AI REST
#' endpoint `:generateContent` and returns the concatenated text from
#' the first candidate.
#'
#' @param prompt Character scalar -- the user prompt.
#' @param model Optional Gemini model override.
#' @param system Optional system instruction.
#' @param temperature Numeric. Default 0.1.
#' @param max_output_tokens Integer. Default 2048.
#' @param timeout_s Numeric HTTP timeout. Default 120.
#' @param cfg Pre-resolved config list, or NULL to auto-resolve.
#' @return Character scalar -- trimmed generated text.
#' @export
morie_vertex_ask_gemini <- function(prompt, model = NULL, system = NULL,
                                    temperature = 0.1,
                                    max_output_tokens = 2048L,
                                    timeout_s = 120,
                                    cfg = NULL) {
  if (!requireNamespace("httr2", quietly = TRUE) ||
      !requireNamespace("jsonlite", quietly = TRUE)) {
    stop("morie_vertex_ask_gemini requires httr2 and jsonlite.")
  }
  if (is.null(cfg)) cfg <- morie_vertex_resolve_config()
  if (is.null(model)) model <- cfg$model
  token <- morie_vertex_access_token(cfg)
  endpoint <- sprintf(
    "https://%s-aiplatform.googleapis.com/v1/projects/%s/locations/%s/publishers/google/models/%s:generateContent",
    cfg$location, cfg$project, cfg$location, model)

  payload <- list(
    contents = list(
      list(role = "user", parts = list(list(text = prompt)))
    ),
    generationConfig = list(
      temperature = as.numeric(temperature),
      maxOutputTokens = as.integer(max_output_tokens)
    )
  )
  if (!is.null(system) && nzchar(system)) {
    payload$systemInstruction <- list(parts = list(list(text = system)))
  }

  req <- httr2::request(endpoint)
  req <- httr2::req_headers(req,
    Authorization = paste("Bearer", token),
    `Content-Type` = "application/json")
  req <- httr2::req_body_raw(req,
    jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null"),
    type = "application/json")
  req <- httr2::req_timeout(req, timeout_s)
  req <- httr2::req_error(req, is_error = function(resp) FALSE)
  resp <- httr2::req_perform(req)
  status <- httr2::resp_status(resp)
  body <- httr2::resp_body_string(resp)
  if (status != 200L) {
    stop(sprintf("Vertex API returned %d: %s",
                 status, substr(body, 1L, 400L)))
  }
  data <- jsonlite::fromJSON(body, simplifyVector = FALSE)
  parts <- tryCatch(data$candidates[[1]]$content$parts,
                    error = function(e) NULL)
  if (is.null(parts)) {
    stop(sprintf("unexpected Vertex response shape: %s",
                 substr(body, 1L, 400L)))
  }
  trimws(paste0(
    vapply(parts, function(p) as.character(p$text %||% ""), character(1)),
    collapse = ""))
}

#' Tiny smoke test for the Vertex AI client
#' @return Named list (ok / error / model / project / location / reply).
#' @export
morie_vertex_health_check <- function() {
  out <- list(ok = FALSE, error = NULL, model = NULL)
  tryCatch({
    cfg <- morie_vertex_resolve_config()
    out$project  <- cfg$project
    out$location <- cfg$location
    out$model    <- cfg$model
    reply <- morie_vertex_ask_gemini(
      "reply with just OK and nothing else",
      cfg = cfg, temperature = 0, max_output_tokens = 8L)
    out$reply <- reply
    out$ok <- nzchar(reply)
  }, error = function(e) {
    out$error <<- sprintf("%s: %s", class(e)[1], conditionMessage(e))
  })
  out
}
