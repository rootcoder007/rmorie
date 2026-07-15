# SPDX-License-Identifier: AGPL-3.0-or-later
# Central mock registry. Rules (see data-raw + the 2026-07-15 test brief):
#   Layer 1: real package installed            -> the real test runs
#   Layer 2: package missing, mock registered  -> mock-backed test runs
#   Layer 3: package missing, no mock          -> skip with a clear reason
# Mocks live ONLY here; they mirror the REAL return shapes (including S3
# classes) so expect_s3_class()/expect_named() assertions stay honest.
# Never mock what CI installs for real - transport-layer mocks below fake
# only the network round-trip; parsers and type coercion run for real.

# ---------------------------------------------------------------------------
# Dispatcher: run `body` with the real package when present; otherwise bind
# the registered mock over the rmorie-internal binding `real_fn`; otherwise
# skip. `local_mocked_bindings` scopes the rebinding to the calling test.
with_mocked_or_real <- function(pkg, real_fn, body, env = parent.frame()) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    return(force(body))
  }
  factory <- mock_registry[[real_fn]]
  if (is.null(factory)) {
    testthat::skip(sprintf("`%s` not installed and no mock for `%s`",
                           pkg, real_fn))
  }
  testthat::local_mocked_bindings(
    .list = stats::setNames(list(factory()), real_fn),
    .package = "rmorie",
    .env = env
  )
  force(body)
}

# ---------------------------------------------------------------------------
# Transport-layer canned responses. rmorie's network access goes through the
# private wrappers .morie_read_text(url) / .morie_download(url, ext), so
# rebinding those exercises every parser for real without a socket.
canned_bodies <- list(
  ckan_package_search = paste0(
    '{"success": true, "result": {"count": 1, "results": [',
    '{"id": "abc-123", "name": "test-package", "title": "Test Package",',
    ' "resources": [',
    '  {"id": "r1", "name": "data.csv", "format": "csv",',
    '   "datastore_active": true,  "url": "https://example.com/data.csv"},',
    '  {"id": "r2", "name": "notes.txt", "format": "txt",',
    '   "datastore_active": false, "url": "https://example.com/notes.txt"}',
    ']}]}}'
  ),
  arcgis_features = paste0(
    '{"features": [',
    ' {"attributes": {"id": 1, "name": "Alpha"}},',
    ' {"attributes": {"id": 2, "name": "Beta"}}',
    '], "exceededTransferLimit": false}'
  )
)

# Bind .morie_read_text to serve a canned body for the current test only.
local_canned_read_text <- function(body_name, env = parent.frame()) {
  body <- canned_bodies[[body_name]]
  stopifnot(!is.null(body))
  testthat::local_mocked_bindings(
    .morie_read_text = function(url) body,
    .package = "rmorie",
    .env = env
  )
}

# ---------------------------------------------------------------------------
# Statistical-dependency mock factories. Shapes mirror the real objects the
# wrapped packages return; test-mock-shapes.R pins these contracts.
mock_registry <- list(

  # MatchIt::matchit result consumed by morie_matching_* wrappers.
  matchit = function() {
    function(formula, data, ...) {
      n <- nrow(data)
      treat_var <- all.vars(formula)[1]
      tr <- as.integer(data[[treat_var]])
      treat_idx <- which(tr == 1L)
      ctrl_idx <- which(tr == 0L)
      k <- min(length(treat_idx), length(ctrl_idx))
      mm <- matrix(as.character(ctrl_idx[seq_len(k)]),
                   ncol = 1,
                   dimnames = list(as.character(treat_idx[seq_len(k)]), NULL))
      w <- numeric(n)
      w[c(treat_idx[seq_len(k)], ctrl_idx[seq_len(k)])] <- 1
      structure(
        list(match.matrix = mm, weights = w, treat = tr,
             distance = stats::plogis(stats::rnorm(n, 0, 0.5)),
             estimand = "ATT",
             call = match.call(), info = list(method = "nearest"),
             nn = matrix(c(k, k), 1, 2)),
        class = "matchit"
      )
    }
  },

  # survey::svyglm-shaped fit consumed by the ebac IPW pipeline.
  svyglm = function() {
    function(formula, design, ...) {
      cf <- c(`(Intercept)` = 0.1, x1 = 0.3)
      structure(
        list(coefficients = cf,
             vcov = matrix(c(0.01, 0.001, 0.001, 0.01), 2, 2,
                           dimnames = list(names(cf), names(cf))),
             residuals = rep(0, 10),
             fitted.values = rep(0.5, 10),
             rank = 2L, df.residual = 8L,
             call = match.call(), terms = stats::terms(formula)),
        class = c("svyglm", "glm", "lm")
      )
    }
  }
)

get_mock <- function(name) mock_registry[[name]]
