# R/taphonomy.R, the network-facing and optional-backend parts: the MorphoSource client with
# the transport replaced by recorders, the posterior summary from draws, and the Stan
# backends' refusals when their packages are absent.

.tp_pkg <- if (isNamespaceLoaded("rmorie")) "rmorie" else "morie"
.tp <- function(name) get(name, envir = environment(morie_taphonomy_bhm))

test_that("the MorphoSource client builds its query, sends the key only when it has one, and reads the reply", {
  withr::local_envvar(MORPHOSOURCE_API_URL = NA, MORPHOSOURCE_API_KEY = NA)
  expect_equal(.tp(".morie_morphosource_api")(), "https://www.morphosource.org/api")
  withr::local_envvar(MORPHOSOURCE_API_URL = "https://ms.example/api")
  expect_equal(.tp(".morie_morphosource_api")(), "https://ms.example/api")
  key <- .tp(".morie_morphosource_key")
  expect_null(key(NULL, required = FALSE))
  expect_error(key(NULL, required = TRUE), "MorphoSource API key required")
  expect_equal(key("abc"), "abc")
  withr::local_envvar(MORPHOSOURCE_API_KEY = "fromenv")
  expect_equal(key(NULL), "fromenv")
  expect_equal(key(""), "fromenv")
  sp <- .tp(".morie_morphosource_search_params")
  expect_equal(sp(), list(per_page = 10L, page = 1L))
  expect_equal(sp("femur", media_type = "Mesh", taxonomy_gbif = "Homo", visibility = "open", media_tag = "ct",
                  per_page = 5, page = 2),
               list(q = "femur", search_field = "all_fields", f.media_type = "Mesh", f.taxonomy_gbif = "Homo",
                    f.publication_status = "open", f.tag = "ct", per_page = 5L, page = 2L))
  rec <- new.env()
  testthat::local_mocked_bindings(
    .morie_dataset_http_text_with_status = function(url, query = NULL, headers = character(), ...) {
      rec$url <- url
      rec$query <- query
      rec$headers <- headers
      rec$reply
    },
    .package = .tp_pkg)
  rec$reply <- list(status_code = 200L, body = paste0(
    "{\"response\":{\"media\":[{\"id\":\"000123\",\"title\":\"Femur\"},{\"id\":\"000124\"}],",
    "\"pages\":{\"total_pages\":7}}}"))
  r <- morie_taphonomy_morphosource_search("femur", per_page = 2)
  expect_equal(rec$url, "https://ms.example/api/media")
  expect_equal(rec$query$q, "femur")
  expect_equal(rec$headers, "Authorization: fromenv")
  expect_equal(r$n, 2L)
  expect_equal(r$total_pages, 7L)
  expect_equal(r$df, data.frame(id = c("000123", "000124"), title = c("Femur", NA), stringsAsFactors = FALSE))
  withr::local_envvar(MORPHOSOURCE_API_KEY = NA)
  rec$reply <- list(status_code = 200L, body = "{\"response\":{\"physical_objects\":null}}")
  r2 <- morie_taphonomy_morphosource_search(type = "physical-objects")
  expect_equal(rec$url, "https://ms.example/api/physical-objects")
  expect_equal(rec$headers, character())
  expect_equal(r2$n, 0L)
  expect_true(is.na(r2$total_pages))
  rec$reply <- list(status_code = 500L, body = "")
  expect_error(morie_taphonomy_morphosource_search("x"), "MorphoSource search failed \\(HTTP 500\\)")
})

test_that("a MorphoSource download needs a use statement and a key, then writes the archive it is sent", {
  withr::local_envvar(MORPHOSOURCE_API_URL = "https://ms.example/api", MORPHOSOURCE_API_KEY = NA)
  expect_error(morie_taphonomy_morphosource_fetch("000123"), "non-empty `use_statement`")
  expect_error(morie_taphonomy_morphosource_fetch("000123", ""), "non-empty `use_statement`")
  expect_error(morie_taphonomy_morphosource_fetch("000123", "research"), "MorphoSource API key required")
  rec <- new.env()
  testthat::local_mocked_bindings(
    .morie_dataset_http_post_json_with_status = function(url, body, query = NULL, headers = character(), ...) {
      rec$url <- url
      rec$body <- body
      rec$headers <- headers
      rec$reply
    },
    .morie_dataset_http_bytes = function(url, query = NULL, headers = character(), ...) {
      rec$dl <- url
      rec$dl_headers <- headers
      as.raw(c(0x50, 0x4b, 0x03, 0x04))
    },
    .package = .tp_pkg)
  dest <- file.path(withr::local_tempdir(), "out")
  rec$reply <- list(status_code = 200L, body = "{\"response\":{\"media\":{\"download_url\":\"https://files.example/a.zip\"}}}")
  p <- morie_taphonomy_morphosource_fetch("000123", "research use", use_categories = c("teaching"),
                                          use_category_other = "none", dest = dest, api_key = "k1")
  expect_equal(p, file.path(dest, "morphosource_000123.zip"))
  expect_equal(readBin(p, "raw", 10), as.raw(c(0x50, 0x4b, 0x03, 0x04)))
  expect_equal(rec$url, "https://ms.example/api/download/000123")
  expect_equal(rec$body, list(use_statement = "research use", agreements_accepted = TRUE, use_categories = "teaching",
                              use_category_other = "none"))
  expect_equal(rec$headers, "Authorization: k1")
  expect_equal(rec$dl, "https://files.example/a.zip")
  expect_equal(rec$dl_headers, "Authorization: k1")
  rec$reply <- list(status_code = 403L, body = "")
  expect_error(morie_taphonomy_morphosource_fetch("000123", "r", api_key = "k1"), "Restricted media: request download permission")
  rec$reply <- list(status_code = 404L, body = "")
  expect_error(morie_taphonomy_morphosource_fetch("000123", "r", api_key = "k1"), "download request failed \\(HTTP 404\\)")
  rec$reply <- list(status_code = 200L, body = "{\"response\":{\"media\":{}}}")
  expect_error(morie_taphonomy_morphosource_fetch("000123", "r", api_key = "k1"), "returned no download_url")
})

test_that("the posterior summary from draws recomputes, and the Stan backends refuse without their packages", {
  set.seed(1)
  bmat <- cbind(a = stats::rnorm(500, 1), b = stats::rnorm(500, -0.2))
  cf <- .tp(".morie_bhm_coefs_from_draws")(bmat, c("a", "b"))
  expect_equal(cf$term, c("a", "b"))
  expect_equal(cf$post_mean, unname(colMeans(bmat)))
  expect_equal(cf$post_sd, unname(apply(bmat, 2, stats::sd)))
  expect_equal(cf$ci_lower, unname(apply(bmat, 2, stats::quantile, 0.025)))
  expect_equal(cf$ci_upper, unname(apply(bmat, 2, stats::quantile, 0.975)))
  expect_equal(cf$prob_positive, unname(colMeans(bmat > 0)))
  testthat::local_mocked_bindings(requireNamespace = function(package, ...) FALSE, .package = "base")
  expect_error(.tp(".morie_bhm_cmdstanr")(matrix(1, 2, 1), 1:2, "x", 0, 1, NULL, NULL, 1L, 10L, 1L),
               "needs the 'cmdstanr' package")
  expect_error(.tp(".morie_bhm_formula_stan")("brms", data.frame(y = 1, x = 1), "y", "x", NULL, 0, 1, "x", 1L, 10L, 1L),
               "backend = 'brms' needs the 'brms' package")
  expect_error(.tp(".morie_bhm_formula_stan")("rstanarm", data.frame(y = 1, x = 1), "y", "x", NULL, 0, 1, "x", 1L, 10L, 1L),
               "backend = 'rstanarm' needs the 'rstanarm' package")
})

test_that("the conjugate preservation model recomputes from its closed form, with priors and pooled groups", {
  set.seed(3)
  n <- 60
  d <- data.frame(preservation_score = stats::rnorm(n), lime_treatment = stats::rbinom(n, 1, 0.5),
                  ph = stats::runif(n, 5, 8), site = sample(c("a", "b", "c"), n, TRUE), stringsAsFactors = FALSE)
  d$preservation_score <- 2 + 1.5 * d$lime_treatment - 0.3 * d$ph + stats::rnorm(n, sd = 0.5)
  expect_error(morie_taphonomy_bhm(list()), "must be a data.frame")
  expect_error(morie_taphonomy_bhm(d[0, ]), "is empty")
  expect_error(morie_taphonomy_bhm(d, outcome = "nope"), "column 'nope' not found")
  expect_error(morie_taphonomy_bhm(d[, "preservation_score", drop = FALSE], covariates = "preservation_score"),
               "no covariates available")
  pri <- list(lime_treatment = list(mean = 1, sd = 0.5), ignored = list(mean = 9, sd = 9))
  fit <- morie_taphonomy_bhm(d, covariates = c("lime_treatment", "ph"), group = "site", priors = pri,
                             prior_sd_default = 4)
  X <- cbind(1, d$lime_treatment, d$ph)
  y <- d$preservation_score
  ols <- stats::lm.fit(X, y)
  sigma2 <- sum(ols$residuals^2) / (n - 3)
  m0 <- c(0, 1, 0)
  s0 <- c(4, 0.5, 4)
  L0 <- diag(1 / s0^2)
  Sigma <- solve(crossprod(X) / sigma2 + L0)
  mu <- as.numeric(Sigma %*% (crossprod(X, y) / sigma2 + L0 %*% m0))
  expect_equal(fit$coefficients$term, c("(Intercept)", "lime_treatment", "ph"))
  expect_equal(fit$coefficients$post_mean, mu)
  expect_equal(fit$coefficients$post_sd, sqrt(diag(Sigma)))
  expect_equal(fit$coefficients$ci_lower, mu - 1.959964 * sqrt(diag(Sigma)))
  expect_equal(fit$coefficients$prob_positive, stats::pnorm(mu / sqrt(diag(Sigma))))
  expect_equal(fit$sigma, sqrt(sigma2))
  expect_equal(fit$fitted, as.numeric(X %*% mu))
  expect_equal(fit$n, n)
  expect_equal(fit$backend, "conjugate (closed form)")
  resid <- y - fit$fitted
  g <- factor(d$site)
  gm <- tapply(resid, g, mean)
  nj <- tapply(resid, g, length)
  tau2 <- max(0, stats::var(gm) - mean(sigma2 / nj))
  lambda <- tau2 / (tau2 + sigma2 / nj)
  expect_equal(fit$group_effects$group, c("a", "b", "c"))
  expect_equal(fit$group_effects$raw_mean, as.numeric(gm))
  expect_equal(fit$group_effects$shrinkage, as.numeric(lambda))
  expect_equal(fit$group_effects$pooled_intercept, as.numeric(lambda * gm))
  expect_equal(fit$group_effects$n, as.integer(nj))
  expect_match(fit$interpretation, "3 group intercepts partially pooled")
  expect_match(fit$interpretation, "Posterior effect of 'lime_treatment'")
  # the default covariate set comes from the schema; a character covariate is coded as a factor
  d2 <- d
  d2$lime_treatment <- ifelse(d2$lime_treatment == 1, "yes", "no")
  fit2 <- morie_taphonomy_bhm(d2, covariates = c("lime_treatment", "ph"))
  Xf <- cbind(1, as.numeric(as.factor(d2$lime_treatment)), d2$ph)
  expect_equal(fit2$fitted, as.numeric(Xf %*% fit2$coefficients$post_mean))
  expect_null(fit2$group_effects)
  # the Stan backends are dispatched with the same design and priors
  seen <- NULL
  testthat::local_mocked_bindings(
    .morie_bhm_cmdstanr = function(X, y, terms, m0, s0, gfac, group, chains, iter, seed) {
      seen <<- list(terms = terms, m0 = m0, s0 = s0, gfac = gfac, chains = chains)
      list(backend = "cmdstanr (NUTS)")
    },
    .morie_bhm_formula_stan = function(backend, frame, outcome, covariates, group, m0, s0, terms, chains, iter, seed) {
      seen <<- list(backend = backend, outcome = outcome, covariates = covariates, m0 = m0)
      list(backend = backend)
    },
    .package = .tp_pkg)
  r <- morie_taphonomy_bhm(d, covariates = c("lime_treatment", "ph"), group = "site", priors = pri,
                           backend = "cmdstanr", chains = 2L)
  expect_equal(r$backend, "cmdstanr (NUTS)")
  expect_equal(unname(seen$m0), c(0, 1, 0))
  expect_equal(unname(seen$s0), c(10, 0.5, 10))
  expect_equal(seen$chains, 2L)
  expect_equal(nlevels(seen$gfac), 3L)
  r2 <- morie_taphonomy_bhm(d, covariates = "ph", backend = "brms")
  expect_equal(r2$backend, "brms")
  expect_equal(seen$covariates, "ph")
  expect_equal(seen$outcome, "preservation_score")
})

test_that("the USGS soil archive is downloaded once and its CSV member read", {
  skip_if(!nzchar(Sys.which("zip")), "no zip binary")
  dest <- withr::local_tempdir()
  src <- withr::local_tempdir()
  utils::write.csv(data.frame(`Site ID` = 1:5, ph = seq(5, 7, length.out = 5), check.names = FALSE),
                   file.path(src, "ngdb_soil.csv"), row.names = FALSE)
  writeLines("readme", file.path(src, "README.txt"))
  zipfile <- file.path(src, "ngdbsoil.zip")
  withr::with_dir(src, utils::zip(zipfile, c("ngdb_soil.csv", "README.txt"), flags = "-q"))
  calls <- 0L
  testthat::local_mocked_bindings(
    .morie_download = function(url, ext = "", ...) {
      calls <<- calls + 1L
      f <- tempfile(fileext = ".zip")
      file.copy(zipfile, f)
      f
    },
    .package = .tp_pkg)
  df <- morie_taphonomy_fetch_usgs_soil(dest = file.path(dest, "soil"), nrows = 3L, url = "https://x.example/ngdbsoil.zip")
  expect_equal(nrow(df), 3L)
  expect_equal(names(df), c("Site ID", "ph"))
  expect_equal(attr(df, "source"), "https://x.example/ngdbsoil.zip")
  expect_true(file.exists(file.path(dest, "soil", "ngdbsoil.zip")))
  expect_equal(calls, 1L)
  full <- morie_taphonomy_fetch_usgs_soil(dest = file.path(dest, "soil"), nrows = NULL, url = "https://x.example/ngdbsoil.zip")
  expect_equal(nrow(full), 5L)
  expect_equal(calls, 1L)
  morie_taphonomy_fetch_usgs_soil(dest = file.path(dest, "soil"), url = "https://x.example/ngdbsoil.zip", refresh = TRUE)
  expect_equal(calls, 2L)
  withr::with_dir(src, utils::zip(file.path(src, "nocsv.zip"), "README.txt", flags = "-q"))
  expect_error(.tp(".morie_read_usgs_soil_zip")(file.path(src, "nocsv.zip")), "no CSV member found in nocsv.zip")
})
