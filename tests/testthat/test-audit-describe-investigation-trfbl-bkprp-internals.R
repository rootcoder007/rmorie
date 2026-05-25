# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 2XX: tests for internal helpers across audit_variables.R,
# arsau_analyze.R, describe.R, investigation.R, vrgft.R, trfbl.R,
# bkprp.R, tgrch.R, ingest_tps.R, mrm_primitives_ordinal.R, tps_csi.R,
# and database.R.

# ============================================================== audit_variables.R

test_that(".classify_schema_R returns list-of-taxonomies for a schema", {
  schema <- list(columns = list(
    list(name = "Record_ID", dtype = "int"),
    list(name = "name",     dtype = "string"),
    list(name = "days",     dtype = "int")))
  out <- morie:::.classify_schema_R(schema, dataset_name = "demo")
  expect_type(out, "list")
  expect_length(out, 3L)
})

test_that(".summarise_taxonomies returns counts + flag lists", {
  taxonomies <- list(
    list(column_name = "id",  level = "identifier",
         role = "identifier", cardinality = "binary",
         cross_year_safe = TRUE),
    list(column_name = "y",   level = "ratio",
         role = "outcome", cardinality = "discrete_low",
         cross_year_safe = FALSE),
    list(column_name = "x1",  level = "nominal",
         role = "covariate", cardinality = "discrete_medium",
         cross_year_safe = TRUE))
  out <- morie:::.summarise_taxonomies(taxonomies,
                                         analyzed_set = c("y"),
                                         domain = "test")
  expect_type(out, "list")
})

# =============================================================== arsau_analyze.R

test_that(".morie_arsau_wrap aggregates sub-results into a list", {
  sub_results <- list(
    summary = list(warnings = character(0)),
    timing  = list(warnings = "minor"))
  out <- morie:::.morie_arsau_wrap(
    title = "T", call = "demo",
    sub_results = sub_results,
    data = data.frame(x = 1:3), sidecar = list(),
    year_or_range = "2024", kind = "main_records",
    language = "en", is_valid = TRUE)
  expect_type(out, "list")
})

# ==================================================================== describe.R

test_that(".morie_normalise_describe_name strips morie_ / describe_ / .md", {
  expect_equal(
    morie:::.morie_normalise_describe_name("morie_alpha"), "alpha")
  expect_equal(
    morie:::.morie_normalise_describe_name("describe_alpha.md"), "alpha")
  expect_equal(
    morie:::.morie_normalise_describe_name("alpha"), "alpha")
})

test_that(".morie_load_describe_corpus returns a non-NULL object (or errors when bundle absent)", {
  out <- tryCatch(morie:::.morie_load_describe_corpus(),
                  error = function(e) e)
  if (inherits(out, "error")) {
    skip(sprintf("describe corpus missing in dev tree: %s",
                 conditionMessage(out)))
  }
  expect_false(is.null(out))
  expect_true(length(out) > 0L)
})

# ================================================================ investigation.R

test_that(".morie_hajek_ate returns ate + y1 + y0 list", {
  set.seed(1L); n <- 200L
  t <- stats::rbinom(n, 1L, 0.5)
  ps <- rep(0.5, n)
  y <- 1.0 + 0.5 * t + stats::rnorm(n, sd = 0.3)
  out <- morie:::.morie_hajek_ate(ps, t, y)
  expect_named(out, c("ate", "y1", "y0"))
  expect_true(is.numeric(out$ate))
})

test_that(".morie_fit_propensity clips ps to [0.01, 0.99]", {
  set.seed(2L); n <- 100L
  df <- data.frame(d = stats::rbinom(n, 1L, 0.5),
                   x1 = stats::rnorm(n),
                   x2 = stats::rnorm(n))
  ps <- morie:::.morie_fit_propensity(df, "d", c("x1", "x2"))
  expect_length(ps, n)
  expect_true(all(ps >= 0.01 & ps <= 0.99))
})

# ===================================================================== vrgft.R

test_that(".vrgft_model dispatches all three variogram families", {
  # exponential at h=0 should equal c0; far away approaches c0+c1.
  expect_equal(morie:::.vrgft_model(0, c0 = 0.5, c1 = 1, a = 10,
                                      model = "exponential"), 0.5)
  expect_equal(morie:::.vrgft_model(1e6, c0 = 0.5, c1 = 1, a = 10,
                                      model = "exponential"), 1.5)
  # spherical at h > a saturates at c0 + c1.
  expect_equal(morie:::.vrgft_model(20, c0 = 0.5, c1 = 1, a = 10,
                                      model = "spherical"), 1.5)
})

test_that(".vrgft_model errors on unknown model", {
  expect_error(morie:::.vrgft_model(1, 0, 1, 10, "garch"),
               regexp = "unknown model")
})

test_that(".vrgft_obj returns >=0 WLS objective", {
  mids <- c(1, 2, 3)
  gammas <- c(0.2, 0.4, 0.6)
  weights <- c(1, 1, 1)
  out <- morie:::.vrgft_obj(p = c(0, 1, 2), mids, gammas, weights,
                             model = "exponential")
  expect_true(is.numeric(out) && out >= 0)
})

# ===================================================================== trfbl.R

test_that(".trfbl_layer_norm normalises each row to mean 0 sd 1", {
  set.seed(3L)
  x <- matrix(stats::rnorm(20L), 4L, 5L)
  out <- morie:::.trfbl_layer_norm(x)
  expect_equal(rowMeans(out), rep(0, 4L), tolerance = 1e-9)
  expect_equal(apply(out, 1L, stats::sd),
               rep(sqrt(5 / 4), 4L), tolerance = 1e-3)
})

test_that(".trfbl_gelu approximates 0 at z=0 and z at z>>0", {
  expect_equal(morie:::.trfbl_gelu(0), 0, tolerance = 1e-12)
  # For large z, GELU(z) ~ z.
  expect_equal(morie:::.trfbl_gelu(10), 10, tolerance = 1e-3)
  # For large negative z, GELU(z) ~ 0.
  expect_equal(morie:::.trfbl_gelu(-10), 0, tolerance = 1e-3)
})

# ===================================================================== bkprp.R

test_that(".bkprp_sigma dispatches identity/linear/none + saturating", {
  z <- matrix(c(-1, 0, 1, 2), 2L, 2L)
  expect_equal(morie:::.bkprp_sigma(z, "identity"), z)
  expect_equal(morie:::.bkprp_sigma(z, "linear"),   z)
  expect_equal(morie:::.bkprp_sigma(z, "none"),     z)
  expect_equal(morie:::.bkprp_sigma(0, "sigmoid"), 0.5)
  expect_equal(morie:::.bkprp_sigma(0, "tanh"),    0)
  # NB: relu via pmax(0, .) strips matrix dim attributes; assert via vec.
  expect_equal(morie:::.bkprp_sigma(c(-1, 0, 1, 2), "relu"),
               c(0, 0, 1, 2))
})

test_that(".bkprp_sigma errors on unknown activation", {
  expect_error(morie:::.bkprp_sigma(matrix(0, 1, 1), "wat"),
               regexp = "Unknown activation")
})

test_that(".bkprp_sigma_prime returns shape-correct gradients", {
  z <- matrix(c(-1, 0, 1, 2), 2L, 2L)
  a <- morie:::.bkprp_sigma(z, "sigmoid")
  out <- morie:::.bkprp_sigma_prime(z, "sigmoid", a)
  expect_equal(dim(out), dim(z))
  # ReLU derivative is (z>0); a passed but unused on this branch.
  rd <- morie:::.bkprp_sigma_prime(z, "relu", a)
  expect_equal(unname(as.vector(rd)), c(0, 0, 1, 1))
})

# ==================================================================== tgrch.R

test_that(".tgarch_negll returns 1e10 on infeasible parameters", {
  r <- stats::rnorm(20L)
  out <- morie:::.tgarch_negll(p = c(-0.1, 0, 0, 0), r = r, n = 20L)
  expect_equal(out, 1e10)
})

test_that(".tgarch_negll returns finite scalar on feasible parameters", {
  set.seed(4L); r <- stats::rnorm(50L)
  out <- morie:::.tgarch_negll(p = c(0.01, 0.05, 0.05, 0.85),
                                 r = r, n = 50L)
  expect_true(is.numeric(out) && is.finite(out))
})

# =============================================================== ingest_tps.R

test_that(".morie_tps_features_to_rows returns empty list on empty input", {
  out <- morie:::.morie_tps_features_to_rows(list(),
                                               return_geometry = FALSE)
  expect_type(out, "list")
  expect_length(out, 0L)
})

test_that(".morie_tps_features_to_rows extracts attributes per feature", {
  features <- list(
    list(attributes = list(OBJECTID = 1L, OCC_YEAR = 2024L)),
    list(attributes = list(OBJECTID = 2L, OCC_YEAR = 2025L)))
  out <- morie:::.morie_tps_features_to_rows(features,
                                               return_geometry = FALSE)
  expect_type(out, "list")
  expect_length(out, 2L)
  expect_equal(out[[1]]$OBJECTID, 1L)
  expect_equal(out[[2]]$OCC_YEAR, 2025L)
})

test_that(".morie_tps_features_to_rows appends geom_x/geom_y when requested", {
  features <- list(
    list(attributes = list(OBJECTID = 1L),
         geometry   = list(x = -79.4, y = 43.7)))
  out <- morie:::.morie_tps_features_to_rows(features,
                                               return_geometry = TRUE)
  expect_equal(out[[1]]$geom_x, -79.4)
  expect_equal(out[[1]]$geom_y, 43.7)
})

# ======================================================== mrm_primitives_ordinal.R

test_that(".tso_logit_ll returns numeric log-likelihood", {
  set.seed(5L)
  eta <- stats::rnorm(20L)
  y <- as.integer(stats::rbinom(20L, 1L, 0.5))
  out <- morie:::.tso_logit_ll(eta, y)
  expect_true(is.numeric(out) && is.finite(out))
})

test_that(".tso_fit_po_stacked returns intercepts (K-1) + beta (p)", {
  set.seed(6L); n <- 100L
  X <- matrix(stats::rnorm(n * 2L), n, 2L)
  y <- sample.int(3L, n, replace = TRUE) - 1L  # 0, 1, 2
  out <- morie:::.tso_fit_po_stacked(X, y, K = 3L,
                                       max_iter = 50L, tol = 1e-6)
  expect_named(out, c("intercepts", "beta"))
  expect_length(out$intercepts, 2L)
  expect_length(out$beta, 2L)
})

# ==================================================================== tps_csi.R

test_that(".tps_csi_to_long passes data.frame through with required cols", {
  df <- data.frame(year = c(2020, 2021),
                   category = c("a", "b"),
                   count = c(5L, 7L))
  out <- morie:::.tps_csi_to_long(df, "year")
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 2L)
})

test_that(".tps_csi_to_long errors on data.frame missing required cols", {
  df <- data.frame(year = c(2020), category = c("a"))
  expect_error(morie:::.tps_csi_to_long(df, "year"),
               regexp = "long-format")
})

# ==================================================================== database.R

test_that(".fuzzy_match_key returns NULL when no catalog entry matches", {
  expect_null(morie:::.fuzzy_match_key("nonexistent_key_xyz"))
})

test_that(".fuzzy_match_key resolves a hyphen / case variation to a real catalog key", {
  cat <- morie_dataset_catalog()
  if (nrow(cat) == 0L) skip("dataset catalog is empty in dev tree")
  example_key <- cat$key[1]
  # Same key with hyphens instead of underscores + upper-case.
  munged <- toupper(gsub("_", "-", example_key))
  out <- morie:::.fuzzy_match_key(munged)
  expect_equal(out, example_key)
})
