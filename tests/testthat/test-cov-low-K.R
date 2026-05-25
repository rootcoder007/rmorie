# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage lift batch K: aniso, vecmf, causal, dtrsp, unfdl, entheo_analysis, inspector, study_core, workflow.

# ==== aniso.R ====
test_that("morie_aniso errors when coords rows do not match length(x)", {
  expect_error(
    morie_aniso(x = rnorm(10), coords = matrix(runif(8), 4, 2)),
    "coords rows must match length"
  )
})

test_that("morie_aniso returns trivially-isotropic result for 1D coords", {
  set.seed(1)
  res <- morie_aniso(x = rnorm(20), coords = matrix(runif(20), 20, 1))
  expect_equal(res$statistic, 0)
  expect_equal(res$p_value, 1)
})

# ==== vecmf.R ====
test_that("morie_vecm errors when T<20 or rank out of bounds", {
  expect_error(morie_vecm(matrix(rnorm(10), 5, 2)), "Need T>=20")
})

# ==== causal.R ====
test_that("morie_estimate_gate returns NA row for tiny / single-treatment subgroup", {
  set.seed(1)
  df <- data.frame(
    t = c(rep(0, 5), rep(1, 5), rbinom(290, 1, 0.4)),
    y = rnorm(300),
    x = rnorm(300),
    g = c(rep("tiny", 5), rep("notreat", 5), rep("ok", 290))
  )
  res <- morie_estimate_gate(df, "t", "y", "x", "g")
  tiny <- res[res$group == "tiny", ]
  notreat <- res[res$group == "notreat", ]
  expect_true(is.na(tiny$ate))
  expect_true(is.na(notreat$ate))
})

# ==== dtrsp.R ====
test_that("morie_decision_tree_split entropy criterion runs on small data", {
  set.seed(1)
  x <- matrix(rnorm(80), 40, 2)
  y <- as.factor(rbinom(40, 1, 0.5))
  res <- morie_decision_tree_split(x, y, criterion = "entropy")
  expect_equal(res$criterion, "entropy")
  expect_true(is.finite(res$root_impurity))
})

# ==== unfdl.R ====
test_that("unfdl errors when x is not a matrix", {
  expect_error(unfdl(x = c(1, 2, 3)), "x must be a matrix")
})

test_that("unfdl returns empty skeleton when nrow<2 or ncol<2", {
  res1 <- unfdl(matrix(0, 1, 5))
  expect_equal(res1$n_resp, 0L)
  expect_true(is.na(res1$stress))

  res2 <- unfdl(matrix(0, 5, 1))
  expect_equal(res2$n_resp, 0L)
})

test_that("unfdl converges on a small matrix", {
  set.seed(1)
  P <- abs(matrix(rnorm(20, 3, 0.5), 5, 4))
  res <- unfdl(P, k = 2L, n_iter = 100L, tol = 1e-3)
  expect_equal(res$n_resp, 5L)
  expect_equal(res$n_stim, 4L)
  expect_true(is.finite(res$stress))
})

# ==== entheo_analysis.R ====
test_that("beautiful_loop_metric returns NA shell when EEG/fMRI missing", {
  res <- morie:::beautiful_loop_metric(eeg = NULL, fmri = NULL)
  expect_true(is.na(res$score_dmt))
  expect_true(is.na(res$score_pcb))
})

test_that("san_score returns NA shell when EEG/fMRI missing", {
  res <- morie:::san_score(eeg = NULL, fmri = NULL)
  expect_true(is.na(res$score_dmt))
})

test_that(".entheo_align handles empty inputs", {
  al <- morie:::.entheo_align(numeric(0), numeric(0))
  expect_length(al$e, 0)
  expect_length(al$f, 0)
})

# ==== inspector.R ====
test_that("morie_inspect_output reports missing for nonexistent paths", {
  res <- morie_inspect_output(tempfile(fileext = ".json"))
  expect_false(res$exists)
  expect_equal(res$status, "missing")
})

test_that("morie_inspect_output handles unsupported extensions, csv + rds", {
  # Unsupported extension
  tmp_txt <- tempfile(fileext = ".txt")
  writeLines("hi", tmp_txt)
  withr::defer(unlink(tmp_txt))
  res_txt <- morie_inspect_output(tmp_txt)
  expect_match(res_txt$status, "unsupported-extension")

  # CSV branch
  tmp_csv <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(a = 1:3, b = 4:6), tmp_csv, row.names = FALSE)
  withr::defer(unlink(tmp_csv))
  res_csv <- morie_inspect_output(tmp_csv)
  expect_equal(res_csv$status, "ok")

  # RDS branch
  tmp_rds <- tempfile(fileext = ".rds")
  saveRDS(data.frame(x = 1:2), tmp_rds)
  withr::defer(unlink(tmp_rds))
  res_rds <- morie_inspect_output(tmp_rds)
  expect_equal(res_rds$status, "ok")
})

test_that("morie_verify_statistical_output handles missing file", {
  res_missing <- morie_verify_statistical_output(tempfile(fileext = ".json"))
  expect_false(res_missing$passed)
})

# ==== study_core.R ====
test_that(".safe_divide returns NA when denominator is 0 or NA", {
  expect_true(is.na(morie:::.safe_divide(1, 0)))
  expect_true(is.na(morie:::.safe_divide(1, NA_real_)))
  expect_equal(morie:::.safe_divide(6, 2), 3)
})

test_that(".clip_exp saturates at +/-700", {
  expect_true(is.finite(morie:::.clip_exp(1e6)))
  expect_gt(morie:::.clip_exp(-1e6), 0)
})

# ==== workflow.R ====
test_that("validate_workflow_map errors on bad shapes", {
  expect_error(
    morie:::validate_workflow_map(list(a = "x")),
    "must be a named character vector"
  )
  expect_error(
    morie:::validate_workflow_map(c("x", "y")),
    "must be a named character vector"
  )
})

test_that("morie_run_workflow_step errors on missing/unknown step", {
  expect_error(morie_run_workflow_step(), "exactly one workflow")
  expect_error(morie_run_workflow_step(c("a", "b")), "exactly one workflow")
  expect_error(
    morie_run_workflow_step("nope", script_map = c(a = "foo.R")),
    "Unknown step"
  )
})

test_that("morie_run_pipeline rejects bad/empty steps", {
  expect_error(
    morie_run_pipeline(steps = character()),
    "non-empty character vector"
  )
  expect_error(
    morie_run_pipeline(steps = 1L),
    "non-empty character vector"
  )
})
