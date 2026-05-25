# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage wave 16 -- inspector.R (output inspection / verification)
# and frns_temporal.R (multi-city temporal disparity audit).

test_that("morie_inspect_output reports on json / csv / rds inputs", {
  skip_if_not_installed("jsonlite")
  expect_equal(morie_inspect_output("/no/such/file.json")$status, "missing")

  jf <- tempfile(fileext = ".json")
  jsonlite::write_json(list(estimate = 0.1, se = 0.05), jf,
    auto_unbox = TRUE
  )
  on.exit(unlink(jf), add = TRUE)
  ij <- morie_inspect_output(jf)
  expect_equal(ij$status, "ok")
  expect_equal(ij$format, "json")

  cf <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(a = 1:3, b = 4:6), cf, row.names = FALSE)
  on.exit(unlink(cf), add = TRUE)
  expect_equal(morie_inspect_output(cf)$n_columns, 2L)

  rf <- tempfile(fileext = ".rds")
  saveRDS(data.frame(z = 1:2), rf)
  on.exit(unlink(rf), add = TRUE)
  expect_equal(morie_inspect_output(rf)$status, "ok")
  rf2 <- tempfile(fileext = ".rds")
  saveRDS(list(p = 1, q = 2), rf2)
  on.exit(unlink(rf2), add = TRUE)
  expect_equal(morie_inspect_output(rf2)$status, "ok")
})

test_that("morie_inspect_output flags unsupported and unreadable files", {
  uf <- tempfile(fileext = ".dat")
  file.create(uf)
  on.exit(unlink(uf), add = TRUE)
  expect_match(morie_inspect_output(uf)$status, "unsupported")

  bf <- tempfile(fileext = ".json")
  writeLines("{not valid json", bf)
  on.exit(unlink(bf), add = TRUE)
  expect_match(morie_inspect_output(bf)$status, "read-error")
})

test_that("morie_verify_statistical_output runs its quality gates", {
  skip_if_not_installed("jsonlite")
  expect_false(morie_verify_statistical_output("/no/such.json")$passed)

  vf <- tempfile(fileext = ".json")
  jsonlite::write_json(
    list(
      ate = 0.5, se = 0.1, ci_lower = 0.3, ci_upper = 0.7,
      n = 200, p_value = 0.04
    ), vf,
    auto_unbox = TRUE
  )
  on.exit(unlink(vf), add = TRUE)
  v <- morie_verify_statistical_output(vf)
  expect_true(v$passed)
  expect_true(v$checks$se_nonneg)
  expect_true(v$checks$ci_ordered)
  expect_true(v$checks$estimate_in_ci)

  bf <- tempfile(fileext = ".json")
  jsonlite::write_json(
    list(ate = 0.5, se = -1, ci_lower = 0.9, ci_upper = 0.1, n = -5),
    bf,
    auto_unbox = TRUE
  )
  on.exit(unlink(bf), add = TRUE)
  expect_false(morie_verify_statistical_output(bf)$passed)

  mf <- tempfile(fileext = ".json")
  writeLines("{broken", mf)
  on.exit(unlink(mf), add = TRUE)
  expect_false(morie_verify_statistical_output(mf)$passed)
})

test_that("morie_predpol_temporal_audit validates its inputs", {
  expect_error(morie_predpol_temporal_audit(1:5, 1:6, 1:5, 1:5), "align")
  expect_error(
    morie_predpol_temporal_audit(
      character(0), character(0), numeric(0),
      character(0)
    ),
    "empty"
  )
})

test_that("morie_predpol_temporal_audit audits multi-city temporal disparity", {
  set.seed(1)
  n <- 300
  period <- rep(c("p1", "p2", "p3"), length.out = n)
  city <- rep(c("A", "B"), length.out = n)
  group <- sample(c("X", "Y"), n, replace = TRUE)
  y_pred <- stats::rbinom(n, 1, ifelse(group == "X", 0.7, 0.4))
  res <- morie_predpol_temporal_audit(period, city, y_pred, group,
    privileged = "X"
  )
  expect_true(is.list(res))
  expect_false(is.null(res$per_city))
  expect_true(nzchar(res$interpretation))
  # privileged inferred globally when not supplied
  res2 <- morie_predpol_temporal_audit(period, city, y_pred, group)
  expect_true(any(grepl("inferred", res2$warnings)))
})

test_that("morie_predpol_temporal_audit errors when no cell is auditable", {
  expect_error(
    morie_predpol_temporal_audit(rep("p1", 10), rep("A", 10), rep(1, 10),
      rep("X", 10),
      privileged = "X"
    ),
    "cell"
  )
})
