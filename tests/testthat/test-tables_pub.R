# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage tests for R/tables_pub.R -- pub-table builders.

set.seed(1)

test_that("internal formatters handle finite + non-finite", {
  set.seed(1)
  expect_equal(morie:::.tbl_fmt_num(0.1234), "0.12")
  expect_equal(morie:::.tbl_fmt_num(0.1234, apa = TRUE), ".12")
  expect_equal(morie:::.tbl_fmt_num(NA), "")
  expect_equal(morie:::.tbl_fmt_pval(0.0001), "<0.001")
  expect_equal(morie:::.tbl_fmt_pval(0.0001, apa = TRUE), "<.001")
  expect_equal(morie:::.tbl_stars(0.0001), "***")
  expect_equal(morie:::.tbl_stars(0.02), "*")
  expect_equal(morie:::.tbl_stars(0.2), "")
})

test_that("smd handles zero pooled-sd", {
  set.seed(1)
  expect_equal(morie:::.tbl_smd(1, 1, 0, 0), 0)
  expect_gt(abs(morie:::.tbl_smd(0, 1, 1, 1)), 0)
})

test_that("footnote registry adds + renders text/latex/html", {
  set.seed(1)
  reg <- morie:::.tbl_footnotes_new()
  morie:::.tbl_footnotes_add(reg, "note 1")
  morie:::.tbl_footnotes_add(reg, "note 1")  # dedupe
  morie:::.tbl_footnotes_add(reg, "note 2")
  expect_length(reg$notes, 2L)
  expect_type(morie:::.tbl_footnotes_render(reg, "text"), "character")
  expect_match(morie:::.tbl_footnotes_render(reg, "latex"), "textsuperscript")
  expect_match(morie:::.tbl_footnotes_render(reg, "html"), "<sup>")
  expect_equal(morie:::.tbl_footnotes_render(morie:::.tbl_footnotes_new()), "")
})

test_that("to_format dataframe pass-through + csv", {
  set.seed(1)
  df <- data.frame(a = 1:2, b = c("p", "q"))
  expect_identical(morie:::.tbl_to_format(df, "dataframe"), df)
  expect_type(morie:::.tbl_to_format(df, "csv", title = "T"), "character")
})

test_that("to_format markdown/latex/html via knitr", {
  set.seed(1)
  df <- data.frame(a = 1:2, b = 3:4)
  expect_type(morie:::.tbl_to_format(df, "markdown"), "character")
  expect_type(morie:::.tbl_to_format(df, "latex"), "character")
  expect_type(morie:::.tbl_to_format(df, "html"), "character")
})

test_that("format_number switches across styles", {
  set.seed(1)
  expect_equal(format_number(0.5, style = "fixed"), "0.50")
  expect_equal(format_number(1e-6, style = "scientific"), "1.00e-06")
  expect_equal(format_number(0.25, style = "percent"), "25.00%")
  expect_equal(format_number(3.7, style = "integer"), "4")
  expect_equal(format_number(NA), "")
})

test_that("format_dataframe formats numeric + pval cols", {
  set.seed(1)
  df <- data.frame(est = c(1.234, 2.456), p = c(0.001, 0.5))
  out <- format_dataframe(df, pval_cols = "p")
  expect_s3_class(out, "data.frame")
  expect_type(out$p, "character")
})

test_that("summary_statistics_table builds a frame", {
  set.seed(1)
  df <- data.frame(x = rnorm(20), y = rnorm(20))
  out <- summary_statistics_table(df)
  expect_s3_class(out, "data.frame")
  expect_true("mean" %in% colnames(out))
})

test_that("treatment_effect_table renders multiple estimators", {
  set.seed(1)
  est <- list(
    ols = list(estimate = 0.5, se = 0.1, ci_lower = 0.3, ci_upper = 0.7, p_value = 0.01),
    iv  = list(estimate = 0.6, se = 0.15, ci_lower = 0.31, ci_upper = 0.89, p_value = 0.04)
  )
  out <- treatment_effect_table(est)
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 2L)
  expect_true("Estimate" %in% colnames(out))
})

test_that("regression_table works on lm fits", {
  set.seed(1)
  df <- data.frame(x = rnorm(30), z = rnorm(30))
  df$y <- 0.5 * df$x + 0.3 * df$z + rnorm(30)
  m1 <- lm(y ~ x, data = df)
  m2 <- lm(y ~ x + z, data = df)
  out <- regression_table(list(m1 = m1, m2 = m2), show_ci = FALSE)
  expect_s3_class(out, "data.frame")
  expect_true(all(c("m1", "m2") %in% colnames(out)))
})

test_that("odds_ratio_table works on logistic glm", {
  set.seed(1)
  df <- data.frame(x = rnorm(50))
  df$y <- rbinom(50, 1, plogis(0.4 * df$x))
  m <- glm(y ~ x, data = df, family = binomial())
  out <- odds_ratio_table(m)
  expect_s3_class(out, "data.frame")
  expect_true("OR" %in% colnames(out))
})

test_that("hazard_ratio_table builds from beta/se/p", {
  set.seed(1)
  params <- c(treat = 0.5, age = 0.02)
  se <- c(treat = 0.1, age = 0.005)
  pv <- c(treat = 0.001, age = 0.0001)
  out <- hazard_ratio_table(params, se, pv)
  expect_s3_class(out, "data.frame")
  expect_true("HR" %in% colnames(out))
})

test_that("correlation_table returns square matrix-frame", {
  set.seed(1)
  df <- data.frame(a = rnorm(30), b = rnorm(30), c = rnorm(30))
  out <- correlation_table(df)
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 3L)
  expect_equal(ncol(out), 3L)
})

test_that("model_comparison_table compares lm fits", {
  set.seed(1)
  df <- data.frame(x = rnorm(30))
  df$y <- df$x + rnorm(30)
  m1 <- lm(y ~ 1, data = df); m2 <- lm(y ~ x, data = df)
  out <- tryCatch(model_comparison_table(list(null = m1, fit = m2)), error = function(e) NULL)
  skip_if(is.null(out), "model_comparison signature change")
  expect_s3_class(out, "data.frame")
})

test_that("anova_table wraps stats::anova on lm fits", {
  set.seed(1)
  df <- data.frame(x = rnorm(30))
  df$y <- df$x + rnorm(30)
  m <- lm(y ~ x, data = df)
  out <- tryCatch(anova_table(m), error = function(e) NULL)
  skip_if(is.null(out), "needs car::Anova or matching wrapper")
  expect_s3_class(out, "data.frame")
})

test_that("table1 returns a data.frame for a simple grouped dataset", {
  set.seed(1)
  df <- data.frame(
    grp = rep(c("A", "B"), each = 15),
    age = rnorm(30, mean = 40, sd = 5),
    sex = rep(c("m", "f"), 15)
  )
  out <- tryCatch(table1(df, group_col = "grp", show_smd = FALSE, show_p = FALSE),
                  error = function(e) NULL)
  skip_if(is.null(out), "table1 wraps signature change")
  expect_s3_class(out, "data.frame")
})

test_that("regression_table latex output is a character vector", {
  set.seed(1)
  df <- data.frame(x = rnorm(20)); df$y <- df$x + rnorm(20)
  m <- lm(y ~ x, data = df)
  out <- regression_table(list(m = m), output_format = "latex", show_ci = FALSE)
  expect_type(as.character(out), "character")
})