# R/otis_all_analyze.R: the alternative-treatment and subgroup Ruhela formulations, the Mandela
# classifications, the chi-square families, and the grid / master orchestrators, on the
# package's synthetic OTIS tables with every reported number recomputed.

.ox_pkg <- if (isNamespaceLoaded("rmorie")) "rmorie" else "morie"
.ox <- function(name) get(name, envir = environment(morie_otis_analyze_ruhela_grid))

test_that("the alternative-treatment b02 formulations run on the Toronto and age-50 indicators", {
  b02 <- morie_synth_otis("b02", n = 200L, seed = 1L)
  dir <- withr::local_tempdir()
  r <- morie_otis_analyze_b02_ruhela_alt_region(b02, out_dir = dir)
  expect_s3_class(r, "morie_otis_analysis_result")
  expect_length(r$warnings, 0)
  expect_match(r$title, "Alt-T Ruhela formulation: T=Toronto region")
  expect_gt(length(r$tables[[1]]$rows), 0)
  expect_true(file.exists(file.path(dir, "otis_b02-altR.txt")))
  expect_true(file.exists(file.path(dir, "otis_b02-altR.json")))
  r2 <- morie_otis_analyze_b02_ruhela_alt_age(b02)
  expect_match(r2$title, "T=Age 50\\+")
  expect_gt(length(r2$tables[[1]]$rows), 0)
  expect_error(morie_otis_analyze_b02_ruhela_alt_age(b02[, setdiff(names(b02), "Age_Category")]),
               "missing column\\(s\\): Age_Category")
  expect_error(morie_otis_analyze_b02_ruhela_alt_region(NULL), "pass data = the OTIS b02 table")
})

test_that("the a01 subgroup formulations refuse a thin subgroup and run on a thick one", {
  small <- morie_synth_otis("a01", n = 120L, seed = 1L)
  pair <- morie_otis_make_pair_alert_to_volatility_ruhela(small)
  nf <- sum(.ox(".otis_female")(pair$data$Gender) == 1L)
  nm <- nrow(pair$data) - nf
  expect_lt(nf, 100)
  rf <- morie_otis_analyze_a01_ruhela_subgroup_female(small)
  expect_equal(rf$warnings, sprintf("only %d female cells; too few", nf))
  expect_lt(nm, 100)
  rm <- morie_otis_analyze_a01_ruhela_subgroup_male(small)
  expect_equal(rm$warnings, sprintf("only %d male cells; too few", nm))
  big <- morie_synth_otis("a01", n = 2500L, seed = 2L)
  pair <- morie_otis_make_pair_alert_to_volatility_ruhela(big)
  expect_gte(sum(.ox(".otis_female")(pair$data$Gender) == 1L), 100)
  rf <- morie_otis_analyze_a01_ruhela_subgroup_female(big)
  expect_length(rf$warnings, 0)
  expect_match(rf$title, "Female-only cell frame")
  expect_gt(length(rf$tables[[1]]$rows), 0)
  rm <- morie_otis_analyze_a01_ruhela_subgroup_male(big)
  expect_length(rm$warnings, 0)
  expect_match(rm$title, "Male-only")
  # the b01 siblings carry the same guard
  b01 <- morie_synth_otis("b01", n = 120L, seed = 1L)
  pair <- morie_otis_make_pair_alert_to_volatility_ruhela(b01)
  nf <- sum(.ox(".otis_female")(pair$data$Gender) == 1L)
  nm <- nrow(pair$data) - nf
  expect_lt(max(nf, nm), 100)
  expect_equal(morie_otis_analyze_b01_ruhela_subgroup_female(b01)$warnings, sprintf("only %d female cells; too few", nf))
  expect_equal(morie_otis_analyze_b01_ruhela_subgroup_male(b01)$warnings, sprintf("only %d male cells; too few", nm))
})

test_that("the Mandela classifications count placements and individuals on each side of fifteen days", {
  b05 <- morie_synth_otis("b05", n = 120L, seed = 1L)
  r <- morie_otis_analyze_b05_mandela_classification(b05)
  cls <- .ox(".otis_classify_bin")(b05$Consecutive_Duration)
  expect_setequal(unique(cls), c("Solitary Confinement (<=15d)", "Torture (>=16d)"))
  yrs <- sort(unique(as.integer(b05$EndFiscalYear)))
  n <- as.numeric(b05$Number_SegregationPlacements)
  expected <- lapply(yrs, function(y) {
    s <- sum(n[as.integer(b05$EndFiscalYear) == y & cls == "Solitary Confinement (<=15d)"])
    t <- sum(n[as.integer(b05$EndFiscalYear) == y & cls == "Torture (>=16d)"])
    c(y, s, sprintf("%.1f%%", 100 * s / (s + t)), t, sprintf("%.1f%%", 100 * t / (s + t)), s + t)
  })
  expect_equal(r$payload$rows, expected)
  expect_equal(r$tables[[1]]$rows, expected)
  last_tor <- as.numeric(sub("%", "", expected[[length(expected)]][5]))
  expect_match(r$summary_lines[["Latest provincial torture rate"]],
               sprintf("^%.2f%% -- vs federal 9.9%%; %s$", last_tor, if (last_tor > 9.9) "higher" else "lower"))
  expect_equal(r$payload$federal_reference$n, 1960)
  expect_match(morie_otis_analyze_b05_mandela_classification(b05[, 1:2])$warnings, "b05 missing required columns")
  # c11: individuals, two confinement views
  c11 <- morie_synth_otis("c11", n = 120L, seed = 1L)
  rc <- morie_otis_analyze_c11_mandela_classification(c11)
  cls <- .ox(".otis_classify_bin")(c11$Aggregate_Duration)
  yrs <- sort(unique(as.integer(c11$EndFiscalYear)))
  exp_c11 <- list()
  for (y in yrs) for (k in list(c("NumberIndividuals_Segregation", "Segregation"),
                                c("NumberIndividuals_RestrictiveConfinement", "Restrictive confinement"))) {
    v <- as.numeric(c11[[k[1]]])
    s <- sum(v[as.integer(c11$EndFiscalYear) == y & cls == "Solitary Confinement (<=15d)"])
    t <- sum(v[as.integer(c11$EndFiscalYear) == y & cls == "Torture (>=16d)"])
    if (s + t > 0) {
      exp_c11[[length(exp_c11) + 1L]] <- c(y, k[2], s, sprintf("%.1f%%", 100 * s / (s + t)), t,
                                           sprintf("%.1f%%", 100 * t / (s + t)), s + t)
    }
  }
  expect_equal(rc$payload$rows, exp_c11)
  expect_match(morie_otis_analyze_c11_mandela_classification(c11[, 1:2])$warnings, "c11 missing required columns")
  # the provincial-versus-federal comparison reads the segregation rows back
  cmp <- morie_otis_analyze_otis_mandela_provincial_vs_federal(c11)
  seg <- Filter(function(x) x[2] == "Segregation", exp_c11)
  tor <- vapply(seg, function(x) as.numeric(sub("%", "", x[6])), numeric(1))
  expect_equal(cmp$payload$max_provincial_torture_pct, max(tor))
  expect_equal(cmp$payload$max_provincial_torture_year, seg[[which.max(tor)]][1])
  expect_equal(cmp$payload$gap_pp, max(tor) - 9.9)
  rows <- cmp$tables[[1]]$rows
  expect_equal(rows[[1]][1], "Federal SIUs (CSC, Sprott-Doob T19)")
  expect_equal(length(rows), 1L + length(exp_c11))
  expect_equal(rows[[2]][1], "Ontario Provincial Segregation")
  expect_true(startsWith(cmp$summary_lines[["Provincial-vs-federal gap (peak)"]],
                         sprintf("%+.1f percentage points", max(tor) - 9.9)))
  # a c11 without its columns returns the classification's own warning
  expect_match(morie_otis_analyze_otis_mandela_provincial_vs_federal(c11[, 1:2])$warnings, "c11 missing required")
})

test_that("chi-square with Cramer's V recomputes from stats::chisq.test and names its failures", {
  cc <- .ox(".otis_chi2_cramer")
  tbl <- matrix(c(30, 10, 20, 40), 2, 2)
  st <- cc(tbl)
  ref <- suppressWarnings(stats::chisq.test(tbl))
  expect_equal(st$chi2, as.numeric(ref$statistic))
  expect_equal(st$dof, 1L)
  expect_equal(st$pvalue, ref$p.value)
  expect_equal(st$cramer_v, sqrt(as.numeric(ref$statistic) / 100))
  expect_equal(st$n, 100L)
  expect_equal(st$min_cell, 10L)
  expect_equal(st$min_expected, min(ref$expected))
  empty <- cc(matrix(0, 1, 1))
  expect_equal(empty$n, 0L)
  expect_true(is.na(empty$chi2))
  one <- cc(matrix(5, 1, 1))
  expect_true(is.na(one$chi2))
  expect_match(one$error, "at least")
  ct <- .ox(".otis_contingency_chi2")
  df <- data.frame(r = c("a", "a", "b", "b"), k = c("x", "y", "x", "y"), v = c(30, 10, 20, 40))
  got <- ct(df, "r", "k", "v")
  # rows a / b, columns x / y
  expect_equal(unname(got$table), matrix(c(30, 20, 10, 40), 2, 2), ignore_attr = TRUE)
  expect_equal(got$stats$chi2, st$chi2)
  expect_equal(ct(df, "r", "nope", "v")$stats$error, "missing columns")
  expect_equal(ct(transform(df, v = "x"), "r", "k", "v")$stats$n, 0L)
})

test_that("the c- and d-series chi-square families report every slice and the yearly death counts", {
  ds <- morie_synth_otis_all(n = 80L, seed = 2L)
  r <- morie_otis_analyze_c_chi2(ds)
  rows <- r$tables[[1]]$rows
  expect_length(rows, 7)
  expect_equal(vapply(rows, `[`, "", 1), c("c03", "c04", "c05", "c06", "c07", "c08", "c09"))
  st <- .ox(".otis_contingency_chi2")(ds$c03, "Race", "Gender", "NumberIndividuals_RestrictiveConfinement")$stats
  expect_equal(rows[[1]], c("c03", "Race x Gender", st$n, round(st$chi2, 3), st$dof, sprintf("%.2e", st$pvalue),
                            round(st$cramer_v, 4), st$min_cell))
  expect_equal(r$payload$slices$c03$stats$chi2, st$chi2)
  partial <- morie_otis_analyze_c_chi2(ds["c03"], contingency_value = "NumberIndividuals_Segregation")
  prow <- partial$tables[[1]]$rows
  expect_equal(prow[[2]], c("c04", "Race x Region_MostRecentPlacement", "0", "n/a", "0", "n/a", "n/a", "0"))
  expect_equal(partial$summary_lines[["Contingency value column"]], "NumberIndividuals_Segregation")
  # d-series
  d <- morie_otis_analyze_d_chi2(ds)
  y <- as.integer(ds$d01$Year)
  tab <- table(y)
  yrs <- as.integer(names(tab))
  exp_rows <- lapply(seq_along(yrs), function(i) {
    n_i <- as.integer(tab[i])
    c(yrs[i], n_i, sprintf("[%.1f, %.1f]", stats::qchisq(0.025, 2 * n_i) / 2, stats::qchisq(0.975, 2 * (n_i + 1)) / 2))
  })
  expect_equal(d$tables[[1]]$rows, exp_rows)
  n_first <- as.integer(tab[1])
  n_last <- as.integer(tab[length(tab)])
  rr <- n_last / n_first
  se <- sqrt(1 / n_first + 1 / n_last)
  expect_equal(d$payload$yoy_rate_ratio$rr, rr)
  expect_equal(d$payload$yoy_rate_ratio$ci95_low, exp(log(rr) - 1.96 * se))
  expect_match(d$summary_lines[["Year-over-year RR"]], sprintf("^%.3f \\(%d/%d; n=%d/%d", rr, max(yrs), min(yrs), n_last, n_first))
  expect_equal(d$summary_lines[["d01 total deaths"]], sum(tab))
  expect_equal(d$summary_lines[["d01 year range"]], sprintf("%d-%d", min(yrs), max(yrs)))
  st6 <- .ox(".otis_contingency_chi2")(ds$d06, "Alert_Type", "MedicalCauseOfDeath", "Number_CustodialDeaths")$stats
  expect_equal(d$payload$d06_chi2$chi2, st6$chi2)
  expect_equal(d$tables[[2]]$rows[[1]][1:2], c("d06", "Alert x MedicalCause"))
  # nothing supplied: every line says so
  none <- morie_otis_analyze_d_chi2(list())
  expect_equal(none$summary_lines[["Year-over-year RR"]], "n/a")
  expect_equal(none$summary_lines[["d01 year range"]], "n/a")
  expect_equal(none$summary_lines[["d01 total deaths"]], 0L)
  expect_equal(none$tables[[2]]$rows[[2]], c("d07", "Alert x Housing_Type", "0", "n/a", "0", "n/a", "n/a", "0"))
  expect_error(morie_otis_analyze_c_chi2("x"), "is.list")
})

test_that("the Ruhela grid lists one primary estimate per aggregate analyzer and the master report stacks the sections", {
  ds <- morie_synth_otis_all(n = 80L, seed = 2L)
  dir <- withr::local_tempdir()
  g <- morie_otis_analyze_ruhela_grid(ds[c("b03", "c01")], out_dir = dir)
  rows <- g$tables[[1]]$rows
  expect_length(rows, 24)
  ids <- vapply(rows, `[`, "", 1)
  expect_equal(ids[1:2], c("b03", "b04"))
  expect_true("c01-RC" %in% ids)
  expect_equal(rows[[2]], c("b04", "--", "missing data", "--", "--", "--", "--"))
  b03 <- rows[[1]]
  expect_match(b03[2], "GEE|NB GLM|Poisson GLM")
  expect_match(b03[4], "^[0-9.]+$")
  expect_match(b03[5], "^\\[")
  expect_equal(g$payload$n_aggregates, 24L)
  expect_true(file.exists(file.path(dir, "otis_ruhela_grid.txt")))
  # an analyzer that fails, and one that returns no rows, are reported in place
  testthat::local_mocked_bindings(
    morie_otis_analyze_b03_ruhela_aggregate = function(data, out_dir = NULL) stop("fit exploded"),
    morie_otis_analyze_c01_ruhela_aggregate = function(data, out_dir = NULL) list(warnings = character(), tables = list(list(rows = list()))),
    .package = .ox_pkg)
  g2 <- morie_otis_analyze_ruhela_grid(ds[c("b03", "c01")])
  expect_equal(g2$tables[[1]]$rows[[1]], c("b03", "--", "warn", "fit exploded", "--", "--", "--"))
  expect_equal(g2$tables[[1]]$rows[[7]], c("c01", "--", "no rows", "--", "--", "--", "--"))
  # the per-row arms are replaced: they would otherwise load the live tables for the ids not supplied
  testthat::local_mocked_bindings(
    morie_otis_analyze_a01_ruhela_formulations = function(data, ...) list(warnings = "only 12 cells", tables = list()),
    morie_otis_analyze_b01_ruhela_formulations = function(data, ...) list(warnings = character(), tables = list()),
    morie_otis_analyze_b02_ruhela_formulations = function(data, ...) list(warnings = character(), tables = list(list(rows = list(1)))),
    .package = .ox_pkg)
  m <- morie_otis_analyze_ruhela_master(ds[c("b03", "c01", "c03", "c11", "d01", "d06")], include_per_row = TRUE)
  titles <- vapply(m$tables, `[[`, "", "title")
  expect_length(titles, 4)
  expect_match(titles[1], "^section 1 Aggregate")
  expect_match(titles[2], "^section 2 Per-row")
  expect_match(titles[3], "^section 3 MRM chi-square")
  expect_match(titles[4], "^section 4 Mandela-RF")
  per_row <- m$tables[[2]]$rows
  expect_equal(vapply(per_row, `[`, "", 1), c("a01", "b01", "b02"))
  expect_equal(vapply(per_row, `[`, "", 2), c("warn", "no rows", "see full table"))
  expect_equal(per_row[[1]][3], "only 12 cells")
  expect_equal(m$summary_lines[["Sections"]], 4L)
  expect_true(m$payload$include_per_row)
  chi <- m$tables[[3]]$rows
  expect_equal(unique(vapply(chi, `[`, "", 1)), c("c-series", "d-series"))
  m2 <- morie_otis_analyze_ruhela_master(ds[c("b03", "c01")])
  expect_equal(m2$summary_lines[["Sections"]], 2L)
  expect_false(m2$payload$include_per_row)
})
