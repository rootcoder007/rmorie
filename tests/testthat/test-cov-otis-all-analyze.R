# Coverage for the OTIS per-dataset analyzers: every crosstab, trend and
# ratio is recomputed from the synthetic panel with explicit loops, and the
# aggregate Ruhela formulations are refitted with stats::glm and
# MASS::glm.nb (geepack::geeglm for the clustered row).

.cov_tab <- function(res, title) {
  hit <- Filter(function(t) identical(t$title, title), res$tables)
  expect_length(hit, 1L)
  hit[[1L]]
}

.cov_check_ct <- function(res, df, row, col, value, agg = sum, top = 20L) {
  tab <- .cov_tab(res, sprintf("%s by %s x %s:", value, row, col))
  v <- suppressWarnings(as.numeric(df[[value]]))
  ok <- !is.na(v) & !is.na(df[[row]]) & !is.na(df[[col]])
  rl <- sort(unique(df[[row]][ok]))
  cl <- sort(unique(df[[col]][ok]))
  m <- matrix(0, length(rl), length(cl), dimnames = list(as.character(rl), as.character(cl)))
  for (i in seq_along(rl)) {
    for (j in seq_along(cl)) {
      sel <- ok & df[[row]] == rl[i] & df[[col]] == cl[j]
      if (any(sel)) m[i, j] <- agg(v[sel])
    }
  }
  tot <- rowSums(m)
  expect_identical(tab$headers, c(row, colnames(m), "TOTAL"))
  expect_length(tab$rows, min(top, nrow(m)))
  got_tot <- numeric(0)
  for (r in tab$rows) {
    k <- r[1L]
    expect_equal(as.numeric(r[-1L]), unname(c(trunc(m[k, ]), trunc(tot[[k]]))), tolerance = 0)
    got_tot <- c(got_tot, as.numeric(r[length(r)]))
  }
  expect_false(is.unsorted(-got_tot))
  # the rows kept are the top-`top` totals
  expect_equal(sort(got_tot, decreasing = TRUE), unname(sort(trunc(tot), decreasing = TRUE)[seq_along(got_tot)]), tolerance = 0)
}

.cov_check_trend <- function(res, df, value, yc) {
  tab <- .cov_tab(res, sprintf("%s by %s:", value, yc))
  yrs <- sort(unique(df[[yc]]))
  expect_length(tab$rows, length(yrs))
  for (i in seq_along(yrs)) {
    s <- sum(as.numeric(df[[value]][df[[yc]] == yrs[i]]), na.rm = TRUE)
    expect_equal(as.numeric(tab$rows[[i]]), c(yrs[i], trunc(s)), tolerance = 0)
  }
}

.cov_check_summary <- function(res, df, yc) {
  s <- res$summary_lines
  expect_identical(s[["Rows"]], nrow(df))
  expect_identical(s[["Columns"]], ncol(df))
  y <- as.numeric(df[[yc]])
  expect_identical(s[["Years covered"]], sprintf("%d-%d", as.integer(min(y)), as.integer(max(y))))
  expect_s3_class(res, "morie_otis_analysis_result")
}

test_that("b02 summary statistics, trend and gender-by-region crosstab recompute", {
  d <- morie_synth_otis("b02", n = 40L, seed = 1L)
  res <- morie_otis_analyze_b02(d)
  x <- as.numeric(d$TotalAggregatedDays_Segregation)
  .cov_check_summary(res, d, "EndFiscalYear")
  expect_equal(res$summary_lines[["Mean total days"]], round(mean(x), 2), tolerance = 1e-12)
  expect_equal(res$summary_lines[["Median total days"]], median(x), tolerance = 1e-12)
  expect_identical(res$summary_lines[["Max total days"]], as.integer(max(x)))
  .cov_check_trend(res, d, "TotalAggregatedDays_Segregation", "EndFiscalYear")
  .cov_check_ct(res, d, "Gender", "Region_MostRecentPlacement", "TotalAggregatedDays_Segregation")
  # a panel without the day column keeps only the summary block
  res0 <- morie_otis_analyze_b02(d[, c("EndFiscalYear", "Gender")])
  expect_length(res0$tables, 0L)
  expect_null(res0$summary_lines[["Mean total days"]])
})

test_that("b04 and b08 duration crosstabs take the cell maximum, not the sum", {
  d4 <- morie_synth_otis("b04", n = 40L, seed = 1L)
  r4 <- morie_otis_analyze_b04(d4)
  .cov_check_summary(r4, d4, "EndFiscalYear")
  .cov_check_ct(r4, d4, "Region_AtTimeOfPlacement", "Measure", "NumberConsecutiveDays_Segregation", agg = max)
  d8 <- morie_synth_otis("b08", n = 40L, seed = 2L)
  r8 <- morie_otis_analyze_b08(d8)
  .cov_check_ct(r8, d8, "Institution_AtTimeOfPlacement", "Measure", "NumberConsecutiveDays_Segregation", agg = max, top = 15L)
  # duplicated cells make max and sum differ, so the test can fail on a sum
  dd <- data.frame(EndFiscalYear = 2020L, Region_AtTimeOfPlacement = c("A", "A", "B"),
                   Measure = "Median", NumberConsecutiveDays_Segregation = c(3, 9, 4))
  tab <- morie_otis_analyze_b04(dd)$tables[[1L]]
  expect_identical(tab$rows[[1L]], c("A", "9", "9"))
  expect_identical(tab$rows[[2L]], c("B", "4", "4"))
})

test_that("b05, b06, b09 crosstabs recompute", {
  d5 <- morie_synth_otis("b05", n = 40L, seed = 3L)
  r5 <- morie_otis_analyze_b05(d5)
  .cov_check_summary(r5, d5, "EndFiscalYear")
  .cov_check_ct(r5, d5, "Consecutive_Duration", "EndFiscalYear", "Number_SegregationPlacements")
  d6 <- morie_synth_otis("b06", n = 40L, seed = 4L)
  r6 <- morie_otis_analyze_b06(d6)
  .cov_check_ct(r6, d6, "Reason", "EndFiscalYear", "Number_SegregationPlacements")
  .cov_check_ct(r6, d6, "Reason", "Gender", "Number_SegregationPlacements")
  d9 <- morie_synth_otis("b09", n = 40L, seed = 5L)
  r9 <- morie_otis_analyze_b09(d9)
  .cov_check_ct(r9, d9, "NumberPlacements_Segregation", "Gender", "NumberIndividuals_Segregation")
})

test_that("b07 per-row alert shares recompute", {
  d <- morie_synth_otis("b07", n = 30L, seed = 6L)
  res <- morie_otis_analyze_b07(d)
  rows <- res$tables[[1L]]$rows
  expect_length(rows, nrow(d))
  for (i in seq_len(nrow(d))) {
    w <- d$Number_Segregation_Placements_With_Alert[i]
    wo <- d$Number_Segregation_Placements_Without_Alert[i]
    expect_identical(rows[[i]], c(as.character(d$EndFiscalYear[i]), d$Alert_Type[i], d$Gender[i],
                                  as.character(w), as.character(wo),
                                  sprintf("%.1f%%", if (w + wo > 0) 100 * w / (w + wo) else 0)))
  }
  expect_length(morie_otis_analyze_b07(d[, 1:3])$tables[[1L]]$rows, 0L)
})

test_that("c01 cohort ratios and c03 race totals recompute", {
  d <- morie_synth_otis("c01", n = 25L, seed = 7L)
  res <- morie_otis_analyze_c01(d)
  .cov_check_summary(res, d, "EndFiscalYear")
  for (i in seq_len(nrow(d))) {
    cu <- d$NumberIndividuals_InCustody[i]
    rc <- d$NumberIndividuals_RestrictiveConfinement[i]
    sg <- d$NumberIndividuals_Segregation[i]
    expect_identical(res$tables[[1L]]$rows[[i]][6:7],
                     c(sprintf("%.1f%%", if (cu > 0) 100 * rc / cu else 0),
                       sprintf("%.1f%%", if (cu > 0) 100 * sg / cu else 0)))
  }
  d3 <- morie_synth_otis("c03", n = 40L, seed = 8L)
  r3 <- morie_otis_analyze_c03(d3)
  rows <- r3$tables[[1L]]$rows
  races <- unique(d3$Race)
  expect_length(rows, length(races))
  cust <- vapply(rows, function(r) as.numeric(r[2L]), 1)
  expect_false(is.unsorted(-cust))
  for (r in rows) {
    sel <- d3$Race == r[1L]
    cu <- sum(d3$NumberIndividuals_InCustody[sel])
    rc <- sum(d3$NumberIndividuals_RestrictiveConfinement[sel])
    sg <- sum(d3$NumberIndividuals_Segregation[sel])
    expect_identical(r, c(r[1L], as.character(c(cu, rc, sg)),
                          sprintf("%.1f%%", 100 * rc / cu), sprintf("%.1f%%", 100 * sg / cu)))
  }
  expect_match(r3$interpretation, "Indigenous")
})

test_that("c02 and c04-c12 crosstabs recompute", {
  chk <- function(id, fn, row, col, value, agg = sum, top = 20L, seed = 9L) {
    d <- morie_synth_otis(id, n = 40L, seed = seed)
    r <- fn(d)
    .cov_check_summary(r, d, "EndFiscalYear")
    .cov_check_ct(r, d, row, col, value, agg = agg, top = top)
    expect_match(r$title, paste0("^", id, " -- "))
  }
  rc <- "NumberIndividuals_RestrictiveConfinement"
  chk("c02", morie_otis_analyze_c02, "Institution_MostRecentPlacement", "EndFiscalYear", rc, top = 15L)
  chk("c04", morie_otis_analyze_c04, "Race", "Region_MostRecentPlacement", rc)
  chk("c05", morie_otis_analyze_c05, "Religion", "Region_MostRecentPlacement", rc)
  chk("c06", morie_otis_analyze_c06, "Age_Category", "Region_MostRecentPlacement", rc)
  chk("c08", morie_otis_analyze_c08, "Religion", "Gender", rc)
  chk("c09", morie_otis_analyze_c09, "Age_Category", "Gender", rc)
  chk("c10", morie_otis_analyze_c10, "Institution_MostRecentPlacement", "Measure",
      "TotalAggregatedDays_RestrictiveConfinement", agg = max, top = 15L)
  chk("c11", morie_otis_analyze_c11, "Aggregate_Duration", "EndFiscalYear", rc)
  chk("c12", morie_otis_analyze_c12, "Region_MostRecentPlacement", "Measure",
      "TotalAggregatedDays_RestrictiveConfinement", agg = max)
  d7 <- morie_synth_otis("c07", n = 40L, seed = 10L)
  r7 <- morie_otis_analyze_c07(d7)
  .cov_check_ct(r7, d7, "Alert_Type", "Gender", rc)
  .cov_check_ct(r7, d7, "Alert_Type", "EndFiscalYear", "NumberIndividuals_Segregation")
})

test_that("d01 frequency tables and d02-d07 trends/crosstabs recompute", {
  d <- morie_synth_otis("d01", n = 40L, seed = 11L)
  res <- morie_otis_analyze_d01(d)
  .cov_check_summary(res, d, "Year")
  expect_identical(res$summary_lines[["Distinct individuals"]], length(unique(d$UniqueIndividual_ID)))
  cols <- c("By region:" = "Region_AtTimeOfDeath", "By housing unit type:" = "HousingUnit_Type",
            "By medical cause:" = "MedicalCauseOfDeath", "By means of death:" = "MeansOfDeath")
  for (ttl in names(cols)) {
    tab <- .cov_tab(res, ttl)
    cnt <- vapply(tab$rows, function(r) as.numeric(r[2L]), 1)
    expect_false(is.unsorted(-cnt))
    expect_equal(sum(cnt), nrow(d), tolerance = 0)
    for (r in tab$rows) expect_equal(as.numeric(r[2L]), sum(d[[cols[[ttl]]]] == r[1L]), tolerance = 0)
  }
  simple <- list(d02 = list(morie_otis_analyze_d02, "Gender"), d03 = list(morie_otis_analyze_d03, "Race"),
                 d04 = list(morie_otis_analyze_d04, "Religion"), d05 = list(morie_otis_analyze_d05, "Age_Category"))
  for (id in names(simple)) {
    dd <- morie_synth_otis(id, n = 40L, seed = 12L)
    rr <- simple[[id]][[1L]](dd)
    .cov_check_trend(rr, dd, "Number_CustodialDeaths", "Year")
    .cov_check_ct(rr, dd, simple[[id]][[2L]], "Year", "Number_CustodialDeaths")
  }
  d6 <- morie_synth_otis("d06", n = 40L, seed = 13L)
  .cov_check_ct(morie_otis_analyze_d06(d6), d6, "MedicalCauseOfDeath", "Alert_Type", "Number_CustodialDeaths")
  d7 <- morie_synth_otis("d07", n = 40L, seed = 14L)
  .cov_check_ct(morie_otis_analyze_d07(d7), d7, "HousingUnit_Type", "Alert_Type", "Number_CustodialDeaths")
})

# ---- aggregate Ruhela formulations -----------------------------------------

.cov_glm_irr <- function(work, trt, out, cov, yc) {
  parts <- c(sprintf("factor(`%s`)", c(trt, cov)), sprintf("factor(`%s`)", yc))
  fml <- stats::as.formula(sprintf("`%s` ~ %s", out, paste(parts, collapse = " + ")))
  fit <- suppressWarnings(stats::glm(fml, data = work, family = stats::poisson(),
                                     control = stats::glm.control(maxit = 200L)))
  list(fml = fml, irr = unname(exp(stats::coef(fit)[2L])))
}

.cov_agg_specs <- list(
  b03 = list(fn = morie_otis_analyze_b03_ruhela_aggregate, alias = morie_otis_analyze_b03_mrm_aggregate, trt = function(w) as.integer(tolower(trimws(w$Alert_Presence)) %in% c("yes", "y", "true", "1")),
             out = "Number_SegregationPlacements", cov = c("Alert_Type", "Region_AtTimeOfPlacement")),
  b04 = list(fn = morie_otis_analyze_b04_ruhela_aggregate, alias = morie_otis_analyze_b04_mrm_aggregate, median = TRUE, trt = function(w) as.integer(tolower(w$Gender) == "female"),
             out = "NumberConsecutiveDays_Segregation", cov = "Region_AtTimeOfPlacement"),
  b06 = list(fn = morie_otis_analyze_b06_ruhela_aggregate, alias = morie_otis_analyze_b06_mrm_aggregate, trt = function(w) as.integer(grepl("disciplinary", tolower(w$Reason), fixed = TRUE)),
             out = "Number_SegregationPlacements", cov = c("Gender", "Region_AtTimeOfPlacement")),
  b08 = list(fn = morie_otis_analyze_b08_ruhela_aggregate, alias = morie_otis_analyze_b08_mrm_aggregate, median = TRUE, trt = function(w) as.integer(tolower(w$Gender) == "female"),
             out = "NumberConsecutiveDays_Segregation", cov = "Region_AtTimeOfPlacement"),
  b09 = list(fn = morie_otis_analyze_b09_ruhela_aggregate, alias = morie_otis_analyze_b09_mrm_aggregate, trt = function(w) as.integer(tolower(w$Gender) == "female"),
             out = "NumberIndividuals_Segregation", cov = "NumberPlacements_Segregation"),
  c01 = list(fn = morie_otis_analyze_c01_ruhela_aggregate, alias = morie_otis_analyze_c01_mrm_aggregate, trt = function(w) as.integer(tolower(w$Gender) == "female"),
             out = "NumberIndividuals_RestrictiveConfinement", cov = character(0)),
  c02 = list(fn = morie_otis_analyze_c02_ruhela_aggregate, alias = morie_otis_analyze_c02_mrm_aggregate, trt = function(w) as.integer(tolower(w$Gender) == "female"),
             out = "NumberIndividuals_RestrictiveConfinement", cov = "Region_MostRecentPlacement"),
  c03 = list(fn = morie_otis_analyze_c03_ruhela_aggregate, alias = morie_otis_analyze_c03_mrm_aggregate, trt = function(w) as.integer(tolower(w$Race) == "indigenous"),
             out = "NumberIndividuals_RestrictiveConfinement", cov = "Gender"),
  c04 = list(fn = morie_otis_analyze_c04_ruhela_aggregate, alias = morie_otis_analyze_c04_mrm_aggregate, trt = function(w) as.integer(tolower(w$Race) == "indigenous"),
             out = "NumberIndividuals_RestrictiveConfinement", cov = "Region_MostRecentPlacement"),
  c05 = list(fn = morie_otis_analyze_c05_ruhela_aggregate, alias = morie_otis_analyze_c05_mrm_aggregate, trt = function(w) as.integer(!(tolower(trimws(w$Religion)) %in% c("christian", "no religion", "unknown or not reported"))),
             out = "NumberIndividuals_RestrictiveConfinement", cov = "Region_MostRecentPlacement"),
  c06 = list(fn = morie_otis_analyze_c06_ruhela_aggregate, alias = morie_otis_analyze_c06_mrm_aggregate, trt = function(w) as.integer(grepl("50", w$Age_Category)),
             out = "NumberIndividuals_RestrictiveConfinement", cov = "Region_MostRecentPlacement"),
  c07 = list(fn = morie_otis_analyze_c07_ruhela_aggregate, alias = morie_otis_analyze_c07_mrm_aggregate, prep = function(d) {
    d$Alert_Type[seq(1L, nrow(d), 3L)] <- "No Alert"
    d
  }, trt = function(w) as.integer(!(tolower(trimws(w$Alert_Type)) %in% c("no alert", "none", "no_alert", "no"))),
             out = "NumberIndividuals_RestrictiveConfinement", cov = c("Alert_Type", "Gender")),
  c08 = list(fn = morie_otis_analyze_c08_ruhela_aggregate, alias = morie_otis_analyze_c08_mrm_aggregate, trt = function(w) as.integer(!(tolower(trimws(w$Religion)) %in% c("christian", "no religion", "unknown or not reported"))),
             out = "NumberIndividuals_RestrictiveConfinement", cov = "Gender"),
  c09 = list(fn = morie_otis_analyze_c09_ruhela_aggregate, alias = morie_otis_analyze_c09_mrm_aggregate, trt = function(w) as.integer(grepl("50", w$Age_Category)),
             out = "NumberIndividuals_RestrictiveConfinement", cov = "Gender"),
  c10 = list(fn = morie_otis_analyze_c10_ruhela_aggregate, alias = morie_otis_analyze_c10_mrm_aggregate, median = TRUE, trt = function(w) as.integer(tolower(w$Gender) == "female"),
             out = "TotalAggregatedDays_RestrictiveConfinement", cov = "Region_MostRecentPlacement"),
  c11 = list(fn = morie_otis_analyze_c11_ruhela_aggregate, alias = morie_otis_analyze_c11_mrm_aggregate, trt = function(w) as.integer(w$Aggregate_Duration %in% c("16 to 20 days", "21 to 25 days", "26 to 30 days", "Greater than 30 days")),
             out = "NumberIndividuals_RestrictiveConfinement", cov = character(0)),
  c12 = list(fn = morie_otis_analyze_c12_ruhela_aggregate, alias = morie_otis_analyze_c12_mrm_aggregate, median = TRUE, trt = function(w) as.integer(tolower(w$Gender) == "female"),
             out = "TotalAggregatedDays_RestrictiveConfinement", cov = "Region_MostRecentPlacement"),
  d02 = list(fn = morie_otis_analyze_d02_ruhela_aggregate, alias = morie_otis_analyze_d02_mrm_aggregate, yc = "Year", trt = function(w) as.integer(tolower(w$Gender) == "female"),
             out = "Number_CustodialDeaths", cov = character(0)),
  d03 = list(fn = morie_otis_analyze_d03_ruhela_aggregate, alias = morie_otis_analyze_d03_mrm_aggregate, yc = "Year", trt = function(w) as.integer(tolower(w$Race) == "indigenous"),
             out = "Number_CustodialDeaths", cov = character(0)),
  d04 = list(fn = morie_otis_analyze_d04_ruhela_aggregate, alias = morie_otis_analyze_d04_mrm_aggregate, yc = "Year", trt = function(w) as.integer(!(tolower(trimws(w$Religion)) %in% c("christian", "no religion", "unknown or not reported"))),
             out = "Number_CustodialDeaths", cov = character(0)),
  d05 = list(fn = morie_otis_analyze_d05_ruhela_aggregate, alias = morie_otis_analyze_d05_mrm_aggregate, yc = "Year", trt = function(w) as.integer(grepl("50", w$Age_Category)),
             out = "Number_CustodialDeaths", cov = character(0))
)

test_that("aggregate Ruhela Poisson IRRs match a direct stats::glm refit, and mrm aliases are the same functions", {
  for (id in names(.cov_agg_specs)) {
    sp <- .cov_agg_specs[[id]]
    fn <- sp$fn
    expect_identical(sp$alias, fn)
    d <- morie_synth_otis(id, n = 60L, seed = 21L)
    if (!is.null(sp$prep)) d <- sp$prep(d)
    res <- fn(d)
    w <- d
    if (isTRUE(sp$median)) w <- w[trimws(w$Measure) == "Median", , drop = FALSE]
    yc <- if (is.null(sp$yc)) "EndFiscalYear" else sp$yc
    trt <- res$payload$treatment
    w[[trt]] <- sp$trt(w)
    exp_fit <- .cov_glm_irr(w, trt, sp$out, sp$cov, yc)
    pois <- res$payload$estimands[[1L]]
    expect_identical(pois$label, "Poisson")
    expect_equal(pois$irr, exp_fit$irr, tolerance = 1e-9, info = id)
    expect_equal(c(pois$ci_lo, pois$ci_hi), exp(log(pois$irr) + c(-1.96, 1.96) * pois$se), tolerance = 1e-12)
    expect_identical(res$payload$n_rows, nrow(w))
    expect_identical(res$summary_lines[["Total outcome count"]], as.integer(sum(w[[sp$out]])))
  }
})

test_that("aggregate Ruhela NB rows agree with MASS::glm.nb", {
  skip_if_not_installed("MASS")
  for (id in c("c02", "d03", "b09")) {
    sp <- .cov_agg_specs[[id]]
    d <- morie_synth_otis(id, n = 60L, seed = 21L)
    res <- sp$fn(d)
    nb <- Filter(function(e) identical(e$label, "NB"), res$payload$estimands)
    expect_length(nb, 1L)
    w <- d
    trt <- res$payload$treatment
    w[[trt]] <- sp$trt(w)
    fml <- .cov_glm_irr(w, trt, sp$out, sp$cov, if (is.null(sp$yc)) "EndFiscalYear" else sp$yc)$fml
    ref <- suppressWarnings(MASS::glm.nb(fml, data = w, control = stats::glm.control(maxit = 200L)))
    # theta is found by an alternating ML iteration; both arms stop at
    # glm.control's default epsilon, hence the looser iterative tolerance
    expect_equal(nb[[1L]]$irr, unname(exp(stats::coef(ref)[2L])), tolerance = 1e-6, info = id)
    expect_equal(nb[[1L]]$aic, stats::AIC(ref), tolerance = 1e-6, info = id)
  }
})

test_that("clustered GEE row agrees with geepack::geeglm (exchangeable)", {
  skip_if_not_installed("geepack")
  d <- morie_synth_otis("c01", n = 60L, seed = 22L)
  res <- morie_otis_analyze_c01_ruhela_aggregate_region_cluster(d)
  expect_identical(morie_otis_analyze_c01_mrm_aggregate_region_cluster, morie_otis_analyze_c01_ruhela_aggregate_region_cluster)
  rows <- res$tables[[1L]]$rows
  gee <- Filter(function(r) startsWith(r[1L], "GEE-Poisson"), rows)
  expect_length(gee, 1L)
  w <- d[order(d$EndFiscalYear), , drop = FALSE]
  w$T_female <- as.integer(tolower(w$Gender) == "female")
  w$cid <- as.integer(factor(w$EndFiscalYear))
  ref <- geepack::geeglm(NumberIndividuals_RestrictiveConfinement ~ factor(T_female) + factor(EndFiscalYear),
                         id = cid, data = w, family = stats::poisson(), corstr = "exchangeable")
  # the table stores the IRR rounded to 4 decimals
  expect_equal(as.numeric(gee[[1L]][3L]), round(unname(exp(stats::coef(ref)[2L])), 4), tolerance = 1e-4)
  d4 <- morie_synth_otis("c04", n = 60L, seed = 22L)
  r4 <- morie_otis_analyze_c04_ruhela_aggregate_region_cluster(d4)
  expect_identical(morie_otis_analyze_c04_mrm_aggregate_region_cluster, morie_otis_analyze_c04_ruhela_aggregate_region_cluster)
  expect_identical(r4$payload$cluster_group, "Region_MostRecentPlacement")
  expect_identical(r4$payload$treatment, "T_indigenous")
})

test_that("b07 aggregate pivots to long form: one with-alert and one without-alert row per cell", {
  d <- morie_synth_otis("b07", n = 30L, seed = 23L)
  res <- morie_otis_analyze_b07_ruhela_aggregate(d)
  expect_identical(morie_otis_analyze_b07_mrm_aggregate, morie_otis_analyze_b07_ruhela_aggregate)
  long <- data.frame(EndFiscalYear = rep(d$EndFiscalYear, 2L), Alert_Type = rep(d$Alert_Type, 2L),
                     Gender = rep(d$Gender, 2L),
                     n_placements = c(d$Number_Segregation_Placements_With_Alert, d$Number_Segregation_Placements_Without_Alert),
                     T_alert = rep(1:0, each = nrow(d)))
  expect_identical(res$payload$n_rows, 2L * nrow(d))
  expect_equal(res$payload$estimands[[1L]]$irr, .cov_glm_irr(long, "T_alert", "n_placements", c("Alert_Type", "Gender"), "EndFiscalYear")$irr,
               tolerance = 1e-9)
})

test_that("aggregate Ruhela degenerate inputs: missing columns, zero outcomes, b05 not applicable", {
  st <- morie_otis_analyze_c03_ruhela_aggregate(data.frame(x = 1))
  expect_identical(st$summary_lines$status, "stub")
  expect_match(st$warnings, "missing required columns")
  d <- morie_synth_otis("d02", n = 20L, seed = 24L)
  d$Number_CustodialDeaths <- 0L
  z <- morie_otis_analyze_d02_ruhela_aggregate(d)
  expect_identical(z$summary_lines$status, "empty cell-table or zero outcome")
  b5 <- morie_otis_analyze_b05_ruhela_aggregate(morie_synth_otis("b05", n = 20L, seed = 1L))
  expect_match(b5$warnings, "not applicable|no demographic treatment")
  expect_length(b5$tables, 0L)
  b4 <- morie_synth_otis("b04", n = 20L, seed = 1L)
  b4$Measure <- "Mode"
  expect_identical(morie_otis_analyze_b04_ruhela_aggregate(b4)$warnings, "b04 has no Median rows after filter")
  c7 <- morie_synth_otis("c07", n = 20L, seed = 1L)
  expect_match(morie_otis_analyze_c07_ruhela_aggregate(c7)$warnings, "degenerate")
})

test_that("not-yet-ported DLRM entry points return the documented stub, and their aliases are identical", {
  pairs <- list(
    list(morie_otis_analyze_a01_dlrm, morie_otis_analyze_a01_ruhela_formulations, "morie_otis_analyze_a01_ruhela_formulations"),
    list(morie_otis_analyze_a01_mrm, morie_otis_analyze_a01_ruhela_formulations, "morie_otis_analyze_a01_ruhela_formulations"),
    list(morie_otis_analyze_b01_dlrm, morie_otis_analyze_b01_ruhela_formulations, "morie_otis_analyze_b01_ruhela_formulations"),
    list(morie_otis_analyze_b01_mrm, morie_otis_analyze_b01_ruhela_formulations, "morie_otis_analyze_b01_ruhela_formulations"),
    list(morie_otis_analyze_b02_dlrm, morie_otis_analyze_b02_ruhela_formulations, "morie_otis_analyze_b02_ruhela_formulations"),
    list(morie_otis_analyze_b02_mrm, morie_otis_analyze_b02_ruhela_formulations, "morie_otis_analyze_b02_ruhela_formulations"),
    list(morie_otis_analyze_a01_mrm_alt_age, morie_otis_analyze_a01_ruhela_alt_age, "morie_otis_analyze_a01_ruhela_alt_age"),
    list(morie_otis_analyze_a01_mrm_alt_gender, morie_otis_analyze_a01_ruhela_alt_gender, "morie_otis_analyze_a01_ruhela_alt_gender"),
    list(morie_otis_analyze_a01_mrm_alt_toronto, morie_otis_analyze_a01_ruhela_alt_toronto, "morie_otis_analyze_a01_ruhela_alt_toronto"),
    list(morie_otis_analyze_b01_mrm_alt_age, morie_otis_analyze_b01_ruhela_alt_age, "morie_otis_analyze_b01_ruhela_alt_age"),
    list(morie_otis_analyze_b01_mrm_alt_gender, morie_otis_analyze_b01_ruhela_alt_gender, "morie_otis_analyze_b01_ruhela_alt_gender"),
    list(morie_otis_analyze_b01_mrm_alt_toronto, morie_otis_analyze_b01_ruhela_alt_toronto, "morie_otis_analyze_b01_ruhela_alt_toronto"),
    list(morie_otis_analyze_b02_mrm_alt_age, morie_otis_analyze_b02_ruhela_alt_age, "morie_otis_analyze_b02_ruhela_alt_age"),
    list(morie_otis_analyze_b02_mrm_alt_region, morie_otis_analyze_b02_ruhela_alt_region, "morie_otis_analyze_b02_ruhela_alt_region"),
    list(morie_otis_analyze_a01_mrm_per_year, morie_otis_analyze_a01_ruhela_per_year, "morie_otis_analyze_a01_ruhela_per_year"),
    list(morie_otis_analyze_b01_mrm_per_year, morie_otis_analyze_b01_ruhela_per_year, "morie_otis_analyze_b01_ruhela_per_year"),
    list(morie_otis_analyze_a01_mrm_subgroup_female, morie_otis_analyze_a01_ruhela_subgroup_female, "morie_otis_analyze_a01_ruhela_subgroup_female"),
    list(morie_otis_analyze_a01_mrm_subgroup_male, morie_otis_analyze_a01_ruhela_subgroup_male, "morie_otis_analyze_a01_ruhela_subgroup_male"),
    list(morie_otis_analyze_b01_mrm_subgroup_female, morie_otis_analyze_b01_ruhela_subgroup_female, "morie_otis_analyze_b01_ruhela_subgroup_female"),
    list(morie_otis_analyze_b01_mrm_subgroup_male, morie_otis_analyze_b01_ruhela_subgroup_male, "morie_otis_analyze_b01_ruhela_subgroup_male")
  )
  for (pr in pairs) {
    expect_identical(pr[[1L]], pr[[2L]])
    r <- pr[[1L]]()
    expect_identical(r$summary_lines$status, "stub")
    expect_match(r$summary_lines$reason, pr[[3L]], fixed = TRUE)
    expect_s3_class(r, "morie_otis_analysis_result")
  }
  py <- morie_otis_analyze_ruhela_per_year(data.frame(), ds_id = "zz9", treatment = "T", outcome = "Y", covariates = "G")
  expect_identical(py$summary_lines$status, "stub")
  expect_match(py$title, "morie_otis_analyze_ruhela_per_year(zz9)", fixed = TRUE)
})
