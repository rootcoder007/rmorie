# SPDX-License-Identifier: AGPL-3.0-or-later
#' Ontario Restrictive Confinement (OTIS) primitive analyses
#'
#' Six lightweight callables mirroring the Python module
#' \code{morie.otis}: regional-placement matrices, alert-state combo
#' encoding, regional volatility, restrictive-confinement trends,
#' descriptive statistics, and a partialled-out Plug-in DML (PLR)
#' ATE/ATT estimator. Each public callable returns a named list with
#' classes \code{c("morie_otis_result", "morie_rich_result", "list")}
#' carrying \code{summary_lines}, optional \code{tables}, a
#' plain-language \code{interpretation}, and machine-readable
#' \code{payload} entries.
#'
#' Data sources: anonymized Ontario MCSCS placement records released
#' under the Jahn v. Ontario (2020) settlement. The canonical OTIS
#' table has 76,934 rows (FY 2022/23 -- 2024/25). See
#' \code{\link{morie_otis_load}} in \code{otis_analyze.R} for the
#' canonical loader.
#'
#' Year-lock invariant
#' -------------------
#' OTIS \code{UniqueIndividual_ID} (format \code{YYYY-XXXXX-AA}) is
#' randomly reassigned every fiscal year and re-randomized per dataset
#' file even within a year. The \code{variable_taxonomy.R} registry
#' enforces \code{cross_year_safe = FALSE} for this column. Every
#' aggregation below operates within \code{EndFiscalYear}; cross-year
#' joins on the ID are forbidden by design.
#'
#' @references
#'  Ontario Ministry of the Solicitor General (2025). Restrictive
#'  Confinement Detailed Dataset. \url{https://data.ontario.ca}.
#'
#'  Jahn v. Ontario (2020). Settlement Agreement -- Inmate Data
#'  Disclosure.
#'
#'  Chernozhukov, V. et al. (2018). Double/debiased machine learning
#'  for treatment and structural parameters. \emph{Econometrics
#'  Journal}, 21(1), C1-C68.
#' @name morie_otis_primitives
NULL


# ---------------------------------------------------------------------------
# Constants (mirror morie.otis)
# ---------------------------------------------------------------------------

.OTIS_REGIONS <- c("Central", "Eastern", "Northern", "Toronto", "Western")
.OTIS_AGE_GROUPS <- c("18 to 24", "25 to 49", "50+")


# ---------------------------------------------------------------------------
# Internal result constructor
# ---------------------------------------------------------------------------

.otis_result <- function(title,
                          summary_lines = list(),
                          tables = list(),
                          interpretation = "",
                          warnings = character(0),
                          payload = list()) {
  out <- list(
    title = title,
    summary_lines = summary_lines,
    tables = tables,
    interpretation = interpretation,
    warnings = warnings,
    payload = payload
  )
  class(out) <- c("morie_otis_result", "morie_rich_result", "list")
  out
}


# Tolerant Yes/No/1/0/TRUE -> integer 0/1
.otis_binarise <- function(s) {
  if (is.logical(s)) {
    return(as.integer(s))
  }
  if (is.numeric(s)) {
    s[is.na(s)] <- 0
    return(as.integer(s > 0))
  }
  v <- tolower(trimws(as.character(s)))
  as.integer(v %in% c("yes", "y", "true", "t", "1"))
}


# ---------------------------------------------------------------------------
# 1. Regional placement matrix
# ---------------------------------------------------------------------------

#' Regional placement matrix by age group
#'
#' Builds a count matrix (age x region) and the row-normalised
#' proportion matrix of unique-individual placements for one fiscal
#' year, optionally filtered by gender.
#'
#' @param df data.frame of OTIS placement records.
#' @param year Integer fiscal year (e.g. \code{2024} for FY 2023/24).
#' @param sex Optional gender filter (\code{"Male"} / \code{"Female"});
#'   \code{NULL} = all.
#' @param id_col,age_col,region_col,year_col,gender_col Column names.
#' @return \code{morie_otis_result} list.
#' @examples
#' \dontrun{
#'   df <- morie_otis_load()
#'   morie_otis_rplace(df, year = 2024)
#' }
#' @export
morie_otis_rplace <- function(df, year,
                               sex = NULL,
                               id_col = "unique_individual_id",
                               age_col = "age_category",
                               region_col = "region_at_time_of_placement",
                               year_col = "end_fiscal_year",
                               gender_col = "gender") {
  stopifnot(is.data.frame(df))
  needed <- c(id_col, age_col, region_col, year_col)
  if (!all(needed %in% names(df))) {
    return(.otis_result(
      title = sprintf("OTIS regional placement -- FY %s", year),
      warnings = sprintf("missing column(s): %s",
                         paste(setdiff(needed, names(df)), collapse = ", "))
    ))
  }
  mask <- df[[year_col]] == year &
    !is.na(df[[age_col]]) &
    !is.na(df[[region_col]])
  sub <- df[mask, , drop = FALSE]
  if (!is.null(sex) && gender_col %in% names(df)) {
    sub <- sub[sub[[gender_col]] == sex, , drop = FALSE]
  }
  if (nrow(sub) == 0L) {
    return(.otis_result(
      title = sprintf("OTIS regional placement -- FY %s", year),
      warnings = "no rows after filter"
    ))
  }
  # unique-individual counts per (age, region)
  key <- paste(sub[[age_col]], sub[[region_col]], sep = "\u001f")
  uniq_id <- !duplicated(paste(sub[[id_col]], key))
  s2 <- sub[uniq_id, , drop = FALSE]
  tbl <- table(s2[[age_col]], s2[[region_col]])
  ages <- intersect(.OTIS_AGE_GROUPS, rownames(tbl))
  if (length(ages) == 0L) ages <- rownames(tbl)
  regions <- .OTIS_REGIONS
  M <- matrix(0L, nrow = length(ages), ncol = length(regions),
              dimnames = list(ages, regions))
  for (a in ages) for (r in regions) {
    if (a %in% rownames(tbl) && r %in% colnames(tbl)) {
      M[a, r] <- tbl[a, r]
    }
  }
  row_sums <- rowSums(M)
  P <- M / pmax(row_sums, 1L)
  P[row_sums == 0, ] <- 0

  .otis_result(
    title = sprintf("OTIS regional placement -- FY %s%s", year,
                    if (!is.null(sex)) sprintf(" (sex=%s)", sex) else ""),
    summary_lines = list(
      Year = year,
      `Sex filter` = if (is.null(sex)) "all" else sex,
      `Age groups` = paste(rownames(M), collapse = ", "),
      Regions = paste(colnames(M), collapse = ", "),
      `Total individuals` = sum(M)
    ),
    tables = list(
      list(title = "Counts (age x region):",
           headers = c("age", colnames(M)),
           rows = lapply(seq_len(nrow(M)),
                         function(i) c(rownames(M)[i],
                                       as.integer(M[i, ])))),
      list(title = "Proportions (within age):",
           headers = c("age", colnames(P)),
           rows = lapply(seq_len(nrow(P)),
                         function(i) c(rownames(P)[i],
                                       sprintf("%.1f%%", 100 * P[i, ]))))
    ),
    payload = list(counts = M, props = P, year = year, sex = sex),
    warnings = "OTIS UniqueIndividual_ID is year-locked; counts are within-FY only."
  )
}


# ---------------------------------------------------------------------------
# 2. Alert-state combination encoding
# ---------------------------------------------------------------------------

#' Alert-state combination encoding (8 combos -> complexity index)
#'
#' Encodes 3 binary alert flags into 8 combinations (a1..a8) and
#' aggregates per (id, fiscal year), computing a complexity index
#' \code{ac} = number of distinct combinations observed.
#'
#' @param df data.frame with the three alert columns and id/year cols.
#' @param alert_cols Character-3 of alert column names. Default mirrors
#'   the lower-case Python schema.
#' @param id_col,year_col Column names.
#' @return \code{morie_otis_result}.
#' @export
morie_otis_astcmb <- function(df,
                               alert_cols = c("mental_health_alert",
                                              "suicide_risk_alert",
                                              "suicide_watch_alert"),
                               id_col = "unique_individual_id",
                               year_col = "end_fiscal_year") {
  stopifnot(is.data.frame(df))
  needed <- c(alert_cols, id_col, year_col)
  if (!all(needed %in% names(df))) {
    return(.otis_result(
      title = "OTIS alert-state combinations",
      warnings = sprintf("missing column(s): %s",
                         paste(setdiff(needed, names(df)), collapse = ", "))
    ))
  }
  a <- .otis_binarise(df[[alert_cols[1]]])
  b <- .otis_binarise(df[[alert_cols[2]]])
  cc <- .otis_binarise(df[[alert_cols[3]]])  # not 'c' — shadows base c()
  flags <- data.frame(
    id = df[[id_col]], yr = df[[year_col]],
    a1 = as.integer(a == 1 & b == 0 & cc == 0),
    a2 = as.integer(a == 0 & b == 1 & cc == 0),
    a3 = as.integer(a == 0 & b == 0 & cc == 1),
    a4 = as.integer(a == 1 & b == 1 & cc == 0),
    a5 = as.integer(a == 0 & b == 1 & cc == 1),
    a6 = as.integer(a == 1 & b == 0 & cc == 1),
    a7 = as.integer(a == 1 & b == 1 & cc == 1),
    a8 = as.integer(a == 0 & b == 0 & cc == 0),
    stringsAsFactors = FALSE
  )
  acols <- paste0("a", 1:8)
  agg <- stats::aggregate(flags[, acols], by = list(id = flags$id, yr = flags$yr),
                          FUN = sum)
  # ac = number of distinct combinations observed in this person-year
  agg$ac <- rowSums(agg[, acols] > 0)
  summary_tbl <- as.data.frame(table(ac = agg$ac), stringsAsFactors = FALSE)
  summary_tbl$ac <- as.integer(as.character(summary_tbl$ac))  # not factor levels
  names(summary_tbl) <- c("ac", "n_persons")
  summary_tbl <- summary_tbl[order(as.integer(summary_tbl$ac),
                                    decreasing = TRUE), , drop = FALSE]

  .otis_result(
    title = "OTIS alert-state combination encoding",
    summary_lines = list(
      `Person-year cells` = nrow(agg),
      `Distinct complexity levels` = nrow(summary_tbl)
    ),
    tables = list(list(
      title = "Count by alert-complexity level:",
      headers = c("ac", "n_persons"),
      rows = lapply(seq_len(nrow(summary_tbl)),
                    function(i) c(summary_tbl$ac[i],
                                  as.integer(summary_tbl$n_persons[i])))
    )),
    interpretation = paste(
      "Each (person, fiscal-year) cell is encoded as one of 8 alert",
      "combinations of (mental_health, suicide_risk, suicide_watch).",
      "a8 = no alerts; a7 = all three. The complexity index `ac`",
      "counts how many distinct combinations the person-year occupied.",
      "Higher ac = more concurrent risk-flag patterns within the FY."
    ),
    payload = list(per_person_year = agg, summary = summary_tbl),
    warnings = "OTIS UniqueIndividual_ID is year-locked; ac is within-FY."
  )
}


# ---------------------------------------------------------------------------
# 3. Regional volatility
# ---------------------------------------------------------------------------

#' Regional volatility / placement movement
#'
#' Counts the number of distinct regions an individual was placed in
#' within one fiscal year (union of \code{region_at_time_of_placement}
#' and \code{region_most_recent_placement}).
#'
#' @param df data.frame.
#' @param id_col,year_col,regA_col,regB_col Column names.
#' @return \code{morie_otis_result}.
#' @export
morie_otis_volat <- function(df,
                              id_col = "unique_individual_id",
                              year_col = "end_fiscal_year",
                              regA_col = "region_at_time_of_placement",
                              regB_col = "region_most_recent_placement") {
  stopifnot(is.data.frame(df))
  needed <- c(id_col, year_col, regA_col)
  if (!all(needed %in% names(df))) {
    return(.otis_result(
      title = "OTIS regional volatility",
      warnings = sprintf("missing column(s): %s",
                         paste(setdiff(needed, names(df)), collapse = ", "))
    ))
  }
  has_B <- regB_col %in% names(df)
  key <- paste(df[[id_col]], df[[year_col]], sep = "\u001f")
  split_a <- split(as.character(df[[regA_col]]), key)
  vm <- if (has_B) {
    split_b <- split(as.character(df[[regB_col]]), key)
    vapply(names(split_a), function(k) {
      r <- unique(c(split_a[[k]], split_b[[k]]))
      r <- r[!is.na(r) & nzchar(r)]
      length(r)
    }, integer(1))
  } else {
    vapply(split_a, function(r) {
      r <- unique(r[!is.na(r) & nzchar(r)])
      length(r)
    }, integer(1))
  }
  if (length(vm) == 0L) {
    return(.otis_result(title = "OTIS regional volatility",
                        warnings = "no usable rows"))
  }
  vm_df <- data.frame(cell = names(vm), vm = as.integer(vm),
                       stringsAsFactors = FALSE)
  .otis_result(
    title = "OTIS regional volatility (placement movement)",
    summary_lines = list(
      `Person-year cells` = nrow(vm_df),
      `Mean volatility` = round(mean(vm_df$vm), 3),
      `Median volatility` = stats::median(vm_df$vm),
      Min = min(vm_df$vm),
      Max = max(vm_df$vm)
    ),
    interpretation = paste(
      "Volatility = number of distinct regions a person-year passed",
      "through, taking the union of `region_at_time_of_placement`",
      "and `region_most_recent_placement`. Higher = more movement.",
      "Strictly within-FY -- cross-year mobility is not measurable",
      "because OTIS IDs are year-locked."
    ),
    payload = list(per_person_year = vm_df,
                   mean = mean(vm_df$vm),
                   median = stats::median(vm_df$vm)),
    warnings = "Intra-FY only (OTIS IDs are year-locked)."
  )
}


# ---------------------------------------------------------------------------
# 4. Restrictive-confinement trends
# ---------------------------------------------------------------------------

#' Restrictive-confinement trends over time by region
#'
#' Per-(fiscal year, region) counts of unique individuals and total
#' placements.
#'
#' @param df data.frame.
#' @param id_col,year_col,region_col Column names.
#' @return \code{morie_otis_result} (the trends table is in
#'   \code{payload$trends}).
#' @export
morie_otis_rctrnd <- function(df,
                               id_col = "unique_individual_id",
                               year_col = "end_fiscal_year",
                               region_col = "region_at_time_of_placement") {
  stopifnot(is.data.frame(df))
  needed <- c(id_col, year_col, region_col)
  if (!all(needed %in% names(df))) {
    return(.otis_result(
      title = "OTIS restrictive-confinement trends",
      warnings = sprintf("missing column(s): %s",
                         paste(setdiff(needed, names(df)), collapse = ", "))
    ))
  }
  key <- paste(df[[year_col]], df[[region_col]], sep = "\u001f")
  agg <- lapply(split(seq_len(nrow(df)), key), function(idx) {
    yr <- df[[year_col]][idx[1]]
    rg <- df[[region_col]][idx[1]]
    data.frame(year = yr, region = rg,
               n_individuals = length(unique(df[[id_col]][idx])),
               n_placements = length(idx),
               stringsAsFactors = FALSE)
  })
  trends <- do.call(rbind, agg)
  trends <- trends[order(trends$year, trends$region), , drop = FALSE]
  rownames(trends) <- NULL

  yrs <- sort(unique(trends$year))
  yr_label <- if (length(yrs) >= 2L) sprintf("%s-%s", min(yrs), max(yrs))
              else as.character(yrs[1])

  .otis_result(
    title = "OTIS restrictive-confinement trends over time",
    summary_lines = list(
      `Years observed` = yr_label,
      Rows = nrow(trends)
    ),
    tables = list(list(
      title = "Restrictive-confinement counts by year x region:",
      headers = names(trends),
      rows = lapply(seq_len(nrow(trends)),
                    function(i) as.list(trends[i, , drop = TRUE]))
    )),
    payload = list(trends = trends),
    warnings = "Per-year aggregates (OTIS IDs are year-locked)."
  )
}


# ---------------------------------------------------------------------------
# 5. Full descriptives
# ---------------------------------------------------------------------------

#' Full OTIS descriptive statistics suite
#'
#' Returns unique-individual counts overall and by fiscal year, region,
#' age category, and gender, plus the per-individual placement-count
#' five-number summary.
#'
#' @param df data.frame.
#' @param id_col,year_col Column names.
#' @return \code{morie_otis_result}.
#' @export
morie_otis_otdesc <- function(df,
                               id_col = "unique_individual_id",
                               year_col = "end_fiscal_year") {
  stopifnot(is.data.frame(df))
  if (!id_col %in% names(df)) {
    return(.otis_result(title = "OTIS descriptives",
                        warnings = sprintf("missing id col '%s'", id_col)))
  }
  n_total <- length(unique(df[[id_col]]))
  n_records <- nrow(df)
  tables <- list()

  add_by <- function(col, lab) {
    if (!col %in% names(df)) return(NULL)
    g <- aggregate(df[[id_col]],
                   by = list(group = df[[col]]),
                   FUN = function(x) length(unique(x)))
    names(g) <- c(col, "n")
    tables[[lab]] <<- list(
      title = sprintf("%s:", lab),
      headers = names(g),
      rows = lapply(seq_len(nrow(g)), function(i) as.list(g[i, , drop = TRUE]))
    )
  }
  add_by(year_col, "n_by_year")
  add_by("region_at_time_of_placement", "n_by_region")
  add_by("age_category", "n_by_age")
  add_by("gender", "n_by_gender")

  # placement-count distribution
  freq <- as.integer(table(df[[id_col]]))
  q <- as.numeric(stats::quantile(freq, c(0, 0.25, 0.5, 0.75, 1),
                                  names = FALSE))
  pmnt_summary <- list(min = q[1], q1 = q[2], median = q[3],
                        mean = mean(freq), q3 = q[4], max = q[5])

  .otis_result(
    title = "OTIS descriptives",
    summary_lines = list(
      `Total individuals (unique)` = n_total,
      `Total records` = n_records,
      `Mean placements/person` = round(pmnt_summary$mean, 2),
      `Median placements/person` = pmnt_summary$median,
      `Max placements/person` = pmnt_summary$max,
      `Tables emitted` = length(tables)
    ),
    tables = unname(tables),
    payload = list(
      n_total = n_total, n_records = n_records,
      placement_dist = pmnt_summary
    )
  )
}


# ---------------------------------------------------------------------------
# 6. Cross-fitted DML (PLR) ATE/ATT
# ---------------------------------------------------------------------------

#' Cross-fitted partially linear DML (ATE/ATT) on OTIS
#'
#' Wraps a Frisch-Waugh-Lovell partialling-out estimator with
#' \code{n_folds} cross-fitting on the OLS nuisance functions
#' \eqn{E[Y|X]} and \eqn{E[D|X]}, then regresses outcome residuals on
#' treatment residuals for the ATE; heteroskedasticity-robust standard
#' errors. ATT is the ATE divided by the treated share (a simple
#' weighting approximation; for the production-grade DML use
#' \pkg{DoubleML}).
#'
#' Categorical covariates are dummy-coded with \code{model.matrix}.
#'
#' @param df data.frame.
#' @param outcome,treatment Column names.
#' @param covariates Character vector of covariate column names. If
#'   \code{NULL}, defaults to the standard OTIS set.
#' @param n_folds Integer fold count (default \code{3L}).
#' @param seed Integer RNG seed.
#' @return \code{morie_otis_result}.
#' @references
#'  Chernozhukov, V. et al. (2018). Double/debiased machine learning
#'  for treatment and structural parameters. \emph{Econometrics
#'  Journal}, 21(1), C1-C68.
#' @export
morie_otis_otdml <- function(df,
                              outcome = "Y", treatment = "D",
                              covariates = NULL,
                              n_folds = 3L,
                              seed = 123L) {
  stopifnot(is.data.frame(df))
  if (is.null(covariates)) {
    covariates <- c("gender", "age_category",
                    "region_at_time_of_placement",
                    "region_most_recent_placement")
  }
  needed <- c(outcome, treatment, covariates)
  miss <- setdiff(needed, names(df))
  if (length(miss)) {
    return(.otis_result(title = "OTIS DML PLR",
                        warnings = sprintf("missing column(s): %s",
                                           paste(miss, collapse = ", "))))
  }
  data <- df[, needed, drop = FALSE]
  data <- data[stats::complete.cases(data), , drop = FALSE]
  if (nrow(data) < 20L) {
    return(.otis_result(title = "OTIS DML PLR",
                        warnings = sprintf("only %d complete rows", nrow(data))))
  }
  # Dummy-code categoricals
  rhs <- paste(covariates, collapse = " + ")
  X <- stats::model.matrix(stats::as.formula(paste("~", rhs)), data = data)
  # Drop intercept; partialling adds it back implicitly
  X <- X[, colnames(X) != "(Intercept)", drop = FALSE]
  y <- as.numeric(data[[outcome]])
  d <- as.numeric(data[[treatment]])
  n <- length(y)

  set.seed(seed)
  perm <- sample.int(n)
  fold_size <- n %/% n_folds
  y_res <- numeric(n)
  d_res <- numeric(n)

  for (k in seq_len(n_folds)) {
    lo <- (k - 1L) * fold_size + 1L
    hi <- if (k == n_folds) n else k * fold_size
    test_idx <- perm[lo:hi]
    train_idx <- setdiff(perm, test_idx)
    # OLS nuisance fits (closed form; cheap and portable)
    fit_y <- stats::lm.fit(X[train_idx, , drop = FALSE], y[train_idx])
    fit_d <- stats::lm.fit(X[train_idx, , drop = FALSE], d[train_idx])
    by_ <- fit_y$coefficients
    bd_ <- fit_d$coefficients
    by_[is.na(by_)] <- 0
    bd_[is.na(bd_)] <- 0
    y_res[test_idx] <- y[test_idx] - X[test_idx, , drop = FALSE] %*% by_
    d_res[test_idx] <- d[test_idx] - X[test_idx, , drop = FALSE] %*% bd_
  }

  # ATE = partialled-out OLS coefficient
  bread <- mean(d_res^2)
  if (bread <= 0) {
    return(.otis_result(title = "OTIS DML PLR",
                        warnings = "treatment fully explained by X (no residual variation)"))
  }
  ate <- as.numeric(sum(d_res * y_res) / sum(d_res^2))
  resid <- y_res - d_res * ate
  meat <- mean((d_res^2) * (resid^2))
  se <- sqrt(meat / (bread^2 * n))
  z <- if (se > 0) ate / se else 0
  # Use lower.tail=FALSE to avoid 1-pnorm underflow for large |z|
  pval <- 2 * stats::pnorm(-abs(z))

  p_treat <- mean(d)
  if (p_treat > 0) {
    att <- ate / p_treat
    att_se <- se / p_treat
    att_z <- att / att_se
    att_p <- 2 * stats::pnorm(-abs(att_z))
  } else {
    att <- ate
    att_se <- se
    att_p <- 1
  }

  .otis_result(
    title = sprintf("OTIS DML PLR (cross-fitted): %s -> %s",
                    treatment, outcome),
    summary_lines = list(
      Method = "PLR cross-fitted (OLS nuisance)",
      Treatment = treatment, Outcome = outcome,
      Covariates = paste(covariates, collapse = ", "),
      n = n, `Folds` = n_folds
    ),
    tables = list(list(
      title = "Causal-effect estimates:",
      headers = c("Estimand", "Estimate", "SE", "p-value"),
      rows = list(
        list("ATE", sprintf("%.4f", ate), sprintf("%.4f", se),
             format(signif(pval, 3))),
        list("ATT", sprintf("%.4f", att), sprintf("%.4f", att_se),
             format(signif(att_p, 3)))
      )
    )),
    interpretation = paste(
      "ATE = average effect of D on Y across the OTIS population;",
      "ATT = treatment-weighted approximation among the treated.",
      "p-values use the asymptotic-normal DML approximation. For",
      "paper-grade inference (clustered/robust covariance, ML nuisance",
      "learners) use DoubleML."
    ),
    payload = list(
      ate = ate, ate_se = se, ate_pval = pval,
      att = att, att_se = att_se, att_pval = att_p,
      n = n, method = "PLR-crossfit"
    )
  )
}


# ---------------------------------------------------------------------------
# Backward-compat short aliases (mirror morie.otis)
# ---------------------------------------------------------------------------

#' @rdname morie_otis_primitives
#' @export
morie_otis_regional_placement <- function(...) morie_otis_rplace(...)

#' @rdname morie_otis_primitives
#' @export
morie_otis_alert_state_combo <- function(...) morie_otis_astcmb(...)

#' @rdname morie_otis_primitives
#' @export
morie_otis_volatility <- function(...) morie_otis_volat(...)

#' @rdname morie_otis_primitives
#' @export
morie_otis_rc_trends <- function(...) morie_otis_rctrnd(...)

#' @rdname morie_otis_primitives
#' @export
morie_otis_descriptives <- function(...) morie_otis_otdesc(...)

#' @rdname morie_otis_primitives
#' @export
morie_otis_dml <- function(...) morie_otis_otdml(...)
