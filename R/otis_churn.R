# SPDX-License-Identifier: AGPL-3.0-or-later
#' Goffmanian institutional-churn analyses on OTIS
#'
#' Eleven callables operationalising Goffman's "total institution"
#' framework (Goffman 1961) on the OTIS dataset:
#'
#' \itemize{
#'   \item \code{morie_otis_repeat_placement_concentration(b09)}
#'   \item \code{morie_otis_within_year_placement_count(b01)}
#'   \item \code{morie_otis_within_year_region_diversity(b01)}
#'   \item \code{morie_otis_mortification_cooccurrence(b01)}
#'   \item \code{morie_otis_disciplinary_medical_overlap(b01)}
#'   \item \code{morie_otis_embedding_distribution(b02)}
#'   \item \code{morie_otis_intra_year_transition_matrix(a01)}
#'   \item \code{morie_otis_path_complexity_gini(b01)}
#'   \item \code{morie_otis_region_alert_state_richness(b01)}
#'   \item \code{morie_otis_regC_demog_contingency(b01)}
#'   \item \code{morie_otis_irr_glmm_vm(b01)}: Poisson + NB2 IRR
#'     (requires \pkg{MASS} for the negative-binomial fit; falls back
#'     to Poisson-only when \pkg{MASS} is unavailable).
#' }
#'
#' All metrics are intra-fiscal-year by construction. OTIS
#' \code{UniqueIndividual_ID} is anonymised as \code{YYYY-XXXXX-AA},
#' randomly reassigned each fiscal year and each dataset file, so
#' longitudinal individual-level and cross-dataset linkage are
#' impossible by design (see \code{docs/methods/otis_linkage.md}).
#' The \code{variable_taxonomy.R} registry enforces this with
#' \code{cross_year_safe = FALSE}.
#'
#' @references
#'  Goffman, E. (1961). \emph{Asylums: Essays on the social situation
#'  of mental patients and other inmates.} Anchor Books.
#'
#'  Hill, B. M. (1975). A simple general approach to inference about
#'  the tail of a distribution. \emph{The Annals of Statistics},
#'  3(5), 1163-1174.
#'
#'  Clauset, A., Shalizi, C. R., & Newman, M. E. J. (2009). Power-law
#'  distributions in empirical data. \emph{SIAM Review}, 51(4),
#'  661-703.
#' @name morie_otis_churn
NULL


# ---------------------------------------------------------------------------
# Internal helpers (reuse the OTIS / MRM patterns)
# ---------------------------------------------------------------------------

# Result constructor shared with otis.R (.otis_result lives there but
# we define a thin alias here to avoid load-order coupling). We name it
# `.churn_result` so it can co-exist if file load order changes.
.churn_result <- function(title,
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


# Parse OTIS b09 placement-count bin labels:
#   "1 placement"          -> 1
#   "2 placements"         -> 2
#   "6 to 10 placements"   -> 8 (midpoint)
#   "Greater than 40"      -> 50 (boundary + 10)
.churn_parse_placement_bin <- function(label) {
  s <- tolower(trimws(as.character(label)))
  if (grepl("greater than", s)) {
    m <- regmatches(s, regexpr("[0-9]+", s))
    return(if (length(m)) as.numeric(m) + 10 else NA_real_)
  }
  rng <- regmatches(s, regexpr("[0-9]+\\s+to\\s+[0-9]+", s))
  if (length(rng)) {
    nums <- as.numeric(strsplit(rng, "\\s+to\\s+")[[1]])
    return(mean(nums))
  }
  m <- regmatches(s, regexpr("[0-9]+", s))
  if (length(m)) as.numeric(m) else NA_real_
}


# Yes/No/T/F/1/0 -> integer 0/1
.churn_yn <- function(s) {
  if (is.logical(s)) return(as.integer(s))
  if (is.numeric(s)) {
    s[is.na(s)] <- 0
    return(as.integer(s > 0))
  }
  v <- tolower(trimws(as.character(s)))
  as.integer(v %in% c("yes", "y", "true", "t", "1"))
}


# chi-square + Cramer's V on a 2x2-or-larger crosstab. Returns list
# (chi2, p, v) with NA entries when the table is too sparse.
.churn_chi2_v <- function(tbl, min_cell = 5L) {
  if (any(dim(tbl) < 2L)) {
    return(list(chi2 = NA_real_, p = NA_real_, v = NA_real_))
  }
  if (min(tbl) < min_cell) {
    # still compute, but flag via warning upstream
  }
  chi <- suppressWarnings(stats::chisq.test(tbl, correct = FALSE))
  n <- sum(tbl)
  k <- min(dim(tbl)) - 1L
  v <- if (k > 0L) sqrt(as.numeric(chi$statistic) / (n * k)) else NA_real_
  list(chi2 = as.numeric(chi$statistic),
       p = as.numeric(chi$p.value),
       v = v)
}


# ---------------------------------------------------------------------------
# 1. Repeat-placement concentration (b09)
# ---------------------------------------------------------------------------

#' Repeat-placement concentration (Goffmanian cyclical-inmate)
#'
#' Expands the OTIS b09 banded counts into a per-individual placement-
#' count vector, then reports the Gini coefficient, Hill-MLE power-law
#' alpha, top-10\% share, and a Kolmogorov-Smirnov test against an
#' exponential null. Reuses the \code{.gini_int} / \code{.hill_mle}
#' helpers from \code{mrm_otis.R}.
#'
#' @param df b09 long-form data.frame.
#' @param band_col,count_col Column names.
#' @return \code{morie_otis_result}.
#' @export
morie_otis_repeat_placement_concentration <- function(
  df,
  band_col = "NumberPlacements_Segregation",
  count_col = "NumberIndividuals_Segregation"
) {
  if (!all(c(band_col, count_col) %in% names(df))) {
    return(.churn_result(title = "Repeat-placement concentration",
                          warnings = "b09 columns missing"))
  }
  rows <- df[!is.na(df[[band_col]]) & !is.na(df[[count_col]]), , drop = FALSE]
  midpoints <- vapply(rows[[band_col]], .churn_parse_placement_bin,
                       numeric(1))
  multipliers <- as.integer(rows[[count_col]])
  keep <- is.finite(midpoints) & midpoints > 0 & multipliers > 0
  if (!any(keep)) {
    return(.churn_result(title = "Repeat-placement concentration",
                          warnings = "b09 has no usable rows"))
  }
  expanded <- rep(midpoints[keep], multipliers[keep])
  g <- .gini_int(expanded)
  alpha <- .hill_mle(expanded, x_min = 1)
  alpha_se <- if (is.finite(alpha)) (alpha - 1) / sqrt(length(expanded))
              else NA_real_
  # top-10% share
  ordered_desc <- sort(expanded, decreasing = TRUE)
  n <- length(ordered_desc)
  top10 <- sum(ordered_desc[seq_len(max(1L, n %/% 10L))])
  total <- sum(ordered_desc)
  top10_share <- if (total > 0) top10 / total else 0
  # KS vs exponential
  ks_p <- NA_real_
  if (n >= 5L && stats::sd(expanded) > 0) {
    ks_p <- suppressWarnings(stats::ks.test(expanded, "pexp",
                                              rate = 1 / mean(expanded))$p.value)
  }
  .churn_result(
    title = "Goffmanian: repeat-placement concentration",
    summary_lines = list(
      `OTIS source` = "b09 -- placements per individual",
      `Individuals (in seg)` = n,
      `Mean placements/person` = round(mean(expanded), 2),
      `Median placements/person` = stats::median(expanded),
      `Max placements/person` = max(expanded),
      `Gini` = round(g, 4),
      `Top-10% share` = sprintf("%.1f%%", 100 * top10_share),
      `Power-law alpha (Hill)` = if (is.finite(alpha))
                                    round(alpha, 3) else "n/a",
      `Hill alpha SE` = if (is.finite(alpha_se))
                          round(alpha_se, 3) else "n/a",
      `KS p (vs exponential)` = if (is.finite(ks_p))
                                  signif(ks_p, 3) else "n/a"
    ),
    interpretation = paste(
      sprintf("Gini = %.3f on placements/person.", g),
      "Goffman's total-institution dynamics predict heavy-tail",
      "concentration: a few individuals accumulate many placements.",
      sprintf("Power-law alpha approx %.2f", alpha),
      "(typical Goffmanian range 1.5-2.5 indicates preferential-",
      "attachment-style cycling). KS rejection against the",
      "exponential null implies a non-exponential heavy tail."
    ),
    payload = list(gini = g, alpha_mle = alpha, alpha_se = alpha_se,
                   ks_pvalue = ks_p, top10_share = top10_share,
                   n = n)
  )
}


# ---------------------------------------------------------------------------
# 2. Within-year placement count (b01)
# ---------------------------------------------------------------------------

#' Within-year placement-count distribution
#'
#' Distribution of segregation placements per (individual x fiscal
#' year) cell. Because OTIS IDs are year-locked
#' (\code{YYYY-XXXXX-AA}), each cell is one anonymous person-year;
#' cross-year readmission is not measurable.
#'
#' @param df b01 data.frame.
#' @return \code{morie_otis_result}.
#' @export
morie_otis_within_year_placement_count <- function(df) {
  needed <- c("UniqueIndividual_ID", "EndFiscalYear")
  if (!all(needed %in% names(df))) {
    return(.churn_result(title = "Within-year placement count",
                          warnings = "b01 missing required cols"))
  }
  g <- df[stats::complete.cases(df[, needed, drop = FALSE]), , drop = FALSE]
  if (nrow(g) == 0L) {
    return(.churn_result(title = "Within-year placement count",
                          warnings = "no usable rows"))
  }
  key <- paste(g$UniqueIndividual_ID, g$EndFiscalYear, sep = "\u001f")
  cnt_arr <- as.integer(table(key))
  q <- as.numeric(stats::quantile(cnt_arr, c(0.25, 0.75), names = FALSE))
  frac_multi <- mean(cnt_arr > 1)
  .churn_result(
    title = "Within-year placement count (Goffmanian intra-year cycling)",
    summary_lines = list(
      `Distinct (id x year) cells` = length(cnt_arr),
      `Mean placements / person-year` = round(mean(cnt_arr), 3),
      Median = stats::median(cnt_arr),
      `Q1 / Q3` = sprintf("%.1f / %.1f", q[1], q[2]),
      `Max in one FY` = max(cnt_arr),
      `Cells with 1 placement` = sum(cnt_arr == 1L),
      `Cells with 2 placements` = sum(cnt_arr == 2L),
      `Cells with 3+ placements` = sum(cnt_arr >= 3L),
      `% with multiple placements within FY` =
        sprintf("%.1f%%", 100 * frac_multi),
      `Gini of placement counts` = round(.gini_int(cnt_arr), 3)
    ),
    interpretation = paste(
      sprintf("Of %d person-year cells, %d (%.1f%%)",
              length(cnt_arr), sum(cnt_arr > 1), 100 * frac_multi),
      "received more than one segregation placement in the same",
      "fiscal year.",
      sprintf("Gini = %.3f.", .gini_int(cnt_arr)),
      "Cross-year readmission cannot be measured because OTIS IDs are",
      "year-locked."
    ),
    payload = list(n_cells = length(cnt_arr),
                   mean_count = mean(cnt_arr),
                   median_count = stats::median(cnt_arr),
                   max_count = max(cnt_arr),
                   frac_multi = frac_multi,
                   gini = .gini_int(cnt_arr)),
    warnings = paste("OTIS UniqueIndividual_ID is year-locked",
                      "(YYYY-XXXXX-AA); intra-year only.")
  )
}


# ---------------------------------------------------------------------------
# 3. Within-year region diversity (b01)
# ---------------------------------------------------------------------------

#' Within-year region diversity
#'
#' Distinct \code{Region_AtTimeOfPlacement} values per person-year.
#'
#' @param df b01 data.frame.
#' @return \code{morie_otis_result}.
#' @export
morie_otis_within_year_region_diversity <- function(df) {
  needed <- c("UniqueIndividual_ID", "Region_AtTimeOfPlacement")
  if (!all(needed %in% names(df))) {
    return(.churn_result(title = "Within-year region diversity",
                          warnings = "b01 missing region/id cols"))
  }
  g <- df[stats::complete.cases(df[, needed, drop = FALSE]), , drop = FALSE]
  if (nrow(g) == 0L) {
    return(.churn_result(title = "Within-year region diversity",
                          warnings = "no usable rows"))
  }
  per_id <- tapply(as.character(g$Region_AtTimeOfPlacement),
                   g$UniqueIndividual_ID,
                   function(v) length(unique(v)))
  arr <- as.integer(per_id)
  freq <- as.data.frame(table(`#regions` = arr), stringsAsFactors = FALSE)
  names(freq) <- c("nregions", "n_person_years")
  freq$nregions <- as.integer(freq$nregions)
  freq$pct <- sprintf("%.1f%%", 100 * freq$n_person_years / length(arr))
  multi_pct <- 100 * mean(arr > 1)
  .churn_result(
    title = "Within-year region diversity (intra-year mobility)",
    summary_lines = list(
      `Distinct (id x year) cells` = length(arr),
      `Mean #regions per person-year` = round(mean(arr), 3),
      `In just 1 region` = sum(arr == 1L),
      `In 2 regions` = sum(arr == 2L),
      `In 3+ regions` = sum(arr >= 3L),
      `% multi-region within FY` = sprintf("%.1f%%", multi_pct)
    ),
    tables = list(list(
      title = "Counts by # regions visited within one FY:",
      headers = c("#regions", "n_person_years", "%"),
      rows = lapply(seq_len(nrow(freq)),
                    function(i) list(freq$nregions[i],
                                      freq$n_person_years[i],
                                      freq$pct[i]))
    )),
    interpretation = sprintf(paste(
      "%.1f%% of person-years involve movement across more than one",
      "region within the same fiscal year -- intra-year cross-staff-",
      "regime mobility. Multi-year mobility is not measurable: OTIS",
      "IDs are year-locked."), multi_pct),
    warnings = paste("OTIS UniqueIndividual_ID is year-locked",
                      "(YYYY-XXXXX-AA); intra-year only.")
  )
}


# ---------------------------------------------------------------------------
# 4. Mortification co-occurrence (b01)
# ---------------------------------------------------------------------------

#' Mortification co-occurrence (concurrent alerts)
#'
#' Counts concurrent alert flags per placement and tests independence
#' of MentalHealth vs SuicideRisk via chi-square + Cramer's V.
#'
#' @param df b01 data.frame.
#' @return \code{morie_otis_result}.
#' @export
morie_otis_mortification_cooccurrence <- function(df) {
  cols <- c("MentalHealth_Alert", "SuicideRisk_Alert", "SuicideWatch_Alert")
  have <- intersect(cols, names(df))
  if (length(have) < 2L) {
    return(.churn_result(title = "Mortification co-occurrence",
                          warnings = "need >=2 alert columns"))
  }
  flags <- as.data.frame(lapply(df[have], .churn_yn))
  names(flags) <- have
  n_flags <- rowSums(flags)
  counts <- as.data.frame(table(`#alerts` = n_flags),
                           stringsAsFactors = FALSE)
  names(counts) <- c("nalerts", "n_placements")
  counts$nalerts <- as.integer(counts$nalerts)
  counts$pct <- sprintf("%.1f%%",
                         100 * counts$n_placements / nrow(flags))
  cv <- if (all(c("MentalHealth_Alert", "SuicideRisk_Alert") %in% have)) {
    tbl <- table(flags$MentalHealth_Alert, flags$SuicideRisk_Alert)
    .churn_chi2_v(tbl)
  } else {
    list(chi2 = NA_real_, p = NA_real_, v = NA_real_)
  }
  .churn_result(
    title = "Goffmanian: mortification co-occurrence",
    summary_lines = list(
      `Alerts considered` = paste(have, collapse = ", "),
      Placements = nrow(flags),
      `0 alerts` = sum(n_flags == 0L),
      `1 alert` = sum(n_flags == 1L),
      `2 alerts` = sum(n_flags == 2L),
      `3 alerts` = sum(n_flags == 3L),
      `MH x SR chi2` = round(cv$chi2, 3),
      `chi2 p-value` = signif(cv$p, 3),
      `Cramer's V` = round(cv$v, 4)
    ),
    tables = list(list(
      title = "# concurrent alerts per placement:",
      headers = c("#alerts", "n_placements", "%"),
      rows = lapply(seq_len(nrow(counts)),
                    function(i) list(counts$nalerts[i],
                                      counts$n_placements[i],
                                      counts$pct[i]))
    )),
    interpretation = paste(
      sprintf("Cramer's V = %.3f between MH and Suicide-Risk alerts.",
              cv$v),
      "Higher V = stronger co-occurrence (Goffman's mortification",
      sprintf("stack). chi2 p = %g: rejection of independence implies",
              cv$p),
      "alerts cluster on the same placements rather than being",
      "independently assigned."
    ),
    payload = list(chi2 = cv$chi2, p = cv$p, cramers_v = cv$v)
  )
}


# ---------------------------------------------------------------------------
# 5. Disciplinary x medical-protection overlap (b01)
# ---------------------------------------------------------------------------

#' Disciplinary x medical-protection overlap
#'
#' Goffman's "tinkering trades" tension: same person classified by
#' both punitive and therapeutic rationales. Detects any
#' \code{SegReason_Disciplinary*} flag co-occurring with any
#' \code{SegReason_*Medical*} flag.
#'
#' @param df b01 data.frame.
#' @return \code{morie_otis_result}.
#' @export
morie_otis_disciplinary_medical_overlap <- function(df) {
  disc_cols <- grep("^SegReason_Disciplinary", names(df), value = TRUE)
  med_cols  <- grep("^SegReason_.*Medical", names(df), value = TRUE)
  if (length(disc_cols) == 0L || length(med_cols) == 0L) {
    return(.churn_result(title = "Disciplinary x medical overlap",
                          warnings = "missing disciplinary or medical SegReason cols"))
  }
  any_yes <- function(mat) {
    M <- vapply(mat, .churn_yn, integer(nrow(df)))
    if (is.null(dim(M))) return(as.integer(M > 0))
    as.integer(rowSums(M) > 0)
  }
  has_disc <- any_yes(df[disc_cols])
  has_med  <- any_yes(df[med_cols])
  tbl <- table(disc = has_disc, med = has_med)
  cv <- .churn_chi2_v(tbl)
  .churn_result(
    title = "Goffmanian: disciplinary x medical-protection overlap",
    summary_lines = list(
      Placements = nrow(df),
      `Disciplinary cols` = paste(sub("^SegReason_", "", disc_cols),
                                    collapse = ", "),
      `Medical-protection cols` = paste(sub("^SegReason_", "", med_cols),
                                          collapse = ", "),
      `Both flagged` = sum(has_disc & has_med),
      `Disciplinary only` = sum(has_disc & !has_med),
      `Medical only` = sum(!has_disc & has_med),
      Neither = sum(!has_disc & !has_med),
      `chi2` = round(cv$chi2, 3),
      `chi2 p-value` = signif(cv$p, 3),
      `Cramer's V` = round(cv$v, 4)
    ),
    interpretation = paste(
      "Goffman's 'tinkering trades' surface where punitive and",
      "therapeutic logics co-classify the same person.",
      sprintf("Cramer's V = %.3f measures their dependence; chi2",
              cv$v),
      sprintf("p = %g -- rejection implies joint flagging is",
              cv$p),
      "non-random."
    )
  )
}


# ---------------------------------------------------------------------------
# 6. Embedding distribution (b02)
# ---------------------------------------------------------------------------

#' Total-days embedding distribution (lognormal vs Pareto vs exp)
#'
#' Fits lognormal, Pareto, and exponential distributions to
#' \code{TotalAggregatedDays_Segregation} by AIC and reports which
#' family wins.
#'
#' @param df b02 data.frame.
#' @return \code{morie_otis_result}.
#' @export
morie_otis_embedding_distribution <- function(df) {
  if (!"TotalAggregatedDays_Segregation" %in% names(df)) {
    return(.churn_result(title = "Embedding distribution",
                          warnings = "b02 missing TotalAggregatedDays"))
  }
  x <- suppressWarnings(as.numeric(df$TotalAggregatedDays_Segregation))
  x <- x[is.finite(x) & x > 0]
  if (length(x) < 50L) {
    return(.churn_result(title = "Embedding distribution",
                          warnings = sprintf("only %d valid rows",
                                              length(x))))
  }
  # Lognormal MLE (no MASS dep -- closed form)
  lx <- log(x)
  ln_aic <- {
    mu <- mean(lx)
    sd_ <- stats::sd(lx)
    ll <- sum(stats::dlnorm(x, meanlog = mu, sdlog = sd_, log = TRUE))
    2 * 2 - 2 * ll
  }
  # Exponential MLE
  ex_aic <- {
    rate <- 1 / mean(x)
    ll <- sum(stats::dexp(x, rate = rate, log = TRUE))
    2 * 1 - 2 * ll
  }
  # Pareto MLE (Hill on the full vector with x_min = min(x))
  pa_aic <- tryCatch({
    xm <- min(x)
    alpha <- length(x) / sum(log(x / xm))
    # density: alpha * xm^alpha / x^(alpha+1) for x >= xm
    ll <- sum(log(alpha) + alpha * log(xm) - (alpha + 1) * log(x))
    2 * 2 - 2 * ll
  }, error = function(e) NA_real_)

  fits <- c(lognormal = ln_aic, pareto = pa_aic, exponential = ex_aic)
  valid <- fits[is.finite(fits)]
  best <- if (length(valid)) names(valid)[which.min(valid)] else "n/a"

  .churn_result(
    title = "Goffmanian: institutional embedding (total days)",
    summary_lines = list(
      Records = length(x),
      `Mean total days` = round(mean(x), 1),
      Median = stats::median(x),
      Max = max(x),
      `Lognormal AIC` = if (is.finite(ln_aic)) round(ln_aic, 1)
                         else "n/a",
      `Pareto AIC` = if (is.finite(pa_aic)) round(pa_aic, 1)
                      else "n/a",
      `Exponential AIC` = if (is.finite(ex_aic)) round(ex_aic, 1)
                           else "n/a",
      `Best fit (lowest AIC)` = best
    ),
    interpretation = sprintf(paste(
      "Best fit: %s. Pareto or lognormal beating exponential implies a",
      "heavy-tailed distribution, consistent with Goffman's dichotomy",
      "between the casual short-stayer and the deeply-embedded",
      "long-stayer."), best),
    payload = list(lognormal_aic = ln_aic, pareto_aic = pa_aic,
                   exponential_aic = ex_aic, best_fit = best,
                   n = length(x))
  )
}


# ---------------------------------------------------------------------------
# 7. Intra-year region transition matrix (a01)
# ---------------------------------------------------------------------------

#' Intra-year region-to-region transition matrix
#'
#' Markov transition matrix on \code{Region_AtTimeOfPlacement} within
#' each person-year, with stationary distribution and off-diagonal
#' Theil-T concentration.
#'
#' @param df a01 data.frame.
#' @return \code{morie_otis_result}.
#' @export
morie_otis_intra_year_transition_matrix <- function(df) {
  needed <- c("UniqueIndividual_ID", "EndFiscalYear",
              "Region_AtTimeOfPlacement")
  if (!all(needed %in% names(df))) {
    return(.churn_result(title = "Intra-year transition matrix",
                          warnings = "a01 missing required cols"))
  }
  base <- df[stats::complete.cases(df[, needed, drop = FALSE]), needed,
              drop = FALSE]
  names(base) <- c("id", "yr", "regA")
  base$regA <- as.character(base$regA)
  ord <- order(base$id, base$yr)
  base <- base[ord, , drop = FALSE]
  # lag within (id, yr)
  key <- paste(base$id, base$yr, sep = "\u001f")
  prev <- ave(base$regA, key,
              FUN = function(v) c(NA_character_, head(v, -1L)))
  edges <- data.frame(from = prev, to = base$regA,
                       stringsAsFactors = FALSE)
  edges <- edges[!is.na(edges$from), , drop = FALSE]
  if (nrow(edges) == 0L) {
    return(.churn_result(title = "Intra-year transition matrix",
                          warnings = "no within-year transitions"))
  }
  regions <- sort(unique(c(edges$from, edges$to)))
  counts <- matrix(0, nrow = length(regions), ncol = length(regions),
                    dimnames = list(regions, regions))
  for (i in seq_len(nrow(edges))) {
    counts[edges$from[i], edges$to[i]] <-
      counts[edges$from[i], edges$to[i]] + 1L
  }
  row_sums <- rowSums(counts)
  P <- counts
  P[] <- 0
  ok <- row_sums > 0
  P[ok, ] <- counts[ok, , drop = FALSE] / row_sums[ok]

  # stationary distribution = left eigenvector of eigenvalue 1
  stationary <- tryCatch({
    eig <- eigen(t(P))
    idx <- which.min(abs(eig$values - 1))
    v <- Re(eig$vectors[, idx])
    v <- v / sum(v)
    setNames(round(v, 4), regions)
  }, error = function(e) setNames(rep(NA_real_, length(regions)),
                                    regions))

  # Off-diagonal Theil-T
  off <- counts
  diag(off) <- 0
  off_total <- sum(off)
  theil <- NA_real_
  if (off_total > 0) {
    p <- as.numeric(off) / off_total
    p <- p[p > 0]
    theil <- sum(p * log(p * length(p)))
  }
  diag_share <- sum(diag(counts)) / sum(counts)

  .churn_result(
    title = "Intra-year region transition matrix (Markov)",
    summary_lines = list(
      `Transitions observed` = sum(counts),
      `Region states` = length(regions),
      `Diagonal share (stay-in-region)` = sprintf("%.1f%%",
                                                    100 * diag_share),
      `Off-diagonal Theil-T` = if (is.finite(theil)) round(theil, 4)
                                 else "n/a",
      `Stationary pi` = paste(sprintf("%s=%.3f",
                                       names(stationary), stationary),
                               collapse = ", ")
    ),
    tables = list(list(
      title = "Transition probability matrix P(regA -> regA'):",
      headers = c("from \\ to", regions),
      rows = lapply(regions, function(r)
        c(r, sprintf("%.3f", P[r, ])))
    )),
    interpretation = sprintf(paste(
      "Within-fiscal-year region stickiness is %.1f%% (diagonal share).",
      "Off-diagonal mass concentration Theil-T = %.3f (higher = more",
      "concentrated transitions, lower = more uniform cross-region",
      "mixing). All transitions are intra-year by construction."),
      100 * diag_share, theil),
    payload = list(diag_share = diag_share, theil_off = theil,
                   stationary = stationary,
                   n_transitions = sum(counts)),
    warnings = paste("OTIS IDs year-locked (YYYY-XXXXX-AA);",
                      "transitions are intra-year only.")
  )
}


# ---------------------------------------------------------------------------
# 8. Path complexity Gini (b01)
# ---------------------------------------------------------------------------

#' Path complexity Gini by (year, region)
#'
#' Per-(id, year, region) placement counts, with the Gini coefficient
#' reported overall and split by fiscal year and region.
#'
#' @param df b01 data.frame.
#' @return \code{morie_otis_result}.
#' @export
morie_otis_path_complexity_gini <- function(df) {
  needed <- c("UniqueIndividual_ID", "EndFiscalYear",
              "Region_AtTimeOfPlacement")
  if (!all(needed %in% names(df))) {
    return(.churn_result(title = "Path complexity Gini",
                          warnings = "b01 missing required cols"))
  }
  g <- df[stats::complete.cases(df[, needed, drop = FALSE]), needed,
           drop = FALSE]
  names(g) <- c("id", "yr", "rg")
  key <- paste(g$id, g$yr, g$rg, sep = "\u001f")
  counts <- as.integer(table(key))
  keys <- names(table(key))
  parts <- do.call(rbind, strsplit(keys, "\u001f", fixed = TRUE))
  cells <- data.frame(
    id = parts[, 1], yr = parts[, 2], rg = parts[, 3],
    n_placements = counts, stringsAsFactors = FALSE
  )
  if (nrow(cells) == 0L) {
    return(.churn_result(title = "Path complexity Gini",
                          warnings = "no usable rows"))
  }
  overall <- .gini_int(cells$n_placements)
  by_yr <- tapply(cells$n_placements, cells$yr,
                   function(s) round(.gini_int(s), 4))
  by_rg <- tapply(cells$n_placements, cells$rg,
                   function(s) round(.gini_int(s), 4))
  by_yrg <- aggregate(cells$n_placements,
                       by = list(yr = cells$yr, rg = cells$rg),
                       FUN = function(s) round(.gini_int(s), 4))
  names(by_yrg)[3] <- "gini"
  .churn_result(
    title = "Path complexity Gini (b01)",
    summary_lines = list(
      `Overall Gini` = round(overall, 4),
      Cells = nrow(cells),
      `Total placements` = sum(cells$n_placements),
      `Gini by fiscal year` = paste(sprintf("%s=%.3f",
                                              names(by_yr), by_yr),
                                      collapse = ", "),
      `Gini by region` = paste(sprintf("%s=%.3f",
                                         names(by_rg), by_rg),
                                 collapse = ", ")
    ),
    tables = list(list(
      title = "Gini by (year x region):",
      headers = c("EndFiscalYear", "Region", "Gini"),
      rows = lapply(seq_len(nrow(by_yrg)),
                    function(i) list(by_yrg$yr[i],
                                      by_yrg$rg[i],
                                      by_yrg$gini[i]))
    )),
    interpretation = sprintf(paste(
      "Overall placement-count Gini = %.3f. Higher region- or",
      "year-specific Gini means that cell has more Goffmanian",
      "heavy-tail cycling (a few people accumulating many placements).",
      "All values are intra-year by construction."), overall),
    payload = list(overall_gini = overall,
                   by_year = as.list(by_yr),
                   by_region = as.list(by_rg))
  )
}


# ---------------------------------------------------------------------------
# 9. Region x alert state richness (b01)
# ---------------------------------------------------------------------------

#' Region x alert state richness
#'
#' Distinct (region x alert-combo) states occupied per person-year.
#'
#' @param df b01 data.frame.
#' @return \code{morie_otis_result}.
#' @export
morie_otis_region_alert_state_richness <- function(df) {
  needed <- c("UniqueIndividual_ID", "EndFiscalYear",
              "Region_AtTimeOfPlacement", "MentalHealth_Alert",
              "SuicideRisk_Alert", "SuicideWatch_Alert")
  if (!all(needed %in% names(df))) {
    return(.churn_result(title = "Region x alert state richness",
                          warnings = "b01 missing required cols"))
  }
  base <- df[stats::complete.cases(df[, needed, drop = FALSE]), needed,
              drop = FALSE]
  a_mh <- .churn_yn(base$MentalHealth_Alert)
  a_sr <- .churn_yn(base$SuicideRisk_Alert)
  a_sw <- .churn_yn(base$SuicideWatch_Alert)
  combo <- a_mh * 4L + a_sr * 2L + a_sw
  state <- paste0(as.character(base$Region_AtTimeOfPlacement),
                   ":c", combo)
  key <- paste(base$UniqueIndividual_ID, base$EndFiscalYear,
                sep = "\u001f")
  n_states <- as.integer(tapply(state, key,
                                  function(v) length(unique(v))))
  if (length(n_states) == 0L) {
    return(.churn_result(title = "Region x alert state richness",
                          warnings = "no usable rows"))
  }
  multi_pct <- 100 * mean(n_states > 1L)
  .churn_result(
    title = "Region x alert state richness (Goffmanian role-zone sweep)",
    summary_lines = list(
      `Person-year cells` = length(n_states),
      `Mean distinct states / cell` = round(mean(n_states), 3),
      Median = stats::median(n_states),
      `Max distinct states` = max(n_states),
      `Cells in 1 state` = sum(n_states == 1L),
      `Cells in 2 states` = sum(n_states == 2L),
      `Cells in 3+ states` = sum(n_states >= 3L),
      `% multi-state` = sprintf("%.1f%%", multi_pct)
    ),
    interpretation = sprintf(paste(
      "%.1f%% of person-years span multiple (region x alert-combo)",
      "states within one fiscal year -- a Goffmanian sweep across",
      "institutional role-zones. Possible states = 5 regions x 8",
      "combos = 40."), multi_pct),
    payload = list(mean_states = mean(n_states),
                   median_states = stats::median(n_states),
                   max_states = max(n_states),
                   frac_multi = mean(n_states > 1L))
  )
}


# ---------------------------------------------------------------------------
# 10. Multi-region path x demographics (b01)
# ---------------------------------------------------------------------------

#' Multi-region path x Gender / Age contingency
#'
#' Per-person-year multi-region indicator (\code{regC >= 2}) cross-
#' tabulated with Gender and Age_Category; reports chi-square +
#' Cramer's V on each.
#'
#' @param df b01 data.frame.
#' @return \code{morie_otis_result}.
#' @export
morie_otis_regC_demog_contingency <- function(df) {
  needed <- c("UniqueIndividual_ID", "EndFiscalYear",
              "Region_AtTimeOfPlacement", "Gender", "Age_Category")
  if (!all(needed %in% names(df))) {
    return(.churn_result(title = "Multi-region path x demographics",
                          warnings = "b01 missing required cols"))
  }
  base <- df[stats::complete.cases(df[, needed, drop = FALSE]), needed,
              drop = FALSE]
  key <- paste(base$UniqueIndividual_ID, base$EndFiscalYear,
                sep = "\u001f")
  agg <- data.frame(
    key = key,
    rg = as.character(base$Region_AtTimeOfPlacement),
    gender = as.character(base$Gender),
    age = as.character(base$Age_Category),
    stringsAsFactors = FALSE
  )
  regC <- tapply(agg$rg, agg$key, function(v) length(unique(v)))
  gender <- tapply(agg$gender, agg$key, function(v) v[1])
  age <- tapply(agg$age, agg$key, function(v) v[1])
  cell <- data.frame(
    key = names(regC), regC = as.integer(regC),
    Gender = unname(gender), Age = unname(age),
    multi_region = as.integer(regC >= 2L),
    stringsAsFactors = FALSE
  )
  if (nrow(cell) == 0L) {
    return(.churn_result(title = "Multi-region path x demographics",
                          warnings = "no usable rows"))
  }
  tab_g <- table(cell$Gender, cell$multi_region)
  tab_a <- table(cell$Age, cell$multi_region)
  cv_g <- .churn_chi2_v(tab_g)
  cv_a <- .churn_chi2_v(tab_a)
  rows_g <- lapply(rownames(tab_g), function(g)
    list(g,
         if ("0" %in% colnames(tab_g)) as.integer(tab_g[g, "0"]) else 0L,
         if ("1" %in% colnames(tab_g)) as.integer(tab_g[g, "1"]) else 0L))
  rows_a <- lapply(rownames(tab_a), function(a)
    list(a,
         if ("0" %in% colnames(tab_a)) as.integer(tab_a[a, "0"]) else 0L,
         if ("1" %in% colnames(tab_a)) as.integer(tab_a[a, "1"]) else 0L))
  .churn_result(
    title = "Multi-region path x demographics (chi2 + Cramer's V)",
    summary_lines = list(
      `Person-year cells` = nrow(cell),
      `% multi-region` = sprintf("%.1f%%",
                                   100 * mean(cell$multi_region)),
      `Gender chi2` = round(cv_g$chi2, 3),
      `Gender chi2 p` = signif(cv_g$p, 3),
      `Gender Cramer's V` = round(cv_g$v, 4),
      `Age chi2` = round(cv_a$chi2, 3),
      `Age chi2 p` = signif(cv_a$p, 3),
      `Age Cramer's V` = round(cv_a$v, 4)
    ),
    tables = list(
      list(title = "Gender x multi-region (intra-FY):",
           headers = c("Gender", "single-region", "multi-region"),
           rows = rows_g),
      list(title = "Age x multi-region (intra-FY):",
           headers = c("Age_Category", "single-region", "multi-region"),
           rows = rows_a)
    ),
    interpretation = paste(
      "Cramer's V quantifies association strength of Gender and Age",
      "with within-fiscal-year multi-region cycling. V approx 0",
      "implies independence; V -> 1 implies strong association.",
      "All measures are intra-year (OTIS IDs year-locked)."
    ),
    payload = list(
      frac_multi = mean(cell$multi_region),
      chi2_gender = cv_g$chi2, p_gender = cv_g$p, v_gender = cv_g$v,
      chi2_age = cv_a$chi2, p_age = cv_a$p, v_age = cv_a$v
    )
  )
}


# ---------------------------------------------------------------------------
# 11. Poisson / NB IRR for volatility ~ high-ac treatment (b01)
# ---------------------------------------------------------------------------

#' Poisson + Negative-Binomial IRR for volatility ~ alert complexity
#'
#' Builds the (id x fiscal year) cell with outcome
#' \code{vm} (number of distinct regions visited) and treatment
#' \code{T_high_ac} = 1 if the person-year alert-complexity \code{ac >=
#' 2}, then fits Poisson and (optionally) negative-binomial GLMs
#' adjusting for Year, Gender, and Age. The NB fit uses
#' \pkg{MASS::glm.nb} when available; if not, only Poisson is
#' reported.
#'
#' No random effect / cluster-robust SE -- for paper-grade inference,
#' use the dedicated OTIS DML pipeline.
#'
#' @param df b01 data.frame.
#' @return \code{morie_otis_result}.
#' @export
morie_otis_irr_glmm_vm <- function(df) {
  needed <- c("UniqueIndividual_ID", "EndFiscalYear",
              "Region_AtTimeOfPlacement", "MentalHealth_Alert",
              "SuicideRisk_Alert", "SuicideWatch_Alert",
              "Gender", "Age_Category")
  if (!all(needed %in% names(df))) {
    return(.churn_result(title = "IRR Poisson/NB GLM (vm)",
                          warnings = "b01 missing required cols"))
  }
  base <- df[stats::complete.cases(df[, needed, drop = FALSE]), needed,
              drop = FALSE]
  a_mh <- .churn_yn(base$MentalHealth_Alert)
  a_sr <- .churn_yn(base$SuicideRisk_Alert)
  a_sw <- .churn_yn(base$SuicideWatch_Alert)
  base$combo <- a_mh * 4L + a_sr * 2L + a_sw
  key <- paste(base$UniqueIndividual_ID, base$EndFiscalYear,
                sep = "\u001f")
  py <- do.call(rbind, lapply(split(base, key), function(g) {
    ac <- length(unique(g$combo[g$combo > 0]))
    vm <- length(unique(g$Region_AtTimeOfPlacement))
    data.frame(
      vm = vm,
      T_high_ac = as.integer(ac >= 2L),
      yr = as.character(g$EndFiscalYear[1]),
      sg = as.character(g$Gender[1]),
      ag = as.character(g$Age_Category[1]),
      stringsAsFactors = FALSE
    )
  }))
  py$yr <- factor(py$yr)
  py$sg <- factor(py$sg)
  py$ag <- factor(py$ag)
  if (nrow(py) == 0L || sum(py$vm) == 0L) {
    return(.churn_result(title = "IRR Poisson/NB GLM (vm)",
                          warnings = "empty cell-table or zero outcome"))
  }
  formula_obj <- stats::as.formula("vm ~ T_high_ac + yr + sg + ag")
  out_rows <- list()
  fit_irr <- function(fit, label) {
    co <- summary(fit)$coefficients
    nm <- "T_high_ac"
    if (!nm %in% rownames(co)) {
      return(list(label, "no estimate", "n/a", "n/a", "n/a"))
    }
    beta <- co[nm, "Estimate"]
    se   <- co[nm, "Std. Error"]
    pval <- if ("Pr(>|z|)" %in% colnames(co)) co[nm, "Pr(>|z|)"]
            else co[nm, ncol(co)]
    irr <- exp(beta)
    ci  <- exp(beta + c(-1, 1) * 1.96 * se)
    aic <- tryCatch(stats::AIC(fit), error = function(e) NA_real_)
    list(label, round(irr, 4),
         sprintf("[%.3f, %.3f]", ci[1], ci[2]),
         signif(pval, 3), round(aic, 2))
  }
  # 3MMM.28: bump glm maxit to 200 (default 25 hits ceiling on
  # small synthetic OTIS panels) and suppressWarnings on the per-fit
  # convergence noise. Real OTIS data converges in ~10 iter; the
  # convergence-status is surfaced via fit$converged in the payload.
  # Poisson
  pois_fit <- suppressWarnings(tryCatch(
    stats::glm(formula_obj, data = py,
                family = stats::poisson(),
                control = stats::glm.control(maxit = 200L)),
    error = function(e) NULL))
  if (!is.null(pois_fit)) {
    out_rows[[length(out_rows) + 1L]] <- fit_irr(pois_fit, "Poisson")
  } else {
    out_rows[[length(out_rows) + 1L]] <- list("Poisson", "fit failed",
                                                "--", "--", "--")
  }
  # Negative Binomial (optional)
  if (requireNamespace("MASS", quietly = TRUE)) {
    nb_fit <- suppressWarnings(tryCatch(
      MASS::glm.nb(formula_obj, data = py,
                    control = stats::glm.control(maxit = 200L)),
      error = function(e) NULL))
    if (!is.null(nb_fit)) {
      out_rows[[length(out_rows) + 1L]] <- fit_irr(nb_fit, "NegBin2")
    } else {
      out_rows[[length(out_rows) + 1L]] <- list("NegBin2", "fit failed",
                                                  "--", "--", "--")
    }
  } else {
    out_rows[[length(out_rows) + 1L]] <- list(
      "NegBin2", "MASS not installed", "--", "--", "--"
    )
  }
  .churn_result(
    title = "IRR Poisson/NB GLM -- vm ~ T_high_ac + demog",
    summary_lines = list(
      `Cells (id x year)` = nrow(py),
      `Mean vm / cell` = round(mean(py$vm), 3),
      `T_high_ac prevalence` = sprintf("%.1f%%",
                                         100 * mean(py$T_high_ac))
    ),
    tables = list(list(
      title = "IRR for T_high_ac on vm (covariate-adjusted):",
      headers = c("Family", "IRR", "95% CI", "p-value", "AIC"),
      rows = out_rows
    )),
    interpretation = paste(
      "IRR approx exp(beta) on the alert-complexity treatment.",
      "Poisson assumes equidispersion; NB2 relaxes that. Concordance",
      "between the two is a robustness signal. All inference is on",
      "intra-year (id x FY) cells."
    ),
    payload = list(n_cells = nrow(py),
                   mean_vm = mean(py$vm),
                   irr_results = out_rows),
    warnings = c(
      paste("No random-effect or cluster-robust SE in this",
             "implementation; for paper-grade SEs use the OTIS DML",
             "pipeline."),
      "OTIS IDs are year-locked (YYYY-XXXXX-AA); intra-year only."
    )
  )
}


# ---------------------------------------------------------------------------
# Master driver
# ---------------------------------------------------------------------------

#' Run all 11 OTIS-churn analyses
#'
#' Calls every \code{morie_otis_*} churn callable on its respective
#' input \code{data.frame} and returns a named list of results. Each
#' input is independent: pass \code{NULL} (or omit) to skip a metric.
#' If \code{out_dir} is supplied, each result is also serialised to
#' disk.
#'
#' CRAN-safe: with \code{out_dir = NULL} no files are written.
#'
#' @param b01,b02,b09,a01 Input data.frames (any may be \code{NULL}).
#' @param out_dir Optional output directory.
#' @return Named list of \code{morie_otis_result}.
#' @export
morie_otis_churn_analyze_all <- function(b01 = NULL, b02 = NULL,
                                          b09 = NULL, a01 = NULL,
                                          out_dir = NULL) {
  fns <- list()
  if (!is.null(b09)) {
    fns$repeat_placement_concentration <-
      function() morie_otis_repeat_placement_concentration(b09)
  }
  if (!is.null(b01)) {
    fns$within_year_placement_count <-
      function() morie_otis_within_year_placement_count(b01)
    fns$within_year_region_diversity <-
      function() morie_otis_within_year_region_diversity(b01)
    fns$mortification_cooccurrence <-
      function() morie_otis_mortification_cooccurrence(b01)
    fns$disciplinary_medical_overlap <-
      function() morie_otis_disciplinary_medical_overlap(b01)
    fns$path_complexity_gini <-
      function() morie_otis_path_complexity_gini(b01)
    fns$region_alert_state_richness <-
      function() morie_otis_region_alert_state_richness(b01)
    fns$regC_demog_contingency <-
      function() morie_otis_regC_demog_contingency(b01)
    fns$irr_glmm_vm <- function() morie_otis_irr_glmm_vm(b01)
  }
  if (!is.null(b02)) {
    fns$embedding_distribution <-
      function() morie_otis_embedding_distribution(b02)
  }
  if (!is.null(a01)) {
    fns$intra_year_transition_matrix <-
      function() morie_otis_intra_year_transition_matrix(a01)
  }
  if (!is.null(out_dir)) {
    dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  }
  results <- list()
  for (nm in names(fns)) {
    r <- tryCatch(fns[[nm]](), error = function(e) {
      out <- list(title = sprintf("churn %s (failed)", nm),
                  warnings = sprintf("%s: %s",
                                      class(e)[1], conditionMessage(e)))
      class(out) <- c("morie_otis_result", "morie_rich_result", "list")
      out
    })
    results[[nm]] <- r
    if (!is.null(out_dir)) {
      tryCatch({
        writeLines(format(r),
                   con = file.path(out_dir,
                                   sprintf("churn_%s.txt", nm)))
        if (requireNamespace("jsonlite", quietly = TRUE)) {
          writeLines(jsonlite::toJSON(r$payload, pretty = TRUE,
                                       auto_unbox = TRUE, null = "null",
                                       force = TRUE),
                     con = file.path(out_dir,
                                     sprintf("churn_%s.json", nm)))
        }
      }, error = function(e) {
        warning(sprintf("Could not write %s output: %s", nm,
                        conditionMessage(e)))
      })
    }
  }
  results
}
