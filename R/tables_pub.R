# SPDX-License-Identifier: AGPL-3.0-or-later
#' Publication-ready table generation
#'
#' R port of the Python module \code{morie.tables_pub}. Builds Table 1
#' (baseline characteristics), regression tables, odds-ratio and
#' hazard-ratio tables, correlation matrices, model comparison tables,
#' ANOVA tables, summary-statistics tables and treatment-effect tables.
#'
#' Output rendering goes through \pkg{knitr::kable} for "latex", "html",
#' "markdown", "pipe" and "rst" formats; "dataframe" returns the raw
#' \code{data.frame}, and "text" returns \code{utils::capture.output} on
#' the frame. The \pkg{gt} package is supported as an optional
#' richer-output backend when installed (Suggests-gated).
#'
#' Functions consume R-native model objects:
#' \itemize{
#'   \item \code{regression_table} accepts \code{lm}, \code{glm} or any
#'         object responding to \code{coef}, \code{vcov} and
#'         \code{confint}.
#'   \item \code{odds_ratio_table} accepts a fitted \code{glm} with
#'         \code{family = binomial()}.
#'   \item \code{hazard_ratio_table} accepts a \code{coxph} fit, or
#'         per-parameter \code{beta}, \code{se} and \code{p} vectors.
#'   \item \code{anova_table} wraps \code{stats::anova} / \code{car::Anova}.
#' }
#'
#' @name tables_pub
NULL


# ---------------------------------------------------------------------------
# Formatting helpers
# ---------------------------------------------------------------------------

.tbl_fmt_num <- function(x, digits = 2L, apa = FALSE) {
  if (!is.finite(x)) return("")
  s <- formatC(x, format = "f", digits = digits)
  if (apa && abs(x) < 1) {
    if (startsWith(s, "0"))  s <- substring(s, 2)
    if (startsWith(s, "-0")) s <- paste0("-", substring(s, 3))
  }
  s
}

.tbl_fmt_pval <- function(p, digits = 3L, apa = FALSE) {
  if (!is.finite(p)) return("")
  if (p < 10^(-digits))
    return(sprintf("<%s%s1",
      if (apa) "." else "0.",
      strrep("0", digits - 1L)))
  s <- formatC(p, format = "f", digits = digits)
  if (apa && startsWith(s, "0")) s <- substring(s, 2)
  s
}

.tbl_stars <- function(p) {
  if (!is.finite(p)) return("")
  if (p < 0.001) return("***")
  if (p < 0.01)  return("**")
  if (p < 0.05)  return("*")
  ""
}

.tbl_smd <- function(m1, m2, sd1, sd2) {
  ps <- sqrt((sd1^2 + sd2^2) / 2)
  if (ps < 1e-12) return(0)
  (m1 - m2) / ps
}


# ---------------------------------------------------------------------------
# Footnote registry
# ---------------------------------------------------------------------------

.tbl_footnotes_new <- function() {
  e <- new.env(parent = emptyenv())
  e$notes <- character(0)
  e
}

.tbl_footnotes_add <- function(reg, text) {
  if (!(text %in% reg$notes)) reg$notes <- c(reg$notes, text)
  letters[match(text, reg$notes) %% 26L + 1L]
}

.tbl_footnotes_render <- function(reg, fmt = "text") {
  if (length(reg$notes) == 0L) return("")
  out <- character(0)
  for (i in seq_along(reg$notes)) {
    sym <- letters[((i - 1L) %% 26L) + 1L]
    out <- c(out, switch(fmt,
      "latex"    = sprintf("\\textsuperscript{%s} %s", sym, reg$notes[i]),
      "html"     = sprintf("<sup>%s</sup> %s", sym, reg$notes[i]),
      sprintf("  %s %s", sym, reg$notes[i])))
  }
  paste(out, collapse = "\
")
}


# ---------------------------------------------------------------------------
# Format conversion
# ---------------------------------------------------------------------------

.tbl_to_format <- function(df, fmt = c("dataframe", "latex", "html",
                                        "markdown", "text", "csv"),
                            title = "", footnotes = "") {
  fmt <- match.arg(fmt)
  if (fmt == "dataframe") return(df)
  if (fmt == "csv") {
    buf <- character(0)
    if (nzchar(title)) buf <- c(buf, paste0("# ", title))
    buf <- c(buf, utils::capture.output(utils::write.csv(df, row.names = TRUE)))
    if (nzchar(footnotes)) buf <- c(buf, "", footnotes)
    return(paste(buf, collapse = "\
"))
  }
  kable_fmt <- switch(fmt,
                      "markdown" = "pipe",
                      "latex"    = "latex",
                      "html"     = "html",
                      "text"     = "rst")
  out <- knitr::kable(df, format = kable_fmt, caption = title)
  if (nzchar(footnotes)) out <- paste0(out, "\
\
", footnotes)
  out
}


# ---------------------------------------------------------------------------
# Table 1: baseline characteristics
# ---------------------------------------------------------------------------

#' Table 1 (baseline characteristics) stratified by group
#'
#' @param data Data frame.
#' @param group_col Column defining groups, or NULL.
#' @param continuous_vars Continuous variable names (auto-detect numeric
#'   non-group columns if NULL).
#' @param categorical_vars Categorical names (auto-detect character /
#'   factor / logical if NULL).
#' @param continuous_summary "mean_sd", "median_iqr" or "mean_ci".
#' @param show_p Include p-value column.
#' @param show_smd Include SMD column (2 groups only).
#' @param show_missing Include missing count.
#' @param weights Column name for survey weights or NULL.
#' @param digits Decimal places.
#' @param apa APA-style p-value formatting.
#' @param output_format "dataframe", "latex", "html", "markdown",
#'   "text", "csv".
#' @param title Table title.
#' @export
table1 <- function(data, group_col = NULL,
                    continuous_vars = NULL, categorical_vars = NULL,
                    continuous_summary = c("mean_sd", "median_iqr", "mean_ci"),
                    show_p = TRUE, show_smd = TRUE, show_missing = TRUE,
                    weights = NULL, digits = 2L, apa = FALSE,
                    output_format = "dataframe",
                    title = "Table 1. Baseline Characteristics") {
  continuous_summary <- match.arg(continuous_summary)
  df <- data
  reg <- .tbl_footnotes_new()

  if (is.null(continuous_vars)) {
    is_num <- vapply(df, is.numeric, logical(1))
    continuous_vars <- setdiff(names(df)[is_num], c(group_col, weights))
  }
  if (is.null(categorical_vars)) {
    is_cat <- vapply(df, function(c)
      is.character(c) || is.factor(c) || is.logical(c), logical(1))
    categorical_vars <- setdiff(names(df)[is_cat], c(group_col))
  }
  groups <- if (!is.null(group_col)) sort(unique(stats::na.omit(df[[group_col]])))
            else list(NULL)

  group_label <- function(g) if (is.null(g)) "Overall" else as.character(g)
  group_subset <- function(g) if (is.null(g)) df else df[df[[group_col]] %in% g, ]

  rows <- list()
  row_labels <- character(0)

  # N row
  n_row <- vapply(groups, function(g) as.character(nrow(group_subset(g))), "")
  names(n_row) <- vapply(groups, group_label, "")
  rows[[length(rows) + 1L]] <- as.list(n_row)
  row_labels <- c(row_labels, "N")

  weighted_mean_sd <- function(v, w) {
    keep <- !is.na(v)
    v <- v[keep]
    if (!is.null(w)) { w <- w[keep]
    m <- stats::weighted.mean(v, w)
      s <- sqrt(stats::weighted.mean((v - m)^2, w))
      return(c(m, s)) }
    c(mean(v), stats::sd(v))
  }

  for (var in continuous_vars) {
    row <- list()
    group_stats <- list()
    for (g in groups) {
      sub <- group_subset(g)
      vals <- sub[[var]]
      vals <- vals[!is.na(vals)]
      w_vals <- if (!is.null(weights)) sub[[weights]] else NULL
      if (continuous_summary == "mean_sd") {
        ms <- weighted_mean_sd(sub[[var]], w_vals)
        cell <- sprintf("%s (%s)",
          .tbl_fmt_num(ms[1], digits), .tbl_fmt_num(ms[2], digits))
      } else if (continuous_summary == "median_iqr") {
        cell <- sprintf("%s [%s, %s]",
          .tbl_fmt_num(stats::median(vals), digits),
          .tbl_fmt_num(stats::quantile(vals, 0.25, names = FALSE), digits),
          .tbl_fmt_num(stats::quantile(vals, 0.75, names = FALSE), digits))
        ms <- c(mean(vals), stats::sd(vals))
      } else {
        ms <- weighted_mean_sd(sub[[var]], w_vals)
        n <- length(vals)
        se <- if (n > 0) ms[2] / sqrt(n) else 0
        cell <- sprintf("%s (%s, %s)",
          .tbl_fmt_num(ms[1], digits),
          .tbl_fmt_num(ms[1] - 1.96 * se, digits),
          .tbl_fmt_num(ms[1] + 1.96 * se, digits))
      }
      row[[group_label(g)]] <- cell
      group_stats[[length(group_stats) + 1L]] <- ms
    }
    if (show_p && length(groups) >= 2L && !is.null(group_col)) {
      gv <- lapply(groups, function(g) {
        v <- group_subset(g)[[var]]
        v[!is.na(v)]
      })
      if (length(groups) == 2L) {
        wt <- suppressWarnings(stats::wilcox.test(gv[[1]], gv[[2]]))
        p <- wt$p.value
      } else {
        vals <- unlist(gv)
        grp <- rep(seq_along(gv), lengths(gv))
        p <- stats::kruskal.test(vals, grp)$p.value
      }
      row[["p-value"]] <- .tbl_fmt_pval(p, 3L, apa)
      row[[" "]] <- .tbl_stars(p)
    }
    if (show_smd && length(groups) == 2L) {
      smd <- .tbl_smd(group_stats[[1]][1], group_stats[[2]][1],
                      group_stats[[1]][2], group_stats[[2]][2])
      row[["SMD"]] <- .tbl_fmt_num(abs(smd), 3L)
    }
    if (show_missing) {
      nm <- sum(is.na(df[[var]]))
      pct <- 100 * nm / nrow(df)
      row[["Missing"]] <- sprintf("%d (%s%%)", nm, .tbl_fmt_num(pct, 1L))
    }
    rows[[length(rows) + 1L]] <- row
    label_suffix <- switch(continuous_summary,
                           "mean_sd" = "mean (SD)",
                           "median_iqr" = "median [IQR]",
                           "mean_ci" = "mean (95% CI)")
    row_labels <- c(row_labels, sprintf("%s, %s", var, label_suffix))
  }

  for (var in categorical_vars) {
    cats <- sort(unique(stats::na.omit(df[[var]])), method = "radix")
    header_row <- setNames(as.list(rep("", length(groups))),
                            vapply(groups, group_label, ""))
    rows[[length(rows) + 1L]] <- header_row
    row_labels <- c(row_labels, sprintf("%s, n (%%)", var))
    header_idx <- length(rows)
    for (cat in cats) {
      row <- list()
      for (g in groups) {
        sub <- group_subset(g)
        n_cat <- sum(sub[[var]] == cat, na.rm = TRUE)
        n_total <- sum(!is.na(sub[[var]]))
        pct <- if (n_total > 0) 100 * n_cat / n_total else 0
        row[[group_label(g)]] <- sprintf("%d (%s%%)", n_cat,
                                          .tbl_fmt_num(pct, 1L))
      }
      rows[[length(rows) + 1L]] <- row
      row_labels <- c(row_labels, sprintf("  %s", as.character(cat)))
    }
    if (show_p && length(groups) >= 2L && !is.null(group_col)) {
      tab <- table(df[[var]], df[[group_col]])
      p <- tryCatch(suppressWarnings(stats::chisq.test(tab))$p.value,
                     error = function(e) NA_real_)
      rows[[header_idx]][["p-value"]] <- .tbl_fmt_pval(p, 3L, apa)
      rows[[header_idx]][[" "]] <- .tbl_stars(p)
    }
    if (show_missing) {
      nm <- sum(is.na(df[[var]]))
      pct <- 100 * nm / nrow(df)
      rows[[header_idx]][["Missing"]] <-
        sprintf("%d (%s%%)", nm, .tbl_fmt_num(pct, 1L))
    }
  }

  if (show_p)
    .tbl_footnotes_add(reg,
      "Continuous: Mann-Whitney U (2 groups) or Kruskal-Wallis; Categorical: Chi-square")
  if (show_smd) .tbl_footnotes_add(reg, "SMD = Standardised Mean Difference (absolute value)")
  if (!is.null(weights)) .tbl_footnotes_add(reg, sprintf("Weighted by '%s'", weights))

  # Harmonise columns and assemble data frame
  all_cols <- unique(unlist(lapply(rows, names)))
  norm_rows <- lapply(rows, function(r) {
    out <- setNames(as.list(rep("", length(all_cols))), all_cols)
    for (k in names(r)) out[[k]] <- r[[k]]
    out
  })
  result_df <- do.call(rbind.data.frame,
                       c(lapply(norm_rows, as.data.frame, stringsAsFactors = FALSE),
                         stringsAsFactors = FALSE))
  rownames(result_df) <- row_labels
  fmt <- match.arg(output_format,
                   c("dataframe", "latex", "html", "markdown", "text", "csv"))
  .tbl_to_format(result_df, fmt, title = title,
                  footnotes = .tbl_footnotes_render(reg, fmt))
}


# ---------------------------------------------------------------------------
# Regression table
# ---------------------------------------------------------------------------

.tbl_extract_model <- function(m) {
  b <- stats::coef(m)
  se <- sqrt(diag(stats::vcov(m)))
  z <- b / se
  # Wald p-value (also matches t-test from lm summaries closely)
  s <- summary(m)
  pv <- tryCatch(stats::coef(s)[, ncol(stats::coef(s))],
                  error = function(e) 2 * stats::pnorm(-abs(z)))
  ci <- tryCatch(suppressMessages(stats::confint.default(m)),
                  error = function(e) cbind(b - 1.96 * se, b + 1.96 * se))
  list(params = b, se = se, pvalues = pv, ci = ci,
       nobs = tryCatch(stats::nobs(m), error = function(e) NA_real_),
       rsquared = if (inherits(m, "lm") && !inherits(m, "glm"))
         summary(m)$r.squared else NA_real_,
       aic = tryCatch(stats::AIC(m), error = function(e) NA_real_),
       bic = tryCatch(stats::BIC(m), error = function(e) NA_real_),
       llf = tryCatch(as.numeric(stats::logLik(m)),
                       error = function(e) NA_real_))
}

#' Side-by-side regression table for multiple model fits
#'
#' @param models Named list of fitted models (e.g. \code{lm}, \code{glm}).
#' @param exponentiate Exponentiate coefficients (for OR / HR).
#' @param show_ci Include CI line under each coefficient.
#' @param show_stars Append significance stars.
#' @param confidence Confidence level for CIs.
#' @param digits Decimal places.
#' @param model_stats Vector of model-stat keys from
#'   \code{c("nobs","rsquared","aic","bic","llf")}.
#' @param apa APA p-value formatting.
#' @param output_format Output target.
#' @param title Title.
#' @export
regression_table <- function(models, exponentiate = FALSE,
                              show_ci = TRUE, show_stars = TRUE,
                              confidence = 0.95, digits = 3L,
                              model_stats = c("nobs", "rsquared",
                                               "aic", "bic", "llf"),
                              apa = FALSE,
                              output_format = "dataframe",
                              title = "Regression Results") {
  reg <- .tbl_footnotes_new()
  meta <- lapply(models, .tbl_extract_model)
  all_params <- unique(unlist(lapply(meta, function(m) names(m$params))))
  rows <- list()
  row_labels <- character(0)

  for (param in all_params) {
    coef_row <- list()
    se_row <- list()
    ci_row <- list()
    for (mname in names(models)) {
      mi <- meta[[mname]]
      if (param %in% names(mi$params)) {
        b <- mi$params[[param]]
        se <- mi$se[[param]]
        p <- mi$pvalues[[param]]
        ci_lo <- mi$ci[param, 1]
        ci_hi <- mi$ci[param, 2]
        if (exponentiate) {
          b <- exp(b)
          ci_lo <- exp(ci_lo)
          ci_hi <- exp(ci_hi)
        }
        coef_row[[mname]] <- paste0(.tbl_fmt_num(b, digits, apa),
                                     if (show_stars) .tbl_stars(p) else "")
        se_row[[mname]] <- sprintf("(%s)", .tbl_fmt_num(se, digits, apa))
        if (show_ci) ci_row[[mname]] <- sprintf("[%s, %s]",
          .tbl_fmt_num(ci_lo, digits), .tbl_fmt_num(ci_hi, digits))
      } else {
        coef_row[[mname]] <- ""
        se_row[[mname]] <- ""
        if (show_ci) ci_row[[mname]] <- ""
      }
    }
    rows <- c(rows, list(coef_row, se_row))
    row_labels <- c(row_labels, param, "")
    if (show_ci) { rows <- c(rows, list(ci_row))
    row_labels <- c(row_labels, "") }
  }

  stat_label <- c(nobs = "N", rsquared = "R-squared",
                   aic = "AIC", bic = "BIC", llf = "Log-Likelihood")
  for (stat in model_stats) {
    sr <- list()
    for (mname in names(models)) {
      val <- meta[[mname]][[stat]]
      sr[[mname]] <- if (is.finite(val)) {
        if (stat == "nobs") as.character(as.integer(val))
        else .tbl_fmt_num(val, digits)
      } else ""
    }
    rows <- c(rows, list(sr))
    row_labels <- c(row_labels,
                    if (!is.null(stat_label[stat])) stat_label[stat] else stat)
  }

  if (show_stars)
    .tbl_footnotes_add(reg, "* p < 0.05, ** p < 0.01, *** p < 0.001")
  .tbl_footnotes_add(reg,
    sprintf("Standard errors in parentheses. CI level: %d%%.",
            as.integer(confidence * 100)))

  all_cols <- names(models)
  norm <- lapply(rows, function(r) {
    out <- setNames(as.list(rep("", length(all_cols))), all_cols)
    for (k in names(r)) out[[k]] <- r[[k]]
    out
  })
  result_df <- do.call(rbind.data.frame,
                        c(lapply(norm, as.data.frame, stringsAsFactors = FALSE),
                          stringsAsFactors = FALSE))
  # Disambiguate duplicate empty labels for row.names (SE / CI rows).
  rn <- make.unique(ifelse(nzchar(row_labels), row_labels,
                            paste0(".row", seq_along(row_labels))),
                    sep = "_")
  rownames(result_df) <- rn
  # Stash the human-readable labels in a column for users that need them.
  result_df <- cbind(term = row_labels, result_df, stringsAsFactors = FALSE)
  fmt <- match.arg(output_format,
                    c("dataframe", "latex", "html", "markdown", "text", "csv"))
  .tbl_to_format(result_df, fmt, title = title,
                  footnotes = .tbl_footnotes_render(reg, fmt))
}


# ---------------------------------------------------------------------------
# Odds-ratio table (from glm logistic)
# ---------------------------------------------------------------------------

#' Odds-ratio table from a fitted logistic GLM
#' @param model A fitted \code{glm} with \code{family = binomial()}.
#' @param confidence Confidence level.
#' @param digits Decimal places.
#' @param apa APA formatting.
#' @param output_format Output target.
#' @param title Title.
#' @export
odds_ratio_table <- function(model, confidence = 0.95, digits = 3L,
                              apa = FALSE, output_format = "dataframe",
                              title = "Odds Ratios") {
  alpha <- 1 - confidence
  ci <- suppressMessages(stats::confint.default(model,
                          level = confidence))
  b <- stats::coef(model)
  pv <- summary(model)$coefficients[, 4]
  recs <- lapply(names(b), function(p) {
    or <- exp(b[[p]])
    lo <- exp(ci[p, 1])
    hi <- exp(ci[p, 2])
    pp <- pv[[p]]
    list(Variable = p,
         OR = .tbl_fmt_num(or, digits, apa),
         CI = sprintf("(%s, %s)", .tbl_fmt_num(lo, digits),
                       .tbl_fmt_num(hi, digits)),
         `p-value` = .tbl_fmt_pval(pp, digits, apa),
         ` ` = .tbl_stars(pp))
  })
  result_df <- do.call(rbind.data.frame,
                        c(lapply(recs, as.data.frame, stringsAsFactors = FALSE),
                          stringsAsFactors = FALSE))
  names(result_df)[2:3] <- c("OR", sprintf("%d%% CI",
                                            as.integer(confidence * 100)))
  rownames(result_df) <- result_df$Variable
  result_df$Variable <- NULL
  reg <- .tbl_footnotes_new()
  .tbl_footnotes_add(reg, "OR = Odds Ratio. * p<0.05, ** p<0.01, *** p<0.001")
  fmt <- match.arg(output_format,
                    c("dataframe", "latex", "html", "markdown", "text", "csv"))
  .tbl_to_format(result_df, fmt, title = title,
                  footnotes = .tbl_footnotes_render(reg, fmt))
}


# ---------------------------------------------------------------------------
# Hazard-ratio table (from Cox params)
# ---------------------------------------------------------------------------

#' Hazard-ratio table from Cox model components
#' @param params Named numeric vector of log-HR coefficients.
#' @param se Named numeric vector of standard errors.
#' @param pvalues Named numeric vector of p-values.
#' @param confidence Confidence level.
#' @param digits Decimal places.
#' @param apa APA formatting.
#' @param output_format Output target.
#' @param title Title.
#' @export
hazard_ratio_table <- function(params, se, pvalues, confidence = 0.95,
                                digits = 3L, apa = FALSE,
                                output_format = "dataframe",
                                title = "Hazard Ratios") {
  z <- stats::qnorm(1 - (1 - confidence) / 2)
  recs <- lapply(names(params), function(v) {
    b <- params[[v]]
    s <- se[[v]]
    hr <- exp(b)
    lo <- exp(b - z * s)
    hi <- exp(b + z * s)
    pp <- pvalues[[v]]
    list(Variable = v,
         HR = .tbl_fmt_num(hr, digits, apa),
         CI = sprintf("(%s, %s)", .tbl_fmt_num(lo, digits),
                       .tbl_fmt_num(hi, digits)),
         `p-value` = .tbl_fmt_pval(pp, digits, apa),
         ` ` = .tbl_stars(pp))
  })
  result_df <- do.call(rbind.data.frame,
                        c(lapply(recs, as.data.frame, stringsAsFactors = FALSE),
                          stringsAsFactors = FALSE))
  names(result_df)[2:3] <- c("HR", sprintf("%d%% CI",
                                            as.integer(confidence * 100)))
  rownames(result_df) <- result_df$Variable
  result_df$Variable <- NULL
  reg <- .tbl_footnotes_new()
  .tbl_footnotes_add(reg, "HR = Hazard Ratio. * p<0.05, ** p<0.01, *** p<0.001")
  fmt <- match.arg(output_format,
                    c("dataframe", "latex", "html", "markdown", "text", "csv"))
  .tbl_to_format(result_df, fmt, title = title,
                  footnotes = .tbl_footnotes_render(reg, fmt))
}


# ---------------------------------------------------------------------------
# Correlation matrix table
# ---------------------------------------------------------------------------

#' Pairwise correlation matrix with significance stars
#' @param data Data frame.
#' @param method "pearson", "spearman", "kendall".
#' @param show_stars Annotate cells with significance stars.
#' @param mask_diagonal Replace diagonal with "-".
#' @param digits Decimal places.
#' @param output_format Output target.
#' @param title Title.
#' @export
correlation_table <- function(data, method = "pearson", show_stars = TRUE,
                                mask_diagonal = TRUE, digits = 3L,
                                output_format = "dataframe",
                                title = "Correlation Matrix") {
  numeric_df <- data[, vapply(data, is.numeric, logical(1)), drop = FALSE]
  cols <- names(numeric_df)
  n_v <- length(cols)
  corr <- stats::cor(numeric_df, method = method, use = "pairwise.complete.obs")
  result <- matrix("", n_v, n_v, dimnames = list(cols, cols))
  for (i in seq_len(n_v)) for (j in seq_len(n_v)) {
    if (i == j) {
      result[i, j] <- if (mask_diagonal) "-" else .tbl_fmt_num(1, digits)
      next
    }
    valid <- stats::complete.cases(numeric_df[, c(i, j)])
    if (sum(valid) > 2L) {
      ct <- suppressWarnings(stats::cor.test(
        numeric_df[valid, i], numeric_df[valid, j], method = method))
      p <- ct$p.value
    } else p <- NA_real_
    star <- if (show_stars) .tbl_stars(p) else ""
    result[i, j] <- paste0(.tbl_fmt_num(corr[i, j], digits), star)
  }
  out_df <- as.data.frame(result, stringsAsFactors = FALSE)
  reg <- .tbl_footnotes_new()
  if (show_stars) .tbl_footnotes_add(reg, "* p<0.05, ** p<0.01, *** p<0.001")
  .tbl_footnotes_add(reg, sprintf("Method: %s", tools::toTitleCase(method)))
  fmt <- match.arg(output_format,
                    c("dataframe", "latex", "html", "markdown", "text", "csv"))
  .tbl_to_format(out_df, fmt, title = title,
                  footnotes = .tbl_footnotes_render(reg, fmt))
}


# ---------------------------------------------------------------------------
# Model comparison table
# ---------------------------------------------------------------------------

#' Compare multiple model fits on AIC, BIC, log-likelihood and (optional)
#' LR tests
#' @param models Named list of fitted models.
#' @param nested If TRUE, run LR tests against the previous model in the list.
#' @param digits Decimal places.
#' @param output_format Output target.
#' @param title Title.
#' @export
model_comparison_table <- function(models, nested = FALSE, digits = 3L,
                                     output_format = "dataframe",
                                     title = "Model Comparison") {
  prev_llf <- NA_real_
  prev_df <- NA_real_
  recs <- list()
  for (mname in names(models)) {
    m <- models[[mname]]
    nobs <- tryCatch(stats::nobs(m), error = function(e) NA_real_)
    df_m <- tryCatch(attr(stats::logLik(m), "df"), error = function(e) NA_real_)
    llf <- tryCatch(as.numeric(stats::logLik(m)), error = function(e) NA_real_)
    aic <- tryCatch(stats::AIC(m), error = function(e) NA_real_)
    bic <- tryCatch(stats::BIC(m), error = function(e) NA_real_)
    r2 <- if (inherits(m, "lm") && !inherits(m, "glm"))
            summary(m)$r.squared else NA_real_
    rec <- list(Model = mname,
                 N = if (is.finite(nobs)) as.character(as.integer(nobs)) else "",
                 df = if (is.finite(df_m)) .tbl_fmt_num(df_m, 0L) else "",
                 `Log-Lik` = if (is.finite(llf)) .tbl_fmt_num(llf, digits) else "",
                 AIC = if (is.finite(aic)) .tbl_fmt_num(aic, digits) else "",
                 BIC = if (is.finite(bic)) .tbl_fmt_num(bic, digits) else "")
    if (is.finite(r2)) rec[["R-sq"]] <- .tbl_fmt_num(r2, digits)
    if (nested && is.finite(prev_llf) && is.finite(llf)) {
      lr <- 2 * (llf - prev_llf)
      ddf <- df_m - prev_df
      if (is.finite(lr) && is.finite(ddf) && lr > 0 && ddf > 0) {
        rec[["LR stat"]] <- .tbl_fmt_num(lr, digits)
        rec[["LR p"]] <- .tbl_fmt_pval(stats::pchisq(lr, ddf, lower.tail = FALSE), 3L)
      } else { rec[["LR stat"]] <- ""
      rec[["LR p"]] <- "" }
    } else if (nested) {
      rec[["LR stat"]] <- ""
      rec[["LR p"]] <- ""
    }
    prev_llf <- llf
    prev_df <- df_m
    recs[[length(recs) + 1L]] <- rec
  }
  all_cols <- unique(unlist(lapply(recs, names)))
  recs <- lapply(recs, function(r) {
    out <- setNames(as.list(rep("", length(all_cols))), all_cols)
    for (k in names(r)) out[[k]] <- r[[k]]
    out
  })
  result_df <- do.call(rbind.data.frame,
                        c(lapply(recs, as.data.frame, stringsAsFactors = FALSE),
                          stringsAsFactors = FALSE))
  rownames(result_df) <- result_df$Model
  result_df$Model <- NULL
  reg <- .tbl_footnotes_new()
  if (nested) .tbl_footnotes_add(reg,
    "LR test compares each model to the one above it")
  fmt <- match.arg(output_format,
                    c("dataframe", "latex", "html", "markdown", "text", "csv"))
  .tbl_to_format(result_df, fmt, title = title,
                  footnotes = .tbl_footnotes_render(reg, fmt))
}


# ---------------------------------------------------------------------------
# ANOVA table
# ---------------------------------------------------------------------------

#' ANOVA table from a fitted model
#'
#' Uses \code{stats::anova} for sequential (Type-I) tests, or
#' \code{car::Anova} for Type-II/III if \pkg{car} is installed.
#'
#' @param model An \code{lm}/\code{aov}/\code{glm} fit.
#' @param typ ANOVA type (1, 2, 3).
#' @param digits Decimal places.
#' @param output_format Output target.
#' @param title Title.
#' @export
anova_table <- function(model, typ = 2L, digits = 3L,
                          output_format = "dataframe",
                          title = "ANOVA Table") {
  if (typ == 1L) {
    tab <- as.data.frame(stats::anova(model))
  } else {
    if (!requireNamespace("car", quietly = TRUE))
      stop("Type-II/III ANOVA requires the 'car' package.")
    tab <- as.data.frame(car::Anova(model, type = typ))
  }
  formatted <- tab
  for (col in c("Sum Sq", "Mean Sq", "F value", "F", "LR Chisq")) {
    if (col %in% colnames(formatted))
      formatted[[col]] <- vapply(formatted[[col]],
        function(x) if (is.finite(x)) .tbl_fmt_num(x, digits) else "", "")
  }
  pcol <- intersect(c("Pr(>F)", "Pr(>Chisq)"), colnames(formatted))
  if (length(pcol) > 0L) {
    formatted[["p-value"]] <- vapply(tab[[pcol[1]]],
      function(x) if (is.finite(x)) .tbl_fmt_pval(x, 3L) else "", "")
    formatted[[" "]] <- vapply(tab[[pcol[1]]],
      function(x) if (is.finite(x)) .tbl_stars(x) else "", "")
    formatted[[pcol[1]]] <- NULL
  }
  if ("Df" %in% colnames(formatted))
    formatted[["Df"]] <- vapply(formatted[["Df"]],
      function(x) if (is.finite(x)) as.character(as.integer(x)) else "", "")

  reg <- .tbl_footnotes_new()
  .tbl_footnotes_add(reg,
    sprintf("Type %d ANOVA. * p<0.05, ** p<0.01, *** p<0.001", typ))
  fmt <- match.arg(output_format,
                    c("dataframe", "latex", "html", "markdown", "text", "csv"))
  .tbl_to_format(formatted, fmt, title = title,
                  footnotes = .tbl_footnotes_render(reg, fmt))
}


# ---------------------------------------------------------------------------
# Number formatting
# ---------------------------------------------------------------------------

#' Format a single number according to style conventions
#' @param x Numeric.
#' @param style "fixed", "scientific", "percent", "integer".
#' @param digits Decimal places.
#' @param apa APA-style leading-zero suppression.
#' @export
format_number <- function(x, style = c("fixed", "scientific",
                                          "percent", "integer"),
                            digits = 2L, apa = FALSE) {
  if (!is.finite(x)) return("")
  style <- match.arg(style)
  switch(style,
    "fixed"      = .tbl_fmt_num(x, digits, apa),
    "scientific" = formatC(x, format = "e", digits = digits),
    "percent"    = sprintf("%s%%", .tbl_fmt_num(x * 100, digits)),
    "integer"    = as.character(as.integer(round(x))))
}

#' Apply uniform formatting to numeric columns
#' @param df Data frame.
#' @param numeric_fmt sprintf-style format spec without leading "%".
#' @param pval_cols Columns to format as p-values.
#' @param output_format Output target.
#' @param title Title.
#' @export
format_dataframe <- function(df, numeric_fmt = "%.2f",
                                pval_cols = NULL,
                                output_format = "dataframe",
                                title = "") {
  out <- df
  for (col in names(df)) {
    if (!is.numeric(df[[col]])) next
    if (!is.null(pval_cols) && col %in% pval_cols) {
      out[[col]] <- vapply(df[[col]], .tbl_fmt_pval, "")
    } else {
      out[[col]] <- vapply(df[[col]], function(x)
        if (is.finite(x)) sprintf(numeric_fmt, x) else "", "")
    }
  }
  fmt <- match.arg(output_format,
                    c("dataframe", "latex", "html", "markdown", "text", "csv"))
  .tbl_to_format(out, fmt, title = title, footnotes = "")
}


# ---------------------------------------------------------------------------
# Summary statistics table
# ---------------------------------------------------------------------------

#' Descriptive statistics for a set of variables
#' @param data Data frame.
#' @param variables Variable names (auto-detect numeric if NULL).
#' @param stats Vector of statistic names.
#' @param digits Decimal places.
#' @param output_format Output target.
#' @param title Title.
#' @export
summary_statistics_table <- function(data, variables = NULL,
                                        stats = c("n", "mean", "sd",
                                                   "median", "min",
                                                   "max", "missing"),
                                        digits = 2L,
                                        output_format = "dataframe",
                                        title = "Summary Statistics") {
  if (is.null(variables))
    variables <- names(data)[vapply(data, is.numeric, logical(1))]
  stat_fn <- list(
    n          = function(s) sum(!is.na(s)),
    mean       = function(s) mean(s, na.rm = TRUE),
    sd         = function(s) stats::sd(s, na.rm = TRUE),
    median     = function(s) stats::median(s, na.rm = TRUE),
    min        = function(s) min(s, na.rm = TRUE),
    max        = function(s) max(s, na.rm = TRUE),
    missing    = function(s) sum(is.na(s)),
    pct_missing = function(s) 100 * mean(is.na(s)),
    q25        = function(s) stats::quantile(s, 0.25, na.rm = TRUE, names = FALSE),
    q75        = function(s) stats::quantile(s, 0.75, na.rm = TRUE, names = FALSE),
    iqr        = function(s) stats::IQR(s, na.rm = TRUE),
    skewness   = function(s) {
      m <- mean(s, na.rm = TRUE)
      sd_ <- stats::sd(s, na.rm = TRUE)
      if (sd_ == 0) 0 else mean((s - m)^3, na.rm = TRUE) / sd_^3
    },
    kurtosis   = function(s) {
      m <- mean(s, na.rm = TRUE)
      sd_ <- stats::sd(s, na.rm = TRUE)
      if (sd_ == 0) 0 else mean((s - m)^4, na.rm = TRUE) / sd_^4 - 3
    })

  recs <- lapply(variables, function(v) {
    out <- list(Variable = v)
    for (st in stats) {
      fn <- stat_fn[[st]]
      if (is.null(fn)) { out[[st]] <- ""
      next }
      val <- fn(data[[v]])
      out[[st]] <- if (st %in% c("n", "missing"))
        as.character(as.integer(val)) else .tbl_fmt_num(val, digits)
    }
    out
  })
  result_df <- do.call(rbind.data.frame,
                        c(lapply(recs, as.data.frame, stringsAsFactors = FALSE),
                          stringsAsFactors = FALSE))
  rownames(result_df) <- result_df$Variable
  result_df$Variable <- NULL
  fmt <- match.arg(output_format,
                    c("dataframe", "latex", "html", "markdown", "text", "csv"))
  .tbl_to_format(result_df, fmt, title = title, footnotes = "")
}


# ---------------------------------------------------------------------------
# Treatment-effect table
# ---------------------------------------------------------------------------

#' Summary table of causal effect estimates from multiple estimators
#' @param estimators Named list of lists; each inner list provides
#'   numeric \code{estimate}, \code{se}, \code{ci_lower}, \code{ci_upper}
#'   and \code{p_value}.
#' @param digits Decimal places.
#' @param output_format Output target.
#' @param title Title.
#' @export
treatment_effect_table <- function(estimators, digits = 3L,
                                      output_format = "dataframe",
                                      title = "Treatment Effect Estimates") {
  recs <- lapply(names(estimators), function(nm) {
    v <- estimators[[nm]]
    est <- v$estimate %||% NA_real_
    se <- v$se %||% NA_real_
    lo <- v$ci_lower %||% NA_real_
    hi <- v$ci_upper %||% NA_real_
    pp <- v$p_value %||% NA_real_
    list(Estimator = nm,
         Estimate = .tbl_fmt_num(est, digits),
         SE = .tbl_fmt_num(se, digits),
         `95% CI` = sprintf("(%s, %s)", .tbl_fmt_num(lo, digits),
                              .tbl_fmt_num(hi, digits)),
         `p-value` = .tbl_fmt_pval(pp, 3L),
         ` ` = .tbl_stars(pp))
  })
  result_df <- do.call(rbind.data.frame,
                        c(lapply(recs, as.data.frame, stringsAsFactors = FALSE),
                          stringsAsFactors = FALSE))
  rownames(result_df) <- result_df$Estimator
  result_df$Estimator <- NULL
  reg <- .tbl_footnotes_new()
  .tbl_footnotes_add(reg, "* p<0.05, ** p<0.01, *** p<0.001")
  fmt <- match.arg(output_format,
                    c("dataframe", "latex", "html", "markdown", "text", "csv"))
  .tbl_to_format(result_df, fmt, title = title,
                  footnotes = .tbl_footnotes_render(reg, fmt))
}

# Null-coalesce
