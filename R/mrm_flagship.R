# SPDX-License-Identifier: AGPL-3.0-or-later
#
# The MRM research framework (feat/native-specializations, module 24
# -- the flagship). Multilevel Reconciliation Methodology: load a
# special-investigations dataset with provenance, reconcile it against
# a second source under an explicit matching schema, estimate a causal
# effect by composing the branch's native estimators (matching / IPW /
# AIPW / DML) with multiple-testing correction, and render a
# publication-ready table with rmorie's own renderer (no
# stargazer/gt).

#' Load a special-investigations sample with provenance metadata
#'
#' Wraps the bundled samples (\code{\link{morie_sample}}) in a
#' provenance envelope: source name, retrieval path, row count, and a
#' native SHA-256 checksum of the data (module 22), so an MRM analysis
#' records exactly which snapshot it consumed.
#'
#' @param name One of the bundled sample keys
#'   (\code{"otis_b01"}, \code{"otis_b09"}, \code{"otis_c11"},
#'   \code{"tps_assault"}).
#' @return An object of class \code{morie_mrm_dataset}: list with
#'   \code{data}, \code{provenance} (name, path, n_rows, n_cols,
#'   sha256, loaded_at).
#' @examplesIf requireNamespace("rmoriedata", quietly = TRUE)
#' d <- morie_mrm_load_si_dataset("otis_b01")
#' d$provenance$n_rows
#' @examples
#' \dontshow{if (requireNamespace("rmoriedata", quietly = TRUE)) withAutoprint(\{ # examplesIf}
#' d <- morie_mrm_load_si_dataset("otis_b01")
#' d$provenance$n_rows
#' \dontshow{\}) # examplesIf}
#' @export
morie_mrm_load_si_dataset <- function(name = "otis_b01") {
  data <- morie_sample(name)
  buf <- paste(utils::capture.output(
    utils::write.csv(data, row.names = FALSE)), collapse = "\n")
  structure(
    list(
      data = data,
      provenance = list(
        name = name,
        n_rows = nrow(data),
        n_cols = ncol(data),
        sha256 = .rmorie_sha256_hex_impl(buf),
        loaded_at = format(Sys.time(), tz = "UTC",
                           "%Y-%m-%dT%H:%M:%SZ"))),
    class = c("morie_mrm_dataset", "list"))
}

#' Reconcile two sources under an explicit matching schema
#'
#' The reconciliation step of the MRM: joins a primary and a secondary
#' source on identifier keys, optionally tolerating bounded
#' disagreement on numeric fields (e.g. dates recorded a day apart),
#' and reports the match rate, the orphans on each side, and
#' field-level conflicts among matched records.
#'
#' @param primary,secondary Data frames (or
#'   \code{morie_mrm_dataset} objects).
#' @param keys Character vector of join-key column names present in
#'   both sources.
#' @param compare Optional character vector of shared columns to check
#'   for field-level conflicts among matched rows.
#' @param numeric_tolerance Absolute tolerance under which numeric
#'   disagreements in \code{compare} columns are NOT conflicts
#'   (default 0).
#' @return An object of class \code{morie_mrm_reconciliation}: list
#'   with \code{matched} (merged data frame), \code{unmatched_primary},
#'   \code{unmatched_secondary}, \code{conflicts} (long data frame:
#'   key, field, primary_value, secondary_value), \code{match_rate},
#'   \code{schema}.
#' @examples
#' a <- data.frame(id = 1:5, y = c(1, 2, 3, 4, 5))
#' b <- data.frame(id = c(1:4, 9), y = c(1, 2, 3.5, 4, 9))
#' r <- morie_mrm_reconcile(a, b, keys = "id", compare = "y")
#' r$match_rate
#' nrow(r$conflicts)
#' @export
morie_mrm_reconcile <- function(primary, secondary, keys,
                                compare = NULL,
                                numeric_tolerance = 0) {
  if (inherits(primary, "morie_mrm_dataset")) primary <- primary$data
  if (inherits(secondary, "morie_mrm_dataset")) {
    secondary <- secondary$data
  }
  primary <- .morie_check_data(primary, required = keys,
                               arg = "primary")
  secondary <- .morie_check_data(secondary, required = keys,
                                 arg = "secondary")
  pk <- do.call(paste, c(primary[keys], sep = "\r"))
  sk <- do.call(paste, c(secondary[keys], sep = "\r"))
  in_both_p <- pk %in% sk
  in_both_s <- sk %in% pk
  merged <- merge(primary, secondary, by = keys,
                  suffixes = c(".primary", ".secondary"))
  conflicts <- list()
  for (f in compare %||% character(0)) {
    cp <- paste0(f, ".primary")
    cs <- paste0(f, ".secondary")
    if (!cp %in% names(merged)) next   # column identical -> no suffix
    a <- merged[[cp]]
    b <- merged[[cs]]
    bad <- if (is.numeric(a) && is.numeric(b)) {
      abs(a - b) > numeric_tolerance & !(is.na(a) & is.na(b))
    } else {
      as.character(a) != as.character(b) & !(is.na(a) & is.na(b))
    }
    bad[is.na(bad)] <- TRUE
    if (any(bad)) {
      conflicts[[f]] <- data.frame(
        key = do.call(paste, c(merged[bad, keys, drop = FALSE],
                               sep = "/")),
        field = f,
        primary_value = as.character(a[bad]),
        secondary_value = as.character(b[bad]),
        stringsAsFactors = FALSE)
    }
  }
  conflicts <- if (length(conflicts)) {
    do.call(rbind, c(conflicts, list(make.row.names = FALSE)))
  } else {
    data.frame(key = character(), field = character(),
               primary_value = character(),
               secondary_value = character(),
               stringsAsFactors = FALSE)
  }
  structure(
    list(
      matched = merged,
      unmatched_primary = primary[!in_both_p, , drop = FALSE],
      unmatched_secondary = secondary[!in_both_s, , drop = FALSE],
      conflicts = conflicts,
      match_rate = if (nrow(primary)) mean(in_both_p) else NA_real_,
      schema = list(keys = keys, compare = compare,
                    numeric_tolerance = numeric_tolerance)),
    class = c("morie_mrm_reconciliation", "list"))
}

#' Print method for \code{morie_mrm_reconciliation} objects
#'
#' @param x A \code{morie_mrm_reconciliation} object.
#' @param ... Ignored; accepted for S3 consistency.
#' @examples
#' \donttest{
#' a <- data.frame(id = 1:5, y = c(1, 2, 3, 4, 5))
#' b <- data.frame(id = c(1:4, 9), y = c(1, 2, 3.5, 4, 9))
#' r <- morie_mrm_reconcile(a, b, keys = "id", compare = "y")
#' r$match_rate
#' nrow(r$conflicts)
#' print(r)
#' }
#' @export
print.morie_mrm_reconciliation <- function(x, ...) {
  cat("MRM reconciliation\n")
  cat(sprintf("  keys       : %s\n", paste(x$schema$keys,
                                           collapse = ", ")))
  cat(sprintf("  match rate : %.1f%% (%d matched, %d/%d orphans)\n",
              100 * x$match_rate, nrow(x$matched),
              nrow(x$unmatched_primary), nrow(x$unmatched_secondary)))
  cat(sprintf("  conflicts  : %d field-level\n", nrow(x$conflicts)))
  invisible(x)
}

#' Estimate a causal effect through the full MRM pipeline
#'
#' The estimation step of the MRM: runs the requested native
#' estimators (nearest-neighbour matching, IPW/design-based ATE, AIPW,
#' DML) on the same specification, applies a multiple-testing
#' correction across them, and bundles estimates, corrected p-values,
#' confidence intervals, per-method diagnostics, and a citation block
#' into one results object.
#'
#' @param data Data frame (or \code{morie_mrm_dataset} /
#'   \code{morie_mrm_reconciliation}, whose matched rows are used).
#' @param treatment,outcome Column names (binary 0/1 treatment).
#' @param covariates Character vector of adjustment covariates.
#' @param methods Subset of \code{c("matching", "ate", "aipw",
#'   "dml")} (default all four).
#' @param correction Multiple-testing correction passed to
#'   \code{stats::p.adjust} (default \code{"holm"}).
#' @param seed RNG seed forwarded to the stochastic estimators.
#' @return An object of class \code{morie_mrm_effect}: list with
#'   \code{results} (data frame: method, estimate, std_error,
#'   ci_lower, ci_upper, p_value, p_adjusted), \code{consensus}
#'   (inverse-variance pooled estimate), \code{correction},
#'   \code{spec}, \code{citation}.
#' @examples
#' set.seed(1)
#' n <- 400
#' x <- rnorm(n)
#' t <- rbinom(n, 1, plogis(0.5 * x))
#' y <- 1 + 0.8 * t + 0.5 * x + rnorm(n)
#' df <- data.frame(y = y, t = t, x = x)
#' eff <- morie_mrm_estimate_causal_effect(df, "t", "y", "x",
#'                                         methods = c("ate", "aipw"))
#' eff$results$estimate
#' @export
morie_mrm_estimate_causal_effect <- function(data, treatment, outcome,
                                             covariates,
                                             methods = c("matching",
                                                         "ate",
                                                         "aipw",
                                                         "dml"),
                                             correction = "holm",
                                             seed = 42L) {
  if (inherits(data, "morie_mrm_dataset")) data <- data$data
  if (inherits(data, "morie_mrm_reconciliation")) data <- data$matched
  methods <- match.arg(methods, several.ok = TRUE)
  data <- .morie_check_data(data,
                            required = c(treatment, outcome,
                                         covariates),
                            arg = "data")
  # Module 25 guard: a categorical treatment must never be silently
  # coerced (factor level indices are not data), and covariate coding
  # hazards are surfaced before any estimator runs.
  .morie_guard_binary_treatment(data[[treatment]], treatment)
  audit <- morie_audit_categories(data,
                                  cols = intersect(covariates,
                                                   names(data)))
  if (nrow(audit) && !isTRUE(attr(audit, "clean"))) {
    bad <- audit[nzchar(audit$hazards), , drop = FALSE]
    warning("categorical coding hazards in covariates [",
            paste(bad$column, collapse = ", "),
            "] - run morie_audit_categories(data) and fix before ",
            "trusting these estimates.", call. = FALSE)
  }
  rows <- list()
  diagnostics <- list()
  run <- function(fn) tryCatch(fn(), error = function(e) e)
  if ("matching" %in% methods) {
    r <- run(function() {
      m <- morie_matching_nearest_neighbor(data, treatment, covariates)
      md <- m$matched_data
      tt <- stats::t.test(md[[outcome]][md[[treatment]] == 1],
                          md[[outcome]][md[[treatment]] == 0])
      list(estimate = unname(diff(rev(tt$estimate))),
           se = unname(tt$stderr), p = tt$p.value,
           diag = list(n_pairs = nrow(m$match_pairs)))
    })
    rows[["matching (rmorie native)"]] <- r
  }
  if ("ate" %in% methods) {
    r <- run(function() {
      a <- morie_estimate_ate(data, treatment, outcome, covariates)
      list(estimate = a$ate, se = a$se,
           p = 2 * stats::pnorm(-abs(a$ate / a$se)),
           diag = list(n = nrow(data)))
    })
    rows[["ipw ate (rmorie native)"]] <- r
  }
  if ("aipw" %in% methods) {
    r <- run(function() {
      a <- morie_estimate_aipw(data, treatment, outcome, covariates)
      list(estimate = a$ate, se = a$se,
           p = 2 * stats::pnorm(-abs(a$ate / a$se)),
           diag = list(n = nrow(data)))
    })
    rows[["aipw (rmorie native)"]] <- r
  }
  if ("dml" %in% methods) {
    r <- run(function() {
      a <- morie_estimate_double_ml(data, outcome, treatment,
                                    covariates,
                                    random_state = seed)
      se <- a$se %||% a$std_error
      list(estimate = a$estimate %||% a$ate, se = se,
           p = 2 * stats::pnorm(-abs((a$estimate %||% a$ate) / se)),
           diag = list(n = nrow(data)))
    })
    rows[["dml plr (rmorie native)"]] <- r
  }
  ok <- !vapply(rows, inherits, logical(1), "error")
  if (!any(ok)) {
    stop("every requested estimator failed; first error: ",
         conditionMessage(rows[[1]]), call. = FALSE)
  }
  est <- vapply(rows[ok], function(r) r$estimate, numeric(1))
  se <- vapply(rows[ok], function(r) r$se, numeric(1))
  p <- vapply(rows[ok], function(r) r$p, numeric(1))
  p_adj <- stats::p.adjust(p, method = correction)
  z <- stats::qnorm(0.975)
  results <- data.frame(
    method = names(rows)[ok],
    estimate = est, std_error = se,
    ci_lower = est - z * se, ci_upper = est + z * se,
    p_value = p, p_adjusted = p_adj,
    stringsAsFactors = FALSE, row.names = NULL)
  w <- 1 / se^2
  consensus <- list(estimate = sum(w * est) / sum(w),
                    std_error = sqrt(1 / sum(w)))
  for (nm in names(rows)[!ok]) {
    diagnostics[[nm]] <- conditionMessage(rows[[nm]])
  }
  structure(
    list(results = results, consensus = consensus,
         correction = correction,
         spec = list(treatment = treatment, outcome = outcome,
                     covariates = covariates, n = nrow(data)),
         failed = diagnostics,
         citation = paste(
           # canonical entry from inst/CITATION (never hand-written
           # here, so it cannot drift from the package metadata)
           format(utils::citation("rmorie")[1L], style = "text"),
           collapse = " ")),
    class = c("morie_mrm_effect", "list"))
}

#' Print method for \code{morie_mrm_effect} objects
#'
#' @param x A \code{morie_mrm_effect} object.
#' @param ... Ignored; accepted for S3 consistency.
#' @examples
#' \donttest{
#' set.seed(1)
#' n <- 400
#' x <- rnorm(n)
#' t <- rbinom(n, 1, plogis(0.5 * x))
#' y <- 1 + 0.8 * t + 0.5 * x + rnorm(n)
#' df <- data.frame(y = y, t = t, x = x)
#' eff <- morie_mrm_estimate_causal_effect(df, "t", "y", "x",
#'                                         methods = c("ate", "aipw"))
#' eff$results$estimate
#' print(eff)
#' }
#' @export
print.morie_mrm_effect <- function(x, ...) {
  cat(morie_mrm_report(x, format = "text"), sep = "\n")
  invisible(x)
}

#' Render a publication-ready table from an MRM effect object
#'
#' rmorie's own renderer (no stargazer / gt / gtable): the same table
#' in plain text, Markdown, LaTeX (booktabs-style rules without
#' requiring the package), or HTML.
#'
#' @param effect A \code{morie_mrm_effect} object (or any list with a
#'   compatible \code{results} data frame).
#' @param format One of \code{"text"}, \code{"markdown"},
#'   \code{"latex"}, \code{"html"}.
#' @param digits Significant digits (default 3).
#' @param caption Table caption.
#' @return Character vector of rendered lines (invisibly printed for
#'   \code{"text"}).
#' @examples
#' set.seed(1)
#' df <- data.frame(y = rnorm(300), t = rbinom(300, 1, 0.5),
#'                  x = rnorm(300))
#' eff <- morie_mrm_estimate_causal_effect(df, "t", "y", "x",
#'                                         methods = "ate")
#' cat(morie_mrm_report(eff, format = "markdown"), sep = "\n")
#' @export
morie_mrm_report <- function(effect,
                             format = c("text", "markdown", "latex",
                                        "html"),
                             digits = 3,
                             caption = "MRM causal-effect estimates") {
  format <- match.arg(format)
  res <- effect$results
  fm <- function(v) formatC(v, digits = digits, format = "g")
  stars <- ifelse(res$p_adjusted < 0.001, "***",
           ifelse(res$p_adjusted < 0.01, "**",
           ifelse(res$p_adjusted < 0.05, "*", "")))
  body <- data.frame(
    Method = res$method,
    Estimate = paste0(fm(res$estimate), stars),
    SE = fm(res$std_error),
    `95% CI` = sprintf("[%s, %s]", fm(res$ci_lower), fm(res$ci_upper)),
    `p (adj.)` = fm(res$p_adjusted),
    check.names = FALSE, stringsAsFactors = FALSE)
  foot <- sprintf(
    "Consensus (inverse-variance): %s (SE %s). %s correction; n = %d.",
    fm(effect$consensus$estimate), fm(effect$consensus$std_error),
    effect$correction, effect$spec$n)
  if (format == "text" || format == "markdown") {
    widths <- pmax(nchar(names(body)),
                   apply(body, 2L, function(cc) max(nchar(cc))))
    pad <- function(v, w) formatC(v, width = w, flag = "-")
    line <- function(cells) paste0(
      "| ", paste(mapply(pad, cells, widths), collapse = " | "), " |")
    sep <- paste0("|", paste(vapply(widths + 2L, function(w)
      paste(rep("-", w), collapse = ""), character(1)),
      collapse = "|"), "|")
    out <- c(if (format == "text") caption,
             line(names(body)), sep,
             vapply(seq_len(nrow(body)), function(i)
               line(unlist(body[i, ], use.names = FALSE)),
               character(1)),
             foot)
    return(out)
  }
  if (format == "latex") {
    esc <- function(s) gsub("%", "\\%", gsub("_", "\\_", s,
                                             fixed = TRUE),
                            fixed = TRUE)
    return(c(
      "\\begin{table}[htbp]", "\\centering",
      paste0("\\caption{", esc(caption), "}"),
      paste0("\\begin{tabular}{l", strrep("c", ncol(body) - 1L), "}"),
      "\\hline\\hline",
      paste0(paste(esc(names(body)), collapse = " & "), " \\\\"),
      "\\hline",
      vapply(seq_len(nrow(body)), function(i)
        paste0(paste(esc(unlist(body[i, ], use.names = FALSE)),
                     collapse = " & "), " \\\\"), character(1)),
      "\\hline\\hline",
      paste0("\\multicolumn{", ncol(body), "}{l}{\\footnotesize ",
             esc(foot), "} \\\\"),
      "\\end{tabular}", "\\end{table}"))
  }
  # html
  esc <- function(s) gsub("<", "&lt;", gsub("&", "&amp;", s,
                                            fixed = TRUE),
                          fixed = TRUE)
  c("<table class=\"morie-mrm\">",
    paste0("<caption>", esc(caption), "</caption>"),
    paste0("<thead><tr>",
           paste0("<th>", esc(names(body)), "</th>", collapse = ""),
           "</tr></thead>"),
    "<tbody>",
    vapply(seq_len(nrow(body)), function(i)
      paste0("<tr>", paste0("<td>",
                            esc(unlist(body[i, ], use.names = FALSE)),
                            "</td>", collapse = ""), "</tr>"),
      character(1)),
    "</tbody>",
    paste0("<tfoot><tr><td colspan=\"", ncol(body), "\">",
           esc(foot), "</td></tr></tfoot>"),
    "</table>")
}
