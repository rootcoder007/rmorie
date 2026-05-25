# SPDX-License-Identifier: AGPL-3.0-or-later
#' Cross-category crime analyses for Toronto Police Service (TPS) datasets
#'
#' R-side port of \code{morie.tps_crime}.  Where the Python module
#' uses \code{TPS_REGISTRY} + \code{load_tps_dataset} to materialise
#' per-category data.frames, the R-side callables here accept a named
#' list of pre-loaded data.frames (one entry per TPS category) via the
#' \code{dfs} argument.  Callers are responsible for loading the CSVs
#' (e.g. via \code{utils::read.csv} or \code{readr::read_csv}) and
#' passing them in keyed by canonical TPS category name (\emph{e.g.}
#' "Assault", "Homicides", "BicycleTheft").
#'
#' Callables:
#' \itemize{
#'   \item \code{morie_tps_yoy_panel()}: side-by-side year-over-year
#'         panel across TPS categories.
#'   \item \code{morie_tps_composite_index()}: per-neighbourhood
#'         composite crime-risk index (sum of z-standardised counts,
#'         optionally weighted).
#'   \item \code{morie_tps_bivariate_morans_i()}: bivariate Moran's I
#'         between two TPS categories on a shared HOOD_158 footprint
#'         using a k-NN row-standardised spatial weights matrix.
#'   \item \code{morie_tps_category_correlation_matrix()}: Pearson r
#'         on per-hood incident counts across all supplied categories.
#' }
#'
#' Each callable returns a named list with class
#' \code{c("morie_tps_result", "morie_rich_result", "list")} carrying
#' \code{title}, \code{summary_lines}, \code{tables} (when applicable),
#' \code{interpretation}, \code{warnings}, and a free-form
#' \code{payload}.
#'
#' @name tps_crime
NULL


# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

#' Counts per neighbourhood, dropping 'NSA' / unknowns.
#' @keywords internal
#' @noRd
.tps_hood_counts <- function(df, col = "HOOD_158") {
  if (!is.data.frame(df) || !(col %in% names(df))) {
    return(integer(0))
  }
  s <- df[[col]]
  s <- s[!is.na(s)]
  s <- s[toupper(as.character(s)) != "NSA"]
  if (length(s) == 0L) {
    return(integer(0))
  }
  tab <- table(as.character(s))
  out <- as.integer(tab)
  names(out) <- names(tab)
  out
}

#' Build a rich-result named list with the morie_tps_result class.
#' @keywords internal
#' @noRd
.tps_result <- function(title, summary_lines = list(), tables = list(),
                          interpretation = "", warnings = character(0),
                          payload = list(), call = NULL,
                          sections = list(), ...) {
  out <- list(
    title = title,
    call = call,
    summary_lines = summary_lines,
    tables = tables,
    warnings = warnings,
    interpretation = interpretation,
    payload = payload,
    sections = sections,
    ...
  )
  class(out) <- c("morie_tps_result", "morie_rich_result", "list")
  out
}

#' Detect the year column (OCC_YEAR or REPORT_YEAR).
#' @keywords internal
#' @noRd
.tps_year_col <- function(df) {
  if ("OCC_YEAR" %in% names(df)) {
    return("OCC_YEAR")
  }
  if ("REPORT_YEAR" %in% names(df)) {
    return("REPORT_YEAR")
  }
  NA_character_
}


# ---------------------------------------------------------------------------
# 1. Year-over-year panel
# ---------------------------------------------------------------------------

#' Side-by-side year-over-year panel across TPS categories
#'
#' For each named TPS category in \code{dfs}, groups by the dataset's
#' year column (\code{OCC_YEAR} preferred, \code{REPORT_YEAR}
#' fallback), restricts to plausible years (1990-2030), and joins all
#' series column-wise into a single panel of incident counts.
#'
#' @param dfs Named list of TPS data.frames keyed by category name.
#' @param categories Optional character vector restricting which keys
#'   of \code{dfs} are analysed; defaults to all of them.
#' @return A \code{morie_tps_result} named list with a single
#'   year-by-category table.
#' @export
morie_tps_yoy_panel <- function(dfs, categories = NULL) {
  stopifnot(is.list(dfs))
  cats <- categories %||% sort(names(dfs))
  series <- list()
  for (cat in cats) {
    df <- dfs[[cat]]
    if (!is.data.frame(df)) {
      next
    }
    yc <- .tps_year_col(df)
    if (is.na(yc)) {
      next
    }
    y <- suppressWarnings(as.integer(df[[yc]]))
    y <- y[is.finite(y) & y >= 1990 & y <= 2030]
    if (length(y) == 0L) {
      next
    }
    tab <- table(y)
    s <- as.integer(tab)
    names(s) <- names(tab)
    series[[cat]] <- s
  }
  if (length(series) == 0L) {
    return(.tps_result(
      title = "TPS YoY panel -- cross-category",
      warnings = "no datasets loaded with year column",
      call = "morie_tps_yoy_panel(dfs)"
    ))
  }
  all_years <- sort(unique(as.integer(unlist(lapply(series, names)))))
  panel <- matrix(0L,
    nrow = length(all_years), ncol = length(series),
    dimnames = list(as.character(all_years), names(series))
  )
  for (cat in names(series)) {
    s <- series[[cat]]
    panel[names(s), cat] <- s
  }
  rows <- lapply(seq_len(nrow(panel)), function(i) {
    c(as.integer(all_years[i]), as.integer(panel[i, ]))
  })
  .tps_result(
    title = "TPS -- year-over-year panel (cross-category)",
    summary_lines = list(
      Categories = length(series),
      Years = sprintf("%d-%d", min(all_years), max(all_years)),
      `Total category-years` = as.integer(nrow(panel) * ncol(panel))
    ),
    tables = list(list(
      title = "Incidents per OCC_YEAR (rows) x Category (cols):",
      headers = c("OCC_YEAR", colnames(panel)),
      rows = rows
    )),
    interpretation = paste(
      "One column per TPS category, one row per fiscal year, cell = annual",
      "incident count.  Suitable for side-by-side trend plotting and",
      "year-on-year delta inspection."
    ),
    payload = list(panel = panel, years = all_years,
                   categories = names(series)),
    call = "morie_tps_yoy_panel(dfs)"
  )
}


# ---------------------------------------------------------------------------
# 2. Composite per-neighbourhood crime-risk index
# ---------------------------------------------------------------------------

#' Composite per-neighbourhood crime-risk index across TPS categories
#'
#' For each TPS category, computes per-HOOD_158 counts, z-standardises
#' across neighbourhoods, and sums (or weight-and-sums) the z-scores to
#' yield a single composite per neighbourhood.  Positive composite =
#' neighbourhood with elevated incidence across many crime types;
#' near-zero = average; negative = below-average exposure.
#'
#' @param dfs Named list of TPS data.frames keyed by category.
#' @param categories Optional character vector restricting categories.
#' @param weights Optional named numeric vector of per-category weights;
#'   defaults to 1.0 for every loaded category.
#' @param top_n How many top/bottom neighbourhoods to surface in the
#'   tables (default \code{25L}).
#' @return A \code{morie_tps_result} named list.
#' @export
morie_tps_composite_index <- function(dfs, categories = NULL,
                                        weights = NULL, top_n = 25L) {
  stopifnot(is.list(dfs))
  cats <- categories %||% sort(names(dfs))
  used <- character(0)
  z_panels <- list()
  for (cat in cats) {
    df <- dfs[[cat]]
    if (!is.data.frame(df)) {
      next
    }
    counts <- .tps_hood_counts(df)
    if (length(counts) < 5L) {
      next
    }
    mu <- mean(counts)
    sd <- stats::sd(counts) %||% 1.0
    if (!is.finite(sd) || sd == 0) {
      sd <- 1.0
    }
    z <- (counts - mu) / sd
    z_panels[[cat]] <- z
    used <- c(used, cat)
  }
  if (length(z_panels) == 0L) {
    return(.tps_result(
      title = "TPS composite index",
      warnings = "no usable categories",
      call = "morie_tps_composite_index(dfs)"
    ))
  }
  all_hoods <- sort(unique(unlist(lapply(z_panels, names))))
  panel <- matrix(0,
    nrow = length(all_hoods), ncol = length(z_panels),
    dimnames = list(all_hoods, names(z_panels))
  )
  for (cat in names(z_panels)) {
    z <- z_panels[[cat]]
    panel[names(z), cat] <- z
  }
  if (is.null(weights)) {
    weight_vec <- rep(1.0, ncol(panel))
    names(weight_vec) <- colnames(panel)
  } else {
    weight_vec <- vapply(colnames(panel),
                          function(c) as.numeric(weights[[c]] %||% 0.0),
                          numeric(1))
  }
  composite <- as.numeric(panel %*% weight_vec)
  out <- data.frame(
    hood = rownames(panel),
    composite = composite,
    stringsAsFactors = FALSE
  )
  out <- out[order(-out$composite), , drop = FALSE]

  top_n <- as.integer(top_n)
  top_rows <- lapply(seq_len(min(top_n, nrow(out))), function(i) {
    list(as.character(out$hood[i]), round(out$composite[i], 3))
  })
  bot_rows <- lapply(seq.int(nrow(out), max(1L, nrow(out) - top_n + 1L)),
                      function(i) {
                        list(as.character(out$hood[i]),
                             round(out$composite[i], 3))
                      })

  .tps_result(
    title = "TPS -- composite crime-risk index per neighbourhood",
    summary_lines = list(
      `Categories used` = length(used),
      `Neighbourhoods scored` = as.integer(nrow(panel)),
      `Mean weight` = round(mean(weight_vec), 3),
      `Mean composite` = round(mean(composite), 3),
      `Max composite` = round(max(composite), 3)
    ),
    tables = list(
      list(
        title = sprintf("Top %d neighbourhoods (highest composite):", top_n),
        headers = c("HOOD_158", "Composite z-sum"),
        rows = top_rows
      ),
      list(
        title = sprintf("Bottom %d neighbourhoods (lowest composite):",
                         top_n),
        headers = c("HOOD_158", "Composite z-sum"),
        rows = bot_rows
      )
    ),
    interpretation = paste(
      "Composite is the unweighted (or weighted) sum of z-standardised",
      "counts across all loaded TPS categories.  Positive =",
      "neighbourhood with elevated incidence across the included crime",
      "types; near-zero = average; negative = below-average exposure",
      "across categories."
    ),
    payload = list(used = used, ranking = utils::head(out, 50L),
                   panel = panel, weights = weight_vec),
    call = "morie_tps_composite_index(dfs)"
  )
}


# ---------------------------------------------------------------------------
# 3. Bivariate Moran's I (cat_a, cat_b)
# ---------------------------------------------------------------------------

#' Bivariate Moran's I between two TPS categories
#'
#' Tests whether category A's per-neighbourhood count co-varies with
#' category B's count in NEIGHBOURING neighbourhoods (spatial
#' spillover).  Builds a k-NN row-standardised spatial weights matrix
#' from per-hood centroids derived from category A's WGS84
#' latitude/longitude.  Reports Pearson r alongside as a non-spatial
#' baseline.
#'
#' @param dfs Named list of TPS data.frames keyed by category.
#' @param cat_a Name of category A in \code{dfs}.
#' @param cat_b Name of category B in \code{dfs}.
#' @param k_neighbours Number of nearest neighbours per row in W
#'   (default \code{5L}).
#' @return A \code{morie_tps_result} named list.
#' @export
morie_tps_bivariate_morans_i <- function(dfs, cat_a, cat_b,
                                            k_neighbours = 5L) {
  stopifnot(is.list(dfs))
  df_a <- dfs[[cat_a]]
  df_b <- dfs[[cat_b]]
  if (!is.data.frame(df_a) || !is.data.frame(df_b)) {
    stop(sprintf("Both %s and %s must be data.frames in dfs.",
                 sQuote(cat_a), sQuote(cat_b)),
         call. = FALSE)
  }
  a <- .tps_hood_counts(df_a)
  b <- .tps_hood_counts(df_b)
  common <- intersect(names(a), names(b))
  ttl <- sprintf("Bivariate Moran's I -- %s vs %s", cat_a, cat_b)
  if (length(common) < 5L) {
    return(.tps_result(
      title = ttl,
      warnings = sprintf("only %d common hoods", length(common)),
      call = sprintf("morie_tps_bivariate_morans_i(dfs, %s, %s)",
                     sQuote(cat_a), sQuote(cat_b))
    ))
  }
  a <- as.numeric(a[common])
  b <- as.numeric(b[common])
  names(a) <- common
  names(b) <- common

  need_cols <- c("HOOD_158", "LAT_WGS84", "LONG_WGS84")
  if (!all(need_cols %in% names(df_a))) {
    return(.tps_result(
      title = ttl,
      warnings = paste("category A is missing one of",
                       paste(need_cols, collapse = ", ")),
      call = sprintf("morie_tps_bivariate_morans_i(dfs, %s, %s)",
                     sQuote(cat_a), sQuote(cat_b))
    ))
  }
  sub <- df_a[, need_cols, drop = FALSE]
  sub <- sub[stats::complete.cases(sub), , drop = FALSE]
  sub$HOOD_158 <- as.character(sub$HOOD_158)
  sub <- sub[sub$HOOD_158 %in% common, , drop = FALSE]
  if (nrow(sub) == 0L) {
    return(.tps_result(
      title = ttl,
      warnings = "no usable centroids for common hoods",
      call = sprintf("morie_tps_bivariate_morans_i(dfs, %s, %s)",
                     sQuote(cat_a), sQuote(cat_b))
    ))
  }
  cents <- stats::aggregate(
    cbind(LAT_WGS84, LONG_WGS84) ~ HOOD_158, data = sub, FUN = mean
  )
  cents <- cents[cents$HOOD_158 %in% common, , drop = FALSE]
  rownames(cents) <- cents$HOOD_158
  coords <- as.matrix(cents[, c("LAT_WGS84", "LONG_WGS84")])
  n <- nrow(coords)
  if (n < 5L) {
    return(.tps_result(
      title = ttl,
      warnings = sprintf("only %d hoods with valid centroids", n),
      call = sprintf("morie_tps_bivariate_morans_i(dfs, %s, %s)",
                     sQuote(cat_a), sQuote(cat_b))
    ))
  }

  # Pairwise Euclidean distance (lat/long degrees; close enough for
  # rank-based k-NN within Toronto).
  dist <- as.matrix(stats::dist(coords))
  diag(dist) <- Inf
  k <- min(as.integer(k_neighbours), n - 1L)
  W <- matrix(0, n, n)
  for (i in seq_len(n)) {
    nb <- order(dist[i, ])[seq_len(k)]
    W[i, nb] <- 1
  }
  rsum <- rowSums(W)
  rsum[rsum == 0] <- 1
  W <- W / rsum

  a_v <- a[rownames(coords)]
  b_v <- b[rownames(coords)]
  sd_a <- stats::sd(a_v) * sqrt((n - 1) / n)
  sd_b <- stats::sd(b_v) * sqrt((n - 1) / n)
  if (!is.finite(sd_a) || sd_a == 0) sd_a <- 1e-300
  if (!is.finite(sd_b) || sd_b == 0) sd_b <- 1e-300
  z_a <- (a_v - mean(a_v)) / sd_a
  z_b <- (b_v - mean(b_v)) / sd_b
  Wz_b <- as.numeric(W %*% z_b)
  I_ab <- mean(z_a * Wz_b)
  r <- as.numeric(stats::cor(a_v, b_v))

  .tps_result(
    title = ttl,
    summary_lines = list(
      `Hoods compared` = as.integer(n),
      `Bivariate Moran's I_AB` = round(I_ab, 4),
      `Pearson r (non-spatial)` = round(r, 4),
      `k-nearest neighbours` = k,
      `z-A range` = sprintf("%.2f ... %.2f", min(z_a), max(z_a)),
      `z-B range` = sprintf("%.2f ... %.2f", min(z_b), max(z_b))
    ),
    interpretation = sprintf(paste(
      "I_AB=%+.3f, Pearson r=%+.3f.  Positive I_AB means %s counts in a",
      "hood track %s counts in NEIGHBOURING hoods (spatial spillover).",
      "Compare to Pearson r to see if the association is purely",
      "co-located vs spatially-lagged."
    ), I_ab, r, cat_a, cat_b),
    payload = list(I_ab = I_ab, pearson_r = r, n_hoods = n,
                   k = k, hoods = rownames(coords),
                   z_a = z_a, z_b = z_b),
    call = sprintf("morie_tps_bivariate_morans_i(dfs, %s, %s)",
                   sQuote(cat_a), sQuote(cat_b))
  )
}


# ---------------------------------------------------------------------------
# 4. Cross-category correlation matrix
# ---------------------------------------------------------------------------

#' Pearson correlation across TPS categories' per-hood counts
#'
#' For every category in \code{dfs}, computes per-HOOD_158 counts,
#' aligns onto a common (union) hood index, and reports the Pearson
#' correlation matrix.
#'
#' @param dfs Named list of TPS data.frames keyed by category.
#' @return A \code{morie_tps_result} named list with a single
#'   correlation table.
#' @export
morie_tps_category_correlation_matrix <- function(dfs) {
  stopifnot(is.list(dfs))
  cats <- sort(names(dfs))
  series <- list()
  for (c in cats) {
    df <- dfs[[c]]
    if (!is.data.frame(df)) {
      next
    }
    series[[c]] <- .tps_hood_counts(df)
  }
  if (length(series) == 0L) {
    return(.tps_result(
      title = "TPS correlation matrix",
      warnings = "no data",
      call = "morie_tps_category_correlation_matrix(dfs)"
    ))
  }
  all_hoods <- sort(unique(unlist(lapply(series, names))))
  panel <- matrix(0,
    nrow = length(all_hoods), ncol = length(series),
    dimnames = list(all_hoods, names(series))
  )
  for (c in names(series)) {
    s <- series[[c]]
    panel[names(s), c] <- s
  }
  corr <- round(stats::cor(panel), 3)
  rows <- lapply(rownames(corr), function(rn) {
    c(list(rn), lapply(corr[rn, ], as.numeric))
  })
  .tps_result(
    title = "TPS -- per-hood correlation across categories",
    summary_lines = list(
      Categories = length(series),
      `Hoods (union)` = as.integer(nrow(panel))
    ),
    tables = list(list(
      title = "Pearson r between per-hood counts (categories x categories):",
      headers = c("", colnames(corr)),
      rows = rows
    )),
    interpretation = paste(
      "Pearson correlation of per-HOOD_158 incident counts between every",
      "pair of supplied TPS categories.  Values near 1.0 indicate",
      "neighbourhoods that score similarly on both categories;",
      "values near 0 mean the categories pick out different parts of",
      "the city; negative values mean the categories anti-correlate."
    ),
    payload = list(corr = corr, panel = panel),
    call = "morie_tps_category_correlation_matrix(dfs)"
  )
}


# ---------------------------------------------------------------------------
# print method (shared with tps_csi.R via the morie_tps_result class)
# ---------------------------------------------------------------------------

#' Pretty-print method for \code{morie_tps_result} objects.
#'
#' @param x A \code{morie_tps_result} list.
#' @param ... Ignored.
#' @export
print.morie_tps_result <- function(x, ...) {
  cat(x$title, "\
", strrep("=", nchar(x$title)), "\
", sep = "")
  if (!is.null(x$call) && nzchar(x$call)) {
    cat("Call:", x$call, "\
\
", sep = " ")
  }
  if (length(x$summary_lines) > 0L) {
    nms <- names(x$summary_lines)
    label_w <- max(nchar(nms))
    for (i in seq_along(x$summary_lines)) {
      v <- x$summary_lines[[i]]
      if (is.numeric(v) && length(v) == 1L && is.finite(v)) {
        v <- format(v, digits = 5)
      }
      cat(sprintf("  %-*s  %s\
", label_w, nms[i], format(v)))
    }
    cat("\
")
  }
  if (length(x$tables) > 0L) {
    for (tbl in x$tables) {
      cat(tbl$title, "\
", sep = "")
      cat("  ", paste(tbl$headers, collapse = "  "), "\
", sep = "")
      for (row in tbl$rows) {
        cat("  ",
            paste(vapply(row, function(z) format(z), character(1)),
                  collapse = "  "),
            "\
", sep = "")
      }
      cat("\
")
    }
  }
  if (length(x$warnings) > 0L) {
    for (w in x$warnings) cat("Warning:", w, "\
")
    cat("\
")
  }
  if (length(x$interpretation) > 0L && nzchar(x$interpretation)) {
    cat(x$interpretation, "\
")
  }
  invisible(x)
}


# %||% (NULL-coalescing) -- defined locally if arsau.R hasn't been
# sourced yet; safe redeclaration.
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}
