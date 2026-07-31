# SPDX-License-Identifier: AGPL-3.0-or-later
#
# eda_table.R -- an exploratory-data-analysis table system with an
# explicit index-column mechanism, meta-level auto-summaries, numeric-
# precision control, and accessibility-aware plotting, completing
# rmorie's coverage of the srr "EA" standards.

#' srr exploratory-data-analysis (EA) table standards
#'
#' These EA standards are completed by the morie_eda_table system and its
#' operations (this file), tested in test-srr-standards-EA-full.R.
#'
#' @srrstats {EA2.0} morie_eda_table() uses an explicit index-column
#'   system for tabular data used in joins/filters.
#' @srrstats {EA2.1} The index column is asserted to be unique as a
#'   pre-processing step.
#' @srrstats {EA2.2} Index columns are explicitly identified:
#' @srrstats {EA2.2a} via the `morie_eda_table` class, and
#' @srrstats {EA2.2b} via an `index` attribute recording the column name.
#' @srrstats {EA2.3} morie_eda_join() joins on an explicitly named column,
#'   never on assumed variable names.
#' @srrstats {EA2.4} morie_eda_dataset() provides an explicit multi-table
#'   class rather than relying on ad-hoc lists.
#' @srrstats {EA2.5} Each table in a dataset carries its own validated
#'   index column.
#' @srrstats {EA2.6} morie_eda_as_vector() processes vector data
#'   regardless of additional attributes (attributes are stripped).
#' @srrstats {EA3.0} morie_eda_summary() automatically extracts and
#'   reports per-variable statistics (a meta-level summary) without manual
#'   per-column intervention.
#' @srrstats {EA3.1} morie_eda_compare() provides a standardised
#'   comparison of the summaries of multiple inputs.
#' @srrstats {EA4.1} morie_eda_summary(digits=) gives explicit control of
#'   numeric precision.
#' @srrstats {EA5.0} morie_eda_plot() is accessibility-aware:
#' @srrstats {EA5.0a} it defaults to an enlarged typeface (cex), and
#' @srrstats {EA5.0b} a colourblind-safe default palette.
#' @srrstats {EA5.1} Typeface overrides go through the documented `cex`
#'   argument rather than silently overriding graphics defaults.
#' @srrstats {EA5.3} morie_eda_summary() reports the storage.mode / class
#'   of each column.
#' @srrstats {EA5.4} Summary + plot values are rounded sensibly (via the
#'   `digits` argument and pretty()).
#' @srrstats {EA5.5} morie_eda_plot() places supplied axis units on the
#'   axis labels.
#' @srrstats {EA5.6} rmorie bundles no dynamic-visualisation library;
#'   morie_eda_plot() uses base graphics only (tested).
#' @srrstats {EA6.1} morie_eda_plot() returns the drawn coordinates so
#'   graphical output properties can be tested without vdiffr.
#' @noRd
NULL

#' Construct an EDA table with an explicit index column
#'
#' @param data A data.frame.
#' @param index Name of the (unique) index column. If `NULL`, a
#'   `.row_id` index is created.
#' @return A `morie_eda_table` carrying an `index` attribute.
#' @examples
#' morie_eda_table(mtcars)
#' @export
morie_eda_table <- function(data, index = NULL) {
  data <- .morie_check_data(data, arg = "data")
  if (is.null(index)) {
    data[[".row_id"]] <- seq_len(nrow(data)); index <- ".row_id"
  }
  if (!index %in% names(data)) {
    stop(sprintf("index column '%s' not found", index), call. = FALSE)
  }
  if (anyDuplicated(data[[index]])) {                  # EA2.1
    stop(sprintf("index column '%s' has duplicate values", index),
         call. = FALSE)
  }
  attr(data, "index") <- index                         # EA2.2b
  class(data) <- c("morie_eda_table", "data.frame")    # EA2.2a
  data
}

#' The index column name of an EDA table
#' @param x A `morie_eda_table`.
#' @return The index column name.
#' @examples
#' morie_eda_index(morie_eda_table(mtcars))
#' @export
morie_eda_index <- function(x) attr(x, "index")

#' Join two EDA tables on an explicitly named column
#' @param x,y `morie_eda_table`s (or data.frames).
#' @param by The join column name (explicit; not inferred).
#' @return A merged `morie_eda_table`.
#' @examples
#' a <- morie_eda_table(data.frame(id = 1:3, x = 4:6), index = "id")
#' b <- data.frame(id = 1:3, y = 7:9)
#' morie_eda_join(a, b, by = "id")
#' @export
morie_eda_join <- function(x, y, by) {
  if (missing(by) || is.null(by)) {
    stop("`by` must be given explicitly; joins never assume column names",
         call. = FALSE)
  }
  m <- merge(as.data.frame(x), as.data.frame(y), by = by)
  morie_eda_table(m, index = if (!anyDuplicated(m[[by]])) by else NULL)
}

#' Bundle several EDA tables into an explicit multi-table dataset
#' @param ... Named `morie_eda_table`s or data.frames.
#' @return A `morie_eda_dataset` (each element validated as an EDA table).
#' @examples
#' morie_eda_dataset(a = mtcars, b = iris)
#' @export
morie_eda_dataset <- function(...) {
  tabs <- list(...)
  tabs <- lapply(tabs, function(t)
    if (inherits(t, "morie_eda_table")) t else morie_eda_table(t))
  class(tabs) <- c("morie_eda_dataset", "list")
  tabs
}

#' Strip attributes from a vector for attribute-agnostic processing
#' @param x A vector.
#' @return The vector's underlying values with attributes removed.
#' @examples
#' morie_eda_as_vector(structure(1:3, foo = "bar"))
#' @export
morie_eda_as_vector <- function(x) {                   # EA2.6
  as.vector(unclass(x))
}

#' Automatic per-variable summary of a table (meta-level extraction)
#'
#' @param x A data.frame / `morie_eda_table`.
#' @param digits Numeric precision for the reported statistics (EA4.1).
#' @return A data.frame with one row per variable giving its class /
#'   storage mode, missing count, and (for numeric variables) rounded
#'   summary statistics.
#' @examples
#' morie_eda_summary(mtcars, digits = 2)
#' @export
morie_eda_summary <- function(x, digits = 3L) {
  x <- as.data.frame(x)
  do.call(rbind, lapply(names(x), function(nm) {
    col <- x[[nm]]
    num <- is.numeric(col)
    data.frame(
      variable = nm,
      class = class(col)[1],
      storage_mode = storage.mode(col),               # EA5.3
      n_missing = sum(is.na(col)),
      mean = if (num) round(mean(col, na.rm = TRUE), digits) else NA_real_,
      sd   = if (num) round(stats::sd(col, na.rm = TRUE), digits) else NA_real_,
      min  = if (num) round(min(col, na.rm = TRUE), digits) else NA_real_,
      max  = if (num) round(max(col, na.rm = TRUE), digits) else NA_real_,
      stringsAsFactors = FALSE, row.names = NULL)
  }))
}

#' Standardised comparison of several tables' summaries
#' @param ... Named data.frames / `morie_eda_table`s.
#' @param digits Numeric precision.
#' @return A data.frame binding each input's summary with a `source`
#'   column, enabling a like-for-like comparison.
#' @examples
#' morie_eda_compare(a = mtcars[1:16, ], b = mtcars[17:32, ])
#' @export
morie_eda_compare <- function(..., digits = 3L) {
  inputs <- list(...)
  nms <- names(inputs); if (is.null(nms)) nms <- seq_along(inputs)
  do.call(rbind, Map(function(t, nm) {
    s <- morie_eda_summary(t, digits = digits); cbind(source = nm, s)
  }, inputs, nms))
}

# colourblind-safe palette (Okabe-Ito) for EA5.0b
#' Internal helper: Eda Palette
#' @noRd
.eda_palette <- c("#0072B2", "#D55E00", "#009E73", "#CC79A7",
                  "#F0E442", "#56B4E9", "#E69F00", "#000000")

#' Accessible exploratory plot of one variable against another
#'
#' Uses an enlarged default typeface and a colourblind-safe palette,
#' places supplied units on the axes, and rounds axis breaks sensibly.
#' Returns the drawn coordinates so the graphical output can be tested.
#'
#' @param x A data.frame / `morie_eda_table`.
#' @param xvar,yvar Column names to plot.
#' @param group Optional grouping column (mapped to the palette).
#' @param units Optional named character vector of axis units, e.g.
#'   `c(xvar = "kg", yvar = "cm")`.
#' @param cex Typeface / point expansion (default 1.3, enlarged for
#'   accessibility).
#' @param digits Rounding for axis breaks.
#' @return Invisibly, a data.frame of the plotted `x`/`y`/`colour`.
#' @examples
#' morie_eda_plot(mtcars, "hp", "mpg")
#' @export
morie_eda_plot <- function(x, xvar, yvar, group = NULL, units = NULL,
                           cex = 1.3, digits = 2L) {
  x <- as.data.frame(x)
  xlab <- if (!is.null(units) && xvar %in% names(units))
    sprintf("%s (%s)", xvar, units[[xvar]]) else xvar        # EA5.5
  ylab <- if (!is.null(units) && yvar %in% names(units))
    sprintf("%s (%s)", yvar, units[[yvar]]) else yvar
  col_idx <- if (!is.null(group)) as.integer(factor(x[[group]])) else 1L
  cols <- .eda_palette[((col_idx - 1L) %% length(.eda_palette)) + 1L]
  plot(x[[xvar]], x[[yvar]], xlab = xlab, ylab = ylab,
       cex = cex, cex.lab = cex, cex.axis = cex, pch = 19, col = cols,
       xaxp = c(range(pretty(x[[xvar]])), 5))                # EA5.4 pretty
  invisible(data.frame(x = x[[xvar]], y = x[[yvar]], colour = cols,
                       stringsAsFactors = FALSE))
}

#' Print method for \code{morie_eda_table} objects
#'
#' @param x A `morie_eda_table`.
#' @param ... Passed to the data.frame print method.
#' @return `x`, invisibly.
#' @examples
#' \donttest{
#' obj <- morie_eda_table(mtcars)
#' print(obj)
#' }
#' @export
print.morie_eda_table <- function(x, ...) {
  cat(sprintf("<morie_eda_table> %d x %d  index: %s\n",
              nrow(x), ncol(x), attr(x, "index")))
  print(utils::head(as.data.frame(x)), ...)
  invisible(x)
}
