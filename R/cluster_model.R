# SPDX-License-Identifier: AGPL-3.0-or-later
#
# cluster_model.R -- a k-means clustering model object completing
# rmorie's coverage of the srr "UL" (unsupervised) standards: label
# ordering by group size, row-name propagation, prediction of new data
# without re-fitting, a no-fit mode, print/summary/plot methods, missing-
# value handling, and batch processing.

#' srr unsupervised (UL) clustering-object standards
#'
#' These UL standards are completed by the morie_cluster object and its
#' methods (this file), tested in test-srr-standards-UL-full.R.
#'
#' @srrstats {UL1.2} morie_cluster() warns when input lacks non-default
#'   row names (used to label output).
#' @srrstats {UL1.3} Row names of the input are propagated to the cluster
#'   assignments in the output.
#' @srrstats {UL1.4b} The `scale` argument is documented with the
#'   consequence of applying / not applying it (examples contrast both).
#' @srrstats {UL2.0} morie_cluster() diagnoses markedly different
#'   predictor scales and can optionally rescale (scale=), warning when
#'   scales differ by orders of magnitude.
#' @srrstats {UL2.2} morie_cluster(na_action=) explicitly controls
#'   missing-value handling (omit / fail).
#' @srrstats {UL3.0} Cluster labels are assigned in decreasing group-size
#'   order (label 1 is the largest cluster).
#' @srrstats {UL3.2} A `case_labels` argument labels cases when the input
#'   has no row names.
#' @srrstats {UL3.3} predict.morie_cluster() assigns new data to existing
#'   clusters without re-running the algorithm.
#' @srrstats {UL4.1} morie_cluster(nofit=TRUE) returns an unfitted spec.
#' @srrstats {UL4.3a} print.morie_cluster() restricts the number of
#'   assignment rows shown.
#' @srrstats {UL4.4} summary.morie_cluster() summarises cluster sizes and
#'   within-cluster dispersion.
#' @srrstats {UL6.0} plot.morie_cluster() is the default plot method.
#' @srrstats {UL6.1} plot dispatch on the morie_cluster class exists.
#' @srrstats {UL6.2} The plot places at most a readable number of centroid
#'   labels, warning if too many clusters to label.
#' @srrstats {UL7.1} A test shows that clustering scaleless/degenerate
#'   input yields a trivial (single-cluster) result.
#' @srrstats {UL7.2} A test confirms labels follow decreasing group sizes.
#' @srrstats {UL7.3} A test confirms input row names are recovered from
#'   the output.
#' @srrstats {UL7.4} A test confirms predicting new data is faster than a
#'   full re-fit.
#' @srrstats {UL7.5} morie_cluster_batch() batch-processes several inputs
#'   and is tested.
#' @srrstats {UL7.5a} A test confirms batch results equal per-item fits.
#' @noRd
NULL

#' k-means clustering with a full model-object contract
#'
#' @param x A numeric matrix or data.frame (numeric columns used).
#' @param k Number of clusters.
#' @param scale Standardise columns before clustering. Applying `scale`
#'   removes the dominance of large-variance columns; not applying it lets
#'   them dominate the distance metric -- the examples contrast both.
#' @param na_action "omit" (drop incomplete rows) or "fail" (error).
#' @param case_labels Optional labels for cases when `x` has no row names.
#' @param nofit If TRUE, return an unfitted specification.
#' @param seed RNG seed.
#' @return A `morie_cluster` object (or `morie_cluster_spec` if
#'   `nofit = TRUE`) whose cluster labels are ordered by decreasing size.
#' @examples
#' # with vs without scaling changes which columns drive the clusters
#' morie_cluster(iris[1:4], k = 3)
#' morie_cluster(iris[1:4], k = 3, scale = TRUE)
#' @export
morie_cluster <- function(x, k = 2L, scale = FALSE,
                          na_action = c("omit", "fail"),
                          case_labels = NULL, nofit = FALSE, seed = 42L) {
  na_action <- match.arg(na_action)
  if (isTRUE(nofit)) {
    spec <- list(k = k, scale = scale)
    class(spec) <- c("morie_cluster_spec", "morie_rich_result", "list")
    return(spec)
  }
  xm <- as.matrix(x[vapply(as.data.frame(x), is.numeric, logical(1))])
  storage.mode(xm) <- "double"
  rn <- rownames(xm)
  if (is.null(rn)) {
    if (!is.null(case_labels)) rn <- as.character(case_labels)     # UL3.2
    else {
      warning("input has no row names; using positional labels", call. = FALSE)
      rn <- as.character(seq_len(nrow(xm)))                        # UL1.2
    }
  }
  if (anyNA(xm)) {
    if (na_action == "fail") stop("missing values with na_action='fail'",
                                  call. = FALSE)
    keep <- stats::complete.cases(xm); xm <- xm[keep, , drop = FALSE]
    rn <- rn[keep]
  }
  # scale diagnostics (UL2.0)
  col_sds <- apply(xm, 2, stats::sd)
  if (!scale && length(col_sds) > 1 &&
      max(col_sds) / min(col_sds[col_sds > 0]) > 100) {
    warning("predictor scales differ by >100x; consider scale = TRUE",
            call. = FALSE)
  }
  centers_scale <- NULL
  if (scale) { centers_scale <- list(m = colMeans(xm), s = col_sds)
    xm <- scale(xm) }

  set.seed(seed)
  km <- stats::kmeans(xm, centers = k, nstart = 10L)

  # relabel clusters by decreasing size (UL3.0)
  ord <- order(-tabulate(km$cluster, nbins = k))
  remap <- match(seq_len(k), ord)
  labels <- remap[km$cluster]
  centers <- km$centers[ord, , drop = FALSE]
  rownames(centers) <- seq_len(k)

  out <- list(
    assignments = stats::setNames(labels, rn), centers = centers,
    k = k, sizes = tabulate(labels, nbins = k),
    withinss = km$withinss[ord], tot_withinss = km$tot.withinss,
    scale = scale, scale_params = centers_scale,
    case_names = rn, feature_names = colnames(xm), n_obs = nrow(xm))
  class(out) <- c("morie_cluster", "morie_rich_result", "list")
  out
}

#' Assign new data to existing clusters without re-fitting
#' @param object A `morie_cluster`.
#' @param newdata New numeric data with the training columns.
#' @param ... Unused.
#' @return Integer cluster labels for the new rows (named by row name).
#' @examples
#' cl <- morie_cluster(iris[1:4], k = 3)
#' predict(cl, iris[1:5, 1:4])
#' @export
predict.morie_cluster <- function(object, newdata, ...) {
  xm <- as.matrix(newdata[, object$feature_names, drop = FALSE])
  storage.mode(xm) <- "double"
  if (object$scale) {
    xm <- sweep(sweep(xm, 2, object$scale_params$m), 2,
                object$scale_params$s, "/")
  }
  # nearest centroid (no re-optimisation)
  d <- as.matrix(stats::dist(rbind(object$centers, xm)))
  d <- d[(object$k + 1):nrow(d), seq_len(object$k), drop = FALSE]
  lab <- apply(d, 1, which.min)
  stats::setNames(lab, rownames(newdata))
}

#' @param x A `morie_cluster`.
#' @param max_rows Maximum assignment rows to print.
#' @param ... Unused.
#' @return `x`, invisibly.
#' @export
print.morie_cluster <- function(x, max_rows = 10L, ...) {
  cat(sprintf("<morie_cluster> k=%d  n=%d\n", x$k, x$n_obs))
  cat("  sizes (largest first):", paste(x$sizes, collapse = ", "), "\n")
  cat("  assignments (first rows):\n")
  print(utils::head(x$assignments, max_rows))          # UL4.3a
  if (x$n_obs > max_rows) cat(sprintf("  ... %d more\n", x$n_obs - max_rows))
  invisible(x)
}

#' @param object A `morie_cluster`.
#' @param ... Unused.
#' @return A data.frame with per-cluster size and within-cluster SS.
#' @export
summary.morie_cluster <- function(object, ...) {
  data.frame(cluster = seq_len(object$k), size = object$sizes,
             withinss = object$withinss)
}

#' Default 2-D cluster plot
#' @param x A `morie_cluster`.
#' @param ... Passed to [plot()].
#' @return `NULL`, invisibly.
#' @export
plot.morie_cluster <- function(x, ...) {
  ctr <- x$centers
  if (ncol(ctr) < 2) { plot(ctr[, 1], rep(0, nrow(ctr)), xlab = "dim 1",
                            ylab = "", ...); return(invisible(NULL)) }
  plot(ctr[, 1], ctr[, 2], xlab = x$feature_names[1], ylab = x$feature_names[2],
       pch = 19, main = "morie_cluster centroids", ...)
  if (x$k <= 20L) graphics::text(ctr[, 1], ctr[, 2], labels = seq_len(x$k),
                                 pos = 3)
  else warning("too many clusters to label readably", call. = FALSE)  # UL6.2
  invisible(NULL)
}

#' @param x A `morie_cluster_spec`.
#' @param ... Unused.
#' @return `x`, invisibly.
#' @export
print.morie_cluster_spec <- function(x, ...) {
  cat(sprintf("<morie_cluster_spec> (unfitted) k=%d scale=%s\n", x$k, x$scale))
  invisible(x)
}

#' Batch-cluster several data sets
#' @param datasets A named list of data inputs.
#' @param k Number of clusters (applied to each).
#' @param ... Passed to [morie_cluster()].
#' @return A named list of `morie_cluster` objects.
#' @examples
#' morie_cluster_batch(list(a = iris[1:4], b = iris[1:4]), k = 3)
#' @export
morie_cluster_batch <- function(datasets, k = 2L, ...) {
  stopifnot(is.list(datasets))
  lapply(datasets, function(d) morie_cluster(d, k = k, ...))
}
