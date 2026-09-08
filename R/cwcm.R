# SPDX-License-Identifier: AGPL-3.0-or-later
#' Centering within cluster mean
#'
#' Enders and Tofighi (2007), Psychological Methods 12(2), 121-138:
#' x_ij(CWC) = x_ij - xbar_j.  The paper is paywalled; the transformation
#' is arithmetic and is quoted in its standard published form.  CWC
#' removes all between-cluster variance from the predictor, so the cluster
#' means are returned too -- reintroducing them as a level-2 predictor is
#' Enders and Tofighi's recommendation.
#'
#' @param y the covariate.
#' @param cluster cluster identifier per observation.
#' @return list: estimate, centered, cluster_means, cluster_ids,
#'   icc_between, n, method.
#' @keywords internal
#' @examples
#' Cwcenter(c(1, 2, 5, 6), c("a", "a", "b", "b"))$cluster_means
#' @export
Cwcenter <- function(y, cluster) {
  v <- .s03vec(y)
  g <- as.character(cluster)
  ids <- character(0)
  for (cc in g) if (!(cc %in% ids)) ids <- c(ids, cc)
  means <- numeric(length(ids))
  for (i in seq_along(ids)) means[i] <- .s03mean(v[g == ids[i]])
  cent <- numeric(length(v))
  for (i in seq_along(v)) cent[i] <- v[i] - means[match(g[i], ids)]
  gm <- .s03mean(v)
  ssb <- 0
  for (i in seq_along(ids)) {
    nj <- sum(g == ids[i])
    ssb <- ssb + nj * (means[i] - gm)^2
  }
  sst <- 0
  for (x in v) sst <- sst + (x - gm)^2
  list(estimate = cent, centered = cent, cluster_means = means,
       cluster_ids = ids,
       icc_between = if (sst > 0) ssb / sst else NaN, n = length(v),
       method = "Centering within cluster mean (CWC)")
}
