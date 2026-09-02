# SPDX-License-Identifier: AGPL-3.0-or-later
#' Item nonresponse adjustment by weighting classes
#'
#' Units are partitioned into classes on observed variables; within a
#' class the responding units carry the adjustment factor (sum of base
#' weights in the class) / (sum of base weights of respondents in the
#' class), the inverse of the estimated response probability under the
#' assumption that response is independent of the item value within a
#' class.  The adjusted estimate equals the class-size-weighted average
#' of the class respondent means.
#'
#' Formula: f_c = W_c / W_c(respondents).
#'
#' @param y Item values; entries with R = 0 are unused.
#' @param R Response indicator, 0 or 1.
#' @param X Class-defining variables, one row per unit; rows with equal
#'   values form a class.  \code{NULL} puts everyone in one class.
#' @param weights Base design weights; \code{NULL} means all ones.
#' @return List with \code{estimate}, \code{se}, \code{n_classes},
#'   \code{response_rate}, \code{min_class_rate}, \code{max_class_rate},
#'   \code{adjusted_total}, \code{n_respondents}, \code{n},
#'   \code{method}.
#' @references Kalton and Flores-Cervantes (2003), Weighting methods,
#'   Journal of Official Statistics 19(2):81-97.
#' @export
#' @examples
#' set.seed(1)
#' X <- matrix(rep(1:2, each = 10), 20, 1)
#' R <- rep(1, 20)
#' R[c(3, 7, 15)] <- 0
#' Itnnrs(y = rnorm(20), R = R, X = X)
Itnnrs <- function(y, R, X, weights = NULL) {
  yv <- .s03vec(y); n <- length(yv)
  if (n == 0L) stop("item_nonresponse: y is empty")
  r <- .s03vec(R)
  if (length(r) != n) stop("item_nonresponse: y and R have different lengths")
  if (any(r != 0 & r != 1)) stop("item_nonresponse: R must be 0 or 1")
  if (is.null(X)) {
    keys <- rep("all", n)
  } else {
    rows <- .s03mat(X)
    if (nrow(rows) != n) stop("item_nonresponse: X and y have different lengths")
    keys <- apply(rows, 1L, function(z) paste(sprintf("%.12g", z), collapse = "|"))
  }
  d <- if (!is.null(weights)) .s03vec(weights) else rep(1, n)
  if (length(d) != n) stop("item_nonresponse: weights and y have different lengths")
  ord <- unique(keys)
  tot <- vapply(ord, function(k) sum(d[keys == k]), 0)
  resp <- vapply(ord, function(k) sum(d[keys == k & r == 1]), 0)
  if (any(resp <= 0)) stop("item_nonresponse: weighting class with no respondents")
  fac <- tot / resp
  fi <- fac[match(keys, ord)]
  aw <- d * fi
  num <- sum(aw[r == 1] * yv[r == 1]); den <- sum(aw[r == 1])
  est <- num / den
  se <- sqrt(sum((aw[r == 1] * (yv[r == 1] - est))^2)) / den
  rates <- resp / tot
  .t1_result(estimate = est, se = se, n_classes = length(ord),
             response_rate = sum(resp) / sum(tot),
             min_class_rate = min(rates), max_class_rate = max(rates),
             adjusted_total = den, n_respondents = sum(r), n = n,
             method = "weighting-class adjustment f_c = W_c / W_c(resp), Kalton & Flores-Cervantes (2003)")
}
