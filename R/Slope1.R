# SPDX-License-Identifier: AGPL-3.0-or-later
#' Slope One and Weighted Slope One collaborative-filtering predictors
#'
#' Predicts user u's rating of item i.  For two items j and i let
#' \code{S_{j,i}(chi)} be the set of users who rated both.  The average
#' deviation of item i with respect to item j is
#' \code{dev_{j,i} = sum_{u in S_{j,i}} (u_j - u_i) / card(S_{j,i})}.
#' Since \code{dev_{j,i} + u_i} is itself a prediction of \code{u_j}, the
#' SLOPE ONE prediction averages them,
#' \code{P(u)_j = sum_{i in R_j} (dev_{j,i} + u_i) / card(R_j)} with
#' \code{R_j} the items the user rated other than j that have at least one
#' co-rating user, and the WEIGHTED SLOPE ONE prediction weights each term
#' by how many users support it,
#' \code{P_wS1(u)_j = sum (dev_{j,i} + u_i) c_{j,i} / sum c_{j,i}} over the
#' items the user rated other than j, where \code{c_{j,i}} is
#' \code{card(S_{j,i}(chi))}.
#'
#' Both are returned; \code{estimate} is the weighted form, the scheme
#' Lemire & Maclachlan recommend.  When no co-rated pair supports the target
#' item the prediction falls back to the user's own mean and
#' \code{fallback} is set to 1.
#'
#' @param R Users-by-items rating matrix; missing ratings are NA or NaN.
#' @param u Zero-based row index of the user whose rating is predicted.
#' @param i Zero-based column index of the target item.
#' @return List with \code{estimate} (weighted prediction),
#'   \code{weighted_slope_one}, \code{slope_one}, \code{user_mean},
#'   \code{n_pairs}, \code{support}, \code{fallback}, \code{observed},
#'   \code{error}, \code{n_users}, \code{n_items}, \code{user}, \code{item},
#'   \code{method}.
#' @references Lemire, D. & Maclachlan, A. (2005). Slope one predictors for
#'   online rating-based collaborative filtering. Proceedings of the 2005
#'   SIAM International Conference on Data Mining, 471-475.
#'   \doi{10.1137/1.9781611972757.43}
#' @export
#' @examples
#' R <- matrix(c(5, 3, NA, 4, NA, 2, NA, 5, 3), 3, 3, byrow = TRUE)
#' Slope1(R, u = 0, i = 2)
Slope1 <- function(R, u, i) {
  M <- .s03mat(R)
  nu <- nrow(M); ni <- ncol(M)
  if (nu == 0L) stop("slope_one: R is empty")
  u <- as.integer(u); i <- as.integer(i)
  if (u < 0L || u >= nu) stop("slope_one: u is out of range")
  if (i < 0L || i >= ni) stop("slope_one: i is out of range")
  ur <- u + 1L; ic <- i + 1L
  rated <- function(a, b) !is.na(M[a, b])

  rated_items <- integer(0)
  for (b in seq_len(ni)) if (b != ic && rated(ur, b)) rated_items <- c(rated_items, b)
  own <- numeric(0)
  for (b in seq_len(ni)) if (rated(ur, b)) own <- c(own, M[ur, b])
  user_mean <- if (length(own) > 0L) sum(own) / length(own) else NA_real_

  num_s1 <- 0; cnt_s1 <- 0L; num_w <- 0; den_w <- 0; support <- 0L
  for (b in rated_items) {
    cc <- 0L; s <- 0
    for (a in seq_len(nu)) {
      if (rated(a, ic) && rated(a, b)) {
        s <- s + M[a, ic] - M[a, b]
        cc <- cc + 1L
      }
    }
    if (cc == 0L) next
    dev <- s / cc
    pred <- dev + M[ur, b]
    num_s1 <- num_s1 + pred
    cnt_s1 <- cnt_s1 + 1L
    num_w <- num_w + pred * cc
    den_w <- den_w + cc
    support <- support + cc
  }

  fallback <- 0
  if (cnt_s1 == 0L || den_w == 0) {
    p_s1 <- user_mean; p_w <- user_mean; fallback <- 1
  } else {
    p_s1 <- num_s1 / cnt_s1; p_w <- num_w / den_w
  }

  observed <- if (rated(ur, ic)) M[ur, ic] else NA_real_
  .t1_result(estimate = p_w, weighted_slope_one = p_w, slope_one = p_s1,
             user_mean = user_mean, n_pairs = cnt_s1, support = support,
             fallback = fallback, observed = observed,
             error = if (!is.na(observed)) p_w - observed else NA_real_,
             n_users = nu, n_items = ni, user = u, item = i,
             method = "Weighted Slope One predictor (Lemire & Maclachlan 2005)")
}
