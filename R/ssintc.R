# SPDX-License-Identifier: AGPL-3.0-or-later
#' Nonparametric survival when each event time is only bracketed
#'
#' Interval censoring only says the event happened somewhere in
#' \code{(L, R]}. The likelihood cannot distinguish points inside an
#' interval, so all mass sits on the innermost intervals and the NPMLE is
#' a discrete distribution on those. The self-consistency equation is an
#' EM run for a fixed number of sweeps, so the arms cannot stop apart.
#'
#' Formula: with \code{alpha_ij = 1} when region j lies inside
#' observation i, iterate
#' \code{p_j = (1/n) sum_i alpha_ij p_j / sum_k alpha_ik p_k}.
#'
#' @param L Left endpoints; 0 for left-censored.
#' @param R Right endpoints; Inf for right-censored.
#' @param event Kept for interface compatibility.
#' @param n_iter EM sweeps.
#' @return List with \code{estimate}, \code{p}, \code{surv}, \code{q},
#'   \code{r}, \code{n}, \code{m}.
#' @references Turnbull, B. W. (1976). JRSS B 38:290-295, equation (10).
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Ssintc(V, V)
Ssintc <- function(L, R, event = NULL, n_iter = 200) {
  Lv <- as.numeric(L)
  Rv <- as.numeric(R)
  n <- length(Lv)
  lefts <- sort(unique(Lv))
  rights <- sort(unique(Rv[is.finite(Rv)]))
  both <- c(lefts, rights)
  qs <- numeric(0)
  rs <- numeric(0)
  for (q in lefts) for (r in rights) {
    if (q < r && !any(both > q & both < r)) { qs <- c(qs, q)
    rs <- c(rs, r) }
  }
  key <- paste(qs, rs)
  keep <- !duplicated(key)
  qs <- qs[keep]
  rs <- rs[keep]
  o <- order(qs, rs)
  qs <- qs[o]
  rs <- rs[o]
  m <- length(qs)
  if (m == 0L) {
    return(.t1_result(estimate = NaN, p = numeric(0), surv = numeric(0),
                      q = numeric(0), r = numeric(0), n = n, m = 0L,
                      method = "Turnbull NPMLE, interval censoring"))
  }
  alpha <- matrix(0, n, m)
  for (i in seq_len(n)) for (j in seq_len(m)) {
    if (Lv[i] <= qs[j] && rs[j] <= Rv[i]) alpha[i, j] <- 1
  }
  p <- rep(1 / m, m)
  for (it in seq_len(as.integer(n_iter))) {
    new <- rep(0, m)
    for (i in seq_len(n)) {
      den <- sum(alpha[i, ] * p)
      if (den <= 0) next
      new <- new + alpha[i, ] * p / den
    }
    tot <- sum(new)
    if (tot > 0) p <- new / tot
  }
  surv <- 1 - cumsum(p)
  med <- NaN
  hit <- which(surv <= 0.5)
  if (length(hit)) med <- rs[hit[1]]
  .t1_result(estimate = med, p = p, surv = surv, q = qs, r = rs, n = n, m = m,
             method = "Turnbull NPMLE, interval censoring")
}
