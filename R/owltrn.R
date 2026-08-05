# SPDX-License-Identifier: AGPL-3.0-or-later
#' Outcome-weighted learning for an optimal treatment regime
#'
#' Zhao et al.'s reformulation is that maximizing the value
#' \code{V(d) = E[Y 1{A = d(X)} / pi(A | X)]} is the same problem as
#' MINIMIZING the weighted misclassification risk
#' \code{E[(Y / pi(A | X)) 1{A != d(X)}]}, a classification problem in
#' which the outcome is the weight and the observed treatment is the
#' label. The surrogate is theirs -- the weighted HINGE loss with a
#' ridge penalty, a linear-kernel weighted SVM,
#' \code{min_b (1/n) sum w_i max(0, 1 - A_i x_i'b) + lam ||b_slope||^2},
#' minimized by full-batch subgradient descent on the Pegasos step
#' schedule \code{eta_t = 1 / (lam t)}: a fixed iteration count, no
#' sampling and no tolerance test. The intercept is not penalized.
#'
#' The cheaper surrogate -- weighted least squares of the label on the
#' covariates -- has the right population minimiser but is not robust
#' to leverage. On a design where the high-weight units sit near the
#' decision boundary and the low-weight ones sit far from it, the
#' squared loss lets the far units outvote the informative ones and
#' returns the exactly INVERTED rule. That happened on this module's
#' own anchor fixture, which is why the hinge is used.
#'
#' The rule is \code{d(x) = 1{x'b > 0}}. NEGATIVE outcomes break the
#' equivalence, because a negative weight rewards misclassification;
#' the standard fix is to shift \code{Y} by its minimum. That shift is
#' NOT innocuous -- it changes the relative weights and so can change
#' the learned rule, which is exactly the objection that motivated
#' residual weighted learning -- so the amount shifted is RETURNED in
#' \code{shift} rather than applied silently.
#'
#' @param y Observed outcome, larger is better.
#' @param D Observed binary treatment, 0/1.
#' @param W Covariates, no intercept column; one is added.
#' @param pi Propensity \code{P(A = D_i | W_i)} per unit, or
#'   \code{NULL} for the marginal randomization probability.
#' @param lam Ridge penalty on the slope coefficients, positive.
#' @param n_iter Subgradient iterations.
#' @return List with \code{beta}, \code{estimate}, \code{value},
#'   \code{value_all_treated}, \code{value_all_control}, \code{rule},
#'   \code{n_treated_by_rule}, \code{hinge}, \code{shift}, \code{n},
#'   \code{p}.
#' @references Zhao, Y., Zeng, D., Rush, A. J. & Kosorok, M. R. (2012).
#'   Estimating individualized treatment rules using outcome weighted
#'   learning. Journal of the American Statistical Association,
#'   107(499), 1106-1118. doi:10.1080/01621459.2012.695674
#'   Shalev-Shwartz, S., Singer, Y., Srebro, N. & Cotter, A. (2011).
#'   Pegasos: primal estimated sub-gradient solver for SVM.
#'   Mathematical Programming, 127(1), 3-30.
#' @export
Owltrn <- function(y, D, W, pi = NULL, lam = 0.01, n_iter = 2000L) {
  yv <- as.numeric(y)
  n <- length(yv)
  if (n == 0L) stop("Owltrn: y is empty")
  Dv <- as.numeric(D)
  if (length(Dv) != n) stop("Owltrn: y and D have different lengths")
  if (!all(Dv %in% c(0, 1))) stop("Owltrn: D must be binary 0/1")
  Wm <- .t1_cbind1(W)
  if (nrow(Wm) != n) stop("Owltrn: W and y have different lengths")
  p <- ncol(Wm)
  if (is.null(pi)) {
    pt <- sum(Dv) / n
    if (pt <= 0 || pt >= 1) stop("Owltrn: both treatments must be observed")
    pv <- ifelse(Dv == 1, pt, 1 - pt)
  } else {
    pv <- as.numeric(pi)
    if (length(pv) != n) stop("Owltrn: pi and y have different lengths")
    if (any(pv <= 0 | pv > 1)) stop("Owltrn: pi must lie in (0, 1]")
  }
  lm_ <- as.numeric(lam)
  if (lm_ <= 0) stop("Owltrn: lam must be positive")
  Tn <- as.integer(n_iter)
  if (Tn < 1L) stop("Owltrn: n_iter must be at least 1")

  ymin <- min(yv)
  shift <- if (ymin < 0) -ymin else 0
  ys <- yv + shift
  w <- ys / pv
  wbar <- sum(w) / n
  if (wbar <= 0) stop("Owltrn: every weight is zero")
  w <- w / wbar
  lab <- ifelse(Dv == 1, 1, -1)

  beta <- numeric(p)
  for (t in seq_len(Tn)) {
    eta <- 1 / (lm_ * t)
    gr <- numeric(p)
    if (p > 1L) gr[2:p] <- lm_ * beta[2:p]
    marg <- lab * as.numeric(Wm %*% beta)
    act <- which(marg < 1)
    if (length(act)) {
      for (i in act) gr <- gr - w[i] * lab[i] * Wm[i, ] / n
    }
    beta <- beta - eta * gr
  }
  f <- as.numeric(Wm %*% beta)
  hinge <- sum(w * pmax(0, 1 - lab * f)) / n +
    lm_ * (if (p > 1L) sum(beta[2:p]^2) else 0)
  rule <- as.numeric(f > 0)
  value <- function(rec) {
    m <- as.numeric(rec == Dv)
    den <- sum(m / pv)
    if (den > 0) sum(ys * m / pv) / den else NaN
  }
  .t1_result(beta = beta, estimate = value(rule), value = value(rule),
             value_all_treated = value(rep(1, n)),
             value_all_control = value(rep(0, n)), rule = rule,
             n_treated_by_rule = sum(rule), hinge = hinge, shift = shift,
             n = n, p = p,
             method = "Outcome-weighted learning, weighted hinge (Zhao et al. 2012)")
}
