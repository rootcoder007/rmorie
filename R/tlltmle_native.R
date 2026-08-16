# morie.fn -- function file (rootcoder007/morie)
#' LTMLE: targeting the sequential regressions.
#' 
#' The g-computation estimand can be written as iterated conditional
#' expectations: regress the outcome on the history at the last time
#' point, evaluate that fit under the intervention rule, treat the result
#' as the outcome one step earlier, and repeat. Estimating those
#' regressions with machine learning gives a substitution estimator that
#' is *not* asymptotically linear -- the bias of a data-adaptive fit does
#' not vanish fast enough. Targeting fixes precisely that.
#' 
#' **Each step is a one-dimensional fluctuation.** Since the outcome
#' regressions are bounded in :math:`[0,1]` (after scaling), the loss is
#' the Bernoulli log-likelihood and the submodel is logistic with the
#' initial fit as **offset**:
#' 
#' .. math:: \mathrm{logit}\,\bar Q_t(\epsilon) =
#'           \mathrm{logit}\,\bar Q_t^0 + \epsilon\, H_t,
#' 
#' where the **clever covariate** :math:`H_t` is the inverse cumulative
#' probability of following the rule through time :math:`t`,
#' 
#' .. math:: H_t = \frac{\prod_{s \le t}
#'           I(A_s = d_s)}{\prod_{s \le t} g_s}.
#' 
#' Fitting :math:`\epsilon` by maximum likelihood makes the updated fit
#' solve the efficient influence curve equation for that component; doing
#' it at every time point, backwards, makes the whole estimator solve
#' :math:`P_n D^* = 0`.
#' 
#' **Double robustness, stated exactly.** The estimator is consistent if
#' *either* the sequential outcome regressions *or* the treatment
#' mechanism are consistently estimated -- not both. The anchor exploits
#' that: it breaks each arm separately and requires the estimate to
#' survive, then breaks both and requires it to fail. Two wrong arms are
#' the case that must not silently pass.
#' 
#' **Positivity is the binding constraint.** The clever covariate is an
#' inverse probability; as the cumulative probability of the rule
#' approaches zero it explodes, and the second-order remainder is bounded
#' only when :math:`g_{0} > \delta > 0`. That is why the module reports
#' the largest clever covariate rather than hiding it.
#' 
#' References
#' ----------
#' van der Laan, M. J. & Rose, S. (2018) *Targeted Learning in Data
#' Science*, Springer, doi:10.1007/978-3-319-65304-4. Chap. 4 (the
#' g-computation formula as iterated conditional expectations; the
#' efficient influence curve of the longitudinal parameter; the
#' Bernoulli log-likelihood loss and the logistic submodel through the
#' initial estimator with the clever covariate; the sequential definition
#' of loss and submodel for each Q_t; the second-order remainder and the
#' positivity condition g > delta > 0 that bounds it; and the use of a
#' super learner containing the highly adaptive lasso as the initial
#' estimator). Chap. 3 (the sequential regressions being targeted).
#' 
#' van der Laan, M. J. & Gruber, S. (2012) "Targeted minimum loss based
#' estimation of causal effects of multiple time point interventions",
#' *International Journal of Biostatistics* 8(1), Article 9,
#' doi:10.1515/1557-4679.1370.
#' 
#' Bang, H. & Robins, J. M. (2005) "Doubly robust estimation in missing
#' data and causal inference models", *Biometrics* 61(4), 962-973,
#' doi:10.1111/j.1541-0420.2005.00377.x.

#' .tlltmle_logit
#'
#' Part of the tlltmle_native implementation; see the file header for
#' the source it follows.
#'
#' @param p See Usage.
#' @return A numeric value.
#' @export
.tlltmle_logit <- function(p) {
  q <- pmin(pmax(as.numeric(p), 1e-9), 1 - 1e-9)
  return(log(q / (1.0 - q)))
}

#' .tlltmle_expit
#'
#' Part of the tlltmle_native implementation; see the file header for
#' the source it follows.
#'
#' @param x See Usage.
#' @return The value of \code{ifelse}.
#' @export
.tlltmle_expit <- function(x) {
  ifelse(x > -700, 1.0 / (1.0 + exp(-x)), 0.0)
}

#' tlltmle_clever_covariate
#'
#' Part of the tlltmle_native implementation; see the file header for
#' the source it follows.
#'
#' @param A See Usage.
#' @param g See Usage.
#' @param rule Defaults to \code{1}.
#' @return A list with \code{H}, \code{max}, \code{mean}, \code{note}.
#' @export
tlltmle_clever_covariate <- function(A, g, rule = 1.0) {
  a <- as.numeric(A)
  gg <- as.numeric(g)
  if (length(a) != length(gg)) {
    stop(sprintf("ltmle: %d treatments but %d propensities",
                 length(a), length(gg)))
  }
  if (any(gg <= 0.0 | gg >= 1.0)) {
    stop("ltmle: propensities must lie strictly inside (0,1)")
  }
  r <- as.numeric(rule)
  h <- ifelse(a == r, 1.0, 0.0) / gg
  list(H = h, max = max(h), mean = sum(h) / length(h),
       note = "a large clever covariate IS the positivity violation showing itself")
}

#' tlltmle_fluctuate
#'
#' Part of the tlltmle_native implementation; see the file header for
#' the source it follows.
#'
#' @param Q See Usage.
#' @param H See Usage.
#' @param Y See Usage.
#' @param iters Defaults to \code{100}.
#' @param tol Defaults to \code{1e-10}.
#' @return A list with \code{epsilon}, \code{Q_star}, \code{score}.
#' @export
tlltmle_fluctuate <- function(Q, H, Y, iters = 100, tol = 1e-10) {
  q <- as.numeric(Q)
  h <- as.numeric(H)
  y <- as.numeric(Y)
  n <- length(q)
  if (!(length(h) == length(y) && length(h) == n)) {
    stop("ltmle: the inputs differ in length")
  }
  off <- .tlltmle_logit(q)
  eps <- 0.0
  for (k in seq_len(as.integer(iters))) {
    p <- .tlltmle_expit(off + eps * h)
    gr <- sum(h * (y - p))
    he <- sum(h * h * p * (1.0 - p))
    if (he < 1e-12) break
    step <- gr / he
    eps <- eps + step
    if (abs(step) < as.numeric(tol)) break
  }
  upd <- .tlltmle_expit(off + eps * h)
  list(epsilon = eps, Q_star = upd,
       score = sum(h * (y - upd)) / n)
}

#' tlltmle_tmle_point
#'
#' Part of the tlltmle_native implementation; see the file header for
#' the source it follows.
#'
#' @param A See Usage.
#' @param Y See Usage.
#' @param Q1 See Usage.
#' @param Q0 See Usage.
#' @param g See Usage.
#' @return A list with \code{estimate}, \code{psi}, \code{epsilon}, \code{se}, \code{ci}, \code{mean_eic}, \code{solves_eic}, \code{max_clever_covariate}, \code{initial_plugin}, \code{method}, \code{note}.
#' @export
tlltmle_tmle_point <- function(A, Y, Q1, Q0, g) {
  a <- as.numeric(A)
  y <- as.numeric(Y)
  q1 <- as.numeric(Q1)
  q0 <- as.numeric(Q0)
  gg <- as.numeric(g)
  n <- length(a)
  H <- a / gg - (1.0 - a) / (1.0 - gg)
  qa <- ifelse(a == 1.0, q1, q0)
  fl <- tlltmle_fluctuate(qa, H, y)
  e <- fl$epsilon
  q1s <- .tlltmle_expit(.tlltmle_logit(q1) + e * (1.0 / gg))
  q0s <- .tlltmle_expit(.tlltmle_logit(q0) - e * (1.0 / (1.0 - gg)))
  psi <- sum(q1s - q0s) / n
  qas <- ifelse(a == 1.0, q1s, q0s)
  d <- H * (y - qas) + q1s - q0s - psi
  m <- sum(d) / n
  se <- sqrt(sum((d - m)^2) / n^2)
  list(estimate = psi, psi = psi, epsilon = e, se = se,
       ci = c(psi - 1.96 * se, psi + 1.96 * se),
       mean_eic = m, solves_eic = abs(m) < 1e-6,
       max_clever_covariate = max(abs(H)),
       initial_plugin = sum(q1 - q0) / n,
       method = "TMLE with a logistic submodel and clever covariate; van der Laan & Rose (2018) Chap. 4",
       note = "consistent if EITHER the outcome regression OR the treatment mechanism is consistent")
}

#' tlltmle_ltmle
#'
#' Part of the tlltmle_native implementation; see the file header for
#' the source it follows.
#'
#' @param Q_seq See Usage.
#' @param H_seq See Usage.
#' @param Y_seq See Usage.
#' @return A list with \code{estimate}, \code{psi}, \code{epsilons}, \code{Q_star}, \code{T}, \code{method}.
#' @export
tlltmle_ltmle <- function(Q_seq, H_seq, Y_seq) {
  T_ <- length(Q_seq)
  if (T_ < 1) stop("ltmle: the sequence is empty")
  if (length(H_seq) != T_) {
    stop(sprintf("ltmle: %d fits but %d clever covariates",
                 T_, length(H_seq)))
  }
  eps <- c()
  current <- as.numeric(Y_seq[[T_]])
  stars <- list()
  for (t in (T_ - 1):0) {
    fl <- tlltmle_fluctuate(Q_seq[[t + 1]], H_seq[[t + 1]], current)
    eps <- c(eps, fl$epsilon)
    current <- fl$Q_star
    stars <- c(stars, list(current))
  }
  psi <- sum(current) / length(current)
  list(estimate = psi, psi = psi, epsilons = rev(eps),
       Q_star = rev(stars), T = T_,
       method = "LTMLE by backward sequential fluctuation; van der Laan & Rose (2018) Chap. 4")
}

#' tlltmle_influence_curve_se
#'
#' Part of the tlltmle_native implementation; see the file header for
#' the source it follows.
#'
#' @param d See Usage.
#' @return A numeric value.
#' @export
tlltmle_influence_curve_se <- function(d) {
  v <- as.numeric(d)
  n <- length(v)
  if (n < 2) stop("ltmle: at least 2 observations are needed")
  m <- sum(v) / n
  sqrt(sum((v - m)^2) / (n - 1) / n)
}

#' tlltmle_cheatsheet
#'
#' Part of the tlltmle_native implementation; see the file header for
#' the source it follows.
#'
#' @return A character value.
#' @export
tlltmle_cheatsheet <- function() {
  "tlltmle: write the g-formula as ITERATED conditional expectations, fit them with machine learning, then TARGET each one. Every step is a one-dimensional logistic fluctuation with the initial fit as OFFSET and the clever covariate H = I(A = d)/g as the covariate; the MLE for epsilon makes the update solve the efficient influence curve equation. DOUBLE ROBUST: consistent if EITHER the outcome regressions OR the treatment mechanism is right -- not both. The clever covariate is an inverse probability, so a large one IS the positivity violation."
}

# compact alias per ledger/NAMING.md
tlltmle_longitudinaltmle <- tlltmle_ltmle

morie_tlltmle <- list(
  clever_covariate = tlltmle_clever_covariate,
  fluctuate = tlltmle_fluctuate,
  tmle_point = tlltmle_tmle_point,
  ltmle = tlltmle_ltmle,
  influence_curve_se = tlltmle_influence_curve_se,
  cheatsheet = tlltmle_cheatsheet,
  longitudinaltmle = tlltmle_longitudinaltmle
)
