# SPDX-License-Identifier: AGPL-3.0-or-later
#
# RDD shelf, wave3 w5_08: Imbens-Kalyanaraman plug-in bandwidth
# (Causrddh), sharp local-linear RDD (Causrdd), fuzzy Wald-ratio RDD
# (Causrddf). Bit-identical mirrors of src/morie/fn/causrdd{h,,f}.py,
# including BOTH documented errata of NBER w14726 (eq 4.8 missing
# factor 2; Section 6.2 regularization counts taken within the
# median-trimmed sample) validated against the paper's own Section
# 6.2 worked example on inst/extdata/quasiex/lee2008_house.csv
# (h_opt 0.264863, printed 0.2649). The pre-existing
# .morie_rdd_ik_native (2160-constant published-version variant) is a
# DIFFERENT algorithm and is left untouched.

#' Imbens-Kalyanaraman plug-in bandwidth for RDD (NBER w14726)
#'
#' Section 4.4 algorithm, edge-kernel constant C_K = 3.4375, with the
#' two documented errata followed per the paper's own Section 6.2
#' worked example (see the Python docstring of morie.fn.causrddh for
#' the full derivation).
#'
#' @param x Running variable.
#' @param y Outcome.
#' @param cutoff Threshold c.
#' @return List mirroring the Python payload: \code{estimate},
#'   \code{h1}, \code{f_hat}, \code{sigma2}, \code{n_left_h1},
#'   \code{n_right_h1}, \code{mean_left_h1}, \code{mean_right_h1},
#'   \code{m3}, \code{h2_left}, \code{h2_right}, \code{m2_left},
#'   \code{m2_right}, \code{n2_left}, \code{n2_right},
#'   \code{n2_left_full}, \code{n2_right_full}, \code{r_left},
#'   \code{r_right}, \code{h_unregularized}, \code{kernel_constant},
#'   \code{n}, \code{method}.
#' @references Imbens, G. and Kalyanaraman, K. (2009), NBER Working
#'   Paper 14726, Section 4.4 eqs (4.8)-(4.13) and Section 6.2;
#'   published as Review of Economic Studies 79(3), 933-959 (2012),
#'   \doi{10.1093/restud/rdr043}. Local source:
#'   fetched-wave3/imbens-kalyanaraman-2009-w14726-optimal-bandwidth-rdd.pdf.
#' @export
#' @examples
#' set.seed(1)
#' x <- runif(200, -1, 1)
#' y <- 1 + 2 * x + 0.5 * (x >= 0) + rnorm(200, 0, 0.3)
#' h <- Causrddh(x, y, cutoff = 0)
#' is.numeric(h$estimate)
Causrddh <- function(x, y, cutoff = 0) {
  xa <- as.numeric(x); ya <- as.numeric(y)
  n <- length(xa)
  if (n < 10L) stop("need at least 10 observations", call. = FALSE)
  c0 <- as.numeric(cutoff)[1]
  d <- xa - c0
  sx <- stats::sd(xa)
  h1 <- 1.84 * sx * n^(-0.2)
  il <- d >= -h1 & d < 0
  ir <- d >= 0 & d <= h1
  nl <- sum(il); nr <- sum(ir)
  if (nl < 3L || nr < 3L) {
    stop("fewer than 3 observations within the pilot window on one side of the cutoff", call. = FALSE)
  }
  yl <- ya[il]; yr <- ya[ir]
  s2l <- stats::var(yl); s2r <- stats::var(yr)
  f_hat <- (nl + nr) / (2 * n * h1)
  sigma2 <- ((nl - 1) * s2l + (nr - 1) * s2r) / (nl + nr)
  left <- d < 0; right <- d >= 0
  n_neg <- sum(left); n_pos <- sum(right)
  med_l <- stats::median(d[left]); med_r <- stats::median(d[right])
  keep <- d >= med_l & d <= med_r
  dk <- d[keep]; yk <- ya[keep]
  Xc <- cbind(1, as.numeric(dk >= 0), dk, dk^2, dk^3)
  g <- qr.solve(crossprod(Xc), crossprod(Xc, yk))
  m3 <- 6 * g[5L]
  base <- (sigma2 / (f_hat * max(m3 * m3, 0.01)))^(1 / 7)
  h2r <- 3.56 * base * n_pos^(-1 / 7)
  h2l <- 3.56 * base * n_neg^(-1 / 7)
  quad <- function(mask, mask_trim) {
    dm <- d[mask]; ym <- ya[mask]
    n2 <- length(dm)
    if (n2 < 4L) stop("fewer than 4 observations in a pilot quadratic window", call. = FALSE)
    Xq <- cbind(1, dm, dm^2)
    b <- qr.solve(crossprod(Xq), crossprod(Xq, ym))
    n2_trim <- sum(mask & mask_trim)
    list(m2 = 2 * b[3L], n2_full = n2, n2 = max(n2_trim, 1L))
  }
  qr_ <- quad(right & d <= h2r, d <= med_r)
  ql_ <- quad(left & d >= -h2l, d >= med_l)
  rr <- 720 * sigma2 / (qr_$n2 * h2r^4)
  rl <- 720 * sigma2 / (ql_$n2 * h2l^4)
  curv <- (qr_$m2 - ql_$m2)^2
  CK <- 3.4375
  h_opt <- CK * (2 * sigma2 / (f_hat * (curv + rr + rl)))^0.2 * n^(-0.2)
  h_unreg <- if (curv > 0) CK * (2 * sigma2 / (f_hat * curv))^0.2 * n^(-0.2) else Inf
  list(estimate = h_opt, h1 = h1, f_hat = f_hat, sigma2 = sigma2,
       n_left_h1 = nl, n_right_h1 = nr,
       mean_left_h1 = mean(yl), mean_right_h1 = mean(yr),
       m3 = m3, h2_left = h2l, h2_right = h2r,
       m2_left = ql_$m2, m2_right = qr_$m2,
       n2_left = ql_$n2, n2_right = qr_$n2,
       n2_left_full = ql_$n2_full, n2_right_full = qr_$n2_full,
       r_left = rl, r_right = rr, h_unregularized = h_unreg,
       kernel_constant = CK, n = n,
       method = "Imbens-Kalyanaraman (2009/2012) plug-in bandwidth, edge kernel, NBER w14726 algorithm")
}

#' .morie_w508_llr_side
#'
#' A step of the rdd_w508_native implementation. Called by \code{Causrdd}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param dm A vector; its length is taken.
#' @param ym A matrix; passed to \code{\%*\%}.
#' @param w Numeric; combined arithmetically in the body.
#' @return A list with \code{a}, \code{b}, \code{v}.
#' @export
.morie_w508_llr_side <- function(dm, ym, w) {
  n <- length(dm)
  X <- cbind(1, dm)
  XtW <- t(X * w)
  A <- XtW %*% X
  b <- solve(A, XtW %*% ym)
  e <- as.vector(ym - X %*% b)
  meat <- (XtW * rep(e^2, each = 2L)) %*% (X * w)
  Ainv <- solve(A)
  V <- Ainv %*% meat %*% Ainv
  list(a = b[1L], b = b[2L], v = V[1L, 1L])
}

#' Sharp regression-discontinuity estimate by local linear fits
#'
#' tau = alpha_plus - alpha_minus from kernel-weighted one-sided
#' linear fits at the cutoff; IK plug-in bandwidth
#' (\code{\link{Causrddh}}) and edge (triangular) kernel by default.
#' HC0 sandwich per side, sides independent (rdrobust vce = "hc0"
#' convention). Exact on side-linear data for any bandwidth.
#'
#' @param x Running variable.
#' @param y Outcome.
#' @param cutoff Threshold c.
#' @param h Bandwidth; IK plug-in when NULL.
#' @param kernel "triangular" or "uniform".
#' @return List with \code{estimate}, \code{se}, \code{ci},
#'   \code{intercept_left}, \code{intercept_right},
#'   \code{slope_left}, \code{slope_right}, \code{h}, \code{kernel},
#'   \code{n_left}, \code{n_right}, \code{n_used}, \code{method}.
#' @references Imbens, G. and Kalyanaraman, K. (2009), NBER w14726,
#'   Sections 2-3; White (1980) HC0; Calonico, Cattaneo and Titiunik
#'   (2014), Econometrica 82(6), 2295-2326 (vce convention).
#' @export
#' @examples
#' set.seed(1)
#' x <- runif(300, -1, 1)
#' y <- 1 + 2 * x + 0.5 * (x >= 0) + rnorm(300, 0, 0.3)
#' r <- Causrdd(x, y, cutoff = 0)
#' abs(r$estimate - 0.5) < 0.4
Causrdd <- function(x, y, cutoff = 0, h = NULL, kernel = "triangular") {
  xa <- as.numeric(x); ya <- as.numeric(y)
  c0 <- as.numeric(cutoff)[1]
  if (is.null(h)) h <- Causrddh(xa, ya, cutoff = c0)$estimate
  h <- as.numeric(h)[1]
  if (h <= 0) stop("bandwidth must be positive", call. = FALSE)
  d <- xa - c0
  u <- d / h
  w <- switch(kernel,
    triangular = pmax(1 - abs(u), 0),
    uniform = ifelse(abs(u) <= 1, 0.5, 0),
    stop("kernel must be triangular or uniform", call. = FALSE))
  lm_ <- d < 0 & w > 0
  rm_ <- d >= 0 & w > 0
  n_l <- sum(lm_); n_r <- sum(rm_)
  if (n_l < 3L || n_r < 3L) {
    stop("fewer than 3 observations with positive kernel weight on one side", call. = FALSE)
  }
  fl <- .morie_w508_llr_side(d[lm_], ya[lm_], w[lm_])
  fr <- .morie_w508_llr_side(d[rm_], ya[rm_], w[rm_])
  tau <- fr$a - fl$a
  se <- sqrt(fl$v + fr$v)
  z <- 1.959963984540054
  list(estimate = tau, se = se, ci = c(tau - z * se, tau + z * se),
       intercept_left = fl$a, intercept_right = fr$a,
       slope_left = fl$b, slope_right = fr$b, h = h, kernel = kernel,
       n_left = n_l, n_right = n_r, n_used = n_l + n_r,
       se_note = "HC0 sandwich per side, sides independent; rdrobust vce=hc0 convention",
       method = "sharp RDD, one-sided local linear fits at the cutoff")
}

#' Fuzzy regression-discontinuity estimate (Wald ratio of jumps)
#'
#' tau_FRD = (outcome jump)/(treatment jump), each from
#' \code{\link{Causrdd}} with its own IK bandwidth (Imbens-Lemieux
#' 2008 separate-bandwidth prescription per IK Section 5.1). Delta-
#' method se with independent jumps (covariance term omitted and
#' documented). Reduces exactly to the sharp estimate when the first
#' stage jumps 0 to 1.
#'
#' @param x Running variable.
#' @param y Outcome.
#' @param treat Treatment received (0/1 or probability).
#' @param cutoff Threshold c.
#' @param h Outcome bandwidth; IK plug-in when NULL.
#' @param h_treat First-stage bandwidth; IK plug-in when NULL.
#' @param kernel "triangular" or "uniform".
#' @return List with \code{estimate}, \code{se}, \code{ci},
#'   \code{jump_outcome}, \code{jump_treatment}, \code{se_outcome},
#'   \code{se_treatment}, \code{h_outcome}, \code{h_treatment},
#'   \code{kernel}, \code{method}.
#' @references Hahn, J., Todd, P. and van der Klaauw, W. (2001),
#'   Econometrica 69(1), 201-209 (estimand); Imbens and Kalyanaraman
#'   (2009), NBER w14726, Section 5.1 (implemented text, in the local
#'   registry); Imbens and Lemieux (2008), J Econometrics 142(2),
#'   615-635.
#' @export
#' @examples
#' set.seed(1)
#' x <- runif(300, -1, 1)
#' treat <- rbinom(300, 1, ifelse(x >= 0, 0.85, 0.15))
#' y <- 1 + 2 * x + 0.6 * treat + rnorm(300, 0, 0.3)
#' r <- Causrddf(x, y, treat, cutoff = 0)
#' is.list(r)
Causrddf <- function(x, y, treat, cutoff = 0, h = NULL, h_treat = NULL,
                     kernel = "triangular") {
  xa <- as.numeric(x); ya <- as.numeric(y); wa <- as.numeric(treat)
  c0 <- as.numeric(cutoff)[1]
  if (is.null(h)) h <- Causrddh(xa, ya, cutoff = c0)$estimate
  if (is.null(h_treat)) h_treat <- Causrddh(xa, wa, cutoff = c0)$estimate
  fy <- Causrdd(xa, ya, cutoff = c0, h = h, kernel = kernel)
  fw <- Causrdd(xa, wa, cutoff = c0, h = h_treat, kernel = kernel)
  ty <- fy$estimate; tw <- fw$estimate
  if (abs(tw) < 1e-12) {
    stop("no first-stage discontinuity: the treatment jump at the cutoff is numerically zero", call. = FALSE)
  }
  tau <- ty / tw
  se <- sqrt((fy$se^2 + tau^2 * fw$se^2) / tw^2)
  z <- 1.959963984540054
  list(estimate = tau, se = se, ci = c(tau - z * se, tau + z * se),
       jump_outcome = ty, jump_treatment = tw,
       se_outcome = fy$se, se_treatment = fw$se,
       h_outcome = as.numeric(h), h_treatment = as.numeric(h_treat),
       kernel = kernel, sharp_outcome = fy, sharp_treatment = fw,
       se_note = "delta method with independent jumps; Imbens-Lemieux covariance term omitted and documented",
       method = "fuzzy RDD, Wald ratio of local linear jumps")
}
