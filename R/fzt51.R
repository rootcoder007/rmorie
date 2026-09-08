# SPDX-License-Identifier: AGPL-3.0-or-later

#' Equivalence of the naive kernel and empirical goodness-of-fit statistics (Theorem 5.1)
#'
#' Theorem 5.1: under `H0: F_X = F`,
#' \deqn{|KS_n - \widehat{KS}| \to_p 0\quad\text{and}\quad |CvM_n - \widehat{CvM}| \to_p
#' 0,}{|KS_n - KShat| ->_p 0 and |CvM_n - CvMhat| ->_p 0,}
#' where the hatted statistics use the NAIVE kernel distribution function
#' estimator of (5.3)-(5.4).
#'
#' The consequence Sec. 5.1 draws is practical: the smoothed statistics have
#' the very same Kolmogorov and Cramer-von Mises limiting distributions, so the
#' SAME critical values are used. Smoothing is not a new test, it is a
#' better-calibrated computation of the same test.
#'
#' Reports the two differences against a caller-supplied tolerance. This is a
#' convergence DIAGNOSTIC, not a hypothesis test: "converges in probability to
#' zero" is a statement about a sequence, and no single sample can confirm or
#' refute it. Hence `close`, not a p-value.
#'
#' @param ks_emp,ks_kernel The empirical and naive-kernel KS statistics.
#' @param cvm_emp,cvm_kernel The empirical and naive-kernel CvM statistics.
#' @param tol Tolerance against which the differences are reported.
#' @return Named list with ``ksdiff``, ``cvmdiff``, ``close``, ``tol``, ``method``.
#' @references Fauzi and Maesono (2023), Theorem 5.1.
#' @examples
#' Kerngofeq(ks_emp = 0.20, ks_kernel = 0.21, cvm_emp = 0.30, cvm_kernel = 0.31)
#' @export
Kerngofeq <- function(ks_emp, ks_kernel, cvm_emp, cvm_kernel, tol = 0.05) {
  if (tol <= 0) stop("tol must be positive.")
  ksd <- abs(ks_emp - ks_kernel)
  cvmd <- abs(cvm_emp - cvm_kernel)
  list(ksdiff = ksd, cvmdiff = cvmd, close = (ksd < tol && cvmd < tol),
       tol = tol,
       method = "naive kernel vs empirical GOF equivalence (Theorem 5.1)")
}

# CANONICAL TEST
# r <- Kerngofeq(ks_emp = 0.20, ks_kernel = 0.21, cvm_emp = 0.30, cvm_kernel = 0.31)
# stopifnot(r$close)

#' @rdname Kerngofeq
#' @keywords internal
#' @export
morie_fauzi_thm5_1_naive_kernel_equiv <- Kerngofeq
