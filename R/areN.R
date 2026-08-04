# SPDX-License-Identifier: AGPL-3.0-or-later
#' Asymptotic relative efficiency of two estimators.
#'
#' ARE(T2, T1) = (var1/n1) / (var2/n2); the normal benchmarks 2/pi
#' (median vs mean) and 3/pi (Hodges-Lehmann vs mean) are returned as
#' closed forms.
#'
#' @param var1,var2 Asymptotic variances, strictly positive.
#' @param n1,n2 Sample sizes the variances refer to.
#'
#' @return List with are, logare, var1, var2, normalmedian, normalhl.
#' @references Hodges and Lehmann (1956), Annals of Mathematical
#'   Statistics 27(2), 324-335.  Standard published form; the article is
#'   not in the local corpus and was not read.
#' @export
Areratio <- function(var1, var2, n1 = 1, n2 = 1) {
  v1 <- as.numeric(var1); v2 <- as.numeric(var2)
  m1 <- as.numeric(n1); m2 <- as.numeric(n2)
  if (v1 <= 0 || v2 <= 0) stop("variances must be strictly positive")
  if (m1 <= 0 || m2 <= 0) stop("sample sizes must be strictly positive")
  are <- (v1 / m1) / (v2 / m2)
  .t1_result(are = are, logare = log(are), var1 = v1, var2 = v2,
             normalmedian = 2 / pi, normalhl = 3 / pi,
             method = "Asymptotic relative efficiency (Hodges-Lehmann 1956)")
}
