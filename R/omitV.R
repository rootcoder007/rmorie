# SPDX-License-Identifier: AGPL-3.0-or-later
#' Omitted variable bias, classical and partial-R2 form.
#'
#' Formula: bias = delta gamma; equivalently |bias| = se(tau_res) sqrt(R2_YZ.DX R2_DZ.X / (1 - R2_DZ.X)) sqrt(df)
#'
#' @param delta Coefficient of the confounder in the treatment regression.
#' @param gamma Coefficient of the confounder in the outcome regression.
#' @param estimate The estimate obtained without the confounder.
#' @param se Its standard error.
#' @param df Residual degrees of freedom of that regression.
#' @param r2_yz Partial R2 of the confounder with the outcome given treatment and covariates.
#' @param r2_dz Partial R2 of the confounder with the treatment given covariates.

#' @return List with ``bias``, ``adjusted_estimate``, ``adjusted_se``, ``adjusted_t``, ``relative_bias``, ``bias_factor``.
#' @references Cinelli and Hazlett (2020), Making Sense of Sensitivity: Extending Omitted Variable Bias, JRSS-B 82:39-67. Verified against the author's copy of the paper: bias = delta gamma (Section 4.1), equation (12) for the adjusted standard error, equation (13) for the bias in partial-R2 form, equation (14) for the relative bias.
#' @export
#' @examples
#' Ovbias()
Ovbias <- function(delta = NULL, gamma = NULL, estimate = NULL, se = NULL, df = NULL, r2_yz = NULL, r2_dz = NULL) {
  bias <- NA_real_
  if (!is.null(delta) && !is.null(gamma)) bias <- as.numeric(delta) * as.numeric(gamma)
  adj_se <- NA_real_; adj_t <- NA_real_; rel <- NA_real_; bf <- NA_real_
  if (!is.null(r2_yz) && !is.null(r2_dz) && !is.null(se) && !is.null(df)) {
    ry <- as.numeric(r2_yz); rd <- as.numeric(r2_dz)
    s <- as.numeric(se); d <- as.numeric(df)
    if (ry < 0 || ry >= 1 || rd < 0 || rd >= 1)
      stop("partial R2 values must be in [0, 1)")
    if (d <= 1) stop("df must exceed 1")
    bias <- s * sqrt(ry * rd / (1 - rd)) * sqrt(d)
    adj_se <- s * sqrt((1 - ry) / (1 - rd) * d / (d - 1))
    bf <- sqrt(ry) * sqrt(rd / (1 - rd))
    if (!is.null(estimate)) {
      f_yd <- abs(as.numeric(estimate) / s) / sqrt(d)
      rel <- if (f_yd > 0) bf / f_yd else Inf
    }
  }
  adj <- NA_real_
  if (!is.null(estimate) && !is.na(bias)) {
    e <- as.numeric(estimate)
    adj <- if (bias >= 0) e - sign(e) * bias else e - bias
    if (!is.na(adj_se) && adj_se > 0) adj_t <- adj / adj_se
  }
  .t1_result(bias = bias, adjusted_estimate = adj, adjusted_se = adj_se,
             adjusted_t = adj_t, relative_bias = rel, bias_factor = bf,
             method = "Omitted variable bias (Cinelli-Hazlett)")
}
