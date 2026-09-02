# morie.fn -- function file (rootcoder007/morie)
#' Laplace mechanism -- alias of the shipped implementation in dpglap.
#'
#' The generated stub for this module described the Laplace mechanism
#' M(D) = f(D) + Lap(sensitivity/epsilon).  That exact mechanism already
#' ships as \code{dpglap.dp_laplace_mechanism} (Dwork-Roth Definition 3.3),
#' so this module aliases it rather than adding a second implementation.
#'
#' References
#' ----------
#' Dwork, C., McSherry, F., Nissim, K., & Smith, A. (2006). Calibrating
#'     noise to sensitivity in private data analysis. Theory of
#'     Cryptography (TCC 2006), LNCS 3876, 265-284.
#' Dwork, C., & Roth, A. (2014). The algorithmic foundations of
#'     differential privacy. Foundations and Trends in Theoretical
#'     Computer Science, 9(3-4), 211-487. Definition 3.3 and Theorem 3.6.

# Private helper: sample n values from Laplace(0, scale) via inverse CDF.
#' @keywords internal
#' @noRd
.laplc_sample_laplace <- function(scale, n = 1L) {
  u <- runif(n)
  # Inverse CDF of Laplace(0, scale):
  #   if u < 0.5:  x =  scale * log(2 * u)
  #   if u >= 0.5: x = -scale * log(2 * (1 - u))
  ifelse(u < 0.5,
          scale * log(2 * u),
         -scale * log(2 * (1 - u)))
}

#' Laplace mechanism (primary entry point).
#'
#' Implements M(D) = f(D) + Lap(sensitivity / epsilon), Dwork-Roth
#' Definition 3.3.
#'
#' @param value Numeric scalar. The true query result f(D).
#' @param sensitivity Numeric scalar. The L1 sensitivity of the query.
#' @param epsilon Numeric scalar. The privacy budget (must be > 0).
#' @param seed Integer or NULL. Optional seed for reproducibility.
#' @return A named list (RichResult) with the noisy value and metadata.
#' @examples
#' morie_laplc(value = c(1, 2, 3, 4, 5, 6, 7, 8), sensitivity = c(1, 2, 3, 4, 5, 6, 7, 8), epsilon = c(1, 2, 3, 4, 5, 6, 7, 8))
morie_laplc <- function(value, sensitivity, epsilon, seed = NULL) {
  if (!is.null(seed)) {
    set.seed(as.integer(seed))
  }

  scale <- sensitivity / epsilon
  noise <- .laplc_sample_laplace(scale, 1L)
  noisy_value <- value + noise

  list(
    value        = noisy_value,
    true_value   = value,
    noise        = noise,
    scale        = scale,
    sensitivity  = sensitivity,
    epsilon      = epsilon,
    mechanism    = "laplace"
  )
}

#' Legacy stub name, kept for compatibility.
#' @noRd
morie_laplace_mechanism <- morie_laplc

#' Cheatsheet.
morie_laplc_cheatsheet <- function() {
  "laplc: Laplace mechanism (alias of dpglap.dp_laplace_mechanism)."
}
