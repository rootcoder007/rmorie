# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Proportion of the total effect mediated (Propme). Bit-identical
# mirror of src/morie/fn/propme.py.

#' Proportion mediated and mediated-to-direct ratio
#'
#' For the single-mediator model M = i1 + a X, Y = i2 + cp X + b M,
#' MacKinnon, Warsi and Dwyer (1995) define the proportion of the total
#' effect that is mediated as \eqn{ab / (c' + ab)} and the ratio of the
#' mediated to the nonmediated effect as \eqn{ab / c'}. When the three
#' path standard errors are supplied, first-order delta-method
#' variances are returned. The core proportion is the same measure as
#' \code{PropMd} (NIE over NIE plus NDE, with NIE = ab and NDE equal to
#' the direct effect under the linear model) and is delegated to it
#' rather than redefined. Variance forms:
#' \eqn{Var(PM) = (b^2 c'^2 s_a^2 + a^2 c'^2 s_b^2 + a^2 b^2 s_{c'}^2)
#' / (c' + ab)^4} and
#' \eqn{Var(ratio) = (b^2/c'^2) s_a^2 + (a^2/c'^2) s_b^2 +
#' (a^2 b^2 / c'^4) s_{c'}^2}, the first-order (uncorrected) solutions
#' in the same paper.
#'
#' @param a Path X to M.
#' @param b Path M to Y adjusted for X.
#' @param c_prime Direct effect X to Y adjusted for M.
#' @param se_a,se_b,se_c_prime Optional standard errors of the paths.
#' @return List with \code{estimate}, \code{same_sign},
#'   \code{indirect}, \code{total},
#'   \code{ratio}, \code{method}, and \code{se}, \code{se_ratio} when
#'   the standard errors are supplied.
#' @references MacKinnon, D. P., Warsi, G. and Dwyer, J. H. (1995), A
#'   simulation study of mediated effect measures, Multivariate
#'   Behavioral Research 30(1), 41-62, doi:10.1207/s15327906mbr3001_3;
#'   full text verified at
#'   https://pmc.ncbi.nlm.nih.gov/articles/PMC2821114/
#' @export
Propme <- function(a, b, c_prime,
                   se_a = NULL, se_b = NULL, se_c_prime = NULL) {
  a <- as.numeric(a)
  b <- as.numeric(b)
  c_prime <- as.numeric(c_prime)
  ab <- a * b
  total <- c_prime + ab
  if (total == 0) stop("total effect c_prime + a*b is zero", call. = FALSE)
  core <- PropMd(ab, c_prime)
  out <- list(estimate = core$estimate,
              same_sign = core$same_sign,
              indirect = ab,
              total = total,
              ratio = if (c_prime != 0) ab / c_prime else NaN,
              method = "MacKinnon-Warsi-Dwyer (1995) proportion mediated")
  if (!is.null(se_a) && !is.null(se_b) && !is.null(se_c_prime)) {
    sa2 <- as.numeric(se_a)^2
    sb2 <- as.numeric(se_b)^2
    sc2 <- as.numeric(se_c_prime)^2
    var_pm <- (b * b * c_prime * c_prime * sa2 +
               a * a * c_prime * c_prime * sb2 +
               a * a * b * b * sc2) / total^4
    out$se <- sqrt(var_pm)
    out$se_ratio <- if (c_prime != 0) {
      sqrt((b * b / c_prime^2) * sa2 + (a * a / c_prime^2) * sb2 +
           (a * a * b * b / c_prime^4) * sc2)
    } else {
      NaN
    }
  }
  out
}
