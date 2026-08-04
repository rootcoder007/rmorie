# SPDX-License-Identifier: AGPL-3.0-or-later
#' Conditional mutual information
#'
#' Cover and Thomas (2006), Elements of Information Theory, 2nd ed.,
#' section 2.5, eq. (2.60): I(X;Y|Z) = sum_z p(z) I(X;Y|Z=z) = H(X|Z) +
#' H(Y|Z) - H(X,Y|Z).  The book is not open access; the identity is quoted
#' in its standard published form.  I(X;Y|Z) can be LARGER than I(X;Y) --
#' conditioning can create dependence, as in the XOR example -- so both are
#' returned and their difference, the interaction information, is reported
#' rather than left implicit.
#'
#' @param y first variable (first slot, for signature stability).
#' @param x second variable, or the first of the pair when y2 is given.
#' @param y2 second of the pair.
#' @param z the conditioning variable.
#' @return list: estimate, cmi, mi, interaction, per_level, bits, n, method.
#' @keywords internal
#' @examples
#' Condmi(c(0, 0, 1, 1), c(0, 1, 0, 1), NULL, c(1, 1, 2, 2))$cmi
#' @export
Condmi <- function(y, x = NULL, y2 = NULL, z = NULL) {
  if (is.null(y2)) { a <- as.character(y); b <- as.character(x) }
  else { a <- as.character(x); b <- as.character(y2) }
  cc <- as.character(if (!is.null(z)) z else rep(0, length(a)))
  n <- length(a)
  lv <- sort(unique(cc), method = "radix")
  cmi <- 0; per <- list()
  for (v in lv) {
    idx <- which(cc == v)
    pz <- length(idx) / n
    if (length(idx) < 2L) { per[[length(per) + 1L]] <- c(pz, 0); next }
    sub <- Mutinfo(a[idx], b[idx])
    cmi <- cmi + pz * sub$mi
    per[[length(per) + 1L]] <- c(pz, sub$mi)
  }
  uncond <- Mutinfo(a, b)$mi
  list(estimate = cmi, cmi = cmi, mi = uncond, interaction = uncond - cmi,
       per_level = per, bits = cmi / log(2), n = n,
       method = "Conditional mutual information (Cover and Thomas 2006, eq. 2.60)")
}
