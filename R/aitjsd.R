# SPDX-License-Identifier: AGPL-3.0-or-later
#' Jensen-Shannon divergence for compositional data.
#'
#' A composition carries only relative information, so it is closed to
#' the simplex before the divergence is taken -- which is exactly what
#' \code{Jsdiv(normalize = TRUE)} does, and why the arithmetic is not
#' duplicated here.  Closure makes the answer invariant to the total.
#' This is an information divergence on the closed parts, not an
#' Aitchison distance: it is not subcompositionally invariant and uses
#' no log-ratios.  A zero part contributes zero, where a log-ratio
#' geometry would be undefined -- a reason to prefer it on sparse
#' compositions and a reason not to call it Aitchison.
#'
#' @param p,q Non-negative compositions over the same parts.
#' @param base Logarithm base; 2 gives bits.
#' @return As \code{Jsdiv}.
#' @references Lin (1991), IEEE Transactions on Information Theory 37:145-151; coded form from philentropy.  See Jsdiv for the full note.
#' @export
Compjsd <- function(p, q, base = 2) Jsdiv(p, q, base = base, normalize = TRUE)
