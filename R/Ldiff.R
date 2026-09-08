# SPDX-License-Identifier: AGPL-3.0-or-later
#' l-diversity (alias)
#'
#' Alias of \code{Dpld}, which implements distinct, entropy and
#' recursive (c, l)-diversity of Machanavajjhala et al. (2007). The
#' generated stub for module ldiff described that same check, so this is
#' an alias.
#'
#' @param X See Usage.
#' @param quasi_ids See Usage.
#' @param sensitive See Usage.
#' @param l See Usage.
#' @param c See Usage.
#' @references Machanavajjhala, A., Kifer, D., Gehrke, J., and
#'   Venkitasubramaniam, M. (2007). l-diversity: privacy beyond
#'   k-anonymity. ACM Transactions on Knowledge Discovery from Data 1(1),
#'   article 3, sections 4.1-4.2.
#'
#' Delegating wrapper (not a bare assignment) so that source collation
#' order does not matter at load time; the body is a single call to the
#' target, so outputs are exactly identical.
#' @export
Ldiff <- function(X, quasi_ids, sensitive, l, c = 1) {
  Dpld(X, quasi_ids, sensitive, l, c = c)
}
