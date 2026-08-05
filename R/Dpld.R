# SPDX-License-Identifier: AGPL-3.0-or-later
#' l-diversity of a released table, in all three senses
#'
#' Principle 2 of the paper says a q*-block is l-diverse if it contains
#' at least l well-represented values of the sensitive attribute.  The
#' two instantiations are entropy l-diversity, Definition 4.1,
#' \code{-sum_s p log p >= log(l)} for every block, and recursive
#' (c, l)-diversity, Definition 4.2, \code{r_1 < c (r_l + ... + r_m)}
#' with the block's sensitive-value counts sorted descending and
#' 1-diversity always satisfied.  Distinct l-diversity, the number of
#' distinct sensitive values, follows from the first and is the weakest
#' of the three.  All three are reported because they disagree: the
#' paper's Figure 4 is 3-diverse in the distinct sense and 2.8-diverse
#' in the entropy sense.
#'
#' @param X Records; only its length is used, to check against the blocks.
#' @param quasi_ids Quasi-identifier block, one row per record.
#' @param sensitive Sensitive attribute value per record.
#' @param l Required diversity level, at least 1.
#' @param c Constant of recursive (c, l)-diversity, strictly positive.
#' @return List with \code{estimate}, \code{distinct_l}, \code{entropy_l},
#'   \code{min_entropy}, \code{c_min}, \code{satisfies_distinct},
#'   \code{satisfies_entropy}, \code{satisfies_recursive},
#'   \code{n_blocks}, \code{min_block_size}, \code{l}, \code{c}, \code{n}.
#' @references Machanavajjhala, A., Kifer, D., Gehrke, J. and
#'   Venkitasubramaniam, M. (2007). l-diversity: privacy beyond
#'   k-anonymity. ACM Transactions on Knowledge Discovery from Data
#'   1(1), article 3, Definitions 4.1 and 4.2.
#' @export
Dpld <- function(X, quasi_ids, sensitive, l, c = 1) {
  n <- length(.s03vec(X))
  if (n == 0L) stop("Dpld: empty input, X has no records")
  rows <- .s03mat(quasi_ids)
  if (nrow(rows) != n) stop("Dpld: quasi_ids and X must have the same length")
  sv <- as.character(sensitive)
  if (length(sv) != n) stop("Dpld: sensitive and X must have the same length")
  ll <- as.integer(l)
  if (ll < 1L) stop("Dpld: l must be at least 1")
  if (!(c > 0)) stop("Dpld: c must be strictly positive")
  key <- apply(rows, 1L, function(r) paste(sprintf("%.12g", r), collapse = "|"))
  order_keys <- unique(key)
  distinct_l <- Inf; min_ent <- Inf; c_min <- 0; min_size <- Inf
  for (kk in order_keys) {
    vals <- sv[key == kk]
    lab <- unique(vals)
    cnt <- vapply(lab, function(s) sum(vals == s), 0)
    m <- length(lab); tot <- length(vals)
    p <- cnt / tot
    ent <- -sum(p * log(p))
    r <- sort(cnt, decreasing = TRUE)
    tail <- if (ll <= m) sum(r[ll:m]) else 0
    need <- if (tail <= 0) Inf else r[1L] / tail
    if (c_min < need) c_min <- need
    if (m < distinct_l) distinct_l <- m
    if (ent < min_ent) min_ent <- ent
    if (tot < min_size) min_size <- tot
  }
  .t1_result(estimate = distinct_l, distinct_l = distinct_l,
             entropy_l = exp(min_ent), min_entropy = min_ent, c_min = c_min,
             satisfies_distinct = if (distinct_l >= ll) 1 else 0,
             satisfies_entropy = if (min_ent >= log(ll) - 1e-12) 1 else 0,
             satisfies_recursive = if (ll == 1L || c_min < c) 1 else 0,
             n_blocks = length(order_keys), min_block_size = min_size,
             l = ll, c = c, n = n, method = "l-diversity baseline")
}
