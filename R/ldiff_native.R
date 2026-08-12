# l-diversity of a release (distinct, entropy, recursive (c, l)).
# Source: Machanavajjhala, A., Kifer, D., Gehrke, J. and
# Venkitasubramaniam, M. (2007), l-diversity: privacy beyond
# k-anonymity, ACM TKDD 1(1), article 3, Definitions 4.1 and 4.2
# (Secs. 4.1-4.2: distinct, entropy and recursive (c, l)-diversity).
# Native implementation mirroring Python morie.fn.dpld.l_diversity
# (which morie.fn.ldiff exposes): same block keying to 12 significant
# digits, same descending-count ordering with first-appearance ties.

#' l-diversity of an anonymised release
#'
#' Groups records into equivalence blocks by their quasi-identifiers
#' and evaluates the three diversity criteria of Machanavajjhala et
#' al. (2007) on the sensitive attribute within each block:
#'
#' \itemize{
#'   \item distinct l-diversity: every block holds at least l
#'     distinct sensitive values (their Definition 4.1 discussion);
#'   \item entropy l-diversity: every block has Shannon entropy of
#'     the sensitive distribution at least log(l) (Definition 4.1);
#'   \item recursive (c, l)-diversity: in every block, with counts
#'     r_1 >= r_2 >= ... >= r_m, r_1 < c (r_l + ... + r_m)
#'     (Definition 4.2).
#' }
#'
#' The reported \code{c_min} is the largest r_1 / (r_l + ... + r_m)
#' over blocks, so the release is recursive (c, l)-diverse exactly
#' when \code{c_min < c}.
#'
#' @param X Records; only the length is used, to check the blocks.
#' @param quasi_ids Quasi-identifier matrix, one row per record.
#' @param sensitive Sensitive attribute per record (compared as
#'   labels, so numbers and strings both work).
#' @param l Required diversity level (at least 1).
#' @param c Constant of recursive (c, l)-diversity (positive).
#' @return A list with elements \code{estimate}, \code{distinct_l},
#'   \code{entropy_l}, \code{min_entropy}, \code{c_min},
#'   \code{satisfies_distinct}, \code{satisfies_entropy},
#'   \code{satisfies_recursive}, \code{n_blocks},
#'   \code{min_block_size}, \code{l}, \code{c}, \code{n},
#'   \code{method}.
#' @references Machanavajjhala, A., Kifer, D., Gehrke, J. and
#'   Venkitasubramaniam, M. (2007). l-diversity: privacy beyond
#'   k-anonymity. ACM Transactions on Knowledge Discovery from Data,
#'   1(1), article 3.
#' @export
morie_ldiff <- function(X, quasi_ids, sensitive, l, c = 1) {
  n <- length(as.vector(X))
  if (n == 0) stop("empty input: X has no records")
  rows <- as.matrix(quasi_ids)
  if (nrow(rows) != n) stop("quasi_ids and X must have the same length")
  sv <- as.character(sensitive)
  if (length(sv) != n) stop("sensitive and X must have the same length")
  ll <- as.integer(l)
  if (ll < 1L) stop("l must be at least 1")
  c <- as.numeric(c)
  if (!(c > 0)) stop("c must be strictly positive")
  # block key: 12 significant digits, matching the Python "%.12g"
  keys <- vapply(seq_len(n), function(i)
    paste(formatC(as.numeric(rows[i, ]), format = "g", digits = 12),
          collapse = "|"), character(1))
  order_keys <- unique(keys)
  distinct_l <- NA_real_; min_ent <- NA_real_
  c_min <- 0; min_size <- NA_real_
  for (key in order_keys) {
    vals <- sv[keys == key]
    seen <- unique(vals)                 # first-appearance order
    cnt <- vapply(seen, function(s) sum(vals == s), numeric(1))
    m <- length(seen)
    tot <- length(vals)
    p <- cnt / tot
    ent <- -sum(p * log(p))
    r <- sort(cnt, decreasing = TRUE)
    tail <- if (ll <= m) sum(r[ll:m]) else 0
    need <- if (tail <= 0) Inf else r[1] / tail
    if (c_min < need) c_min <- need
    if (is.na(distinct_l) || m < distinct_l) distinct_l <- m
    if (is.na(min_ent) || ent < min_ent) min_ent <- ent
    if (is.na(min_size) || tot < min_size) min_size <- tot
  }
  list(estimate = as.numeric(distinct_l),
       distinct_l = as.numeric(distinct_l),
       entropy_l = exp(min_ent), min_entropy = min_ent,
       c_min = c_min,
       satisfies_distinct = if (distinct_l >= ll) 1 else 0,
       satisfies_entropy = if (min_ent >= log(ll) - 1e-12) 1 else 0,
       satisfies_recursive = if (ll == 1L || c_min < c) 1 else 0,
       n_blocks = as.numeric(length(order_keys)),
       min_block_size = as.numeric(min_size),
       l = as.numeric(ll), c = c, n = n,
       method = "l-diversity baseline")
}
