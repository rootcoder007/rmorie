# SPDX-License-Identifier: AGPL-3.0-or-later
#' Hardy-Weinberg equilibrium goodness-of-fit test
#'
#' Equilibrium proportions p^2, 2pq, q^2 with p estimated by gene counting,
#' compared with the observed counts by the Pearson statistic on 1 df.
#' Sources consulted: Hardy (1908), Mendelian proportions in a mixed
#' population, Science 28, 49-50; Weinberg (1908).  The proportions are theirs;
#' the chi-square goodness-of-fit test around them is the standard textbook
#' application.
#'
#' @param genotypes numeric vector of three counts, n_AA, n_Aa, n_aa.
#' @return list: statistic, p_value, df, p, q, expected, n, method.
#' @keywords internal
#' @examples
#' hwetst(c(25, 50, 25))
#' @export
hwetst <- function(genotypes) {
  g <- as.numeric(genotypes)
  n_aa <- g[1]; n_ab <- g[2]; n_bb <- g[3]
  ntot <- n_aa + n_ab + n_bb
  if (ntot <= 0) {
    return(list(statistic = NA_real_, p_value = NA_real_, df = 1L,
                p = NA_real_, n = 0L,
                method = "Hardy-Weinberg equilibrium test (Hardy 1908; Weinberg 1908)"))
  }
  p <- (2 * n_aa + n_ab) / (2 * ntot)
  q <- 1 - p
  expct <- c(ntot * p * p, 2 * ntot * p * q, ntot * q * q)
  obs <- c(n_aa, n_ab, n_bb)
  stat <- sum(ifelse(expct > 0, (obs - expct)^2 / expct, 0))
  list(statistic = as.numeric(stat),
       p_value = stats::pchisq(stat, df = 1, lower.tail = FALSE),
       df = 1L, p = as.numeric(p), q = as.numeric(q), expected = expct,
       n = as.integer(ntot),
       method = "Hardy-Weinberg equilibrium test (Hardy 1908; Weinberg 1908)")
}

# CANONICAL TEST
# r <- hwetst(c(25, 50, 25))
# stopifnot(abs(r$statistic) < 1e-12, abs(r$p - 0.5) < 1e-12)

#' @rdname hwetst
#' @keywords internal
#' @export
morie_hardy_weinberg <- hwetst
