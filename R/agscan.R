# SPDX-License-Identifier: AGPL-3.0-or-later
#' Self-consistency of a policy network across repeated runs
#'
#' The AlphaZero literature states no self-consistency statistic: Silver
#' et al. (2018), arXiv:1712.01815 (FETCHED), reports run-to-run variation
#' only as Elo curves.  Rather than invent an attribution, this computes
#' the two quantities that are well defined for a set of policies over one
#' action space and cites them where they ARE defined: Shannon entropy
#' H(p) = -sum p log p (Shannon 1948, Bell System Technical Journal 27,
#' 379-423, eq. 11) and the Jensen-Shannon divergence JSD = H(pbar) -
#' mean_i H(p_i) (Lin 1991, IEEE Trans. Inf. Theory 37(1), 145-151, eq.
#' 3.1).  JSD is zero exactly when every run gave the same policy.
#'
#' @param policy_net the policies, one row per run, or a function
#'   seed -> p applied to every entry of `seeds`.
#' @param seeds run labels; they are not fed to any generator here.
#' @return list: estimate, jsd, entropies, mean_entropy, sd_entropy,
#'   range_entropy, mean_policy, n, method.
#' @keywords internal
#' @examples
#' Selfconsis(matrix(c(0.5, 0.5, 0.9, 0.1), 2, 2, byrow = TRUE))$jsd
#' @export
Selfconsis <- function(policy_net, seeds = NULL) {
  ent1 <- function(p) {
    h <- 0
    for (x in p) if (x > 0) h <- h - x * log(x)
    h
  }
  if (is.function(policy_net)) {
    sd_ <- if (!is.null(seeds)) seeds else list()
    rows <- lapply(seq_along(sd_), function(i) .s03vec(policy_net(sd_[[i]])))
  } else {
    m0 <- .s03mat(policy_net)
    rows <- lapply(seq_len(nrow(m0)), function(i) as.numeric(m0[i, ]))
  }
  m <- length(rows)
  if (m == 0L) {
    return(list(estimate = NaN, jsd = NaN, entropies = numeric(0), n = 0L,
                method = "Policy self-consistency"))
  }
  norm <- lapply(rows, function(p) { t <- 0; for (x in p) t <- t + x
                                     if (t > 0) p / t else p })
  K <- length(norm[[1]])
  pbar <- numeric(K)
  for (p in norm) for (a in seq_len(K)) pbar[a] <- pbar[a] + p[a] / m
  ent <- vapply(norm, ent1, 0)
  jsd <- ent1(pbar) - .s03mean(ent)
  list(estimate = jsd, jsd = jsd, entropies = ent, mean_entropy = .s03mean(ent),
       sd_entropy = if (m > 1L) .s03sd(ent, 1L) else NaN,
       range_entropy = max(ent) - min(ent), mean_policy = pbar, n = m,
       method = paste0("Self-consistency by Shannon entropy (1948 eq. 11) and ",
                       "Jensen-Shannon divergence (Lin 1991 eq. 3.1); the ",
                       "AlphaZero papers state no such statistic"))
}
