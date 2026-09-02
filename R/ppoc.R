# SPDX-License-Identifier: AGPL-3.0-or-later
#' PPO's clipped surrogate objective
#'
#' Schulman, Wolski, Dhariwal, Radford and Klimov (2017), Proximal policy
#' optimization algorithms, arXiv:1707.06347 (FETCHED as PDF), equation
#' (7): L^CLIP = E_t\[min(r_t A_t, clip(r_t, 1 - eps, 1 + eps) A_t)\] with
#' r_t = pi_theta(a_t|s_t) / pi_old(a_t|s_t), the paper taking the minimum
#' so the objective is a lower bound on the unclipped one.  Equation (9)
#' adds the value and entropy terms, L^(CLIP+VF+S) = E_t[L^CLIP - c1 L^VF
#' + c2 S], included when the value targets and entropy are supplied.  The
#' objective is maximised.
#'
#' @param env the advantages A_t.
#' @param policy the probability ratios r_t.
#' @param clip_eps the clipping range.
#' @param ratio,adv explicit ratios and advantages.
#' @param logp_new,logp_old log probabilities; ratio = exp(new - old).
#' @param v_pred,v_targ value predictions and targets.
#' @param entropy per-step policy entropy.
#' @param c1,c2 value and entropy coefficients.
#' @return list: estimate, l_clip, l_vf, l_entropy, total, frac_clipped,
#'   n, method.
#' @keywords internal
#' @examples
#' Ppoclip(c(1, -1), c(1.3, 0.8))$l_clip
#' @export
Ppoclip <- function(env, policy = NULL, clip_eps = 0.2, ratio = NULL,
                    adv = NULL, logp_new = NULL, logp_old = NULL,
                    v_pred = NULL, v_targ = NULL, entropy = NULL,
                    c1 = 0.5, c2 = 0.01) {
  a <- .s03vec(if (!is.null(adv)) adv else env)
  if (!is.null(ratio)) {
    r <- .s03vec(ratio)
  } else if (!is.null(logp_new) && !is.null(logp_old)) {
    r <- exp(.s03vec(logp_new) - .s03vec(logp_old))
  } else {
    r <- .s03vec(policy)
  }
  e <- as.numeric(clip_eps)
  n <- length(a)
  tot <- 0; nclip <- 0
  for (i in seq_len(n)) {
    un <- r[i] * a[i]
    cr <- if (r[i] < 1 - e) 1 - e else if (r[i] > 1 + e) 1 + e else r[i]
    cl <- cr * a[i]
    if (cl < un) { tot <- tot + cl; nclip <- nclip + 1 } else tot <- tot + un
  }
  lclip <- if (n) tot / n else NaN
  lvf <- NaN
  if (!is.null(v_pred) && !is.null(v_targ)) {
    vp <- .s03vec(v_pred); vt <- .s03vec(v_targ)
    s <- 0
    for (i in seq_along(vp)) s <- s + (vp[i] - vt[i])^2
    lvf <- if (length(vp)) s / length(vp) else NaN
  }
  lent <- if (!is.null(entropy)) .s03mean(.s03vec(entropy)) else NaN
  total <- lclip
  if (!is.nan(lvf)) total <- total - as.numeric(c1) * lvf
  if (!is.nan(lent)) total <- total + as.numeric(c2) * lent
  list(estimate = lclip, l_clip = lclip, l_vf = lvf, l_entropy = lent,
       total = total, frac_clipped = if (n) nclip / n else NaN, n = n,
       method = "PPO clipped surrogate objective (Schulman et al. 2017, eq. 7)")
}
