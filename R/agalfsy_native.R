# R arm of agalfsy -- the docking MDP of Wang et al. (2022).
# Wang, C., Chen, Y., Zhang, Y., Li, K., Lin, M., Pan, F., Wu, W. and
# Zhang, J. (2022) A reinforcement learning approach for protein-ligand
# binding pose prediction. BMC Bioinformatics 23, 368,
# doi:10.1186/s12859-022-04912-7.
# Scardino, Di Filippo and Cavasotto (2022) iScience 26(1), 105920,
# doi:10.1016/j.isci.2022.105920 -- the AlphaFold receptor caveat.
# Jumper et al. (2021) Nature 596, 583-589 -- the predicted receptor.
# Mirrors src/morie/fn/agalfsy.py.

.agalfsy_EPS <- 1e-12
.agalfsy_BOX <- 18.0
.agalfsy_TRANS <- 0.1
.agalfsy_ROT <- 1.0

#' .agalfsy_coords
#'
#' A step of the agalfsy_native implementation. Called by \code{morie_agalfsy_rl_pose_search}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param x A matrix; passed to \code{as.matrix}.
#' @param what See Usage.
#' @return The value of \code{m}, as built in the body.
#' @export
.agalfsy_coords <- function(x, what) {
  if (is.matrix(x)) m <- x
  else if (is.data.frame(x)) m <- as.matrix(x)
  else if (is.list(x)) m <- do.call(rbind, lapply(x, as.numeric))
  else m <- matrix(as.numeric(x), ncol = 3L, byrow = TRUE)
  storage.mode(m) <- "double"
  if (ncol(m) != 3L)
    stop(sprintf("%s: each atom needs exactly x, y, z", what))
  if (nrow(m) == 0L) stop(sprintf("%s: no atoms", what))
  m
}

#' .agalfsy_centroid
#'
#' A step of the agalfsy_native implementation. Called by \code{.agalfsy_rotate}, \code{morie_agalfsy_rl_pose_search}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param P A matrix; passed to \code{nrow}.
#' @return A numeric value.
#' @export
.agalfsy_centroid <- function(P) colSums(P) / nrow(P)

# Same atom order, no superposition: the ligand moves rigidly so the
# correspondence is fixed, and fitting it away would hide the very
# displacement the reward is scoring.
#' Same atom order, no superposition: the ligand moves rigidly so the
#'
#' correspondence is fixed, and fitting it away would hide the very
#' displacement the reward is scoring.
#'
#' @param A A matrix; indexed by row and column.
#' @param B A matrix; indexed by row and column.
#' @return A numeric value.
#' @export
.agalfsy_rmsd <- function(A, B) {
  n <- nrow(A)
  s <- 0.0
  for (i in seq_len(n)) for (a in 1:3) {
    d <- A[i, a] - B[i, a]
    s <- s + d * d
  }
  sqrt(s / n)
}

#' .agalfsy_rotate
#'
#' A step of the agalfsy_native implementation. Called by \code{.agalfsy_apply}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param P A matrix; indexed by row and column.
#' @param axis See Usage.
#' @param deg Numeric; combined arithmetically in the body.
#' @return The value of \code{out}, as built in the body.
#' @export
.agalfsy_rotate <- function(P, axis, deg) {
  cen <- .agalfsy_centroid(P)
  t <- deg * pi / 180.0
  ct <- cos(t); st <- sin(t)
  out <- P
  for (i in seq_len(nrow(P))) {
    x <- P[i, 1] - cen[1]; y <- P[i, 2] - cen[2]; z <- P[i, 3] - cen[3]
    if (axis == 0L) {
      ny <- ct * y - st * z; nz <- st * y + ct * z; y <- ny; z <- nz
    } else if (axis == 1L) {
      nx <- ct * x + st * z; nz <- -st * x + ct * z; x <- nx; z <- nz
    } else {
      nx <- ct * x - st * y; ny <- st * x + ct * y; x <- nx; y <- ny
    }
    out[i, ] <- c(x + cen[1], y + cen[2], z + cen[3])
  }
  out
}

#' .agalfsy_translate
#'
#' A step of the agalfsy_native implementation. Called by \code{.agalfsy_apply}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param P A matrix; indexed by row and column.
#' @param axis Numeric; combined arithmetically in the body.
#' @param step Numeric; combined arithmetically in the body.
#' @return The value of \code{P}, as built in the body.
#' @export
.agalfsy_translate <- function(P, axis, step) {
  P[, axis + 1L] <- P[, axis + 1L] + step
  P
}

# The twelve actions: 0-5 translations, 6-11 rotations. Indexing stays
# 0-based here to match the Python arm's action numbering, which is a
# REPORTED quantity (it appears in the trajectory).
#' The twelve actions: 0-5 translations, 6-11 rotations. Indexing stays
#'
#' 0-based here to match the Python arm\'s action numbering, which is a
#' REPORTED quantity (it appears in the trajectory).
#'
#' @param P Passed to \code{.agalfsy_translate}.
#' @param a Numeric; combined arithmetically in the body.
#' @return The value of \code{.agalfsy_rotate}.
#' @export
.agalfsy_apply <- function(P, a) {
  if (a < 6L)
    return(.agalfsy_translate(P, a %/% 2L,
                              if (a %% 2L == 0L) .agalfsy_TRANS
                              else -.agalfsy_TRANS))
  a <- a - 6L
  .agalfsy_rotate(P, a %/% 2L,
                  if (a %% 2L == 0L) .agalfsy_ROT else -.agalfsy_ROT)
}

#' .agalfsy_reward
#'
#' A step of the agalfsy_native implementation. Called by \code{morie_agalfsy_rl_pose_search}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param site Passed to \code{.agalfsy_rmsd}.
#' @param before Passed to \code{.agalfsy_rmsd}.
#' @param after Passed to \code{.agalfsy_rmsd}.
#' @return One of two values, depending on the branch taken.
#' @export
.agalfsy_reward <- function(site, before, after) {
  r <- exp(-.agalfsy_rmsd(site, after) / .agalfsy_BOX) -
    exp(-.agalfsy_rmsd(site, before) / .agalfsy_BOX)
  if (r < 0.0) 2.0 * r else r
}

#' morie_agalfsy_rl_pose_search
#'
#' A step of the agalfsy_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param receptor Passed to \code{.agalfsy_coords}.
#' @param ligand Passed to \code{.agalfsy_coords}.
#' @param site Optional; may be \code{NULL}. Passed to \code{.agalfsy_coords}.
#' @param policy Defaults to \code{NULL}.
#' @param critic Defaults to \code{NULL}.
#' @param max_steps Coerced to integer by the body, with \code{as.integer}. Defaults to \code{600L}.
#' @param min_steps Coerced to integer by the body, with \code{as.integer}. Defaults to \code{300L}.
#' @param window Coerced to integer by the body, with \code{as.integer}. Defaults to \code{50L}.
#' @param tol Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.3}.
#' @param box Numeric; combined arithmetically in the body. Defaults to \code{18}.
#' @param seed Defaults to \code{2}.
#' @return A list with \code{estimate}, \code{pose}, \code{rmsd}, \code{rmsd_start}, \code{dcc}, \code{success}, \code{improved}, \code{steps}, \code{stop_reason}, \code{reward_total}, \code{trajectory}, \code{policy_kind}, \code{n_actions}, \code{translation_step}, \code{rotation_step_deg}, \code{box}, \code{method}, \code{note}.
#' @export
morie_agalfsy_rl_pose_search <- function(receptor, ligand, site = NULL,
                                         policy = NULL, critic = NULL,
                                         max_steps = 600L, min_steps = 300L,
                                         window = 50L, tol = 0.3,
                                         box = 18.0, seed = 2) {
  R <- .agalfsy_coords(receptor, "agalfsy receptor")
  L0 <- .agalfsy_coords(ligand, "agalfsy ligand")
  S <- if (!is.null(site)) .agalfsy_coords(site, "agalfsy site") else NULL
  if (!is.null(S) && nrow(S) != nrow(L0))
    stop(sprintf(paste0("agalfsy: the site pose has %d atoms and the ligand ",
                        "%d -- the reward is an RMSD over matched atoms"),
                 nrow(S), nrow(L0)))
  if (is.null(policy) && is.null(S))
    stop(paste0("agalfsy: with no policy the search falls back to the greedy ",
                "oracle, which maximises the published reward and therefore ",
                "needs `site`. Supply a trained policy to run blind."))
  box <- as.numeric(box)
  if (!(box > 0.0)) stop("agalfsy: box must be positive")

  kind <- if (!is.null(policy)) "supplied policy"
          else "greedy oracle (needs the answer; benchmark only)"
  start_c <- .agalfsy_centroid(L0)
  L <- L0
  traj <- list()
  crit_hist <- numeric(0)
  total <- 0.0
  stop_reason <- "max_steps"
  step <- 0L
  for (step in seq_len(as.integer(max_steps))) {
    if (!is.null(policy)) {
      a <- as.integer(policy(L, R, step))
      if (!(a >= 0L && a < 12L))
        stop(sprintf(paste0("agalfsy: policy returned action %d, the action ",
                            "space is 0..11"), a))
      nxt <- .agalfsy_apply(L, a)
    } else {
      best_a <- 0L; best_r <- NULL; nxt <- NULL
      for (cand in 0:11) {
        trial <- .agalfsy_apply(L, as.integer(cand))
        r <- .agalfsy_reward(S, L, trial)
        if (is.null(best_r) || r > best_r) {
          best_a <- as.integer(cand); best_r <- r; nxt <- trial
        }
      }
      a <- best_a
    }
    r <- if (!is.null(S)) .agalfsy_reward(S, L, nxt) else 0.0
    total <- total + r
    L <- nxt
    traj[[length(traj) + 1L]] <- c(step, a, r)

    cen <- .agalfsy_centroid(L)
    if (max(abs(cen - start_c)) > box / 2.0) {
      stop_reason <- "left_box"
      break
    }

    cv <- if (!is.null(critic)) as.numeric(critic(L, R, step))
          else if (!is.null(S)) .agalfsy_rmsd(S, L) else 0.0
    crit_hist <- c(crit_hist, cv)
    if (step >= as.integer(min_steps) &&
        length(crit_hist) >= as.integer(window)) {
      w <- crit_hist[seq.int(length(crit_hist) - as.integer(window) + 1L,
                             length(crit_hist))]
      if ((max(w) - min(w)) < as.numeric(tol)) {
        stop_reason <- "critic_stabilised"
        break
      }
    }
  }

  final_rmsd <- if (!is.null(S)) .agalfsy_rmsd(S, L) else NA_real_
  start_rmsd <- if (!is.null(S)) .agalfsy_rmsd(S, L0) else NA_real_
  dcc <- if (!is.null(S))
    sqrt(sum((.agalfsy_centroid(L) - .agalfsy_centroid(S))^2)) else NA_real_

  list(estimate = final_rmsd,
       pose = L,
       rmsd = final_rmsd,
       rmsd_start = start_rmsd,
       dcc = dcc,
       success = if (!is.null(S)) (dcc < 4.0) else NULL,
       improved = if (!is.null(S)) (final_rmsd < start_rmsd) else NULL,
       steps = as.integer(step),
       stop_reason = stop_reason,
       reward_total = total,
       trajectory = do.call(rbind, traj),
       policy_kind = kind,
       n_actions = 12L,
       translation_step = .agalfsy_TRANS,
       rotation_step_deg = .agalfsy_ROT,
       box = box,
       method = paste0("A3C docking MDP of Wang et al. (2022): 12 discrete ",
                       "actions (six 0.1 A translations, six 1 degree ",
                       "rotations), reward exp(-d/18) differenced and ",
                       "negatives doubled, stopping when the critic range ",
                       "falls below 0.3 over 50 steps after at least 300"),
       note = paste0("policy_kind is the first thing to read. The greedy ",
                     "fallback maximises the published reward, which is a ",
                     "function of the true site, so its rmsd is an upper ",
                     "bound on what a trained agent could do and not a ",
                     "docking prediction. Separately, a rigid AlphaFold ",
                     "receptor is a weak basis for screening even when the ",
                     "pose search is exact: Scardino et al. (2022) report a ",
                     "mean enrichment factor at 1% of 8.8 against 20.5 for ",
                     "experimental structures, several targets enriching ",
                     "not at all."))
}

#' .agalfsy_cheatsheet
#'
#' A step of the agalfsy_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @return A character value.
#' @export
.agalfsy_cheatsheet <- function() {
  paste0("agalfsy: morie_agalfsy_rl_pose_search(receptor, ligand, site) -> ",
         "ligand pose by the A3C docking MDP of Wang et al. (2022), ",
         "BMC Bioinf 23:368")
}

morie_agalfsy <- morie_agalfsy_rl_pose_search
