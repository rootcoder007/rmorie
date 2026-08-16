# morie native arm -- timesfm
#
# TimesFM: a decoder-only foundation model with input patching.
#
# The history is cut into patches, each patch becomes a token, and a
# causal decoder attends over them. Decoder-only rather than
# encoder-decoder because forecasting is autoregressive over patches:
# a sequence of N patches supplies N training signals, not one.
#
# The design decision with real consequences is the asymmetry between
# the input patch p and the output patch q. A horizon H needs
# ceil(H/q) autoregressive steps, not ceil(H/p), so q > p cuts the
# number of generation steps and with it the error accumulation that
# makes long rollouts drift. q = p recovers the symmetric case and
# q >= H makes the forecast a single direct prediction -- all three
# reachable from one model, which is what lets one checkpoint serve
# many horizons.
#
# What is implemented here is the patching contract and the rollout
# arithmetic -- architecture, not weights. rollout takes a predictor
# function so the accounting is checkable exactly without a 200M
# parameter checkpoint.
#
# Das, A., Kong, W., Sen, R. & Zhou, Y. (2024) "A decoder-only
# foundation model for time-series forecasting", Proceedings of the
# 41st International Conference on Machine Learning, PMLR 235,
# arXiv:2310.10688.
# Nie, Y., Nguyen, N. H., Sinthong, P. & Kalagnanam, J. (2023) "A Time
# Series is Worth 64 Words: Long-term Forecasting with Transformers",
# ICLR 2023, arXiv:2211.14730 -- the patching idea, used here with the
# input/output asymmetry added.

#' morie_timesfm_input_patches
#'
#' A step of the timesfm_native implementation. Called by \code{morie_timesfm}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param x See Usage.
#' @param patch_len See Usage.
#' @param pad_value Defaults to \code{0}.
#' @return A list with \code{patches}, \code{n_patches}, \code{patch_len}, \code{n_padded}, \code{L}, \code{note}.
#' @export
morie_timesfm_input_patches <- function(x, patch_len, pad_value = 0) {
  v <- as.numeric(unlist(x))
  p <- as.integer(patch_len)
  if (p < 1L) stop("timesfm: patch_len must be at least 1")
  if (length(v) == 0L) stop("timesfm: the history is empty")
  rem <- length(v) %% p
  pad <- (p - rem) %% p
  padded <- c(rep(as.numeric(pad_value), pad), v)
  n <- length(padded) %/% p
  patches <- lapply(seq_len(n), function(i) {
    padded[((i - 1L) * p + 1L):(i * p)]
  })
  list(patches = patches, n_patches = n, patch_len = p, n_padded = pad,
       L = length(v),
       note = paste("padded on the LEFT so the newest point ends the",
                    "final patch"))
}

#' morie_timesfm_causal_mask
#'
#' A step of the timesfm_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param n_patches See Usage.
#' @return A list with \code{mask}, \code{n_patches}, \code{training_signals}.
#' @export
morie_timesfm_causal_mask <- function(n_patches) {
  n <- as.integer(n_patches)
  if (n < 1L) stop("timesfm: need at least one patch")
  m <- matrix(0, nrow = n, ncol = n)
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      if (j <= i) m[i, j] <- 1
    }
  }
  list(mask = m, n_patches = n, training_signals = n)
}

#' morie_timesfm_rollout_steps
#'
#' A step of the timesfm_native implementation. Called by \code{morie_timesfm}, \code{morie_timesfm_horizon_plan}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param horizon See Usage.
#' @param output_patch_len See Usage.
#' @return A list with \code{steps}, \code{horizon}, \code{output_patch_len}, \code{single_step}.
#' @export
morie_timesfm_rollout_steps <- function(horizon, output_patch_len) {
  H <- as.integer(horizon)
  q <- as.integer(output_patch_len)
  if (H < 1L) stop("timesfm: the horizon must be at least 1")
  if (q < 1L) stop("timesfm: output_patch_len must be at least 1")
  list(steps = as.integer(ceiling(H / q)), horizon = H,
       output_patch_len = q, single_step = q >= H)
}

#' morie_timesfm_horizon_plan
#'
#' A step of the timesfm_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param horizon See Usage.
#' @param input_patch_len See Usage.
#' @param output_patch_len See Usage.
#' @return A list with \code{steps_asymmetric}, \code{steps_symmetric}, \code{steps_direct}, \code{input_patch_len}, \code{output_patch_len}, \code{horizon}, \code{speedup_vs_symmetric}, \code{note}.
#' @export
morie_timesfm_horizon_plan <- function(horizon, input_patch_len,
                                       output_patch_len) {
  H <- as.integer(horizon)
  p <- as.integer(input_patch_len)
  q <- as.integer(output_patch_len)
  sa <- morie_timesfm_rollout_steps(H, q)$steps
  ss <- morie_timesfm_rollout_steps(H, p)$steps
  list(steps_asymmetric = sa, steps_symmetric = ss,
       steps_direct = morie_timesfm_rollout_steps(H, H)$steps,
       input_patch_len = p, output_patch_len = q, horizon = H,
       speedup_vs_symmetric = ss / sa,
       note = paste("q > p cuts generation steps; q >= H makes the",
                    "forecast a single direct prediction"))
}

#' morie_timesfm
#'
#' A step of the timesfm_native implementation. Called by \code{morie_timesf}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param history See Usage.
#' @param predictor See Usage.
#' @param horizon Passed to \code{morie_timesfm_rollout_steps}.
#' @param input_patch_len See Usage.
#' @param output_patch_len See Usage.
#' @return A list with \code{estimate}, \code{forecast}, \code{steps}, \code{horizon}, \code{input_patch_len}, \code{output_patch_len}, \code{context_grew_to}, \code{method}.
#' @export
morie_timesfm <- function(history, predictor, horizon, input_patch_len,
                          output_patch_len) {
  v <- as.numeric(unlist(history))
  p <- as.integer(input_patch_len)
  q <- as.integer(output_patch_len)
  plan <- morie_timesfm_rollout_steps(horizon, q)
  out <- numeric(0)
  ctx <- v
  for (s in seq_len(plan$steps)) {
    pat <- morie_timesfm_input_patches(ctx, p)$patches
    nxt <- as.numeric(predictor(pat))
    if (length(nxt) != q) {
      stop(sprintf(paste("timesfm: the predictor returned %d values but",
                         "output_patch_len is %d"),
                   length(nxt), q))
    }
    out <- c(out, nxt)
    ctx <- c(ctx, nxt)
  }
  H <- as.integer(horizon)
  list(estimate = out[seq_len(H)], forecast = out[seq_len(H)],
       steps = plan$steps, horizon = H,
       input_patch_len = p, output_patch_len = q,
       context_grew_to = length(ctx),
       method = paste("decoder-only patched rollout; Das, Kong, Sen &",
                      "Zhou (2024)"))
}

#' morie_timesfm_cheatsheet
#'
#' A step of the timesfm_native implementation. Called by \code{.timesf_cheatsheet}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @return A character value.
#' @export
morie_timesfm_cheatsheet <- function() {
  paste("timesfm: decoder-only + input patching. Causal attention",
        "over patches means N patches give N training signals,",
        "not one. The design choice that matters: the OUTPUT",
        "patch may be LONGER than the input patch, so a horizon H",
        "needs ceil(H/q) generation steps rather than ceil(H/p)",
        "-- fewer rollouts, less accumulated drift. q >= H is a",
        "single direct prediction. 200M parameters and O(100B)",
        "timepoints beats prompting a large language model, at a",
        "fraction of the cost.")
}
