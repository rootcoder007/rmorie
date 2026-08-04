# SPDX-License-Identifier: AGPL-3.0-or-later
#' Knee-joint sound generation model, patellofemoral crepitus
#'
#' Rangayyan and Krishnan, Biomedical Signal Analysis, 3rd ed., Wiley-IEEE
#' Press, 2024, Section 7.7.3 "Modeling sound generation in knee joints",
#' pp. 402-404, after Beverland, Kernohan, McCoy and Mollan, "What is
#' physiological patellofemoral crepitus?", Proc. XIV Int. Conf. on Medical and
#' Biological Engineering, pp. 1249-1250, Espoo, 1985.
#'
#' "Beverland et al. studied the PPC signals produced during very slow movement
#' of the leg (at about 4 deg/s) ... Reproducible series of bursts of vibration
#' were recorded ... as the wheel in the model is slowly rotated clockwise
#' (representing extension), it would initially stick to the overlying patella
#' (hardboard) due to static friction ... A point would be reached where the
#' static friction would be overcome, when the patella would slip ... thereby
#' confirming the stick-slip frictional model for the generation of PPC
#' signals."
#'
#' Section 7.7.3 is entirely descriptive: the book prints no equation for this
#' model and none is invented here.  What the section does specify is the
#' structure of the signal the model generates, a reproducible train of
#' vibration bursts, one burst per slip event, separated by quiet stick
#' intervals, and that is what this function measures.  The stick/slip decision
#' is the model's own: the surface sticks while the tangential force stays
#' below the static-friction limit and slips once it exceeds it.  With the
#' short-time RMS envelope standing in for the tangential force and its median
#' standing in for the kinetic level, the sample is in slip when
#' envelope(n) > (mu_s / mu_k) * median(envelope), so the single parameter of
#' the detector is the static-to-kinetic friction ratio, the `force` argument.
#' This is stated explicitly because it is a reading: the section names the
#' mechanism, not a threshold.
#'
#' Ceiling of the median baseline: once slip occupies more than half the
#' record the median of the envelope falls inside the bursts, the threshold
#' rises with them, and no slip is reported.  That is the correct answer for
#' this model, since a permanently sliding surface has no stick-slip structure
#' and generates no burst train, but it does mean the detector is for the
#' sparse-burst regime of Figure 7.27 and not for continuous vibration.
#'
#' @param vag the vibration signal recorded over the patella.
#' @param fs sampling rate in Hz.
#' @param force the static-to-kinetic friction ratio mu_s / mu_k; at least 1.
#' @param window optional causal short-time RMS window in samples; ten
#'   milliseconds by default.
#' @return list: estimate, burst_rate, slip, n_bursts, onsets, offsets,
#'   intervals, mean_interval, duty, envelope, baseline, threshold,
#'   friction_ratio, window, n, fs, method.
#' @keywords internal
#' @examples
#' x <- rep(0.01, 60); x[c(11:15, 41:45)] <- 1
#' rgkneejt(x, 100, 1.5, window = 1)$n_bursts
#' @export
rgkneejt <- function(vag, fs, force = 1.5, window = NULL) {
  x <- as.numeric(vag)
  N <- length(x)
  if (N < 3L) stop("rangayyan_knee_joint_sound: need at least three samples")
  fsv <- as.numeric(fs)
  if (fsv <= 0) stop("rangayyan_knee_joint_sound: fs must be positive")
  ratio <- as.numeric(force)
  if (is.na(ratio) || !(ratio >= 1))
    stop("rangayyan_knee_joint_sound: the friction ratio must be at least 1")
  if (is.null(window)) {
    w <- as.integer(round(0.010 * fsv))   # 10 ms
    if (w < 1L) w <- 1L
  } else {
    w <- as.integer(window)
    if (w < 1L) stop("rangayyan_knee_joint_sound: window must be at least one sample")
  }
  if (w > N) w <- N

  env <- numeric(N)                        # causal short-time RMS, Sect. 5.6.1
  for (i in seq_len(N)) {
    lo <- max(1L, i - w + 1L)
    seg <- x[lo:i]
    env[i] <- sqrt(sum(seg * seg) / length(seg))
  }
  baseline <- .rgkneejt_median(env)
  thr <- ratio * baseline
  slip <- as.integer(env > thr)

  onsets <- integer(0); offsets <- integer(0)
  i <- 1L
  while (i <= N) {
    if (slip[i] == 1L) {
      j <- i
      while (j <= N && slip[j] == 1L) j <- j + 1L
      onsets <- c(onsets, i - 1L)          # zero-based, to match Python
      offsets <- c(offsets, j - 1L)
      i <- j
    } else i <- i + 1L
  }
  gaps <- if (length(onsets) > 1L) diff(onsets) / fsv else numeric(0)
  mean_gap <- if (length(gaps)) sum(gaps) / length(gaps) else NA_real_
  duty <- sum(slip) / N
  rate <- length(onsets) / (N / fsv)
  list(estimate = rate, burst_rate = rate, slip = slip,
       n_bursts = length(onsets), onsets = onsets, offsets = offsets,
       intervals = gaps, mean_interval = mean_gap, duty = duty,
       envelope = env, baseline = baseline, threshold = thr,
       friction_ratio = ratio, window = w, n = N, fs = fsv,
       method = "Rangayyan (2024) Sect. 7.7.3 pp.402-404, Beverland et al. (1985) stick-slip model; burst = a maximal run of the RMS envelope above mu_s/mu_k times its median")
}

.rgkneejt_median <- function(v) {
  s <- sort(v)
  m <- length(s)
  if (m == 0L) return(NA_real_)
  if (m %% 2L == 1L) s[(m + 1L) %/% 2L] else 0.5 * (s[m %/% 2L] + s[m %/% 2L + 1L])
}
