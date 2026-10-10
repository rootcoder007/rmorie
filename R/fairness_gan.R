# SPDX-License-Identifier: AGPL-3.0-or-later
#' Counterfactual fairness via GAN-style generative models
#'
#' R ports of the JAX GAN primitives in \code{morie.fairness.gan}.
#' Two callables: \code{morie_fairness_spatial_gan} learns a 2-D
#' coordinate distribution and samples synthetic points;
#' \code{morie_fairness_ctgan_debiaser} rebalances a tabular dataset
#' so every group's favourable-outcome rate matches a privileged
#' group's. Both run on a native base-R backend on every install: the
#' spatial generator is a small leaky-ReLU MLP fitted by a seeded
#' random-search (keep-if-better) loop, and the debiaser resamples
#' observed rows conditional on (group, outcome). Neither \pkg{torch} nor
#' \pkg{reticulate}/JAX is used.
#'
#' @name morie_fairness_gan
NULL


# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

#' .fairness_result
#'
#' A step of the fairness_gan implementation. Called by
#' \code{morie_fairness_ctgan_debiaser},
#' \code{morie_fairness_spatial_gan}.
#' See the file header for the source the module follows.
#'
#' @param title Carried through into a list the body builds.
#' @param call Carried through into a list the body builds.
#' @param summary_lines Carried through into a list the body builds.
#'   Defaults to \code{list()}.
#' @param warnings Carried through into a list the body builds.
#'   Defaults to \code{character(0)}.
#' @param interpretation Carried through into a list the body builds. Defaults to \code{""}.
#' @param ... Passed through.
#' @return The value of \code{out}, as built in the body.
#' @export
#' @keywords internal
.fairness_result <- function(title, call, summary_lines = list(),
                              warnings = character(0),
                              interpretation = "", ...) {
  out <- list(
    title = title, call = call, summary_lines = summary_lines,
    warnings = warnings, interpretation = interpretation, ...
  )
  class(out) <- c("morie_fairness_result", "morie_rich_result", "list")
  out
}

#' Backend descriptor for the fairness GAN callables
#'
#' A step of the fairness_gan implementation. Called by
#' \code{morie_fairness_ctgan_debiaser}, \code{morie_fairness_spatial_gan}.
#' The computation is base R, so the backend is always \code{"native"};
#' no deep-learning runtime is probed or required.
#'
#' @return A list with \code{kind} (\code{"native"}) and \code{note}.
#' @export
#' @examples
#' res <- .fairness_backend()
#' res
#' @keywords internal
.fairness_backend <- function() {
  list(kind = "native",
       note = "Using the native base-R backend (no torch / JAX needed).")
}

#' .fairness_he_init
#'
#' A step of the fairness_gan implementation. Called by \code{morie_fairness_spatial_gan}.
#' See the file header for the source the module follows.
#'
#' @param sizes A vector; its length is taken and its elements indexed.
#' @return The value of \code{params}, as built in the body.
#' @export
#' @keywords internal
.fairness_he_init <- function(sizes) {
  params <- vector("list", length(sizes) - 1L)
  for (i in seq_len(length(sizes) - 1L)) {
    w <- matrix(stats::rnorm(sizes[i] * sizes[i + 1L]),
                nrow = sizes[i], ncol = sizes[i + 1L])
    w <- w * sqrt(2.0 / sizes[i])
    params[[i]] <- list(W = w, b = rep(0.0, sizes[i + 1L]))
  }
  params
}

#' .fairness_mlp_forward
#'
#' A step of the fairness_gan implementation. Called by \code{morie_fairness_spatial_gan}.
#' See the file header for the source the module follows.
#'
#' @param params A vector; its length is taken and its elements indexed.
#' @param x A matrix; passed to \code{\%*\%}.
#' @return The value of \code{x}, as built in the body.
#' @export
#' @keywords internal
.fairness_mlp_forward <- function(params, x) {
  for (i in seq_along(params)) {
    x <- sweep(x %*% params[[i]]$W, 2L, params[[i]]$b, "+")
    if (i < length(params)) {
      x <- ifelse(x > 0, x, 0.2 * x)  # leaky relu
    }
  }
  x
}


# ---------------------------------------------------------------------------
# 1. Spatial GAN -- counterfactual location generator
# ---------------------------------------------------------------------------

#' Learn a 2-D crime/patrol location distribution
#'
#' Fits a small leaky-ReLU MLP generator to an (n, 2) matrix of
#' standardised coordinates and returns a fitted object that can
#' \code{sample()} synthetic points. Mirrors the API of the JAX
#' \code{SpatialGAN} class, but the fit is native base R: at most 50
#' seeded random-search steps of size \code{lr} on the generator weights,
#' each kept only when it lowers the minibatch squared error (no
#' adversarial discriminator training, no torch / JAX).
#'
#' @param points Numeric matrix or data.frame with two columns (x, y).
#' @param steps Integer training iterations requested (the native
#'   random-search fit runs at most 50).
#' @param batch_size Integer minibatch size.
#' @param latent_dim Generator noise dimension.
#' @param hidden Hidden-layer width.
#' @param lr Learning rate.
#' @param seed Reproducibility seed.
#' @return A \code{morie_fairness_result} with the fitted parameters in
#'   \code{$gp}, standardisation in \code{$mean}/\code{$std}, and a
#'   \code{$sample(n, seed)} closure; \code{$backend} is \code{"native"}.
#' @examples
#' set.seed(1)
#' pts <- matrix(rnorm(60), ncol = 2L)
#' r <- morie_fairness_spatial_gan(pts, steps = 5L, batch_size = 8L,
#'   latent_dim = 4L, hidden = 8L, seed = 1L)
#' r$backend
#' @export
morie_fairness_spatial_gan <- function(points, steps = 1500L,
                                        batch_size = 128L,
                                        latent_dim = 16L, hidden = 64L,
                                        lr = 2e-3, seed = 0L) {
  pts <- as.matrix(points)
  if (!is.numeric(pts) || ncol(pts) != 2L || nrow(pts) < 2L) {
    return(.fairness_result(
      "morie Spatial GAN",
      sprintf("morie_fairness_spatial_gan(points=<%dr x %dc>)",
              nrow(pts), ncol(pts)),
      warnings = "points must be an (n, 2) numeric matrix with n >= 2",
      interpretation = "No analysis: input shape is invalid.",
      fitted = FALSE
    ))
  }

  bk <- .fairness_backend()
  call_str <- sprintf("morie_fairness_spatial_gan(n=%d, steps=%d)",
                      nrow(pts), as.integer(steps))

  mu <- colMeans(pts)
  sigma <- apply(pts, 2L, stats::sd) + 1e-8
  std_pts <- sweep(sweep(pts, 2L, mu, "-"), 2L, sigma, "/")

  .rmorie_local_seed(as.integer(seed))
  gp <- .fairness_he_init(c(latent_dim, hidden, hidden, 2L))
  dp <- .fairness_he_init(c(2L, hidden, hidden, 1L))

  # Native base-R fit: a seeded random-search loop (at most 50 steps)
  # on the generator weights, keeping a perturbation only when it lowers
  # the minibatch squared error. The discriminator weights are
  # initialised for API parity with the JAX class but not trained.
  history <- numeric(0)
  for (t in seq_len(min(as.integer(steps), 50L))) {
    idx <- sample.int(nrow(std_pts),
                      min(as.integer(batch_size), nrow(std_pts)),
                      replace = TRUE)
    z <- matrix(stats::rnorm(length(idx) * latent_dim),
                nrow = length(idx), ncol = latent_dim)
    fake <- .fairness_mlp_forward(gp, z)
    loss <- mean((fake - std_pts[idx, , drop = FALSE])^2)
    # a random-perturbation step of size lr on the generator, kept when
    # it lowers the batch loss
    cand <- lapply(gp, function(layer) list(
      W = layer$W + as.numeric(lr) * matrix(stats::rnorm(length(layer$W)), nrow(layer$W)),
      b = layer$b + as.numeric(lr) * stats::rnorm(length(layer$b))))
    cand_loss <- mean((.fairness_mlp_forward(cand, z) - std_pts[idx, , drop = FALSE])^2)
    if (cand_loss < loss) {
      gp <- cand
      loss <- cand_loss
    }
    history <- c(history, loss)
  }

  sample_fn <- function(n, seed = NULL) {
    if (!is.null(seed)) .rmorie_local_seed(as.integer(seed))
    z <- matrix(stats::rnorm(as.integer(n) * latent_dim),
                nrow = as.integer(n), ncol = latent_dim)
    out <- .fairness_mlp_forward(gp, z)
    sweep(sweep(out, 2L, sigma, "*"), 2L, mu, "+")
  }

  interp <- sprintf(
    "A 2-D %s-backed GAN was initialised over %d training points (%d steps requested). The fitted object exposes $sample(n, seed) which draws synthetic coordinates in the original (un-standardised) space.",
    bk$kind, nrow(pts), as.integer(steps)
  )

  .fairness_result(
    "morie Spatial GAN", call_str,
    summary_lines = list(
      Backend = bk$kind,
      `Training points` = nrow(pts),
      `Latent dim` = as.integer(latent_dim),
      `Hidden width` = as.integer(hidden),
      `Steps requested` = as.integer(steps)
    ),
    interpretation = interp,
    backend = bk$kind, fitted = TRUE,
    gp = gp, mean = mu, std = sigma,
    history = history, sample = sample_fn,
    latent_dim = as.integer(latent_dim)
  )
}


# ---------------------------------------------------------------------------
# 2. CTGAN-style debiaser
# ---------------------------------------------------------------------------

#' Rebalance a biased tabular dataset by group
#'
#' A port of the CTGAN-style debiaser from
#' arXiv:2603.18987. Conditions a tabular GAN on (group, outcome) and
#' synthesises rows in which every group's favourable-outcome rate
#' matches the privileged group's, so the Disparate-Impact Ratio of
#' the debiased data moves toward 1.
#'
#' @param df Data.frame with at least the group, outcome and feature
#'   columns.
#' @param outcome_col Binary outcome column (favorable=1 default).
#' @param feature_cols Character vector of continuous feature columns.
#' @param group_col Protected-attribute column (default "group").
#' @param favorable The favourable outcome value (default 1).
#' @param privileged The group whose favourable rate is targeted.
#' @param n Number of synthetic rows to return.
#' @param steps Training iterations in the Python CTGAN; the native
#'   debiaser resamples rows rather than training a generator, so this is
#'   carried for interface parity and has no effect.
#' @param seed Sampling/training seed.
#' @return \code{morie_fairness_result}; \code{$debiased} carries the
#'   synthesised data.frame (native base-R resampling: each synthetic row
#'   draws its group from the observed group shares, its outcome at the
#'   privileged group's favourable rate, and its features from an observed
#'   row with that group and outcome).
#' @examples
#' df <- data.frame(group = c("A", "B"), outcome = c(1, 0), stringsAsFactors = FALSE)
#' r <- morie_fairness_ctgan_debiaser(df, outcome_col = "outcome",
#'   feature_cols = c("x1"), group_col = "group", n = 5L)
#' r$warnings
#' @export
morie_fairness_ctgan_debiaser <- function(df, outcome_col, feature_cols,
                                           group_col = "group",
                                           favorable = 1L,
                                           privileged = NULL,
                                           n = 1000L, steps = 1500L,
                                           seed = 0L) {
  stopifnot(is.data.frame(df))
  feature_cols <- as.character(feature_cols)
  warnings <- character(0)

  needed <- c(outcome_col, feature_cols, group_col)
  missing_cols <- setdiff(needed, names(df))
  if (length(missing_cols) > 0L) {
    return(.fairness_result(
      "morie CTGAN Debiaser",
      "morie_fairness_ctgan_debiaser(...)",
      warnings = sprintf("missing column(s): %s",
                         paste(missing_cols, collapse = ", ")),
      interpretation = "No analysis: required column(s) absent.",
      fitted = FALSE
    ))
  }
  if (length(feature_cols) == 0L) {
    return(.fairness_result(
      "morie CTGAN Debiaser",
      "morie_fairness_ctgan_debiaser(...)",
      warnings = "need at least one feature column",
      interpretation = "No analysis: feature_cols is empty.",
      fitted = FALSE
    ))
  }

  bk <- .fairness_backend()
  call_str <- sprintf(
    "morie_fairness_ctgan_debiaser(n=%d, group_col=%s, outcome_col=%s, k_feat=%d)",
    as.integer(n), group_col, outcome_col, length(feature_cols)
  )
  groups <- sort(unique(as.character(df[[group_col]])))
  ng <- length(groups)
  if (ng < 2L) {
    return(.fairness_result(
      "morie CTGAN Debiaser", call_str,
      warnings = "need at least two groups",
      interpretation = "No analysis: only one group present.",
      fitted = FALSE
    ))
  }
  if (is.null(privileged)) privileged <- groups[1L]
  if (!(privileged %in% groups)) {
    return(.fairness_result(
      "morie CTGAN Debiaser", call_str,
      warnings = sprintf("privileged group '%s' absent", privileged),
      interpretation = "No analysis: privileged group not seen in df.",
      fitted = FALSE
    ))
  }

  # Per-group favourable rate (the target for rebalancing).
  fav_mask <- df[[outcome_col]] == favorable
  group_vec <- as.character(df[[group_col]])
  fav_rate <- vapply(groups, function(g) {
    sel <- group_vec == g
    if (!any(sel)) 0.0 else mean(fav_mask[sel])
  }, numeric(1))
  names(fav_rate) <- groups
  target_rate <- fav_rate[[privileged]]
  group_props <- vapply(groups,
                        function(g) mean(group_vec == g),
                        numeric(1))
  names(group_props) <- groups

  # Native base-R debiaser: resample feature rows from the observed
  # conditional distribution P(features | group, outcome), with the
  # outcome drawn at the privileged group's favourable rate, so every
  # group's favourable rate matches the privileged one. No generator is
  # trained and no deep-learning runtime is used.
  .rmorie_local_seed(as.integer(seed))
  n_out <- as.integer(n)
  gi <- sample.int(ng, n_out, replace = TRUE, prob = group_props)
  oi <- as.integer(stats::runif(n_out) < target_rate)
  feats <- matrix(NA_real_, nrow = n_out, ncol = length(feature_cols))
  colnames(feats) <- feature_cols
  feat_mat <- as.matrix(df[, feature_cols, drop = FALSE])
  for (i in seq_len(n_out)) {
    grp <- groups[gi[i]]
    desired_outcome <- if (oi[i] == 1L) favorable else 0L
    pool <- which(group_vec == grp &
                  (df[[outcome_col]] == desired_outcome))
    if (length(pool) == 0L) {
      pool <- which(group_vec == grp)
    }
    if (length(pool) == 0L) {
      pool <- seq_len(nrow(df))
    }
    src <- pool[sample.int(length(pool), 1L)]
    feats[i, ] <- feat_mat[src, ]
  }
  debiased <- data.frame(
    .group = groups[gi],
    .outcome = ifelse(oi == 1L, favorable, 0L),
    feats,
    stringsAsFactors = FALSE
  )
  names(debiased)[1L:2L] <- c(group_col, outcome_col)

  interp <- sprintf(
    paste0(
      "Synthesised %d rows in which every group's favourable-outcom",
      "e rate is rebalanced to the privileged group ('%s', observed",
      " rate %.3f). Backend in use: %s. The debiased frame is in $d",
      "ebiased and is auditable with morie.fairness metrics; this r",
      "edistributes disparity but does not by itself remove structu",
      "ral bias."
    ),
    n_out, privileged, target_rate, bk$kind
  )

  .fairness_result(
    "morie CTGAN Debiaser", call_str,
    summary_lines = list(
      Backend = bk$kind,
      `Rows synthesised` = n_out,
      Groups = ng,
      `Privileged group` = privileged,
      `Target favourable rate` = target_rate
    ),
    warnings = warnings,
    interpretation = interp,
    backend = bk$kind, fitted = TRUE,
    groups = groups, group_fav_rate = fav_rate,
    target_rate = target_rate,
    debiased = debiased,
    value = target_rate
  )
}
