# MOMENT: masked time-series modelling across many datasets.
# Sources: Goswami, M. et al. (2024), "MOMENT: A Family of Open
# Time-series Foundation Models", ICML 2024, arXiv:2402.03885;
# Devlin, J. et al. (2019), "BERT", arXiv:1810.04805; Nie, Y. et al.
# (2023), "A Time Series is Worth 64 Words", arXiv:2211.14730.
#
# Native implementation mirroring Python morie.fn.momento exactly:
# per-series normalisation, truncation to a whole number of patches,
# independent channels, mask-with-zeros, and MSE on the masked
# positions only.

.GHC_MOM_EPS <- 1e-12
.GHC_MOM_TASKS <- c("forecast", "impute", "classify", "anomaly")

#' Harmonise a list of series into one batch
#'
#' Per-series normalisation, truncation to a whole number of patches,
#' channels kept independent.
#'
#' @param series_list A list of series (each a list of numeric rows,
#'   i.e. a time x channel matrix in row form).
#' @param patch_len Patch length.
#' @param normalise Apply per-series mean/sd normalisation.
#' @return A list with batch, meta, n_series, n_patches, patch_len,
#'   note.
#' @export
#' @examples
#' set.seed(1)
#' series_list <- list(rnorm(20), rnorm(24), rnorm(18))
#' morie_momento_harmonise(series_list, patch_len = 4)
#' @keywords internal
morie_momento_harmonise <- function(series_list, patch_len,
                                     normalise = TRUE) {
  P <- as.integer(patch_len)
  if (P < 1L) stop("momento: patch_len must be at least 1")
  if (length(series_list) == 0L) stop("momento: no series given")
  out <- list()
  meta <- list()
  for (s in series_list) {
    M <- as.matrix(s)
    if (nrow(M) == 0L) stop("momento: one of the series is empty")
    storage.mode(M) <- "double"
    D <- ncol(M)
    L <- (nrow(M) %/% P) * P
    if (L < P)
      stop(paste0("momento: a series has ", nrow(M), " points, fewer ",
                  "than one patch of ", P))
    for (d in seq_len(D)) {
      col <- M[seq_len(L), d]
      if (normalise) {
        m <- sum(col) / L
        sd <- sqrt(sum((col - m)^2) / max(L - 1, 1))
        if (sd <= .GHC_MOM_EPS) col <- rep(0, L)
        else col <- (col - m) / sd
      } else {
        m <- 0
        sd <- 1
      }
      np <- L %/% P
      patches <- vector("list", np)
      for (i in seq_len(np))
        patches[[i]] <- col[((i - 1L) * P + 1L):(i * P)]
      out[[length(out) + 1L]] <- patches
      meta[[length(meta) + 1L]] <- list(mean = m, sd = sd,
                                        n_patches = np)
    }
  }
  n <- min(vapply(meta, function(x) x$n_patches, integer(1)))
  for (i in seq_along(out)) out[[i]] <- out[[i]][seq_len(n)]
  list(batch = out, meta = meta, n_series = length(out),
       n_patches = n, patch_len = P,
       note = paste0("each channel is its own row, so datasets with ",
                     "different channel counts share a batch"))
}

#' Mask named patches with a fill value
#'
#' @param patches A list of numeric patches.
#' @param mask_idx Integer indices of patches to mask.
#' @param fill Fill value.
#' @return A list with masked, mask, mask_idx, mask_rate, n_patches.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
#' morie_momento_mask_patches(V, M)
#' @keywords internal
morie_momento_mask_patches <- function(patches, mask_idx, fill = 0) {
  P <- lapply(patches, as.numeric)
  n <- length(P)
  idx <- sort(unique(as.integer(mask_idx)))
  if (any(idx < 0 | idx >= n))
    stop(paste0("momento: a mask index is outside 0..", n - 1L))
  if (length(idx) == 0L)
    stop("momento: nothing was masked, so there is nothing to learn from")
  if (length(idx) == n)
    stop(paste0("momento: every patch was masked, leaving no ",
                "context to reconstruct from"))
  masked <- vector("list", n)
  for (i in seq_len(n))
    masked[[i]] <- if (i %in% (idx + 1L)) rep(as.numeric(fill), length(P[[i]]))
              else P[[i]]
  list(masked = masked,
       mask = seq_len(n) %in% (idx + 1L),
       mask_idx = idx,
       mask_rate = length(idx) / n,
       n_patches = n)
}

#' Masked-position MSE
#'
#' @param truth,reconstruction Patch lists.
#' @param mask Boolean vector of length n_patches.
#' @return A list with mse, n_scored, scored.
#' @export
#' @examples
#' morie_momento_masked_loss(truth = c(1, 2, 3, 4, 5, 6, 7, 8),
#'   reconstruction = c(1, 2, 3, 4, 5, 6, 7, 8), mask = c(1, 2, 3, 4, 5, 6, 7, 8))
#' @keywords internal
morie_momento_masked_loss <- function(truth, reconstruction, mask) {
  T <- lapply(truth, as.numeric)
  R <- lapply(reconstruction, as.numeric)
  if (length(T) != length(R) || length(T) != length(mask))
    stop(paste0("momento: truth, reconstruction and mask must agree ",
                "in length (", length(T), ", ", length(R), ", ",
                length(mask), ")"))
  tot <- 0
  cnt <- 0
  for (i in seq_along(T)) {
    if (!mask[i]) next
    if (length(T[[i]]) != length(R[[i]]))
      stop(paste0("momento: patch ", i - 1L, " differs in length ",
                  "between truth and reconstruction"))
    for (j in seq_along(T[[i]])) {
      tot <- tot + (T[[i]][j] - R[[i]][j])^2
      cnt <- cnt + 1L
    }
  }
  if (cnt == 0L)
    stop("momento: no position was masked, so the loss is undefined")
  list(mse = tot / cnt, n_scored = cnt,
       scored = paste0("masked positions only -- scoring the visible ",
                       "ones would reward copying"))
}

#' Construct a task-appropriate mask
#'
#' @param n_patches Number of patches.
#' @param task One of forecast, impute, classify, anomaly.
#' @param span Span of the mask.
#' @param start Starting patch (for impute).
#' @return Integer vector of mask indices.
#' @export
#' @examples
#' morie_momento_task_mask(n_patches = 5L)
#' @keywords internal
morie_momento_task_mask <- function(n_patches, task = "forecast",
                                     span = 1, start = NULL) {
  n <- as.integer(n_patches)
  s <- as.integer(span)
  if (!(task %in% .GHC_MOM_TASKS))
    stop(paste0("momento: task must be one of ",
                paste(.GHC_MOM_TASKS, collapse = ", "), ", got ",
                task))
  if (s < 1L || s >= n)
    stop(paste0("momento: the span must lie in 1..", n - 1L,
                ", got ", s))
  if (task == "forecast") return(seq.int(n - s, n - 1L))
  if (task == "impute") {
    st <- if (is.null(start)) max(1L, (n - s) %/% 2L) else as.integer(start)
    if (st + s > n)
      stop("momento: the imputation gap runs past the end")
    return(seq.int(st, st + s - 1L))
  }
  seq.int(n - s, n - 1L)
}

#' Reconstruction error vs mask rate
#'
#' @param patches Patch list.
#' @param reconstructor Function (masked, mask) -> reconstruction.
#' @param rates Sequence of mask rates in (0, 1).
#' @param seed Seed for the shared generator.
#' @return A list with curve, n_patches, rates, mse.
#' @export
#' @examples
#' set.seed(1)
#' patches <- lapply(1:6, function(i) rnorm(8))
#' recon <- function(masked, mask) masked
#' morie_momento_reconstruction_curve(patches, recon, rates = c(0.2, 0.5, 0.8))
#' @keywords internal
morie_momento_reconstruction_curve <- function(patches, reconstructor,
                                                rates, seed = 0) {
  P <- lapply(patches, as.numeric)
  n <- length(P)
  e <- .ghc_rng(seed)
  out <- list()
  for (rr in rates) {
    m <- max(1L, min(n - 1L, as.integer(round(as.numeric(rr) * n))))
    u <- .ghc_unif(e, n)
    ord <- order(u)
    idx <- ord[seq_len(m)] - 1L
    mk <- morie_momento_mask_patches(P, idx)
    rec <- reconstructor(mk$masked, mk$mask)
    L <- morie_momento_masked_loss(P, rec, mk$mask)
    out[[length(out) + 1L]] <- list(rate = mk$mask_rate, mse = L$mse,
                                     n_masked = m)
  }
  list(curve = out, n_patches = n,
       rates = vapply(out, function(o) o$rate, numeric(1)),
       mse = vapply(out, function(o) o$mse, numeric(1)))
}

morie_momento <- morie_momento_harmonise

# -- restored: morie-only definition kept through the rmorie sync --
#' momento_cheatsheet
#'
#' A step of the momento_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @return A character value.
#' @export
momento_cheatsheet <- function() {
  paste(paste0(
    "momento: masked time-series pretraining. Mask patches with Z",
    "EROS and reconstruct; the loss counts the MASKED positions o",
    "nly, since scoring visible ones rewards copying. The hard pa",
    "rt is multi-dataset pretraining: series differ in resolution",
    ", channel count, length and amplitude, so harmonise per-seri",
    "es and keep channels independent. Mask rate is a real knob -",
    "- too low is interpolation, too high leaves no context. Task",
    " changes only WHERE the mask goes: tail for forecasting, int",
    "erior for imputation."
  ))
}

# -- restored: morie-only definition kept through the rmorie sync --
#' momento_harmonise
#'
#' A step of the momento_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param series_list A vector; its length is taken.
#' @param patch_len Coerced to integer by the body, with \code{as.integer}.
#' @param normalise A flag; the body branches on it. Defaults to \code{TRUE}.
#' @return A list with \code{batch}, \code{meta}, \code{n_series}, \code{n_patches},
#' \code{patch_len}, \code{note}.
#' @export
momento_harmonise <- function(series_list, patch_len, normalise = TRUE) {
  P <- as.integer(patch_len)
  if (P < 1L) stop("momento: patch_len must be at least 1")
  if (length(series_list) == 0L) stop("momento: no series given")
  out <- list()
  meta <- list()
  for (s in series_list) {
    M <- as.matrix(s)
    if (nrow(M) == 0L || ncol(M) == 0L) {
      stop("momento: one of the series is empty")
    }
    D <- ncol(M)
    L <- (nrow(M) %/% P) * P
    if (L < P) {
      stop(sprintf("momento: a series has %d points, fewer than one patch of %d",
                   nrow(M), P))
    }
    for (d in seq_len(D)) {
      col <- M[seq_len(L), d]
      if (normalise) {
        m <- mean(col)
        if (L > 1L) {
          sd_ <- sqrt(sum((col - m)^2) / (L - 1))
        } else {
          sd_ <- 0
        }
        if (sd_ <= 1e-12) {
          col <- rep(0, L)
        } else {
          col <- (col - m) / sd_
        }
      } else {
        m <- 0
        sd_ <- 1
      }
      patches <- split(col, (seq_along(col) - 1L) %/% P)
      out[[length(out) + 1L]] <- patches
      meta[[length(meta) + 1L]] <- list(mean = m, sd = sd_, n_patches = L %/% P)
    }
  }
  n <- min(vapply(meta, function(x) x$n_patches, integer(1)))
  batch <- lapply(out, function(row) row[seq_len(n)])
  list(batch = batch, meta = meta, n_series = length(out),
       n_patches = n, patch_len = P,
       note = "each channel is its own row, so datasets with different channel counts share a batch")
}

# -- restored: morie-only definition kept through the rmorie sync --
#' momento_masked_loss
#'
#' A step of the momento_native implementation. Called by \code{momento_reconstruction_curve}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param truth A vector; its length is taken and its elements indexed.
#' @param reconstruction A vector; its length is taken and its elements indexed.
#' @param mask A vector; its length is taken and its elements indexed.
#' @return A list with \code{mse}, \code{n_scored}, \code{scored}.
#' @export
momento_masked_loss <- function(truth, reconstruction, mask) {
  n <- length(truth)
  if (length(reconstruction) != n || length(mask) != n) {
    stop(sprintf("momento: truth, reconstruction and mask must agree in length (%d, %d, %d)",
                 n, length(reconstruction), length(mask)))
  }
  tot <- 0
  cnt <- 0
  for (i in seq_len(n)) {
    if (!mask[[i]]) next
    if (length(truth[[i]]) != length(reconstruction[[i]])) {
      stop(sprintf("momento: patch %d differs in length between truth and reconstruction", i))
    }
    d <- truth[[i]] - reconstruction[[i]]
    tot <- tot + sum(d * d)
    cnt <- cnt + length(d)
  }
  if (cnt == 0L) {
    stop("momento: no position was masked, so the loss is undefined")
  }
  list(mse = tot / cnt, n_scored = cnt,
       scored = "masked positions only -- scoring the visible ones would reward copying")
}

# -- restored: morie-only definition kept through the rmorie sync --
#' momento_mask_patches
#'
#' A step of the momento_native implementation. Called by \code{momento_reconstruction_curve}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param patches A vector; its length is taken and its elements indexed.
#' @param mask_idx Coerced to integer by the body, with \code{as.integer}.
#' @param fill A count; the body uses it as \code{rep(...)}. Defaults to \code{0}.
#' @return A list with \code{masked}, \code{mask}, \code{mask_idx}, \code{mask_rate},
#' \code{n_patches}.
#' @export
momento_mask_patches <- function(patches, mask_idx, fill = 0) {
  n <- length(patches)
  idx <- sort(unique(as.integer(mask_idx)))
  if (any(idx < 0L | idx >= n)) {
    stop(sprintf("momento: a mask index is outside 0..%d", n - 1L))
  }
  if (length(idx) == 0L) {
    stop("momento: nothing was masked, so there is nothing to learn from")
  }
  if (length(idx) == n) {
    stop("momento: every patch was masked, leaving no context to reconstruct from")
  }
  masked <- lapply(seq_len(n), function(i) {
    if (i %in% idx) rep(fill, length(patches[[i]])) else as.numeric(patches[[i]])
  })
  list(masked = masked,
       mask = seq_len(n) %in% idx,
       mask_idx = idx,
       mask_rate = length(idx) / n,
       n_patches = n)
}

# -- restored: morie-only definition kept through the rmorie sync --
#' momento_reconstruction_curve
#'
#' A step of the momento_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param patches Iterated over elementwise, with \code{lapply}.
#' @param reconstructor Accepted by the signature and not used anywhere in the body.
#' @param rates See Usage.
#' @param seed Passed to \code{set.seed}. Defaults to \code{0}.
#' @return A list with \code{curve}, \code{n_patches}, \code{rates}, \code{mse}.
#' @export
momento_reconstruction_curve <- function(patches, reconstructor, rates, seed = 0) {
  P <- lapply(patches, as.numeric)
  n <- length(P)
  .rmorie_local_seed(seed)
  out <- list()
  for (r in rates) {
    m <- max(1L, min(n - 1L, as.integer(round(r * n))))
    idx <- sample.int(n, m)
    mk <- momento_mask_patches(P, idx)
    rec <- reconstructor(mk$masked, mk$mask)
    L <- momento_masked_loss(P, rec, mk$mask)
    out[[length(out) + 1L]] <- list(rate = mk$mask_rate, mse = L$mse, n_masked = m)
  }
  list(curve = out, n_patches = n,
       rates = vapply(out, function(o) o$rate, numeric(1)),
       mse = vapply(out, function(o) o$mse, numeric(1)))
}

# -- restored: morie-only definition kept through the rmorie sync --
#' momento_task_mask
#'
#' A step of the momento_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param n_patches Coerced to integer by the body, with \code{as.integer}.
#' @param task One of \code{"forecast"}, \code{"impute"}. Defaults to \code{"forecast"}.
#' @param span Coerced to integer by the body, with \code{as.integer}. Defaults to \code{1}.
#' @param start Optional; may be \code{NULL}. Coerced to integer by the body, with
#' \code{as.integer}.
#' @return The value of \code{seq.int}.
#' @export
momento_task_mask <- function(n_patches, task = "forecast", span = 1, start = NULL) {
  n <- as.integer(n_patches)
  s <- as.integer(span)
  tasks <- c("forecast", "impute", "classify", "anomaly")
  if (!(task %in% tasks)) {
    stop(sprintf("momento: task must be one of %s, got %s",
                 paste(tasks, collapse = ", "), task))
  }
  if (!(s >= 1L && s < n)) {
    stop(sprintf("momento: the span must lie in 1..%d, got %d", n - 1L, s))
  }
  if (task == "forecast") {
    return(seq.int(n - s, n - 1L))
  }
  if (task == "impute") {
    st <- if (is.null(start)) max(1L, (n - s) %/% 2L) else as.integer(start)
    if (st + s > n) {
      stop("momento: the imputation gap runs past the end")
    }
    return(seq.int(st, st + s - 1L))
  }
  seq.int(n - s, n - 1L)
}
