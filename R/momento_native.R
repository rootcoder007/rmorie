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
morie_momento_harmonise <- function(series_list, patch_len,
                                     normalise = TRUE) {
  P <- as.integer(patch_len)
  if (P < 1L) stop("momento: patch_len must be at least 1")
  if (length(series_list) == 0L) stop("momento: no series given")
  out <- list(); meta <- list()
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
        m <- 0; sd <- 1
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
morie_momento_masked_loss <- function(truth, reconstruction, mask) {
  T <- lapply(truth, as.numeric)
  R <- lapply(reconstruction, as.numeric)
  if (length(T) != length(R) || length(T) != length(mask))
    stop(paste0("momento: truth, reconstruction and mask must agree ",
                "in length (", length(T), ", ", length(R), ", ",
                length(mask), ")"))
  tot <- 0; cnt <- 0
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
morie_momento_task_mask <- function(n_patches, task = "forecast",
                                     span = 1, start = NULL) {
  n <- as.integer(n_patches); s <- as.integer(span)
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
