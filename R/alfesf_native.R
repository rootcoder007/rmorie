# R arm of alfesf -- ESMFold/AlphaFold confidence: decode, run, fit, calibrate.
# Lin, Z. et al. (2023) Science 379(6637), 1123-1130, doi:10.1126/science.ade2574.
# Jumper, J. et al. (2021) Nature 596(7873), 583-589, SI 1.9.6 (pLDDT as a
#   binned expectation) and 1.9.7 (pTM, d0 = 1.24 (N-15)^(1/3) - 1.8).
# Zhang, Y. & Skolnick, J. (2004) Proteins 57(4), 702-710 -- TM score, d0.
# Guo, C. et al. (2017) ICML 70, 1321-1330 -- temperature scaling.
# Mirrors src/morie/fn/alfesf.py.

.alfesf_EPS <- 1e-12
.alfesf_LDDT_BINS <- 50L
.alfesf_PAE_WIDTH <- 0.5

#' .alfesf_rows
#'
#' Part of the alfesf_native implementation; see the file header for the
#' source it follows.
#'
#' @param x See Usage.
#' @param what See Usage.
#' @return The value of \code{m}, as built in the body.
#' @export
.alfesf_rows <- function(x, what) {
  if (is.matrix(x)) m <- x
  else if (is.data.frame(x)) m <- as.matrix(x)
  else if (is.list(x)) m <- do.call(rbind, lapply(x, as.numeric))
  else m <- matrix(as.numeric(x), nrow = 1L)
  storage.mode(m) <- "double"
  if (nrow(m) == 0L) stop(sprintf("%s: empty", what))
  m
}

#' .alfesf_softmax_rows
#'
#' Part of the alfesf_native implementation; see the file header for the
#' source it follows.
#'
#' @param M See Usage.
#' @param temp See Usage.
#' @return The value of \code{out}, as built in the body.
#' @export
.alfesf_softmax_rows <- function(M, temp) {
  out <- M
  for (i in seq_len(nrow(M))) {
    z <- M[i, ] / temp
    mx <- max(z)
    ex <- exp(z - mx)
    out[i, ] <- ex / sum(ex)
  }
  out
}

#' .alfesf_lddt_centres
#'
#' Part of the alfesf_native implementation; see the file header for the
#' source it follows.
#'
#' @param nb See Usage.
#' @return A numeric value.
#' @export
.alfesf_lddt_centres <- function(nb) ((seq_len(nb) - 1L) + 0.5) * 100.0 / nb

#' .alfesf_pae_centres
#'
#' Part of the alfesf_native implementation; see the file header for the
#' source it follows.
#'
#' @param nb See Usage.
#' @param width See Usage.
#' @return A numeric value.
#' @export
.alfesf_pae_centres <- function(nb, width) ((seq_len(nb) - 1L) + 0.5) * width

# Zhang-Skolnick normalisation. Below 16 residues the cube-root term goes
# negative and the published clamp at 0.5 applies -- which is why pTM on a
# very short chain saturates well below 1 however confident the model is.
#' Zhang-Skolnick normalisation. Below 16 residues the cube-root term
#' goes
#'
#' negative and the published clamp at 0.5 applies -- which is why pTM
#' on a very short chain saturates well below 1 however confident the
#' model is.
#'
#' @param n See Usage.
#' @return One of two values, depending on the branch taken.
#' @export
.alfesf_d0 <- function(n) {
  if (n <= 15L) return(0.5)
  v <- 1.24 * (n - 15.0)^(1.0 / 3.0) - 1.8
  if (v > 0.5) v else 0.5
}

# Multinomial logistic head by full-batch gradient ascent. Deterministic:
# zero init, fixed step, fixed iteration count, so both language arms walk
# the same path. No line search -- a search that branches on a
# floating-point comparison is exactly what makes two arms disagree.
#' Multinomial logistic head by full-batch gradient ascent.
#' Deterministic:
#'
#' zero init, fixed step, fixed iteration count, so both language arms
#' walk the same path. No line search -- a search that branches on a
#' floating-point comparison is exactly what makes two arms disagree.
#'
#' @param X See Usage.
#' @param y See Usage.
#' @param n_bins See Usage.
#' @param l2 See Usage.
#' @param iters See Usage.
#' @param lr See Usage.
#' @return A list with \code{W}, \code{b}.
#' @export
.alfesf_fit_multinomial <- function(X, y, n_bins, l2, iters, lr) {
  n <- nrow(X); d <- ncol(X)
  W <- matrix(0.0, d, n_bins)
  b <- numeric(n_bins)
  for (it in seq_len(as.integer(iters))) {
    gW <- matrix(0.0, d, n_bins)
    gb <- numeric(n_bins)
    for (i in seq_len(n)) {
      z <- numeric(n_bins)
      for (cc in seq_len(n_bins)) {
        s <- 0.0
        for (a in seq_len(d)) s <- s + X[i, a] * W[a, cc]
        z[cc] <- s + b[cc]
      }
      mx <- max(z)
      ex <- exp(z - mx)
      p <- ex / sum(ex)
      for (cc in seq_len(n_bins)) {
        g <- (if (cc - 1L == y[i]) 1.0 else 0.0) - p[cc]
        gb[cc] <- gb[cc] + g
        for (a in seq_len(d)) gW[a, cc] <- gW[a, cc] + g * X[i, a]
      }
    }
    for (cc in seq_len(n_bins)) {
      b[cc] <- b[cc] + lr * gb[cc] / n
      for (a in seq_len(d)) W[a, cc] <- W[a, cc] + lr * (gW[a, cc] / n - l2 * W[a, cc])
    }
  }
  list(W = W, b = b)
}

#' .alfesf_fit_temperature
#'
#' Part of the alfesf_native implementation; see the file header for the
#' source it follows.
#'
#' @param L See Usage.
#' @param y See Usage.
#' @param iters Defaults to \code{200L}.
#' @param lr Defaults to \code{0.5}.
#' @return A numeric value.
#' @export
.alfesf_fit_temperature <- function(L, y, iters = 200L, lr = 0.5) {
  logt <- 0.0
  n <- nrow(L)
  for (it in seq_len(as.integer(iters))) {
    g <- 0.0
    t <- exp(logt)
    for (i in seq_len(n)) {
      z <- L[i, ] / t
      mx <- max(z)
      ex <- exp(z - mx)
      p <- ex / sum(ex)
      zc <- L[i, y[i] + 1L] / t
      g <- g - zc + sum(p * L[i, ] / t)
    }
    logt <- logt + lr * g / n
  }
  exp(logt)
}

#' morie_alfesf_esmfold_confidence
#'
#' Part of the alfesf_native implementation; see the file header for the
#' source it follows.
#'
#' @param lddt_logits Defaults to \code{NULL}.
#' @param pae_logits Defaults to \code{NULL}.
#' @param features Defaults to \code{NULL}.
#' @param weights Defaults to \code{NULL}.
#' @param lddt Defaults to \code{NULL}.
#' @param chain_id Defaults to \code{NULL}.
#' @param temperature Defaults to \code{1}.
#' @param l2 Defaults to \code{0.001}.
#' @param iters Defaults to \code{300L}.
#' @param lr Defaults to \code{0.5}.
#' @param pae_bin_width Defaults to \code{0.5}.
#' @return A list with \code{estimate}, \code{plddt}, \code{plddt_mean}, \code{ptm}, \code{iptm}, \code{pae}, \code{d0}, \code{temperature}, \code{weights}, \code{route}, \code{n_lddt_bins}, \code{n_pae_bins}, \code{method}, \code{note}.
#' @export
morie_alfesf_esmfold_confidence <- function(lddt_logits = NULL,
                                            pae_logits = NULL,
                                            features = NULL, weights = NULL,
                                            lddt = NULL, chain_id = NULL,
                                            temperature = 1.0, l2 = 1e-3,
                                            iters = 300L, lr = 0.5,
                                            pae_bin_width = 0.5) {
  route <- NULL
  fitted_W <- NULL; fitted_b <- NULL

  if (!is.null(features) && !is.null(lddt)) {
    X <- .alfesf_rows(features, "alfesf features")
    obs <- as.numeric(lddt)
    if (length(obs) != nrow(X))
      stop(sprintf("alfesf: %d feature rows but %d lddt values",
                   nrow(X), length(obs)))
    if (any(obs < 0.0 | obs > 100.0))
      stop(sprintf("alfesf: lddt must be on 0..100, got %g",
                   obs[which(obs < 0.0 | obs > 100.0)[1]]))
    y <- pmin(as.integer(obs / 100.0 * .alfesf_LDDT_BINS),
              .alfesf_LDDT_BINS - 1L)
    fit <- .alfesf_fit_multinomial(X, y, .alfesf_LDDT_BINS, l2, iters, lr)
    fitted_W <- fit$W; fitted_b <- fit$b
    lddt_logits <- X %*% fitted_W
    for (cc in seq_len(ncol(lddt_logits)))
      lddt_logits[, cc] <- lddt_logits[, cc] + fitted_b[cc]
    route <- "fitted a multinomial LDDT head from features and observations"
  } else if (!is.null(features) && !is.null(weights)) {
    X <- .alfesf_rows(features, "alfesf features")
    W <- .alfesf_rows(weights$W, "alfesf weights W")
    b <- as.numeric(weights$b)
    if (nrow(W) != ncol(X))
      stop(sprintf(paste0("alfesf: weights W has %d rows but the features ",
                          "have %d columns"), nrow(W), ncol(X)))
    if (length(b) != ncol(W))
      stop(sprintf(paste0("alfesf: bias length %d does not match the %d ",
                          "output bins"), length(b), ncol(W)))
    lddt_logits <- X %*% W
    for (cc in seq_len(ncol(lddt_logits)))
      lddt_logits[, cc] <- lddt_logits[, cc] + b[cc]
    route <- "ran a supplied LDDT head over the features"
  } else if (!is.null(features)) {
    stop(paste0("alfesf: features were given with neither `weights` to run ",
                "nor `lddt` to fit. Nothing is bundled and nothing will be ",
                "invented: supply trained parameters, or observations to ",
                "train on, or pass the head's logits directly."))
  }

  if (is.null(lddt_logits) && is.null(pae_logits))
    stop(paste0("alfesf: give lddt_logits and/or pae_logits to decode, or ",
                "features with weights (to run) or with lddt (to fit)."))
  if (is.null(route)) route <- "decoded supplied logits"

  if (is.character(temperature)) {
    if (temperature != "fit")
      stop("alfesf: temperature must be a number or 'fit'")
    if (is.null(lddt) || is.null(lddt_logits))
      stop(paste0("alfesf: temperature='fit' needs observed lddt and the ",
                  "logits to calibrate"))
    obs <- as.numeric(lddt)
    y <- pmin(as.integer(obs / 100.0 * .alfesf_LDDT_BINS),
              .alfesf_LDDT_BINS - 1L)
    temp_used <- .alfesf_fit_temperature(.alfesf_rows(lddt_logits, "logits"), y)
    route <- paste0(route, "; temperature calibrated")
  } else {
    temp_used <- as.numeric(temperature)
    if (!(temp_used > 0.0)) stop("alfesf: temperature must be positive")
  }

  plddt <- NULL
  if (!is.null(lddt_logits)) {
    L <- .alfesf_rows(lddt_logits, "alfesf lddt_logits")
    nb <- ncol(L)
    cen <- .alfesf_lddt_centres(nb)
    P <- .alfesf_softmax_rows(L, temp_used)
    plddt <- as.numeric(P %*% cen)
  }

  ptm <- NULL; iptm <- NULL; pae <- NULL; d0 <- NULL
  if (!is.null(pae_logits)) {
    flat <- .alfesf_rows(pae_logits, "alfesf pae_logits")
    n <- as.integer(round(sqrt(nrow(flat))))
    if (n * n != nrow(flat))
      stop(sprintf(paste0("alfesf: %d aligned-error rows is not a square ",
                          "number of residue pairs"), nrow(flat)))
    nb <- ncol(flat)
    cen <- .alfesf_pae_centres(nb, pae_bin_width)
    Pp <- .alfesf_softmax_rows(flat, temp_used)
    pae <- matrix(0.0, n, n)
    for (i in seq_len(n)) for (j in seq_len(n))
      pae[i, j] <- sum(Pp[(i - 1L) * n + j, ] * cen)
    d0 <- .alfesf_d0(n)
    f <- 1.0 / (1.0 + (cen / d0)^2)
    per_i <- numeric(n)
    for (i in seq_len(n)) {
      s <- 0.0
      for (j in seq_len(n)) s <- s + sum(Pp[(i - 1L) * n + j, ] * f)
      per_i[i] <- s / n
    }
    ptm <- max(per_i)
    if (!is.null(chain_id)) {
      ch <- as.vector(chain_id)
      if (length(ch) != n)
        stop(sprintf("alfesf: %d chain labels for %d residues",
                     length(ch), n))
      if (length(unique(ch)) > 1L) {
        inter <- c()
        for (i in seq_len(n)) {
          js <- which(ch != ch[i])
          if (!length(js)) next
          s <- 0.0
          for (j in js) s <- s + sum(Pp[(i - 1L) * n + j, ] * f)
          inter <- c(inter, s / length(js))
        }
        if (length(inter)) iptm <- max(inter)
      }
    }
  }

  list(estimate = if (!is.null(plddt)) mean(plddt) else ptm,
       plddt = plddt,
       plddt_mean = if (!is.null(plddt)) mean(plddt) else NULL,
       ptm = ptm, iptm = iptm, pae = pae, d0 = d0,
       temperature = temp_used,
       weights = if (!is.null(fitted_W)) list(W = fitted_W, b = fitted_b)
                 else NULL,
       route = route,
       n_lddt_bins = if (!is.null(lddt_logits))
         ncol(.alfesf_rows(lddt_logits, "l")) else NULL,
       n_pae_bins = if (!is.null(pae)) ncol(pae) else NULL,
       method = paste0("ESMFold/AlphaFold confidence: pLDDT as the ",
                       "expectation of the binned LDDT distribution, pTM as ",
                       "the Zhang-Skolnick TM expectation under the ",
                       "aligned-error distribution with d0 = 1.24 ",
                       "(N-15)^(1/3) - 1.8, ipTM restricted to inter-chain ",
                       "pairs"),
       note = paste0("route says which of the four paths ran. No network ",
                     "weights are bundled: supply them, fit them here from ",
                     "observed LDDT, or pass logits straight from a model ",
                     "you ran elsewhere. iptm is None when chain_id is ",
                     "absent or names a single chain -- reporting ptm in ",
                     "its place would be wrong, since ipTM is by definition ",
                     "the inter-chain restriction."))
}

#' .alfesf_cheatsheet
#'
#' Part of the alfesf_native implementation; see the file header for the
#' source it follows.
#'
#' @return A character value.
#' @export
.alfesf_cheatsheet <- function() {
  paste0("alfesf: morie_alfesf_esmfold_confidence(lddt_logits, pae_logits) ",
         "-> pLDDT, pTM, ipTM; or features+weights to run, features+lddt to ",
         "fit (Lin et al. 2023 Science 379:1123; Jumper et al. 2021 SI 1.9)")
}

morie_alfesf <- morie_alfesf_esmfold_confidence
