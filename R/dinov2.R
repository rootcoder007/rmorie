# SPDX-License-Identifier: AGPL-3.0-or-later

#' DINOv2 representation objective
#'
#' Formula: DINO + iBOT mask + KoLeo
#'
#' Three terms.  The image-level DINO cross-entropy between the
#' sharpened teacher and the student; the patch-level iBOT
#' cross-entropy over the MASKED patches only; and the KoLeo
#' regulariser -mean log d_i, where d_i is the distance from embedding i
#' to its nearest neighbour, which spreads the batch out and stops
#' features from piling up.
#'
#' @param x An n x d matrix of batch embeddings, for KoLeo.
#' @param student A (1 + P) x k matrix: class token then patch tokens.
#' @param teacher A (1 + P) x k teacher matrix, same layout.
#' @param tau Student temperature.
#' @param tau_t Teacher temperature.
#' @param mask 0/1 flag per patch, or NULL for every second patch.
#' @param w_ibot,w_koleo Weights of the iBOT and KoLeo terms.
#' @return List with \code{estimate}, \code{loss}, \code{dino},
#'   \code{ibot}, \code{koleo}, \code{n_masked}, \code{n}, \code{d},
#'   \code{method}.
#' @references Oquab et al. (2024), DINOv2, TMLR 2024; Zhou et al.
#'   (2022), iBOT, ICLR 2022; Sablayrolles et al. (2019), ICLR 2019.
#' @export
Dinov2 <- function(x, student, teacher, tau = 0.1, tau_t = 0.04,
                   mask = NULL, w_ibot = 1, w_koleo = 0.1) {
  S <- .s03mat(student); TT <- .s03mat(teacher)
  if (!nrow(S) || !nrow(TT))
    stop("empty input: student and teacher logits are required")
  if (nrow(S) != nrow(TT) || ncol(S) != ncol(TT))
    stop("student and teacher must have the same shape")
  if (!(tau > 0 && tau_t > 0))
    stop("temperatures must be strictly positive")
  P <- nrow(S) - 1L
  if (P < 0L) stop("logits must hold a class token")
  ps <- .dino_softmax(S[1, ], tau)
  pt <- .dino_softmax(TT[1, ], tau_t)
  dino <- 0
  for (k in seq_along(ps)) dino <- dino - pt[k] * log(ps[k] + 1e-300)
  mk <- if (is.null(mask)) as.integer(seq_len(P) %% 2L == 0L) else
    as.integer(.s03vec(mask) != 0)
  if (length(mk) != P) stop("mask must have one flag per patch")
  ibot <- 0; nm <- 0L
  if (P > 0L) for (i in seq_len(P)) {
    if (!mk[i]) next
    a <- .dino_softmax(S[1L + i, ], tau)
    b <- .dino_softmax(TT[1L + i, ], tau_t)
    s <- 0
    for (k in seq_along(a)) s <- s - b[k] * log(a[k] + 1e-300)
    ibot <- ibot + s
    nm <- nm + 1L
  }
  ibot <- if (nm > 0L) ibot / nm else 0
  E <- .s03mat(x)
  n <- nrow(E)
  if (n < 2L) stop("KoLeo needs at least two embeddings")
  d <- ncol(E)
  En <- matrix(0, n, d)
  for (i in seq_len(n)) En[i, ] <- .clip_l2norm(E[i, ])
  koleo <- 0
  for (i in seq_len(n)) {
    best <- Inf
    for (j in seq_len(n)) {
      if (i == j) next
      dd <- 0
      for (k in seq_len(d)) dd <- dd + (En[i, k] - En[j, k])^2
      dd <- sqrt(dd)
      if (dd < best) best <- dd
    }
    koleo <- koleo - log(best + 1e-12)
  }
  koleo <- koleo / n
  loss <- dino + w_ibot * ibot + w_koleo * koleo
  .t1_result(estimate = loss, loss = loss, dino = dino, ibot = ibot,
             koleo = koleo, n_masked = nm, n = n, d = d,
             method = "DINOv2 objective: DINO + iBOT + KoLeo")
}
