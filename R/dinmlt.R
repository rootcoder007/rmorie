# SPDX-License-Identifier: AGPL-3.0-or-later

.dino_softmax <- function(v, tau, center = NULL) {
  n <- length(v)
  cc <- if (is.null(center)) numeric(n) else center
  z <- (v - cc) / tau
  m <- max(z)
  e <- exp(z - m)
  s <- 0
  for (t in e) s <- s + t
  e / s
}

#' DINO multi-crop consistency
#'
#' Formula: 2 global + 8 local crops; consistency
#'
#' Every crop goes through the student, only the GLOBAL crops go through
#' the teacher, and the loss is the cross-entropy of the student
#' distribution against the sharpened, centred teacher over all pairs of
#' different crops.  With V views of which G are global that is G(V-1)
#' terms.  Centring plus sharpening is what stops the collapse to a
#' constant output.
#'
#' @param image A V x d matrix of per-crop logits, global crops first.
#' @param global_size Number of global crops G.
#' @param local_size Number of local crops; V must equal G + local_size.
#' @param tau_s Student temperature.
#' @param tau_t Teacher temperature (sharpening).
#' @param center Teacher centre vector, or NULL for the global mean.
#' @return List with \code{estimate}, \code{loss}, \code{n_pairs},
#'   \code{teacher}, \code{student_entropy}, \code{V}, \code{d},
#'   \code{method}.
#' @references Caron et al. (2021), Emerging Properties in
#'   Self-Supervised Vision Transformers, ICCV 2021:9650-9660.
#' @export
Dinmlt <- function(image, global_size = 2, local_size = 8, tau_s = 0.1,
                   tau_t = 0.04, center = NULL) {
  M <- .s03mat(image)
  V <- nrow(M)
  if (V == 0L) stop("empty input: image has no crops")
  d <- ncol(M)
  G <- as.integer(global_size); L <- as.integer(local_size)
  if (G < 1L) stop("global_size must be at least 1")
  if (V != G + L) stop("image must hold global_size + local_size rows")
  if (!(tau_s > 0 && tau_t > 0))
    stop("temperatures must be strictly positive")
  if (is.null(center)) {
    cc <- numeric(d)
    for (k in seq_len(d)) {
      s <- 0
      for (g in seq_len(G)) s <- s + M[g, k]
      cc[k] <- s / G
    }
  } else {
    cc <- .s03vec(center)
    if (length(cc) != d)
      stop("center must have one entry per output dimension")
  }
  teach <- matrix(0, G, d)
  for (g in seq_len(G)) teach[g, ] <- .dino_softmax(M[g, ], tau_t, cc)
  stud <- matrix(0, V, d)
  for (v in seq_len(V)) stud[v, ] <- .dino_softmax(M[v, ], tau_s)
  tot <- 0; npair <- 0L
  for (g in seq_len(G)) for (v in seq_len(V)) {
    if (v == g) next
    s <- 0
    for (k in seq_len(d)) s <- s - teach[g, k] * log(stud[v, k] + 1e-300)
    tot <- tot + s
    npair <- npair + 1L
  }
  loss <- if (npair > 0L) tot / npair else NaN
  ent <- 0
  for (v in seq_len(V)) for (k in seq_len(d))
    ent <- ent - stud[v, k] * log(stud[v, k] + 1e-300)
  .t1_result(estimate = loss, loss = loss, n_pairs = npair,
             teacher = teach, student_entropy = ent / V, V = V, d = d,
             method = "DINO multi-crop student-teacher consistency")
}
