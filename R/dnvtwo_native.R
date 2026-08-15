# SPDX-License-Identifier: AGPL-3.0-or-later
# morie.fn.dnvtwo -- DINOv2: curation, KoLeo, Sinkhorn, distillation.
#
# R mirror of morie.fn.dnvtwo.
# The five exports here are the building blocks of DINOv2
# (Oquab et al., 2024, TMLR): cosine-similarity dedup, retrieval
# augmentation, the Kozachenko-Leonenko spread regulariser, the
# Sinkhorn-Knopp centering used by SwAV, and the DINO image-level
# self-distillation loss with its iBOT patch-level variant.
#
# All operations are performed in the same order as the Python
# reference so that the three-way parity harness can assert bit-level
# agreement at 1e-9; no library linear algebra, no pseudo-random
# numbers, no library() calls.
#
# References
# Oquab, M., Darcet, T., Moutakanni, T., Vo, H., Szafraniec, M.,
# Khalidov, V., Fernandez, P., Haziza, D., Massa, F., El-Nouby, A. et
# al. (2024) "DINOv2: Learning Robust Visual Features without
# Supervision", TMLR 01/2024, arXiv:2304.07193.
# Caron, M., Touvron, H., Misra, I., Jegou, H., Mairal, J., Bojanowski,
# P. & Joulin, A. (2021) "Emerging Properties in Self-Supervised Vision
# Transformers", ICCV 2021, arXiv:2104.14294. DINO.
# Zhou, J., Wei, C., Wang, H., Shen, W., Xie, C., Yuille, A. & Kong, T.
# (2022) "iBOT: Image BERT Pre-Training with Online Tokenizer", ICLR
# 2022, arXiv:2111.07832.
# Sablayrolles, A., Douze, M., Schmid, C. & Jegou, H. (2019)
# "Spreading vectors for similarity search", ICLR 2019,
# arXiv:1806.03198.

.dnvtwo_eps <- 1e-12

# L2-normalise a single row vector.  Mirrors Python's _norm().
.dnvtwo_norm_vec <- function(v) {
  v <- as.numeric(v)
  s2 <- sum(v * v)
  if (s2 > .dnvtwo_eps) v / sqrt(s2) else v
}

# Cosine similarity between two numeric vectors.
.dnvtwo_cos_raw <- function(a, b) {
  a <- .dnvtwo_norm_vec(a)
  b <- .dnvtwo_norm_vec(b)
  sum(a * b)
}

# Coerce to list-of-rows of doubles (mirrors k.mat).
.dnvtwo_mat <- function(x) {
  if (is.null(x)) return(list())
  if (is.matrix(x)) {
    storage.mode(x) <- "double"
    return(lapply(seq_len(nrow(x)), function(i) as.numeric(x[i, ])))
  }
  if (is.data.frame(x)) x <- as.matrix(x)
  if (is.list(x)) return(lapply(x, function(r) as.numeric(r)))
  list(as.numeric(x))
}

# Coerce to a single numeric vector (mirrors k.vec).
.dnvtwo_vec <- function(x) {
  if (is.null(x)) return(numeric(0))
  as.numeric(x)
}

#' Deduplicate by cosine similarity
#'
#' Walks the rows of an embedding matrix and drops any whose cosine
#' similarity to an already-kept row is at least the threshold.
#'
#' @param embeddings Numeric matrix (n x d), or list of row vectors.
#' @param threshold Cosine-similarity threshold for "near-duplicate".
#' @return List with \code{keep}, \code{dropped}, \code{n_before},
#'   \code{n_after}, \code{note}.
#' @references Oquab et al. (2024), DINOv2, TMLR 2024, Sec. 2.
#' @export
deduplicate <- function(embeddings, threshold = 0.999) {
  E <- .dnvtwo_mat(embeddings)
  th <- as.numeric(threshold)
  keep <- integer(0)
  dropped <- list()
  n <- length(E)
  for (i in seq_len(n)) {
    dup <- NA_integer_
    if (length(keep)) {
      for (j in keep) {
        if (.dnvtwo_cos_raw(E[[i]], E[[j]]) >= th) {
          dup <- j
          break
        }
      }
    }
    if (is.na(dup)) {
      keep <- c(keep, i)
    } else {
      dropped[[length(dropped) + 1L]] <- c(i, dup)
    }
  }
  list(keep = as.integer(keep),
       dropped = dropped,
       n_before = n,
       n_after = length(keep),
       note = "similarity, not metadata -- no annotation is required")
}

#' Retrieve uncurated nearest neighbours for each curated query
#'
#' @param curated Numeric matrix (n_c x d).
#' @param uncurated Numeric matrix (n_u x d).
#' @param per_query Number of neighbours requested per curated row.
#' @param min_similarity Floor on cosine similarity to keep a pick.
#' @return List with \code{retrieved}, \code{per_query},
#'   \code{duplication}, \code{n_added}, \code{max_times_retrieved},
#'   \code{note}.
#' @references Oquab et al. (2024), DINOv2, TMLR 2024, Sec. 2.
#' @export
retrieve_augment <- function(curated, uncurated, per_query = 2L,
                             min_similarity = 0) {
  C <- .dnvtwo_mat(curated)
  U <- .dnvtwo_mat(uncurated)
  if (!length(C))
    stop("dnvtwo: the curated corpus is empty, so there is nothing to retrieve against")
  pq <- as.integer(per_query)
  ms <- as.numeric(min_similarity)
  picked <- integer(0)
  per <- vector("list", length(C))
  for (qi in seq_along(C)) {
    sims <- vapply(U, function(u) .dnvtwo_cos_raw(C[[qi]], u),
                   numeric(1))
    ord <- order(-sims)
    got <- integer(0)
    if (length(ord) > 0L) {
      for (k in ord[seq_len(min(pq, length(ord)))]) {
        if (sims[k] >= ms) got <- c(got, as.integer(k))
      }
    }
    per[[qi]] <- got
    picked <- c(picked, got)
  }
  counts <- table(picked)
  list(retrieved = sort(unique(picked)),
       per_query = setNames(per, as.character(seq_along(C) - 1L)),
       duplication = as.list(as.table(counts)),
       n_added = length(unique(picked)),
       max_times_retrieved = if (length(counts)) max(counts) else 0L,
       note = "a few dominant modes would otherwise be retrieved by every query")
}

#' Kozachenko-Leonenko spread regulariser
#'
#' @param features Numeric matrix (n x d), or list of row vectors.
#' @return List with \code{loss}, \code{nearest_distances},
#'   \code{min_distance}, \code{note}.
#' @references Oquab et al. (2024), DINOv2, TMLR 2024, Sec. 4;
#'   Sablayrolles et al. (2019), ICLR 2019.
#' @export
koleo <- function(features) {
  raw <- .dnvtwo_mat(features)
  if (length(raw) < 2L)
    stop("dnvtwo: KoLeo needs at least 2 features")
  F <- lapply(raw, .dnvtwo_norm_vec)
  n <- length(F)
  d <- length(F[[1L]])
  dmin <- numeric(n)
  for (i in seq_len(n)) {
    best <- Inf
    for (j in seq_len(n)) {
      if (j == i) next
      acc <- 0
      for (a in seq_len(d)) {
        diff <- F[[i]][a] - F[[j]][a]
        acc <- acc + diff * diff
      }
      d_ij <- sqrt(acc)
      if (d_ij < best) best <- d_ij
    }
    dmin[i] <- best
  }
  tot <- 0
  for (i in seq_len(n)) tot <- tot + log(max(dmin[i], .dnvtwo_eps))
  list(loss = -tot / n,
       nearest_distances = dmin,
       min_distance = min(dmin),
       note = "collapsed features give a huge loss; a uniform span gives the smallest")
}

#' Sinkhorn-Knopp centering (SwAV style)
#'
#' @param scores Numeric matrix (n x K).
#' @param iterations Number of Sinkhorn-Knopp iterations.
#' @param epsilon Temperature on the exp transform.
#' @return List with \code{Q} (matrix), \code{iterations},
#'   \code{row_sums}, \code{note}.
#' @references Caron et al. (2021), DINO, ICCV 2021; Oquab et al.
#'   (2024), DINOv2, TMLR 2024, Sec. 4.
#' @export
sinkhorn_knopp <- function(scores, iterations = 3L, epsilon = 0.05) {
  S <- .dnvtwo_mat(scores)
  n <- length(S)
  K <- if (n > 0L) length(S[[1L]]) else 0L
  eps <- as.numeric(epsilon)
  it <- as.integer(iterations)
  Q <- matrix(0, nrow = n, ncol = K)
  if (n > 0L && K > 0L) {
    for (i in seq_len(n)) {
      for (j in seq_len(K)) {
        Q[i, j] <- exp(S[[i]][j] / eps)
      }
    }
    tot <- sum(Q)
    if (tot == 0) tot <- 1
    Q <- Q / tot
    for (iter in seq_len(it)) {
      for (j in seq_len(K)) {
        c <- sum(Q[, j])
        if (c == 0) c <- 1
        Q[, j] <- Q[, j] / c / K
      }
      for (i in seq_len(n)) {
        r <- sum(Q[i, ])
        if (r == 0) r <- 1
        Q[i, ] <- Q[i, ] / r / n
      }
    }
  }
  Q_final <- Q * n
  list(Q = Q_final,
       iterations = it,
       row_sums = rowSums(Q_final),
       note = "3 iterations, as specified")
}

#' DINO self-distillation loss (image- or patch-level)
#'
#' @param student Numeric vector.
#' @param teacher Numeric vector.
#' @param temperature_s Student softmax temperature.
#' @param temperature_t Teacher softmax temperature.
#' @param patch_level If TRUE, label the level as "patch" (iBOT).
#' @return Named list mirroring the RichResult payload: \code{estimate},
#'   \code{loss}, \code{student}, \code{teacher}, \code{level},
#'   \code{teacher_entropy}, \code{method}, \code{note}.
#' @references Oquab et al. (2024), DINOv2, TMLR 2024; Caron et al.
#'   (2021), DINO, ICCV 2021; Zhou et al. (2022), iBOT, ICLR 2022.
#' @export
self_distillation_loss <- function(student, teacher, temperature_s = 0.1,
                                   temperature_t = 0.04,
                                   patch_level = FALSE) {
  s <- .dnvtwo_vec(student)
  t <- .dnvtwo_vec(teacher)
  if (length(s) != length(t))
    stop("dnvtwo: the student and teacher outputs differ in width")
  ts <- as.numeric(temperature_s)
  tt <- as.numeric(temperature_t)
  if (ts <= 0 || tt <= 0)
    stop("dnvtwo: the temperatures must be positive")

  soft <- function(x, T) {
    m <- max(x)
    e <- exp((x - m) / T)
    z <- sum(e)
    e / z
  }

  ps <- soft(s, ts)
  pt <- soft(t, tt)
  loss <- -sum(pt * log(pmax(ps, .dnvtwo_eps)))
  list(estimate = loss,
       loss = loss,
       student = ps,
       teacher = pt,
       level = if (isTRUE(patch_level)) "patch" else "image",
       teacher_entropy = -sum(pt * log(pmax(pt, .dnvtwo_eps))),
       method = "self-distillation with a sharpened teacher; Oquab et al. (2024)",
       note = paste0("the teacher is sharper (lower temperature), ",
                     "which is what gives the student a target to ",
                     "move toward"))
}

#' One-paragraph summary of the DINOv2 recipe.
#'
#' @return Character string.
#' @export
cheatsheet <- function() {
  paste0("dnvtwo: self-supervision lost feature quality when scaled ",
         "to UNCURATED data -- the cause is data quality and ",
         "diversity, not the objective. So CURATE automatically: ",
         "embed, DEDUPLICATE, then RETRIEVE uncurated images ",
         "against a small curated corpus, using similarity rather ",
         "than metadata, and rebalance clusters so a few dominant ",
         "modes do not take over. Learn at BOTH image level ",
         "(self-distillation) and patch level (iBOT), which is why ",
         "the features work for dense tasks. KoLeo = -(1/n) sum log ",
         "d_{n,i} keeps the batch spread out; coincident features ",
         "send it to infinity.")
}

# compact alias per ledger/NAMING.md
dinov2 <- self_distillation_loss

# public names resolved by fn/_lazy_map.json
dinov2_repr <- self_distillation_loss

# Module entry point: morie_dnvtwo.
# Returns a namespace-style list of the public exports so callers can
# write morie_dnvtwo$deduplicate(...) etc., while the individual
# functions remain available at the top level after source().
morie_dnvtwo <- list(
  deduplicate = deduplicate,
  retrieve_augment = retrieve_augment,
  koleo = koleo,
  sinkhorn_knopp = sinkhorn_knopp,
  self_distillation_loss = self_distillation_loss,
  dinov2 = self_distillation_loss,
  dinov2_repr = self_distillation_loss,
  cheatsheet = cheatsheet
)

#' @rdname deduplicate
#' @export
morie_dnvtwo <- deduplicate

#' @rdname deduplicate
#' @export
morie_dnvtwo <- deduplicate

#' @rdname deduplicate
#' @export
morie_dnvtwo <- deduplicate

#' @rdname deduplicate
#' @export
morie_dnvtwo <- deduplicate

#' @rdname deduplicate
#' @export
morie_dnvtwo <- deduplicate

#' @rdname deduplicate
#' @export
morie_dnvtwo <- deduplicate

#' @rdname deduplicate
#' @export
morie_dnvtwo <- deduplicate

#' @rdname deduplicate
#' @export
morie_dnvtwo <- deduplicate

#' @rdname deduplicate
#' @export
morie_dnvtwo <- deduplicate

#' @rdname deduplicate
#' @export
morie_dnvtwo <- deduplicate

#' @rdname deduplicate
#' @export
morie_dnvtwo <- deduplicate

#' @rdname deduplicate
#' @export
morie_dnvtwo <- deduplicate

#' @rdname deduplicate
#' @export
morie_dnvtwo <- deduplicate

#' @rdname deduplicate
#' @export
morie_dnvtwo <- deduplicate

#' @rdname deduplicate
#' @export
morie_dnvtwo <- deduplicate

#' @rdname deduplicate
#' @export
morie_dnvtwo <- deduplicate

#' @rdname deduplicate
#' @export
morie_dnvtwo <- deduplicate

#' @rdname deduplicate
#' @export
morie_dnvtwo <- deduplicate

#' @rdname deduplicate
#' @export
morie_dnvtwo <- deduplicate

#' @rdname deduplicate
#' @export
morie_dnvtwo <- deduplicate

#' @rdname deduplicate
#' @export
morie_dnvtwo <- deduplicate
