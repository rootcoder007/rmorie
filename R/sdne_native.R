# morie.fn -- function file (rootcoder007/morie)
# SDNE: first- and second-order proximity, jointly.
#
# Network embedding had used shallow models -- IsoMap, Laplacian
# Eigenmaps, LINE -- whose representational capacity cannot capture a
# highly non-linear network structure. SDNE is a deep model, and its
# argument is about **which** structure to preserve.
#
# **Two proximities, doing different jobs.**
#
# * **First-order** proximity is the pairwise similarity between
#   vertices that are *linked*. It is local, and in a real network it is
#   desperately sparse: many legitimate links are simply missing, so it
#   cannot describe the structure on its own.
# * **Second-order** proximity is the similarity of two vertices'
#   *neighbourhoods*. Two vertices need not be linked to be similar --
#   which is exactly what rescues the sparse case.
#
# They enter through different halves of a semi-supervised architecture:
# an unsupervised autoencoder reconstructs the adjacency row (second
# order, global), and a supervised Laplacian-eigenmaps term pulls linked
# vertices together (first order, local).
#
# **The reconstruction penalty must be re-weighted, and this is the part
# that is easy to get wrong.** The adjacency row is mostly zeros, so a
# plain squared error is minimised by predicting zero everywhere -- a
# perfect score for a useless embedding. SDNE imposes **more penalty on
# the non-zero entries**: \|(Xhat - X) o B\|^2 with b_ij = beta > 1
# where an edge exists and 1 where it does not. second_order_loss
# computes both, and the anchor shows the all-zero reconstruction
# winning at beta = 1 and losing at beta > 1.
#
# References
# ----------
# Wang, D., Cui, P. & Zhu, W. (2016) "Structural Deep Network
# Embedding", Proceedings of the 22nd ACM SIGKDD International
# Conference on Knowledge Discovery and Data Mining (KDD '16),
# 1225-1234, doi:10.1145/2939672.2939753. Sec. 1 and 3: that shallow
# models such as IsoMap, Laplacian Eigenmaps and LINE have limited
# representation ability and cannot capture the highly non-linear
# network structure; the semi-supervised deep model exploiting
# first-order and second-order proximity JOINTLY, with the unsupervised
# component reconstructing the second-order proximity to preserve the
# GLOBAL structure and the supervised component using first-order
# proximity to preserve the LOCAL structure; that first-order proximity
# is the local pairwise similarity only between linked vertices and is
# insufficient because network sparsity means many legitimate links are
# missing; and the re-weighted reconstruction imposing more penalty on
# the reconstruction error of non-zero elements than on zero elements.
#
# Belkin, M. & Niyogi, P. (2003) "Laplacian Eigenmaps for
# Dimensionality Reduction and Data Representation", Neural
# Computation 15(6), 1373-1396, doi:10.1162/089976603321780317. The
# first-order term.
#
# Tang, J., Qu, M., Wang, M., Zhang, M., Yan, J. & Mei, Q. (2015)
# "LINE: Large-scale Information Network Embedding", WWW 2015,
# 1067-1077, doi:10.1145/2736277.2741093, arXiv:1503.03578. The
# shallow model whose two proximities this deepens.

.sdne_EPS <- 1e-12

#' .sdne_mat
#'
#' A step of the sdne_native implementation. Called by \code{.sdne_first_order_loss},
#' \code{.sdne_penalty_matrix}, \code{.sdne_proximity_counts} and 2 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x A vector; its length is taken and its elements indexed.
#' @return The value of \code{out}, as built in the body.
#' @export
#' @examples
#' x <- c(1.2, 2.4, 3.1, 4.8, 5.3, 6.7, 7.1, 8.9)
#' res <- .sdne_mat(x = x)
#' res
.sdne_mat <- function(x) {
  if (is.matrix(x)) {
    storage.mode(x) <- "double"
    return(x)
  }
  nr <- length(x)
  nc <- if (nr > 0L) length(x[[1L]]) else 0L
  out <- matrix(0, nrow = nr, ncol = nc)
  for (i in seq_len(nr)) {
    out[i, ] <- as.numeric(x[[i]])
  }
  out
}

#' .sdne_penalty_matrix
#'
#' A step of the sdne_native implementation. Called by \code{.sdne_second_order_loss}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param adjacency Passed to \code{.sdne_mat}.
#' @param beta Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{5}.
#' @return A list with \code{B}, \code{beta}, \code{n_nonzero}, \code{n_zero}.
#' @export
.sdne_penalty_matrix <- function(adjacency, beta = 5.0) {
  A <- .sdne_mat(adjacency)
  b <- as.numeric(beta)
  if (b < 1.0) {
    stop("sdne: beta must be at least 1; below it the zeros would be weighted MORE than the edges")
  }
  B <- ifelse(A != 0.0, b, 1.0)
  list(B = B,
       beta = b,
       n_nonzero = sum(A != 0.0),
       n_zero = sum(A == 0.0))
}

#' .sdne_second_order_loss
#'
#' A step of the sdne_native implementation. Called by \code{morie_sdne}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param adjacency Passed to \code{.sdne_mat}.
#' @param reconstruction Passed to \code{.sdne_mat}.
#' @param beta Passed to \code{.sdne_penalty_matrix}. Defaults to \code{5}.
#' @return A list with \code{loss}, \code{unweighted}, \code{beta}, \code{note}.
#' @export
.sdne_second_order_loss <- function(adjacency, reconstruction, beta = 5.0) {
  X <- .sdne_mat(adjacency)
  H <- .sdne_mat(reconstruction)
  if (nrow(X) != nrow(H) || ncol(X) != ncol(H)) {
    stop(sprintf("sdne: the adjacency is %dx%d but the reconstruction is %dx%d",
                 nrow(X), ncol(X), nrow(H), ncol(H)))
  }
  B <- .sdne_penalty_matrix(X, beta)$B
  diff <- H - X
  weighted <- sum((diff * B)^2)
  plain <- sum(diff^2)
  list(loss = weighted,
       unweighted = plain,
       beta = as.numeric(beta),
       note = "the row is mostly zeros, so the unweighted loss rewards predicting nothing")
}

#' .sdne_first_order_loss
#'
#' A step of the sdne_native implementation. Called by \code{morie_sdne}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param adjacency Passed to \code{.sdne_mat}.
#' @param embeddings Passed to \code{.sdne_mat}.
#' @return A list with \code{loss}, \code{linked_pairs}, \code{note}.
#' @export
.sdne_first_order_loss <- function(adjacency, embeddings) {
  S <- .sdne_mat(adjacency)
  Y <- .sdne_mat(embeddings)
  n <- nrow(S)
  if (nrow(Y) != n) {
    stop(sprintf("sdne: %d vertices but %d embeddings", n, nrow(Y)))
  }
  tot <- 0.0
  pairs <- 0L
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      if (S[i, j] == 0.0 || i == j) next
      pairs <- pairs + 1L
      tot <- tot + S[i, j] * sum((Y[i, ] - Y[j, ])^2)
    }
  }
  list(loss = tot,
       linked_pairs = pairs,
       note = "zero iff every LINKED pair shares an embedding")
}

#' .sdne_proximity_counts
#'
#' A step of the sdne_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param adjacency Passed to \code{.sdne_mat}.
#' @return A list with \code{first_order_pairs}, \code{second_order_pairs},
#' \code{total_pairs}, \code{density}, \code{ratio}, \code{note}.
#' @export
.sdne_proximity_counts <- function(adjacency) {
  A <- .sdne_mat(adjacency)
  n <- nrow(A)
  first <- 0L
  second <- 0L
  if (n >= 2L) {
    for (i in 1:(n - 1L)) {
      for (j in (i + 1L):n) {
        if (A[i, j] != 0.0) first <- first + 1L
        shared <- sum((A[i, ] != 0.0) & (A[j, ] != 0.0))
        if (shared > 0L) second <- second + 1L
      }
    }
  }
  total <- n * (n - 1L) %/% 2L
  list(first_order_pairs = first,
       second_order_pairs = second,
       total_pairs = total,
       density = if (total > 0L) first / as.numeric(total) else 0.0,
       ratio = if (first > 0L) second / as.numeric(first) else Inf,
       note = "many legitimate links are missing, so the first-order set is far the smaller")
}

#' morie_sdne
#'
#' A step of the sdne_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param adjacency Passed to \code{.sdne_second_order_loss}.
#' @param reconstruction Passed to \code{.sdne_second_order_loss}.
#' @param embeddings Passed to \code{.sdne_first_order_loss}.
#' @param beta Passed to \code{.sdne_second_order_loss}. Defaults to \code{5}.
#' @param alpha Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.1}.
#' @param nu Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0}.
#' @param parameters Optional; may be \code{NULL}. Passed to \code{.sdne_mat}.
#' @return A list with \code{estimate}, \code{loss}, \code{second_order},
#' \code{first_order}, \code{regulariser}, \code{alpha}, \code{beta}, \code{method},
#' \code{note}.
#' @export
morie_sdne <- function(adjacency, reconstruction, embeddings, beta = 5.0,
                       alpha = 0.1, nu = 0.0, parameters = NULL) {
  s2 <- .sdne_second_order_loss(adjacency, reconstruction, beta)
  s1 <- .sdne_first_order_loss(adjacency, embeddings)
  reg <- 0.0
  if (!is.null(parameters)) {
    P <- .sdne_mat(parameters)
    reg <- as.numeric(nu) * sum(P^2)
  }
  total <- s2$loss + as.numeric(alpha) * s1$loss + reg
  list(estimate = total,
       loss = total,
       second_order = s2$loss,
       first_order = s1$loss,
       regulariser = reg,
       alpha = as.numeric(alpha),
       beta = as.numeric(beta),
       method = "SDNE joint objective; Wang, Cui & Zhu (2016)",
       note = "unsupervised autoencoder for the GLOBAL structure, supervised Laplacian term for the LOCAL one")
}

#' morie_sdne_cheatsheet
#'
#' A step of the sdne_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
morie_sdne_cheatsheet <- function() {
  paste0("sdne: shallow embeddings (IsoMap, Laplacian Eigenmaps, ",
         "LINE) cannot capture a highly NON-LINEAR network, so go ",
         "deep -- and preserve TWO proximities jointly. FIRST-order ",
         "is the local similarity between LINKED vertices, and in a ",
         "sparse network most legitimate links are missing, so it ",
         "is not enough. SECOND-order is the similarity of ",
         "NEIGHBOURHOODS, which needs no edge between the pair. An ",
         "autoencoder reconstructs the adjacency row (global) and a ",
         "Laplacian term pulls linked vertices together (local). ",
         "The reconstruction MUST re-weight: with B = 1 the ",
         "all-zero output wins, so put beta > 1 on the edges.")
}

# compact alias per ledger/NAMING.md
morie_structuraldeepnetwork <- morie_sdne
