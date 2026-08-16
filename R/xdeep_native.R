# morie.fn -- function file (rootcoder007/morie)
# xDeepFM: explicit, vector-wise, bounded-degree interactions.
#
# A plain deep network can approximate any function, so it will
# eventually represent feature interactions -- but *implicitly*, and at
# the **bit-wise** level: the units mix individual embedding
# *coordinates* across fields. Factorization machines model interactions
# at the **vector-wise** level, where a whole field embedding interacts
# with another whole field embedding. Those are different objects, and
# the paper's argument is that the difference matters and that a DNN's
# implicit interactions leave the maximum degree unknown.
#
# **The Compressed Interaction Network.** CIN builds interactions
# explicitly, one degree per layer:
#
# .. math:: X^{k}_{h,*} = \sum_{i=1}^{m_{k-1}}\sum_{j=1}^{m}
#          W^{k,h}_{ij}\,\big(X^{k-1}_{i,*} \circ X^0_{j,*}\big),
#
# where :math:`\circ` is the element-wise (Hadamard) product. Layer
# :math:`k` therefore contains exactly degree-:math:`(k+1)` interactions
# -- the degree is *bounded and known*, which is what "explicit" buys.
# The product being element-wise across whole embeddings is what makes it
# vector-wise rather than bit-wise, and the anchor checks both
# properties: the degree of each layer, and that swapping in a bit-wise
# mixing breaks the field structure.
#
# **Why it resembles a CNN and an RNN.** The outer product of
# :math:`X^{k-1}` with :math:`X^0` forms a tensor that is compressed by
# :math:`W^{k,h}` exactly as a filter compresses a feature map, and
# :math:`X^0` is reused at every layer as an RNN reuses its input --
# which is where the "compressed" in the name comes from.
#
# **The combination.** xDeepFM sums a linear term, the CIN, and a plain
# DNN, so bounded-degree explicit interactions and arbitrary implicit
# ones are both available and neither has to do the other's job.
#
# References
# ----------
# Lian, J., Zhou, X., Zhang, F., Chen, Z., Xie, X. & Sun, G. (2018)
# "xDeepFM: Combining Explicit and Implicit Feature Interactions for
# Recommender Systems", *Proceedings of the 24th ACM SIGKDD
# International Conference on Knowledge Discovery and Data Mining (KDD
# '18)*, 1754-1763, doi:10.1145/3219819.3220023, arXiv:1803.05170.
# Rendle, S. (2010) "Factorization Machines", *ICDM 2010*, 995-1000,
# doi:10.1109/ICDM.2010.127. The vector-wise tradition; implemented in
# :mod:`fmFM`.
# Guo, H., Tang, R., Ye, Y., Li, Z. & He, X. (2017) "DeepFM: A
# Factorization-Machine based Neural Network for CTR Prediction",
# *IJCAI 2017*, 1725-1731, doi:10.24963/ijcai.2017/239.

.xdeep_EPS <- 1e-12

#' .xdeep_to_vec
#'
#' Part of the xdeep_native implementation; see the file header for the
#' source it follows.
#'
#' @param a See Usage.
#' @return A vector, from \code{as.numeric}.
#' @export
.xdeep_to_vec <- function(a) {
  as.numeric(a)
}

#' .xdeep_to_mat
#'
#' Part of the xdeep_native implementation; see the file header for the
#' source it follows.
#'
#' @param M See Usage.
#' @return The value of \code{m}, as built in the body.
#' @export
.xdeep_to_mat <- function(M) {
  m <- as.matrix(M)
  storage.mode(m) <- "double"
  m
}

#' xdeep_hadamard
#'
#' Part of the xdeep_native implementation; see the file header for the
#' source it follows.
#'
#' @param a See Usage.
#' @param b See Usage.
#' @return A numeric value.
#' @export
xdeep_hadamard <- function(a, b) {
  x <- .xdeep_to_vec(a)
  y <- .xdeep_to_vec(b)
  if (length(x) != length(y)) {
    stop(sprintf("xdeep: embeddings differ in length (%d, %d)",
                 length(x), length(y)))
  }
  x * y
}

#' xdeep_cin_layer
#'
#' Part of the xdeep_native implementation; see the file header for the
#' source it follows.
#'
#' @param X_prev See Usage.
#' @param X0 See Usage.
#' @param W See Usage.
#' @return The value of \code{out}, as built in the body.
#' @export
xdeep_cin_layer <- function(X_prev, X0, W) {
  P <- .xdeep_to_mat(X_prev)
  Z <- .xdeep_to_mat(X0)
  mp <- nrow(P)
  m <- nrow(Z)
  d <- ncol(Z)
  if (ncol(P) != d) {
    stop("xdeep: the feature maps and input fields differ in embedding size")
  }
  H <- length(W)
  out <- vector("list", H)
  for (h in seq_len(H)) {
    Wh <- .xdeep_to_mat(W[[h]])
    acc <- numeric(d)
    for (i in seq_len(mp)) {
      for (j in seq_len(m)) {
        w <- Wh[i, j]
        if (w == 0) next
        acc <- acc + w * (P[i, ] * Z[j, ])
      }
    }
    out[[h]] <- acc
  }
  out
}

#' xdeep_cin
#'
#' Part of the xdeep_native implementation; see the file header for the
#' source it follows.
#'
#' @param X0 See Usage.
#' @param Ws See Usage.
#' @return A list with \code{estimate}, \code{pooled}, \code{layers}, \code{degrees}, \code{n_layers}, \code{method}, \code{note}.
#' @export
xdeep_cin <- function(X0, Ws) {
  Z <- .xdeep_to_mat(X0)
  cur <- Z
  layers <- list()
  pooled <- numeric(0)
  for (W in Ws) {
    cur <- xdeep_cin_layer(cur, Z, W)
    layers[[length(layers) + 1L]] <- cur
    for (row in cur) {
      pooled <- c(pooled, sum(row))
    }
  }
  list(
    estimate = pooled,
    pooled = pooled,
    layers = layers,
    degrees = seq_along(Ws) + 1L,
    n_layers = length(Ws),
    method = "Compressed Interaction Network; Lian et al. (2018)",
    note = paste("layer k contains exactly degree-(k+1) interactions,",
                 "so the maximum degree is BOUNDED and known")
  )
}

#' xdeep_interaction_degree
#'
#' Part of the xdeep_native implementation; see the file header for the
#' source it follows.
#'
#' @param layer_index See Usage.
#' @return A list with \code{layer}, \code{degree}, \code{note}.
#' @export
xdeep_interaction_degree <- function(layer_index) {
  i <- as.integer(layer_index)
  if (i < 0L) {
    stop("xdeep: the layer index cannot be negative")
  }
  list(
    layer = i,
    degree = i + 2L,
    note = paste("explicit and bounded, unlike a DNN's implicit",
                 "interactions of unknown maximum degree")
  )
}

#' xdeep_xdeepfm_score
#'
#' Part of the xdeep_native implementation; see the file header for the
#' source it follows.
#'
#' @param x_linear See Usage.
#' @param w_linear See Usage.
#' @param X0 See Usage.
#' @param Ws See Usage.
#' @param w_cin See Usage.
#' @param dnn_output Defaults to \code{0}.
#' @param w_dnn Defaults to \code{1}.
#' @param bias Defaults to \code{0}.
#' @return A list with \code{logit}, \code{probability}, \code{linear}, \code{cin}, \code{dnn}, \code{note}.
#' @export
xdeep_xdeepfm_score <- function(x_linear, w_linear, X0, Ws, w_cin,
                                dnn_output = 0.0, w_dnn = 1.0,
                                bias = 0.0) {
  xl <- .xdeep_to_vec(x_linear)
  wl <- .xdeep_to_vec(w_linear)
  if (length(xl) != length(wl)) {
    stop("xdeep: the linear term is mis-sized")
  }
  lin <- sum(xl * wl)
  c <- xdeep_cin(X0, Ws)$pooled
  wc <- .xdeep_to_vec(w_cin)
  if (length(wc) != length(c)) {
    stop(sprintf("xdeep: %d CIN weights for %d pooled units",
                 length(wc), length(c)))
  }
  ci <- sum(wc * c)
  dnn_term <- as.numeric(w_dnn) * as.numeric(dnn_output)
  z <- as.numeric(bias) + lin + ci + dnn_term
  prob <- if (z > -700) 1.0 / (1.0 + exp(-z)) else 0.0
  list(
    logit = z,
    probability = prob,
    linear = lin,
    cin = ci,
    dnn = dnn_term,
    note = paste("explicit and implicit interactions side by side,",
                 "neither doing the other's job")
  )
}

#' xdeep_cheatsheet
#'
#' Part of the xdeep_native implementation; see the file header for the
#' source it follows.
#'
#' @return A character value.
#' @export
xdeep_cheatsheet <- function() {
  paste("xdeep: a DNN represents interactions IMPLICITLY and",
        "BIT-WISE -- mixing individual embedding coordinates",
        "across fields, with no statement about the maximum",
        "degree. FMs work VECTOR-WISE, whole embedding against",
        "whole embedding. CIN does that explicitly, one degree per",
        "layer: layer k = degree k+1, built by a Hadamard product",
        "with X^0 and then compressed -- CNN-like compression,",
        "RNN-like reuse of the input. xDeepFM sums linear + CIN +",
        "DNN so bounded explicit and arbitrary implicit",
        "interactions coexist.")
}

# compact alias per ledger/NAMING.md
xdeep_xdeepfm <- xdeep_cin

# entry point
morie_xdeep <- xdeep_cin
