# morie.fn -- function file (rootcoder007/morie)
# AnoGAN: anomaly detection by inverting a generator.
# 
# Anomalies are, by construction, what the training data does not
# contain. So train a GAN on **normal** data only, and it learns a
# manifold of normal appearance. A new image is then scored by how well
# it can be *reproduced* from that manifold.
# 
# **The inversion is the method.** A GAN maps :math:`z \to G(z)` but
# provides no inverse, so the latent code for a query image is found by
# optimisation: fix the trained generator and descend on :math:`z` to
# minimise a loss against the query. Nothing about the network changes;
# the search is over the latent space alone, which is why an anomalous
# image cannot simply be memorised.
# 
# **Two loss terms doing different work.**
# 
# * **Residual loss** :math:`\sum |x - G(z)|` -- pixel disagreement.
# * **Discrimination loss** :math:`\sum |f(x) - f(G(z))|`, comparing
#   *intermediate discriminator features* rather than its output.
#   Comparing the discriminator's scalar verdict instead would give
#   almost no gradient; the feature layer is what makes the term
#   informative.
# 
# The score is :math:`(1-\lambda)L_R + \lambda L_D`, and the **residual
# map** :math:`|x - G(z)|` localises the anomaly rather than only
# flagging the image -- which is the clinically useful part.
# 
# **The failure mode is a generator that is too good.** If :math:`G`
# can reproduce anything, every image scores zero and nothing is
# anomalous. Restricted capacity is therefore load-bearing, not an
# implementation compromise, and ``score_separation`` measures whether
# normal and anomalous scores actually separate rather than assuming it.
# 
# References
# ----------
# Schlegl, T., Seebock, P., Waldstein, S. M., Schmidt-Erfurth, U. &
# Langs, G. (2017) "Unsupervised Anomaly Detection with Generative
# Adversarial Networks to Guide Marker Discovery", *Information
# Processing in Medical Imaging (IPMI 2017)*, LNCS 10265, 146-157,
# doi:10.1007/978-3-319-59050-9_12, arXiv:1703.05921. Training a GAN on
# normal data to learn a manifold of normal anatomical variability;
# mapping a query image back to the latent space by iterative
# optimisation of z with the generator fixed; the anomaly score
# combining a residual loss on pixel differences with a discrimination
# loss on intermediate discriminator features; and the residual image
# localising anomalies.
# 
# Goodfellow, I. J., Pouget-Abadie, J., Mirza, M., Xu, B.,
# Warde-Farley, D., Ozair, S., Courville, A. & Bengio, Y. (2014)
# "Generative Adversarial Nets", *NIPS 2014*, 2672-2680,
# arXiv:1406.2661.
# 
# Radford, A., Metz, L. & Chintala, S. (2016) "Unsupervised
# Representation Learning with Deep Convolutional Generative Adversarial
# Networks", *ICLR 2016*, arXiv:1511.06434. The DCGAN architecture used.

# Private helpers for the gan_an module
#' Private helpers for the gan_an module
#'
#' A step of the gan_an_native implementation. Called by \code{.gan_an_discrimination_loss_impl}, \code{.gan_an_residual_loss_impl}, \code{morie_gan_an} and 2 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x See Usage.
#' @return A vector, from \code{as.numeric}.
#' @export
.gan_an_as_num <- function(x) {
  as.numeric(x)
}

#' .gan_an_residual_loss_impl
#'
#' A step of the gan_an_native implementation. Called by \code{.gan_an_anomaly_score_impl}, \code{residual_loss}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x Passed to \code{.gan_an_as_num}.
#' @param g_z Passed to \code{.gan_an_as_num}.
#' @return A numeric value.
#' @export
.gan_an_residual_loss_impl <- function(x, g_z) {
  a <- .gan_an_as_num(x)
  b <- .gan_an_as_num(g_z)
  if (length(a) != length(b)) {
    stop("gan_an: the query and reconstruction differ in size (",
         length(a), ", ", length(b), ")")
  }
  sum(abs(a - b))
}

#' .gan_an_discrimination_loss_impl
#'
#' A step of the gan_an_native implementation. Called by \code{.gan_an_anomaly_score_impl}, \code{discrimination_loss}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param f_x Passed to \code{.gan_an_as_num}.
#' @param f_gz Passed to \code{.gan_an_as_num}.
#' @return A numeric value.
#' @export
.gan_an_discrimination_loss_impl <- function(f_x, f_gz) {
  a <- .gan_an_as_num(f_x)
  b <- .gan_an_as_num(f_gz)
  if (length(a) != length(b)) {
    stop("gan_an: the feature vectors differ in size")
  }
  if (length(a) < 2) {
    stop("gan_an: a scalar discriminator output carries no gradient; ",
         "use an intermediate feature layer")
  }
  sum(abs(a - b))
}

#' .gan_an_anomaly_score_impl
#'
#' A step of the gan_an_native implementation. Called by \code{anomaly_score}, \code{morie_gan_an}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x Passed to \code{.gan_an_residual_loss_impl}.
#' @param g_z Passed to \code{.gan_an_residual_loss_impl}.
#' @param f_x Passed to \code{.gan_an_discrimination_loss_impl}.
#' @param f_gz Passed to \code{.gan_an_discrimination_loss_impl}.
#' @param lam Defaults to \code{0.1}.
#' @return A list with \code{score}, \code{residual}, \code{discrimination}, \code{lambda}.
#' @export
.gan_an_anomaly_score_impl <- function(x, g_z, f_x, f_gz, lam = 0.1) {
  l <- as.numeric(lam)
  if (l < 0.0 || l > 1.0) {
    stop("gan_an: lambda must lie in [0,1], got ", lam)
  }
  lr <- .gan_an_residual_loss_impl(x, g_z)
  ld <- .gan_an_discrimination_loss_impl(f_x, f_gz)
  list(
    score = (1.0 - l) * lr + l * ld,
    residual = lr,
    discrimination = ld,
    lambda = l
  )
}

#' morie_gan_an
#'
#' A step of the gan_an_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x Passed to \code{.gan_an_anomaly_score_impl}.
#' @param generator See Usage.
#' @param feature_fn See Usage.
#' @param z_dim See Usage.
#' @param steps Defaults to \code{200}.
#' @param lr Defaults to \code{0.05}.
#' @param lam Passed to \code{.gan_an_anomaly_score_impl}. Defaults to \code{0.1}.
#' @param seed Defaults to \code{0}.
#' @param h Numeric; combined arithmetically in the body. Defaults to \code{1e-04}.
#' @param step_decay Defaults to \code{0.05}.
#' @return A list with \code{estimate}, \code{score}, \code{z}, \code{reconstruction}, \code{loss_history}, \code{residual}, \code{discrimination}, \code{final_step}, \code{method}, \code{note}.
#' @export
morie_gan_an <- function(x, generator, feature_fn, z_dim, steps = 200,
                         lr = 0.05, lam = 0.1, seed = 0, h = 1e-4,
                         step_decay = 0.05) {
  e <- .ghc_rng(as.integer(seed))
  z <- (.ghc_unif(e, as.integer(z_dim)) - 0.5) * 2.0

  fx <- .gan_an_as_num(feature_fn(x))
  hist <- numeric(0)

  compute_loss <- function(zv) {
    g <- generator(zv)
    res <- .gan_an_anomaly_score_impl(x, g, fx, feature_fn(g), lam)
    res$score
  }

  best_z <- z
  best <- compute_loss(z)

  steps_int <- as.integer(steps)
  for (t in seq_len(steps_int) - 1) {
    base <- compute_loss(z)
    hist <- c(hist, base)
    if (base < best) {
      best <- base
      best_z <- z
    }
    grad <- numeric(length(z))
    for (i in seq_along(z)) {
      up <- z
      up[i] <- up[i] + h
      grad[i] <- (compute_loss(up) - base) / h
    }
    eta <- as.numeric(lr) / (1.0 + t * as.numeric(step_decay))
    z <- z - eta * grad
  }

  final_loss <- compute_loss(z)
  if (final_loss < best) {
    best <- final_loss
    best_z <- z
  }

  z <- best_z
  g <- generator(z)
  fin <- .gan_an_anomaly_score_impl(x, g, fx, feature_fn(g), lam)

  final_eta <- as.numeric(lr) / (1.0 + (steps_int - 1) * as.numeric(step_decay))

  list(
    estimate = fin$score,
    score = fin$score,
    z = z,
    reconstruction = g,
    loss_history = hist,
    residual = fin$residual,
    discrimination = fin$discrimination,
    final_step = final_eta,
    method = "AnoGAN latent inversion; Schlegl et al. (2017)",
    note = "the generator is FIXED; only z moves, so an off-manifold image cannot be memorised"
  )
}

#' residual_loss
#'
#' A step of the gan_an_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x Passed to \code{.gan_an_residual_loss_impl}.
#' @param g_z Passed to \code{.gan_an_residual_loss_impl}.
#' @return The value of \code{.gan_an_residual_loss_impl}.
#' @export
residual_loss <- function(x, g_z) {
  .gan_an_residual_loss_impl(x, g_z)
}

#' discrimination_loss
#'
#' A step of the gan_an_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param f_x Passed to \code{.gan_an_discrimination_loss_impl}.
#' @param f_gz Passed to \code{.gan_an_discrimination_loss_impl}.
#' @return The value of \code{.gan_an_discrimination_loss_impl}.
#' @export
discrimination_loss <- function(f_x, f_gz) {
  .gan_an_discrimination_loss_impl(f_x, f_gz)
}

#' anomaly_score
#'
#' A step of the gan_an_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x Passed to \code{.gan_an_anomaly_score_impl}.
#' @param g_z Passed to \code{.gan_an_anomaly_score_impl}.
#' @param f_x Passed to \code{.gan_an_anomaly_score_impl}.
#' @param f_gz Passed to \code{.gan_an_anomaly_score_impl}.
#' @param lam Passed to \code{.gan_an_anomaly_score_impl}. Defaults to \code{0.1}.
#' @return The value of \code{.gan_an_anomaly_score_impl}.
#' @export
anomaly_score <- function(x, g_z, f_x, f_gz, lam = 0.1) {
  .gan_an_anomaly_score_impl(x, g_z, f_x, f_gz, lam)
}

#' residual_map
#'
#' A step of the gan_an_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x Passed to \code{.gan_an_as_num}.
#' @param g_z Passed to \code{.gan_an_as_num}.
#' @param shape Optional; may be \code{NULL}. A vector; indexed elementwise.
#' @return A list with \code{map}, \code{max}, \code{note}.
#' @export
residual_map <- function(x, g_z, shape = NULL) {
  a <- .gan_an_as_num(x)
  b <- .gan_an_as_num(g_z)
  if (length(a) != length(b)) {
    stop("gan_an: the query and reconstruction differ in size")
  }
  r <- abs(a - b)
  if (!is.null(shape)) {
    h <- as.integer(shape[1])
    w <- as.integer(shape[2])
    if (h * w != length(r)) {
      stop("gan_an: the shape ", h, "x", w, " does not match ",
           length(r), " values")
    }
    r <- matrix(r, nrow = h, ncol = w, byrow = TRUE)
  }
  list(
    map = r,
    max = max(r),
    note = "localises the anomaly rather than only flagging the image"
  )
}

#' score_separation
#'
#' A step of the gan_an_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param normal_scores Passed to \code{.gan_an_as_num}.
#' @param anomalous_scores Passed to \code{.gan_an_as_num}.
#' @return A list with \code{auc}, \code{mean_normal}, \code{mean_anomalous}, \code{separated}, \code{note}.
#' @export
score_separation <- function(normal_scores, anomalous_scores) {
  a <- .gan_an_as_num(normal_scores)
  b <- .gan_an_as_num(anomalous_scores)
  if (length(a) == 0 || length(b) == 0) {
    stop("gan_an: both populations are needed")
  }
  hits <- 0
  for (x in a) {
    for (y in b) {
      if (y > x) hits <- hits + 1
    }
  }
  auc <- hits / (length(a) * length(b))
  list(
    auc = auc,
    mean_normal = sum(a) / length(a),
    mean_anomalous = sum(b) / length(b),
    separated = auc > 0.7,
    note = "an over-capable generator reconstructs anomalies too and collapses this to 0.5"
  )
}

#' .gan_an_cheatsheet
#'
#' A step of the gan_an_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
.gan_an_cheatsheet <- function() {
  "gan_an: train a GAN on NORMAL data only, then score a query by how well it can be reproduced from that manifold. A GAN has no inverse, so find z by OPTIMISATION with the generator FIXED -- nothing adapts, so an off-manifold image stays badly reconstructed. Two losses: pixel residual, and a discrimination loss on INTERMEDIATE discriminator features (the scalar verdict would give no gradient). The residual map LOCALISES the anomaly. A generator that can reproduce anything scores everything zero -- limited capacity is load-bearing."
}

# compact alias per ledger/NAMING.md
anogan <- morie_gan_an

# public names resolved by fn/_lazy_map.json
gan_anomaly <- morie_gan_an
gananomaly <- morie_gan_an
