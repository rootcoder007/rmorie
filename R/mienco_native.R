# mienco.fn -- function file (rootcoder007/morie)
# Deep InfoMax: maximise mutual information, but locally.
#
# Learn a representation without labels by maximising the mutual
# information between the input and its encoding. The obvious reading of
# that -- maximise I(X; E_psi(X)) for the whole image -- is
# weaker than it sounds: a representation can capture the global
# statistics and still be useless for anything that depends on
# *structure*, and mutual information alone is invariant to any
# bijection, so nothing forces the representation to be organised.
#
# So maximise it locally, and average. Deep InfoMax's central result
# is that maximising the *average* MI between the global summary and
# **local patches** of the feature map works far better for downstream
# tasks than the global objective alone. A feature that must predict
# every patch cannot describe only what is common; it has to encode
# content shared across the image, which is what a classifier wants.
#
# The estimator is a discriminator, not an integral. MI is estimated
# in the Donsker-Varadhan / Jensen-Shannon family by discriminating
# *paired* samples -- a patch and the summary from the same image --
# from *unpaired* ones drawn from a different image. The JSD form,
#
#     I^JSD = E_P[-sp(-T(x, E(x)))] - E_{P x ~P}[sp(T(x', E(x)))],
#
# with sp(z) = log(1+e^z), is bounded and behaves
# better in practice than the DV form, whose value is unbounded and
# whose gradient is high-variance. Both are implemented; the anchor shows
# the JSD estimator staying finite where DV runs away.
#
# A prior on the representation is a separate knob. Matching the
# encoding to a prior distribution adversarially controls *how* the
# information is stored -- compactness, independence -- which the MI term
# alone does not constrain at all.
#
# References
# ----------
# Hjelm, R. D., Fedorov, A., Lavoie-Marchildon, S., Grewal, K.,
# Bachman, P., Trischler, A. & Bengio, Y. (2019) "Learning deep
# representations by mutual information estimation and maximization",
# International Conference on Learning Representations (ICLR 2019),
# arXiv:1808.06670. The abstract and Sec. 1-3: maximising mutual
# information between the input and the output of a deep encoder;
# structure matters, and maximising the AVERAGE MI between the global
# representation and LOCAL patches greatly improves representation
# quality for downstream tasks compared with the global objective; the
# Donsker-Varadhan and Jensen-Shannon estimators built from a
# discriminator over paired versus unpaired samples; and matching the
# representation to a prior to control its characteristics.
#
# Belghazi, M. I., Baratin, A., Rajeswar, S., Ozair, S., Bengio, Y.,
# Courville, A. & Hjelm, R. D. (2018) "Mutual Information Neural
# Estimation", ICML 2018, PMLR 80, 531-540, arXiv:1801.04062. The
# Donsker-Varadhan estimator.
#
# Zhu, Y., Xu, Y., Yu, F., Liu, Q., Wu, S. & Wang, L. (2020) "Deep
# Graph Contrastive Representation Learning", arXiv:2006.04131. The
# graph-domain descendant; implemented in :mod:`grace`.

#' .mienco_softplus
#'
#' A step of the mienco_native implementation. Called by \code{.mienco_jsd_estimate},
#' \code{.mienco_prior_matching_loss}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param z Coerced to numeric by the body, with \code{as.numeric}.
#' @return The value of \code{ifelse}.
#' @export
#' @examples
#' y <- c(2.9, 5.1, 6.8, 9.4, 11.2, 13.1, 15.0, 17.6)
#' res <- .mienco_softplus(z = y)
#' res
.mienco_softplus <- function(z) {
  v <- as.numeric(z)
  ifelse(v > 0, v + log1p(exp(-v)), log1p(exp(v)))
}

#' .mienco_jsd_estimate
#'
#' A step of the mienco_native implementation. Called by \code{morie_mienco}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param paired Coerced to numeric by the body, with \code{as.numeric}.
#' @param unpaired Coerced to numeric by the body, with \code{as.numeric}.
#' @return A numeric value.
#' @export
.mienco_jsd_estimate <- function(paired, unpaired) {
  p <- as.numeric(paired)
  q <- as.numeric(unpaired)
  if (length(p) == 0 || length(q) == 0) {
    stop("mienco: both paired and unpaired scores are needed")
  }
  mean(-.mienco_softplus(-p)) - mean(.mienco_softplus(q))
}

#' .mienco_dv_estimate
#'
#' A step of the mienco_native implementation. Called by \code{morie_mienco}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param paired Coerced to numeric by the body, with \code{as.numeric}.
#' @param unpaired Coerced to numeric by the body, with \code{as.numeric}.
#' @return A numeric value.
#' @export
.mienco_dv_estimate <- function(paired, unpaired) {
  p <- as.numeric(paired)
  q <- as.numeric(unpaired)
  m <- max(q)
  lse <- m + log(sum(exp(q - m)) / length(q))
  mean(p) - lse
}

#' morie_mienco
#'
#' A step of the mienco_native implementation. Called by \code{.mienco_global_objective}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param summary Passed to \code{critic}.
#' @param patches A vector; its length is taken.
#' @param other_patches A vector; its length is taken.
#' @param critic Accepted by the signature and not used anywhere in the body.
#' @param estimator Compared against \code{"jsd"}. Defaults to \code{"jsd"}.
#' @return A list with \code{estimate}, \code{mi_lower_bound}, \code{estimator},
#' \code{n_patches}, \code{n_negative_patches}, \code{method}, \code{note}.
#' @export
morie_mienco <- function(summary, patches, other_patches, critic, estimator = "jsd") {
  estimators <- c("jsd", "dv")
  if (!(estimator %in% estimators)) {
    stop(sprintf("mienco: estimator must be one of %s, got '%s'",
                 paste(estimators, collapse = ", "),
                 estimator))
  }
  pos <- sapply(patches, function(p) critic(summary, p))
  neg <- sapply(other_patches, function(p) critic(summary, p))
  est <- if (estimator == "jsd") {
    .mienco_jsd_estimate(pos, neg)
  } else {
    .mienco_dv_estimate(pos, neg)
  }
  list(
    estimate = est,
    mi_lower_bound = est,
    estimator = estimator,
    n_patches = length(patches),
    n_negative_patches = length(other_patches),
    method = "local Deep InfoMax; Hjelm et al. (2019)",
    note = "averaging over LOCAL patches beats the global objective for downstream tasks"
  )
}

#' .mienco_global_objective
#'
#' A step of the mienco_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param summary Passed to \code{morie_mienco}.
#' @param whole Carried through into a list the body builds.
#' @param other_whole Carried through into a list the body builds.
#' @param critic Passed to \code{morie_mienco}.
#' @param estimator Passed to \code{morie_mienco}. Defaults to \code{"jsd"}.
#' @return The value of \code{morie_mienco}.
#' @export
.mienco_global_objective <- function(summary, whole, other_whole, critic, estimator = "jsd") {
  morie_mienco(summary, list(whole), list(other_whole), critic, estimator)
}

#' .mienco_prior_matching_loss
#'
#' A step of the mienco_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param samples Iterated over elementwise, with \code{sapply}.
#' @param prior_samples Iterated over elementwise, with \code{sapply}.
#' @param discriminator Accepted by the signature and not used anywhere in the body.
#' @return A numeric value.
#' @export
.mienco_prior_matching_loss <- function(samples, prior_samples, discriminator) {
  a <- sapply(samples, function(s) as.numeric(discriminator(s)))
  b <- sapply(prior_samples, function(s) as.numeric(discriminator(s)))
  if (length(a) == 0 || length(b) == 0) {
    stop("mienco: both encoded and prior samples are needed")
  }
  mean(.mienco_softplus(-b)) + mean(.mienco_softplus(a))
}

#' .mienco_cheatsheet
#'
#' A step of the mienco_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
#' @examples
#' res <- .mienco_cheatsheet()
#' res
.mienco_cheatsheet <- function() {
  paste0("mienco: unsupervised representations by maximising mutual ",
         "information -- but GLOBAL MI is weak, since MI is ",
         "invariant to any bijection and a summary can capture ",
         "global statistics while encoding no structure. The ",
         "central result: maximise the AVERAGE MI between the ",
         "summary and LOCAL PATCHES. MI is estimated by a ",
         "discriminator separating paired from unpaired samples; ",
         "the JSD form is BOUNDED where Donsker-Varadhan is not. A ",
         "prior-matching term separately controls how the ",
         "information is stored.")
}

# compact alias per ledger/NAMING.md
deepinfomax <- morie_mienco

# public names resolved by fn/_lazy_map.json
mi_neural_encoder <- morie_mienco
