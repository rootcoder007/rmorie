# morie.fn -- function file (rootcoder007/morie), R arm.
# Sources: Rezende, D. J. and Mohamed, S. (2015), Variational Inference
# with Normalizing Flows, ICML 2015 (PMLR 37, 1530-1538;
# arXiv:1505.05770) -- the change of variables with the
# log-determinant correction, planar and radial flows with O(d)
# determinants, and the invertibility condition u'w >= -1 on the
# planar flow parameters; Kucukelbir, A., Tran, D., Ranganath, R.,
# Gelman, A. and Blei, D. M. (2017), Automatic Differentiation
# Variational Inference, JMLR 18(14), 1-45 (arXiv:1603.00788) --
# mapping constrained latents to R^K, fitting a Gaussian there, and
# correcting with the Jacobian; Blei, D. M., Kucukelbir, A. and
# McAuliffe, J. D. (2017), Variational Inference: A Review for
# Statisticians, JASA 112(518), 859-877 (the ELBO as the common
# yardstick).
#
# Native R implementation mirroring Python morie.fn.baynav exactly:
# the same planar flow z + u*tanh(w'z + b), the same matrix
# determinant lemma result |1 + u'psi(z)|, the same projection of u
# to keep u'w >= -1, the same ADVI bijections to R with their
# log-Jacobians, and the same Monte Carlo ELBO with the same
# divisor-by-(n-1) variance.

#' Variational inference: normalizing flows and automatic
#' differentiation VI
#'
#' Two routes to the same bound. \code{planar_flow},
#' \code{enforce_invertibility} and \code{flow_log_density} are the
#' normalizing-flow arm of Rezende and Mohamed (2015): a simple base
#' density is pushed through invertible maps, the change of variables
#' subtracts the log-determinants, and a planar flow's rank-one
#' Jacobian is computable in O(d) by the matrix determinant lemma.
#' \code{transform_to_real} is the ADVI arm of Kucukelbir et al.
#' (2017): constrained latents are mapped to \code{R^K} by a
#' bijection, a Gaussian is fitted there, and the density is
#' corrected by the log-Jacobian. \code{elbo} is the common yardstick
#' (Blei et al. 2017) -- both approaches optimise it, so a flow of
#' depth 0 must equal plain mean-field.
#'
#' @param u,w,z Numeric vectors of equal length.
#' @param b Numeric scalar.
#' @param value Numeric scalar for \code{transform_to_real}.
#' @param support One of \code{"positive"}, \code{"unit"},
#'   \code{"real"}.
#' @param eps Positive tolerance; default \code{1e-10}. Not a tuning
#'   knob for the R arm.
#' @param z0 Numeric vector; the base sample.
#' @param log_q0 Numeric scalar; the base log-density at \code{z0}.
#' @param layers List of \code{list(u, w, b)} triples, one per
#'   planar-flow layer.
#' @param log_joint,log_q Functions of a sample, each returning a
#'   numeric scalar.
#' @param samples List of samples to evaluate the bound on.
#' @return A named list whose element names match the Python payload
#'   keys. \code{enforce_invertibility} returns
#'   \code{u, adjusted, u_dot_w, u_dot_w_after, note};
#'   \code{planar_flow} returns
#'   \code{z, log_det, det, invertibility_adjusted, note};
#'   \code{flow_log_density} returns
#'   \code{estimate, log_q, z, log_dets, depth, method, note};
#'   \code{transform_to_real} returns
#'   \code{real, log_jacobian, inverse};
#'   \code{elbo} returns \code{elbo, se, n_samples, note}.
#' @references Rezende, D. J. and Mohamed, S. (2015). Variational
#'   Inference with Normalizing Flows. ICML 2015, PMLR 37,
#'   1530-1538. Kucukelbir, A. et al. (2017). Automatic
#'   Differentiation Variational Inference. JMLR 18(14), 1-45.
#'   Blei, D. M., Kucukelbir, A. and McAuliffe, J. D. (2017).
#'   Variational Inference: A Review for Statisticians. JASA
#'   112(518), 859-877.
#' @export
morie_baynav <- function(u, w, b, value, support, eps,
                         z0, log_q0, layers,
                         log_joint, log_q, samples) {
  # The Python arm exposes five public functions and a cheatsheet.
  # Mirror the maths and the argument names exactly; mirror every
  # error condition so both arms refuse the same inputs. None of
  # these functions draw random numbers, so the shared RNG helpers
  # are not required.
  stop("morie_baynav is a namespace; call enforce_invertibility, ",
       "planar_flow, flow_log_density, transform_to_real or elbo ",
       "directly")
}

#' Project \code{u} so that \code{u'w >= -1}
#'
#' Outside this region the planar flow is not invertible and the
#' change-of-variables formula is simply wrong, so the reported bound
#' is not a bound. The projection follows the Python arm exactly.
#'
#' @param u,w Numeric vectors of equal length.
#' @return A list with \code{u, adjusted, u_dot_w, u_dot_w_after, note}.
#' @export
enforce_invertibility <- function(u, w) {
  uv <- as.numeric(u); wv <- as.numeric(w)
  if (length(uv) != length(wv))
    stop("baynav: u and w differ in length")
  uw <- sum(uv * wv)
  if (uw >= -1.0) {
    return(list(u = uv, adjusted = FALSE, u_dot_w = uw))
  }
  m <- -1.0 + log1p(exp(uw))
  nw <- sum(wv * wv)
  if (nw <= 1e-12)
    stop(paste0("baynav: w is zero, so the flow is degenerate"))
  out <- uv + (m - uw) * wv / nw
  list(u = out, adjusted = TRUE, u_dot_w = uw,
       u_dot_w_after = sum(out * wv),
       note = "u'w >= -1 is required for invertibility")
}

#' Planar flow \code{z + u*tanh(w'z + b)} with its log-determinant
#'
#' The Jacobian is rank-one, so the matrix determinant lemma gives
#' \code{|1 + u'psi(z)|} in O(d) rather than O(d^3).
#'
#' @param z,u,w Numeric vectors of equal length.
#' @param b Numeric scalar.
#' @return A list with \code{z, log_det, det, invertibility_adjusted,
#'   note}.
#' @export
planar_flow <- function(z, u, w, b) {
  zv <- as.numeric(z)
  fixed <- enforce_invertibility(u, w)
  uv <- fixed$u
  wv <- as.numeric(w)
  a <- sum(wv * zv) + as.numeric(b)
  t <- tanh(a)
  out <- zv + uv * t
  dt <- 1.0 - t * t
  psi <- dt * wv
  det <- 1.0 + sum(uv * psi)
  list(z = out, log_det = log(max(abs(det), 1e-12)), det = det,
       invertibility_adjusted = fixed$adjusted,
       note = "rank-one Jacobian, so the determinant is O(d)")
}

#' Push a sample through a flow and correct the density
#'
#' \code{log q_K = log q_0 - sum_k log|det J_k|}. The subtraction
#' is the whole content of the change of variables. Depth 0 leaves
#' the density untouched, which is the mean-field case.
#'
#' @param z0 Numeric vector; the base sample.
#' @param log_q0 Numeric scalar; the base log-density at \code{z0}.
#' @param layers List of \code{list(u, w, b)} triples.
#' @return A list with \code{estimate, log_q, z, log_dets, depth,
#'   method, note}.
#' @export
flow_log_density <- function(z0, log_q0, layers) {
  z <- as.numeric(z0)
  lq <- as.numeric(log_q0)
  dets <- numeric(0)
  for (layer in layers) {
    r <- planar_flow(z, layer$u, layer$w, layer$b)
    z <- r$z
    dets <- c(dets, r$log_det)
    lq <- lq - r$log_det
  }
  list(estimate = lq, log_q = lq, z = z, log_dets = dets,
       depth = length(layers),
       method = "normalizing flow; Rezende & Mohamed (2015)",
       note = paste0("depth 0 leaves the density untouched, which ",
                     "is the mean-field case"))
}

#' ADVI's bijection to \code{R} with its log-Jacobian
#'
#' Fitting a Gaussian to a positive parameter directly puts mass on
#' impossible values; transforming first is what makes one automatic
#' recipe cover every model.
#'
#' @param value Numeric scalar.
#' @param support One of \code{"positive"}, \code{"unit"},
#'   \code{"real"}.
#' @param eps Positive tolerance; default \code{1e-10}. Not a tuning
#'   knob for the R arm.
#' @return A list with \code{real, log_jacobian, inverse}.
#' @export
transform_to_real <- function(value, support = "positive", eps = 1e-10) {
  # The Python arm's `eps` is accepted but only used to guard
  # against log(0) for positive parameters; the R guard is
  # `v <= 0.0`, mirroring the Python `if v <= 0.0` check, so the
  # tolerance is not consulted. Pass it through to keep the signature.
  eps <- as.numeric(eps)
  v <- as.numeric(value)
  if (identical(support, "positive")) {
    if (v <= 0.0)
      stop(paste0("baynav: a positive parameter must be positive, ",
                  "got ", format(v)))
    return(list(real = log(v), log_jacobian = -log(v),
                inverse = exp(log(v))))
  }
  if (identical(support, "unit")) {
    if (!(v > 0.0 && v < 1.0))
      stop(paste0("baynav: a unit parameter must lie in (0,1), got ",
                  "format(v)"))
    z <- log(v / (1.0 - v))
    return(list(real = z,
                log_jacobian = -log(v) - log(1.0 - v),
                inverse = 1.0 / (1.0 + exp(-z))))
  }
  if (identical(support, "real"))
    return(list(real = v, log_jacobian = 0.0, inverse = v))
  stop(paste0("baynav: support must be positive, unit or real, got ",
              "format(support)"))
}

#' Monte Carlo ELBO
#'
#' \code{E_q\[log p(x,z) - log q(z)\]}. The common yardstick: both
#' approaches optimise it, so a deeper flow must not lower it. The
#' variance uses the (n-1) denominator so both arms agree to the
#' last bit when the same samples are passed in.
#'
#' @param log_joint,log_q Functions of a sample, each returning a
#'   numeric scalar.
#' @param samples List of samples.
#' @return A list with \code{elbo, se, n_samples, note}.
#' @export
elbo <- function(log_joint, log_q, samples) {
  if (length(samples) == 0L)
    stop("baynav: no samples given")
  vals <- vapply(samples, function(s)
    as.numeric(log_joint(s)) - as.numeric(log_q(s)), numeric(1))
  m <- sum(vals) / length(vals)
  var <- sum((vals - m)^2) / max(length(vals) - 1L, 1L)
  list(elbo = m, se = sqrt(var / length(vals)),
       n_samples = length(vals),
       note = paste0("a lower bound on the log evidence; enriching ",
                     "the family can only raise it"))
}
