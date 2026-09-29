.win_scale <- function(method, fi, fo) {
  switch(method, xavier_uniform = sqrt(6 / (fi + fo)), xavier_normal = sqrt(2 / (fi + fo)),
         he_uniform = sqrt(6 / fi), he_normal = sqrt(2 / fi), lecun_normal = sqrt(1 / fi), NA)
}

.win_var <- function(method, fi, fo) {
  switch(method, xavier_uniform = 2 / (fi + fo), xavier_normal = 2 / (fi + fo), he_uniform = 2 / fi,
         he_normal = 2 / fi, lecun_normal = 1 / fi, orthogonal = 1 / max(fi, fo), NA)
}

.win_mgs <- function(M) {
  Q <- M
  for (j in seq_len(ncol(M))) {
    for (k in seq_len(j - 1)) Q[, j] <- Q[, j] - sum(Q[, k] * Q[, j]) * Q[, k]
    Q[, j] <- Q[, j] / sqrt(sum(Q[, j]^2))
  }
  Q
}

.win_draw <- function(rows, cols, method, gain, seed, fi, fo) {
  k <- rows * cols
  if (method %in% c("xavier_uniform", "he_uniform")) {
    a <- gain * .win_scale(method, fi, fo)
    v <- -a + 2 * a * .morie_random_uniform(k, seed = seed)
  } else if (method %in% c("xavier_normal", "he_normal", "lecun_normal")) {
    v <- gain * .win_scale(method, fi, fo) * .morie_random_normal(k, seed = seed)
  } else if (method == "orthogonal") {
    M <- matrix(.morie_random_normal(k, seed = seed), rows, cols, byrow = TRUE)
    Q <- if (rows >= cols) .win_mgs(M) else t(.win_mgs(t(M)))
    return(gain * Q)
  } else {
    stop("Unknown method: ", method)
  }
  matrix(v, rows, cols, byrow = TRUE)
}

.win_moments <- function(W) c(mean(W), mean((W - mean(W))^2))

#' Neural network weight initialisation (Philox)
#'
#' R arm of \code{morie.fn.vctrs}, \code{xavir} and \code{xvrig}; the draws
#' come from the Philox generator, row by row, so both arms give the same
#' matrices. \code{WeightInit}: Xavier/Glorot, He/Kaiming and LeCun uniform
#' or normal initialisation and orthogonal initialisation (modified
#' Gram-Schmidt of a Gaussian matrix). \code{Xavir}: Xavier matrix fan_in by
#' fan_out. \code{Xvrig}: Xavier matrix fan_out by fan_in.
#'
#' @param fan_in,fan_out Layer sizes.
#' @param method Initialisation method.
#' @param gain Scaling factor.
#' @param seed Philox seed.
#' @param uniform Uniform (TRUE) or normal.
#' @param distribution \code{"normal"} or \code{"uniform"}.
#' @return A list with the weights and their moments.
#' @references Glorot, X. and Bengio, Y. (2010). Understanding the
#'   difficulty of training deep feedforward neural networks. AISTATS,
#'   249-256.
#'
#'   He, K., Zhang, X., Ren, S. and Sun, J. (2015). Delving deep into
#'   rectifiers. ICCV, 1026-1034.
#'
#'   Saxe, A. M., McClelland, J. L. and Ganguli, S. (2014). Exact solutions to
#'   the nonlinear dynamics of learning in deep linear neural networks.
#'   ICLR.
#' @examples
#' WeightInit(3, 2, seed = 1)$value
#' crossprod(WeightInit(5, 3, method = "orthogonal")$value)
#' @export
WeightInit <- function(fan_in, fan_out, method = "xavier_uniform", gain = 1, seed = 42) {
  if (fan_in < 1 || fan_out < 1) stop("fan_in and fan_out must be positive")
  if (is.na(.win_var(method, fan_in, fan_out))) stop("Unknown method: ", method)
  W <- .win_draw(fan_in, fan_out, method, gain, seed, fan_in, fan_out)
  list(value = W, method = method, fan_in = fan_in, fan_out = fan_out, variance = .win_moments(W)[2],
       expected_variance = gain^2 * .win_var(method, fan_in, fan_out), gain = gain)
}

#' @rdname WeightInit
#' @export
Xavir <- function(fan_in, fan_out, seed = 42, uniform = TRUE) {
  if (fan_in <= 0 || fan_out <= 0) stop("fan_in and fan_out must be > 0")
  W <- .win_draw(fan_in, fan_out, if (uniform) "xavier_uniform" else "xavier_normal", 1, seed, fan_in, fan_out)
  mv <- .win_moments(W)
  list(value = sqrt(mv[2]), weights = W, fan_in = fan_in, fan_out = fan_out, mean = mv[1], std = sqrt(mv[2]),
       shape = c(fan_in, fan_out), method = if (uniform) "uniform" else "normal")
}

#' @rdname WeightInit
#' @export
Xvrig <- function(fan_in, fan_out, distribution = "normal", seed = NULL) {
  if (fan_in <= 0 || fan_out <= 0) stop("fan_in and fan_out must be positive")
  if (!distribution %in% c("normal", "uniform")) stop("distribution must be 'normal' or 'uniform'")
  m <- if (distribution == "normal") "xavier_normal" else "xavier_uniform"
  W <- .win_draw(fan_out, fan_in, m, 1, if (is.null(seed)) 0 else seed, fan_in, fan_out)
  list(value = sqrt(.win_moments(W)[2]), weights = W, fan_in = fan_in, fan_out = fan_out, distribution = distribution)
}
