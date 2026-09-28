#' Coding-theory results from MacKay
#'
#' \code{BoundedDistanceNoise}: Shannon versus bounded-distance noise limits
#' of a rate-R code. \code{SelfDualCodeCheck}: is G = (I | t(P)) self-dual
#' (P orthogonal mod 2). \code{RunlengthCapacity}: capacity of the channel
#' forbidding runs of more than L ones. \code{RepetitionErrorApprox}:
#' leading-order repetition-code error. \code{RobustSoliton}: Luby's robust
#' soliton degree distribution. \code{CodeLengthDecomposition}: L = H + KL -
#' log2 z. \code{McGillInteractionInformation}: I(X;Y|Z) - I(X;Y).
#' Identical to the Python arm \code{morie.fn.infocodes}.
#'
#' @param rate Code rate in (0, 1).
#' @param P K by K binary matrix.
#' @param L Maximum run length.
#' @param N Blocklength (odd).
#' @param f Bit-flip probability in (0, 1/2).
#' @param target Optional target error probability.
#' @param K Number of source packets.
#' @param c,delta Robust soliton constants.
#' @param p Symbol probabilities.
#' @param lengths Codeword lengths.
#' @param pxyz Three-way array of joint probabilities or counts.
#' @return List.
#' @references MacKay, D. J. C. (2003). Information Theory, Inference, and
#'   Learning Algorithms. Cambridge University Press.
#'
#'   Luby, M. (2002). LT codes. Proceedings of the 43rd IEEE Symposium on
#'   Foundations of Computer Science, 271-280.
#'
#'   McGill, W. J. (1954). Multivariate information transmission.
#'   Psychometrika 19, 97-116.
#' @examples
#' BoundedDistanceNoise(0.5)
#' RobustSoliton(10000, 0.2, 0.05)$S
#' CodeLengthDecomposition(c(0.5, 0.25, 0.125, 0.125), c(1, 2, 3, 3))$L
#' @export
BoundedDistanceNoise <- function(rate) {
  if (rate <= 0 || rate >= 1) stop("rate must lie in (0, 1)")
  h2 <- function(f) -f * log2(f) - (1 - f) * log2(1 - f)
  lo <- 1e-300
  hi <- 0.5
  for (it in 1:200) {
    mid <- 0.5 * (lo + hi)
    if (h2(mid) < 1 - rate) lo <- mid else hi <- mid
  }
  f <- 0.5 * (lo + hi)
  list(f_shannon = f, f_bd = f / 2)
}

#' @rdname BoundedDistanceNoise
#' @export
SelfDualCodeCheck <- function(P) {
  P <- as.matrix(P) %% 2
  K <- nrow(P)
  if (ncol(P) != K) stop("P must be K x K")
  ptp <- (t(P) %*% P) %% 2
  G <- cbind(diag(K), t(P))
  ggt <- (G %*% t(G)) %% 2
  list(self_dual = all(ptp == diag(K)), PtP = ptp, GGt = ggt)
}

.ic_ssum <- function(v) {
  s <- 0
  for (a in v) s <- s + a
  s
}

#' @rdname BoundedDistanceNoise
#' @export
RunlengthCapacity <- function(L) {
  if (L < 1) stop("L must be >= 1")
  lo <- 0
  hi <- 1
  for (it in 1:200) {
    mid <- 0.5 * (lo + hi)
    if (.ic_ssum(2^(-mid * seq_len(L + 1))) > 1) lo <- mid else hi <- mid
  }
  list(capacity = 0.5 * (lo + hi), simple_rate = 1 / (1 + 2^-L))
}

#' @rdname BoundedDistanceNoise
#' @export
RepetitionErrorApprox <- function(N, f, target = NULL) {
  if (f <= 0 || f >= 0.5) stop("f must lie in (0, 1/2)")
  q <- 4 * f * (1 - f)
  out <- list(pb = q^(N / 2))
  if (!is.null(target)) out$n_target <- 2 * log(target) / log(q)
  out
}

#' @rdname BoundedDistanceNoise
#' @export
RobustSoliton <- function(K, c = 0.1, delta = 0.5) {
  if (K < 2 || c <= 0 || delta <= 0 || delta >= 1) stop("need K >= 2, c > 0 and 0 < delta < 1")
  S <- c * log(K / delta) * sqrt(K)
  m <- floor(K / S + 0.5)
  if (m < 1 || m > K) stop("K / S must round to a degree in 1..K")
  d <- seq_len(K)
  rho <- c(1 / K, 1 / (d[-1] * (d[-1] - 1)))
  tau <- numeric(K)
  if (m > 1) tau[seq_len(m - 1)] <- S / (K * seq_len(m - 1))
  tau[m] <- S * log(S / delta) / K
  Z <- .ic_ssum(rho + tau)
  mu <- (rho + tau) / Z
  list(S = S, m = as.integer(m), Z = Z, rho = rho, tau = tau, mu = mu, mean_degree = .ic_ssum(d * mu))
}

#' @rdname BoundedDistanceNoise
#' @export
CodeLengthDecomposition <- function(p, lengths) {
  p <- as.numeric(p)
  ln <- as.numeric(lengths)
  if (length(p) != length(ln) || min(p) < 0) stop("p and lengths must match and p must be non-negative")
  p <- p / .ic_ssum(p)
  z <- .ic_ssum(2^-ln)
  q <- 2^-ln / z
  pos <- p > 0
  list(L = .ic_ssum(p * ln), H = .ic_ssum(-p[pos] * log2(p[pos])),
       kl = .ic_ssum(p[pos] * log2(p[pos] / q[pos])), kraft = z, q = q)
}

.ic_mi <- function(pxy) {
  px <- vapply(seq_len(nrow(pxy)), function(i) .ic_ssum(pxy[i, ]), 0)
  py <- vapply(seq_len(ncol(pxy)), function(j) .ic_ssum(pxy[, j]), 0)
  s <- 0
  for (i in seq_len(nrow(pxy))) {
    for (j in seq_len(ncol(pxy))) {
      if (pxy[i, j] > 0) s <- s + pxy[i, j] * log2(pxy[i, j] / (px[i] * py[j]))
    }
  }
  s
}

#' @rdname BoundedDistanceNoise
#' @export
McGillInteractionInformation <- function(pxyz) {
  P <- pxyz + 0
  d <- dim(P)
  tot <- 0
  for (i in seq_len(d[1])) for (j in seq_len(d[2])) for (k in seq_len(d[3])) tot <- tot + P[i, j, k]
  P <- P / tot
  pxy <- matrix(0, d[1], d[2])
  for (i in seq_len(d[1])) for (j in seq_len(d[2])) pxy[i, j] <- .ic_ssum(P[i, j, ])
  ixy <- .ic_mi(pxy)
  icond <- 0
  for (k in seq_len(d[3])) {
    pz <- 0
    for (i in seq_len(d[1])) for (j in seq_len(d[2])) pz <- pz + P[i, j, k]
    if (pz > 0) icond <- icond + pz * .ic_mi(matrix(P[, , k], d[1], d[2]) / pz)
  }
  list(ii = icond - ixy, i_xy = ixy, i_xy_given_z = icond)
}
