# Chinese remainder theorem solver.
# Source: Stein (2009), Elementary Number Theory, Sec. 2.2, Question
# 2.2.1 (Sun Tzu's problem), Theorem 2.2.2, Algorithm 2.2.3
# (fetched-wave3/stein-2009-elementary-number-theory.pdf); Knuth,
# TAOCP Vol. 2, Sec. 4.3.2.  Mirrors Python morie.fn.crtT exactly.
# Exact arithmetic via base R bignum-free integers: doubles are exact
# below 2^53, so moduli products are limited to that range (checked).

.crtt_egcd <- function(a, b) {
  old_r <- a; r <- b
  old_s <- 1; s <- 0
  old_t <- 0; t_ <- 1
  while (r != 0) {
    q <- old_r %/% r
    tmp <- old_r - q * r; old_r <- r; r <- tmp
    tmp <- old_s - q * s; old_s <- s; s <- tmp
    tmp <- old_t - q * t_; old_t <- t_; t_ <- tmp
  }
  c(old_r, old_s, old_t)
}

#' Chinese remainder theorem
#'
#' Solves x = a_i (mod m_i) for pairwise coprime moduli by folding
#' pairs with Stein's Algorithm 2.2.3 (x = a + (b - a) c m where
#' c m + d n = 1 from the extended Euclidean algorithm); the solution
#' is unique modulo prod(m_i).  Exact for products below 2^53.
#'
#' @param residues Integer vector a_i.
#' @param moduli Pairwise coprime integer moduli (each >= 2).
#' @return A list with elements \code{estimate} (least non-negative
#'   solution), \code{modulus}, \code{residues}, \code{moduli},
#'   \code{method}.
#' @references Stein, W. (2009). Elementary Number Theory: Primes,
#'   Congruences, and Secrets. Springer, Sec. 2.2.  Knuth, D. E.
#'   (1997). The Art of Computer Programming, Vol. 2, Sec. 4.3.2.
#' @export
morie_crtt <- function(residues, moduli) {
  a <- as.numeric(residues)
  m <- as.numeric(moduli)
  if (length(a) != length(m) || !length(a)) {
    stop("residues and moduli must be non-empty and paired")
  }
  if (any(m < 2)) stop("moduli must be >= 2")
  if (prod(m) >= 2^53) stop("product of moduli exceeds exact double range")
  if (any(m >= 2^26)) {
    # keep every intermediate product below 2^53: t is reduced mod m_i
    # before multiplying, so the largest product is m_i^2 < 2^52
    stop("each modulus must be below 2^26 for exact double arithmetic")
  }
  x <- a[1] %% m[1]
  mod <- m[1]
  for (i in seq_along(a)[-1]) {
    eg <- .crtt_egcd(mod, m[i])
    if (eg[1] != 1) {
      stop(sprintf("moduli must be pairwise coprime (gcd(%d, %d) = %d)",
                   mod, m[i], eg[1]))
    }
    # t = (a_i - x) c mod m_i, all factors reduced first so products
    # stay exact; then x_new = x + t * mod < mod * m_i < 2^53
    t_ <- ((((a[i] - x) %% m[i]) * (eg[2] %% m[i])) %% m[i])
    x <- (x + t_ * mod) %% (mod * m[i])
    mod <- mod * m[i]
  }
  list(estimate = x, modulus = mod, residues = a, moduli = m,
       method = "Chinese remainder theorem (Stein Alg. 2.2.3)")
}
