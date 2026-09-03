# Chinese remainder theorem solver (Sun Tzu, 4th c.; Stein 2009, Thm 2.2.2).
# Sources: Stein, W. (2009). Elementary Number Theory: Primes,
# Congruences, and Secrets, Springer, Sec. 2.2, Question 2.2.1,
# Theorem 2.2.2, Algorithm 2.2.3. Sun Tzu (Sunzi Suanjing, 4th c.),
# as quoted by Stein. Knuth, D. E. (1997). The Art of Computer
# Programming, Vol. 2, Sec. 4.3.2 (CRT in computer arithmetic, as
# cited by the stub).

#' Extended Euclid: returns (g, c, d) with c*a + d*b = g
#'
#' A step of the crtT_mixedcase_native implementation. Called by \code{crtT}.
#' See the file header for the source the module follows.
#' for the source it follows.
#'
#' @param a See Usage.
#' @param b See Usage.
#' @return A list with \code{g}, \code{c}, \code{d}.
#' @export
.crt_egcd <- function(a, b) {
  # extended Euclid: returns (g, c, d) with c*a + d*b = g
  old_r <- a
  r <- b
  old_s <- 1
  s <- 0
  old_t <- 0
  t <- 1
  while (r != 0) {
    q <- old_r %/% r
    tmp <- r
    r <- old_r - q * r
    old_r <- tmp
    tmp <- s
    s <- old_s - q * s
    old_s <- tmp
    tmp <- t
    t <- old_t - q * t
    old_t <- tmp
  }
  list(g = old_r, c = old_s, d = old_t)
}

#' crtT
#'
#' A step of the crtT_mixedcase_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' for the source it follows.
#'
#' @param residues Coerced to integer by the body, with \code{as.integer}.
#' @param moduli Coerced to integer by the body, with \code{as.integer}.
#' @return A list with \code{estimate}, \code{modulus}, \code{residues}, \code{moduli},
#' \code{method}.
#' @export
crtT <- function(residues, moduli) {
  a <- as.integer(residues)
  m <- as.integer(moduli)
  if (length(a) != length(m) || length(a) == 0L)
    stop("residues and moduli must be non-empty and paired")
  if (any(m < 2L))
    stop("moduli must be >= 2")
  x <- a[1L] %% m[1L]
  mod <- m[1L]
  if (length(a) > 1L) {
    for (k in 2:length(a)) {
      ai <- a[k]
      mi <- m[k]
      eg <- .crt_egcd(mod, mi)
      g <- eg$g
      c <- eg$c
      if (g != 1L)
        stop(sprintf("moduli must be pairwise coprime (gcd(%d, %d) = %d)",
                     mod, mi, g))
      # Algorithm 2.2.3: x_new = x + (b - x) c mod, with c mod + d mi = 1
      x <- (x + (ai - x) * c * mod) %% (mod * mi)
      mod <- mod * mi
    }
  }
  list(estimate = x,
       modulus = mod,
       residues = a,
       moduli = m,
       method = "Chinese remainder theorem (Stein Alg. 2.2.3)")
}

chinese_remainder <- crtT

morie_crtT <- crtT

#' crtT_cheatsheet
#'
#' A step of the crtT_mixedcase_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' for the source it follows.
#'
#' @return A character value.
#' @export
crtT_cheatsheet <- function() {
  "crtT: fold pairs via x + (b-x)*c*m with cm+dn=1 (ext. Euclid)"
}

# The 2026-08-11 arm of this module was a second
# implementation of the same paper; it has been removed and
# its exported name kept as an alias. The formals were
# identical, so this is exact and the man page still applies.
morie_crtt <- morie_crtT
