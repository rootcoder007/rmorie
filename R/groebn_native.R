# Buchberger's algorithm for Gröbner bases.
#
# Source: morie.fn.groebn (Buchberger 1965 PhD thesis -- the
# S-polynomial, the all-S-polynomials-reduce-to-zero criterion, the
# completion loop, and the termination argument via Dickson's lemma).
# Buchberger 1979 (EUROSAM '79, Springer LNCS 72) is the first
# criterion: a pair whose leading monomials are coprime always
# reduces to zero and can be skipped; the implementation honours
# `prune = TRUE` for that. The reduced basis -- monic, with no term
# of any element divisible by the leading term of another -- is
# unique for a given ideal and order; `reduce_basis` produces it
# and the comparison anchor in this repository checks it.
#
# Polynomials are named lists where names are exponent-tuple strings
# (e.g. "2_1_0") and values are length-2 integer vectors representing
# exact rationals (num, den). This mirrors Python's
# {exp_tuple: Fraction} representation exactly.

# ---------------------------------------------------------------------------
# Exact rationals (Fraction-equivalent)
# ---------------------------------------------------------------------------
#' .groebn_fr
#'
#' Exact rationals (Fraction-equivalent)
#' ---------------------------------------------------------------------------
#'
#' @param num Numeric; passed to \code{abs}.
#' @param den Numeric; combined arithmetically in the body. Defaults to \code{1L}.
#' @return A vector, from \code{c}.
#' @export
.groebn_fr <- function(num, den = 1L) {
  num <- as.integer(num)
  den <- as.integer(den)
  if (den < 0L) { num <- -num; den <- -den }
  if (den == 0L) stop("groebn: division by zero in rational")
  if (num == 0L) return(c(0L, 1L))
  g <- .groebn_gcd(abs(num), den)
  c(num %/% g, den %/% g)
}

#' .groebn_gcd
#'
#' A step of the groebn_native implementation. Called by \code{.groebn_fr}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param a Numeric; combined arithmetically in the body.
#' @param b Numeric; combined arithmetically in the body.
#' @return One of two values, depending on the branch taken.
#' @export
.groebn_gcd <- function(a, b) {
  a <- as.integer(a); b <- as.integer(b)
  while (b != 0L) { t <- b; b <- a %% b; a <- t }
  if (a < 0L) -a else a
}

#' .groebn_fr_add
#'
#' A step of the groebn_native implementation. Called by \code{.groebn_add}, \code{.groebn_mul}, \code{.groebn_poly}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param p A vector; indexed elementwise.
#' @param q A vector; indexed elementwise.
#' @return The value of \code{.groebn_fr}.
#' @export
.groebn_fr_add <- function(p, q) {
  .groebn_fr(p[1L] * q[2L] + q[1L] * p[2L], p[2L] * q[2L])
}

#' .groebn_fr_sub
#'
#' A step of the groebn_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param p A vector; indexed elementwise.
#' @param q A vector; indexed elementwise.
#' @return The value of \code{.groebn_fr}.
#' @export
.groebn_fr_sub <- function(p, q) {
  .groebn_fr(p[1L] * q[2L] - q[1L] * p[2L], p[2L] * q[2L])
}

#' .groebn_fr_neg
#'
#' A step of the groebn_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param p A vector; indexed elementwise.
#' @return A vector, from \code{c}.
#' @export
.groebn_fr_neg <- function(p) c(-p[1L], p[2L])

#' .groebn_fr_mul
#'
#' A step of the groebn_native implementation. Called by \code{.groebn_mul}, \code{.groebn_scale}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param p A vector; indexed elementwise.
#' @param q A vector; indexed elementwise.
#' @return The value of \code{.groebn_fr}.
#' @export
.groebn_fr_mul <- function(p, q) {
  .groebn_fr(p[1L] * q[1L], p[2L] * q[2L])
}

#' .groebn_fr_div
#'
#' A step of the groebn_native implementation. Called by \code{.groebn_divide}, \code{.groebn_reduce_basis}, \code{.groebn_spoly}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param p A vector; indexed elementwise.
#' @param q A vector; indexed elementwise.
#' @return The value of \code{.groebn_fr}.
#' @export
.groebn_fr_div <- function(p, q) {
  if (q[1L] == 0L) stop("groebn: division by zero rational")
  .groebn_fr(p[1L] * q[2L], p[2L] * q[1L])
}

#' .groebn_fr_is_zero
#'
#' A step of the groebn_native implementation. Called by \code{.groebn_add}, \code{.groebn_mul}, \code{.groebn_poly} and 1 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param p A vector; indexed elementwise.
#' @return A logical value.
#' @export
.groebn_fr_is_zero <- function(p) p[1L] == 0L

#' .groebn_fr_eq
#'
#' A step of the groebn_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param p A vector; indexed elementwise.
#' @param q A vector; indexed elementwise.
#' @return A logical value.
#' @export
.groebn_fr_eq <- function(p, q) p[1L] == q[1L] && p[2L] == q[2L]

#' .groebn_as_fr
#'
#' A step of the groebn_native implementation. Called by \code{.groebn_poly}, \code{.groebn_scale}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x Optional; may be \code{NULL}. A vector; its length is taken and its elements indexed.
#' @return Nothing; this branch always raises.
#' @export
.groebn_as_fr <- function(x) {
  if (is.null(x)) return(.groebn_fr(0L))
  if (is.character(x) && length(x) == 1L) {
    parts <- strsplit(x, "/", fixed = TRUE)[[1L]]
    if (length(parts) == 1L)
      return(.groebn_fr(as.integer(parts[1L]), 1L))
    return(.groebn_fr(as.integer(parts[1L]), as.integer(parts[2L])))
  }
  if (is.numeric(x) && length(x) == 1L)
    return(.groebn_fr(as.integer(x), 1L))
  if (is.integer(x) && length(x) == 1L)
    return(.groebn_fr(x, 1L))
  if (is.integer(x) && length(x) == 2L)
    return(.groebn_fr(x[1L], x[2L]))
  if (is.list(x) && length(x) == 2L &&
      is.numeric(x[[1L]]) && is.numeric(x[[2L]]))
    return(.groebn_fr(as.integer(x[[1L]]), as.integer(x[[2L]])))
  stop("groebn: cannot convert value to fraction")
}

# ---------------------------------------------------------------------------
# Orders and key
# ---------------------------------------------------------------------------
.groebn_orders <- c("lex", "grlex", "grevlex")

#' .groebn_key
#'
#' A step of the groebn_native implementation. Called by \code{.groebn_buchberger}, \code{.groebn_monomials}, \code{.groebn_reduce_basis}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param order One of \code{"grevlex"}, \code{"grlex"}, \code{"lex"}.
#' @return One of two values, depending on the branch taken.
#' @export
.groebn_key <- function(order) {
  if (order == "lex") {
    function(e) as.integer(e)
  } else if (order == "grlex") {
    function(e) c(sum(e), as.integer(e))
  } else if (order == "grevlex") {
    function(e) c(sum(e), -as.integer(rev(e)))
  } else {
    stop(sprintf("groebn: order must be one of %s, got %s",
                 paste(.groebn_orders, collapse = ", "),
                 paste(deparse(order), collapse = " ")))
  }
}

# ---------------------------------------------------------------------------
# Exponent keys
# ---------------------------------------------------------------------------
#' .groebn_parse_key
#'
#' Exponent keys
#' ---------------------------------------------------------------------------
#'
#' @param s Character; passed to \code{strsplit}.
#' @return The value of \code{as.integer}.
#' @export
.groebn_parse_key <- function(s) {
  as.integer(strsplit(s, "_", fixed = TRUE)[[1L]])
}

#' .groebn_format_key
#'
#' A step of the groebn_native implementation. Called by \code{.groebn_divide}, \code{.groebn_mul}, \code{.groebn_poly} and 1 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param e Coerced to integer by the body, with \code{as.integer}.
#' @return A character value.
#' @export
.groebn_format_key <- function(e) {
  paste(as.integer(e), collapse = "_")
}

# ---------------------------------------------------------------------------
# Polynomial constructor
# ---------------------------------------------------------------------------
#' .groebn_poly
#'
#' Polynomial constructor
#' ---------------------------------------------------------------------------
#'
#' @param terms A vector; its length is taken and its elements indexed.
#' @param nvars Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.
#' @return The value of \code{out}, as built in the body.
#' @export
.groebn_poly <- function(terms, nvars = NULL) {
  out <- list()
  n <- NULL

  is_named_dict <- is.list(terms) && length(terms) > 0L &&
    !is.null(names(terms)) && length(names(terms)) == length(terms)

  if (is_named_dict) {
    ok <- TRUE
    parsed <- vector("list", length(terms))
    for (idx in seq_along(terms)) {
      k <- names(terms)[idx]
      parts <- strsplit(k, "_", fixed = TRUE)[[1L]]
      ev <- suppressWarnings(as.integer(parts))
      if (any(is.na(ev)) || any(ev < 0L)) { ok <- FALSE; break }
      parsed[[idx]] <- ev
      if (is.null(n)) n <- length(ev)
      else if (length(ev) != n)
        stop(sprintf("groebn: exponent vectors of differing length, %d and %d",
                     n, length(ev)))
    }
    if (ok) {
      for (idx in seq_along(terms)) {
        ev <- parsed[[idx]]
        k_norm <- .groebn_format_key(ev)
        cf <- .groebn_as_fr(terms[[idx]])
        cur <- out[[k_norm]]
        if (is.null(cur)) {
          out[[k_norm]] <- cf
        } else {
          s <- .groebn_fr_add(cur, cf)
          if (.groebn_fr_is_zero(s)) out[[k_norm]] <- NULL
          else out[[k_norm]] <- s
        }
      }
    } else {
      # Fall through to pair-list interpretation
      n <- NULL
      out <- list()
      items <- terms
      for (item in items) {
        if (is.null(item) || length(item) < 2L) next
        if (is.list(item)) { e <- item[[1L]]; cf <- item[[2L]] }
        else { e <- item[1L]; cf <- item[2L] }
        ev <- suppressWarnings(as.integer(e))
        if (any(is.na(ev)) || any(ev < 0L))
          stop(sprintf("groebn: negative or invalid exponent in %s",
                       paste(ev, collapse = "_")))
        if (is.null(n)) n <- length(ev)
        else if (length(ev) != n)
          stop(sprintf("groebn: exponent vectors of differing length, %d and %d",
                       n, length(ev)))
        k_norm <- .groebn_format_key(ev)
        cfr <- .groebn_as_fr(cf)
        cur <- out[[k_norm]]
        if (is.null(cur)) out[[k_norm]] <- cfr
        else {
          s <- .groebn_fr_add(cur, cfr)
          if (.groebn_fr_is_zero(s)) out[[k_norm]] <- NULL
          else out[[k_norm]] <- s
        }
      }
    }
  } else {
    items <- if (is.list(terms)) terms else list(terms)
    for (item in items) {
      if (is.null(item) || length(item) < 2L) next
      if (is.list(item)) { e <- item[[1L]]; cf <- item[[2L]] }
      else { e <- item[1L]; cf <- item[2L] }
      ev <- suppressWarnings(as.integer(e))
      if (any(is.na(ev)) || any(ev < 0L))
        stop(sprintf("groebn: negative or invalid exponent in %s",
                     paste(ev, collapse = "_")))
      if (is.null(n)) n <- length(ev)
      else if (length(ev) != n)
        stop(sprintf("groebn: exponent vectors of differing length, %d and %d",
                     n, length(ev)))
      k_norm <- .groebn_format_key(ev)
      cfr <- .groebn_as_fr(cf)
      cur <- out[[k_norm]]
      if (is.null(cur)) out[[k_norm]] <- cfr
      else {
        s <- .groebn_fr_add(cur, cfr)
        if (.groebn_fr_is_zero(s)) out[[k_norm]] <- NULL
        else out[[k_norm]] <- s
      }
    }
  }

  if (!is.null(nvars)) {
    if (length(out) > 0L) {
      actual_n <- length(.groebn_parse_key(names(out)[1L]))
      if (actual_n != as.integer(nvars))
        stop(sprintf("groebn: %d variables declared but the exponents have %d",
                     as.integer(nvars), actual_n))
    }
  }

  out
}

# ---------------------------------------------------------------------------
# Polynomial queries
# ---------------------------------------------------------------------------
#' .groebn_nvars
#'
#' Polynomial queries
#' ---------------------------------------------------------------------------
#'
#' @param F See Usage.
#' @return A numeric value.
#' @export
.groebn_nvars <- function(F) {
  for (f in F) {
    if (length(f) > 0L) {
      nms <- names(f)
      if (length(nms) > 0L)
        return(length(.groebn_parse_key(nms[1L])))
    }
  }
  0L
}

#' .groebn_monomials
#'
#' A step of the groebn_native implementation. Called by \code{.groebn_leading_monomial}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param f See Usage.
#' @param order Passed to \code{.groebn_key}. Defaults to \code{"lex"}.
#' @return The value of \code{[}.
#' @export
.groebn_monomials <- function(f, order = "lex") {
  nms <- names(f)
  if (length(nms) == 0L) return(character(0L))
  kf <- .groebn_key(order)
  keys <- lapply(nms, .groebn_parse_key)
  klist <- lapply(keys, kf)
  kmat <- do.call(rbind, klist)
  ord <- do.call(order, c(as.data.frame(kmat), list(decreasing = TRUE)))
  nms[ord]
}

#' .groebn_leading_monomial
#'
#' A step of the groebn_native implementation. Called by \code{.groebn_buchberger}, \code{.groebn_divide}, \code{.groebn_leading_coeff} and 3 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param f Passed to \code{.groebn_monomials}.
#' @param order Passed to \code{.groebn_monomials}. Defaults to \code{"lex"}.
#' @return The value of \code{[}.
#' @export
.groebn_leading_monomial <- function(f, order = "lex") {
  ms <- .groebn_monomials(f, order)
  if (length(ms) == 0L) return(NULL)
  ms[1L]
}

#' .groebn_leading_coeff
#'
#' A step of the groebn_native implementation. Called by \code{.groebn_reduce_basis}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param f A vector; indexed elementwise.
#' @param order Passed to \code{.groebn_leading_monomial}. Defaults to \code{"lex"}.
#' @return The value of \code{[[}.
#' @export
.groebn_leading_coeff <- function(f, order = "lex") {
  lm <- .groebn_leading_monomial(f, order)
  if (is.null(lm)) return(.groebn_fr(0L))
  f[[lm]]
}

#' .groebn_leading_term
#'
#' A step of the groebn_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param f A vector; indexed elementwise.
#' @param order Passed to \code{.groebn_leading_monomial}. Defaults to \code{"lex"}.
#' @return The value of \code{out}, as built in the body.
#' @export
.groebn_leading_term <- function(f, order = "lex") {
  lm <- .groebn_leading_monomial(f, order)
  if (is.null(lm)) return(list())
  out <- list()
  out[[lm]] <- f[[lm]]
  out
}

# ---------------------------------------------------------------------------
# Polynomial arithmetic
# ---------------------------------------------------------------------------
#' .groebn_add
#'
#' Polynomial arithmetic
#' ---------------------------------------------------------------------------
#'
#' @param f See Usage.
#' @param g A vector; indexed elementwise.
#' @return The value of \code{out}, as built in the body.
#' @export
.groebn_add <- function(f, g) {
  out <- f
  for (k in names(g)) {
    cur <- out[[k]]
    if (is.null(cur)) {
      out[[k]] <- g[[k]]
    } else {
      s <- .groebn_fr_add(cur, g[[k]])
      if (.groebn_fr_is_zero(s)) out[[k]] <- NULL
      else out[[k]] <- s
    }
  }
  out
}

#' .groebn_sub
#'
#' A step of the groebn_native implementation. Called by \code{.groebn_divide}, \code{.groebn_spoly}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param f Passed to \code{.groebn_add}.
#' @param g Iterated over elementwise, with \code{lapply}.
#' @return The value of \code{.groebn_add}.
#' @export
.groebn_sub <- function(f, g) {
  neg_g <- lapply(g, .groebn_fr_neg)
  .groebn_add(f, neg_g)
}

#' .groebn_mul
#'
#' A step of the groebn_native implementation. Called by \code{.groebn_divide}, \code{.groebn_spoly}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param f A vector; its length is taken and its elements indexed.
#' @param g A vector; its length is taken and its elements indexed.
#' @return The value of \code{out}, as built in the body.
#' @export
.groebn_mul <- function(f, g) {
  if (length(f) == 0L || length(g) == 0L) return(list())
  out <- list()
  for (k1 in names(f)) {
    e1 <- .groebn_parse_key(k1)
    c1 <- f[[k1]]
    for (k2 in names(g)) {
      e2 <- .groebn_parse_key(k2)
      c2 <- g[[k2]]
      e_new <- e1 + e2
      k_new <- .groebn_format_key(e_new)
      prod <- .groebn_fr_mul(c1, c2)
      cur <- out[[k_new]]
      if (is.null(cur)) {
        out[[k_new]] <- prod
      } else {
        s <- .groebn_fr_add(cur, prod)
        if (.groebn_fr_is_zero(s)) out[[k_new]] <- NULL
        else out[[k_new]] <- s
      }
    }
  }
  out
}

#' .groebn_scale
#'
#' A step of the groebn_native implementation. Called by \code{.groebn_reduce_basis}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param f Iterated over elementwise, with \code{lapply}.
#' @param c Passed to \code{.groebn_as_fr}.
#' @return The value of \code{lapply}.
#' @export
.groebn_scale <- function(f, c) {
  q <- .groebn_as_fr(c)
  if (.groebn_fr_is_zero(q)) return(list())
  lapply(f, function(v) .groebn_fr_mul(v, q))
}

#' .groebn_divides
#'
#' A step of the groebn_native implementation. Called by \code{.groebn_divide}, \code{.groebn_reduce_basis}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param a See Usage.
#' @param b See Usage.
#' @return A logical value.
#' @export
.groebn_divides <- function(a, b) {
  all(a <= b)
}

#' .groebn_lcm
#'
#' A step of the groebn_native implementation. Called by \code{.groebn_spoly}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param a See Usage.
#' @param b See Usage.
#' @return The value of \code{pmax}.
#' @export
.groebn_lcm <- function(a, b) {
  pmax(a, b)
}

# ---------------------------------------------------------------------------
# S-polynomial and division
# ---------------------------------------------------------------------------
#' .groebn_spoly
#'
#' S-polynomial and division
#' ---------------------------------------------------------------------------
#'
#' @param f A vector; its length is taken and its elements indexed.
#' @param g A vector; its length is taken and its elements indexed.
#' @param order Passed to \code{.groebn_leading_monomial}. Defaults to \code{"lex"}.
#' @return The value of \code{.groebn_sub}.
#' @export
.groebn_spoly <- function(f, g, order = "lex") {
  if (length(f) == 0L || length(g) == 0L)
    stop("groebn: the S-polynomial of the zero polynomial is not defined")
  lf <- .groebn_leading_monomial(f, order)
  lg <- .groebn_leading_monomial(g, order)
  lf_v <- .groebn_parse_key(lf)
  lg_v <- .groebn_parse_key(lg)
  L <- .groebn_lcm(lf_v, lg_v)
  lcf <- f[[lf]]
  lcg <- g[[lg]]
  a <- list()
  a[[.groebn_format_key(L - lf_v)]] <- .groebn_fr_div(.groebn_fr(1L), lcf)
  b <- list()
  b[[.groebn_format_key(L - lg_v)]] <- .groebn_fr_div(.groebn_fr(1L), lcg)
  .groebn_sub(.groebn_mul(a, f), .groebn_mul(b, g))
}

#' .groebn_divide
#'
#' A step of the groebn_native implementation. Called by \code{.groebn_normal_form}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param f See Usage.
#' @param G Iterated over elementwise, with \code{Filter}.
#' @param order Passed to \code{.groebn_leading_monomial}. Defaults to \code{"lex"}.
#' @return A list with \code{quotients}, \code{remainder}.
#' @export
.groebn_divide <- function(f, G, order = "lex") {
  Gs <- Filter(function(g) length(g) > 0L, G)
  if (length(Gs) == 0L)
    stop("groebn: division by an empty set")
  q <- replicate(length(Gs), list(), simplify = FALSE)
  r <- list()
  p <- f
  while (length(p) > 0L) {
    lm <- .groebn_leading_monomial(p, order)
    lc <- p[[lm]]
    lm_v <- .groebn_parse_key(lm)
    divided <- FALSE
    for (i in seq_along(Gs)) {
      g <- Gs[[i]]
      lg <- .groebn_leading_monomial(g, order)
      lg_v <- .groebn_parse_key(lg)
      if (.groebn_divides(lg_v, lm_v)) {
        lcg <- g[[lg]]
        t_factor <- .groebn_fr_div(lc, lcg)
        t <- list()
        t[[.groebn_format_key(lm_v - lg_v)]] <- t_factor
        q[[i]] <- .groebn_add(q[[i]], t)
        p <- .groebn_sub(p, .groebn_mul(t, g))
        divided <- TRUE
        break
      }
    }
    if (!divided) {
      r[[lm]] <- lc
      p[[lm]] <- NULL
    }
  }
  list(quotients = q, remainder = r)
}

#' .groebn_normal_form
#'
#' A step of the groebn_native implementation. Called by \code{.groebn_buchberger}, \code{.groebn_ideal_member}, \code{.groebn_reduce_basis}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param f Passed to \code{.groebn_divide}.
#' @param G Passed to \code{.groebn_divide}.
#' @param order Passed to \code{.groebn_divide}. Defaults to \code{"lex"}.
#' @return The value of \code{$}.
#' @export
.groebn_normal_form <- function(f, G, order = "lex") {
  .groebn_divide(f, G, order)$remainder
}

# ---------------------------------------------------------------------------
# Buchberger completion and reduce_basis
# ---------------------------------------------------------------------------
#' .groebn_buchberger
#'
#' Buchberger completion and reduce_basis
#' ---------------------------------------------------------------------------
#'
#' @param F Iterated over elementwise, with \code{lapply}.
#' @param order Passed to \code{.groebn_key}. Defaults to \code{"lex"}.
#' @param prune A flag; the body branches on it. Defaults to \code{TRUE}.
#' @param reduced A flag; the body branches on it. Defaults to \code{TRUE}.
#' @return A list with \code{estimate}, \code{basis}, \code{order}, \code{size}, \code{reduced}, \code{n_pairs}, \code{n_reductions}, \code{n_skipped}, \code{pruned}, \code{method}.
#' @export
.groebn_buchberger <- function(F, order = "lex", prune = TRUE, reduced = TRUE) {
  .groebn_key(order)  # validate
  G <- lapply(F, function(f) if (is.null(f) || length(f) == 0L) NULL else f)
  G <- Filter(Negate(is.null), G)
  if (length(G) == 0L)
    stop("groebn: no non-zero generators given")
  n <- .groebn_nvars(G)
  for (g in G) {
    for (e in names(g)) {
      ev <- .groebn_parse_key(e)
      if (length(ev) != n)
        stop("groebn: generators in different numbers of variables")
    }
  }
  pairs <- list()
  ng <- length(G)
  for (i in seq_len(ng)) {
    for (j in seq_len(ng)) {
      if (j > i) pairs[[length(pairs) + 1L]] <- c(i, j)
    }
  }
  n_pairs <- 0L
  n_skipped <- 0L
  n_reductions <- 0L
  while (length(pairs) > 0L) {
    pr <- pairs[[1L]]
    pairs[[1L]] <- NULL
    i <- pr[1L]; j <- pr[2L]
    n_pairs <- n_pairs + 1L
    li <- .groebn_leading_monomial(G[[i]], order)
    lj <- .groebn_leading_monomial(G[[j]], order)
    li_v <- .groebn_parse_key(li)
    lj_v <- .groebn_parse_key(lj)
    if (isTRUE(prune) && all((li_v == 0L) | (lj_v == 0L))) {
      n_skipped <- n_skipped + 1L
      next
    }
    n_reductions <- n_reductions + 1L
    r <- .groebn_normal_form(.groebn_spoly(G[[i]], G[[j]], order), G, order)
    if (length(r) > 0L) {
      G[[length(G) + 1L]] <- r
      new_len <- length(G)
      for (k in seq_len(new_len - 1L)) {
        pairs[[length(pairs) + 1L]] <- c(k, new_len)
      }
    }
  }
  basis <- if (isTRUE(reduced)) .groebn_reduce_basis(G, order) else G
  method <- "Buchberger (1965) completion"
  if (isTRUE(prune))
    method <- paste0(method, " with the 1979 coprimality criterion")
  list(
    estimate = basis,
    basis = basis,
    order = order,
    size = length(basis),
    reduced = isTRUE(reduced),
    n_pairs = n_pairs,
    n_reductions = n_reductions,
    n_skipped = n_skipped,
    pruned = isTRUE(prune),
    method = method
  )
}

#' .groebn_reduce_basis
#'
#' A step of the groebn_native implementation. Called by \code{.groebn_buchberger}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param G Iterated over elementwise, with \code{Filter}.
#' @param order Passed to \code{.groebn_leading_monomial}. Defaults to \code{"lex"}.
#' @return The value of \code{[}.
#' @export
.groebn_reduce_basis <- function(G, order = "lex") {
  H <- Filter(function(g) length(g) > 0L, G)
  keep <- list()
  for (i in seq_along(H)) {
    g <- H[[i]]
    lg <- .groebn_leading_monomial(g, order)
    lg_v <- .groebn_parse_key(lg)
    drop <- FALSE
    for (j in seq_along(H)) {
      if (j == i) next
      h <- H[[j]]
      lh <- .groebn_leading_monomial(h, order)
      lh_v <- .groebn_parse_key(lh)
      if (j > i && lh == lg) next
      if (.groebn_divides(lh_v, lg_v)) { drop <- TRUE; break }
    }
    if (!drop) keep[[length(keep) + 1L]] <- g
  }
  out <- list()
  for (i in seq_along(keep)) {
    g <- keep[[i]]
    rest <- c(keep[-i], out)
    if (length(rest) > 0L) {
      r <- .groebn_normal_form(g, rest, order)
    } else {
      r <- g
    }
    if (length(r) > 0L) {
      lcr <- .groebn_leading_coeff(r, order)
      out[[length(out) + 1L]] <-
        .groebn_scale(r, .groebn_fr_div(.groebn_fr(1L), lcr))
    }
  }
  if (length(out) == 0L) return(out)
  lms <- vapply(out, function(p) .groebn_leading_monomial(p, order),
                character(1L))
  kf <- .groebn_key(order)
  keys <- lapply(lms, kf)
  kmat <- do.call(rbind, keys)
  ord <- do.call(order, c(as.data.frame(kmat), list(decreasing = TRUE)))
  out[ord]
}

# ---------------------------------------------------------------------------
# Ideal membership
# ---------------------------------------------------------------------------
#' .groebn_ideal_member
#'
#' Ideal membership
#' ---------------------------------------------------------------------------
#'
#' @param f Passed to \code{.groebn_normal_form}.
#' @param F Passed to \code{.groebn_buchberger}.
#' @param order Passed to \code{.groebn_buchberger}. Defaults to \code{"lex"}.
#' @param basis Defaults to \code{NULL}.
#' @return A list with \code{estimate}, \code{member}, \code{remainder}, \code{order}, \code{basis}, \code{method}.
#' @export
.groebn_ideal_member <- function(f, F, order = "lex", basis = NULL) {
  G <- if (!is.null(basis)) basis
       else .groebn_buchberger(F, order)$basis
  r <- .groebn_normal_form(f, G, order)
  list(
    estimate = length(r) == 0L,
    member = length(r) == 0L,
    remainder = r,
    order = order,
    basis = G,
    method = paste0("Buchberger (1965): zero normal form over a ",
                    "Groebner basis is necessary and sufficient")
  )
}

# ---------------------------------------------------------------------------
# Entry point
# ---------------------------------------------------------------------------
#' morie_groebn
#'
#' Entry point
#' ---------------------------------------------------------------------------
#'
#' @param polys Iterated over elementwise, with \code{lapply}.
#' @param order Passed to \code{.groebn_buchberger}. Defaults to \code{"lex"}.
#' @param prune Passed to \code{.groebn_buchberger}. Defaults to \code{TRUE}.
#' @param reduced Passed to \code{.groebn_buchberger}. Defaults to \code{TRUE}.
#' @return The value of \code{.groebn_buchberger}.
#' @export
morie_groebn <- function(polys, order = "lex", prune = TRUE, reduced = TRUE) {
  polys_norm <- lapply(polys, .groebn_poly)
  .groebn_buchberger(polys_norm, order = order, prune = prune, reduced = reduced)
}
