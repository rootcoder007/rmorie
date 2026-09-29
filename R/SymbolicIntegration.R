#' Symbolic integration and differentiation
#'
#' \code{SymbolicIntegrate}: antiderivative of an elementary expression
#' parsed with \code{\link{ShuntingYard}} and kept in a canonical
#' sum-of-products form. Rules, in order: constants and linearity, the
#' elementary table for arguments linear in x, rational functions by
#' polynomial division and partial fractions over the Durand-Kerner roots of
#' the denominator (the rational part of the Risch-Bronstein algorithm),
#' integration by parts, exp-trig products, derivative-divides substitution
#' and distribution over sums; the result is verified by differentiating it
#' back at five points. \code{SymbolicDiff}: derivative by the sum,
#' product, power and chain rules. Identical to the Python arm
#' \code{morie.fn.symint}.
#'
#' @param expr Expression string.
#' @param x Variable name.
#' @return List. SymbolicIntegrate: antiderivative (NULL when no rule
#'   applies), verified, max_rel_error, expression. SymbolicDiff: derivative,
#'   expression.
#' @references Bronstein, M. (1997). Symbolic Integration I: Transcendental
#'   Functions. Springer.
#'
#'   Geddes, K. O., Czapor, S. R. and Labahn, G. (1992). Algorithms for
#'   Computer Algebra. Kluwer.
#' @examples
#' SymbolicIntegrate("x*exp(2*x)")$antiderivative
#' SymbolicIntegrate("1/(x^2 + 1)")$antiderivative
#' SymbolicDiff("x^3*sin(x)")$derivative
#' @export
SymbolicIntegrate <- function(expr, x = "x") {
  e <- .si_parse(expr)
  F <- .si_integrate(e, x, 0)
  if (is.null(F)) return(list(antiderivative = NULL, verified = FALSE, max_rel_error = NaN, expression = .si_str(e)))
  ck <- .si_check(F, e, x)
  list(antiderivative = .si_str(F), verified = ck$n > 0 && ck$err < 1e-8, max_rel_error = ck$err, expression = .si_str(e))
}

#' @rdname SymbolicIntegrate
#' @export
SymbolicDiff <- function(expr, x = "x") {
  e <- .si_parse(expr)
  list(derivative = .si_str(.si_d(e, x)), expression = .si_str(e))
}

.si_fns <- c("sin", "cos", "tan", "exp", "log", "asin", "acos", "atan", "sinh", "cosh")

.si_num <- function(v) list(t = "num", v = as.numeric(v))
.si_sym <- function(s) list(t = "sym", v = s)
.si_isnum <- function(e, v) e$t == "num" && e$v == v

.si_fmt <- function(v) {
  if (v == 0) return("0")
  if (is.finite(v) && v == trunc(v) && abs(v) < 1e15) return(sprintf("%.0f", v))
  sprintf("%.15g", v)
}

.si_prec <- function(e) {
  if (e$t == "add") return(1)
  if (e$t == "mul") return(2)
  if (e$t == "num" && e$v < 0) return(1)
  if (e$t == "pow") return(3)
  4
}

.si_str <- function(e) {
  switch(e$t,
    num = .si_fmt(e$v),
    sym = e$v,
    fn = paste0(e$f, "(", .si_str(e$a), ")"),
    pow = {
      if (.si_isnum(e$e, 0.5)) return(paste0("sqrt(", .si_str(e$b), ")"))
      bs <- if (.si_prec(e$b) > 3) .si_str(e$b) else paste0("(", .si_str(e$b), ")")
      xs <- if (.si_prec(e$e) > 3 && !(e$e$t == "num" && e$e$v < 0)) .si_str(e$e) else paste0("(", .si_str(e$e), ")")
      paste0(bs, "^", xs)
    },
    mul = {
      fs <- e$x
      sign <- ""
      parts <- character(0)
      rest <- fs
      if (fs[[1]]$t == "num") {
        cc <- fs[[1]]$v
        rest <- fs[-1]
        if (cc < 0) {
          sign <- "-"
          cc <- -cc
        }
        if (cc != 1) parts <- c(parts, .si_fmt(cc))
      }
      for (f in rest) {
        s <- .si_str(f)
        parts <- c(parts, if (.si_prec(f) > 2) s else paste0("(", s, ")"))
      }
      paste0(sign, paste(parts, collapse = "*"))
    },
    add = {
      s <- .si_str(e$x[[1]])
      for (term in e$x[-1]) {
        ts <- .si_str(term)
        s <- if (startsWith(ts, "-")) paste0(s, " - ", substring(ts, 2)) else paste0(s, " + ", ts)
      }
      s
    }
  )
}

.si_free <- function(e, x) {
  switch(e$t,
    num = TRUE,
    sym = e$v != x,
    fn = .si_free(e$a, x),
    pow = .si_free(e$b, x) && .si_free(e$e, x),
    all(vapply(e$x, .si_free, TRUE, x = x))
  )
}

.si_isint <- function(v) is.finite(v) && v == trunc(v) && abs(v) <= 64

.si_npow <- function(b, n) {
  v <- b^n
  if (is.finite(v)) v else NULL
}

.si_simp <- function(e) {
  t <- e$t
  if (t %in% c("num", "sym")) return(e)
  if (t == "fn") {
    a <- .si_simp(e$a)
    nm <- e$f
    if (a$t == "num") {
      v <- a$v
      if (v == 0 && nm %in% c("sin", "tan", "asin", "atan", "sinh")) return(.si_num(0))
      if (v == 0 && nm %in% c("cos", "exp", "cosh")) return(.si_num(1))
      if (v == 1 && nm == "log") return(.si_num(0))
    }
    if (nm == "log" && a$t == "fn" && a$f == "exp") return(a$a)
    if (nm == "exp" && a$t == "fn" && a$f == "log") return(a$a)
    return(list(t = "fn", f = nm, a = a))
  }
  if (t == "pow") {
    b <- .si_simp(e$b)
    x <- .si_simp(e$e)
    if (x$t == "num") {
      if (x$v == 0) return(.si_num(1))
      if (x$v == 1) return(b)
      if (b$t == "num" && .si_isint(x$v) && b$v != 0 && !is.null(.si_npow(b$v, x$v))) return(.si_num(.si_npow(b$v, x$v)))
      if (b$t == "num" && b$v == 1) return(.si_num(1))
      if (b$t == "pow" && b$e$t == "num" && .si_isint(x$v)) return(.si_simp(list(t = "pow", b = b$b, e = .si_num(b$e$v * x$v))))
      if (b$t == "mul" && .si_isint(x$v)) return(.si_simp(list(t = "mul", x = lapply(b$x, function(f) list(t = "pow", b = f, e = x)))))
    }
    return(list(t = "pow", b = b, e = x))
  }
  if (t == "mul") {
    fs <- list()
    for (f in e$x) {
      f <- .si_simp(f)
      if (f$t == "mul") fs <- c(fs, f$x) else fs[[length(fs) + 1]] <- f
    }
    coef <- 1
    bases <- list()
    exps <- numeric(0)
    ord <- character(0)
    for (f in fs) {
      if (f$t == "num") {
        coef <- coef * f$v
        next
      }
      if (f$t == "pow" && f$e$t == "num") {
        b <- f$b
        xv <- f$e$v
      } else {
        b <- f
        xv <- 1
      }
      k <- .si_str(b)
      if (!(k %in% ord)) {
        ord <- c(ord, k)
        bases[[k]] <- b
        exps[k] <- 0
      }
      exps[k] <- exps[k] + xv
    }
    if (coef == 0) return(.si_num(0))
    out <- list()
    for (k in sort(ord, method = "radix")) {
      b <- bases[[k]]
      xv <- exps[[k]]
      if (xv == 0) next
      p <- if (xv == 1) b else .si_simp_pow(b, xv)
      if (p$t == "num") coef <- coef * p$v else out[[length(out) + 1]] <- p
    }
    if (!length(out)) return(.si_num(coef))
    if (coef == 1 && length(out) == 1) return(out[[1]])
    return(list(t = "mul", x = c(if (coef != 1) list(.si_num(coef)) else list(), out)))
  }
  ts <- list()
  for (f in e$x) {
    f <- .si_simp(f)
    if (f$t == "add") ts <- c(ts, f$x) else ts[[length(ts) + 1]] <- f
  }
  const <- 0
  terms <- list()
  coefs <- numeric(0)
  ord <- character(0)
  for (f in ts) {
    if (f$t == "num") {
      const <- const + f$v
      next
    }
    sc <- .si_split_coef(f)
    k <- .si_str(sc$rest)
    if (!(k %in% ord)) {
      ord <- c(ord, k)
      terms[[k]] <- sc$rest
      coefs[k] <- 0
    }
    coefs[k] <- coefs[k] + sc$c
  }
  out <- list()
  for (k in sort(ord, method = "radix")) {
    cc <- coefs[[k]]
    if (cc == 0) next
    out[[length(out) + 1]] <- if (cc == 1) terms[[k]] else .si_simp(list(t = "mul", x = list(.si_num(cc), terms[[k]])))
  }
  if (const != 0) out[[length(out) + 1]] <- .si_num(const)
  if (!length(out)) return(.si_num(0))
  if (length(out) == 1) return(out[[1]])
  list(t = "add", x = out)
}

.si_simp_pow <- function(b, xv) {
  if (b$t == "num" && .si_isint(xv) && b$v != 0 && !is.null(.si_npow(b$v, xv))) return(.si_num(.si_npow(b$v, xv)))
  list(t = "pow", b = b, e = .si_num(xv))
}

.si_split_coef <- function(f) {
  if (f$t == "mul" && f$x[[1]]$t == "num") {
    rest <- f$x[-1]
    return(list(c = f$x[[1]]$v, rest = if (length(rest) == 1) rest[[1]] else list(t = "mul", x = rest)))
  }
  list(c = 1, rest = f)
}

.si_add <- function(...) .si_simp(list(t = "add", x = list(...)))
.si_mul <- function(...) .si_simp(list(t = "mul", x = list(...)))
.si_pow <- function(b, x) .si_simp(list(t = "pow", b = b, e = if (is.list(x)) x else .si_num(x)))
.si_fn <- function(nm, a) .si_simp(list(t = "fn", f = nm, a = a))

.si_parse <- function(expr) {
  rpn <- ShuntingYard(as.character(expr))$rpn
  st <- list()
  pop <- function() {
    v <- st[[length(st)]]
    st[[length(st)]] <<- NULL
    v
  }
  push <- function(v) st[[length(st) + 1]] <<- v
  for (tok in rpn) {
    v <- suppressWarnings(as.numeric(tok))
    if (!is.na(v) || tok %in% c("NaN", "nan")) {
      push(.si_num(v))
      next
    }
    if (tok == "neg") {
      push(list(t = "mul", x = list(.si_num(-1), pop())))
    } else if (tok %in% c("+", "-", "*", "/", "^")) {
      b <- pop()
      a <- pop()
      push(switch(tok,
        "+" = list(t = "add", x = list(a, b)),
        "-" = list(t = "add", x = list(a, list(t = "mul", x = list(.si_num(-1), b)))),
        "*" = list(t = "mul", x = list(a, b)),
        "/" = list(t = "mul", x = list(a, list(t = "pow", b = b, e = .si_num(-1)))),
        "^" = list(t = "pow", b = a, e = b)
      ))
    } else if (tok == "sqrt") {
      push(list(t = "pow", b = pop(), e = .si_num(0.5)))
    } else if (tok %in% .si_fns) {
      push(list(t = "fn", f = tok, a = pop()))
    } else if (tok %in% c("abs", "min", "max")) {
      stop(tok, " is not supported")
    } else if (tok == "pi") {
      push(.si_num(pi))
    } else {
      push(.si_sym(tok))
    }
  }
  if (length(st) != 1) stop("malformed expression")
  .si_simp(st[[1]])
}

.si_d <- function(e, x) {
  if (.si_free(e, x)) return(.si_num(0))
  t <- e$t
  if (t == "sym") return(.si_num(1))
  if (t == "add") return(.si_simp(list(t = "add", x = lapply(e$x, .si_d, x = x))))
  if (t == "mul") {
    fs <- e$x
    terms <- lapply(seq_along(fs), function(i) list(t = "mul", x = c(list(.si_d(fs[[i]], x)), fs[-i])))
    return(.si_simp(list(t = "add", x = terms)))
  }
  if (t == "pow") {
    b <- e$b
    n <- e$e
    if (.si_free(n, x)) return(.si_mul(n, .si_pow(b, .si_add(n, .si_num(-1))), .si_d(b, x)))
    if (.si_free(b, x)) return(.si_mul(e, .si_fn("log", b), .si_d(n, x)))
    return(.si_mul(e, .si_add(.si_mul(.si_d(n, x), .si_fn("log", b)), .si_mul(n, .si_d(b, x), .si_pow(b, -1)))))
  }
  u <- e$a
  du <- .si_d(u, x)
  one_minus_u2 <- function() .si_add(.si_num(1), .si_mul(.si_num(-1), .si_pow(u, 2)))
  g <- switch(e$f,
    sin = .si_fn("cos", u),
    cos = .si_mul(.si_num(-1), .si_fn("sin", u)),
    tan = .si_pow(.si_fn("cos", u), -2),
    exp = e,
    log = .si_pow(u, -1),
    asin = .si_pow(one_minus_u2(), -0.5),
    acos = .si_mul(.si_num(-1), .si_pow(one_minus_u2(), -0.5)),
    atan = .si_pow(.si_add(.si_num(1), .si_pow(u, 2)), -1),
    sinh = .si_fn("cosh", u),
    .si_fn("sinh", u)
  )
  .si_mul(g, du)
}

.si_ev <- function(e, env) {
  switch(e$t,
    num = e$v,
    sym = {
      if (is.null(env[[e$v]])) stop("unbound symbol")
      env[[e$v]]
    },
    add = {
      s <- 0
      for (f in e$x) s <- s + .si_ev(f, env)
      s
    },
    mul = {
      s <- 1
      for (f in e$x) s <- s * .si_ev(f, env)
      s
    },
    pow = {
      b <- .si_ev(e$b, env)
      n <- .si_ev(e$e, env)
      if (is.nan(b) || is.nan(n)) return(NaN)
      if (b == 0 && n < 0) return(NaN)
      if (b < 0 && !(is.finite(n) && n == trunc(n))) return(NaN)
      b^n
    },
    {
      a <- .si_ev(e$a, env)
      if (is.nan(a)) return(NaN)
      if (e$f == "log") return(if (a > 0) log(a) else NaN)
      if (e$f %in% c("asin", "acos") && abs(a) > 1) return(NaN)
      v <- suppressWarnings(switch(e$f, sin = sin(a), cos = cos(a), tan = tan(a), exp = exp(a), asin = asin(a),
                                   acos = acos(a), atan = atan(a), sinh = sinh(a), cosh = cosh(a)))
      if (is.finite(v)) v else NaN
    }
  )
}

.si_padd <- function(p, q) {
  n <- max(length(p), length(q))
  vapply(seq_len(n), function(i) (if (i <= length(p)) p[i] else 0) + (if (i <= length(q)) q[i] else 0), 0)
}

.si_pmul <- function(p, q) {
  out <- numeric(length(p) + length(q) - 1)
  for (i in seq_along(p)) for (j in seq_along(q)) out[i + j - 1] <- out[i + j - 1] + p[i] * q[j]
  out
}

.si_ptrim <- function(p) {
  while (length(p) > 1 && p[length(p)] == 0) p <- p[-length(p)]
  p
}

.si_ev_const <- function(e) {
  v <- tryCatch(.si_ev(e, list()), error = function(err) NULL)
  if (is.null(v) || !is.finite(v)) NULL else v
}

.si_poly <- function(e, x) {
  if (.si_free(e, x)) {
    v <- .si_ev_const(e)
    return(if (is.null(v)) NULL else v)
  }
  t <- e$t
  if (t == "sym") return(c(0, 1))
  if (t == "add") {
    out <- 0
    for (f in e$x) {
      p <- .si_poly(f, x)
      if (is.null(p)) return(NULL)
      out <- .si_padd(out, p)
    }
    return(.si_ptrim(out))
  }
  if (t == "mul") {
    out <- 1
    for (f in e$x) {
      p <- .si_poly(f, x)
      if (is.null(p)) return(NULL)
      out <- .si_pmul(out, p)
    }
    return(.si_ptrim(out))
  }
  if (t == "pow" && e$e$t == "num" && .si_isint(e$e$v) && e$e$v > 0 && e$e$v <= 32) {
    p <- .si_poly(e$b, x)
    if (is.null(p)) return(NULL)
    out <- 1
    for (k in seq_len(e$e$v)) out <- .si_pmul(out, p)
    return(.si_ptrim(out))
  }
  NULL
}

.si_rat <- function(e, x) {
  p <- .si_poly(e, x)
  if (!is.null(p)) return(list(N = p, D = 1))
  t <- e$t
  if (t == "add") {
    N <- 0
    D <- 1
    for (f in e$x) {
      r <- .si_rat(f, x)
      if (is.null(r)) return(NULL)
      N <- .si_padd(.si_pmul(N, r$D), .si_pmul(r$N, D))
      D <- .si_pmul(D, r$D)
    }
    return(list(N = .si_ptrim(N), D = .si_ptrim(D)))
  }
  if (t == "mul") {
    N <- 1
    D <- 1
    for (f in e$x) {
      r <- .si_rat(f, x)
      if (is.null(r)) return(NULL)
      N <- .si_pmul(N, r$N)
      D <- .si_pmul(D, r$D)
    }
    return(list(N = .si_ptrim(N), D = .si_ptrim(D)))
  }
  if (t == "pow" && e$e$t == "num" && .si_isint(e$e$v) && e$e$v >= -32 && e$e$v < 0) {
    p <- .si_poly(e$b, x)
    if (is.null(p)) return(NULL)
    out <- 1
    for (k in seq_len(-e$e$v)) out <- .si_pmul(out, p)
    return(list(N = 1, D = .si_ptrim(out)))
  }
  NULL
}

.si_pdivmod <- function(N, D) {
  nq <- max(length(N) - length(D) + 1, 1)
  q <- numeric(nq)
  dl <- D[length(D)]
  if (length(N) >= length(D)) {
    for (i in rev(seq_len(length(N) - length(D) + 1))) {
      cc <- N[i + length(D) - 1] / dl
      q[i] <- cc
      for (j in seq_along(D)) N[i + j - 1] <- N[i + j - 1] - cc * D[j]
    }
  }
  r <- if (length(D) > 1) N[seq_len(min(length(N), length(D) - 1))] else 0
  list(q = .si_ptrim(q), r = .si_ptrim(r))
}

.si_cmul <- function(a, b) c(a[1] * b[1] - a[2] * b[2], a[1] * b[2] + a[2] * b[1])
.si_cdiv <- function(a, b) {
  den <- b[1] * b[1] + b[2] * b[2]
  c((a[1] * b[1] + a[2] * b[2]) / den, (a[2] * b[1] - a[1] * b[2]) / den)
}

.si_cpoly <- function(p, z) {
  v <- c(0, 0)
  for (cc in rev(p)) {
    v <- .si_cmul(v, z)
    v <- c(v[1] + cc, v[2])
  }
  v
}

.si_roots <- function(p) {
  n <- length(p) - 1
  q <- p / p[length(p)]
  w <- c(0.4, 0.9)
  z <- list(c(1, 0))
  for (k in seq_len(n - 1)) z[[length(z) + 1]] <- .si_cmul(z[[length(z)]], w)
  z <- lapply(z, function(zz) .si_cmul(zz, w))
  for (it in 1:500) {
    mx <- 0
    new <- vector("list", n)
    for (i in seq_len(n)) {
      den <- c(1, 0)
      for (j in seq_len(n)) if (j != i) den <- .si_cmul(den, z[[i]] - z[[j]])
      step <- .si_cdiv(.si_cpoly(q, z[[i]]), den)
      new[[i]] <- z[[i]] - step
      mx <- max(mx, abs(step[1]) + abs(step[2]))
    }
    z <- new
    if (mx < 1e-15) break
  }
  z
}

.si_snap <- function(v) {
  for (den in 1:1000) {
    r <- round(v * den)
    if (abs(v * den - r) < 1e-9 * max(1, abs(v * den))) return(r / den)
  }
  v
}

.si_cluster <- function(z, p) {
  used <- rep(FALSE, length(z))
  out <- list()
  for (i in seq_along(z)) {
    if (used[i]) next
    grp <- which(!used & vapply(z, function(zz) abs(zz[1] - z[[i]][1]) + abs(zz[2] - z[[i]][2]) < 1e-6, TRUE))
    used[grp] <- TRUE
    re <- 0
    im <- 0
    for (j in grp) {
      re <- re + z[[j]][1]
      im <- im + z[[j]][2]
    }
    re <- re / length(grp)
    im <- im / length(grp)
    if (length(grp) > 1) {
      dp <- p
      for (s in seq_len(length(grp) - 1)) dp <- (seq_along(dp)[-1] - 1) * dp[-1]
      d2 <- (seq_along(dp)[-1] - 1) * dp[-1]
      for (s in 1:30) {
        den <- .si_cpoly(d2, c(re, im))
        if (den[1] == 0 && den[2] == 0) break
        st <- .si_cdiv(.si_cpoly(dp, c(re, im)), den)
        re <- re - st[1]
        im <- im - st[2]
      }
    }
    if (abs(im) < 1e-9 * max(1, abs(re))) im <- 0
    out[[length(out) + 1]] <- list(z = c(.si_snap(re), .si_snap(im)), mu = length(grp))
  }
  out
}

.si_tshift <- function(p, r) {
  out <- p
  n <- length(out)
  for (k in seq_len(n - 1)) {
    for (i in rev(k:(n - 1))) {
      m <- .si_cmul(out[[i + 1]], r)
      out[[i]] <- out[[i]] + m
    }
  }
  out
}

.si_series_div <- function(a, b, m) {
  out <- list()
  for (k in seq_len(m)) {
    s <- if (k <= length(a)) a[[k]] else c(0, 0)
    for (j in seq_len(k - 1)) {
      if (j + 1 <= length(b)) s <- s - .si_cmul(b[[j + 1]], out[[k - j]])
    }
    out[[k]] <- .si_cdiv(s, b[[1]])
  }
  out
}

.si_int_rational <- function(N, D, x) {
  X <- .si_sym(x)
  qr <- .si_pdivmod(N, D)
  terms <- list()
  for (k in seq_along(qr$q)) {
    cc <- qr$q[k]
    if (cc != 0) terms[[length(terms) + 1]] <- .si_mul(.si_num(.si_snap(cc / k)), .si_pow(X, k))
  }
  r <- qr$r
  if (length(r) == 1 && r[1] == 0) return(if (length(terms)) .si_simp(list(t = "add", x = terms)) else .si_num(0))
  if (length(D) - 1 > 12) return(NULL)
  roots <- .si_cluster(.si_roots(D), D)
  if (sum(vapply(roots, function(rt) rt$mu, 0)) != length(D) - 1) return(NULL)
  for (rt in roots) {
    zr <- rt$z[1]
    zi <- rt$z[2]
    mu <- rt$mu
    if (zi < 0) next
    D1 <- lapply(D, function(cc) c(cc, 0))
    for (s in seq_len(mu)) {
      n <- length(D1) - 1
      out <- vector("list", n)
      acc <- c(0, 0)
      for (i in rev(seq_len(n))) {
        acc <- D1[[i + 1]] + .si_cmul(acc, c(zr, zi))
        out[[i]] <- acc
      }
      D1 <- out
    }
    a <- .si_tshift(lapply(r, function(cc) c(cc, 0)), c(zr, zi))
    bs <- .si_tshift(D1, c(zr, zi))
    g <- .si_series_div(a, bs, mu)
    for (j in seq_len(mu)) {
      cc <- g[[mu - j + 1]]
      cc <- c(.si_snap(cc[1]), .si_snap(cc[2]))
      if (zi == 0) {
        if (cc[1] == 0) next
        lin <- .si_add(X, .si_num(-zr))
        terms[[length(terms) + 1]] <- if (j == 1) .si_mul(.si_num(cc[1]), .si_fn("log", lin)) else
          .si_mul(.si_num(.si_snap(-cc[1] / (j - 1))), .si_pow(lin, -(j - 1)))
      } else {
        if (j > 1) return(NULL)
        quad <- .si_add(.si_pow(.si_add(X, .si_num(-zr)), 2), .si_num(.si_snap(zi * zi)))
        if (cc[1] != 0) terms[[length(terms) + 1]] <- .si_mul(.si_num(cc[1]), .si_fn("log", quad))
        if (cc[2] != 0) {
          terms[[length(terms) + 1]] <- .si_mul(.si_num(.si_snap(-2 * cc[2])),
                                                .si_fn("atan", .si_mul(.si_add(X, .si_num(-zr)), .si_num(.si_snap(1 / zi)))))
        }
      }
    }
  }
  if (length(terms)) .si_simp(list(t = "add", x = terms)) else .si_num(0)
}

.si_linear <- function(u, x) {
  p <- .si_poly(u, x)
  if (!is.null(p) && length(p) == 2 && p[2] != 0) return(c(p[2], p[1]))
  NULL
}

.si_table <- function(e, x) {
  X <- .si_sym(x)
  t <- e$t
  if (t == "sym") return(.si_mul(.si_num(0.5), .si_pow(X, 2)))
  if (t == "pow") {
    b <- e$b
    n <- e$e
    if (.si_isnum(n, -0.5)) {
      q <- .si_poly(b, x)
      if (!is.null(q) && length(q) == 3 && q[2] == 0 && q[1] != 0) {
        cc <- q[1]
        a2 <- q[3]
        if (a2 < 0 && cc > 0) return(.si_mul(.si_num(.si_snap(1 / sqrt(-a2))), .si_fn("asin", .si_mul(X, .si_num(.si_snap(sqrt(-a2 / cc)))))))
        if (a2 > 0) return(.si_mul(.si_num(.si_snap(1 / sqrt(a2))), .si_fn("log", .si_add(.si_mul(.si_num(.si_snap(sqrt(a2))), X), .si_pow(b, 0.5)))))
      }
    }
    lin <- .si_linear(b, x)
    if (!is.null(lin) && .si_free(n, x)) {
      a <- lin[1]
      if (.si_isnum(n, -1)) return(.si_mul(.si_num(.si_snap(1 / a)), .si_fn("log", b)))
      n1 <- .si_add(n, .si_num(1))
      return(.si_mul(.si_pow(b, n1), .si_pow(.si_mul(.si_num(a), n1), -1)))
    }
    lin <- .si_linear(n, x)
    if (!is.null(lin) && .si_free(b, x)) return(.si_mul(e, .si_pow(.si_mul(.si_num(lin[1]), .si_fn("log", b)), -1)))
    return(NULL)
  }
  if (t == "fn") {
    lin <- .si_linear(e$a, x)
    if (is.null(lin)) return(NULL)
    u <- e$a
    inv <- .si_num(.si_snap(1 / lin[1]))
    root <- function() .si_pow(.si_add(.si_num(1), .si_mul(.si_num(-1), .si_pow(u, 2))), 0.5)
    return(switch(e$f,
      sin = .si_mul(.si_num(-1), inv, .si_fn("cos", u)),
      cos = .si_mul(inv, .si_fn("sin", u)),
      tan = .si_mul(.si_num(-1), inv, .si_fn("log", .si_fn("cos", u))),
      exp = .si_mul(inv, e),
      log = .si_mul(inv, .si_add(.si_mul(u, e), .si_mul(.si_num(-1), u))),
      sinh = .si_mul(inv, .si_fn("cosh", u)),
      cosh = .si_mul(inv, .si_fn("sinh", u)),
      asin = .si_mul(inv, .si_add(.si_mul(u, e), root())),
      acos = .si_mul(inv, .si_add(.si_mul(u, e), .si_mul(.si_num(-1), root()))),
      atan = .si_mul(inv, .si_add(.si_mul(u, e), .si_mul(.si_num(-0.5), .si_fn("log", .si_add(.si_num(1), .si_pow(u, 2)))))),
      NULL
    ))
  }
  NULL
}

.si_factors <- function(e) if (e$t == "mul") e$x else list(e)

.si_parts <- function(fs, x, depth) {
  isp <- vapply(fs, function(f) !is.null(.si_poly(f, x)), TRUE)
  polys <- fs[isp]
  rest <- fs[!isp]
  if (length(rest) != 1 || !length(polys)) return(NULL)
  g <- rest[[1]]
  P <- .si_simp(list(t = "mul", x = polys))
  if (g$t == "fn" && g$f %in% c("exp", "sin", "cos", "sinh", "cosh") && !is.null(.si_linear(g$a, x))) {
    out <- list()
    sign <- 1
    cur <- g
    Pk <- P
    for (s in 1:40) {
      cur <- .si_integrate(cur, x, depth + 1)
      if (is.null(cur)) return(NULL)
      out[[length(out) + 1]] <- .si_mul(.si_num(sign), Pk, cur)
      Pk <- .si_d(Pk, x)
      sign <- -sign
      if (.si_isnum(Pk, 0)) return(.si_simp(list(t = "add", x = out)))
    }
    return(NULL)
  }
  if (g$t == "fn" && g$f %in% c("log", "atan", "asin", "acos") && !is.null(.si_linear(g$a, x))) {
    Q <- .si_integrate(P, x, depth + 1)
    if (is.null(Q)) return(NULL)
    ri <- .si_integrate(.si_mul(Q, .si_d(g, x)), x, depth + 1)
    if (is.null(ri)) return(NULL)
    return(.si_add(.si_mul(Q, g), .si_mul(.si_num(-1), ri)))
  }
  NULL
}

.si_exp_trig <- function(fs, x) {
  if (length(fs) != 2) return(NULL)
  ex <- Filter(function(f) f$t == "fn" && f$f == "exp", fs)
  tr <- Filter(function(f) f$t == "fn" && f$f %in% c("sin", "cos"), fs)
  if (length(ex) != 1 || length(tr) != 1) return(NULL)
  la <- .si_linear(ex[[1]]$a, x)
  lb <- .si_linear(tr[[1]]$a, x)
  if (is.null(la) || is.null(lb)) return(NULL)
  a <- la[1]
  b <- lb[1]
  v <- tr[[1]]$a
  s <- .si_fn("sin", v)
  cc <- .si_fn("cos", v)
  k <- .si_num(.si_snap(1 / (a * a + b * b)))
  inner <- if (tr[[1]]$f == "sin") .si_add(.si_mul(.si_num(a), s), .si_mul(.si_num(-b), cc)) else
    .si_add(.si_mul(.si_num(a), cc), .si_mul(.si_num(b), s))
  .si_mul(k, ex[[1]], inner)
}

.si_subexprs <- function(e) {
  out <- list()
  if (e$t %in% c("fn", "pow")) out <- list(e)
  if (e$t == "fn") return(c(out, .si_subexprs(e$a)))
  if (e$t == "pow") return(c(out, .si_subexprs(e$b), .si_subexprs(e$e)))
  if (e$t %in% c("add", "mul")) for (f in e$x) out <- c(out, .si_subexprs(f))
  out
}

.si_subst <- function(e, old, new) {
  if (identical(e, old)) return(new)
  switch(e$t,
    fn = list(t = "fn", f = e$f, a = .si_subst(e$a, old, new)),
    pow = list(t = "pow", b = .si_subst(e$b, old, new), e = .si_subst(e$e, old, new)),
    add = , mul = list(t = e$t, x = lapply(e$x, .si_subst, old = old, new = new)),
    e
  )
}

.si_usub <- function(e, x, depth) {
  seen <- character(0)
  for (g in .si_subexprs(e)) {
    k <- .si_str(g)
    if (k %in% seen || .si_free(g, x) || !is.null(.si_linear(g, x))) next
    seen <- c(seen, k)
    cands <- if (g$t == "fn") list(g) else list(g, g$b)
    for (cand in cands) {
      if (.si_free(cand, x) || !is.null(.si_linear(cand, x)) || identical(cand, .si_sym(x))) next
      dg <- .si_d(cand, x)
      if (.si_isnum(dg, 0)) next
      u <- .si_sym("_u")
      h <- .si_simp(list(t = "mul", x = list(.si_subst(e, cand, u), list(t = "pow", b = dg, e = .si_num(-1)))))
      if (!.si_free(h, x)) next
      H <- .si_integrate(h, "_u", depth + 1)
      if (!is.null(H)) return(.si_simp(.si_subst(H, u, cand)))
    }
  }
  NULL
}

.si_expand <- function(e) {
  if (e$t != "mul") return(NULL)
  fs <- e$x
  for (i in seq_along(fs)) {
    if (fs[[i]]$t == "add") {
      others <- fs[-i]
      return(.si_simp(list(t = "add", x = lapply(fs[[i]]$x, function(tt) list(t = "mul", x = c(others, list(tt)))))))
    }
  }
  NULL
}

.si_integrate <- function(e, x, depth) {
  if (depth > 8) return(NULL)
  X <- .si_sym(x)
  e <- .si_simp(e)
  if (.si_free(e, x)) return(.si_mul(e, X))
  if (e$t == "add") {
    out <- list()
    for (f in e$x) {
      r <- .si_integrate(f, x, depth + 1)
      if (is.null(r)) return(NULL)
      out[[length(out) + 1]] <- r
    }
    return(.si_simp(list(t = "add", x = out)))
  }
  fs <- .si_factors(e)
  isc <- vapply(fs, .si_free, TRUE, x = x)
  if (any(isc)) {
    r <- .si_integrate(.si_simp(list(t = "mul", x = fs[!isc])), x, depth + 1)
    return(if (is.null(r)) NULL else .si_simp(list(t = "mul", x = c(fs[isc], list(r)))))
  }
  r <- .si_table(e, x)
  if (!is.null(r)) return(r)
  rt <- .si_rat(e, x)
  if (!is.null(rt)) {
    r <- .si_int_rational(rt$N, rt$D, x)
    if (!is.null(r)) return(r)
  }
  r <- .si_parts(fs, x, depth)
  if (!is.null(r)) return(r)
  r <- .si_exp_trig(fs, x)
  if (!is.null(r)) return(r)
  r <- .si_usub(e, x, depth)
  if (!is.null(r)) return(r)
  ex <- .si_expand(e)
  if (!is.null(ex) && !identical(ex, e)) return(.si_integrate(ex, x, depth + 1))
  NULL
}

.si_check <- function(F, f, x) {
  dF <- .si_d(F, x)
  worst <- 0
  n <- 0
  for (v in c(0.37, 1.13, 2.71, -0.83, 0.61)) {
    env <- stats::setNames(list(v), x)
    a <- .si_ev(dF, env)
    b <- .si_ev(f, env)
    if (is.finite(a) && is.finite(b)) {
      worst <- max(worst, abs(a - b) / max(1, abs(b)))
      n <- n + 1
    }
  }
  list(err = if (n) worst else NaN, n = n)
}
