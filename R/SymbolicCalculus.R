# Symbolic calculus: limits, symbolic matrices, first-order ODEs and the rational case of the Risch algorithm.
# The expression core, the shunting-yard parser and the integrator are copied from ExactAlgebra.R and
# SymbolicIntegration.R (batch G); everything below .sc_check is new.

.sc_sy_prec <- c("+" = 1, "-" = 1, "*" = 2, "/" = 2, "^" = 4, neg = 3)
.sc_sy_funcs <- c("sin", "cos", "tan", "exp", "log", "sqrt", "abs", "asin", "acos", "atan", "sinh", "cosh")

.sc_sy_tokenize <- function(s) {
  ch <- strsplit(s, "")[[1]]
  out <- character(0)
  i <- 1
  L <- length(ch)
  isd <- function(x) grepl("^[0-9]$", x)
  while (i <= L) {
    c0 <- ch[i]
    if (grepl("^\\s$", c0)) {
      i <- i + 1
    } else if (isd(c0) || c0 == ".") {
      j <- i
      while (j <= L && (isd(ch[j]) || ch[j] == ".")) j <- j + 1
      if (j <= L && ch[j] %in% c("e", "E") && j + 1 <= L && (isd(ch[j + 1]) || ch[j + 1] %in% c("+", "-"))) {
        j <- j + 2
        while (j <= L && isd(ch[j])) j <- j + 1
      }
      out <- c(out, paste(ch[i:(j - 1)], collapse = ""))
      i <- j
    } else if (grepl("^[A-Za-z_]$", c0)) {
      j <- i
      while (j <= L && grepl("^[A-Za-z0-9_]$", ch[j])) j <- j + 1
      out <- c(out, paste(ch[i:(j - 1)], collapse = ""))
      i <- j
    } else if (c0 %in% c("+", "-", "*", "/", "^", "(", ")", ",")) {
      out <- c(out, c0)
      i <- i + 1
    } else {
      stop("unexpected character ", c0)
    }
  }
  out
}

.sc_sy_isnum <- function(t) !is.na(suppressWarnings(as.numeric(t)))

.sc_sy_call <- function(f, x) {
  v <- suppressWarnings(switch(f, sin = sin(x), cos = cos(x), tan = tan(x), exp = exp(x), log = if (x == 0) NaN else log(x),
                               sqrt = sqrt(x), abs = abs(x), asin = asin(x), acos = acos(x), atan = atan(x),
                               sinh = sinh(x), cosh = cosh(x)))
  if (is.infinite(v) && f != "exp") NaN else v
}

.sc_sy_binop <- function(t, a, b) {
  switch(t,
    "+" = a + b,
    "-" = a - b,
    "*" = a * b,
    "/" = if (b == 0) (if (a == 0 || is.nan(a)) NaN else sign(a) * Inf) else a / b,
    min = min(a, b),
    max = max(a, b),
    "^" = if (a == 0 && b < 0) Inf else if (a < 0 && b != round(b)) NaN else a^b
  )
}

.sc_shunting <- function(tokens, variables = NULL) {
  toks <- if (length(tokens) == 1 && is.character(tokens)) .sc_sy_tokenize(tokens) else as.character(tokens)
  prec <- .sc_sy_prec
  isfun <- function(t) t %in% c(.sc_sy_funcs, "min", "max")
  out <- character(0)
  st <- character(0)
  prev <- NULL
  unaryctx <- function() is.null(prev) || prev %in% names(prec) || prev %in% c("(", ",")
  for (t in toks) {
    if (.sc_sy_isnum(t)) {
      out <- c(out, t)
    } else if (isfun(t)) {
      st <- c(st, t)
    } else if (t == ",") {
      while (length(st) && st[length(st)] != "(") {
        out <- c(out, st[length(st)])
        st <- st[-length(st)]
      }
      if (!length(st)) stop("misplaced comma")
    } else if (t %in% names(prec)) {
      op <- t
      if (t == "-" && unaryctx()) {
        op <- "neg"
      } else if (t == "+" && unaryctx()) {
        prev <- t
        next
      }
      while (length(st) && st[length(st)] %in% names(prec) && op != "neg") {
        top <- st[length(st)]
        if (prec[[top]] > prec[[op]] || (prec[[top]] == prec[[op]] && !(op %in% c("^", "neg")))) {
          out <- c(out, top)
          st <- st[-length(st)]
        } else {
          break
        }
      }
      st <- c(st, op)
    } else if (t == "(") {
      st <- c(st, t)
    } else if (t == ")") {
      while (length(st) && st[length(st)] != "(") {
        out <- c(out, st[length(st)])
        st <- st[-length(st)]
      }
      if (!length(st)) stop("mismatched parentheses")
      st <- st[-length(st)]
      if (length(st) && isfun(st[length(st)])) {
        out <- c(out, st[length(st)])
        st <- st[-length(st)]
      }
    } else {
      out <- c(out, t)
    }
    prev <- t
  }
  while (length(st)) {
    if (st[length(st)] == "(") stop("mismatched parentheses")
    out <- c(out, st[length(st)])
    st <- st[-length(st)]
  }
  val <- NULL
  known <- vapply(out, function(t) .sc_sy_isnum(t) || t %in% names(prec) || isfun(t) || t %in% names(variables), TRUE)
  if (all(known)) {
    vs <- numeric(0)
    for (t in out) {
      if (.sc_sy_isnum(t)) {
        vs <- c(vs, as.numeric(t))
      } else if (t %in% names(variables)) {
        vs <- c(vs, as.numeric(variables[[t]]))
      } else if (t == "neg") {
        vs[length(vs)] <- -vs[length(vs)]
      } else if (t %in% .sc_sy_funcs) {
        vs[length(vs)] <- .sc_sy_call(t, vs[length(vs)])
      } else {
        b <- vs[length(vs)]
        a <- vs[length(vs) - 1]
        vs <- c(vs[seq_len(length(vs) - 2)], .sc_sy_binop(t, a, b))
      }
    }
    if (length(vs) != 1) stop("malformed expression")
    val <- vs
  }
  list(rpn = out, value = val, tokens = toks)
}

.sc_fns <- c("sin", "cos", "tan", "exp", "log", "asin", "acos", "atan", "sinh", "cosh")

.sc_num <- function(v) list(t = "num", v = as.numeric(v))
.sc_sym <- function(s) list(t = "sym", v = s)
.sc_isnum <- function(e, v) e$t == "num" && e$v == v

.sc_fmt <- function(v) {
  if (v == 0) return("0")
  if (is.finite(v) && v == trunc(v) && abs(v) < 1e15) return(sprintf("%.0f", v))
  sprintf("%.15g", v)
}

.sc_prec <- function(e) {
  if (e$t == "add") return(1)
  if (e$t == "mul") return(2)
  if (e$t == "num" && e$v < 0) return(1)
  if (e$t == "pow") return(3)
  4
}

.sc_str <- function(e) {
  switch(e$t,
    num = .sc_fmt(e$v),
    sym = e$v,
    fn = paste0(e$f, "(", .sc_str(e$a), ")"),
    pow = {
      if (.sc_isnum(e$e, 0.5)) return(paste0("sqrt(", .sc_str(e$b), ")"))
      bs <- if (.sc_prec(e$b) > 3) .sc_str(e$b) else paste0("(", .sc_str(e$b), ")")
      xs <- if (.sc_prec(e$e) > 3 && !(e$e$t == "num" && e$e$v < 0)) .sc_str(e$e) else paste0("(", .sc_str(e$e), ")")
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
        if (cc != 1) parts <- c(parts, .sc_fmt(cc))
      }
      for (f in rest) {
        s <- .sc_str(f)
        parts <- c(parts, if (.sc_prec(f) > 2) s else paste0("(", s, ")"))
      }
      paste0(sign, paste(parts, collapse = "*"))
    },
    add = {
      s <- .sc_str(e$x[[1]])
      for (term in e$x[-1]) {
        ts <- .sc_str(term)
        s <- if (startsWith(ts, "-")) paste0(s, " - ", substring(ts, 2)) else paste0(s, " + ", ts)
      }
      s
    }
  )
}

.sc_free <- function(e, x) {
  switch(e$t,
    num = TRUE,
    sym = e$v != x,
    fn = .sc_free(e$a, x),
    pow = .sc_free(e$b, x) && .sc_free(e$e, x),
    all(vapply(e$x, .sc_free, TRUE, x = x))
  )
}

.sc_isint <- function(v) is.finite(v) && v == trunc(v) && abs(v) <= 64

.sc_npow <- function(b, n) {
  v <- b^n
  if (is.finite(v)) v else NULL
}

.sc_simp <- function(e) {
  t <- e$t
  if (t %in% c("num", "sym")) return(e)
  if (t == "fn") {
    a <- .sc_simp(e$a)
    nm <- e$f
    if (a$t == "num") {
      v <- a$v
      if (v == 0 && nm %in% c("sin", "tan", "asin", "atan", "sinh")) return(.sc_num(0))
      if (v == 0 && nm %in% c("cos", "exp", "cosh")) return(.sc_num(1))
      if (v == 1 && nm == "log") return(.sc_num(0))
    }
    if (nm == "log" && a$t == "fn" && a$f == "exp") return(a$a)
    if (nm == "exp" && a$t == "fn" && a$f == "log") return(a$a)
    return(list(t = "fn", f = nm, a = a))
  }
  if (t == "pow") {
    b <- .sc_simp(e$b)
    x <- .sc_simp(e$e)
    if (x$t == "num") {
      if (x$v == 0) return(.sc_num(1))
      if (x$v == 1) return(b)
      if (b$t == "num" && .sc_isint(x$v) && b$v != 0 && !is.null(.sc_npow(b$v, x$v))) return(.sc_num(.sc_npow(b$v, x$v)))
      if (b$t == "num" && b$v == 1) return(.sc_num(1))
      if (b$t == "pow" && b$e$t == "num" && .sc_isint(x$v)) return(.sc_simp(list(t = "pow", b = b$b, e = .sc_num(b$e$v * x$v))))
      if (b$t == "mul" && .sc_isint(x$v)) return(.sc_simp(list(t = "mul", x = lapply(b$x, function(f) list(t = "pow", b = f, e = x)))))
    }
    return(list(t = "pow", b = b, e = x))
  }
  if (t == "mul") {
    fs <- list()
    for (f in e$x) {
      f <- .sc_simp(f)
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
      k <- .sc_str(b)
      if (!(k %in% ord)) {
        ord <- c(ord, k)
        bases[[k]] <- b
        exps[k] <- 0
      }
      exps[k] <- exps[k] + xv
    }
    if (coef == 0) return(.sc_num(0))
    out <- list()
    for (k in sort(ord, method = "radix")) {
      b <- bases[[k]]
      xv <- exps[[k]]
      if (xv == 0) next
      p <- if (xv == 1) b else .sc_simp_pow(b, xv)
      if (p$t == "num") coef <- coef * p$v else out[[length(out) + 1]] <- p
    }
    if (!length(out)) return(.sc_num(coef))
    if (coef == 1 && length(out) == 1) return(out[[1]])
    return(list(t = "mul", x = c(if (coef != 1) list(.sc_num(coef)) else list(), out)))
  }
  ts <- list()
  for (f in e$x) {
    f <- .sc_simp(f)
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
    sc <- .sc_split_coef(f)
    k <- .sc_str(sc$rest)
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
    out[[length(out) + 1]] <- if (cc == 1) terms[[k]] else .sc_simp(list(t = "mul", x = list(.sc_num(cc), terms[[k]])))
  }
  if (const != 0) out[[length(out) + 1]] <- .sc_num(const)
  if (!length(out)) return(.sc_num(0))
  if (length(out) == 1) return(out[[1]])
  list(t = "add", x = out)
}

.sc_simp_pow <- function(b, xv) {
  if (b$t == "num" && .sc_isint(xv) && b$v != 0 && !is.null(.sc_npow(b$v, xv))) return(.sc_num(.sc_npow(b$v, xv)))
  list(t = "pow", b = b, e = .sc_num(xv))
}

.sc_split_coef <- function(f) {
  if (f$t == "mul" && f$x[[1]]$t == "num") {
    rest <- f$x[-1]
    return(list(c = f$x[[1]]$v, rest = if (length(rest) == 1) rest[[1]] else list(t = "mul", x = rest)))
  }
  list(c = 1, rest = f)
}

.sc_add <- function(...) .sc_simp(list(t = "add", x = list(...)))
.sc_mul <- function(...) .sc_simp(list(t = "mul", x = list(...)))
.sc_pow <- function(b, x) .sc_simp(list(t = "pow", b = b, e = if (is.list(x)) x else .sc_num(x)))
.sc_fn <- function(nm, a) .sc_simp(list(t = "fn", f = nm, a = a))

.sc_parse <- function(expr) {
  rpn <- .sc_shunting(as.character(expr))$rpn
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
      push(.sc_num(v))
      next
    }
    if (tok == "neg") {
      push(list(t = "mul", x = list(.sc_num(-1), pop())))
    } else if (tok %in% c("+", "-", "*", "/", "^")) {
      b <- pop()
      a <- pop()
      push(switch(tok,
        "+" = list(t = "add", x = list(a, b)),
        "-" = list(t = "add", x = list(a, list(t = "mul", x = list(.sc_num(-1), b)))),
        "*" = list(t = "mul", x = list(a, b)),
        "/" = list(t = "mul", x = list(a, list(t = "pow", b = b, e = .sc_num(-1)))),
        "^" = list(t = "pow", b = a, e = b)
      ))
    } else if (tok == "sqrt") {
      push(list(t = "pow", b = pop(), e = .sc_num(0.5)))
    } else if (tok %in% .sc_fns) {
      push(list(t = "fn", f = tok, a = pop()))
    } else if (tok %in% c("abs", "min", "max")) {
      stop(tok, " is not supported")
    } else if (tok == "pi") {
      push(.sc_num(pi))
    } else {
      push(.sc_sym(tok))
    }
  }
  if (length(st) != 1) stop("malformed expression")
  .sc_simp(st[[1]])
}

.sc_d <- function(e, x) {
  if (.sc_free(e, x)) return(.sc_num(0))
  t <- e$t
  if (t == "sym") return(.sc_num(1))
  if (t == "add") return(.sc_simp(list(t = "add", x = lapply(e$x, .sc_d, x = x))))
  if (t == "mul") {
    fs <- e$x
    terms <- lapply(seq_along(fs), function(i) list(t = "mul", x = c(list(.sc_d(fs[[i]], x)), fs[-i])))
    return(.sc_simp(list(t = "add", x = terms)))
  }
  if (t == "pow") {
    b <- e$b
    n <- e$e
    if (.sc_free(n, x)) return(.sc_mul(n, .sc_pow(b, .sc_add(n, .sc_num(-1))), .sc_d(b, x)))
    if (.sc_free(b, x)) return(.sc_mul(e, .sc_fn("log", b), .sc_d(n, x)))
    return(.sc_mul(e, .sc_add(.sc_mul(.sc_d(n, x), .sc_fn("log", b)), .sc_mul(n, .sc_d(b, x), .sc_pow(b, -1)))))
  }
  u <- e$a
  du <- .sc_d(u, x)
  one_minus_u2 <- function() .sc_add(.sc_num(1), .sc_mul(.sc_num(-1), .sc_pow(u, 2)))
  g <- switch(e$f,
    sin = .sc_fn("cos", u),
    cos = .sc_mul(.sc_num(-1), .sc_fn("sin", u)),
    tan = .sc_pow(.sc_fn("cos", u), -2),
    exp = e,
    log = .sc_pow(u, -1),
    asin = .sc_pow(one_minus_u2(), -0.5),
    acos = .sc_mul(.sc_num(-1), .sc_pow(one_minus_u2(), -0.5)),
    atan = .sc_pow(.sc_add(.sc_num(1), .sc_pow(u, 2)), -1),
    sinh = .sc_fn("cosh", u),
    .sc_fn("sinh", u)
  )
  .sc_mul(g, du)
}

.sc_ev <- function(e, env) {
  switch(e$t,
    num = e$v,
    sym = {
      if (is.null(env[[e$v]])) stop("unbound symbol")
      env[[e$v]]
    },
    add = {
      s <- 0
      for (f in e$x) s <- s + .sc_ev(f, env)
      s
    },
    mul = {
      s <- 1
      for (f in e$x) s <- s * .sc_ev(f, env)
      s
    },
    pow = {
      b <- .sc_ev(e$b, env)
      n <- .sc_ev(e$e, env)
      if (is.nan(b) || is.nan(n)) return(NaN)
      if (b == 0 && n < 0) return(NaN)
      if (b < 0 && !(is.finite(n) && n == trunc(n))) return(NaN)
      b^n
    },
    {
      a <- .sc_ev(e$a, env)
      if (is.nan(a)) return(NaN)
      if (e$f == "log") return(if (a > 0) log(a) else NaN)
      if (e$f %in% c("asin", "acos") && abs(a) > 1) return(NaN)
      v <- suppressWarnings(switch(e$f, sin = sin(a), cos = cos(a), tan = tan(a), exp = exp(a), asin = asin(a),
                                   acos = acos(a), atan = atan(a), sinh = sinh(a), cosh = cosh(a)))
      if (is.finite(v)) v else NaN
    }
  )
}

.sc_padd <- function(p, q) {
  n <- max(length(p), length(q))
  vapply(seq_len(n), function(i) (if (i <= length(p)) p[i] else 0) + (if (i <= length(q)) q[i] else 0), 0)
}

.sc_pmul <- function(p, q) {
  out <- numeric(length(p) + length(q) - 1)
  for (i in seq_along(p)) for (j in seq_along(q)) out[i + j - 1] <- out[i + j - 1] + p[i] * q[j]
  out
}

.sc_ptrim <- function(p) {
  while (length(p) > 1 && p[length(p)] == 0) p <- p[-length(p)]
  p
}

.sc_ev_const <- function(e) {
  v <- tryCatch(.sc_ev(e, list()), error = function(err) NULL)
  if (is.null(v) || !is.finite(v)) NULL else v
}

.sc_poly <- function(e, x) {
  if (.sc_free(e, x)) {
    v <- .sc_ev_const(e)
    return(if (is.null(v)) NULL else v)
  }
  t <- e$t
  if (t == "sym") return(c(0, 1))
  if (t == "add") {
    out <- 0
    for (f in e$x) {
      p <- .sc_poly(f, x)
      if (is.null(p)) return(NULL)
      out <- .sc_padd(out, p)
    }
    return(.sc_ptrim(out))
  }
  if (t == "mul") {
    out <- 1
    for (f in e$x) {
      p <- .sc_poly(f, x)
      if (is.null(p)) return(NULL)
      out <- .sc_pmul(out, p)
    }
    return(.sc_ptrim(out))
  }
  if (t == "pow" && e$e$t == "num" && .sc_isint(e$e$v) && e$e$v > 0 && e$e$v <= 32) {
    p <- .sc_poly(e$b, x)
    if (is.null(p)) return(NULL)
    out <- 1
    for (k in seq_len(e$e$v)) out <- .sc_pmul(out, p)
    return(.sc_ptrim(out))
  }
  NULL
}

.sc_rat <- function(e, x) {
  p <- .sc_poly(e, x)
  if (!is.null(p)) return(list(N = p, D = 1))
  t <- e$t
  if (t == "add") {
    N <- 0
    D <- 1
    for (f in e$x) {
      r <- .sc_rat(f, x)
      if (is.null(r)) return(NULL)
      N <- .sc_padd(.sc_pmul(N, r$D), .sc_pmul(r$N, D))
      D <- .sc_pmul(D, r$D)
    }
    return(list(N = .sc_ptrim(N), D = .sc_ptrim(D)))
  }
  if (t == "mul") {
    N <- 1
    D <- 1
    for (f in e$x) {
      r <- .sc_rat(f, x)
      if (is.null(r)) return(NULL)
      N <- .sc_pmul(N, r$N)
      D <- .sc_pmul(D, r$D)
    }
    return(list(N = .sc_ptrim(N), D = .sc_ptrim(D)))
  }
  if (t == "pow" && e$e$t == "num" && .sc_isint(e$e$v) && e$e$v >= -32 && e$e$v < 0) {
    p <- .sc_poly(e$b, x)
    if (is.null(p)) return(NULL)
    out <- 1
    for (k in seq_len(-e$e$v)) out <- .sc_pmul(out, p)
    return(list(N = 1, D = .sc_ptrim(out)))
  }
  NULL
}

.sc_pdivmod <- function(N, D) {
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
  list(q = .sc_ptrim(q), r = .sc_ptrim(r))
}

.sc_cmul <- function(a, b) c(a[1] * b[1] - a[2] * b[2], a[1] * b[2] + a[2] * b[1])
.sc_cdiv <- function(a, b) {
  den <- b[1] * b[1] + b[2] * b[2]
  c((a[1] * b[1] + a[2] * b[2]) / den, (a[2] * b[1] - a[1] * b[2]) / den)
}

.sc_cpoly <- function(p, z) {
  v <- c(0, 0)
  for (cc in rev(p)) {
    v <- .sc_cmul(v, z)
    v <- c(v[1] + cc, v[2])
  }
  v
}

.sc_roots <- function(p) {
  n <- length(p) - 1
  q <- p / p[length(p)]
  w <- c(0.4, 0.9)
  z <- list(c(1, 0))
  for (k in seq_len(n - 1)) z[[length(z) + 1]] <- .sc_cmul(z[[length(z)]], w)
  z <- lapply(z, function(zz) .sc_cmul(zz, w))
  for (it in 1:500) {
    mx <- 0
    new <- vector("list", n)
    for (i in seq_len(n)) {
      den <- c(1, 0)
      for (j in seq_len(n)) if (j != i) den <- .sc_cmul(den, z[[i]] - z[[j]])
      step <- .sc_cdiv(.sc_cpoly(q, z[[i]]), den)
      new[[i]] <- z[[i]] - step
      mx <- max(mx, abs(step[1]) + abs(step[2]))
    }
    z <- new
    if (mx < 1e-15) break
  }
  z
}

.sc_snap <- function(v) {
  for (den in 1:1000) {
    r <- round(v * den)
    if (abs(v * den - r) < 1e-9 * max(1, abs(v * den))) return(r / den)
  }
  v
}

.sc_cluster <- function(z, p) {
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
        den <- .sc_cpoly(d2, c(re, im))
        if (den[1] == 0 && den[2] == 0) break
        st <- .sc_cdiv(.sc_cpoly(dp, c(re, im)), den)
        re <- re - st[1]
        im <- im - st[2]
      }
    }
    if (abs(im) < 1e-9 * max(1, abs(re))) im <- 0
    out[[length(out) + 1]] <- list(z = c(.sc_snap(re), .sc_snap(im)), mu = length(grp))
  }
  out
}

.sc_tshift <- function(p, r) {
  out <- p
  n <- length(out)
  for (k in seq_len(n - 1)) {
    for (i in rev(k:(n - 1))) {
      m <- .sc_cmul(out[[i + 1]], r)
      out[[i]] <- out[[i]] + m
    }
  }
  out
}

.sc_series_div <- function(a, b, m) {
  out <- list()
  for (k in seq_len(m)) {
    s <- if (k <= length(a)) a[[k]] else c(0, 0)
    for (j in seq_len(k - 1)) {
      if (j + 1 <= length(b)) s <- s - .sc_cmul(b[[j + 1]], out[[k - j]])
    }
    out[[k]] <- .sc_cdiv(s, b[[1]])
  }
  out
}

.sc_int_rational <- function(N, D, x) {
  X <- .sc_sym(x)
  qr <- .sc_pdivmod(N, D)
  terms <- list()
  for (k in seq_along(qr$q)) {
    cc <- qr$q[k]
    if (cc != 0) terms[[length(terms) + 1]] <- .sc_mul(.sc_num(.sc_snap(cc / k)), .sc_pow(X, k))
  }
  r <- qr$r
  if (length(r) == 1 && r[1] == 0) return(if (length(terms)) .sc_simp(list(t = "add", x = terms)) else .sc_num(0))
  if (length(D) - 1 > 12) return(NULL)
  roots <- .sc_cluster(.sc_roots(D), D)
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
        acc <- D1[[i + 1]] + .sc_cmul(acc, c(zr, zi))
        out[[i]] <- acc
      }
      D1 <- out
    }
    a <- .sc_tshift(lapply(r, function(cc) c(cc, 0)), c(zr, zi))
    bs <- .sc_tshift(D1, c(zr, zi))
    g <- .sc_series_div(a, bs, mu)
    for (j in seq_len(mu)) {
      cc <- g[[mu - j + 1]]
      cc <- c(.sc_snap(cc[1]), .sc_snap(cc[2]))
      if (zi == 0) {
        if (cc[1] == 0) next
        lin <- .sc_add(X, .sc_num(-zr))
        terms[[length(terms) + 1]] <- if (j == 1) .sc_mul(.sc_num(cc[1]), .sc_fn("log", lin)) else
          .sc_mul(.sc_num(.sc_snap(-cc[1] / (j - 1))), .sc_pow(lin, -(j - 1)))
      } else {
        if (j > 1) return(NULL)
        quad <- .sc_add(.sc_pow(.sc_add(X, .sc_num(-zr)), 2), .sc_num(.sc_snap(zi * zi)))
        if (cc[1] != 0) terms[[length(terms) + 1]] <- .sc_mul(.sc_num(cc[1]), .sc_fn("log", quad))
        if (cc[2] != 0) {
          terms[[length(terms) + 1]] <- .sc_mul(.sc_num(.sc_snap(-2 * cc[2])),
                                                .sc_fn("atan", .sc_mul(.sc_add(X, .sc_num(-zr)), .sc_num(.sc_snap(1 / zi)))))
        }
      }
    }
  }
  if (length(terms)) .sc_simp(list(t = "add", x = terms)) else .sc_num(0)
}

.sc_linear <- function(u, x) {
  p <- .sc_poly(u, x)
  if (!is.null(p) && length(p) == 2 && p[2] != 0) return(c(p[2], p[1]))
  NULL
}

.sc_table <- function(e, x) {
  X <- .sc_sym(x)
  t <- e$t
  if (t == "sym") return(.sc_mul(.sc_num(0.5), .sc_pow(X, 2)))
  if (t == "pow") {
    b <- e$b
    n <- e$e
    if (.sc_isnum(n, -0.5)) {
      q <- .sc_poly(b, x)
      if (!is.null(q) && length(q) == 3 && q[2] == 0 && q[1] != 0) {
        cc <- q[1]
        a2 <- q[3]
        if (a2 < 0 && cc > 0) return(.sc_mul(.sc_num(.sc_snap(1 / sqrt(-a2))), .sc_fn("asin", .sc_mul(X, .sc_num(.sc_snap(sqrt(-a2 / cc)))))))
        if (a2 > 0) return(.sc_mul(.sc_num(.sc_snap(1 / sqrt(a2))), .sc_fn("log", .sc_add(.sc_mul(.sc_num(.sc_snap(sqrt(a2))), X), .sc_pow(b, 0.5)))))
      }
    }
    lin <- .sc_linear(b, x)
    if (!is.null(lin) && .sc_free(n, x)) {
      a <- lin[1]
      if (.sc_isnum(n, -1)) return(.sc_mul(.sc_num(.sc_snap(1 / a)), .sc_fn("log", b)))
      n1 <- .sc_add(n, .sc_num(1))
      return(.sc_mul(.sc_pow(b, n1), .sc_pow(.sc_mul(.sc_num(a), n1), -1)))
    }
    lin <- .sc_linear(n, x)
    if (!is.null(lin) && .sc_free(b, x)) return(.sc_mul(e, .sc_pow(.sc_mul(.sc_num(lin[1]), .sc_fn("log", b)), -1)))
    return(NULL)
  }
  if (t == "fn") {
    lin <- .sc_linear(e$a, x)
    if (is.null(lin)) return(NULL)
    u <- e$a
    inv <- .sc_num(.sc_snap(1 / lin[1]))
    root <- function() .sc_pow(.sc_add(.sc_num(1), .sc_mul(.sc_num(-1), .sc_pow(u, 2))), 0.5)
    return(switch(e$f,
      sin = .sc_mul(.sc_num(-1), inv, .sc_fn("cos", u)),
      cos = .sc_mul(inv, .sc_fn("sin", u)),
      tan = .sc_mul(.sc_num(-1), inv, .sc_fn("log", .sc_fn("cos", u))),
      exp = .sc_mul(inv, e),
      log = .sc_mul(inv, .sc_add(.sc_mul(u, e), .sc_mul(.sc_num(-1), u))),
      sinh = .sc_mul(inv, .sc_fn("cosh", u)),
      cosh = .sc_mul(inv, .sc_fn("sinh", u)),
      asin = .sc_mul(inv, .sc_add(.sc_mul(u, e), root())),
      acos = .sc_mul(inv, .sc_add(.sc_mul(u, e), .sc_mul(.sc_num(-1), root()))),
      atan = .sc_mul(inv, .sc_add(.sc_mul(u, e), .sc_mul(.sc_num(-0.5), .sc_fn("log", .sc_add(.sc_num(1), .sc_pow(u, 2)))))),
      NULL
    ))
  }
  NULL
}

.sc_factors <- function(e) if (e$t == "mul") e$x else list(e)

.sc_parts <- function(fs, x, depth) {
  isp <- vapply(fs, function(f) !is.null(.sc_poly(f, x)), TRUE)
  polys <- fs[isp]
  rest <- fs[!isp]
  if (length(rest) != 1 || !length(polys)) return(NULL)
  g <- rest[[1]]
  P <- .sc_simp(list(t = "mul", x = polys))
  if (g$t == "fn" && g$f %in% c("exp", "sin", "cos", "sinh", "cosh") && !is.null(.sc_linear(g$a, x))) {
    out <- list()
    sign <- 1
    cur <- g
    Pk <- P
    for (s in 1:40) {
      cur <- .sc_integrate(cur, x, depth + 1)
      if (is.null(cur)) return(NULL)
      out[[length(out) + 1]] <- .sc_mul(.sc_num(sign), Pk, cur)
      Pk <- .sc_d(Pk, x)
      sign <- -sign
      if (.sc_isnum(Pk, 0)) return(.sc_simp(list(t = "add", x = out)))
    }
    return(NULL)
  }
  if (g$t == "fn" && g$f %in% c("log", "atan", "asin", "acos") && !is.null(.sc_linear(g$a, x))) {
    Q <- .sc_integrate(P, x, depth + 1)
    if (is.null(Q)) return(NULL)
    ri <- .sc_integrate(.sc_mul(Q, .sc_d(g, x)), x, depth + 1)
    if (is.null(ri)) return(NULL)
    return(.sc_add(.sc_mul(Q, g), .sc_mul(.sc_num(-1), ri)))
  }
  NULL
}

.sc_exp_trig <- function(fs, x) {
  if (length(fs) != 2) return(NULL)
  ex <- Filter(function(f) f$t == "fn" && f$f == "exp", fs)
  tr <- Filter(function(f) f$t == "fn" && f$f %in% c("sin", "cos"), fs)
  if (length(ex) != 1 || length(tr) != 1) return(NULL)
  la <- .sc_linear(ex[[1]]$a, x)
  lb <- .sc_linear(tr[[1]]$a, x)
  if (is.null(la) || is.null(lb)) return(NULL)
  a <- la[1]
  b <- lb[1]
  v <- tr[[1]]$a
  s <- .sc_fn("sin", v)
  cc <- .sc_fn("cos", v)
  k <- .sc_num(.sc_snap(1 / (a * a + b * b)))
  inner <- if (tr[[1]]$f == "sin") .sc_add(.sc_mul(.sc_num(a), s), .sc_mul(.sc_num(-b), cc)) else
    .sc_add(.sc_mul(.sc_num(a), cc), .sc_mul(.sc_num(b), s))
  .sc_mul(k, ex[[1]], inner)
}

.sc_subexprs <- function(e) {
  out <- list()
  if (e$t %in% c("fn", "pow")) out <- list(e)
  if (e$t == "fn") return(c(out, .sc_subexprs(e$a)))
  if (e$t == "pow") return(c(out, .sc_subexprs(e$b), .sc_subexprs(e$e)))
  if (e$t %in% c("add", "mul")) for (f in e$x) out <- c(out, .sc_subexprs(f))
  out
}

.sc_subst <- function(e, old, new) {
  if (identical(e, old)) return(new)
  switch(e$t,
    fn = list(t = "fn", f = e$f, a = .sc_subst(e$a, old, new)),
    pow = list(t = "pow", b = .sc_subst(e$b, old, new), e = .sc_subst(e$e, old, new)),
    add = , mul = list(t = e$t, x = lapply(e$x, .sc_subst, old = old, new = new)),
    e
  )
}

.sc_usub <- function(e, x, depth) {
  seen <- character(0)
  for (g in .sc_subexprs(e)) {
    k <- .sc_str(g)
    if (k %in% seen || .sc_free(g, x) || !is.null(.sc_linear(g, x))) next
    seen <- c(seen, k)
    cands <- if (g$t == "fn") list(g) else list(g, g$b)
    for (cand in cands) {
      if (.sc_free(cand, x) || !is.null(.sc_linear(cand, x)) || identical(cand, .sc_sym(x))) next
      dg <- .sc_d(cand, x)
      if (.sc_isnum(dg, 0)) next
      u <- .sc_sym("_u")
      h <- .sc_simp(list(t = "mul", x = list(.sc_subst(e, cand, u), list(t = "pow", b = dg, e = .sc_num(-1)))))
      if (!.sc_free(h, x)) next
      H <- .sc_integrate(h, "_u", depth + 1)
      if (!is.null(H)) return(.sc_simp(.sc_subst(H, u, cand)))
    }
  }
  NULL
}

.sc_expand <- function(e) {
  if (e$t != "mul") return(NULL)
  fs <- e$x
  for (i in seq_along(fs)) {
    if (fs[[i]]$t == "add") {
      others <- fs[-i]
      return(.sc_simp(list(t = "add", x = lapply(fs[[i]]$x, function(tt) list(t = "mul", x = c(others, list(tt)))))))
    }
  }
  NULL
}

.sc_integrate <- function(e, x, depth) {
  if (depth > 8) return(NULL)
  X <- .sc_sym(x)
  e <- .sc_simp(e)
  if (.sc_free(e, x)) return(.sc_mul(e, X))
  if (e$t == "add") {
    out <- list()
    for (f in e$x) {
      r <- .sc_integrate(f, x, depth + 1)
      if (is.null(r)) return(NULL)
      out[[length(out) + 1]] <- r
    }
    return(.sc_simp(list(t = "add", x = out)))
  }
  fs <- .sc_factors(e)
  isc <- vapply(fs, .sc_free, TRUE, x = x)
  if (any(isc)) {
    r <- .sc_integrate(.sc_simp(list(t = "mul", x = fs[!isc])), x, depth + 1)
    return(if (is.null(r)) NULL else .sc_simp(list(t = "mul", x = c(fs[isc], list(r)))))
  }
  r <- .sc_table(e, x)
  if (!is.null(r)) return(r)
  rt <- .sc_rat(e, x)
  if (!is.null(rt)) {
    r <- .sc_int_rational(rt$N, rt$D, x)
    if (!is.null(r)) return(r)
  }
  r <- .sc_parts(fs, x, depth)
  if (!is.null(r)) return(r)
  r <- .sc_exp_trig(fs, x)
  if (!is.null(r)) return(r)
  r <- .sc_usub(e, x, depth)
  if (!is.null(r)) return(r)
  ex <- .sc_expand(e)
  if (!is.null(ex) && !identical(ex, e)) return(.sc_integrate(ex, x, depth + 1))
  NULL
}

.sc_check <- function(F, f, x) {
  dF <- .sc_d(F, x)
  worst <- 0
  n <- 0
  for (v in c(0.37, 1.13, 2.71, -0.83, 0.61)) {
    env <- stats::setNames(list(v), x)
    a <- .sc_ev(dF, env)
    b <- .sc_ev(f, env)
    if (is.finite(a) && is.finite(b)) {
      worst <- max(worst, abs(a - b) / max(1, abs(b)))
      n <- n + 1
    }
  }
  list(err = if (n) worst else NaN, n = n)
}

# ---------------------------------------------------------------- polynomial tools for the Risch reduction
.sc_pdeg <- function(p) {
  p <- .sc_ptrim(p)
  if (length(p) == 1 && p[1] == 0) -1 else length(p) - 1
}

.sc_pmonic <- function(p) p / p[length(p)]

.sc_pder <- function(p) if (length(p) > 1) .sc_ptrim(p[-1] * seq_len(length(p) - 1)) else 0

.sc_pzero <- function(p, tol = 1e-10) max(abs(p)) <= tol

.sc_pclean <- function(p, tol = 1e-10) {
  m <- max(abs(p))
  if (m == 0) m <- 1
  p[abs(p) < tol * m] <- 0
  .sc_ptrim(p)
}

.sc_pgcd <- function(a, b) {
  a <- .sc_pclean(a)
  b <- .sc_pclean(b)
  while (!.sc_pzero(b)) {
    r <- .sc_pdivmod(a, b)$r
    a <- b
    b <- .sc_pclean(r)
  }
  if (.sc_pzero(a)) 1 else .sc_pmonic(a)
}

.sc_pxgcd <- function(a, b) {
  r0 <- .sc_pclean(a)
  r1 <- .sc_pclean(b)
  s0 <- 1
  s1 <- 0
  t0 <- 0
  t1 <- 1
  while (!.sc_pzero(r1)) {
    dv <- .sc_pdivmod(r0, r1)
    r0new <- r1
    r1 <- .sc_pclean(dv$r)
    r0 <- r0new
    s <- .sc_pclean(.sc_padd(s0, -.sc_pmul(dv$q, s1)))
    s0 <- s1
    s1 <- s
    tt <- .sc_pclean(.sc_padd(t0, -.sc_pmul(dv$q, t1)))
    t0 <- t1
    t1 <- tt
  }
  lead <- r0[length(r0)]
  list(g = .sc_pmonic(r0), s = s0 / lead, t = t0 / lead)
}

.sc_psquarefree <- function(p) {
  p <- .sc_pmonic(.sc_pclean(p))
  out <- list()
  cc <- .sc_pgcd(p, .sc_pder(p))
  w <- .sc_pclean(.sc_pdivmod(p, cc)$q)
  i <- 1
  while (.sc_pdeg(w) > 0) {
    y <- .sc_pgcd(w, cc)
    g <- .sc_pclean(.sc_pdivmod(w, y)$q)
    if (.sc_pdeg(g) > 0) out[[length(out) + 1]] <- list(m = i, f = .sc_pmonic(g))
    w <- y
    cc <- .sc_pclean(.sc_pdivmod(cc, y)$q)
    i <- i + 1
  }
  out
}

.sc_pmod <- function(a, m) if (.sc_pdeg(a) < .sc_pdeg(m)) .sc_pclean(a) else .sc_pclean(.sc_pdivmod(a, m)$r)

.sc_ppow <- function(p, k) {
  out <- 1
  for (i in seq_len(k)) out <- .sc_pmul(out, p)
  out
}

.sc_poly_expr <- function(p, x) {
  X <- .sc_sym(x)
  terms <- list()
  for (k in seq_along(p)) {
    if (p[k] == 0) next
    terms[[length(terms) + 1]] <- if (k == 1) .sc_num(.sc_snap(p[k])) else
      .sc_mul(.sc_num(.sc_snap(p[k])), .sc_pow(X, k - 1))
  }
  if (length(terms)) .sc_simp(list(t = "add", x = terms)) else .sc_num(0)
}

.sc_hermite <- function(A, D, x) {
  A <- .sc_pclean(A)
  D <- .sc_pmonic(.sc_pclean(D))
  parts <- list()
  repeat {
    sf <- .sc_psquarefree(D)
    tops <- vapply(sf, function(e) if (e$m >= 2 && .sc_pdeg(e$f) > 0) e$m else 0, 0)
    top <- if (length(tops)) max(tops) else 0
    if (top == 0) break
    V <- sf[[which(tops == top)[1]]]$f
    U <- .sc_pclean(.sc_pdivmod(D, .sc_ppow(V, top))$q)
    xg <- .sc_pxgcd(U, .sc_ppow(V, top))
    if (.sc_pdeg(xg$g) > 0) return(NULL)
    A1 <- .sc_pmod(.sc_pmul(A, xg$s), .sc_ppow(V, top))
    A2 <- .sc_pclean(.sc_pdivmod(.sc_pclean(.sc_padd(A, -.sc_pmul(A1, U))), .sc_ppow(V, top))$q)
    xg1 <- .sc_pxgcd(V, .sc_pder(V))
    s1 <- .sc_pmul(A1, xg1$s)
    t1 <- .sc_pmul(A1, xg1$t)
    j <- top - 1
    parts[[length(parts) + 1]] <- list(num = -t1 / j, den = .sc_ppow(V, j))
    rest <- .sc_padd(s1, .sc_pder(t1) / j)
    A <- .sc_pclean(.sc_padd(.sc_pmul(rest, U), .sc_pmul(A2, .sc_ppow(V, j))))
    D <- .sc_pmonic(.sc_pmul(U, .sc_ppow(V, j)))
  }
  dv <- .sc_pdivmod(A, D)
  g <- .sc_num(0)
  for (pt in parts) {
    g <- .sc_add(g, .sc_mul(.sc_poly_expr(.sc_pclean(pt$num), x), .sc_pow(.sc_poly_expr(.sc_pclean(pt$den), x), -1)))
  }
  list(g = .sc_simp(g), q = .sc_pclean(dv$q), B = .sc_pclean(dv$r), D = D)
}

.sc_rothstein_trager <- function(B, D, x) {
  X <- .sc_sym(x)
  if (.sc_pdeg(D) == 0) return(.sc_num(0))
  roots <- .sc_cluster(.sc_roots(D), D)
  if (sum(vapply(roots, function(r) r$mu, 0)) != .sc_pdeg(D) || any(vapply(roots, function(r) r$mu, 0) != 1)) {
    return(NULL)
  }
  Dp <- .sc_pder(D)
  terms <- list()
  for (r in roots) {
    zr <- r$z[1]
    zi <- r$z[2]
    if (zi < 0) next
    cc <- .sc_cdiv(.sc_cpoly(B, c(zr, zi)), .sc_cpoly(Dp, c(zr, zi)))
    cc <- c(.sc_snap(cc[1]), .sc_snap(cc[2]))
    if (zi == 0) {
      if (cc[1] != 0) {
        terms[[length(terms) + 1]] <- .sc_mul(.sc_num(cc[1]), .sc_fn("log", .sc_add(X, .sc_num(.sc_snap(-zr)))))
      }
    } else {
      quad <- .sc_add(.sc_pow(.sc_add(X, .sc_num(.sc_snap(-zr))), 2), .sc_num(.sc_snap(zi * zi)))
      if (cc[1] != 0) terms[[length(terms) + 1]] <- .sc_mul(.sc_num(cc[1]), .sc_fn("log", quad))
      if (cc[2] != 0) {
        terms[[length(terms) + 1]] <- .sc_mul(
          .sc_num(.sc_snap(-2 * cc[2])),
          .sc_fn("atan", .sc_mul(.sc_add(X, .sc_num(.sc_snap(-zr))), .sc_num(.sc_snap(1 / zi))))
        )
      }
    }
  }
  if (length(terms)) .sc_simp(list(t = "add", x = terms)) else .sc_num(0)
}

.sc_generators <- function(e, x) {
  if (e$t == "fn") return(unique(c(e$f, .sc_generators(e$a, x))))
  if (e$t == "pow") {
    g <- character(0)
    if (!(e$e$t == "num" && .sc_isint(e$e$v))) g <- if (!.sc_free(e$b, x)) "algebraic" else "constant power"
    return(unique(c(g, .sc_generators(e$b, x), .sc_generators(e$e, x))))
  }
  if (e$t %in% c("add", "mul")) return(unique(unlist(lapply(e$x, .sc_generators, x = x))))
  character(0)
}

#' Symbolic calculus: limits, symbolic matrices, first-order ODEs and the rational Risch algorithm
#'
#' \code{SymbolicLimit}: the limit of an elementary expression, by
#' substitution when the expression is continuous at the point, by the
#' leading Taylor coefficients of numerator and denominator, by L'Hopital's
#' rule for the indeterminate forms zero over zero and infinity over
#' infinity (at most twelve differentiations), and at an infinite limit point
#' by the exact degree comparison of a rational function, by the dominant
#' monomial of an exp-log expression written as a sum of terms
#' \code{c exp(P(x)) x^a (log x)^b} (Gruntz's most-rapidly-varying comparison
#' restricted to that class), or by the squeeze theorem for a bounded
#' numerator over an unbounded denominator. Anything else raises an error
#' rather than returning a guess.
#'
#' \code{MatrixSymbolic}: determinant by Laplace cofactor expansion, inverse
#' as the adjugate over the determinant, characteristic polynomial
#' \code{det(A - t I)} expanded in \code{eigen_var}, and eigenvalues in
#' closed form when that polynomial is linear or quadratic, numerically from
#' the Durand-Kerner roots when all its coefficients are numbers, and not at
#' all for a higher degree with symbolic entries (Abel-Ruffini).
#'
#' \code{OdeSymbolic}: classification and closed-form solution of
#' \code{dy/dx = f(x, y)} (or of a differential form \code{M dx + N dy = 0}):
#' linear by the integrating factor \code{exp(-int p dx)}, Bernoulli through
#' \code{v = y^(1-n)}, separable as the implicit \code{int dy/h = int g dx +
#' C}, exact through the potential \code{int M dx + int (N - d/dy int M dx)
#' dy}, and homogeneous reported with the substitution \code{v = y/x}. Each
#' criterion is checked at five sample points.
#'
#' \code{RischIntegration}: the rational case of the Risch-Bronstein
#' algorithm: Euclidean division for the polynomial part, Hermite reduction
#' (Yun squarefree decomposition and Bezout identities) for the rational
#' part, and the Rothstein-Trager residues at the roots of the squarefree
#' denominator for the logarithmic part, with conjugate pairs combined into
#' real logarithms and arctangents. The transcendental tower and algebraic
#' extensions are reported as out of scope rather than guessed.
#'
#' The expression core, the shunting-yard parser and the integrator are
#' copied from \code{ExactAlgebra.R} and \code{SymbolicIntegration.R}.
#' Identical to the Python arm \code{morie.fn.symcalc}.
#'
#' @param expr Expression string.
#' @param x Variable name (the integration or limit variable).
#' @param x0 Limit point; \code{"inf"}, \code{"-inf"} and numbers are accepted.
#' @param side \code{"both"}, \code{"+"} or \code{"-"}.
#' @param M Square matrix of numbers or expression strings, or the
#'   \code{M} coefficient of a differential form in \code{OdeSymbolic}.
#' @param eigen_var Name of the characteristic-polynomial variable.
#' @param ode Right-hand side \code{f(x, y)}, or \code{"y' = f(x, y)"}.
#' @param y Name of the dependent variable.
#' @param N The \code{N} coefficient of a differential form.
#' @return \code{SymbolicLimit}: list with limit, method, lhopital_steps and
#'   expression. \code{MatrixSymbolic}: list with determinant, inverse,
#'   char_poly, char_coeffs, eigenvalues, eigenvalues_numeric, trace and n.
#'   \code{OdeSymbolic}: list with classes, solution, form, coefficients,
#'   exponent and rhs. \code{RischIntegration}: list with integral,
#'   polynomial_part, rational_part, log_part, rational, out_of_scope,
#'   generators, verified and max_rel_error.
#' @references Risch, R. H. (1969). The problem of integration in finite
#'   terms. Transactions of the American Mathematical Society 139, 167-189.
#'
#'   Bronstein, M. (1997). Symbolic Integration I: Transcendental Functions.
#'   Springer, sections 2.2 and 2.4.
#'
#'   Yun, D. Y. Y. (1976). On square-free decomposition algorithms.
#'   Proceedings of SYMSAC 76, 26-35.
#'
#'   Gruntz, D. (1996). On Computing Limits in a Symbolic Manipulation
#'   System. PhD thesis, ETH Zurich.
#'
#'   Horn, R. A. and Johnson, C. R. (2013). Matrix Analysis, 2nd edn,
#'   sections 0.3 and 1.2. Cambridge University Press.
#'
#'   Boyce, W. E. and DiPrima, R. C. (2012). Elementary Differential
#'   Equations, 10th edn, sections 2.1, 2.2, 2.4 and 2.6. Wiley.
#' @examples
#' SymbolicLimit("sin(x)/x", "x", 0)$limit
#' SymbolicLimit("x*exp(-x)", "x", "inf")$limit
#' MatrixSymbolic(list(c("a", "b"), c("c", "d")))$determinant
#' OdeSymbolic("dy/dx = 2*x*y")$solution
#' RischIntegration("(x^2 + 1)/(x^3 - x)")$integral
#' @export
RischIntegration <- function(expr, x = "x") {
  e <- .sc_parse(expr)
  gens <- sort(.sc_generators(e, x))
  rat <- .sc_rat(e, x)
  if (is.null(rat)) {
    tr <- setdiff(gens, c("algebraic", "constant power"))
    why <- if (length(tr)) {
      paste0("the transcendental tower (generators: ", paste(tr, collapse = ", "), ") is not implemented")
    } else {
      "algebraic extensions (fractional powers of x) are not implemented"
    }
    return(list(integral = NULL, polynomial_part = NULL, rational_part = NULL, log_part = NULL, rational = FALSE,
                out_of_scope = why, generators = gens, verified = FALSE, max_rel_error = NaN))
  }
  N <- rat$N
  D <- rat$D
  lead <- D[length(D)]
  N <- N / lead
  D <- .sc_pmonic(D)
  dv <- .sc_pdivmod(N, D)
  poly_part <- .sc_num(0)
  for (k in seq_along(dv$q)) {
    if (dv$q[k] != 0) {
      poly_part <- .sc_add(poly_part, .sc_mul(.sc_num(.sc_snap(dv$q[k] / k)), .sc_pow(.sc_sym(x), k)))
    }
  }
  hr <- if (.sc_pdeg(D) > 0) .sc_hermite(.sc_pclean(dv$r), D, x) else list(g = .sc_num(0), q = 0, B = 0, D = 1)
  if (is.null(hr)) {
    return(list(integral = NULL, polynomial_part = .sc_str(poly_part), rational_part = NULL, log_part = NULL,
                rational = TRUE, out_of_scope = "the Hermite reduction failed on this denominator",
                generators = gens, verified = FALSE, max_rel_error = NaN))
  }
  for (k in seq_along(hr$q)) {
    if (hr$q[k] != 0) {
      poly_part <- .sc_add(poly_part, .sc_mul(.sc_num(.sc_snap(hr$q[k] / k)), .sc_pow(.sc_sym(x), k)))
    }
  }
  poly_part <- .sc_simp(poly_part)
  logs <- if (!.sc_pzero(hr$B)) .sc_rothstein_trager(.sc_pclean(hr$B), hr$D, x) else .sc_num(0)
  if (is.null(logs)) {
    return(list(integral = NULL, polynomial_part = .sc_str(poly_part), rational_part = .sc_str(hr$g),
                log_part = NULL, rational = TRUE,
                out_of_scope = "the roots of the squarefree part were not separated", generators = gens,
                verified = FALSE, max_rel_error = NaN))
  }
  total <- .sc_simp(.sc_add(.sc_add(poly_part, hr$g), logs))
  ck <- .sc_check(total, e, x)
  list(integral = .sc_str(total), polynomial_part = .sc_str(poly_part), rational_part = .sc_str(hr$g),
       log_part = .sc_str(logs), rational = TRUE, out_of_scope = NULL, generators = gens,
       verified = ck$n > 0 && ck$err < 1e-8, max_rel_error = ck$err)
}

# ---------------------------------------------------------------- limits
.sc_num_at <- function(e, x, v) {
  out <- try(.sc_ev(e, stats::setNames(list(v), x)), silent = TRUE)
  if (inherits(out, "try-error") || is.null(out)) NaN else out
}

.sc_num_at2 <- function(e, env) {
  out <- try(.sc_ev(e, env), silent = TRUE)
  if (inherits(out, "try-error") || is.null(out)) NaN else out
}

# is the value at x0 the limit? a nearby value must agree with it, so that 1^inf and its like are rejected
.sc_continuous_at <- function(e, x, x0, v, side) {
  hs <- numeric(0)
  if (side %in% c("both", "+")) hs <- c(hs, 1e-6)
  if (side %in% c("both", "-")) hs <- c(hs, -1e-6)
  for (h in hs) {
    w <- .sc_num_at(e, x, x0 + h)
    if (!is.finite(w) || abs(w - v) > 1e-3 * max(1, abs(v))) return(FALSE)
  }
  TRUE
}

.sc_split_ratio <- function(e, x) {
  fs <- .sc_factors(e)
  nu <- list()
  de <- list()
  for (f in fs) {
    if (f$t == "pow" && f$e$t == "num" && f$e$v < 0) {
      de[[length(de) + 1]] <- .sc_pow(f$b, -f$e$v)
    } else {
      nu[[length(nu) + 1]] <- f
    }
  }
  list(nu = if (length(nu)) .sc_simp(list(t = "mul", x = nu)) else .sc_num(1),
       de = if (length(de)) .sc_simp(list(t = "mul", x = de)) else .sc_num(1))
}

.sc_series_coefs <- function(e, x, x0, order) {
  out <- numeric(0)
  d <- e
  fact <- 1
  for (k in 0:order) {
    if (k > 0) {
      d <- .sc_d(d, x)
      fact <- fact * k
    }
    v <- .sc_num_at(d, x, x0)
    if (!is.finite(v)) return(NULL)
    out <- c(out, v / fact)
  }
  out
}

.sc_lead <- function(cs, tol = 1e-9) {
  k <- which(abs(cs) > tol)
  if (!length(k)) NULL else list(k = k[1] - 1, c = cs[k[1]])
}

.sc_rat_limit_inf <- function(e, x) {
  r <- .sc_rat(e, x)
  if (is.null(r)) return(NULL)
  N <- .sc_pclean(r$N)
  D <- .sc_pclean(r$D)
  if (.sc_pzero(D)) return(NULL)
  if (.sc_pzero(N)) return(0)
  dn <- .sc_pdeg(N)
  dd <- .sc_pdeg(D)
  if (dn < dd) return(0)
  if (dn == dd) return(.sc_snap(N[length(N)] / D[length(D)]))
  if (N[length(N)] / D[length(D)] > 0) Inf else -Inf
}

.sc_bounded_fns <- c("sin", "cos", "atan")

.sc_is_bounded <- function(e, x) {
  if (e$t == "num") return(TRUE)
  if (e$t == "sym") return(e$v != x)
  if (e$t == "fn") return(e$f %in% .sc_bounded_fns)
  if (e$t %in% c("mul", "add")) return(all(vapply(e$x, .sc_is_bounded, TRUE, x = x)))
  if (e$t == "pow") return(.sc_is_bounded(e$b, x) && e$e$t == "num" && e$e$v >= 0)
  FALSE
}

.sc_amul <- function(a, b) {
  p <- a$p
  for (k in names(b$p)) p[[k]] <- (if (is.null(p[[k]])) 0 else p[[k]]) + b$p[[k]]
  p <- p[vapply(p, function(v) v != 0, TRUE)]
  list(c = a$c * b$c, p = p, a = a$a + b$a, b = a$b + b$b)
}

.sc_apow <- function(m, k) {
  if (m$c < 0 && k != round(k)) stop("a negative coefficient raised to a fractional power")
  p <- lapply(m$p, function(v) v * k)
  list(c = m$c^k, p = p, a = m$a * k, b = m$b * k)
}

.sc_asym <- function(e, x) {
  if (e$t == "num") return(list(list(c = e$v, p = list(), a = 0, b = 0)))
  if (e$t == "sym") return(if (e$v == x) list(list(c = 1, p = list(), a = 1, b = 0)) else NULL)
  if (e$t == "add") {
    out <- list()
    for (f in e$x) {
      m <- .sc_asym(f, x)
      if (is.null(m)) return(NULL)
      out <- c(out, m)
    }
    return(out)
  }
  if (e$t == "mul") {
    out <- list(list(c = 1, p = list(), a = 0, b = 0))
    for (f in e$x) {
      m <- .sc_asym(f, x)
      if (is.null(m)) return(NULL)
      nxt <- list()
      for (aa in out) for (bb in m) nxt[[length(nxt) + 1]] <- .sc_amul(aa, bb)
      out <- nxt
    }
    return(out)
  }
  if (e$t == "pow") {
    if (e$e$t != "num") return(NULL)
    m <- .sc_asym(e$b, x)
    if (is.null(m) || length(m) != 1) return(NULL)
    return(list(.sc_apow(m[[1]], e$e$v)))
  }
  if (e$t == "fn" && e$f == "exp") {
    m <- .sc_asym(e$a, x)
    if (is.null(m)) return(NULL)
    p <- list()
    for (mm in m) {
      if (length(mm$p) || mm$b != 0) return(NULL)
      key <- as.character(mm$a)
      p[[key]] <- (if (is.null(p[[key]])) 0 else p[[key]]) + mm$c
    }
    cst <- if (!is.null(p[["0"]])) exp(p[["0"]]) else 1
    p[["0"]] <- NULL
    p <- p[vapply(p, function(v) v != 0, TRUE)]
    names(p) <- names(p)
    return(list(list(c = cst, p = p, a = 0, b = 0)))
  }
  if (e$t == "fn" && e$f == "log") {
    m <- .sc_asym(e$a, x)
    if (is.null(m) || length(m) != 1) return(NULL)
    mm <- m[[1]]
    if (length(mm$p) || mm$b != 0 || mm$c <= 0) return(NULL)
    out <- list(list(c = mm$a, p = list(), a = 0, b = 1))
    if (mm$c != 1) out[[2]] <- list(c = log(mm$c), p = list(), a = 0, b = 0)
    return(out)
  }
  NULL
}

.sc_aorder <- function(m) {
  if (length(m$p)) {
    ex <- as.numeric(names(m$p))
    d <- max(ex)
    cf <- m$p[[which(ex == d)[1]]]
    if (cf > 0) c(1, d, cf, m$a, m$b) else c(-1, -d, -cf, -m$a, -m$b)
  } else {
    c(0, 0, 0, m$a, m$b)
  }
}

.sc_ocmp <- function(u, v) {
  for (i in seq_along(u)) {
    if (u[i] > v[i]) return(1)
    if (u[i] < v[i]) return(-1)
  }
  0
}

.sc_asym_limit <- function(e, x) {
  ms <- .sc_asym(e, x)
  if (is.null(ms)) return(NULL)
  ms <- ms[vapply(ms, function(m) m$c != 0, TRUE)]
  if (!length(ms)) return(0)
  ords <- lapply(ms, .sc_aorder)
  best <- ords[[1]]
  for (o in ords) if (.sc_ocmp(o, best) > 0) best <- o
  istop <- vapply(ords, function(o) .sc_ocmp(o, best) == 0, TRUE)
  cf <- sum(vapply(ms[istop], function(m) m$c, 0))
  zero <- c(0, 0, 0, 0, 0)
  if (best[1] == 0 && best[4] == 0 && best[5] == 0) {
    if (all(vapply(ords[!istop], function(o) .sc_ocmp(o, best) < 0, TRUE))) return(cf)
    return(NULL)
  }
  if (.sc_ocmp(best, zero) > 0) {
    if (cf == 0) return(NULL)
    return(if (cf > 0) Inf else -Inf)
  }
  0
}

#' @rdname RischIntegration
#' @export
SymbolicLimit <- function(expr, x = "x", x0 = 0, side = "both") {
  if (!(side %in% c("both", "+", "-"))) stop("side must be 'both', '+' or '-'")
  e <- .sc_parse(expr)
  if (is.character(x0)) {
    s <- tolower(trimws(x0))
    x0 <- if (s %in% c("inf", "+inf", "infinity")) Inf else if (s %in% c("-inf", "-infinity")) -Inf else as.numeric(s)
  }
  x0 <- as.numeric(x0)
  if (is.infinite(x0)) {
    sub <- .sc_simp(.sc_subst(e, .sc_sym(x), .sc_mul(.sc_num(sign(x0)), .sc_pow(.sc_sym(x), -1))))
    pos <- if (x0 > 0) e else .sc_simp(.sc_subst(e, .sc_sym(x), .sc_mul(.sc_num(-1), .sc_sym(x))))
    v <- .sc_rat_limit_inf(pos, x)
    if (!is.null(v)) return(list(limit = v, method = "degrees", lhopital_steps = 0, expression = .sc_str(e)))
    v <- .sc_asym_limit(pos, x)
    if (!is.null(v)) return(list(limit = v, method = "order", lhopital_steps = 0, expression = .sc_str(e)))
    sp <- .sc_split_ratio(pos, x)
    if (.sc_is_bounded(sp$nu, x) && !.sc_is_bounded(sp$de, x)) {
      g <- .sc_asym_limit(sp$de, x)
      if (is.null(g)) g <- .sc_rat_limit_inf(sp$de, x)
      if (!is.null(g) && is.infinite(g)) {
        return(list(limit = 0, method = "squeeze", lhopital_steps = 0, expression = .sc_str(e)))
      }
    }
    inner <- SymbolicLimit(.sc_str(sub), x, 0, side = "+")
    return(list(limit = inner$limit, method = paste0(inner$method, "+reciprocal"),
                lhopital_steps = inner$lhopital_steps, expression = .sc_str(e)))
  }
  v <- .sc_num_at(e, x, x0)
  if (is.finite(v) && .sc_continuous_at(e, x, x0, v, side)) {
    return(list(limit = v, method = "substitution", lhopital_steps = 0, expression = .sc_str(e)))
  }
  sp <- .sc_split_ratio(e, x)
  nu <- sp$nu
  de <- sp$de
  cn <- .sc_series_coefs(nu, x, x0, 8)
  cd <- .sc_series_coefs(de, x, x0, 8)
  if (!is.null(cn) && !is.null(cd)) {
    ln <- .sc_lead(cn)
    ld <- .sc_lead(cd)
    if (!is.null(ln) && !is.null(ld)) {
      if (ln$k >= ld$k) {
        out <- if (ln$k == ld$k) ln$c / ld$c else 0
        return(list(limit = out, method = "series", lhopital_steps = 0, expression = .sc_str(e)))
      }
      sgn <- sign(ln$c / ld$c)
      if ((ld$k - ln$k) %% 2 == 1) {
        if (side == "both") stop("the two one-sided limits differ; pass side='+' or side='-'")
        if (side == "-") sgn <- -sgn
      }
      return(list(limit = Inf * sgn, method = "series", lhopital_steps = 0, expression = .sc_str(e)))
    }
    if (is.null(ln) && is.null(ld)) stop("numerator and denominator vanish identically to the expanded order")
  }
  eps <- if (side == "-") -1e-6 else 1e-6
  steps <- 0
  while (steps < 12) {
    a <- .sc_num_at(nu, x, x0 + eps)
    b <- .sc_num_at(de, x, x0 + eps)
    if (!(is.nan(a) || is.nan(b))) {
      zero <- abs(a) < 1e-4 && abs(b) < 1e-4
      big <- abs(a) > 1e4 && abs(b) > 1e4
      if (!(zero || big)) break
    }
    nu <- .sc_d(nu, x)
    de <- .sc_d(de, x)
    steps <- steps + 1
    w <- .sc_num_at(.sc_simp(.sc_mul(nu, .sc_pow(de, -1))), x, x0)
    if (is.finite(w)) {
      return(list(limit = w, method = "lhopital", lhopital_steps = steps, expression = .sc_str(e)))
    }
  }
  stop("cannot determine this limit with substitution, series, L'Hopital or order comparison")
}

# ---------------------------------------------------------------- symbolic linear algebra
.sc_minor <- function(rows, dr, dc) {
  n <- length(rows)
  lapply(seq_len(n)[-dr], function(i) rows[[i]][-dc])
}

.sc_det_expr <- function(rows) {
  n <- length(rows)
  if (n == 1) return(rows[[1]][[1]])
  if (n == 2) {
    return(.sc_simp(.sc_add(.sc_mul(rows[[1]][[1]], rows[[2]][[2]]),
                            .sc_mul(.sc_num(-1), .sc_mul(rows[[1]][[2]], rows[[2]][[1]])))))
  }
  total <- .sc_num(0)
  for (j in seq_len(n)) {
    if (identical(rows[[1]][[j]], .sc_num(0))) next
    sub <- .sc_det_expr(.sc_minor(rows, 1, j))
    total <- .sc_add(total, .sc_mul(.sc_num(if (j %% 2 == 0) -1 else 1), .sc_mul(rows[[1]][[j]], sub)))
  }
  .sc_simp(total)
}

.sc_pdeg_expr <- function(e, x, cap = 8) {
  d <- e
  for (k in 0:cap) {
    if (.sc_free(d, x)) return(k)
    d <- .sc_simp(.sc_d(d, x))
  }
  stop("the expression is not polynomial in ", x)
}

.sc_coefs_expr <- function(e, x) {
  out <- list()
  d <- e
  fact <- 1
  for (k in 0:.sc_pdeg_expr(e, x)) {
    if (k > 0) {
      d <- .sc_d(d, x)
      fact <- fact * k
    }
    out[[k + 1]] <- .sc_simp(.sc_mul(.sc_num(1 / fact), .sc_subst(d, .sc_sym(x), .sc_num(0))))
  }
  out
}

#' @rdname RischIntegration
#' @export
MatrixSymbolic <- function(M, eigen_var = "t") {
  rows <- lapply(seq_along(M), function(i) {
    lapply(M[[i]], function(v) if (is.character(v)) .sc_parse(v) else .sc_num(as.numeric(v)))
  })
  n <- length(rows)
  if (any(vapply(rows, length, 0) != n)) stop("the matrix must be square")
  if (n > 6) stop("symbolic cofactor expansion is limited to 6 by 6")
  det <- .sc_det_expr(rows)
  T <- .sc_sym(eigen_var)
  shifted <- lapply(seq_len(n), function(i) {
    lapply(seq_len(n), function(j) if (i == j) .sc_add(rows[[i]][[j]], .sc_mul(.sc_num(-1), T)) else rows[[i]][[j]])
  })
  cp <- .sc_det_expr(shifted)
  if (n %% 2 == 1) cp <- .sc_simp(.sc_mul(.sc_num(-1), cp))
  cc <- .sc_coefs_expr(cp, eigen_var)
  cp <- .sc_simp(list(t = "add", x = lapply(seq_along(cc), function(k) {
    if (k == 1) cc[[k]] else .sc_mul(cc[[k]], .sc_pow(T, k - 1))
  })))
  coefs <- .sc_poly(cp, eigen_var)
  inv <- NULL
  if (!(det$t == "num" && det$v == 0)) {
    inv <- character(0)
    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        cof <- if (n > 1) .sc_det_expr(.sc_minor(rows, j, i)) else .sc_num(1)
        sgn <- if ((i + j) %% 2 == 1) -1 else 1
        inv <- c(inv, .sc_str(.sc_simp(.sc_mul(.sc_num(sgn), .sc_mul(cof, .sc_pow(det, -1))))))
      }
    }
  }
  eig <- NULL
  eig_num <- NULL
  if (!is.null(coefs) && all(is.finite(coefs))) {
    p <- .sc_ptrim(vapply(coefs, .sc_snap, 0))
    if (.sc_pdeg(p) >= 1) {
      rt <- .sc_cluster(.sc_roots(p), p)
      vals <- numeric(0)
      for (r in rt) if (abs(r$z[2]) < 1e-9) vals <- c(vals, rep(.sc_snap(r$z[1]), r$mu))
      eig_num <- if (length(vals) == .sc_pdeg(p)) sort(vals) else NULL
    }
  }
  if (length(cc) == 2) {
    eig <- .sc_str(.sc_simp(.sc_mul(.sc_num(-1), .sc_mul(cc[[1]], .sc_pow(cc[[2]], -1)))))
  } else if (length(cc) == 3) {
    disc <- .sc_simp(.sc_add(.sc_pow(cc[[2]], 2), .sc_mul(.sc_num(-4), .sc_mul(cc[[3]], cc[[1]]))))
    root <- if (disc$t == "num" && disc$v >= 0) .sc_num(sqrt(disc$v)) else .sc_pow(disc, 0.5)
    eig <- vapply(c(1, -1), function(sg) {
      .sc_str(.sc_simp(.sc_mul(.sc_add(.sc_mul(.sc_num(-1), cc[[2]]), .sc_mul(.sc_num(sg), root)),
                               .sc_pow(.sc_mul(.sc_num(2), cc[[3]]), -1))))
    }, "")
  }
  list(determinant = .sc_str(det), inverse = inv, char_poly = .sc_str(cp),
       char_coeffs = vapply(cc, .sc_str, ""), eigenvalues = eig, eigenvalues_numeric = eig_num,
       trace = .sc_str(.sc_simp(list(t = "add", x = lapply(seq_len(n), function(i) rows[[i]][[i]])))), n = n)
}

# ---------------------------------------------------------------- first-order ODEs
.sc_ode_pts <- list(c(0.3, 0.7), c(1.1, 0.4), c(-0.6, 1.3), c(2.2, -0.9), c(0.9, 2.5))

.sc_zero_at <- function(e, pts) {
  for (env in pts) {
    v <- .sc_num_at2(e, env)
    if (is.nan(v) || abs(v) > 1e-9) return(FALSE)
  }
  TRUE
}

.sc_expify <- function(P) {
  terms <- if (P$t == "add") P$x else list(P)
  facs <- list()
  rest <- list()
  for (tt in terms) {
    if (tt$t == "fn" && tt$f == "log") {
      facs[[length(facs) + 1]] <- tt$a
    } else if (tt$t == "mul" && length(tt$x) == 2 && tt$x[[1]]$t == "num" && tt$x[[2]]$t == "fn" &&
                 tt$x[[2]]$f == "log") {
      facs[[length(facs) + 1]] <- .sc_pow(tt$x[[2]]$a, tt$x[[1]]$v)
    } else {
      rest[[length(rest) + 1]] <- tt
    }
  }
  out <- if (length(facs)) .sc_simp(list(t = "mul", x = facs)) else .sc_num(1)
  if (length(rest)) out <- .sc_simp(.sc_mul(out, .sc_fn("exp", .sc_simp(list(t = "add", x = rest)))))
  .sc_simp(out)
}

#' @rdname RischIntegration
#' @export
OdeSymbolic <- function(ode = NULL, x = "x", y = "y", M = NULL, N = NULL) {
  pts <- lapply(.sc_ode_pts, function(p) stats::setNames(list(p[1], p[2]), c(x, y)))
  if (!is.null(M) || !is.null(N)) {
    if (is.null(M) || is.null(N)) stop("a differential form needs both M and N")
    Me <- .sc_parse(M)
    Ne <- .sc_parse(N)
    rhs <- .sc_str(.sc_simp(.sc_mul(.sc_num(-1), .sc_mul(Me, .sc_pow(Ne, -1)))))
    if (!.sc_zero_at(.sc_simp(.sc_add(.sc_d(Me, y), .sc_mul(.sc_num(-1), .sc_d(Ne, x)))), pts)) {
      return(list(classes = character(0), solution = NULL, form = NULL,
                  coefficients = list(M = .sc_str(Me), N = .sc_str(Ne)), exponent = NULL, rhs = rhs))
    }
    F <- .sc_integrate(Me, x, 0)
    sol <- NULL
    if (!is.null(F)) {
      rest <- .sc_integrate(.sc_simp(.sc_add(Ne, .sc_mul(.sc_num(-1), .sc_d(F, y)))), y, 0)
      if (!is.null(rest)) sol <- paste(.sc_str(.sc_simp(.sc_add(F, rest))), "= C")
    }
    return(list(classes = "exact", solution = sol, form = "implicit",
                coefficients = list(M = .sc_str(Me), N = .sc_str(Ne)), exponent = NULL, rhs = rhs))
  }
  s <- as.character(ode)
  if (grepl("=", s, fixed = TRUE)) {
    lhs <- sub("=.*$", "", s)
    if (!(gsub(" ", "", lhs) %in% c(paste0(y, "'"), paste0("d", y, "/d", x)))) {
      stop("the left-hand side must be ", y, "' or d", y, "/d", x)
    }
    s <- sub("^[^=]*=", "", s)
  }
  f <- .sc_parse(s)
  classes <- character(0)
  coefs <- list()
  solution <- NULL
  form <- NULL
  n_exp <- NULL
  fy <- .sc_simp(.sc_d(f, y))
  fyy <- .sc_simp(.sc_d(fy, y))
  if (.sc_zero_at(fyy, pts) && .sc_free(fy, y)) {
    classes <- c(classes, "linear")
    p <- fy
    q <- .sc_simp(.sc_subst(f, .sc_sym(y), .sc_num(0)))
    coefs$p <- .sc_str(p)
    coefs$q <- .sc_str(q)
    P <- .sc_integrate(.sc_simp(.sc_mul(.sc_num(-1), p)), x, 0)
    if (!is.null(P)) {
      mu <- .sc_expify(P)
      inv <- .sc_expify(.sc_simp(.sc_mul(.sc_num(-1), P)))
      inner <- .sc_integrate(.sc_simp(.sc_mul(mu, q)), x, 0)
      if (!is.null(inner)) {
        solution <- .sc_str(.sc_simp(.sc_mul(.sc_add(inner, .sc_sym("C")), inv)))
        form <- "explicit"
      }
    }
  }
  if (!length(classes)) {
    terms <- if (f$t == "add") f$x else list(f)
    pw <- list()
    ok <- TRUE
    for (term in terms) {
      k <- NULL
      rest <- list()
      for (fac in .sc_factors(term)) {
        if (identical(fac, .sc_sym(y))) {
          k <- 1
        } else if (fac$t == "pow" && identical(fac$b, .sc_sym(y)) && fac$e$t == "num") {
          k <- fac$e$v
        } else if (!.sc_free(fac, y)) {
          ok <- FALSE
        } else {
          rest[[length(rest) + 1]] <- fac
        }
      }
      if (!ok) break
      pw[[length(pw) + 1]] <- list(k = if (is.null(k)) 0 else k,
                                   c = if (length(rest)) .sc_simp(list(t = "mul", x = rest)) else .sc_num(1))
    }
    ks <- sort(vapply(pw, function(e) e$k, 0))
    if (ok && length(pw) == 2 && !identical(ks, c(0, 1))) {
      if (1 %in% ks) {
        n_exp <- ks[ks != 1][1]
        p <- pw[[which(vapply(pw, function(e) e$k == 1, TRUE))[1]]]$c
        q <- pw[[which(vapply(pw, function(e) e$k != 1, TRUE))[1]]]$c
      } else {
        n_exp <- ks[2]
        p <- .sc_num(0)
        q <- pw[[which(vapply(pw, function(e) e$k == ks[2], TRUE))[1]]]$c
      }
      if (!(n_exp %in% c(0, 1))) {
        classes <- c(classes, "bernoulli")
        coefs$p <- .sc_str(p)
        coefs$q <- .sc_str(q)
        m <- 1 - n_exp
        P <- .sc_integrate(.sc_simp(.sc_mul(.sc_num(-m), p)), x, 0)
        if (!is.null(P)) {
          mu <- .sc_expify(P)
          inv <- .sc_expify(.sc_simp(.sc_mul(.sc_num(-1), P)))
          inner <- .sc_integrate(.sc_simp(.sc_mul(mu, .sc_mul(.sc_num(m), q))), x, 0)
          if (!is.null(inner)) {
            v <- .sc_simp(.sc_mul(.sc_add(inner, .sc_sym("C")), inv))
            solution <- .sc_str(.sc_simp(.sc_pow(v, 1 / m)))
            form <- "explicit"
          }
        }
      }
    }
  }
  logdx <- .sc_simp(.sc_mul(.sc_d(f, x), .sc_pow(f, -1)))
  if (.sc_zero_at(.sc_simp(.sc_d(logdx, y)), pts)) {
    classes <- c("separable", classes)
    g <- NULL
    h <- NULL
    for (y1 in c(1, 2, 0.5, -1, 3)) {
      cand <- .sc_simp(.sc_subst(f, .sc_sym(y), .sc_num(y1)))
      if (.sc_zero_at(cand, pts)) next
      for (x1 in c(1, 2, 0.5, -1, 3)) {
        den <- .sc_simp(.sc_subst(cand, .sc_sym(x), .sc_num(x1)))
        scale <- .sc_num_at2(den, list())
        if (is.finite(scale) && abs(scale) > 1e-8) {
          g <- cand
          h <- .sc_simp(.sc_mul(.sc_subst(f, .sc_sym(x), .sc_num(x1)), .sc_pow(den, -1)))
          break
        }
      }
      if (!is.null(g)) break
    }
    if (!is.null(g)) {
      coefs$g <- .sc_str(g)
      coefs$h <- .sc_str(h)
      if (is.null(solution)) {
        L <- .sc_integrate(.sc_simp(.sc_pow(h, -1)), y, 0)
        R <- .sc_integrate(g, x, 0)
        if (!is.null(L) && !is.null(R)) {
          solution <- paste(.sc_str(.sc_simp(L)), "=", .sc_str(.sc_simp(.sc_add(R, .sc_sym("C")))))
          form <- "implicit"
        }
      }
    }
  }
  scaled <- .sc_simp(.sc_subst(.sc_subst(f, .sc_sym(x), .sc_mul(.sc_num(1.7), .sc_sym(x))),
                               .sc_sym(y), .sc_mul(.sc_num(1.7), .sc_sym(y))))
  if (.sc_zero_at(.sc_simp(.sc_add(scaled, .sc_mul(.sc_num(-1), f))), pts)) classes <- c(classes, "homogeneous")
  if (.sc_zero_at(.sc_simp(.sc_d(f, y)), pts)) classes <- c(classes, "exact")
  list(classes = classes, solution = solution, form = form, coefficients = coefs, exponent = n_exp,
       rhs = .sc_str(f))
}
