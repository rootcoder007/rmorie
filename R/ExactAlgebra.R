#' Exact algebra: Jordan form, Galois groups, shunting yard
#'
#' \code{JordanCanonical}: Jordan canonical form A = P J P^-1 of an integer
#' matrix with integer spectrum in exact (rational) arithmetic: characteristic
#' polynomial by Faddeev-LeVerrier, integer roots by the rational-root
#' theorem, block counts from rank N^(k-1) - rank N^k and Jordan chains grown
#' from the longest blocks down. \code{GaloisGroup}: Galois group over Q of an
#' integer polynomial of degree 1 to 4 (Kappe-Warren test for quartics).
#' \code{ShuntingYard}: Dijkstra's infix to reverse Polish conversion and
#' evaluation. Identical to the Python arm \code{morie.fn.symalg}; exact
#' while the integers involved stay below 2^53.
#'
#' @param A Square integer matrix.
#' @param poly Integer coefficients, highest degree first.
#' @param tokens Expression string or character vector of tokens.
#' @param variables Named list of variable values.
#' @return List. JordanCanonical: J, P, blocks (eigenvalue, size), charpoly,
#'   eigenvalues. GaloisGroup: group, order, degree, monic, discriminant,
#'   resolvent and roots or factors. ShuntingYard: rpn, value, tokens.
#' @references Horn, R. A. and Johnson, C. R. (2013). Matrix Analysis, 2nd
#'   edn. Cambridge University Press.
#'
#'   Kappe, L.-C. and Warren, B. (1989). An elementary test for the Galois
#'   group of a quartic polynomial. American Mathematical Monthly 96,
#'   133-137.
#'
#'   Dijkstra, E. W. (1961). Algol 60 translation. Mathematisch Centrum
#'   report MR 35/61, Amsterdam.
#' @examples
#' JordanCanonical(rbind(c(5, 4, 2, 1), c(0, 1, -1, -1), c(-1, -1, 3, 0), c(1, 1, -1, 2)))$J
#' GaloisGroup(c(1, 0, 0, 0, -2))$group
#' ShuntingYard("3 + 4 * 2 / (1 - 5) ^ 2 ^ 3")$value
#' @export
JordanCanonical <- function(A) {
  A <- as.matrix(A)
  n <- nrow(A)
  if (ncol(A) != n) stop("A must be square")
  if (any(A != round(A))) stop("A must have integer entries")
  A <- round(A) + 0
  cp <- .sy_charpoly(A)
  ir <- .sy_introots(cp)
  roots <- ir$roots
  if (length(ir$rest) > 1) stop("eigenvalues are not all integers; the exact Jordan form needs an algebraic extension")
  eig <- sort(unique(roots))
  cols <- list()
  blocks <- list()
  for (lam in eig) {
    mult <- sum(roots == lam)
    N <- A - lam * diag(n)
    powers <- list(diag(n))
    ranks <- n
    while (n - ranks[length(ranks)] < mult) {
      powers[[length(powers) + 1]] <- powers[[length(powers)]] %*% N
      ranks <- c(ranks, .sy_rank(powers[[length(powers)]]))
    }
    m <- length(ranks) - 1
    chains <- list()
    for (k in rev(seq_len(m))) {
      nge <- ranks[k] - ranks[k + 1]
      ngt <- if (k < m) ranks[k + 1] - ranks[k + 2] else 0
      need <- nge - ngt
      if (need == 0) next
      base <- if (k > 1) .sy_nullspace(powers[[k]]) else list()
      have <- list()
      for (ch in chains) have <- c(have, ch[seq_len(min(k, length(ch)))])
      found <- 0
      for (v in .sy_nullspace(powers[[k + 1]])) {
        if (.sy_rank_rows(c(base, have, list(v))) > .sy_rank_rows(c(base, have))) {
          chain <- list(v)
          for (s in seq_len(k - 1)) chain <- c(list(as.vector(N %*% chain[[1]])), chain)
          chains[[length(chains) + 1]] <- chain
          have <- c(have, chain)
          found <- found + 1
          if (found == need) break
        }
      }
    }
    for (ch in chains) {
      cols <- c(cols, ch)
      blocks[[length(blocks) + 1]] <- c(lam, length(ch))
    }
  }
  P <- matrix(unlist(cols), n, n)
  J <- matrix(0, n, n)
  pos <- 0
  for (b in blocks) {
    for (t in seq_len(b[2])) {
      J[pos + t, pos + t] <- b[1]
      if (t > 1) J[pos + t - 1, pos + t] <- 1
    }
    pos <- pos + b[2]
  }
  list(J = J, P = P, blocks = do.call(rbind, blocks), charpoly = cp, eigenvalues = eig)
}

.sy_gcd <- function(a, b) {
  a <- abs(a)
  b <- abs(b)
  while (b > 0) {
    t <- a %% b
    a <- b
    b <- t
  }
  a
}

.sy_rref <- function(M) {
  M <- as.matrix(M)
  m <- nrow(M)
  n <- ncol(M)
  Nm <- M + 0
  Dm <- matrix(1, m, n)
  red <- function(a, b) {
    if (a == 0) return(c(0, 1))
    g <- .sy_gcd(a, b)
    if (b < 0) g <- -g
    c(a / g, b / g)
  }
  piv <- integer(0)
  r <- 1
  for (cc in seq_len(n)) {
    if (r > m) break
    p <- which(Nm[r:m, cc] != 0)
    if (length(p) == 0) next
    p <- p[1] + r - 1
    tn <- Nm[r, ]
    td <- Dm[r, ]
    Nm[r, ] <- Nm[p, ]
    Dm[r, ] <- Dm[p, ]
    Nm[p, ] <- tn
    Dm[p, ] <- td
    pn <- Nm[r, cc]
    pd <- Dm[r, cc]
    for (j in seq_len(n)) {
      q <- red(Nm[r, j] * pd, Dm[r, j] * pn)
      Nm[r, j] <- q[1]
      Dm[r, j] <- q[2]
    }
    for (i in seq_len(m)) {
      if (i != r && Nm[i, cc] != 0) {
        fn <- Nm[i, cc]
        fd <- Dm[i, cc]
        for (j in seq_len(n)) {
          q <- red(Nm[i, j] * fd * Dm[r, j] - fn * Nm[r, j] * Dm[i, j], Dm[i, j] * fd * Dm[r, j])
          Nm[i, j] <- q[1]
          Dm[i, j] <- q[2]
        }
      }
    }
    piv <- c(piv, cc)
    r <- r + 1
  }
  list(N = Nm, D = Dm, piv = piv)
}

.sy_rank <- function(M) length(.sy_rref(M)$piv)

.sy_rank_rows <- function(rows) if (length(rows) == 0) 0 else .sy_rank(do.call(rbind, rows))

.sy_nullspace <- function(M) {
  n <- ncol(M)
  R <- .sy_rref(M)
  free <- setdiff(seq_len(n), R$piv)
  lapply(free, function(f) {
    vn <- numeric(n)
    vd <- rep(1, n)
    vn[f] <- 1
    for (i in seq_along(R$piv)) {
      vn[R$piv[i]] <- -R$N[i, f]
      vd[R$piv[i]] <- R$D[i, f]
    }
    den <- 1
    for (d in vd) den <- den * d / .sy_gcd(den, d)
    w <- vn * (den / vd)
    g <- 0
    for (x in w) g <- .sy_gcd(g, x)
    if (g > 0) w / g else w
  })
}

.sy_charpoly <- function(A) {
  n <- nrow(A)
  M <- matrix(0, n, n)
  cf <- 1
  for (k in seq_len(n)) {
    M <- A %*% M + cf[length(cf)] * diag(n)
    cf <- c(cf, -sum(diag(A %*% M)) / k)
  }
  cf
}

.sy_peval <- function(cf, x) {
  v <- 0
  for (a in cf) v <- v * x + a
  v
}

.sy_introots <- function(cf) {
  roots <- numeric(0)
  while (length(cf) > 1 && cf[length(cf)] == 0) {
    roots <- c(roots, 0)
    cf <- cf[-length(cf)]
  }
  if (length(cf) > 1) {
    d0 <- abs(cf[length(cf)])
    dv <- which(d0 %% seq_len(d0) == 0)
    for (r in sort(c(-dv, dv))) {
      while (length(cf) > 1 && .sy_peval(cf, r) == 0) {
        roots <- c(roots, r)
        out <- cf[1]
        for (a in cf[-c(1, length(cf))]) out <- c(out, a + out[length(out)] * r)
        cf <- out
      }
    }
  }
  list(roots = sort(roots), rest = cf)
}

.sy_issq <- function(d) d >= 0 && floor(sqrt(d) + 0.5)^2 == d

.sy_disc3 <- function(a, b, c) a * a * b * b - 4 * b^3 - 4 * a^3 * c - 27 * c * c + 18 * a * b * c

.sy_quadfactor <- function(cf) {
  a <- cf[2]
  b <- cf[3]
  cc <- cf[4]
  d <- cf[5]
  if (d == 0) return(NULL)
  d0 <- abs(d)
  for (q0 in which(d0 %% seq_len(d0) == 0)) {
    for (q in c(q0, -q0)) {
      s <- d / q
      if (s != q) {
        num <- cc - q * a
        if (num %% (s - q) == 0) {
          p <- num / (s - q)
          r <- a - p
          if (q + s + p * r == b) return(c(p, q, r, s))
        }
      } else {
        disc <- a * a - 4 * (b - q - s)
        if (.sy_issq(disc) && (a + floor(sqrt(disc) + 0.5)) %% 2 == 0) {
          p <- (a + floor(sqrt(disc) + 0.5)) / 2
          r <- a - p
          if (p * s + q * r == cc) return(c(p, q, r, s))
        }
      }
    }
  }
  NULL
}

#' @rdname JordanCanonical
#' @export
GaloisGroup <- function(poly) {
  cf <- as.numeric(poly)
  while (length(cf) > 0 && cf[1] == 0) cf <- cf[-1]
  n <- length(cf) - 1
  if (n < 1 || n > 4) stop("degree must be between 1 and 4")
  a0 <- cf[1]
  cf <- c(1, vapply(seq_len(n), function(k) cf[k + 1] * a0^(k - 1), 0))
  ir <- .sy_introots(cf)
  out <- list(degree = n, monic = cf, discriminant = NULL, resolvent = NULL)
  if (n == 1) return(c(out, list(group = "trivial", order = 1)))
  if (length(ir$roots) > 0) return(c(out, list(group = "reducible", order = NULL, integer_roots = ir$roots, cofactor = ir$rest)))
  if (n == 2) {
    out$discriminant <- cf[2]^2 - 4 * cf[3]
    return(c(out, list(group = "C2", order = 2)))
  }
  if (n == 3) {
    D <- .sy_disc3(cf[2], cf[3], cf[4])
    out$discriminant <- D
    g <- if (.sy_issq(D)) "A3" else "S3"
    return(c(out, list(group = g, order = if (g == "A3") 3 else 6)))
  }
  fac <- .sy_quadfactor(cf)
  if (!is.null(fac)) return(c(out, list(group = "reducible", order = NULL, quadratic_factors = list(c(1, fac[1:2]), c(1, fac[3:4])))))
  a <- cf[2]
  b <- cf[3]
  cc <- cf[4]
  d <- cf[5]
  R <- c(1, -b, a * cc - 4 * d, -(a * a * d - 4 * b * d + cc * cc))
  D <- .sy_disc3(R[2], R[3], R[4])
  rr <- sort(unique(.sy_introots(R)$roots))
  out$discriminant <- D
  out$resolvent <- R
  out$resolvent_roots <- rr
  splits <- function(x) .sy_issq(x) || .sy_issq(x * D)
  g <- if (length(rr) == 0) {
    if (.sy_issq(D)) "A4" else "S4"
  } else if (length(rr) == 3) {
    "V4"
  } else {
    r <- rr[1]
    if (splits(r * r - 4 * d) && splits(a * a - 4 * (b - r))) "C4" else "D4"
  }
  out$group <- g
  out$order <- c(A4 = 12, S4 = 24, V4 = 4, C4 = 4, D4 = 8)[[g]]
  out
}

.sy_prec <- c("+" = 1, "-" = 1, "*" = 2, "/" = 2, "^" = 4, neg = 3)
.sy_funcs <- c("sin", "cos", "tan", "exp", "log", "sqrt", "abs")

.sy_tokenize <- function(s) {
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

.sy_isnum <- function(t) !is.na(suppressWarnings(as.numeric(t)))

.sy_call <- function(f, x) {
  v <- suppressWarnings(switch(f, sin = sin(x), cos = cos(x), tan = tan(x), exp = exp(x), log = if (x == 0) NaN else log(x),
                               sqrt = sqrt(x), abs = abs(x)))
  if (is.infinite(v) && f != "exp") NaN else v
}

.sy_binop <- function(t, a, b) {
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

#' @rdname JordanCanonical
#' @export
ShuntingYard <- function(tokens, variables = NULL) {
  toks <- if (length(tokens) == 1 && is.character(tokens)) .sy_tokenize(tokens) else as.character(tokens)
  prec <- .sy_prec
  isfun <- function(t) t %in% c(.sy_funcs, "min", "max")
  out <- character(0)
  st <- character(0)
  prev <- NULL
  unaryctx <- function() is.null(prev) || prev %in% names(prec) || prev %in% c("(", ",")
  for (t in toks) {
    if (.sy_isnum(t)) {
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
  known <- vapply(out, function(t) .sy_isnum(t) || t %in% names(prec) || isfun(t) || t %in% names(variables), TRUE)
  if (all(known)) {
    vs <- numeric(0)
    for (t in out) {
      if (.sy_isnum(t)) {
        vs <- c(vs, as.numeric(t))
      } else if (t %in% names(variables)) {
        vs <- c(vs, as.numeric(variables[[t]]))
      } else if (t == "neg") {
        vs[length(vs)] <- -vs[length(vs)]
      } else if (t %in% .sy_funcs) {
        vs[length(vs)] <- .sy_call(t, vs[length(vs)])
      } else {
        b <- vs[length(vs)]
        a <- vs[length(vs) - 1]
        vs <- c(vs[seq_len(length(vs) - 2)], .sy_binop(t, a, b))
      }
    }
    if (length(vs) != 1) stop("malformed expression")
    val <- vs
  }
  list(rpn = out, value = val, tokens = toks)
}
