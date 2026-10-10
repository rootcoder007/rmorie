# SPDX-License-Identifier: AGPL-3.0-or-later
# Linear and generalized linear mixed models, native.
#
# Source: Bates, D., Maechler, M., Bolker, B. and Walker, S. (2015),
# Fitting linear mixed-effects models using lme4, Journal of
# Statistical Software 67(1), 1-48.  The model is
#     y = X beta + Z b + e,   b = Lambda(theta) u,   u ~ N(0, sigma^2 I),
# with Lambda block diagonal: one lower-triangular factor T_k per
# random-effects term, repeated once per level of its grouping factor.
# For given theta the penalized least-squares system (Sec. 3.2-3.4)
#     L L' = Lambda' Z' W Z Lambda + I,   RZX = L^{-1} Lambda' Z' W X,
#     RX' RX = X' W X - RZX' RZX
# gives beta, u and the penalized residual sum of squares r2, and the
# profiled deviance (eqs. 34 and 41) is minimised over theta alone.
#
# No n x n (nor n x q) matrix is ever formed.  Random effects are
# split into
#   * block 1: the grouping factor with the most random effects.  Its
#     part of L L' is block diagonal (one q1 x q1 block per level), so
#     all of its algebra runs "batched": arrays indexed (level, i, j)
#     and loops over the q1 columns only, never over levels or rows.
#     Its crossproducts come from one rowsum() pass over the data.
#   * the rest (other grouping factors, crossed or nested): a dense
#     matrix of size q2 = sum(levels x columns) of those factors,
#     reached through the Schur complement of block 1, so only q2 is
#     ever factorised densely.
# One evaluation costs O(n q1^2 + m1 q1^3) plus, for the rest, the
# Schur update and a dense Cholesky, O(q2^3).  The Schur update
# K'K is accumulated from the co-occurring non-zeros of each level of
# block 1 when that is sparse (crossed designs: sum over levels of
# nnz^2 terms) and by a dense O(m1 q1 q2^2) product otherwise.
# Measured with reference BLAS: q2 = 200 about 15 ms per evaluation,
# q2 = 1000 about 0.5 s.  Above q2 = 5000 a warning is given (each
# evaluation then takes many seconds), and the m1 q1 x q2 coupling
# block must fit in memory (an error is raised above 2^28 entries,
# 2 GB); lme4's sparse Cholesky has no such limit.
#
# GLMMs: Laplace approximation (nAGQ = 1) by penalised iteratively
# reweighted least squares (Sec. 5 of the vignette "Computational
# methods" and lme4's glmer).  As glmer does, stage 1 optimises theta
# with beta updated inside PIRLS (nAGQ = 0), stage 2 optimises
# (theta, beta) jointly with u updated by PIRLS.  The fixed-effect
# covariance is lme4's: the default inverts the finite-difference
# Hessian of the Laplace deviance over (theta, beta) (deriv12, step
# 1e-4); "RX" uses sigma^2 (RX' RX)^{-1} of the final PIRLS system.

# ---------------------------------------------------------------------
# formula handling
# ---------------------------------------------------------------------

#' Is an expression a random-effects bar?
#'
#' @param e A language object.
#' @return \code{TRUE} for a call to \code{|} or \code{||}.
#' @keywords internal
#' @noRd
.lmm_is_bar <- function(e) {
  is.call(e) && (identical(e[[1L]], as.name("|")) ||
                   identical(e[[1L]], as.name("||")))
}

#' Split a formula right-hand side into signed terms
#'
#' @param e A language object (the right-hand side).
#' @param sign 1 or -1, the sign carried into \code{e}.
#' @return A list of lists with \code{expr}, \code{sign}, \code{bar}.
#' @keywords internal
#' @noRd
.lmm_split <- function(e, sign = 1) {
  if (is.call(e) && identical(e[[1L]], as.name("+")) && length(e) == 3L)
    return(c(.lmm_split(e[[2L]], sign), .lmm_split(e[[3L]], sign)))
  if (is.call(e) && identical(e[[1L]], as.name("+")) && length(e) == 2L)
    return(.lmm_split(e[[2L]], sign))
  if (is.call(e) && identical(e[[1L]], as.name("-")) && length(e) == 3L)
    return(c(.lmm_split(e[[2L]], sign), .lmm_split(e[[3L]], -sign)))
  if (is.call(e) && identical(e[[1L]], as.name("-")) && length(e) == 2L)
    return(.lmm_split(e[[2L]], -sign))
  if (is.call(e) && identical(e[[1L]], as.name("(")) && .lmm_is_bar(e[[2L]]))
    return(list(list(expr = e[[2L]], sign = sign, bar = TRUE)))
  if (.lmm_is_bar(e)) return(list(list(expr = e, sign = sign, bar = TRUE)))
  list(list(expr = e, sign = sign, bar = FALSE))
}

#' Build a formula object from a call without evaluating text
#'
#' @param lhs Left-hand side expression or \code{NULL}.
#' @param rhs Right-hand side expression.
#' @param env Environment of the formula.
#' @return A formula.
#' @keywords internal
#' @noRd
.lmm_formula <- function(lhs, rhs, env) {
  cl <- if (is.null(lhs)) call("~", rhs) else call("~", lhs, rhs)
  structure(cl, class = "formula", .Environment = env)
}

#' Names of the variables in a grouping expression (a, a:b)
#'
#' @param e A language object.
#' @return Character vector of variable names.
#' @keywords internal
#' @noRd
.lmm_grp_vars <- function(e) {
  if (is.name(e)) return(as.character(e))
  if (is.call(e) && identical(e[[1L]], as.name(":")))
    return(c(.lmm_grp_vars(e[[2L]]), .lmm_grp_vars(e[[3L]])))
  if (is.call(e) && identical(e[[1L]], as.name("(")))
    return(.lmm_grp_vars(e[[2L]]))
  stop("a grouping factor must be a variable, an interaction a:b or a ",
       "nesting a/b; got '", paste(deparse(e), collapse = " "), "'",
       call. = FALSE)
}

#' Expand a grouping expression with nesting (a/b -> a, a:b)
#'
#' @param e A language object.
#' @return A list of character vectors (variables of each factor).
#' @keywords internal
#' @noRd
.lmm_grp_expand <- function(e) {
  if (is.call(e) && identical(e[[1L]], as.name("/"))) {
    left <- .lmm_grp_expand(e[[2L]])
    right <- .lmm_grp_expand(e[[3L]])
    last <- left[[length(left)]]
    return(c(left, lapply(right, function(r) c(last, r))))
  }
  if (is.call(e) && identical(e[[1L]], as.name("(")))
    return(.lmm_grp_expand(e[[2L]]))
  list(.lmm_grp_vars(e))
}

#' Expand one bar term into elementary random-effects terms
#'
#' \code{(x | a/b)} becomes terms on \code{a} and \code{a:b};
#' \code{(1 + x || g)} becomes \code{(1 | g)} and \code{(0 + x | g)}.
#'
#' @param bar A call to \code{|} or \code{||}.
#' @return A list of lists with \code{lhs} (expression) and
#'   \code{vars} (grouping variables).
#' @keywords internal
#' @noRd
.lmm_bar_expand <- function(bar) {
  lhs <- bar[[2L]]
  grps <- .lmm_grp_expand(bar[[3L]])
  lhss <- list(lhs)
  if (identical(bar[[1L]], as.name("||"))) {
    tt <- stats::terms(.lmm_formula(NULL, lhs, baseenv()))
    labs <- attr(tt, "term.labels")
    lhss <- list()
    if (attr(tt, "intercept") == 1L) lhss <- list(1)
    for (lb in labs) {
      lhss[[length(lhss) + 1L]] <- call("+", 0, .lmm_term_expr(lb, lhs))
    }
  }
  out <- list()
  for (g in grps) for (l in lhss) out[[length(out) + 1L]] <- list(lhs = l, vars = g)
  out
}

#' Recover a term expression of a one-sided formula by its label
#'
#' Walks the original expression for the sub-expression whose deparse
#' equals \code{label}; no text is parsed.
#'
#' @param label Term label from \code{terms()}.
#' @param lhs The expression the label came from.
#' @return The matching language object.
#' @keywords internal
#' @noRd
.lmm_term_expr <- function(label, lhs) {
  found <- NULL
  walk <- function(e) {
    if (!is.null(found)) return(invisible())
    if (identical(paste(deparse(e), collapse = ""), label)) {
      found <<- e
      return(invisible())
    }
    if (is.call(e)) for (i in seq_along(e)[-1L]) walk(e[[i]])
  }
  walk(lhs)
  if (is.null(found)) {
    # interactions a:b written as a*b expand to labels not present as
    # sub-expressions; rebuild them from their variable names
    parts <- strsplit(label, ":", fixed = TRUE)[[1L]]
    if (all(make.names(parts) == parts)) {
      found <- as.name(parts[1L])
      for (pp in parts[-1L]) found <- call(":", found, as.name(pp))
    } else {
      stop("cannot expand the term '", label, "' of an uncorrelated (||) ",
           "random-effects term; write the terms out with single bars",
           call. = FALSE)
    }
  }
  found
}

#' Turn nlme's random argument into bar expressions
#'
#' @param random A one-sided formula \code{~ x | g} (also
#'   \code{~ 1 | a/b}) or a named list of one-sided formulas
#'   \code{list(a = ~ 1, b = ~ x)} (b nested in a), as nlme::lme takes.
#' @return A list of bar calls.
#' @keywords internal
#' @noRd
.lmm_random_bars <- function(random) {
  if (is.null(random)) return(list())
  if (inherits(random, "formula")) {
    rhs <- random[[length(random)]]
    if (!.lmm_is_bar(rhs))
      stop("random must be a one-sided formula with a grouping factor, ",
           "like ~ x | g or ~ 1 | a/b", call. = FALSE)
    return(list(rhs))
  }
  if (is.list(random) && !is.null(names(random)) && all(nzchar(names(random)))) {
    out <- list()
    grp <- NULL
    for (nm in names(random)) {
      f <- random[[nm]]
      if (!inherits(f, "formula"))
        stop("each element of a random list must be a one-sided formula",
             call. = FALSE)
      grp <- if (is.null(grp)) as.name(nm) else call(":", grp, as.name(nm))
      out[[length(out) + 1L]] <- call("|", f[[length(f)]], grp)
    }
    return(out)
  }
  stop("random must be a one-sided formula (~ x | g) or a named list of ",
       "one-sided formulas", call. = FALSE)
}

# ---------------------------------------------------------------------
# model setup
# ---------------------------------------------------------------------

#' Build the model structure from a mixed-model formula
#'
#' @param formula Two-sided formula with bar terms.
#' @param data Data frame.
#' @param random Optional nlme-style random argument.
#' @param weights Optional prior weights (vector).
#' @param offset Optional offset (vector).
#' @return A list: \code{y}, \code{X}, \code{w}, \code{offset},
#'   \code{blocks}, \code{terms}, theta bookkeeping and the frame.
#' @keywords internal
#' @noRd
.lmm_setup <- function(formula, data, random = NULL, weights = NULL,
                       offset = NULL) {
  if (!inherits(formula, "formula") || length(formula) != 3L)
    stop("formula must be a two-sided formula", call. = FALSE)
  if (!is.data.frame(data)) data <- as.data.frame(data)
  env <- environment(formula)
  if (is.null(env)) env <- parent.frame()
  parts <- .lmm_split(formula[[3L]])
  bars <- lapply(Filter(function(p) p$bar, parts), function(p) p$expr)
  bars <- c(bars, .lmm_random_bars(random))
  if (length(bars) == 0L)
    stop("no random-effects terms: give terms like (1 | g) in the formula ",
         "or random = ~ 1 | g", call. = FALSE)
  fx <- Filter(function(p) !p$bar, parts)
  pos <- Filter(function(p) p$sign > 0, fx)
  neg <- Filter(function(p) p$sign < 0, fx)
  rhs <- if (length(pos)) pos[[1L]]$expr else 1
  for (p in pos[-1L]) rhs <- call("+", rhs, p$expr)
  for (p in neg) rhs <- call("-", rhs, p$expr)
  fixed <- .lmm_formula(formula[[2L]], rhs, env)

  rterms <- do.call(c, lapply(bars, .lmm_bar_expand))
  allv <- unique(c(all.vars(fixed),
                   unlist(lapply(rterms, function(t) all.vars(t$lhs))),
                   unlist(lapply(rterms, function(t) t$vars))))
  n0 <- nrow(data)
  if (!is.null(weights) && length(weights) != n0)
    stop("weights must have one value per row of data", call. = FALSE)
  if (!is.null(offset) && length(offset) != n0)
    stop("offset must have one value per row of data", call. = FALSE)
  allrhs <- as.name(allv[1L])
  for (v in allv[-1L]) allrhs <- call("+", allrhs, as.name(v))
  mfa <- stats::model.frame(.lmm_formula(NULL, allrhs, env), data,
                            na.action = stats::na.pass)
  cc <- stats::complete.cases(mfa)
  if (!is.null(weights)) cc <- cc & !is.na(weights)
  if (!is.null(offset)) cc <- cc & !is.na(offset)
  d <- mfa[cc, , drop = FALSE]
  for (v in names(d)) if (is.factor(d[[v]])) d[[v]] <- droplevels(d[[v]])
  rownames(d) <- NULL
  n <- nrow(d)
  if (n < 2L) stop("fewer than 2 complete observations", call. = FALSE)
  w <- if (is.null(weights)) rep(1, n) else as.numeric(weights[cc])
  if (any(w < 0)) stop("weights must be non-negative", call. = FALSE)
  off <- if (is.null(offset)) rep(0, n) else as.numeric(offset[cc])

  mf <- stats::model.frame(fixed, d, na.action = stats::na.fail)
  y <- stats::model.response(mf)
  X <- stats::model.matrix(attr(mf, "terms"), mf)
  mo <- stats::model.offset(mf)
  if (!is.null(mo)) off <- off + as.numeric(mo)
  assign <- attr(X, "assign")
  # rank-deficient fixed effects: drop columns, as lme4 does
  if (ncol(X) > 0L) {
    qx <- qr(X, tol = 1e-7)
    if (qx$rank < ncol(X)) {
      keep <- sort(qx$pivot[seq_len(qx$rank)])
      message(sprintf(paste("fixed-effect model matrix is rank deficient so",
                            "dropping %d column(s): %s"), ncol(X) - qx$rank,
                      paste(colnames(X)[-keep], collapse = ", ")))
      X <- X[, keep, drop = FALSE]
      assign <- assign[keep]
    }
  }
  attr(X, "assign") <- assign

  # elementary terms -> blocks by grouping factor
  terms <- list()
  blocks <- list()
  for (t in rterms) {
    gl <- paste(t$vars, collapse = ":")
    for (v in t$vars) if (!v %in% names(d))
      stop("grouping variable '", v, "' not found", call. = FALSE)
    Zt <- stats::model.matrix(.lmm_formula(NULL, t$lhs, env), d)
    attr(Zt, "assign") <- NULL
    attr(Zt, "contrasts") <- NULL
    if (ncol(Zt) == 0L) next
    if (is.null(blocks[[gl]])) {
      gf <- if (length(t$vars) == 1L) factor(d[[t$vars]]) else
        interaction(d[t$vars], drop = TRUE, sep = ":", lex.order = TRUE)
      gf <- droplevels(gf)
      blocks[[gl]] <- list(name = gl, vars = t$vars, g = as.integer(gf),
                           levels = levels(gf), m = nlevels(gf),
                           Z = matrix(0, n, 0), q = 0L, terms = integer(0))
    }
    b <- blocks[[gl]]
    k <- length(terms) + 1L
    cols <- b$q + seq_len(ncol(Zt))
    terms[[k]] <- list(group = gl, cnames = colnames(Zt), p = ncol(Zt),
                       cols = cols, block = gl)
    b$Z <- cbind(b$Z, Zt)
    b$q <- b$q + ncol(Zt)
    b$terms <- c(b$terms, k)
    blocks[[gl]] <- b
  }
  # theta bookkeeping (lme4 order: per term, lower triangle by column)
  nth <- vapply(terms, function(t) t$p * (t$p + 1L) / 2L, 1)
  ends <- cumsum(nth)
  lower <- numeric(0)
  start <- numeric(0)
  tnames <- character(0)
  for (k in seq_along(terms)) {
    t <- terms[[k]]
    terms[[k]]$theta_idx <- (ends[k] - nth[k]) + seq_len(nth[k])
    M <- matrix(0, t$p, t$p)
    diagm <- diag(t$p) == 1
    lw <- ifelse(diagm[lower.tri(M, diag = TRUE)], 0, -Inf)
    lower <- c(lower, lw)
    start <- c(start, as.numeric(diagm[lower.tri(M, diag = TRUE)]))
    ij <- which(lower.tri(M, diag = TRUE), arr.ind = TRUE)
    tnames <- c(tnames, paste0(t$group, ".", ifelse(
      ij[, 1] == ij[, 2], t$cnames[ij[, 1]],
      paste0(t$cnames[ij[, 1]], ".", t$cnames[ij[, 2]]))))
  }
  # the block that runs batched: most random effects
  sizes <- vapply(blocks, function(b) b$m * b$q, 1)
  i1 <- which.max(sizes)
  rest <- setdiff(seq_along(blocks), i1)
  off2 <- integer(0)
  q2 <- 0
  for (h in rest) {
    off2 <- c(off2, q2)
    q2 <- q2 + blocks[[h]]$m * blocks[[h]]$q
  }
  names(off2) <- names(blocks)[rest]
  if (q2 > 5000)
    warning(sprintf(paste("%d random effects outside the largest grouping",
                          "factor are handled by a dense Cholesky of that",
                          "size; each deviance evaluation costs O(%d^3)"),
                    q2, q2), call. = FALSE)
  if (blocks[[i1]]$m * blocks[[i1]]$q * q2 > 2^28)
    stop(sprintf(paste("the coupling between the largest grouping factor",
                       "(%d effects) and the others (%d) needs a dense",
                       "matrix of more than 2^28 entries"),
                 blocks[[i1]]$m * blocks[[i1]]$q, q2), call. = FALSE)
  st <- list(n = n, y = y, X = X, w = w, offset = off, blocks = blocks,
             terms = terms, i1 = i1, rest = rest, off2 = off2, q2 = q2,
             theta_lower = lower, theta_start = start, theta_names = tnames,
             fixed = fixed, frame = d, mf = mf, nobs_dropped = n0 - n)
  b1 <- blocks[[i1]]
  st$blocks[[i1]]$ZZ <- b1$Z[, rep(seq_len(b1$q), b1$q), drop = FALSE] *
    b1$Z[, rep(seq_len(b1$q), each = b1$q), drop = FALSE]
  st$pairs <- .lmm_pairs(st)
  st$sp <- .lmm_sparse_pattern(st)
  st
}

#' Sparsity pattern of the coupling block for a sparse Schur update
#'
#' When each level of the largest grouping factor meets few levels of
#' the others (crossed designs), \code{crossprod(K)} in the Schur
#' complement is accumulated from the co-occurring non-zeros of each
#' level instead of a dense \eqn{m_1 q_1 q_2^2} product.
#'
#' @param st Model structure.
#' @return \code{NULL} (use the dense product) or a list of index
#'   vectors (\code{za}, \code{zb} into the coupling matrix,
#'   \code{gi} into the per-level weights, \code{tg} target cells).
#' @keywords internal
#' @noRd
.lmm_sparse_pattern <- function(st) {
  if (st$q2 == 0) return(NULL)
  b1 <- st$blocks[[st$i1]]
  m1 <- b1$m
  q1 <- b1$q
  q2 <- st$q2
  rr <- integer(0)
  cc <- integer(0)
  for (pr in st$pairs) if (pr$to1) {
    rr <- c(rr, pr$rr)
    cc <- c(cc, pr$cc)
  }
  g <- (rr - 1L) %% m1 + 1L
  o <- order(g)
  rr <- rr[o]
  cc <- cc[o]
  g <- g[o]
  cnt <- tabulate(g, m1)
  nterm <- sum(as.numeric(cnt)^2)
  if (nterm * 20 > as.numeric(m1) * q1 * q2^2 || nterm > 5e7) return(NULL)
  starts <- cumsum(c(1L, cnt))[seq_len(m1)]
  a <- rep(seq_along(rr), times = cnt[g])
  b <- starts[g[a]] + sequence(cnt[g]) - 1L
  ia <- (rr[a] - 1L) %/% m1 + 1L
  ib <- (rr[b] - 1L) %/% m1 + 1L
  za <- rr[a] + m1 * q1 * (cc[a] - 1)
  zb <- rr[b] + m1 * q1 * (cc[b] - 1)
  gi <- g[a] + m1 * (ia - 1) + m1 * q1 * (ib - 1)
  tg <- cc[a] + q2 * (cc[b] - 1)
  ord <- order(tg)
  tg <- tg[ord]
  utg <- unique(tg)
  list(za = as.integer(za[ord]), zb = as.integer(zb[ord]),
       gi = as.integer(gi[ord]),
       ends = c(which(diff(tg) != 0), length(tg)), cells = utg)
}

#' Precompute the level-pair indices of crossed blocks
#'
#' @param st Model structure from \code{.lmm_setup}.
#' @return A list of pair records (keys and target positions).
#' @keywords internal
#' @noRd
.lmm_pairs <- function(st) {
  B <- st$blocks
  out <- list()
  if (st$q2 == 0) return(out)
  b1 <- B[[st$i1]]
  idx <- c(st$i1, st$rest)
  for (a in seq_along(idx)) for (bb in seq_along(idx)) {
    if (bb < a) next
    h <- idx[a]
    k <- idx[bb]
    if (h == st$i1 && k == st$i1) next
    Bh <- B[[h]]
    Bk <- B[[k]]
    if (h == k) {
      key <- Bh$g
    } else {
      key <- Bh$g + Bh$m * (Bk$g - 1)
    }
    uk <- sort(unique(key))
    gi <- match(key, uk)
    lh <- if (h == k) uk else (uk - 1) %% Bh$m + 1
    lk <- if (h == k) uk else (uk - 1) %/% Bh$m + 1
    ii <- rep(seq_len(Bh$q), Bk$q)
    jj <- rep(seq_len(Bk$q), each = Bh$q)
    K <- length(uk)
    # target rows/cols for the K x (qh qk) rowsum result
    LH <- rep(lh, times = length(ii))
    LK <- rep(lk, times = length(ii))
    II <- rep(ii, each = K)
    JJ <- rep(jj, each = K)
    if (h == st$i1) {
      rr <- LH + b1$m * (II - 1)
    } else {
      rr <- st$off2[[Bh$name]] + (LH - 1) * Bh$q + II
    }
    cc <- st$off2[[Bk$name]] + (LK - 1) * Bk$q + JJ
    out[[length(out) + 1L]] <- list(h = h, k = k, gi = gi, ii = ii, jj = jj,
                                    rr = rr, cc = cc, to1 = (h == st$i1),
                                    same = (h == k))
  }
  out
}

#' Weighted crossproducts of Z and V, without forming Z
#'
#' @param st Model structure.
#' @param w Weights (length n).
#' @param V Matrix of right-hand sides (n x r).
#' @return A list with \code{C1} (m1, q1, q1), \code{ZV1} (m1, q1, r),
#'   \code{Z12} (m1 q1 x q2), \code{ZZ2} (q2 x q2), \code{ZV2}
#'   (q2 x r) and \code{VV} (r x r).
#' @keywords internal
#' @noRd
.lmm_cp <- function(st, w, V) {
  B <- st$blocks
  b1 <- B[[st$i1]]
  m1 <- b1$m
  q1 <- b1$q
  r <- ncol(V)
  wZ1 <- b1$Z * w
  C1 <- rowsum(b1$ZZ * w, b1$g, reorder = TRUE)
  ZV1 <- rowsum(wZ1[, rep(seq_len(q1), r), drop = FALSE] *
                  V[, rep(seq_len(r), each = q1), drop = FALSE], b1$g,
                reorder = TRUE)
  out <- list(C1 = array(C1, c(m1, q1, q1)), ZV1 = array(ZV1, c(m1, q1, r)),
              VV = crossprod(V * w, V))
  q2 <- st$q2
  if (q2 > 0) {
    ZZ2 <- matrix(0, q2, q2)
    Z12 <- matrix(0, m1 * q1, q2)
    ZV2 <- matrix(0, q2, r)
    for (h in st$rest) {
      Bh <- B[[h]]
      tmp <- rowsum((Bh$Z * w)[, rep(seq_len(Bh$q), r), drop = FALSE] *
                      V[, rep(seq_len(r), each = Bh$q), drop = FALSE], Bh$g,
                    reorder = TRUE)
      tmp <- aperm(array(tmp, c(Bh$m, Bh$q, r)), c(2L, 1L, 3L))
      ZV2[st$off2[[Bh$name]] + seq_len(Bh$m * Bh$q), ] <- tmp
    }
    for (pr in st$pairs) {
      Bh <- B[[pr$h]]
      Bk <- B[[pr$k]]
      v <- rowsum((Bh$Z * w)[, pr$ii, drop = FALSE] * Bk$Z[, pr$jj, drop = FALSE],
                  pr$gi, reorder = TRUE)
      v <- as.vector(v)
      if (pr$to1) {
        Z12[cbind(pr$rr, pr$cc)] <- v
      } else {
        ZZ2[cbind(pr$rr, pr$cc)] <- v
        if (!pr$same) ZZ2[cbind(pr$cc, pr$rr)] <- v
      }
    }
    out$ZZ2 <- ZZ2
    out$Z12 <- Z12
    out$ZV2 <- ZV2
  }
  out
}

#' Relative covariance factors T for every block
#'
#' @param theta Covariance parameters (lme4 order).
#' @param st Model structure.
#' @return A list of lower-triangular matrices, one per block.
#' @keywords internal
#' @noRd
.lmm_tmats <- function(theta, st) {
  lapply(st$blocks, function(b) {
    Tm <- matrix(0, b$q, b$q)
    for (k in b$terms) {
      t <- st$terms[[k]]
      Tk <- matrix(0, t$p, t$p)
      Tk[lower.tri(Tk, diag = TRUE)] <- theta[t$theta_idx]
      Tm[t$cols, t$cols] <- Tk
    }
    Tm
  })
}

# ---- batched algebra over the levels of block 1 ----------------------

#' Batched T' A: out[g, l, c] = sum_i T[i, l] A[g, i, c]
#'
#' @param Tm q x q matrix.
#' @param A Array (m, q, r).
#' @return Array (m, q, r).
#' @keywords internal
#' @noRd
.lmm_btl <- function(Tm, A) {
  d <- dim(A)
  if (d[2L] == 1L) return(A * Tm[1L, 1L])
  a <- aperm(A, c(1L, 3L, 2L))
  res <- matrix(a, d[1L] * d[3L], d[2L]) %*% Tm
  aperm(array(res, c(d[1L], d[3L], d[2L])), c(1L, 3L, 2L))
}

#' Batched T' C T + I
#'
#' @param Tm q x q matrix.
#' @param C Array (m, q, q).
#' @return Array (m, q, q).
#' @keywords internal
#' @noRd
.lmm_btct <- function(Tm, C) {
  d <- dim(C)
  if (d[2L] == 1L) return(C * Tm[1L, 1L]^2 + 1)
  Bm <- .lmm_btl(Tm, C)
  out <- array(matrix(Bm, d[1L] * d[2L], d[2L]) %*% Tm, d)
  for (j in seq_len(d[2L])) out[, j, j] <- out[, j, j] + 1
  out
}

#' Batched Cholesky factor (lower) of an array of q x q blocks
#'
#' @param A Array (m, q, q) of positive definite blocks.
#' @return Array (m, q, q), lower triangular per level.
#' @keywords internal
#' @noRd
.lmm_bchol <- function(A) {
  d <- dim(A)
  q <- d[2L]
  L <- array(0, d)
  for (j in seq_len(q)) {
    s <- A[, j, j]
    if (j > 1L) for (k in seq_len(j - 1L)) s <- s - L[, j, k]^2
    if (any(!(s > 0))) stop("batched Cholesky: block not positive definite",
                            call. = FALSE)
    dj <- sqrt(s)
    L[, j, j] <- dj
    if (j < q) for (i in (j + 1L):q) {
      s <- A[, i, j]
      if (j > 1L) for (k in seq_len(j - 1L)) s <- s - L[, i, k] * L[, j, k]
      L[, i, j] <- s / dj
    }
  }
  L
}

#' Batched forward solve L x = b
#'
#' @param L Array (m, q, q), lower triangular per level.
#' @param B Array (m, q, r).
#' @return Array (m, q, r).
#' @keywords internal
#' @noRd
.lmm_bfs <- function(L, B) {
  d <- dim(B)
  q <- d[2L]
  if (q == 1L) return(B / L[, 1L, 1L])
  Xs <- array(0, d)
  for (i in seq_len(q)) {
    s <- B[, i, ]
    if (i > 1L) for (k in seq_len(i - 1L)) s <- s - L[, i, k] * Xs[, k, ]
    Xs[, i, ] <- s / L[, i, i]
  }
  Xs
}

#' Batched back solve L' x = b
#'
#' @param L Array (m, q, q), lower triangular per level.
#' @param B Array (m, q, r).
#' @return Array (m, q, r).
#' @keywords internal
#' @noRd
.lmm_bbs <- function(L, B) {
  d <- dim(B)
  q <- d[2L]
  if (q == 1L) return(B / L[, 1L, 1L])
  Xs <- array(0, d)
  for (i in rev(seq_len(q))) {
    s <- B[, i, ]
    if (i < q) for (k in (i + 1L):q) s <- s - L[, k, i] * Xs[, k, ]
    Xs[, i, ] <- s / L[, i, i]
  }
  Xs
}

#' Right-multiply by Lambda of the non-batched blocks
#'
#' @param M Matrix with q2 columns.
#' @param st Model structure.
#' @param Ts List of T matrices.
#' @return \code{M \%*\% Lambda2}.
#' @keywords internal
#' @noRd
.lmm_rlam <- function(M, st, Ts) {
  nr <- nrow(M)
  for (h in st$rest) {
    Bh <- st$blocks[[h]]
    idx <- st$off2[[Bh$name]] + seq_len(Bh$m * Bh$q)
    Tm <- Ts[[h]]
    if (Bh$q == 1L) {
      M[, idx] <- M[, idx] * Tm[1L, 1L]
    } else {
      a <- aperm(array(M[, idx], c(nr, Bh$q, Bh$m)), c(1L, 3L, 2L))
      res <- matrix(a, nr * Bh$m, Bh$q) %*% Tm
      M[, idx] <- aperm(array(res, c(nr, Bh$m, Bh$q)), c(1L, 3L, 2L))
    }
  }
  M
}

#' Penalized least squares for given theta and crossproducts
#'
#' @param cp Crossproducts from \code{.lmm_cp}; the last column of V is
#'   the response, the first \code{p} the fixed-effects columns.
#' @param st Model structure.
#' @param Ts T matrices.
#' @param p Number of fixed-effect columns in V (0 for u-only).
#' @return A list with \code{beta}, \code{u1} (m1 x q1), \code{u2},
#'   \code{ldL2}, \code{RX} and \code{ldRX2}.
#' @keywords internal
#' @noRd
.lmm_pls <- function(cp, st, Ts, p) {
  b1 <- st$blocks[[st$i1]]
  m1 <- b1$m
  q1 <- b1$q
  q2 <- st$q2
  r <- p + 1L
  T1 <- Ts[[st$i1]]
  L11 <- .lmm_bchol(.lmm_btct(T1, cp$C1))
  ld <- 0
  for (j in seq_len(q1)) ld <- ld + 2 * sum(log(L11[, j, j]))
  c1 <- matrix(.lmm_bfs(L11, .lmm_btl(T1, cp$ZV1)), m1 * q1, r)
  if (q2 > 0) {
    sp <- st$sp
    if (is.null(sp)) {
      A12 <- .lmm_rlam(matrix(.lmm_btl(T1, array(cp$Z12, c(m1, q1, q2))),
                              m1 * q1, q2), st, Ts)
      Km <- matrix(.lmm_bfs(L11, array(A12, c(m1, q1, q2))), m1 * q1, q2)
      ZZ2 <- cp$ZZ2
      KK <- crossprod(Km)
      KtC1 <- crossprod(Km, c1)
    } else {
      # Km = L11^-1 T1' Z12 Lambda2 is never formed:
      # Km'Km = Lambda2' (Z12' G Z12) Lambda2, G_g = T1 (L11 L11')^-1 T1',
      # accumulated over the co-occurring non-zeros of each level
      H <- .lmm_bfs(L11, array(rep(t(T1), each = m1), c(m1, q1, q1)))
      G <- array(0, c(m1, q1, q1))
      for (i in seq_len(q1)) for (j in seq_len(q1)) {
        G[, i, j] <- if (q1 == 1L) H[, 1L, 1L]^2 else
          rowSums(H[, , i, drop = FALSE] * H[, , j, drop = FALSE])
      }
      v <- G[sp$gi] * cp$Z12[sp$za] * cp$Z12[sp$zb]
      M <- matrix(0, q2, q2)
      # run sums over the target-sorted terms (cumsum accumulates in
      # long double)
      cs <- cumsum(v)[sp$ends]
      M[sp$cells] <- cs - c(0, cs[-length(cs)])
      ZZ2 <- cp$ZZ2 - M
      KK <- 0
      # Km' c1 = Lambda2' Z12' (T1 L11^-T c1)
      y1 <- .lmm_btl(t(T1), .lmm_bbs(L11, array(c1, c(m1, q1, r))))
      KtC1 <- t(.lmm_rlam(crossprod(matrix(y1, m1 * q1, r), cp$Z12), st, Ts))
    }
    A22 <- t(.lmm_rlam(t(.lmm_rlam(ZZ2, st, Ts)), st, Ts))
    diag(A22) <- diag(A22) + 1
    R22 <- chol(A22 - KK)
    ld <- ld + 2 * sum(log(diag(R22)))
    RV2 <- t(.lmm_rlam(t(cp$ZV2), st, Ts)) - KtC1
    c2 <- backsolve(R22, RV2, transpose = TRUE)
    C <- rbind(c1, c2)
  } else {
    C <- c1
  }
  beta <- numeric(0)
  RX <- matrix(0, 0, 0)
  ldRX2 <- 0
  cu <- C[, r]
  if (p > 0) {
    MX <- cp$VV[seq_len(p), seq_len(p), drop = FALSE] -
      crossprod(C[, seq_len(p), drop = FALSE])
    RX <- chol(MX)
    ldRX2 <- 2 * sum(log(diag(RX)))
    rhs <- cp$VV[seq_len(p), r] - crossprod(C[, seq_len(p), drop = FALSE], cu)
    cb <- backsolve(RX, rhs, transpose = TRUE)
    beta <- as.numeric(backsolve(RX, cb))
    cu <- cu - C[, seq_len(p), drop = FALSE] %*% beta
  }
  u2 <- numeric(0)
  cu1 <- cu[seq_len(m1 * q1)]
  if (q2 > 0) {
    u2 <- as.numeric(backsolve(R22, cu[m1 * q1 + seq_len(q2)]))
    if (is.null(sp)) {
      cu1 <- cu1 - Km %*% u2
    } else {
      lu2 <- as.numeric(.lmm_rlam(matrix(u2, 1L), st, Ts))
      cu1 <- cu1 - as.numeric(.lmm_bfs(L11, .lmm_btl(T1, array(
        cp$Z12 %*% lu2, c(m1, q1, 1L)))))
    }
  }
  u1 <- matrix(.lmm_bbs(L11, array(cu1, c(m1, q1, 1L))), m1, q1)
  list(beta = beta, u1 = u1, u2 = u2, ldL2 = ld, RX = RX, ldRX2 = ldRX2)
}

#' Random effects b = Lambda u, per block
#'
#' @param u1 Spherical effects of block 1 (m1 x q1).
#' @param u2 Spherical effects of the other blocks (vector).
#' @param st Model structure.
#' @param Ts T matrices.
#' @return A list of (levels x q) matrices, one per block.
#' @keywords internal
#' @noRd
.lmm_b <- function(u1, u2, st, Ts) {
  out <- vector("list", length(st$blocks))
  out[[st$i1]] <- u1 %*% t(Ts[[st$i1]])
  for (h in st$rest) {
    Bh <- st$blocks[[h]]
    U <- matrix(u2[st$off2[[Bh$name]] + seq_len(Bh$m * Bh$q)], Bh$m, Bh$q,
                byrow = TRUE)
    out[[h]] <- U %*% t(Ts[[h]])
  }
  out
}

#' Z b without forming Z
#'
#' @param bl List of b matrices from \code{.lmm_b}.
#' @param st Model structure.
#' @return Numeric vector of length n.
#' @keywords internal
#' @noRd
.lmm_zb <- function(bl, st) {
  s <- numeric(st$n)
  for (h in seq_along(st$blocks)) {
    Bh <- st$blocks[[h]]
    s <- s + rowSums(Bh$Z * bl[[h]][Bh$g, , drop = FALSE])
  }
  s
}

#' Finite-difference gradient and Hessian (lme4's deriv12 stencil)
#'
#' Central differences with step \code{delta}; at a lower bound the
#' backward step is shortened to the bound, as in lme4.
#'
#' @param fun Objective.
#' @param x Point.
#' @param delta Step.
#' @param lower Lower bounds (\code{NA} for none).
#' @param fx Value at \code{x}.
#' @return A list with \code{gradient} and \code{Hessian}.
#' @keywords internal
#' @noRd
.lmm_deriv12 <- function(fun, x, delta = 1e-4, lower = rep(NA, length(x)),
                         fx = fun(x)) {
  nx <- length(x)
  H <- matrix(NA_real_, nx, nx)
  g <- numeric(nx)
  xadd <- x + delta
  xsub <- x - delta
  ld <- rep(delta, nx)
  act <- !is.na(lower) & xsub < lower
  ld[act] <- x[act] - lower[act]
  xsub[act] <- lower[act]
  at <- function(i, vi, j = NULL, vj = NULL) {
    z <- x
    z[i] <- vi[i]
    if (!is.null(j)) z[j] <- vj[j]
    z
  }
  fa <- fs <- numeric(nx)
  for (j in seq_len(nx)) {
    fa[j] <- fun(at(j, xadd))
    fs[j] <- fun(at(j, xsub))
    H[j, j] <- fa[j] / delta^2 - 2 * fx / (delta * ld[j]) + fs[j] / ld[j]^2
    g[j] <- (fa[j] - fs[j]) / (delta + ld[j])
    for (i in seq_len(j - 1L)) {
      H[i, j] <- H[j, i] <-
        fun(at(i, xadd, j, xadd)) / (delta + delta)^2 -
        fun(at(i, xadd, j, xsub)) / (delta + ld[j])^2 -
        fun(at(i, xsub, j, xadd)) / (ld[i] + delta)^2 +
        fun(at(i, xsub, j, xsub)) / (ld[i] + ld[j])^2
    }
  }
  list(gradient = g, Hessian = H)
}

#' Variance components from theta
#'
#' @param theta Covariance parameters.
#' @param st Model structure.
#' @param sigma Residual SD (1 for GLMMs).
#' @return A list with \code{varcorr} (per term: group, sd,
#'   correlation, covariance) and a lme4-style data frame \code{table}.
#' @keywords internal
#' @noRd
.lmm_varcorr <- function(theta, st, sigma) {
  vc <- list()
  rows <- list()
  grp_count <- list()
  for (t in st$terms) {
    Tk <- matrix(0, t$p, t$p)
    Tk[lower.tri(Tk, diag = TRUE)] <- theta[t$theta_idx]
    S <- sigma^2 * tcrossprod(Tk)
    dimnames(S) <- list(t$cnames, t$cnames)
    sds <- sqrt(diag(S))
    Cr <- S / outer(sds, sds)
    Cr[!is.finite(Cr)] <- NaN
    diag(Cr) <- 1
    nm <- t$group
    cnt <- grp_count[[nm]]
    grp_count[[nm]] <- if (is.null(cnt)) 1L else cnt + 1L
    label <- if (is.null(cnt)) nm else paste0(nm, ".", cnt)
    vc[[label]] <- list(group = nm, sd = setNames(sds, t$cnames),
                        corr = Cr, cov = S)
    for (i in seq_len(t$p)) rows[[length(rows) + 1L]] <-
      data.frame(grp = label, var1 = t$cnames[i], var2 = NA_character_,
                 vcov = S[i, i], sdcor = sds[i], stringsAsFactors = FALSE)
    if (t$p > 1L) for (j in 1:(t$p - 1L)) for (i in (j + 1L):t$p)
      rows[[length(rows) + 1L]] <-
        data.frame(grp = label, var1 = t$cnames[j], var2 = t$cnames[i],
                   vcov = S[i, j], sdcor = Cr[i, j], stringsAsFactors = FALSE)
  }
  list(varcorr = vc, table = do.call(rbind, rows))
}

#' Conditional modes as one data frame per grouping factor
#'
#' @param bl List of b matrices.
#' @param st Model structure.
#' @return Named list of data frames (levels x effects).
#' @keywords internal
#' @noRd
.lmm_ranef <- function(bl, st) {
  out <- list()
  for (h in seq_along(st$blocks)) {
    Bh <- st$blocks[[h]]
    df <- as.data.frame(bl[[h]])
    names(df) <- colnames(Bh$Z)
    rownames(df) <- Bh$levels
    out[[Bh$name]] <- df
  }
  out
}

#' nlme's containment ("inner-outer") denominator degrees of freedom
#'
#' Reproduces nlme::lme's rule (Pinheiro and Bates 2000, Sec. 2.4.2):
#' a fixed-effect column is assigned to the innermost level within
#' whose groups it varies.  Defined for nested grouping factors only;
#' \code{NULL} otherwise.
#'
#' @param st Model structure.
#' @return Named numeric vector of degrees of freedom, or \code{NULL}.
#' @keywords internal
#' @noRd
.lmm_nlme_df <- function(st) {
  B <- st$blocks
  X <- st$X
  if (ncol(X) == 0L) return(NULL)
  ms <- vapply(B, function(b) b$m, 1)
  ord <- order(ms)
  # nesting check: each inner level lies in exactly one outer level
  for (a in seq_along(ord)[-1L]) {
    outer <- B[[ord[a - 1L]]]$g
    inner <- B[[ord[a]]]$g
    if (any(ms[ord[a]] == ms[ord[a - 1L]]) ||
        nrow(unique(cbind(outer, inner))) != ms[ord[a]]) return(NULL)
  }
  N <- st$n
  ng <- ms[ord]
  dfX <- c(ng, N) - c(0, ng)
  Q <- length(ord)
  const <- apply(X, 2L, function(x) {
    all(abs(if (x[1L] == 0) x else x / x[1L] - 1) < sqrt(.Machine$double.eps))
  })
  strat <- rep(NA_integer_, ncol(X))
  for (j in which(!const)) {
    s <- 1L
    for (lv in seq_len(Q)) {
      g <- B[[ord[lv]]]$g
      first <- X[match(g, g), j]
      if (any(X[, j] != first)) s <- lv + 1L
    }
    strat[j] <- s
  }
  if (any(!const)) {
    tb <- table(strat[!const])
    ix <- as.integer(names(tb))
    dfX[ix] <- dfX[ix] - as.numeric(tb)
    if (!all(!const)) dfX[1L] <- dfX[1L] - 1 else dfX[-1L] <- dfX[-1L] + 1
  }
  out <- numeric(ncol(X))
  out[!const] <- dfX[strat[!const]]
  out[const] <- max(dfX)
  setNames(out, colnames(X))
}

# ---------------------------------------------------------------------
# linear mixed model
# ---------------------------------------------------------------------

#' Linear mixed-effects model (native, lme4/nlme compatible)
#'
#' Fits a linear mixed model by REML or maximum likelihood with the
#' profiled-deviance method of lme4: the fixed effects and the residual
#' variance are profiled out, and the deviance is minimised over the
#' relative covariance parameters \code{theta} only.  Formulas use
#' lme4's syntax -- \code{(1 | g)}, \code{(x | g)}, uncorrelated
#' \code{(x || g)}, nested \code{(1 | a/b)}, crossed
#' \code{(1 | a) + (1 | b)} -- or nlme's, a fixed formula with
#' \code{random = ~ x | g}.
#'
#' The computation never forms an \eqn{n \times n} (or
#' \eqn{n \times q}) matrix.  The grouping factor with the most random
#' effects is handled level by level in vectorised form from per-level
#' crossproducts accumulated in one pass over the data, so a single
#' grouping factor costs \eqn{O(n q^2 + m q^3)} per deviance evaluation.
#' Other grouping factors enter through a Schur complement and a dense
#' Cholesky factor of their total number of random effects \eqn{q_2}
#' (the update is accumulated sparsely for crossed designs); that is
#' fast up to a few thousand (about 0.5 s per evaluation at
#' \eqn{q_2 = 1000}), warns above 5000, and needs the dense coupling
#' block (\eqn{m_1 q_1 \times q_2}) to fit in memory.
#'
#' @param formula A two-sided formula: fixed effects plus random terms
#'   \code{(terms | group)} or \code{(terms || group)}.  With nlme's
#'   interface this is the fixed-effects formula.
#' @param data A data frame.
#' @param REML Logical; \code{TRUE} (default) for REML, \code{FALSE}
#'   for maximum likelihood.
#' @param random Optional nlme-style random effects: a one-sided
#'   formula \code{~ x | g} (also \code{~ 1 | a/b}) or a named list of
#'   one-sided formulas \code{list(a = ~ 1, b = ~ 1)} (b nested in a).
#' @param weights Optional prior weights (residual variance
#'   \eqn{\sigma^2 / w_i}).
#' @param offset Optional offset added to the linear predictor.
#' @param start Optional starting value for \code{theta}.
#' @param control Optional list passed to \code{stats::nlminb}'s
#'   \code{control}.
#' @param ... Ignored (for compatibility with calls written for
#'   \code{lmer}).
#' @return An object of class \code{morie_lmm}: \code{coefficients},
#'   \code{vcov}, \code{fixef_table} (estimate, SE, t, and nlme's
#'   containment df and p-value when the grouping factors are nested),
#'   \code{sigma}, \code{theta}, \code{varcorr} (per term: SDs,
#'   correlations, covariance), \code{varcorr_table} (as
#'   \code{as.data.frame(VarCorr())}), \code{ranef} (conditional modes
#'   per grouping factor), \code{fitted}, \code{residuals},
#'   \code{logLik}, \code{df}, \code{AIC}, \code{BIC},
#'   \code{deviance} (ML deviance at the estimates, or \code{NA} for
#'   REML), \code{REMLcrit}, \code{ngroups}, \code{nobs},
#'   \code{convergence} (nlminb code and message, iterations,
#'   evaluations, largest free-parameter gradient, singular flag) and
#'   \code{devfun}, the profiled deviance as a function of theta (this
#'   object's \code{theta} order), for checking against other fitters.
#' @references Bates, D., Maechler, M., Bolker, B. and Walker, S.
#'   (2015). Fitting linear mixed-effects models using lme4. Journal of
#'   Statistical Software, 67(1), 1-48.
#'
#'   Pinheiro, J. C. and Bates, D. M. (2000). Mixed-Effects Models in S
#'   and S-PLUS. Springer.
#' @examples
#' set.seed(1)
#' d <- data.frame(g = factor(rep(1:20, each = 8)), x = rep(0:7, 20))
#' d$y <- 1 + 0.5 * d$x + rnorm(20)[d$g] + rnorm(20, 0, 0.2)[d$g] * d$x +
#'   rnorm(160)
#' fit <- morie_lmm(y ~ x + (x | g), d)
#' fit
#' fit2 <- morie_lmm(y ~ x, d, random = ~ x | g)
#' @export
morie_lmm <- function(formula, data, REML = TRUE, random = NULL,
                      weights = NULL, offset = NULL, start = NULL,
                      control = list(), ...) {
  cl <- match.call()
  st <- .lmm_setup(formula, data, random, weights, offset)
  y <- st$y
  if (!is.numeric(y) || !is.null(dim(y)))
    stop("the response must be a numeric vector", call. = FALSE)
  y <- as.numeric(y)
  X <- st$X
  p <- ncol(X)
  n <- st$n
  w <- st$w
  yo <- y - st$offset
  cp <- .lmm_cp(st, w, cbind(X, yo))
  ldW <- sum(log(w[w > 0]))
  nmp <- if (REML) n - p else n
  eval_theta <- function(theta) {
    Ts <- .lmm_tmats(theta, st)
    s <- .lmm_pls(cp, st, Ts, p)
    bl <- .lmm_b(s$u1, s$u2, st, Ts)
    res <- yo - .lmm_zb(bl, st)
    if (p > 0) res <- res - as.numeric(X %*% s$beta)
    r2 <- sum(w * res^2) + sum(s$u1^2) + sum(s$u2^2)
    dev <- s$ldL2 - ldW + nmp * (1 + log(2 * pi * r2 / nmp))
    if (REML) dev <- dev + s$ldRX2
    list(dev = dev, sol = s, bl = bl, r2 = r2, res = res, Ts = Ts)
  }
  devfun <- function(theta) {
    v <- tryCatch(eval_theta(theta)$dev, error = function(e) Inf)
    if (!is.finite(v)) Inf else v
  }
  th0 <- if (is.null(start)) st$theta_start else as.numeric(start)
  if (length(th0) != length(st$theta_start))
    stop(sprintf("start must have length %d", length(st$theta_start)),
         call. = FALSE)
  ctrl <- utils::modifyList(list(eval.max = 2000, iter.max = 1000,
                                 rel.tol = 1e-12), control)
  opt <- .lmm_optimize(th0, devfun, st$theta_lower, ctrl)
  theta <- opt$par
  fin <- eval_theta(theta)
  s <- fin$sol
  sigma <- sqrt(fin$r2 / nmp)
  beta <- setNames(s$beta, colnames(X))
  V <- if (p > 0) sigma^2 * chol2inv(s$RX) else matrix(0, 0, 0)
  dimnames(V) <- list(colnames(X), colnames(X))
  se <- sqrt(diag(V))
  tab <- data.frame(Estimate = beta, Std.Error = se, t.value = beta / se,
                    row.names = colnames(X))
  names(tab)[3L] <- "t value"
  dfn <- .lmm_nlme_df(st)
  if (!is.null(dfn)) {
    tab$df <- dfn
    tab[["Pr(>|t|)"]] <- 2 * stats::pt(abs(beta / se), dfn, lower.tail = FALSE)
  }
  vc <- .lmm_varcorr(theta, st, sigma)
  fitted <- y - fin$res
  ntheta <- length(theta)
  dfm <- p + ntheta + 1
  ll <- -fin$dev / 2
  grad <- .lmm_free_grad(devfun, theta, st$theta_lower)
  ngroups <- vapply(st$blocks, function(b) b$m, 1)
  out <- list(call = cl, formula = formula, fixed = st$fixed, REML = REML,
              coefficients = beta, vcov = V, fixef_table = tab,
              sigma = sigma, theta = setNames(theta, st$theta_names),
              varcorr = vc$varcorr, varcorr_table = vc$table,
              ranef = .lmm_ranef(fin$bl, st), u = list(s$u1, s$u2),
              fitted = fitted, residuals = y - fitted,
              logLik = ll, df = dfm, AIC = -2 * ll + 2 * dfm,
              BIC = -2 * ll + log(n) * dfm,
              deviance = if (REML) NA_real_ else fin$dev,
              REMLcrit = if (REML) fin$dev else NA_real_,
              ngroups = ngroups, nobs = n, nobs_dropped = st$nobs_dropped,
              convergence = list(code = opt$convergence, message = opt$message,
                                 iterations = opt$iterations,
                                 evaluations = opt$evaluations,
                                 objective = opt$objective, max_grad = grad,
                                 singular = any(theta[st$theta_lower == 0] < 1e-4)),
              devfun = devfun)
  class(out) <- "morie_lmm"
  out
}

#' Bounded minimisation with a confirming restart
#'
#' Runs \code{stats::nlminb}, then (if \code{polish}) restarts it at
#' the solution.  The fit counts as converged (code 0) when nlminb
#' reports convergence or when the restart cannot lower the objective by
#' more than \code{1e-8 * (|f| + 1)}, i.e. the stopping point is
#' confirmed as a minimum.
#'
#' @param par Starting values.
#' @param f Objective.
#' @param lower Lower bounds.
#' @param ctrl nlminb control list.
#' @param polish Logical; restart at the solution.
#' @return A list with \code{par}, \code{objective}, \code{convergence},
#'   \code{message}, \code{iterations}, \code{evaluations}.
#' @keywords internal
#' @noRd
.lmm_optimize <- function(par, f, lower, ctrl, polish = TRUE) {
  o1 <- stats::nlminb(par, f, lower = lower, control = ctrl)
  out <- list(par = o1$par, objective = o1$objective,
              convergence = o1$convergence, message = o1$message,
              iterations = o1$iterations,
              evaluations = unname(o1$evaluations[1L]))
  if (polish && is.finite(o1$objective)) {
    o2 <- stats::nlminb(o1$par, f, lower = lower, control = ctrl)
    gain <- o1$objective - o2$objective
    if (gain > 0) {
      out$par <- o2$par
      out$objective <- o2$objective
    }
    out$iterations <- out$iterations + o2$iterations
    out$evaluations <- out$evaluations + unname(o2$evaluations[1L])
    if (out$convergence != 0 && gain < 1e-8 * (abs(o2$objective) + 1)) {
      out$convergence <- 0L
      out$message <- paste0(o1$message, "; minimum confirmed by restart")
    }
  }
  out
}

#' Largest absolute finite-difference gradient over free parameters
#'
#' @param f Objective.
#' @param x Point.
#' @param lower Lower bounds.
#' @return The largest absolute central-difference gradient of the
#'   parameters not at their bound.
#' @keywords internal
#' @noRd
.lmm_free_grad <- function(f, x, lower) {
  h <- 1e-5
  g <- vapply(seq_along(x), function(i) {
    if (is.finite(lower[i]) && x[i] - lower[i] < 2 * h) return(NA_real_)
    e <- replace(numeric(length(x)), i, h)
    (f(x + e) - f(x - e)) / (2 * h)
  }, 1)
  if (all(is.na(g))) 0 else max(abs(g), na.rm = TRUE)
}

# ---------------------------------------------------------------------
# generalized linear mixed model
# ---------------------------------------------------------------------

#' Resolve the GLMM family
#'
#' @param family Character (\code{"binomial"}, \code{"poisson"}) or a
#'   \code{stats::family} object of those families.
#' @return A \code{stats::family} object.
#' @keywords internal
#' @noRd
.lmm_family <- function(family) {
  if (is.function(family)) family <- family()
  if (is.character(family)) {
    family <- match.arg(family[1L], c("binomial", "poisson"))
    family <- switch(family, binomial = stats::binomial(),
                     poisson = stats::poisson())
  }
  if (!inherits(family, "family") ||
      !family$family %in% c("binomial", "poisson"))
    stop("family must be binomial or poisson", call. = FALSE)
  family
}

#' Generalized linear mixed model by the Laplace approximation (native)
#'
#' Fits a binomial or Poisson GLMM with lme4-style random effects by
#' the Laplace approximation (\code{nAGQ = 1}), computed by penalised
#' iteratively reweighted least squares (PIRLS).  As \code{lme4::glmer}
#' does, a first stage optimises the covariance parameters with the
#' fixed effects inside PIRLS (\code{nAGQ = 0}); the second stage
#' optimises covariance parameters and fixed effects jointly.  The same
#' structured algebra as \code{\link{morie_lmm}} is used inside every
#' PIRLS step, so nothing of size \eqn{n \times n} is formed.
#'
#' @param formula A two-sided formula with lme4-style random terms.  A
#'   binomial response may be 0/1, a factor (first level = failure),
#'   logical, a proportion with \code{weights} giving the trials, or
#'   \code{cbind(successes, failures)}.
#' @param data A data frame.
#' @param family \code{"binomial"} (logit) or \code{"poisson"} (log),
#'   or a \code{stats::family} object of either (other links allowed).
#' @param random Optional nlme-style random effects (see
#'   \code{\link{morie_lmm}}).
#' @param weights Optional prior weights (binomial: number of trials).
#' @param offset Optional offset.
#' @param nAGQ 1 (Laplace, default) or 0 (stage 1 only, as glmer).
#' @param vcov_method \code{"hessian"} (default, lme4's default:
#'   inverse finite-difference Hessian of the deviance over theta and
#'   beta) or \code{"RX"} (the PIRLS solution, \code{use.hessian =
#'   FALSE} in lme4).  Both are returned.
#' @param control Optional list passed to \code{stats::nlminb}.
#' @param ... Ignored.
#' @return An object of class \code{c("morie_glmm", "morie_lmm")} with
#'   the components of \code{\link{morie_lmm}} (\code{sigma} is 1; the
#'   table has z values and normal p-values), plus \code{family},
#'   \code{vcov_RX}, \code{vcov_hessian}, \code{mu} and \code{eta}.
#'   \code{fitted} is the conditional mean; \code{residuals} are
#'   deviance residuals.
#' @references Bates, D., Maechler, M., Bolker, B. and Walker, S.
#'   (2015). Fitting linear mixed-effects models using lme4. Journal of
#'   Statistical Software, 67(1), 1-48.
#' @examples
#' set.seed(2)
#' d <- data.frame(g = factor(rep(1:30, each = 10)), x = rnorm(300))
#' d$y <- rbinom(300, 1, plogis(-0.5 + d$x + rnorm(30)[d$g]))
#' morie_glmm(y ~ x + (1 | g), d, family = "binomial")
#' @export
morie_glmm <- function(formula, data, family = c("binomial", "poisson"),
                       random = NULL, weights = NULL, offset = NULL,
                       nAGQ = 1L, vcov_method = c("hessian", "RX"),
                       control = list(), ...) {
  cl <- match.call()
  fam <- .lmm_family(family)
  vcov_method <- match.arg(vcov_method)
  if (!nAGQ %in% c(0, 1))
    stop("nAGQ must be 0 or 1 (adaptive quadrature is not implemented)",
         call. = FALSE)
  st <- .lmm_setup(formula, data, random, weights, offset)
  yr <- st$y
  pw <- st$w
  if (fam$family == "binomial") {
    if (is.matrix(yr) && ncol(yr) == 2L) {
      tot <- yr[, 1] + yr[, 2]
      y <- ifelse(tot > 0, yr[, 1] / tot, 0)
      pw <- pw * tot
    } else if (is.factor(yr)) {
      y <- as.numeric(yr != levels(yr)[1L])
    } else {
      y <- as.numeric(yr)
    }
    if (any(y < 0 | y > 1)) stop("binomial responses must lie in [0, 1]",
                                 call. = FALSE)
  } else {
    y <- as.numeric(yr)
    if (any(y < 0)) stop("poisson responses must be non-negative",
                         call. = FALSE)
  }
  X <- st$X
  p <- ncol(X)
  n <- st$n
  off <- st$offset
  ntheta <- length(st$theta_start)
  q1 <- st$blocks[[st$i1]]$q
  m1 <- st$blocks[[st$i1]]$m
  # -2 log-likelihood of the conditional distribution (what glmer's
  # family aic() returns), with its mu-free part computed once
  pos <- y > 0
  if (fam$family == "binomial") {
    neg <- y < 1
    k2 <- -2 * sum(lgamma(pw + 1) - lgamma(pw * y + 1) - lgamma(pw * (1 - y) + 1))
    wy <- (pw * y)[pos]
    wny <- (pw * (1 - y))[neg]
    m2ll <- function(mu) {
      k2 - 2 * (sum(wy * log(mu[pos])) + sum(wny * log1p(-mu[neg])))
    }
  } else {
    k2 <- 2 * sum(pw * lgamma(y + 1))
    wy <- (pw * y)[pos]
    m2ll <- function(mu) k2 - 2 * (sum(wy * log(mu[pos])) - sum(pw * mu))
  }
  # PIRLS state, kept between evaluations (warm starts, as lme4)
  S <- new.env(parent = emptyenv())
  S$u1 <- matrix(0, m1, q1)
  S$u2 <- numeric(st$q2)
  S$beta <- numeric(p)
  eta_of <- function(beta, u1, u2, Ts) {
    e <- off + .lmm_zb(.lmm_b(u1, u2, st, Ts), st)
    if (p > 0) e <- e + as.numeric(X %*% beta)
    e
  }
  pirls <- function(theta, beta, u_only, tol = 1e-11, maxit = 100L) {
    Ts <- .lmm_tmats(theta, st)
    u1 <- S$u1
    u2 <- S$u2
    if (!u_only) beta <- S$beta
    eta <- eta_of(beta, u1, u2, Ts)
    mu <- fam$linkinv(eta)
    pdev <- m2ll(mu) + sum(u1^2) + sum(u2^2)
    ldL2 <- NA_real_
    sol <- NULL
    for (it in seq_len(maxit)) {
      me <- fam$mu.eta(eta)
      W <- pw * me^2 / fam$variance(mu)
      xb <- if (p > 0) as.numeric(X %*% beta) else 0
      z <- eta - off + (y - mu) / me
      if (u_only) {
        cp <- .lmm_cp(st, W, matrix(z - xb))
        sol <- .lmm_pls(cp, st, Ts, 0L)
      } else {
        cp <- .lmm_cp(st, W, cbind(X, z))
        sol <- .lmm_pls(cp, st, Ts, p)
      }
      ldL2 <- sol$ldL2
      d1 <- sol$u1 - u1
      d2 <- sol$u2 - u2
      db <- if (u_only) numeric(p) else sol$beta - beta
      step <- 1
      accepted <- FALSE
      for (k in 0:30) {
        nu1 <- u1 + step * d1
        nu2 <- u2 + step * d2
        nb <- beta + step * db
        neta <- eta_of(nb, nu1, nu2, Ts)
        nmu <- fam$linkinv(neta)
        npdev <- m2ll(nmu) + sum(nu1^2) + sum(nu2^2)
        if (is.finite(npdev) && npdev <= pdev + 1e-10 * abs(pdev)) {
          accepted <- TRUE
          break
        }
        step <- step / 2
      }
      if (!accepted) break
      change <- pdev - npdev
      u1 <- nu1
      u2 <- nu2
      beta <- nb
      eta <- neta
      mu <- nmu
      pdev <- npdev
      # stop once the Newton step itself is negligible, so that the
      # weights behind L (and hence ldL2) are those at the mode
      if (step == 1 && max(abs(c(d1, d2, db))) < tol) break
      if (abs(change) < 1e-15 * (abs(pdev) + 0.1) && step < 1) break
    }
    S$u1 <- u1
    S$u2 <- u2
    if (!u_only) S$beta <- beta
    list(dev = pdev + ldL2, beta = beta,
         u1 = u1, u2 = u2, mu = mu, eta = eta, Ts = Ts, iterations = it)
  }
  safe <- function(f) function(x) {
    v <- tryCatch(f(x), error = function(e) Inf)
    if (!is.finite(v)) Inf else v
  }
  # starting fixed effects: the GLM without random effects
  g0 <- stats::glm.fit(X, y, weights = pw, offset = off, family = fam)
  S$beta <- as.numeric(g0$coefficients)
  S$beta[is.na(S$beta)] <- 0
  ctrl <- utils::modifyList(list(eval.max = 3000, iter.max = 1500,
                                 rel.tol = 1e-12), control)
  f0 <- safe(function(th) pirls(th, NULL, FALSE)$dev)
  opt0 <- .lmm_optimize(st$theta_start, f0, st$theta_lower, ctrl,
                        polish = FALSE)
  fin0 <- pirls(opt0$par, NULL, FALSE)
  opt <- opt0
  par <- opt0$par
  lowerj <- c(st$theta_lower, rep(-Inf, p))
  if (nAGQ == 1) {
    S$beta <- fin0$beta
    f1 <- safe(function(pp) pirls(pp[seq_len(ntheta)], pp[ntheta + seq_len(p)],
                                  TRUE)$dev)
    opt <- .lmm_optimize(c(opt0$par, fin0$beta), f1, lowerj, ctrl)
    par <- opt$par
    fin <- pirls(par[seq_len(ntheta)], par[ntheta + seq_len(p)], TRUE)
    beta <- fin$beta
  } else {
    fin <- fin0
    beta <- fin0$beta
    par <- c(opt0$par, beta)
    f1 <- safe(function(pp) pirls(pp[seq_len(ntheta)], pp[ntheta + seq_len(p)],
                                  TRUE)$dev)
  }
  theta <- par[seq_len(ntheta)]
  names(beta) <- colnames(X)
  # RX at the final weights
  me <- fam$mu.eta(fin$eta)
  W <- pw * me^2 / fam$variance(fin$mu)
  z <- fin$eta - off + (y - fin$mu) / me
  solRX <- .lmm_pls(.lmm_cp(st, W, cbind(X, z)), st, fin$Ts, p)
  VRX <- if (p > 0) chol2inv(solRX$RX) else matrix(0, 0, 0)
  dimnames(VRX) <- list(colnames(X), colnames(X))
  Vh <- NULL
  if (p > 0) {
    keep <- list(u1 = S$u1, u2 = S$u2)
    fd <- function(pp) {
      S$u1 <- keep$u1
      S$u2 <- keep$u2
      f1(pp)
    }
    # central differences at every parameter, as lme4's optwrap (no
    # bounds: a negative diagonal theta gives the same deviance)
    dd <- .lmm_deriv12(fd, c(theta, beta), fx = fin$dev)
    S$u1 <- keep$u1
    S$u2 <- keep$u2
    Hi <- tryCatch(solve(dd$Hessian), error = function(e) NULL)
    if (!is.null(Hi)) {
      i <- ntheta + seq_len(p)
      Vh <- Hi[i, i, drop = FALSE] + t(Hi[i, i, drop = FALSE])
      if (any(!is.finite(Vh)) ||
          min(eigen(Vh, symmetric = TRUE, only.values = TRUE)$values) <= 0)
        Vh <- NULL
    }
    if (!is.null(Vh)) dimnames(Vh) <- dimnames(VRX)
  }
  if (vcov_method == "hessian" && is.null(Vh) && p > 0)
    warning("the finite-difference Hessian is not positive definite; ",
            "using the RX-based covariance", call. = FALSE)
  V <- if (vcov_method == "hessian" && !is.null(Vh)) Vh else VRX
  se <- sqrt(diag(V))
  zv <- beta / se
  tab <- data.frame(Estimate = beta, Std.Error = se, z = zv,
                    p = 2 * stats::pnorm(abs(zv), lower.tail = FALSE),
                    row.names = colnames(X))
  names(tab)[3:4] <- c("z value", "Pr(>|z|)")
  vc <- .lmm_varcorr(theta, st, 1)
  bl <- .lmm_b(fin$u1, fin$u2, st, fin$Ts)
  ll <- -fin$dev / 2
  dfm <- p + ntheta
  dres <- sign(y - fin$mu) * sqrt(pmax(fam$dev.resids(y, fin$mu, pw), 0))
  grad <- .lmm_free_grad(f1, par, lowerj)
  out <- list(call = cl, formula = formula, fixed = st$fixed, REML = FALSE,
              family = fam, nAGQ = nAGQ,
              coefficients = beta, vcov = V, vcov_RX = VRX, vcov_hessian = Vh,
              vcov_method = if (identical(V, Vh)) "hessian" else "RX",
              fixef_table = tab, sigma = 1,
              theta = setNames(theta, st$theta_names),
              varcorr = vc$varcorr, varcorr_table = vc$table,
              ranef = .lmm_ranef(bl, st), u = list(fin$u1, fin$u2),
              fitted = fin$mu, mu = fin$mu, eta = fin$eta, residuals = dres,
              logLik = ll, df = dfm, AIC = -2 * ll + 2 * dfm,
              BIC = -2 * ll + log(n) * dfm, deviance = fin$dev,
              REMLcrit = NA_real_,
              ngroups = vapply(st$blocks, function(b) b$m, 1), nobs = n,
              nobs_dropped = st$nobs_dropped,
              convergence = list(code = opt$convergence, message = opt$message,
                                 iterations = opt$iterations,
                                 evaluations = opt$evaluations,
                                 objective = opt$objective, max_grad = grad,
                                 stage1_objective = opt0$objective,
                                 pirls_iterations = fin$iterations,
                                 singular = any(theta[st$theta_lower == 0] < 1e-4)),
              devfun = f1)
  class(out) <- c("morie_glmm", "morie_lmm")
  out
}

# ---------------------------------------------------------------------
# methods
# ---------------------------------------------------------------------

#' Print a morie mixed model
#'
#' @param x A \code{morie_lmm} or \code{morie_glmm} object.
#' @param digits Significant digits.
#' @param ... Ignored.
#' @return \code{x}, invisibly.
#' @export
print.morie_lmm <- function(x, digits = 5, ...) {
  glmm <- inherits(x, "morie_glmm")
  if (glmm) {
    cat(sprintf("Generalized linear mixed model fit by maximum likelihood (Laplace) [%s, %s link]\n",
                x$family$family, x$family$link))
  } else {
    cat(sprintf("Linear mixed model fit by %s\n", if (x$REML) "REML" else "maximum likelihood"))
  }
  cat("Formula:", paste(deparse(x$formula), collapse = " "), "\n")
  crit <- if (!glmm && x$REML) c("REML criterion" = x$REMLcrit) else
    c(AIC = x$AIC, BIC = x$BIC, logLik = x$logLik, deviance = -2 * x$logLik)
  print(round(crit, 4))
  cat("\nRandom effects:\n")
  vt <- x$varcorr_table
  sdrows <- vt[is.na(vt$var2), ]
  show <- data.frame(Groups = sdrows$grp, Name = sdrows$var1,
                     Variance = signif(sdrows$vcov, digits),
                     Std.Dev. = signif(sdrows$sdcor, digits))
  if (!glmm) show <- rbind(show, data.frame(Groups = "Residual", Name = "",
                                            Variance = signif(x$sigma^2, digits),
                                            Std.Dev. = signif(x$sigma, digits)))
  print(show, row.names = FALSE)
  cr <- vt[!is.na(vt$var2), ]
  if (nrow(cr)) {
    cat("Correlations:\n")
    print(data.frame(Groups = cr$grp, var1 = cr$var1, var2 = cr$var2,
                     Corr = round(cr$sdcor, 3)), row.names = FALSE)
  }
  cat(sprintf("Number of obs: %d, groups: %s\n", x$nobs,
              paste(sprintf("%s, %d", names(x$ngroups), x$ngroups), collapse = "; ")))
  cat("\nFixed effects:\n")
  print(signif(as.matrix(x$fixef_table), digits))
  if (x$convergence$code != 0)
    cat("\nOptimizer message:", x$convergence$message, "\n")
  if (isTRUE(x$convergence$singular)) cat("boundary (singular) fit\n")
  invisible(x)
}

#' Summarise a morie mixed model
#'
#' @param object A \code{morie_lmm} or \code{morie_glmm} object.
#' @param ... Ignored.
#' @return An object of class \code{summary.morie_lmm} holding the
#'   fixed-effects table, variance components, information criteria,
#'   group counts and convergence information.
#' @export
summary.morie_lmm <- function(object, ...) {
  out <- object[c("call", "formula", "REML", "fixef_table", "varcorr",
                  "varcorr_table", "sigma", "logLik", "AIC", "BIC",
                  "REMLcrit", "ngroups", "nobs", "convergence")]
  out$coefficients <- object$fixef_table
  out$glmm <- inherits(object, "morie_glmm")
  out$object <- object
  class(out) <- "summary.morie_lmm"
  out
}

#' Print a morie mixed-model summary
#'
#' @param x A \code{summary.morie_lmm} object.
#' @param ... Passed to \code{print.morie_lmm}.
#' @return \code{x}, invisibly.
#' @export
print.summary.morie_lmm <- function(x, ...) {
  print.morie_lmm(x$object, ...)
  cat(sprintf("\nConvergence: code %d (%s), %d iterations, max |gradient| %.2g\n",
              x$convergence$code, x$convergence$message,
              as.integer(x$convergence$iterations), x$convergence$max_grad))
  invisible(x)
}

#' Fixed-effect estimates of a morie mixed model
#'
#' @param object A \code{morie_lmm} object.
#' @param ... Ignored.
#' @return Named numeric vector.
#' @export
coef.morie_lmm <- function(object, ...) object$coefficients

#' Covariance of the fixed-effect estimates
#'
#' @param object A \code{morie_lmm} object.
#' @param ... Ignored.
#' @return Matrix.
#' @export
vcov.morie_lmm <- function(object, ...) object$vcov

#' Fitted values of a morie mixed model
#'
#' @param object A \code{morie_lmm} object.
#' @param ... Ignored.
#' @return Numeric vector (conditional on the random-effect modes; the
#'   conditional mean for GLMMs).
#' @export
fitted.morie_lmm <- function(object, ...) object$fitted

#' Residuals of a morie mixed model
#'
#' @param object A \code{morie_lmm} object.
#' @param ... Ignored.
#' @return Numeric vector (response residuals for LMMs, deviance
#'   residuals for GLMMs, as lme4).
#' @export
residuals.morie_lmm <- function(object, ...) object$residuals

#' Log-likelihood of a morie mixed model
#'
#' @param object A \code{morie_lmm} object.
#' @param ... Ignored.
#' @return A \code{logLik} object (the REML criterion over -2 for REML
#'   fits, as lme4 and nlme).
#' @export
logLik.morie_lmm <- function(object, ...) {
  structure(object$logLik, df = object$df, nobs = object$nobs,
            class = "logLik")
}
