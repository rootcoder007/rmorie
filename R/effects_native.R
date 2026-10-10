# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) morie contributors
#
# This file is part of morie. morie is free software: you can
# redistribute it and/or modify it under the terms of the GNU Affero
# General Public License as published by the Free Software Foundation,
# either version 3 of the License, or (at your option) any later
# version. See LICENSE for the full text.

# Native engine behind morie_effects_emmeans(), morie_effects_predictions(),
# morie_effects_comparisons() and morie_effects_slopes() (R/effects.R).
#
# Everything is a linear map of the coefficient vector b (or a smooth
# function of X b for a GLM), so every standard error is an exact
# delta-method quadratic form J V J' with the analytic Jacobian J and
# V = vcov(model). The conventions follow emmeans 2.x (reference grid,
# equal-weight averaging, df rule, response-scale back-transformation) and
# marginaleffects 1.x (unit-level estimates, "+1" / "b - a" contrasts,
# centred finite-difference slopes with eps = 1e-4 * range, z-based
# inference, "invlink(link)" GLM predictions); both packages are used only
# by the cross-validation tests (tests/testthat/test-effects-native-parity.R).

# -- model setup ---------------------------------------------------------

#' @noRd
.mfx_setup <- function(model, fn) {
  if (!inherits(model, "lm")) {
    stop(sprintf(
      "%s(): the native engine supports `lm` and `glm` fits; got class `%s`.",
      fn, paste(class(model), collapse = "/")
    ), call. = FALSE)
  }
  if (!is.null(model$call$offset)) {
    stop(sprintf(
      "%s(): an `offset =` argument is not supported; put offset() in the formula.",
      fn
    ), call. = FALSE)
  }
  is_glm <- inherits(model, "glm")
  fam <- if (is_glm) model$family else stats::gaussian()
  bfull <- stats::coef(model)
  keep <- !is.na(bfull)
  b <- bfull[keep]
  V <- as.matrix(stats::vcov(model))
  V <- V[names(b), names(b), drop = FALSE]
  tt <- stats::delete.response(stats::terms(model))
  fac <- attr(tt, "factors")
  vlist <- as.list(attr(tt, "variables"))[-1L]
  used <- if (length(fac)) rowSums(fac != 0) > 0 else logical(0)
  vars <- unique(unlist(lapply(vlist[used], all.vars)))
  # variables the formula turns into factors (factor(cyl), as.factor(x), ordered(g)): emmeans and
  # marginaleffects treat them as categorical whatever their type in the data
  wrapped <- unique(unlist(lapply(vlist[used], function(e) {
    if (is.call(e) && as.character(e[[1L]])[1L] %in% c("factor", "as.factor", "ordered", "as.ordered")) all.vars(e)
  })))
  list(
    model = model, is_glm = is_glm, family = fam, b = b, V = V, tt = tt,
    xlev = model$xlevels, contrasts = model$contrasts, vars = vars, factor_vars = wrapped,
    data = .mfx_data(model, vars, fn),
    df_resid = model$df.residual
  )
}

# Recover the data the model was fitted on (rows actually used).
#' @noRd
.mfx_data <- function(model, vars, fn) {
  mf <- stats::model.frame(model)
  d <- NULL
  if (is.data.frame(model$data)) {
    d <- model$data
  } else if (is.name(model$call$data)) {
    d <- get0(as.character(model$call$data),
      envir = environment(stats::formula(model)), inherits = TRUE
    )
  }
  if (is.data.frame(d) && all(vars %in% names(d)) &&
    all(rownames(mf) %in% rownames(d))) {
    return(d[rownames(mf), , drop = FALSE])
  }
  if (all(vars %in% names(mf))) {
    return(as.data.frame(mf))
  }
  stop(sprintf(
    "%s(): could not recover the model's data; pass `newdata` explicitly.",
    fn
  ), call. = FALSE)
}

# Kind of each predictor: "factor" (factor / character), "logical",
# "binary" (numeric taking only 0 / 1) or "numeric".
#' @noRd
.mfx_kind <- function(s, v) {
  x <- s$data[[v]]
  if (is.factor(x) || is.character(x) || v %in% s$factor_vars) {
    return("factor")
  }
  if (is.logical(x)) {
    return("logical")
  }
  ux <- unique(x[!is.na(x)])
  if (is.numeric(x) && length(ux) <= 2L && all(ux %in% c(0, 1))) {
    return("binary")
  }
  "numeric"
}

#' @noRd
.mfx_levels <- function(s, v) {
  if (!is.null(s$xlev[[v]])) {
    return(s$xlev[[v]])
  }
  x <- s$data[[v]]
  if (is.factor(x)) {
    return(levels(x))
  }
  if (is.logical(x)) {
    return(c(FALSE, TRUE))
  }
  sort(unique(x[!is.na(x)]))
}

# Design matrix (columns = estimable coefficients) and offset for newdata.
#' @noRd
.mfx_design <- function(s, nd) {
  mf <- stats::model.frame(s$tt, nd, xlev = s$xlev, na.action = stats::na.pass)
  X <- stats::model.matrix(s$tt, mf, contrasts.arg = s$contrasts)
  miss <- setdiff(names(s$b), colnames(X))
  if (length(miss)) {
    stop("could not rebuild the design matrix for columns: ",
      paste(miss, collapse = ", "),
      call. = FALSE
    )
  }
  X <- X[, names(s$b), drop = FALSE]
  off <- stats::model.offset(mf)
  if (is.null(off)) off <- rep(0, nrow(X))
  list(X = X, offset = off)
}

# Mean (on `type` scale) and its Jacobian w.r.t. b at each row of nd.
#' @noRd
.mfx_mean <- function(s, nd, type) {
  des <- .mfx_design(s, nd)
  eta <- as.numeric(des$X %*% s$b) + des$offset
  if (type == "link") {
    return(list(est = eta, J = des$X))
  }
  list(
    est = s$family$linkinv(eta),
    J = des$X * s$family$mu.eta(eta)
  )
}

# Assemble estimate / SE / z / p / CI columns.
#' @noRd
.mfx_infer <- function(est, J, V, conf_level, df) {
  if (is.null(V)) {
    se <- rep(NA_real_, length(est))
  } else {
    se <- sqrt(pmax(rowSums((J %*% V) * J), 0))
  }
  stat <- est / se
  if (is.finite(df)) {
    p <- 2 * stats::pt(-abs(stat), df)
    q <- stats::qt(1 - (1 - conf_level) / 2, df)
  } else {
    p <- 2 * stats::pnorm(-abs(stat))
    q <- stats::qnorm(1 - (1 - conf_level) / 2)
  }
  data.frame(
    estimate = est, std.error = se, statistic = stat, p.value = p,
    s.value = -log2(p), conf.low = est - q * se, conf.high = est + q * se,
    row.names = NULL
  )
}

# Split / average unit-level estimates by grouping columns.
# `keys` is a data.frame of grouping columns (one row per unit row).
#' @noRd
.mfx_average <- function(est, J, keys) {
  if (ncol(keys) == 0L) {
    return(list(
      keys = keys[1L, , drop = FALSE], est = mean(est),
      J = matrix(colMeans(J), nrow = 1L, dimnames = list(NULL, colnames(J)))
    ))
  }
  ord <- do.call(order, unname(as.list(keys)))
  kstr <- do.call(paste, c(lapply(unname(as.list(keys)), as.character), sep = "\r"))
  u <- unique(kstr[ord])
  grp <- lapply(u, function(k) which(kstr == k))
  list(
    keys = keys[match(u, kstr), , drop = FALSE],
    est = vapply(grp, function(i) mean(est[i]), numeric(1)),
    J = do.call(rbind, lapply(grp, function(i) colMeans(J[i, , drop = FALSE])))
  )
}

# Parse the shared marginaleffects-style options in `...`.
#' @noRd
.mfx_opts <- function(dots, allowed, fn, s) {
  bad <- setdiff(names(dots), allowed)
  if (length(dots) && (is.null(names(dots)) || any(names(dots) == ""))) {
    stop(sprintf("%s(): all extra arguments must be named.", fn), call. = FALSE)
  }
  if (length(bad)) {
    stop(sprintf(
      "%s(): argument(s) %s not supported by the native engine (supported: %s).",
      fn, paste0("`", bad, "`", collapse = ", "),
      paste0("`", allowed, "`", collapse = ", ")
    ), call. = FALSE)
  }
  conf_level <- if (is.null(dots$conf_level)) 0.95 else dots$conf_level
  df <- if (is.null(dots$df)) Inf else dots$df
  vc <- if (is.null(dots$vcov)) TRUE else dots$vcov
  V <- if (isTRUE(vc)) {
    s$V
  } else if (isFALSE(vc)) {
    NULL
  } else if (is.matrix(vc)) {
    vc <- vc[names(s$b), names(s$b), drop = FALSE]
    vc
  } else {
    stop(sprintf(
      "%s(): `vcov` must be TRUE, FALSE or a covariance matrix.",
      fn
    ), call. = FALSE)
  }
  by <- if (is.null(dots$by)) FALSE else dots$by
  if (!(isTRUE(by) || isFALSE(by) || is.character(by))) {
    stop(sprintf("%s(): `by` must be TRUE, FALSE or column names.", fn),
      call. = FALSE
    )
  }
  type <- dots$type
  if (!is.null(type)) {
    type <- match.arg(type, c("response", "link"))
    if (type == "link" && !s$is_glm) type <- "response"
  }
  list(conf_level = conf_level, df = df, V = V, by = by, type = type)
}

#' @noRd
.mfx_newdata <- function(s, newdata, fn) {
  if (is.null(newdata)) {
    return(s$data)
  }
  if (!is.data.frame(newdata)) {
    stop(sprintf(
      "%s(): `newdata` must be NULL or a data frame for the native engine.",
      fn
    ), call. = FALSE)
  }
  newdata
}

#' @noRd
.mfx_by_keys <- function(nd, by, fn) {
  if (isTRUE(by)) {
    return(nd[, 0L, drop = FALSE])
  }
  miss <- setdiff(by, names(nd))
  if (length(miss)) {
    stop(sprintf(
      "%s(): `by` column(s) not found: %s", fn,
      paste(miss, collapse = ", ")
    ), call. = FALSE)
  }
  nd[, by, drop = FALSE]
}

# -- predictions -----------------------------------------------------------

#' @noRd
.mfx_predictions <- function(model, newdata, dots) {
  fn <- "morie_effects_predictions"
  s <- .mfx_setup(model, fn)
  o <- .mfx_opts(dots, c("type", "by", "conf_level", "vcov", "df"), fn, s)
  nd <- .mfx_newdata(s, newdata, fn)
  if (isFALSE(o$by) && s$is_glm && is.null(o$type)) {
    # marginaleffects' "invlink(link)": inference on the link scale,
    # estimate and interval mapped back; no response-scale SE.
    m <- .mfx_mean(s, nd, "link")
    inf <- .mfx_infer(m$est, m$J, o$V, o$conf_level, o$df)
    out <- data.frame(
      rowid = seq_len(nrow(nd)),
      estimate = s$family$linkinv(m$est),
      std.error = NA_real_, statistic = NA_real_,
      p.value = inf$p.value, s.value = inf$s.value,
      conf.low = s$family$linkinv(inf$conf.low),
      conf.high = s$family$linkinv(inf$conf.high)
    )
    lo <- pmin(out$conf.low, out$conf.high)
    out$conf.high <- pmax(out$conf.low, out$conf.high)
    out$conf.low <- lo
    out <- cbind(out, nd[, setdiff(names(nd), names(out)), drop = FALSE])
    rownames(out) <- NULL
    attr(out, "type") <- "invlink(link)"
    return(out)
  }
  type <- if (is.null(o$type)) "response" else o$type
  m <- .mfx_mean(s, nd, type)
  if (isFALSE(o$by)) {
    out <- cbind(
      data.frame(rowid = seq_len(nrow(nd))),
      .mfx_infer(m$est, m$J, o$V, o$conf_level, o$df)
    )
    out <- cbind(out, nd[, setdiff(names(nd), names(out)), drop = FALSE])
  } else {
    a <- .mfx_average(m$est, m$J, .mfx_by_keys(nd, o$by, fn))
    out <- cbind(a$keys, .mfx_infer(a$est, a$J, o$V, o$conf_level, o$df))
  }
  rownames(out) <- NULL
  attr(out, "type") <- type
  out
}

# -- comparisons / slopes ----------------------------------------------------

# Build the (term, contrast, lo-data, hi-data, divisor) list for one variable.
#' @noRd
.mfx_contrasts <- function(s, v, nd, step = NULL, slope = FALSE, eps = NULL) {
  kind <- .mfx_kind(s, v)
  out <- list()
  if (kind %in% c("factor", "logical", "binary")) {
    lev <- switch(kind,
      factor = .mfx_levels(s, v),
      logical = c(FALSE, TRUE),
      binary = c(0, 1)
    )
    for (l in lev[-1L]) {
      lo <- nd
      hi <- nd
      if (kind == "factor") {
        make <- function(val) {
          if (is.factor(nd[[v]])) factor(rep(val, nrow(nd)), levels = levels(nd[[v]])) else rep(val, nrow(nd))
        }
        lo[[v]] <- make(lev[1L])
        hi[[v]] <- make(l)
      } else {
        lo[[v]] <- rep(lev[1L], nrow(nd))
        hi[[v]] <- rep(l, nrow(nd))
      }
      out[[length(out) + 1L]] <- list(
        term = v, contrast = paste(l, "-", lev[1L]), lo = lo, hi = hi, div = 1
      )
    }
    return(out)
  }
  x <- nd[[v]]
  if (slope) {
    e <- if (!is.null(eps)) {
      eps
    } else {
      r <- diff(range(s$data[[v]], na.rm = TRUE, finite = TRUE)) * 1e-4
      if (r == 0) 1e-4 else r
    }
    lo <- nd
    hi <- nd
    lo[[v]] <- x - e / 2
    hi[[v]] <- x + e / 2
    return(list(list(term = v, contrast = "dY/dX", lo = lo, hi = hi, div = e)))
  }
  st <- if (is.null(step)) 1 else step
  if (!(is.numeric(st) && length(st) == 1L && is.finite(st))) {
    stop(sprintf(
      "morie_effects_comparisons(): the contrast for `%s` must be a single number (step size).",
      v
    ), call. = FALSE)
  }
  hi <- nd
  hi[[v]] <- x + st
  list(list(term = v, contrast = paste0("+", format(st)), lo = nd, hi = hi, div = 1))
}

#' @noRd
.mfx_compare <- function(model, variables, newdata, dots, slope) {
  fn <- if (slope) "morie_effects_slopes" else "morie_effects_comparisons"
  s <- .mfx_setup(model, fn)
  allowed <- c("newdata", "type", "by", "conf_level", "vcov", "df")
  allowed <- c(allowed, if (slope) c("eps", "slope") else "comparison")
  o <- .mfx_opts(dots, allowed, fn, s)
  if (slope && !is.null(dots$slope) && !identical(dots$slope, "dydx")) {
    stop(fn, "(): only slope = \"dydx\" is supported by the native engine.",
      call. = FALSE
    )
  }
  if (!slope && !is.null(dots$comparison) &&
    !identical(dots$comparison, "difference")) {
    stop(fn, "(): only comparison = \"difference\" is supported by the native engine.",
      call. = FALSE
    )
  }
  nd <- .mfx_newdata(s, newdata, fn)
  type <- if (is.null(o$type)) "response" else o$type
  steps <- list()
  if (is.null(variables)) {
    vars <- s$vars
  } else if (is.list(variables)) {
    vars <- names(variables)
    steps <- variables
  } else {
    vars <- as.character(variables)
  }
  bad <- setdiff(vars, s$vars)
  if (length(bad)) {
    stop(sprintf(
      "%s(): variable(s) not in the model: %s", fn,
      paste(bad, collapse = ", ")
    ), call. = FALSE)
  }
  # marginaleffects orders terms alphabetically; numeric -> "+1" (or
  # dY/dX for slopes), categorical -> each level minus the reference.
  vars <- sort(vars)
  pieces <- list()
  for (v in vars) {
    for (ct in .mfx_contrasts(s, v, nd,
      step = steps[[v]], slope = slope,
      eps = dots$eps
    )) {
      mh <- .mfx_mean(s, ct$hi, type)
      ml <- .mfx_mean(s, ct$lo, type)
      pieces[[length(pieces) + 1L]] <- list(
        term = ct$term, contrast = ct$contrast,
        est = (mh$est - ml$est) / ct$div, J = (mh$J - ml$J) / ct$div,
        lo = ml$est, hi = mh$est
      )
    }
  }
  if (isFALSE(o$by)) {
    out <- do.call(rbind, lapply(pieces, function(p) {
      cbind(
        data.frame(
          rowid = seq_len(nrow(nd)), term = p$term,
          contrast = p$contrast, stringsAsFactors = FALSE
        ),
        .mfx_infer(p$est, p$J, o$V, o$conf_level, o$df),
        nd[, setdiff(names(nd), c("rowid", "term", "contrast")), drop = FALSE],
        data.frame(predicted_lo = p$lo, predicted_hi = p$hi)
      )
    }))
  } else {
    keys <- .mfx_by_keys(nd, o$by, fn)
    # marginaleffects orders averaged rows by term, then by group, then
    # contrast.
    out <- do.call(rbind, lapply(seq_along(pieces), function(i) {
      p <- pieces[[i]]
      a <- .mfx_average(p$est, p$J, keys)
      cbind(
        data.frame(
          term = rep(p$term, length(a$est)),
          contrast = rep(p$contrast, length(a$est)),
          stringsAsFactors = FALSE
        ),
        a$keys,
        .mfx_infer(a$est, a$J, o$V, o$conf_level, o$df),
        data.frame(.pi = i, .gi = seq_along(a$est))
      )
    }))
    out <- out[order(match(out$term, vars), out$.gi, out$.pi), , drop = FALSE]
    out$.pi <- NULL
    out$.gi <- NULL
  }
  rownames(out) <- NULL
  attr(out, "type") <- type
  out
}

# -- emmeans ---------------------------------------------------------------------

#' @noRd
.emm_native <- function(model, specs, dots) {
  fn <- "morie_effects_emmeans"
  s <- .mfx_setup(model, fn)
  allowed <- c("type", "level", "at", "by")
  bad <- setdiff(names(dots), allowed)
  if (length(bad)) {
    stop(sprintf(
      "%s(): argument(s) %s not supported by the native engine (supported: %s).",
      fn, paste0("`", bad, "`", collapse = ", "),
      paste0("`", allowed, "`", collapse = ", ")
    ), call. = FALSE)
  }
  by <- dots$by
  if (inherits(specs, "formula")) {
    if (length(specs) == 3L) {
      stop(fn, "(): two-sided specs (e.g. pairwise ~ g) are not supported; ",
        "use a one-sided formula and contrast the returned means.",
        call. = FALSE
      )
    }
    rhs <- specs[[2L]]
    if (is.call(rhs) && identical(rhs[[1L]], as.name("|"))) {
      spec <- all.vars(rhs[[2L]])
      by <- c(by, all.vars(rhs[[3L]]))
    } else {
      spec <- all.vars(rhs)
    }
  } else if (is.character(specs)) {
    spec <- specs
  } else {
    stop(fn, "(): `specs` must be a one-sided formula or a character vector.",
      call. = FALSE
    )
  }
  pri <- c(spec, setdiff(by, spec))
  miss <- setdiff(pri, s$vars)
  if (length(miss)) {
    stop(sprintf(
      "%s(): spec variable(s) not in the model: %s", fn,
      paste(miss, collapse = ", ")
    ), call. = FALSE)
  }
  type <- if (is.null(dots$type)) "link" else match.arg(dots$type, c("link", "response"))
  level <- if (is.null(dots$level)) 0.95 else dots$level
  at <- if (is.null(dots$at)) list() else dots$at
  lhs <- stats::formula(model)[[2L]]
  if (type == "response" && !s$is_glm && is.call(lhs)) {
    stop(fn, "(): type = \"response\" with a transformed response is not ",
      "supported by the native engine.",
      call. = FALSE
    )
  }
  # Reference grid: categorical predictors at all levels, numeric ones at
  # their mean (or `at`), first variable varying fastest.
  vals <- lapply(s$vars, function(v) {
    if (!is.null(at[[v]])) {
      return(at[[v]])
    }
    k <- .mfx_kind(s, v)
    if (k == "factor") {
      return(.mfx_levels(s, v))
    }
    if (k == "logical") {
      return(c(FALSE, TRUE))
    }
    # emmeans' cov.keep = "2": a numeric covariate with at most two
    # distinct values enters the grid at both values.
    ux <- sort(unique(s$data[[v]][!is.na(s$data[[v]])]))
    if (length(ux) <= 2L) {
      return(ux)
    }
    mean(s$data[[v]], na.rm = TRUE)
  })
  names(vals) <- s$vars
  grid <- expand.grid(vals, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  for (v in s$vars) {
    if (is.factor(s$data[[v]])) {
      grid[[v]] <- factor(grid[[v]], levels = levels(s$data[[v]]))
    }
  }
  des <- .mfx_design(s, grid)
  off <- des$offset
  Xg <- des$X
  combos <- expand.grid(vals[pri], KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  L <- matrix(0, nrow(combos), ncol(Xg), dimnames = list(NULL, colnames(Xg)))
  offL <- numeric(nrow(combos))
  for (i in seq_len(nrow(combos))) {
    rows <- rep(TRUE, nrow(grid))
    for (v in pri) rows <- rows & (as.character(grid[[v]]) == as.character(combos[[v]][i]))
    L[i, ] <- colMeans(Xg[rows, , drop = FALSE])
    offL[i] <- mean(off[rows])
  }
  eta <- as.numeric(L %*% s$b) + offL
  se <- sqrt(pmax(rowSums((L %*% s$V) * L), 0))
  fam <- s$family$family
  df <- if (!s$is_glm || fam %in% c("gaussian", "Gamma")) s$df_resid else Inf
  q <- stats::qt(1 - (1 - level) / 2, df)
  lo <- eta - q * se
  hi <- eta + q * se
  est_name <- "emmean"
  link <- s$family$link
  if (type == "response" && s$is_glm && link != "identity") {
    est_name <- if (grepl("binomial", fam)) {
      "prob"
    } else if (grepl("poisson", fam)) {
      "rate"
    } else {
      "response"
    }
    se <- se * abs(s$family$mu.eta(eta))
    lo2 <- s$family$linkinv(lo)
    hi2 <- s$family$linkinv(hi)
    lo <- pmin(lo2, hi2)
    hi <- pmax(lo2, hi2)
    eta <- s$family$linkinv(eta)
  }
  for (v in pri) {
    if (.mfx_kind(s, v) == "factor") {
      combos[[v]] <- factor(combos[[v]], levels = .mfx_levels(s, v))
    }
  }
  out <- combos
  out[[est_name]] <- eta
  out$SE <- se
  out$df <- df
  cl <- if (is.finite(df)) c("lower.CL", "upper.CL") else c("asymp.LCL", "asymp.UCL")
  out[[cl[1L]]] <- lo
  out[[cl[2L]]] <- hi
  rownames(out) <- NULL
  attr(out, "estName") <- est_name
  attr(out, "clNames") <- cl
  attr(out, "pri.vars") <- spec
  attr(out, "by.vars") <- if (length(by)) setdiff(by, spec) else NULL
  attr(out, "type") <- type
  attr(out, "linkname") <- link
  attr(out, "linfct") <- L
  out
}
