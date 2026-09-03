# Pearl's three-step counterfactual: abduction, action, prediction.
#
# Mirror of the Python arm (morie.fn.abdpd), verified against the same
# primary source: Pearl (2000), *Causality*, 1st ed., Section 1.4
# pp. 36-37 for the three-step procedure, Theorem 7.1.7 for the
# probabilistic statement.  This is the deterministic form: it recovers a
# point u and returns a single counterfactual value, which is exactly the
# case the book works (binary exogenous variables, evidence "compatible
# with only one realization of U1 and U2").
#
# Equations are a named list: each element list(parents = c(...),
# fn = function(...)) with the fn taking its parents BY NAME.  The solver
# resolves endogenous variables in dependency order.
#
# The optimizer is a small native Nelder-Mead; nothing here calls
# stats::.  When several u reproduce the evidence, the result reports it
# (n_compatible_u, counterfactual_unique) instead of silently picking one.

#' .morie_scm_solve
#'
#' A step of the counterfactual implementation. Called by \code{Counterfactual}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param u Coerced to list by the body, with \code{as.list}.
#' @param equations A vector; indexed elementwise.
#' @return The value of \code{vals}, as built in the body.
#' @export
.morie_scm_solve <- function(u, equations) {
  vals <- as.list(u)
  remaining <- names(equations)
  for (pass in seq_len(length(remaining) + 1)) {
    progress <- FALSE
    still <- character(0)
    for (nm in remaining) {
      eq <- equations[[nm]]
      if (all(eq$parents %in% names(vals))) {
        args <- vals[eq$parents]
        names(args) <- eq$parents
        vals[[nm]] <- do.call(eq$fn, args)
        progress <- TRUE
      } else {
        still <- c(still, nm)
      }
    }
    remaining <- still
    if (!length(remaining)) break
    if (!progress) {
      stop(
        "equations contain a cycle or an unknown parent: ",
        paste(remaining, collapse = ", ")
      )
    }
  }
  vals
}

#' Compact Nelder-Mead (reflection 1, expansion 2, contraction 0.5,
#'
#' shrink 0.5) -- enough for the small abduction problems this serves.
#' ponytail: fold into a shared native optimizer when the stats::optim
#' sweep reaches the 680 call sites.
#'
#' @param f Passed to \code{apply}.
#' @param x0 A vector; its length is taken.
#' @param maxit A count; the body uses it as \code{seq_len(...)}. Defaults to \code{400}.
#' @param tol Numeric; combined arithmetically in the body. Defaults to \code{1e-10}.
#' @return A list with \code{par}, \code{value}.
#' @export
.morie_neldermead <- function(f, x0, maxit = 400, tol = 1e-10) {
  # compact Nelder-Mead (reflection 1, expansion 2, contraction 0.5,
  # shrink 0.5) -- enough for the small abduction problems this serves.
  # ponytail: fold into a shared native optimizer when the stats::optim
  # sweep reaches the 680 call sites.
  n <- length(x0)
  simplex <- matrix(rep(x0, n + 1), nrow = n + 1, byrow = TRUE)
  for (i in seq_len(n)) simplex[i + 1, i] <- simplex[i + 1, i] + 0.5
  fv <- apply(simplex, 1, f)
  for (it in seq_len(maxit)) {
    ord <- order(fv)
    simplex <- simplex[ord, , drop = FALSE]
    fv <- fv[ord]
    if (abs(fv[n + 1] - fv[1]) < tol * (abs(fv[1]) + tol)) break
    centroid <- colMeans(simplex[1:n, , drop = FALSE])
    xr <- centroid + (centroid - simplex[n + 1, ])
    fr <- f(xr)
    if (fr < fv[1]) {
      xe <- centroid + 2 * (centroid - simplex[n + 1, ])
      fe <- f(xe)
      if (fe < fr) {
        simplex[n + 1, ] <- xe
        fv[n + 1] <- fe
      } else {
        simplex[n + 1, ] <- xr
        fv[n + 1] <- fr
      }
    } else if (fr < fv[n]) {
      simplex[n + 1, ] <- xr
      fv[n + 1] <- fr
    } else {
      xc <- centroid + 0.5 * (simplex[n + 1, ] - centroid)
      fc <- f(xc)
      if (fc < fv[n + 1]) {
        simplex[n + 1, ] <- xc
        fv[n + 1] <- fc
      } else {
        for (i in 2:(n + 1)) {
          simplex[i, ] <- simplex[1, ] + 0.5 * (simplex[i, ] - simplex[1, ])
          fv[i] <- f(simplex[i, ])
        }
      }
    }
  }
  list(par = simplex[which.min(fv), ], value = min(fv))
}

#' Counterfactual
#'
#' A step of the counterfactual implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param evidence A vector; indexed elementwise.
#' @param equations Passed to \code{names}.
#' @param exogenous A vector; its length is taken.
#' @param do A vector; indexed elementwise.
#' @param query Carried through into a list the body builds.
#' @param u_support The body requires: candidates is too large; pass a smaller u_support.
#' @return A list with \code{counterfactual}, \code{factual}, \code{abducted},
#' \code{n_compatible_u}, \code{counterfactual_unique}, \code{residual}, \code{do},
#' \code{query}, \code{method}.
#' @export
Counterfactual <- function(evidence, equations, exogenous, do, query,
                           u_support = NULL) {
  if (!length(exogenous)) stop("need at least one exogenous variable")
  for (v in names(do)) {
    if (!v %in% names(equations)) {
      stop("cannot intervene on ", v, ": it has no structural equation")
    }
  }
  if (!query %in% c(names(equations), exogenous)) {
    stop("query ", query, " is not a variable of the model")
  }
  observed <- evidence[names(evidence) %in% names(equations)]
  if (!length(observed)) {
    stop("evidence must fix at least one endogenous variable")
  }

  solve_at <- function(u_vec, eqs) {
    u <- as.list(u_vec)
    names(u) <- exogenous
    known <- evidence[names(evidence) %in% exogenous]
    u[names(known)] <- known
    .morie_scm_solve(u, eqs)
  }
  resid_at <- function(u_vec) {
    vals <- solve_at(u_vec, equations)
    max(abs(unlist(vals[names(observed)]) - unlist(observed)))
  }

  k <- length(exogenous)
  # Support enumeration runs FIRST.  With non-smooth equations the
  # evidence does not pin u uniquely in the continuum -- a step function
  # is satisfied by a whole interval -- so a numerical solver can land on
  # an off-support u that reproduces the evidence yet gives the wrong
  # counterfactual.  Pearl's "compatible with only one realization" is a
  # statement WITHIN the support, so grid solutions take precedence and
  # the gradient path serves genuinely continuous models only.
  support <- as.numeric(if (is.null(u_support)) c(0, 1) else u_support)
  if (!is.null(u_support) && length(support)^k > 200000) {
    stop(
      "discrete abduction over ", length(support), "^", k,
      " candidates is too large; pass a smaller u_support"
    )
  }

  solutions <- list()
  method <- ""
  if (length(support)^k <= 200000) {
    grid <- as.matrix(expand.grid(rep(list(support), k)))
    for (i in seq_len(nrow(grid))) {
      if (resid_at(grid[i, ]) < 1e-9) {
        solutions[[length(solutions) + 1]] <- grid[i, ]
      }
    }
  }
  if (length(solutions)) {
    u_hat <- solutions[[1]]
    resid <- 0
    method <- sprintf(
      "discrete abduction over support (%s)",
      paste(support, collapse = ", ")
    )
  } else {
    nm <- .morie_neldermead(resid_at, rep(0, k))
    u_hat <- nm$par
    resid <- resid_at(u_hat)
    solutions <- list(u_hat)
    method <- "gradient abduction"
  }

  mutilated <- equations
  for (v in names(do)) {
    val <- do[[v]]
    mutilated[[v]] <- list(
      parents = character(0),
      fn = local({
        vv <- val
        function() vv
      })
    )
  }
  cfs <- vapply(solutions, function(u) {
    as.numeric(solve_at(u, mutilated)[[query]])
  }, numeric(1))
  factual <- as.numeric(solve_at(u_hat, equations)[[query]])
  abducted <- as.numeric(u_hat)
  names(abducted) <- exogenous

  list(
    counterfactual = cfs[1],
    factual = factual,
    abducted = abducted,
    n_compatible_u = length(solutions),
    counterfactual_unique = all(abs(cfs - cfs[1]) < 1e-12),
    residual = resid,
    do = do,
    query = query,
    method = paste0(
      "Abduction-action-prediction (Pearl 2000, Sec. 1.4; ",
      method, ")"
    )
  )
}

# alias: pre-policy spelling
morie_counterfactual <- Counterfactual
