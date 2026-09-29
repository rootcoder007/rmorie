.cf_edges <- function(dag) {
  if (is.list(dag) && !is.data.frame(dag)) {
    E <- do.call(rbind, lapply(names(dag), function(p) if (length(dag[[p]])) cbind(p, as.character(dag[[p]]))))
    nodes <- unique(c(names(dag), unlist(dag)))
  } else {
    E <- as.matrix(dag)
    nodes <- unique(as.vector(E))
  }
  list(E = matrix(as.character(E), ncol = 2), nodes = as.character(nodes))
}

.cf_desc <- function(E, v) {
  out <- character(0)
  front <- v
  while (length(front)) {
    nxt <- setdiff(E[E[, 1] %in% front, 2], c(out, v))
    out <- c(out, nxt)
    front <- nxt
  }
  out
}

.cf_anc <- function(E, v) {
  out <- v
  front <- v
  while (length(front)) {
    nxt <- setdiff(E[E[, 2] %in% front, 1], out)
    out <- c(out, nxt)
    front <- nxt
  }
  out
}

.cf_dsep <- function(E, x, y, z) {
  A <- .cf_anc(E, c(x, y, z))
  Ea <- E[E[, 1] %in% A & E[, 2] %in% A, , drop = FALSE]
  und <- Ea
  for (ch in unique(Ea[, 2])) {
    pa <- Ea[Ea[, 2] == ch, 1]
    if (length(pa) > 1) und <- rbind(und, t(utils::combn(pa, 2)))
  }
  und <- und[!(und[, 1] %in% z) & !(und[, 2] %in% z), , drop = FALSE]
  seen <- x
  front <- x
  while (length(front)) {
    nb <- unique(c(und[und[, 1] %in% front, 2], und[und[, 2] %in% front, 1]))
    nb <- setdiff(nb, c(seen, z))
    if (any(nb %in% y)) return(FALSE)
    seen <- c(seen, nb)
    front <- nb
  }
  TRUE
}

.cf_backdoor <- function(E, x, y, z) {
  if (any(z %in% .cf_desc(E, x))) return(FALSE)
  .cf_dsep(E[E[, 1] != x, , drop = FALSE], x, y, z)
}

.cf_directed_paths <- function(E, x, y) {
  out <- list()
  walk <- function(path) {
    last <- path[length(path)]
    if (last == y) {
      out[[length(out) + 1]] <<- path
      return(invisible(NULL))
    }
    for (ch in E[E[, 1] == last, 2]) if (!(ch %in% path)) walk(c(path, ch))
  }
  walk(x)
  out
}

#' Causal formulas: E-value, mediation and the front-door
#'
#' `causal_e_value` is the VanderWeele-Ding E-value `RR* + sqrt(RR*(RR* -
#' 1))`; `unmeasured_conf_bias` the Ding-VanderWeele bounding factor `RR_UD
#' RR_UY / (RR_UD + RR_UY - 1)`; `causal_mediation_baron_kenny` and
#' `mediation_analysis` the Baron-Kenny three-regression decomposition
#' (`morie_baron_kenny`), the latter after residualising baseline covariates;
#' `frontdoor_adjustment` (and `front_door`) Pearl's front-door formula for
#' discrete data; `frontdoor_criterion` Pearl's three front-door conditions
#' checked by d-separation in the moral ancestral graph; `mediation_formula`
#' Pearl's natural direct and indirect effects from cell means.
#'
#' @param RR Risk ratio.
#' @param ci_lower,ci_upper Optional confidence limits.
#' @param rare_outcome Recorded flag (odds or hazard ratios approximate RR).
#' @param X,M,Y,T,y,m,x,z Treatment, mediator, outcome (and covariates `X`
#'   in `mediation_analysis`).
#' @param at Treatment levels to intervene on.
#' @param dag Named list of children or two-column edge matrix.
#' @param Z Candidate front-door set.
#' @param x1,x0 Treatment levels contrasted.
#' @param RR_UD,RR_UY Confounder-treatment and confounder-outcome risk ratios.
#' @param RR_obs Observed risk ratio.
#' @return Lists with the components of the Python arm.
#' @references VanderWeele, T. J. and Ding, P. (2017). Sensitivity analysis
#'   in observational research: introducing the E-value. Annals of Internal
#'   Medicine 167, 268-274. Ding, P. and VanderWeele, T. J. (2016).
#'   Sensitivity analysis without assumptions. Epidemiology 27, 368-377.
#'   Baron, R. M. and Kenny, D. A. (1986). The moderator-mediator variable
#'   distinction. Journal of Personality and Social Psychology 51, 1173-1182.
#'   Pearl, J. (2009). Causality, 2nd ed. Cambridge University Press. Pearl,
#'   J. (2001). Direct and indirect effects. Proceedings of UAI 17, 411-420.
#' @examples
#' causal_e_value(2)$evalue
#' unmeasured_conf_bias(2, 3)$bias_factor
#' @export
causal_e_value <- function(RR, ci_lower = NULL, ci_upper = NULL, rare_outcome = TRUE) {
  if (RR <= 0) stop("RR must be positive")
  e <- function(r) {
    s <- max(r, 1 / r)
    s + sqrt(s * (s - 1))
  }
  eci <- NULL
  if (!is.null(ci_lower) || !is.null(ci_upper)) {
    if (is.null(ci_lower) || is.null(ci_upper)) stop("Supply both confidence limits or neither.")
    eci <- if (ci_lower <= 1 && 1 <= ci_upper) 1 else e(if (ci_lower > 1) ci_lower else ci_upper)
  }
  list(evalue = e(RR), estimate = e(RR), evalue_ci = eci, rr = RR, rr_star = max(RR, 1 / RR),
       rare_outcome = rare_outcome, method = "E-value (VanderWeele & Ding 2017)")
}

#' @rdname causal_e_value
#' @export
unmeasured_conf_bias <- function(RR_UD, RR_UY, RR_obs = NULL) {
  if (RR_UD < 1 || RR_UY < 1) stop("RR_UD and RR_UY must be >= 1 (reciprocate protective ratios first)")
  B <- RR_UD * RR_UY / (RR_UD + RR_UY - 1)
  bound <- explains <- NULL
  if (!is.null(RR_obs)) {
    r <- max(RR_obs, 1 / RR_obs)
    bound <- r / B
    explains <- B >= r * (1 - 1e-9)
  }
  list(bias_factor = B, estimate = B, rr_ud = RR_UD, rr_uy = RR_UY, rr_obs = RR_obs, rr_bound = bound,
       explains_away = explains, method = "Ding-VanderWeele bounding factor B = RR_UD RR_UY / (RR_UD + RR_UY - 1)")
}

#' @rdname causal_e_value
#' @export
causal_mediation_baron_kenny <- function(X, M, Y) morie_baron_kenny(Y, X, M)

#' @rdname causal_e_value
#' @export
mediation_analysis <- function(Y, T, M, X = NULL) {
  y <- as.numeric(Y)
  t <- as.numeric(T)
  m <- as.numeric(M)
  if (!is.null(X)) {
    D <- cbind(1, as.matrix(X))
    res <- function(v) as.vector(v - D %*% qr.solve(D, v))
    y <- res(y)
    t <- res(t)
    m <- res(m)
  }
  morie_baron_kenny(y, t, m)
}

#' @rdname causal_e_value
#' @export
frontdoor_adjustment <- function(x, z, y, at = NULL) {
  n <- length(x)
  if (length(z) != n || length(y) != n) stop("x, z and y must share a length")
  xs <- sort(unique(x))
  zs <- sort(unique(z))
  ys <- sort(unique(y))
  px <- vapply(xs, function(v) mean(x == v), 0)
  targets <- if (is.null(at)) xs else at
  incomplete <- list()
  dist <- list()
  for (t0 in targets) {
    if (!any(x == t0)) stop("at value does not occur in x")
    acc <- stats::setNames(numeric(length(ys)), ys)
    for (zv in zs) {
      pz <- mean(z[x == t0] == zv)
      if (pz == 0) next
      for (k in seq_along(ys)) {
        inner <- 0
        for (j in seq_along(xs)) {
          sel <- x == xs[j] & z == zv
          if (!any(sel)) {
            incomplete[[length(incomplete) + 1]] <- c(xs[j], zv)
            next
          }
          inner <- inner + mean(y[sel] == ys[k]) * px[j]
        }
        acc[k] <- acc[k] + pz * inner
      }
    }
    dist[[as.character(t0)]] <- acc
  }
  list(distribution = dist, incomplete_cells = unique(incomplete), n = n,
       method = "Front-door adjustment (Pearl 2009, Thm. 3.3.4), discrete")
}

#' @rdname causal_e_value
#' @export
front_door <- function(Y, X, M) frontdoor_adjustment(X, M, Y)

#' @rdname causal_e_value
#' @export
frontdoor_criterion <- function(dag, X, Y, Z) {
  g <- .cf_edges(dag)
  E <- g$E
  Zs <- as.character(Z)
  X <- as.character(X)
  Y <- as.character(Y)
  if (!all(c(X, Y, Zs) %in% g$nodes)) stop("X, Y and Z must be nodes of the graph")
  paths <- .cf_directed_paths(E, X, Y)
  unint <- vapply(Filter(function(p) !any(p[-c(1, length(p))] %in% Zs), paths), paste, "", collapse = " -> ")
  c1 <- length(paths) > 0 && length(unint) == 0
  c2 <- all(vapply(Zs, function(zz) .cf_backdoor(E, X, zz, character(0)), TRUE))
  c3 <- all(vapply(Zs, function(zz) .cf_backdoor(E, zz, Y, X), TRUE))
  ok <- c1 && c2 && c3
  reason <- if (ok) "Z satisfies the front-door criterion; use the front-door formula." else if (!c1) {
    if (length(unint)) paste("directed path(s) bypass Z:", paste(unint, collapse = "; ")) else
      "no directed X->Y path exists."
  } else if (!c2) "an unblocked back-door path runs from X to Z." else "X does not block every back-door path from Z to Y."
  list(satisfied = ok, cond1 = c1, cond2 = c2, cond3 = c3, unintercepted_paths = unint, reason = reason,
       method = "Front-door criterion (Pearl 2009, Def. 3.3.3)")
}

#' @rdname causal_e_value
#' @export
mediation_formula <- function(x, m, y, x1 = NULL, x0 = NULL) {
  y <- as.numeric(y)
  if (is.null(x1) || is.null(x0)) {
    lv <- sort(unique(x))
    cnt <- vapply(lv, function(v) sum(x == v), 0)
    o <- order(-cnt)
    if (is.null(x1)) x1 <- lv[o[1]]
    if (is.null(x0)) x0 <- lv[o[2]]
  }
  ml <- sort(unique(m))
  p1 <- vapply(ml, function(v) mean(m[x == x1] == v), 0)
  p0 <- vapply(ml, function(v) mean(m[x == x0] == v), 0)
  ey <- function(xv, mv) {
    s <- x == xv & m == mv
    if (any(s)) mean(y[s]) else NA_real_
  }
  e1 <- vapply(ml, function(v) ey(x1, v), 0)
  e0 <- vapply(ml, function(v) ey(x0, v), 0)
  nde <- sum((e1 - e0) * p0, na.rm = TRUE)
  nie <- sum(e1 * (p1 - p0), na.rm = TRUE)
  list(nde = nde, nie = nie, te = nde + nie, x1 = x1, x0 = x0, n = length(x),
       method = "Pearl mediation formula (discrete, empirical cell means)")
}
