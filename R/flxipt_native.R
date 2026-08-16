# flxipt_native.R
# Super Learner and IPTW with the propensity score it produces.
# van der Laan, Polley & Hubbard (2007) "Super Learner", SAGMB 6(1),
# art. 25, doi:10.2202/1544-6115.1309.  Pirracchio, Petersen & van der
# Laan (2015) "Improving propensity score estimators' robustness to
# model misspecification using super learner", AJE 181(2), 108-119,
# doi:10.1093/aje/kwu253.
#
# Z[i, j] = candidate j's held-out prediction for i; meta-learner is
# fit on Z; refit candidates on all data and combine.  IPTW: A/g + (1-A)/(1-g).

.flxipt_EPS <- 1e-9
.FLXIPT_METAS <- c("nnls", "discrete", "ols")

# --- design expansion per learner kind ------------------------------
#' Design expansion per learner kind ------------------------------
#'
#' A step of the flxipt_native implementation. Called by \code{.flxipt_fit}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param W A matrix; indexed by row and column.
#' @param spec A list; the body reads \code{$cols}, \code{$kind} from it.
#' @return The value of \code{out}, as built in the body.
#' @export
.flxipt_expand <- function(W, spec) {
  W <- as.matrix(W); storage.mode(W) <- "double"
  n <- nrow(W)
  if (n == 0L) return(matrix(0, nrow = 0, ncol = 1))
  p <- ncol(W)
  out <- matrix(1, nrow = n, ncol = 1)
  kind <- spec$kind
  if (kind == "intercept") {
    # nothing
  } else if (kind == "main") {
    out <- cbind(out, W)
  } else if (kind == "quadratic") {
    out <- cbind(out, W, W^2)
  } else if (kind == "interaction") {
    inter <- matrix(0, nrow = n, ncol = p * (p - 1L) / 2L)
    if (ncol(inter) > 0L) {
      k <- 0L
      for (a in seq_len(p)) for (b in (a + 1L):p) {
        k <- k + 1L
        inter[, k] <- W[, a] * W[, b]
      }
    }
    out <- cbind(out, W, inter)
  } else if (kind == "subset") {
    cols <- spec$cols + 1L
    out <- cbind(out, W[, cols, drop = FALSE])
  } else {
    stop(sprintf("flxipt: unknown learner kind '%s'", kind))
  }
  out
}

# --- default library, in the shape Pirracchio et al. use ------------
#' Default library, in the shape Pirracchio et al. use ------------
#'
#' A step of the flxipt_native implementation. Called by \code{super_learner}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param p See Usage.
#' @param ridge_penalties Defaults to \code{c(0, 1, 10)}.
#' @return The value of \code{lib}, as built in the body.
#' @export
default_learners <- function(p, ridge_penalties = c(0, 1, 10)) {
  lib <- list(
    list(name = "intercept", kind = "intercept", penalty = 0),
    list(name = "main", kind = "main", penalty = 0),
    list(name = "quadratic", kind = "quadratic", penalty = 0)
  )
  if (p >= 2L) {
    lib[[length(lib) + 1L]] <- list(name = "interaction", kind = "interaction",
                                    penalty = 0)
    for (pen in ridge_penalties) {
      if (pen > 0) {
        lib[[length(lib) + 1L]] <- list(
          name = sprintf("interaction+ridge%g", pen),
          kind = "interaction", penalty = pen
        )
      }
    }
  }
  lib
}

# --- logistic IRLS, internal: returns coef vector --------------------
#' Logistic IRLS, internal: returns coef vector --------------------
#'
#' A step of the flxipt_native implementation. Called by \code{.flxipt_fit}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param X A matrix; passed to \code{nrow}.
#' @param y Numeric; combined arithmetically in the body.
#' @param ridge Numeric; combined arithmetically in the body. Defaults to \code{1e-10}.
#' @param penalty Numeric; combined arithmetically in the body. Defaults to \code{0}.
#' @param max_iter A count; the body uses it as \code{seq_len(...)}. Defaults to \code{200}.
#' @param tol Defaults to \code{1e-08}.
#' @return The value of \code{b}, as built in the body.
#' @export
.flxipt_logit_irls <- function(X, y, ridge = 1e-10, penalty = 0,
                               max_iter = 200, tol = 1e-8) {
  X <- as.matrix(X); y <- as.numeric(y)
  n <- nrow(X); m <- ncol(X)
  b <- rep(0, m)
  for (it in seq_len(max_iter)) {
    eta <- as.numeric(X %*% b)
    eta <- pmin(pmax(eta, -30), 30)
    p <- 1 / (1 + exp(-eta))
    W <- p * (1 - p)
    W <- pmax(W, 1e-12)
    z <- eta + (y - p) / W
    XtW <- t(X) * W
    A <- XtW %*% X + diag(ridge + penalty, m, m)
    b_new <- tryCatch(as.numeric(solve(A, XtW %*% z)),
                      error = function(e) b)
    if (max(abs(b_new - b)) < tol) { b <- b_new; break }
    b <- b_new
  }
  b
}

# --- internal lstsq with optional ridge ----------------------------
#' Internal lstsq with optional ridge ----------------------------
#'
#' A step of the flxipt_native implementation. Called by \code{.flxipt_fit}, \code{super_learner}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param X A matrix; passed to \code{ncol}.
#' @param y A matrix; passed to \code{\%*\%}.
#' @param ridge A matrix; passed to \code{diag}. Defaults to \code{1e-10}.
#' @return A vector, from \code{as.numeric}.
#' @export
.flxipt_lstsq <- function(X, y, ridge = 1e-10) {
  X <- as.matrix(X); y <- as.numeric(y)
  m <- ncol(X)
  A <- t(X) %*% X + diag(ridge, m, m)
  as.numeric(solve(A, t(X) %*% y))
}

# --- sigmoid --------------------------------------------------------
#' Sigmoid --------------------------------------------------------
#'
#' A step of the flxipt_native implementation. Called by \code{.flxipt_fit}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param v Numeric; combined arithmetically in the body.
#' @return A numeric value.
#' @export
.flxipt_sigmoid <- function(v) {
  v <- pmin(pmax(v, -30), 30)
  1 / (1 + exp(-v))
}

# --- train one candidate on `rows`; return predictor over all rows
#' Train one candidate on `rows`; return predictor over all rows
#'
#' A step of the flxipt_native implementation. Called by \code{super_learner}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y A vector; indexed elementwise.
#' @param W Passed to \code{.flxipt_expand}.
#' @param spec A list; the body reads \code{$penalty} from it.
#' @param rows See Usage.
#' @param binary A flag; the body branches on it.
#' @param ridge Numeric; passed to \code{max}.
#' @return A list with \code{pred}, \code{b}.
#' @export
.flxipt_fit <- function(y, W, spec, rows, binary, ridge) {
  X <- .flxipt_expand(W, spec)
  Xr <- X[rows, , drop = FALSE]
  yr <- y[rows]
  if (binary) {
    b <- .flxipt_logit_irls(Xr, yr, ridge = max(ridge, 1e-10),
                            penalty = if (is.null(spec$penalty)) 0
                                     else spec$penalty)
    pred <- .flxipt_sigmoid(as.numeric(X %*% b))
  } else {
    pen <- if (is.null(spec$penalty)) 0 else spec$penalty
    b <- .flxipt_lstsq(Xr, yr, max(ridge, 1e-10) + pen)
    pred <- as.numeric(X %*% b)
  }
  list(pred = pred, b = b)
}

# --- disjoint validation blocks -------------------------------------
#' Disjoint validation blocks -------------------------------------
#'
#' A step of the flxipt_native implementation. Called by \code{super_learner}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param n A count; the body uses it as \code{seq_len(...)}.
#' @param V Numeric; combined arithmetically in the body.
#' @return The value of \code{lapply}.
#' @export
.flxipt_folds <- function(n, V) {
  V <- max(2L, min(as.integer(V), n))
  lapply(0:(V - 1L), function(v) which(seq_len(n) %% V == v))
}

# --- projection onto {a >= 0, sum a = 1}, exact sort-based ---------
#' Projection onto {a >= 0, sum a = 1}, exact sort-based ---------
#'
#' A step of the flxipt_native implementation. Called by \code{.flxipt_nnls_simplex}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param v A vector; its length is taken.
#' @return The value of \code{pmax}.
#' @export
.flxipt_project_simplex <- function(v) {
  n <- length(v)
  if (n == 0L) return(numeric(0))
  u <- sort(v, decreasing = TRUE)
  css <- 0; rho <- 0; theta <- 0
  for (j in seq_len(n)) {
    css <- css + u[j]
    t <- (css - 1) / j
    if (u[j] - t > 0) { rho <- j; theta <- t }
  }
  pmax(v - theta, 0)
}

# --- nnls_simplex: convex combination minimising ||y - Z a||^2 ----
# Accelerated projected gradient with step from the largest
# eigenvalue of the Gram matrix (estimated by power iteration).
#' Nnls_simplex: convex combination minimising ||y - Z a||^2 ----
#'
#' Accelerated projected gradient with step from the largest eigenvalue
#' of the Gram matrix (estimated by power iteration).
#'
#' @param Z A matrix; passed to \code{nrow}.
#' @param y A matrix; passed to \code{crossprod}.
#' @param iters A count; the body uses it as \code{seq_len(...)}. Defaults to \code{8000}.
#' @param tol Defaults to \code{1e-14}.
#' @return One of two values, depending on the branch taken.
#' @export
.flxipt_nnls_simplex <- function(Z, y, iters = 8000, tol = 1e-14) {
  n <- nrow(Z); J <- ncol(Z)
  if (J == 0L) return(numeric(0))
  if (J == 1L) return(1)
  G <- crossprod(Z) / n
  c <- as.numeric(crossprod(Z, y)) / n

  # power iteration for the largest eigenvalue of G
  v <- rep(1 / sqrt(J), J)
  lam <- 0
  for (iter in seq_len(200L)) {
    u <- as.numeric(G %*% v)
    nrm <- sqrt(sum(u^2))
    if (nrm <= 0) break
    v <- u / nrm
    if (abs(nrm - lam) < 1e-13 * max(nrm, 1)) { lam <- nrm; break }
    lam <- nrm
  }
  step <- if (lam > 0) 1 / lam else 1

  obj <- function(a) {
    as.numeric(t(a) %*% (G %*% a) - 2 * sum(c * a))
  }
  a <- rep(1 / J, J)
  z <- a
  tk <- 1
  for (iter in seq_len(iters)) {
    grad <- as.numeric(G %*% z) - c
    nxt <- .flxipt_project_simplex(z - step * grad)
    tn <- 0.5 * (1 + sqrt(1 + 4 * tk * tk))
    mom <- (tk - 1) / tn
    z <- nxt + mom * (nxt - a)
    shift <- max(abs(nxt - a))
    a <- nxt; tk <- tn
    if (shift < tol) break
  }

  best_vertex <- which.min(diag(G) - 2 * c)
  vert <- if (seq_len(J) == best_vertex) 1 else 0
  if (obj(a) <= obj(vert)) a else vert
}

# --- cross-validated risk per column of Z ---------------------------
#' Cross-validated risk per column of Z ---------------------------
#'
#' A step of the flxipt_native implementation. Called by \code{morie_tmlcic_adaptive_prespecification}, \code{super_learner}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y Numeric; combined arithmetically in the body.
#' @param Z A matrix; indexed by row and column.
#' @param loss One of \code{"l2"}, \code{"nll"}. Defaults to \code{"l2"}.
#' @return The value of \code{out}, as built in the body.
#' @export
cv_risk <- function(y, Z, loss = "l2") {
  y <- as.numeric(y); Z <- as.matrix(Z)
  J <- ncol(Z)
  out <- numeric(J)
  if (loss == "l2") {
    for (j in seq_len(J)) out[j] <- mean((y - Z[, j])^2)
  } else if (loss == "nll") {
    for (j in seq_len(J)) {
      p <- pmin(pmax(Z[, j], .flxipt_EPS), 1 - .flxipt_EPS)
      out[j] <- -mean(y * log(p) + (1 - y) * log(1 - p))
    }
  } else {
    stop(sprintf("flxipt: loss must be l2 or nll, got '%s'", loss))
  }
  out
}

# --- the Super Learner ---------------------------------------------
#' The Super Learner ---------------------------------------------
#'
#' A step of the flxipt_native implementation. Called by \code{flexible_iptw}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y See Usage.
#' @param X Optional; may be \code{NULL}. A matrix; passed to \code{as.matrix}.
#' @param library Defaults to \code{NULL}.
#' @param n_folds Passed to \code{.flxipt_folds}. Defaults to \code{10}.
#' @param meta One of \code{"discrete"}, \code{"nnls"}. Defaults to \code{"nnls"}.
#' @param binary Optional; may be \code{NULL}. A flag; the body branches on it.
#' @param loss Compared against \code{"nll"}. Defaults to \code{"l2"}.
#' @param ridge Numeric; passed to \code{max}. Defaults to \code{1e-08}.
#' @param honest_level_one A flag; the body branches on it. Defaults to \code{TRUE}.
#' @return A list with \code{fitted}, \code{estimate}, \code{weights}, \code{weight_vector}, \code{cv_risk}, \code{cv_risk_ensemble}, \code{best_candidate}, \code{best_candidate_risk}, \code{discrete_choice}, \code{level_one}, \code{candidate_fits}, \code{library}, \code{n}, \code{n_folds}, \code{meta}, \code{loss}, \code{binary}, \code{honest_level_one}, \code{method}.
#' @export
super_learner <- function(y, X, library = NULL, n_folds = 10,
                          meta = "nnls", binary = NULL, loss = "l2",
                          ridge = 1e-8, honest_level_one = TRUE) {
  if (!(meta %in% .FLXIPT_METAS)) {
    stop(sprintf("flxipt: meta must be one of %s, got '%s'",
                 paste(.FLXIPT_METAS, collapse = ", "), meta))
  }
  yv <- as.numeric(y)
  n <- length(yv)
  Wm <- if (is.null(X)) matrix(0, nrow = n, ncol = 0)
        else as.matrix(X)
  storage.mode(Wm) <- "double"
  if (nrow(Wm) != n) {
    stop(sprintf("flxipt: %d covariate rows for %d outcomes",
                 nrow(Wm), n))
  }
  if (n < 8L) {
    stop(sprintf("flxipt: need at least 8 observations, got %d", n))
  }
  p <- ncol(Wm)
  lib <- if (is.null(library)) default_learners(p) else library
  if (length(lib) == 0L) stop("flxipt: the library is empty")
  if (is.null(binary)) binary <- all(yv %in% c(0, 1))
  if (loss == "nll" && !binary) {
    stop("flxipt: the nll loss needs a binary outcome")
  }
  J <- length(lib)
  folds <- .flxipt_folds(n, n_folds)
  Z <- matrix(0, nrow = n, ncol = J)
  for (val in folds) {
    train <- setdiff(seq_len(n), val)
    if (length(train) == 0L) stop("flxipt: an empty training fold")
    src <- if (honest_level_one) train else seq_len(n)
    for (j in seq_along(lib)) {
      pred <- .flxipt_fit(yv, Wm, lib[[j]], src, binary, ridge)$pred
      Z[val, j] <- pred[val]
    }
  }
  risks <- cv_risk(yv, Z, loss)
  best <- which.min(risks)
  if (meta == "discrete") {
    weights <- if (seq_along(lib) == best) 1 else 0
  } else if (meta == "nnls") {
    weights <- .flxipt_nnls_simplex(Z, yv)
  } else {
    weights <- .flxipt_lstsq(Z, yv, max(ridge, 1e-10))
  }
  # refit candidates on all the data; only the combination came from
  # the held-out predictions
  full <- matrix(0, nrow = n, ncol = J)
  for (j in seq_along(lib)) {
    full[, j] <- .flxipt_fit(yv, Wm, lib[[j]], seq_len(n),
                              binary, ridge)$pred
  }
  fitted <- as.numeric(full %*% weights)
  if (binary) fitted <- pmin(pmax(fitted, 0), 1)
  ens <- as.numeric(Z %*% weights)
  if (binary) ens <- pmin(pmax(ens, .flxipt_EPS), 1 - .flxipt_EPS)
  ens_risk <- cv_risk(yv, matrix(ens, ncol = 1), loss)[1]
  list(
    fitted = fitted, estimate = ens_risk,
    weights = setNames(as.numeric(weights),
                       vapply(lib, function(s) s$name, character(1))),
    weight_vector = as.numeric(weights),
    cv_risk = setNames(as.numeric(risks),
                       vapply(lib, function(s) s$name, character(1))),
    cv_risk_ensemble = ens_risk,
    best_candidate = lib[[best]]$name,
    best_candidate_risk = risks[best],
    discrete_choice = lib[[best]]$name,
    level_one = Z, candidate_fits = full,
    library = vapply(lib, function(s) s$name, character(1)),
    n = n, n_folds = length(folds),
    meta = meta, loss = loss, binary = binary,
    honest_level_one = honest_level_one,
    method = paste0("Super Learner, van der Laan, Polley & Hubbard ",
                    "(2007) Sec. 2 eq. (1)")
  )
}

# --- IPTW: propensity by Super Learner, weights A/g + (1-A)/(1-g)
#' IPTW: propensity by Super Learner, weights A/g + (1-A)/(1-g)
#'
#' A step of the flxipt_native implementation. Called by \code{iptw_ate}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param A See Usage.
#' @param H See Usage.
#' @param library Defaults to \code{NULL}.
#' @param n_folds Defaults to \code{10}.
#' @param meta Defaults to \code{"nnls"}.
#' @param trim Defaults to \code{0.01}.
#' @param ridge Defaults to \code{1e-08}.
#' @param stabilize A flag; the body branches on it. Defaults to \code{FALSE}.
#' @return A list with \code{propensity}, \code{weights}, \code{estimate}, \code{sl_weights}, \code{cv_risk}, \code{cv_risk_ensemble}, \code{best_candidate}, \code{max_weight}, \code{min_propensity}, \code{max_propensity}, \code{n}, \code{trim}, \code{stabilized}, \code{library}, \code{method}.
#' @export
flexible_iptw <- function(A, H, library = NULL, n_folds = 10,
                          meta = "nnls", trim = 0.01, ridge = 1e-8,
                          stabilize = FALSE) {
  Av <- as.numeric(A); n <- length(Av)
  if (any(!(Av %in% c(0, 1)))) {
    stop("flxipt: the treatment must be binary 0/1")
  }
  if (!(sum(Av) > 0 && sum(Av) < n)) {
    stop("flxipt: both treatment arms must be non-empty")
  }
  t <- as.numeric(trim)
  if (t < 0 || t >= 0.5) {
    stop(sprintf("flxipt: trim must be in [0, 0.5), got %g", t))
  }
  sl <- super_learner(Av, H, library = library, n_folds = n_folds,
                      meta = meta, binary = TRUE, loss = "l2",
                      ridge = ridge)
  tcap <- max(t, .flxipt_EPS)
  g <- pmin(pmax(sl$fitted, tcap), 1 - tcap)
  if (isTRUE(stabilize)) {
    pa <- sum(Av) / n
    w <- Av * pa / g + (1 - Av) * (1 - pa) / (1 - g)
  } else {
    w <- Av / g + (1 - Av) / (1 - g)
  }
  list(
    propensity = g, weights = w, estimate = mean(w),
    sl_weights = sl$weights, cv_risk = sl$cv_risk,
    cv_risk_ensemble = sl$cv_risk_ensemble,
    best_candidate = sl$best_candidate,
    max_weight = max(w), min_propensity = min(g),
    max_propensity = max(g), n = n, trim = t,
    stabilized = isTRUE(stabilize), library = sl$library,
    method = paste0("IPTW with a Super Learner propensity score, ",
                    "Pirracchio, Petersen & van der Laan (2015) eq. (3)")
  )
}

# --- ATE by weighted regression of Y on A --------------------------
#' ATE by weighted regression of Y on A --------------------------
#'
#' A step of the flxipt_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param y See Usage.
#' @param A See Usage.
#' @param H See Usage.
#' @param library Defaults to \code{NULL}.
#' @param n_folds Defaults to \code{10}.
#' @param meta Defaults to \code{"nnls"}.
#' @param trim Defaults to \code{0.01}.
#' @param ridge Defaults to \code{1e-08}.
#' @param level Defaults to \code{0.95}.
#' @return A list with \code{estimate}, \code{se}, \code{ci}, \code{mean_treated}, \code{mean_control}, \code{propensity}, \code{weights}, \code{sl_weights}, \code{cv_risk}, \code{best_candidate}, \code{max_weight}, \code{min_propensity}, \code{n}, \code{level}, \code{method}.
#' @export
iptw_ate <- function(y, A, H, library = NULL, n_folds = 10,
                     meta = "nnls", trim = 0.01, ridge = 1e-8,
                     level = 0.95) {
  yv <- as.numeric(y); Av <- as.numeric(A)
  n <- length(yv)
  if (length(Av) != n) {
    stop(sprintf("flxipt: %d outcomes but %d treatments", n, length(Av)))
  }
  r <- flexible_iptw(Av, H, library = library, n_folds = n_folds,
                     meta = meta, trim = trim, ridge = ridge)
  w <- r$weights
  w1 <- sum(w[Av == 1])
  w0 <- sum(w[Av == 0])
  if (w1 <= 0 || w0 <= 0) stop("flxipt: an arm carries no weight")
  m1 <- sum(w[Av == 1] * yv[Av == 1]) / w1
  m0 <- sum(w[Av == 0] * yv[Av == 0]) / w0
  psi <- m1 - m0
  # influence-curve SE for the Hajek contrast
  ic <- numeric(n)
  ic[Av == 1] <- w[Av == 1] * (yv[Av == 1] - m1) * n / w1
  ic[Av == 0] <- -w[Av == 0] * (yv[Av == 0] - m0) * n / w0
  se <- sd(ic) / sqrt(n)
  z <- qnorm(0.5 + 0.5 * as.numeric(level))
  list(estimate = psi, se = se, ci = c(psi - z * se, psi + z * se),
       mean_treated = m1, mean_control = m0,
       propensity = r$propensity, weights = w,
       sl_weights = r$sl_weights, cv_risk = r$cv_risk,
       best_candidate = r$best_candidate,
       max_weight = r$max_weight,
       min_propensity = r$min_propensity, n = n, level = as.numeric(level),
       method = paste0("IPTW ATE with a Super Learner propensity ",
                       "score, Pirracchio, Petersen & van der Laan (2015)"))
}

# --- cheatsheet -----------------------------------------------------
#' Cheatsheet -----------------------------------------------------
#'
#' A step of the flxipt_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
.flxipt_cheatsheet <- function() {
  paste0("flxipt: Super Learner. Z[i,j] = candidate j's HELD-OUT ",
         "prediction for i; fit the meta-learner of y on Z (nnls ",
         "convex combination, or discrete = the CV selector); apply ",
         "it to the candidates refitted on all the data (vdL-Polley-",
         "Hubbard 2007 eq. 1). Then IPTW weights A/g + (1-A)/(1-g) ",
         "with g from the ensemble (Pirracchio 2015 eq. 3).")
}

# compact alias per ledger/NAMING.md
flexibleiptw <- flexible_iptw

# house entry point: the package exports one morie_<module>
morie_flxipt <- flexible_iptw
