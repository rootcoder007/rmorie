# Sources:
#   Fletcher, R., & Reeves, C. M. (1964) "Function minimization by conjugate
#   gradients", The Computer Journal 7(2), 149-154.
#   Polak, E., & Ribiere, G. (1969) "Note sur la convergence de methodes de
#   directions conjuguees", Revue francaise d'informatique et de recherche
#   operationnelle, Serie rouge 3(R1), 35-43.
#   Shewchuk, J. R. (1994) "An Introduction to the Conjugate Gradient Method
#   Without the Agonizing Pain", Edition 1 1/4, CMU-CS-94-125.

.cgnonl_BETA_RULES <- c("fletcher-reeves", "polak-ribiere", "polak-ribiere-plus")
.cgnonl_SEARCHES <- c("fletcher-reeves", "exact-quadratic")

#' .cgnonl_dot
#'
#' Part of the cgnonl_native implementation; see the file header for the
#' source it follows.
#'
#' @param a See Usage.
#' @param b See Usage.
#' @return A numeric value.
#' @export
.cgnonl_dot <- function(a, b) sum(a * b)

#' beta_fletcher_reeves
#'
#' Part of the cgnonl_native implementation; see the file header for the
#' source it follows.
#'
#' @param g_new See Usage.
#' @param g_old See Usage.
#' @return A numeric value.
#' @export
beta_fletcher_reeves <- function(g_new, g_old) {
  den <- .cgnonl_dot(g_old, g_old)
  if (den <= 0.0) return(0.0)
  .cgnonl_dot(g_new, g_new) / den
}

#' beta_polak_ribiere
#'
#' Part of the cgnonl_native implementation; see the file header for the
#' source it follows.
#'
#' @param g_new See Usage.
#' @param g_old See Usage.
#' @param plus Defaults to \code{FALSE}.
#' @return One of two values, depending on the branch taken.
#' @export
beta_polak_ribiere <- function(g_new, g_old, plus = FALSE) {
  den <- .cgnonl_dot(g_old, g_old)
  if (den <= 0.0) return(0.0)
  num <- sum(g_new * (g_new - g_old))
  b <- num / den
  if (isTRUE(plus)) max(b, 0.0) else b
}

#' .cgnonl_beta
#'
#' Part of the cgnonl_native implementation; see the file header for the
#' source it follows.
#'
#' @param rule See Usage.
#' @param g_new See Usage.
#' @param g_old See Usage.
#' @return Nothing; this branch always raises.
#' @export
.cgnonl_beta <- function(rule, g_new, g_old) {
  if (rule == "fletcher-reeves") return(beta_fletcher_reeves(g_new, g_old))
  if (rule == "polak-ribiere") return(beta_polak_ribiere(g_new, g_old, plus = FALSE))
  if (rule == "polak-ribiere-plus") return(beta_polak_ribiere(g_new, g_old, plus = TRUE))
  stop(sprintf("cgnonl: beta must be one of %s",
               paste(.cgnonl_BETA_RULES, collapse = ", ")))
}

#' cubic_interpolate
#'
#' Part of the cgnonl_native implementation; see the file header for the
#' source it follows.
#'
#' @param ta See Usage.
#' @param fa See Usage.
#' @param da See Usage.
#' @param tb See Usage.
#' @param fb See Usage.
#' @param db See Usage.
#' @return The value of \code{t}, as built in the body.
#' @export
cubic_interpolate <- function(ta, fa, da, tb, fb, db) {
  h <- tb - ta
  if (h == 0.0) return(ta)
  z <- 3.0 * (fa - fb) / h + da + db
  disc <- z * z - da * db
  if (disc < 0.0) return(0.5 * (ta + tb))
  w <- sqrt(disc)
  denom <- db - da + 2.0 * w
  if (denom == 0.0) return(0.5 * (ta + tb))
  t <- tb - h * (db + w - z) / denom
  if (!(min(ta, tb) <= t && t <= max(ta, tb))) return(0.5 * (ta + tb))
  t
}

#' line_search_fr
#'
#' Part of the cgnonl_native implementation; see the file header for the
#' source it follows.
#'
#' @param f See Usage.
#' @param grad See Usage.
#' @param x See Usage.
#' @param p See Usage.
#' @param f0 See Usage.
#' @param g0 See Usage.
#' @param est Defaults to \code{NULL}.
#' @param max_double Defaults to \code{60L}.
#' @param max_cubic Defaults to \code{40L}.
#' @param tol Defaults to \code{1e-12}.
#' @return A list with \code{t}, \code{x_new}, \code{f_new}, \code{g_new}, \code{n_eval}.
#' @export
line_search_fr <- function(f, grad, x, p, f0, g0, est = NULL,
                           max_double = 60L, max_cubic = 40L,
                           tol = 1e-12) {
  n <- length(x)
  slope0 <- .cgnonl_dot(p, g0)
  if (slope0 >= 0.0)
    stop(sprintf("cgnonl: the search direction is not a descent direction (p'g = %g >= 0)", slope0))
  pnorm <- sqrt(.cgnonl_dot(p, p))
  if (pnorm <= 0.0)
    stop("cgnonl: the search direction is zero")
  unit <- 1.0 / pnorm
  if (is.null(est)) {
    h <- unit
  } else {
    k <- 2.0 * (as.numeric(est) - f0) / slope0
    h <- if (0.0 < k && k < unit) k else unit
  }
  psi <- function(t) {
    xt <- x + t * p
    list(xt = xt, ft = f(xt), gt = grad(xt))
  }
  evals <- 0L
  ta <- 0.0; fa <- f0; da <- slope0
  t <- h
  tb <- fb <- db <- NULL
  xb <- gb <- NULL
  for (i in seq_len(max_double)) {
    r <- psi(t)
    evals <- evals + 1L
    dt <- .cgnonl_dot(p, r$gt)
    if (dt >= 0.0 || r$ft > fa) {
      tb <- t; fb <- r$ft; db <- dt
      xb <- r$xt; gb <- r$gt
      break
    }
    ta <- t; fa <- r$ft; da <- dt
    t <- t * 2.0
  }
  if (is.null(tb)) {
    r <- psi(ta)
    return(list(t = ta, x_new = r$xt, f_new = r$ft, g_new = r$gt, n_eval = evals + 1L))
  }
  best_t <- tb; best_x <- xb; best_f <- fb; best_g <- gb
  if (fa < fb) {
    r2 <- psi(ta)
    evals <- evals + 1L
    best_t <- ta; best_x <- r2$xt; best_f <- r2$ft; best_g <- r2$gt
  }
  for (i in seq_len(max_cubic)) {
    if (abs(tb - ta) <= tol * max(1.0, abs(tb))) break
    tc <- cubic_interpolate(ta, fa, da, tb, fb, db)
    rc <- psi(tc)
    evals <- evals + 1L
    dc <- .cgnonl_dot(p, rc$gt)
    if (rc$ft < best_f) {
      best_t <- tc; best_x <- rc$xt; best_f <- rc$ft; best_g <- rc$gt
    }
    if (abs(dc) <= tol * max(1.0, abs(slope0)))
      return(list(t = tc, x_new = rc$xt, f_new = rc$ft, g_new = rc$gt, n_eval = evals))
    if (dc < 0.0) {
      ta <- tc; fa <- rc$ft; da <- dc
    } else {
      tb <- tc; fb <- rc$ft; db <- dc
    }
  }
  list(t = best_t, x_new = best_x, f_new = best_f, g_new = best_g, n_eval = evals)
}

#' .cgnonl_exact_quadratic_step
#'
#' Part of the cgnonl_native implementation; see the file header for the
#' source it follows.
#'
#' @param x See Usage.
#' @param p See Usage.
#' @param g See Usage.
#' @param hess_vec See Usage.
#' @return A numeric value.
#' @export
.cgnonl_exact_quadratic_step <- function(x, p, g, hess_vec) {
  ap <- hess_vec(p)
  den <- .cgnonl_dot(p, ap)
  if (den <= 0.0)
    stop(sprintf("cgnonl: p'Ap = %g is not positive; the exact quadratic step needs a positive definite A", den))
  -.cgnonl_dot(p, g) / den
}

#' nonlinear_cg
#'
#' Part of the cgnonl_native implementation; see the file header for the
#' source it follows.
#'
#' @param f See Usage.
#' @param grad See Usage.
#' @param x0 See Usage.
#' @param beta Defaults to \code{"fletcher-reeves"}.
#' @param restart Defaults to \code{NULL}.
#' @param max_iter Defaults to \code{NULL}.
#' @param tol Defaults to \code{1e-10}.
#' @param est Defaults to \code{NULL}.
#' @param line_search Defaults to \code{"fletcher-reeves"}.
#' @param hess_vec Defaults to \code{NULL}.
#' @param keep_path Defaults to \code{FALSE}.
#' @return A list with \code{x}, \code{fun}, \code{grad}, \code{gnorm}, \code{n_iter}, \code{n_restart}, \code{n_feval}, \code{converged}, \code{betas}, \code{path}, \code{beta_rule}, \code{line_search}, \code{restart_every}, \code{method}, \code{note}.
#' @export
nonlinear_cg <- function(f, grad, x0, beta = "fletcher-reeves",
                         restart = NULL, max_iter = NULL, tol = 1e-10,
                         est = NULL, line_search = "fletcher-reeves",
                         hess_vec = NULL, keep_path = FALSE) {
  if (!(beta %in% .cgnonl_BETA_RULES))
    stop(sprintf("cgnonl: beta must be one of %s",
                 paste(.cgnonl_BETA_RULES, collapse = ", ")))
  if (!(line_search %in% .cgnonl_SEARCHES))
    stop(sprintf("cgnonl: line_search must be one of %s",
                 paste(.cgnonl_SEARCHES, collapse = ", ")))
  if (line_search == "exact-quadratic" && is.null(hess_vec))
    stop("cgnonl: line_search='exact-quadratic' needs hess_vec, the map p -> Ap")
  x <- as.numeric(x0)
  n <- length(x)
  if (n == 0L) stop("cgnonl: x0 is empty")
  if (is.null(restart)) restart <- n + 1L
  restart <- as.integer(restart)
  if (restart < 0L) stop("cgnonl: restart must not be negative")
  if (is.null(max_iter)) max_iter <- 200L * n
  max_iter <- as.integer(max_iter)
  if (max_iter < 1L) stop("cgnonl: max_iter must be at least 1")

  g <- as.numeric(grad(x))
  fx <- as.numeric(f(x))
  p <- -g
  evals <- 1L
  betas <- numeric(0)
  path <- if (isTRUE(keep_path)) list(as.numeric(x)) else list()
  restarts <- 0L
  it <- 0L
  converged <- .cgnonl_dot(g, g) <= tol * tol

  while (!converged && it < max_iter) {
    it <- it + 1L
    if (line_search == "exact-quadratic") {
      t <- .cgnonl_exact_quadratic_step(x, p, g, hess_vec)
      x_new <- x + t * p
      f_new <- as.numeric(f(x_new))
      g_new <- as.numeric(grad(x_new))
      evals <- evals + 1L
    } else {
      r <- line_search_fr(f, grad, x, p, fx, g, est = est)
      t <- r$t; x_new <- r$x_new; f_new <- r$f_new; g_new <- r$g_new
      evals <- evals + r$n_eval
    }
    g_old <- g
    x <- x_new; fx <- f_new; g <- g_new
    if (isTRUE(keep_path)) path[[length(path) + 1L]] <- as.numeric(x)
    if (.cgnonl_dot(g, g) <= tol * tol) { converged <- TRUE; break }
    if (restart != 0L && it %% restart == 0L) {
      p <- -g
      restarts <- restarts + 1L
      betas <- c(betas, 0.0)
    } else {
      b <- .cgnonl_beta(beta, g, g_old)
      betas <- c(betas, b)
      p <- -g + b * p
      if (.cgnonl_dot(p, g) >= 0.0) {
        p <- -g
        restarts <- restarts + 1L
      }
    }
  }

  list(
    x = x,
    fun = fx,
    grad = g,
    gnorm = sqrt(.cgnonl_dot(g, g)),
    n_iter = it,
    n_restart = restarts,
    n_feval = evals,
    converged = converged,
    betas = betas,
    path = path,
    beta_rule = beta,
    line_search = line_search,
    restart_every = restart,
    method = "Fletcher & Reeves (1964) eq. 20, nonlinear conjugate gradients",
    note = paste0("storage is three vectors -- x, g and p -- which is the ",
                  "paper's stated advantage over Davidon-Fletcher-Powell; ",
                  "restarts to steepest descent every n+1 iterations, ",
                  "which preserves quadratic convergence because they are ",
                  "no more frequent than every n")
  )
}

cgnonl <- nonlinear_cg

#' morie_cgnonl
#'
#' Part of the cgnonl_native implementation; see the file header for the
#' source it follows.
#'
#' @param f See Usage.
#' @param grad See Usage.
#' @param x0 See Usage.
#' @param beta Defaults to \code{"fletcher-reeves"}.
#' @param restart Defaults to \code{NULL}.
#' @param max_iter Defaults to \code{NULL}.
#' @param tol Defaults to \code{1e-10}.
#' @param est Defaults to \code{NULL}.
#' @param line_search Defaults to \code{"fletcher-reeves"}.
#' @param hess_vec Defaults to \code{NULL}.
#' @param keep_path Defaults to \code{FALSE}.
#' @return The value of \code{nonlinear_cg}.
#' @export
morie_cgnonl <- function(f, grad, x0, beta = "fletcher-reeves",
                         restart = NULL, max_iter = NULL, tol = 1e-10,
                         est = NULL, line_search = "fletcher-reeves",
                         hess_vec = NULL, keep_path = FALSE) {
  nonlinear_cg(f, grad, x0, beta, restart, max_iter, tol, est,
               line_search, hess_vec, keep_path)
}
