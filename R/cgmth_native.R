# Conjugate gradient method for unconstrained optimization.
# Sources: Hestenes, M. R., & Stiefel, E. (1952). Methods of
# conjugate gradients for solving linear systems. Journal of
# Research of the National Bureau of Standards, 49(6), 409-436.
# Polak, E., & Ribiere, G. (1969). Note sur la convergence de
# methodes de directions conjuguees. Revue francaise d'informatique
# et de recherche operationnelle, Serie rouge, 3(R1), 35-43, eq. 3.20
# (the beta used below). Polyak, B. T. (1969). The conjugate gradient
# method in extremal problems. USSR Computational Mathematics and
# Mathematical Physics, 9(4), 94-112 (the same beta, independently).
# Shewchuk, J. R. (1994). An Introduction to the Conjugate Gradient
# Method Without the Agonizing Pain, CMU-CS-94-125, section 14.1 (for
# the max(beta, 0) safeguard). Nocedal, J., & Wright, S. J. (2006).
# Numerical Optimization, 2nd ed., Springer, section 3.1 (for the
# Armijo sufficient-decrease backtracking line search, c = 1e-4).

#' cgmth
#'
#' A step of the cgmth_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param f See Usage.
#' @param grad_f See Usage.
#' @param x0 Coerced to numeric by the body, with \code{as.numeric}.
#' @param tol Defaults to \code{1e-06}.
#' @param max_iter A count; the body uses it as \code{seq_len(...)}. Defaults to \code{1000L}.
#' @param full_output A flag; the body branches on it. Defaults to \code{FALSE}.
#' @return The value of \code{x}, as built in the body.
#' @export
cgmth <- function(f, grad_f, x0, tol = 1e-6, max_iter = 1000L,
                  full_output = FALSE) {
  x <- as.numeric(x0)
  g <- grad_f(x)
  d <- -g

  for (iteration in seq_len(max_iter)) {
    if (sqrt(sum(g * g)) < tol) {
      if (isTRUE(full_output))
        return(list(x_min = x,
                    info = list(iterations = iteration - 1L,
                                converged = TRUE,
                                final_value = f(x))))
      return(x)
    }

    # Armijo backtracking line search (Nocedal & Wright sec. 3.1);
    # c = 1e-4 is the conventional value.
    alpha <- 1.0
    c <- 1e-4
    for (kk in seq_len(20L)) {
      if (f(x + alpha * d) <= f(x) + c * alpha * sum(g * d))
        break
      alpha <- alpha * 0.5
    }

    x <- x + alpha * d
    g_new <- grad_f(x)

    # PRP beta: written with r = -gradient, signs cancel.
    beta <- sum(g_new * (g_new - g)) / (sum(g * g) + 1e-14)
    # max(beta, 0) is Shewchuk sec. 14.1, not PRP.
    if (beta < 0) beta <- 0

    d <- -g_new + beta * d
    g <- g_new
  }

  if (isTRUE(full_output))
    return(list(x_min = x,
                info = list(iterations = max_iter,
                            converged = FALSE,
                            final_value = f(x))))
  x
}

morie_cgmth <- cgmth
