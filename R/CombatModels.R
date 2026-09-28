#' Lanchester attrition models
#'
#' \code{LanchesterSquareOutcome}: closed-form square-law winner, survivors
#' and duration. \code{LanchesterSimulate}: fourth-order Runge-Kutta
#' integration of the square, linear, mixed (Deitchman guerrilla) and
#' logarithmic laws with constant reinforcements, stopping when a side is
#' annihilated. \code{LanchesterFit}: Engel (1954) through-origin estimates or
#' the generalised power law by log-linear least squares. Identical to the
#' Python arm \code{morie.fn.combatmod}.
#'
#' @param x0,y0 Initial forces.
#' @param a,b Attrition coefficients (a: Y's effectiveness against X).
#' @param law Attrition law.
#' @param t_end,dt Horizon and step.
#' @param reinforce_x,reinforce_y Reinforcement rates (simulation) or per-period
#'   reinforcements (fit).
#' @param x,y Force histories.
#' @return List.
#' @references Lanchester, F. W. (1916). Aircraft in Warfare: The Dawn of the
#'   Fourth Arm. Constable.
#'
#'   Engel, J. H. (1954). A verification of Lanchester's law. Journal of the
#'   Operations Research Society of America 2, 163-171.
#'
#'   Deitchman, S. J. (1962). A Lanchester model of guerrilla warfare.
#'   Operations Research 10, 818-827.
#' @examples
#' LanchesterSquareOutcome(100, 60, 0.01, 0.01)$survivors
#' tail(LanchesterSimulate(100, 60, 0.01, 0.01, t_end = 200)$x, 1)
#' @export
LanchesterSquareOutcome <- function(x0, y0, a, b) {
  g <- sqrt(a * b)
  fx <- b * x0^2
  fy <- a * y0^2
  if (fx > fy) {
    w <- "x"
    surv <- sqrt(x0^2 - (a / b) * y0^2)
    t <- atanh(y0 * sqrt(a / b) / x0) / g
  } else if (fy > fx) {
    w <- "y"
    surv <- sqrt(y0^2 - (b / a) * x0^2)
    t <- atanh(x0 * sqrt(b / a) / y0) / g
  } else {
    w <- "draw"
    surv <- 0
    t <- Inf
  }
  list(winner = w, survivors = surv, duration = t, fighting_strength_x = fx, fighting_strength_y = fy)
}

.lc_deriv <- function(law, x, y, a, b, p, q) {
  switch(law,
    square = c(-a * y + p, -b * x + q),
    linear = c(-a * x * y + p, -b * x * y + q),
    mixed = c(-a * x * y + p, -b * x + q),
    logarithmic = c(-a * x + p, -b * y + q),
    stop("law must be square, linear, mixed or logarithmic")
  )
}

#' @rdname LanchesterSquareOutcome
#' @export
LanchesterSimulate <- function(x0, y0, a, b, law = "square", t_end = 100, dt = 0.01, reinforce_x = 0,
                               reinforce_y = 0) {
  x <- x0
  y <- y0
  t <- 0
  T <- t
  X <- x
  Y <- y
  ended <- "time"
  for (i in seq_len(round(t_end / dt))) {
    k1 <- .lc_deriv(law, x, y, a, b, reinforce_x, reinforce_y)
    k2 <- .lc_deriv(law, x + dt / 2 * k1[1], y + dt / 2 * k1[2], a, b, reinforce_x, reinforce_y)
    k3 <- .lc_deriv(law, x + dt / 2 * k2[1], y + dt / 2 * k2[2], a, b, reinforce_x, reinforce_y)
    k4 <- .lc_deriv(law, x + dt * k3[1], y + dt * k3[2], a, b, reinforce_x, reinforce_y)
    xn <- x + dt / 6 * (k1[1] + 2 * k2[1] + 2 * k3[1] + k4[1])
    yn <- y + dt / 6 * (k1[2] + 2 * k2[2] + 2 * k3[2] + k4[2])
    if (xn <= 0 || yn <= 0) {
      fx <- if (xn <= 0) x / (x - xn) else Inf
      fy <- if (yn <= 0) y / (y - yn) else Inf
      f <- min(fx, fy)
      t <- t + f * dt
      x2 <- x + f * (xn - x)
      y2 <- y + f * (yn - y)
      x <- x2
      y <- y2
      if (fx <= fy) {
        x <- 0
        ended <- "x annihilated"
      }
      if (fy <= fx) {
        y <- 0
        ended <- if (ended == "time") "y annihilated" else "both annihilated"
      }
      T <- c(T, t)
      X <- c(X, x)
      Y <- c(Y, y)
      break
    }
    x <- xn
    y <- yn
    t <- t + dt
    T <- c(T, t)
    X <- c(X, x)
    Y <- c(Y, y)
  }
  list(t = T, x = X, y = Y, ended_by = ended)
}

#' @rdname LanchesterSquareOutcome
#' @export
LanchesterFit <- function(x, y, dt = 1, law = "square", reinforce_x = NULL, reinforce_y = NULL) {
  n <- length(x) - 1
  P <- if (is.null(reinforce_x)) numeric(n) else reinforce_x
  Q <- if (is.null(reinforce_y)) numeric(n) else reinforce_y
  Lx <- x[1:n] - x[2:(n + 1)] + P
  Ly <- y[1:n] - y[2:(n + 1)] + Q
  r2 <- function(o, f) {
    tss <- sum((o - mean(o))^2)
    if (tss > 0) 1 - sum((o - f)^2) / tss else NaN
  }
  if (law == "power") {
    fitside <- function(L, own, opp) {
      k <- L > 0
      cf <- stats::lm.fit(cbind(1, log(opp[1:n][k]), log(own[1:n][k])), log(L[k] / dt))$coefficients
      list(a = exp(unname(cf[1])), p = unname(cf[2]), q = unname(cf[3]))
    }
    sx <- fitside(Lx, x, y)
    sy <- fitside(Ly, y, x)
    fx <- sx$a * dt * y[1:n]^sx$p * x[1:n]^sx$q
    fy <- sy$a * dt * x[1:n]^sy$p * y[1:n]^sy$q
    return(list(x = sx, y = sy, fitted_x = fx, fitted_y = fy, r2_x = r2(Lx, fx), r2_y = r2(Ly, fy)))
  }
  g <- switch(law,
    square = list(y[1:n], x[1:n]),
    linear = list(x[1:n] * y[1:n], x[1:n] * y[1:n]),
    logarithmic = list(x[1:n], y[1:n]),
    stop("law must be square, linear, logarithmic or power")
  )
  a <- sum(Lx) / (dt * sum(g[[1]]))
  b <- sum(Ly) / (dt * sum(g[[2]]))
  fx <- a * dt * g[[1]]
  fy <- b * dt * g[[2]]
  list(a = a, b = b, fitted_x = fx, fitted_y = fy, r2_x = r2(Lx, fx), r2_y = r2(Ly, fy))
}

#' Salvo model and Colonel Blotto games
#'
#' \code{SalvoExchange}: Hughes (1995) salvo exchanges with offensive,
#' defensive and staying power. \code{BlottoPayoff}: battlefield-by-battlefield
#' payoff (ties split). \code{BlottoBestResponse}: exhaustive best response to
#' a known pure or mixed opponent strategy.
#'
#' @param A,B Initial units.
#' @param alpha,beta Offensive power per unit.
#' @param a1,b1 Staying power.
#' @param a3,b3 Defensive power.
#' @param salvos Number of salvo exchanges.
#' @param alloc_a,alloc_b Allocations.
#' @param values Battlefield values.
#' @param opponent Allocation, or list of \code{list(probability, allocation)}.
#' @param troops Units to allocate.
#' @return List.
#' @references Hughes, W. P. (1995). A salvo model of warships in missile
#'   combat used to evaluate their staying power. Naval Research Logistics 42,
#'   267-289.
#'
#'   Roberson, B. (2006). The Colonel Blotto game. Economic Theory 29, 1-24.
#' @examples
#' SalvoExchange(10, 8, alpha = 2, beta = 2, a1 = 2, b1 = 2, a3 = 1, b3 = 1)$B
#' BlottoBestResponse(c(2, 2, 2), 6)$allocation
#' @export
SalvoExchange <- function(A, B, alpha, beta, a1, b1, a3 = 0, b3 = 0, salvos = 1) {
  As <- A
  Bs <- B
  fer <- NaN
  for (s in seq_len(salvos)) {
    a_ <- As[length(As)]
    b_ <- Bs[length(Bs)]
    dB <- min(b_, max(0, (alpha * a_ - b3 * b_) / b1))
    dA <- min(a_, max(0, (beta * b_ - a3 * a_) / a1))
    if (s == 1 && dA > 0 && a_ > 0 && b_ > 0) fer <- (dB / b_) / (dA / a_)
    As <- c(As, a_ - dA)
    Bs <- c(Bs, b_ - dB)
  }
  list(A = As, B = Bs, fractional_exchange_ratio = fer)
}

#' @rdname SalvoExchange
#' @export
BlottoPayoff <- function(alloc_a, alloc_b, values = NULL) {
  v <- if (is.null(values)) rep(1, length(alloc_a)) else values
  wa <- sum(ifelse(alloc_a > alloc_b, v, ifelse(alloc_a == alloc_b, v / 2, 0)))
  wb <- sum(ifelse(alloc_b > alloc_a, v, ifelse(alloc_a == alloc_b, v / 2, 0)))
  list(value_a = wa, value_b = wb, payoff = wa - wb)
}

#' @rdname SalvoExchange
#' @export
BlottoBestResponse <- function(opponent, troops, values = NULL) {
  opp <- if (is.list(opponent)) opponent else list(list(1, opponent))
  k <- length(opp[[1]][[2]])
  cuts <- utils::combn(0:(troops + k - 2), k - 1)
  best <- -Inf
  arg <- NULL
  for (j in seq_len(ncol(cuts))) {
    alloc <- diff(c(-1, cuts[, j], troops + k - 1)) - 1
    e <- sum(vapply(opp, function(o) o[[1]] * BlottoPayoff(alloc, o[[2]], values)$payoff, 0))
    if (e > best + 1e-12) {
      best <- e
      arg <- alloc
    }
  }
  list(allocation = arg, expected_payoff = best)
}
