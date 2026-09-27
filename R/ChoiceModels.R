.gh_rule <- function(n) {
  J <- matrix(0, n, n)
  for (i in seq_len(n - 1)) J[i, i + 1] <- J[i + 1, i] <- sqrt(i / 2)
  e <- eigen(J, symmetric = TRUE)
  list(nodes = e$values, weights = sqrt(pi) * e$vectors[1, ]^2)
}

#' Panel binary choice models
#'
#' Chamberlain's conditional (fixed-effects) logit, whose likelihood conditions on
#' each unit's number of ones (denominator by dynamic programming), and
#' random-effects logit or probit with a normal unit effect integrated by
#' Gauss-Hermite quadrature; maximised by BFGS with standard errors from the
#' numerical Hessian.
#'
#' @param y Binary outcomes.
#' @param X Covariate matrix (no intercept column).
#' @param group Unit labels.
#' @param model "fe_logit", "re_logit" or "re_probit".
#' @param n_quad Quadrature nodes.
#' @return list(coef, se, sigma, loglik, n_units, converged).
#' @references Chamberlain, G. (1980). Analysis of covariance with qualitative
#'   data. Review of Economic Studies 47, 225-238. Butler, J. S. and Moffitt, R.
#'   (1982). A computationally efficient quadrature procedure for the one-factor
#'   multinomial probit model. Econometrica 50, 761-764.
#' @examples
#' PanelBinaryChoice(c(0, 1, 1, 0, 0, 1), matrix(c(0, 1, 2, 0.5, 0.2, 1.5)), c(1, 1, 1, 2, 2, 2))$n_units
#' @export
PanelBinaryChoice <- function(y, X, group, model = c("fe_logit", "re_logit", "re_probit"), n_quad = 30) {
  model <- match.arg(model)
  Y <- as.integer(y)
  Xm <- as.matrix(X) * 1
  k <- ncol(Xm)
  units <- split(seq_along(Y), group)
  if (model == "fe_logit") {
    keep <- Filter(function(idx) sum(Y[idx]) > 0 && sum(Y[idx]) < length(idx), units)
    nll <- function(b) {
      tot <- 0
      for (idx in keep) {
        eta <- as.vector(Xm[idx, , drop = FALSE] %*% b)
        s <- sum(Y[idx])
        m <- max(eta)
        D <- c(1, numeric(s))
        for (e in eta) {
          w <- exp(e - m)
          for (j in min(s, length(D) - 1):1) D[j + 1] <- D[j + 1] + D[j] * w
        }
        tot <- tot - (sum(eta[Y[idx] == 1]) - s * m - log(D[s + 1]))
      }
      tot
    }
    start <- numeric(k)
    n_units <- length(keep)
  } else {
    gh <- .gh_rule(n_quad)
    Fc <- if (model == "re_logit") stats::plogis else stats::pnorm
    nll <- function(th) {
      b0 <- th[1]
      b <- th[2:(k + 1)]
      s <- exp(th[k + 2])
      tot <- 0
      for (idx in units) {
        eta <- b0 + as.vector(Xm[idx, , drop = FALSE] %*% b)
        acc <- 0
        for (q in seq_along(gh$nodes)) {
          p <- Fc(eta + s * sqrt(2) * gh$nodes[q])
          acc <- acc + gh$weights[q] * prod(ifelse(Y[idx] == 1, p, 1 - p))
        }
        tot <- tot - log(max(acc / sqrt(pi), 1e-300))
      }
      tot
    }
    start <- numeric(k + 2)
    n_units <- length(units)
  }
  r <- BfgsMinimize(nll, start, gtol = 1e-8, max_iter = 500)
  th <- r$x
  p <- length(th)
  H <- matrix(0, p, p)
  for (a in seq_len(p)) for (cc in a:p) {
    ha <- 1e-4 * max(1, abs(th[a]))
    hc <- 1e-4 * max(1, abs(th[cc]))
    f <- function(da, dc) {
      t <- th
      t[a] <- t[a] + da
      t[cc] <- t[cc] + dc
      nll(t)
    }
    H[a, cc] <- H[cc, a] <- (f(ha, hc) - f(ha, -hc) - f(-ha, hc) + f(-ha, -hc)) / (4 * ha * hc)
  }
  se <- tryCatch({
    v <- diag(solve(H))
    ifelse(v > 0, sqrt(pmax(v, 0)), NaN)
  }, error = function(e) rep(NaN, p))
  out <- list(loglik = -r$fun, n_units = n_units, converged = r$converged)
  if (model == "fe_logit") c(list(coef = th, se = se), out) else c(list(coef = th[1:(k + 1)], se = se[1:(k + 1)], sigma = exp(th[k + 2])), out)
}

#' Bliss points from preference ratings
#'
#' External unfolding with the PREFMAP ideal-point model: per rater, least squares
#' of ratings on (1, z_j, ||z_j||^2) gives the ideal point -c / (2 c2) and
#' salience -c2 (anti-ideal points flagged when c2 >= 0); optionally a
#' Nadaraya-Watson (Gaussian kernel) smooth of each rater's ratings over a grid.
#'
#' @param ratings Raters by stimuli matrix.
#' @param stimuli Stimulus positions (vector or matrix rows).
#' @param grid Optional evaluation points for the utility surface.
#' @param bandwidth Kernel bandwidth (default: median inter-stimulus distance).
#' @return list(ideal, salience, anti_ideal, r2, surface, bandwidth).
#' @references Carroll, J. D. (1972). Individual differences and
#'   multidimensional scaling. In Multidimensional Scaling: Theory and
#'   Applications in the Behavioral Sciences, vol. 1, 105-155.
#' @examples
#' z <- 0:3
#' BlissPoints(matrix(10 - (z - 1.2)^2, 1), z)$ideal
#' @export
BlissPoints <- function(ratings, stimuli, grid = NULL, bandwidth = NULL) {
  Z <- if (is.matrix(stimuli)) stimuli * 1 else matrix(as.numeric(stimuli), ncol = 1)
  R <- as.matrix(ratings) * 1
  d <- ncol(Z)
  D <- cbind(1, Z, rowSums(Z^2))
  ideal <- list()
  sal <- anti <- r2 <- numeric(nrow(R))
  for (i in seq_len(nrow(R))) {
    cf <- solve(crossprod(D), crossprod(D, R[i, ]))
    c2 <- cf[length(cf)]
    anti[i] <- c2 >= 0
    sal[i] <- -c2
    ideal[[i]] <- if (c2 == 0) NULL else -cf[2:(d + 1)] / (2 * c2)
    fit <- as.vector(D %*% cf)
    sst <- sum((R[i, ] - mean(R[i, ]))^2)
    r2[i] <- if (sst > 0) 1 - sum((R[i, ] - fit)^2) / sst else 1
  }
  out <- list(ideal = ideal, salience = sal, anti_ideal = as.logical(anti), r2 = r2)
  if (!is.null(grid)) {
    G <- if (is.matrix(grid)) grid * 1 else matrix(as.numeric(grid), ncol = d)
    h <- if (is.null(bandwidth)) {
      ds <- sort(as.vector(stats::dist(Z)))
      ds[length(ds) %/% 2 + 1]
    } else {
      bandwidth
    }
    out$surface <- t(vapply(seq_len(nrow(R)), function(i) vapply(seq_len(nrow(G)), function(g) {
      w <- exp(-0.5 * (sqrt(colSums((t(Z) - G[g, ])^2)) / h)^2)
      sum(w * R[i, ]) / sum(w)
    }, 0), numeric(nrow(G))))
    out$bandwidth <- h
  }
  out
}
