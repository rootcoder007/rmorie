#' Linear model of coregionalization and cokriging
#'
#' \code{LmcCovariance}: cross-covariance matrix \eqn{C(h) = \sum_s B_s
#' \rho_s(h)} of a linear model of coregionalization (a single structure is
#' the intrinsic coregionalization model). \code{LmcCokriging}: ordinary
#' cokriging (one unbiasedness constraint per variable, as gstat) or simple
#' cokriging (known means) of heterotopic data in any dimension, with the
#' cokriging weights. Identical to the Python arm \code{morie.fn.lmckrige}.
#'
#' @param h Distance.
#' @param lmc List of structures, each a list with \code{model} (Exp, Gau,
#'   Sph, Mat, Nug), \code{range}, optional \code{kappa} and a symmetric
#'   p by p matrix \code{B}.
#' @param z Observations.
#' @param coords Matrix of observation coordinates (any dimension).
#' @param var Variable of each observation (0-based).
#' @param new_coords Matrix of target coordinates.
#' @param target Variable to predict (0-based).
#' @param means Known means (simple cokriging), or NULL.
#' @return Matrix (\code{LmcCovariance}) or list with \code{prediction},
#'   \code{variance} and \code{weights}.
#' @references Wackernagel, H. (2003). Multivariate Geostatistics, 3rd edn.
#'   Springer.
#'
#'   Pebesma, E. J. (2004). Multivariable geostatistics in S: the gstat
#'   package. Computers and Geosciences 30, 683-691.
#' @examples
#' lmc <- list(list(model = "Exp", range = 2, B = matrix(c(1, 0.6, 0.6, 1), 2)))
#' LmcCovariance(1, lmc)
#' LmcCokriging(c(1, 2, 1.5, 0.5), rbind(c(0, 0), c(2, 0), c(1, 0), c(0, 1)), c(0, 0, 1, 1),
#'              rbind(c(1, 1)), lmc)$prediction
#' @export
LmcCovariance <- function(h, lmc) {
  lmc <- .lmc_check(lmc)
  p <- nrow(lmc[[1]]$B)
  out <- matrix(0, p, p)
  for (s in lmc) {
    cc <- s[setdiff(names(s), c("B", "nugget"))]
    cc$psill <- 1
    out <- out + s$B * .stk_comp(abs(h), cc)
  }
  out
}

.lmc_check <- function(lmc) {
  if (!is.null(lmc$B)) lmc <- list(lmc)
  p <- nrow(as.matrix(lmc[[1]]$B))
  for (i in seq_along(lmc)) {
    lmc[[i]]$B <- as.matrix(lmc[[i]]$B) + 0
    if (any(dim(lmc[[i]]$B) != p)) stop("every structure needs a p x p coregionalization matrix B")
    if (any(abs(lmc[[i]]$B - t(lmc[[i]]$B)) > 1e-12)) stop("B must be symmetric")
  }
  lmc
}

.lmc_ss <- function(v) {
  s <- 0
  for (a in v) s <- s + a
  s
}

#' @rdname LmcCovariance
#' @export
LmcCokriging <- function(z, coords, var, new_coords, lmc, target = 0L, means = NULL) {
  lmc <- .lmc_check(lmc)
  p <- nrow(lmc[[1]]$B)
  z <- as.numeric(z)
  P <- as.matrix(coords)
  Q <- as.matrix(new_coords)
  vv <- as.integer(var) + 1L
  tg <- target + 1L
  n <- length(z)
  if (nrow(P) != n || length(vv) != n || any(vv < 1 | vv > p) || tg < 1 || tg > p) {
    stop("z, coords and var must match and variables lie in 0..p-1")
  }
  dd <- function(a, b) {
    s <- 0
    for (t in seq_along(a)) s <- s + (a[t] - b[t]) * (a[t] - b[t])
    sqrt(s)
  }
  C <- matrix(0, n, n)
  for (i in seq_len(n)) for (j in seq_len(n)) C[i, j] <- LmcCovariance(dd(P[i, ], P[j, ]), lmc)[vv[i], vv[j]]
  Ci <- solve(C)
  c00 <- LmcCovariance(0, lmc)[tg, tg]
  pred <- numeric(nrow(Q))
  varr <- numeric(nrow(Q))
  W <- vector("list", nrow(Q))
  for (k in seq_len(nrow(Q))) {
    c0 <- vapply(seq_len(n), function(i) LmcCovariance(dd(P[i, ], Q[k, ]), lmc)[vv[i], tg], 0)
    Cic0 <- as.vector(Ci %*% c0)
    if (!is.null(means)) {
      mu <- as.numeric(means)
      pred[k] <- mu[tg] + .lmc_ss(Cic0 * (z - mu[vv]))
      varr[k] <- c00 - .lmc_ss(c0 * Cic0)
      lam <- Cic0
    } else {
      used <- sort(unique(vv))
      if (!tg %in% used) stop("ordinary cokriging needs observations of the target variable")
      X <- outer(vv, used, "==") + 0
      x0 <- as.numeric(used == tg)
      CiX <- Ci %*% X
      Ai <- solve(t(X) %*% CiX)
      r <- x0 - as.vector(t(X) %*% Cic0)
      Air <- as.vector(Ai %*% r)
      lam <- Cic0 + as.vector(CiX %*% Air)
      pred[k] <- .lmc_ss(lam * z)
      varr[k] <- c00 - .lmc_ss(c0 * Cic0) + .lmc_ss(r * Air)
    }
    W[[k]] <- lam
  }
  list(prediction = pred, variance = varr, weights = W)
}
