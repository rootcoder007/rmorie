.srel_matrix <- function(data, items, prefix) {
  if (is.null(items)) items <- paste0(prefix, 1:5)
  if (is.data.frame(data)) {
    X <- as.matrix(data[, items, drop = FALSE])
    X <- X[stats::complete.cases(X), , drop = FALSE]
  } else {
    X <- as.matrix(data)
  }
  storage.mode(X) <- "double"
  X
}

.srel_cor <- function(X) {
  n <- nrow(X)
  p <- ncol(X)
  if (n < 3 || p < 2) stop("need at least 3 respondents and 2 items")
  D <- sweep(X, 2, colSums(X) / n)
  C <- crossprod(D)
  s <- sqrt(diag(C))
  if (any(!(s > 0))) stop("an item has zero variance")
  C / outer(s, s)
}

.srel_onefactor <- function(R, tol = 1e-12, max_iter = 200000) {
  p <- nrow(R)
  lam <- vapply(seq_len(p), function(j) sqrt(min(0.95, max(0.05, max(abs(R[j, -j]))))), 0)
  psi <- 1 - lam^2
  it <- 0
  conv <- FALSE
  while (it < max_iter) {
    it <- it + 1
    a <- lam / psi
    beta <- a / (1 + sum(a * lam))
    Rb <- as.numeric(R %*% beta)
    czz <- 1 + sum(-beta * lam + beta * Rb)
    new <- Rb / czz
    psi <- pmax(diag(R) - new * Rb, 0.005)
    delta <- max(abs(new - lam))
    lam <- new
    if (delta < tol) {
      conv <- TRUE
      break
    }
  }
  list(lambda = lam, psi = psi, iterations = it, converged = conv)
}

.srel_fit <- function(data, items, prefix, what) {
  X <- .srel_matrix(data, items, prefix)
  f <- .srel_onefactor(.srel_cor(X))
  lam <- abs(f$lambda)
  est <- if (what == "ave") mean(lam^2) else sum(lam)^2 / (sum(lam)^2 + sum(1 - lam^2))
  list(measure = if (what == "ave") paste0("AVE_", prefix) else paste0("composite_reliability_", prefix),
       estimate = est, n = nrow(X), loadings = lam, uniquenesses = f$psi, subscale = prefix,
       iterations = f$iterations, converged = f$converged)
}

#' Subscale average variance extracted and composite reliability
#'
#' For a five-item subscale (EA, EE, ER or UA of the MAPQ-II instrument, or
#' any \code{items}), fits the one-factor model to the item correlation
#' matrix by maximum likelihood (Rubin-Thayer EM, uniquenesses bounded below
#' by 0.005 as in \code{stats::factanal}) and reports the average
#' variance extracted \eqn{\mathrm{AVE} = \overline{\lambda^2}} or the
#' composite reliability
#' \eqn{\rho_c = (\sum\lambda)^2 / ((\sum\lambda)^2 + \sum(1 - \lambda^2))}
#' of the absolute standardised loadings. First-principal-component loadings
#' are not factor loadings and overstate both. Identical to the Python arms
#' \code{morie.fn.sav_a} ... \code{morie.fn.scr_u}.
#'
#' @param data Data frame with the item columns (complete rows are used) or a
#'   numeric matrix of item responses.
#' @param items Column names; default the subscale prefix followed by 1 to 5.
#' @return A list with \code{measure}, \code{estimate}, \code{n},
#'   \code{loadings}, \code{uniquenesses}, \code{subscale},
#'   \code{iterations} and \code{converged}.
#' @references Fornell, C. and Larcker, D. F. (1981). Evaluating structural
#'   equation models with unobservable variables and measurement error.
#'   Journal of Marketing Research 18, 39-50.
#'
#'   Rubin, D. B. and Thayer, D. T. (1982). EM algorithms for ML factor
#'   analysis. Psychometrika 47, 69-76.
#' @examples
#' set.seed(1)
#' f <- rnorm(80)
#' X <- sapply(1:5, function(j) f + rnorm(80, sd = 1.5))
#' subscale_ea_ave(X)$estimate
#' subscale_ea_composite_rel(X)$estimate
#' @export
subscale_ea_ave <- function(data, items = NULL) .srel_fit(data, items, "EA", "ave")

#' @rdname subscale_ea_ave
#' @export
subscale_ea_composite_rel <- function(data, items = NULL) .srel_fit(data, items, "EA", "cr")

#' @rdname subscale_ea_ave
#' @export
subscale_ee_ave <- function(data, items = NULL) .srel_fit(data, items, "EE", "ave")

#' @rdname subscale_ea_ave
#' @export
subscale_ee_composite_rel <- function(data, items = NULL) .srel_fit(data, items, "EE", "cr")

#' @rdname subscale_ea_ave
#' @export
subscale_er_ave <- function(data, items = NULL) {
  .morie_arg(data, "m")
  .srel_fit(data, items, "ER", "ave")
}

#' @rdname subscale_ea_ave
#' @export
subscale_er_composite_rel <- function(data, items = NULL) .srel_fit(data, items, "ER", "cr")

#' @rdname subscale_ea_ave
#' @export
subscale_ua_ave <- function(data, items = NULL) .srel_fit(data, items, "UA", "ave")

#' @rdname subscale_ea_ave
#' @export
subscale_ua_composite_rel <- function(data, items = NULL) .srel_fit(data, items, "UA", "cr")
