#' Regional specialization and concentration indices
#'
#' \code{LocationQuotient}: \code{(e_ri / e_r) / (e_i / e)} for a regions x
#' industries matrix (\code{REAT::locq}); \code{method = "difference"} gives
#' \code{e_ri/e_r - e_i/e}. \code{KrugmanIndex}: \code{K = sum |x_i/sum(x) -
#' r_i/sum(r)|} and Hoover \code{K/2}. \code{LqGini}: Gini of location
#' quotients (\code{REAT::gini.spec}). \code{HerfindahlIndex}: HHI, its
#' normalised form, \code{1/H} and the concentration ratio \code{CR_k}.
#' Identical to the Python arm \code{morie.fn.econgeo}.
#'
#' @param E Regions x industries employment matrix.
#' @param method \code{"ratio"} or \code{"difference"}.
#' @param x,ref Employment vectors.
#' @param k Number of largest units in \code{CR_k}.
#' @return Matrix, numeric or list.
#' @references Krugman, P. (1991). Geography and Trade. MIT Press.
#'
#'   Wieland, T. (2019). REAT: A regional economic analysis toolbox for R.
#'   REGION 6(3), R1-R57.
#' @examples
#' LocationQuotient(rbind(c(10, 30), c(30, 30)))
#' KrugmanIndex(c(10, 30, 60), c(30, 30, 40))$K
#' LqGini(c(10, 30, 60), c(30, 30, 40))
#' @export
LocationQuotient <- function(E, method = "ratio") {
  E <- as.matrix(E)
  ci <- colSums(E) / sum(E)
  sh <- E / rowSums(E)
  out <- if (method == "ratio") sweep(sh, 2, ci, "/") else sweep(sh, 2, ci, "-")
  unname(out)
}

#' @rdname LocationQuotient
#' @export
KrugmanIndex <- function(x, ref) {
  if (length(x) != length(ref)) stop("x and ref must have equal length")
  K <- sum(abs(x / sum(x) - ref / sum(ref)))
  list(K = K, hoover = K / 2)
}

#' @rdname LocationQuotient
#' @export
LqGini <- function(x, ref) {
  R <- sort((x / sum(x)) / (ref / sum(ref)))
  n <- length(R)
  m <- mean(R)
  2 / (n * n * m) * sum(seq_len(n) * (R - m))
}

#' @rdname LocationQuotient
#' @export
HerfindahlIndex <- function(x, k = 4) {
  s <- x / sum(x)
  H <- sum(s^2)
  n <- length(x)
  list(H = H, normalized = if (n > 1) (H - 1 / n) / (1 - 1 / n) else NaN, equivalent_number = 1 / H,
       CR = sum(sort(s, decreasing = TRUE)[seq_len(min(k, n))]))
}

.eg_parts <- function(emp, region, regions, sj) {
  sij <- vapply(regions, function(r) sum(emp[region == r]), 0) / sum(emp)
  list(G = sum((sij - sj)^2), H = sum((emp / sum(emp))^2))
}

#' Ellison-Glaeser agglomeration and Duranton-Overman K-density
#'
#' \code{EllisonGlaeser}: Ellison and Glaeser (1997) \code{gamma}, raw
#' concentration \code{G}, plant Herfindahl \code{H} and the null
#' z-statistic, as \code{REAT::ellison.a}. \code{CoagglomerationIndex}:
#' Ellison-Glaeser coagglomeration of a group of industries.
#' \code{DurantonOverman}: kernel density of bilateral distances with
#' reflection at zero, Silverman bandwidth, and local counterfactual bands from
#' Philox redraws among \code{sites}. Region employment vectors follow the
#' sorted region labels.
#'
#' @param plant_emp Plant employment.
#' @param plant_region Plant region labels.
#' @param plant_industry Plant industry labels.
#' @param region_emp Aggregate employment by sorted region label.
#' @param coords Two-column coordinates of the industry's establishments.
#' @param r Distances at which to evaluate the density.
#' @param sites Candidate locations for the counterfactual.
#' @param n_sim Number of counterfactual draws.
#' @param bandwidth Kernel bandwidth (default Silverman's rule).
#' @param level Two-sided band level.
#' @param seed Philox seed.
#' @return List.
#' @references Ellison, G. and Glaeser, E. L. (1997). Geographic
#'   concentration in U.S. manufacturing industries: a dartboard approach.
#'   Journal of Political Economy 105, 889-927.
#'
#'   Duranton, G. and Overman, H. G. (2005). Testing for localization using
#'   micro-geographic data. Review of Economic Studies 72, 1077-1106.
#' @examples
#' EllisonGlaeser(c(10, 20, 30, 40), c("a", "a", "b", "c"), c(100, 100, 100))$gamma
#' DurantonOverman(rbind(c(0, 0), c(1, 0), c(0, 1)), c(0, 1), bandwidth = 0.5)$K
#' @export
EllisonGlaeser <- function(plant_emp, plant_region, region_emp = NULL) {
  regions <- sort(unique(as.character(plant_region)))
  reg <- as.character(plant_region)
  xe <- if (is.null(region_emp)) vapply(regions, function(r) sum(plant_emp[reg == r]), 0) else region_emp
  if (length(xe) != length(regions)) stop("region_emp needs one value per region")
  sj <- unname(xe / sum(xe))
  p <- .eg_parts(plant_emp, reg, regions, sj)
  s2 <- sum(sj^2)
  s3 <- sum(sj^3)
  z4 <- sum((plant_emp / sum(plant_emp))^4)
  varG <- 2 * (p$H^2 * (s2 - 2 * s3 + s2^2) - z4 * (s2 - 4 * s3 + 3 * s2^2))
  list(gamma = (p$G - (1 - s2) * p$H) / ((1 - s2) * (1 - p$H)), G = p$G, H = p$H,
       z = (p$G - (1 - s2) * p$H) / sqrt(varG), regions = regions)
}

#' @rdname EllisonGlaeser
#' @export
CoagglomerationIndex <- function(plant_emp, plant_industry, plant_region, region_emp) {
  reg <- as.character(plant_region)
  ind <- as.character(plant_industry)
  regions <- sort(unique(reg))
  if (length(region_emp) != length(regions)) stop("region_emp needs one value per region")
  sj <- region_emp / sum(region_emp)
  s2 <- sum(sj^2)
  G <- .eg_parts(plant_emp, reg, regions, sj)$G
  inds <- sort(unique(ind))
  parts <- lapply(inds, function(i) {
    k <- ind == i
    p <- .eg_parts(plant_emp[k], reg[k], regions, sj)
    c(w = sum(plant_emp[k]) / sum(plant_emp), H = p$H, g = (p$G - (1 - s2) * p$H) / ((1 - s2) * (1 - p$H)))
  })
  M <- do.call(rbind, parts)
  H <- sum(M[, "w"]^2 * M[, "H"])
  gc <- (G / (1 - s2) - H - sum(M[, "g"] * M[, "w"]^2 * (1 - M[, "H"]))) / (1 - sum(M[, "w"]^2))
  list(gamma_c = gc, G = G, H = H, gamma_i = unname(M[, "g"]), w = unname(M[, "w"]), industries = inds)
}

.do_kd <- function(P, r, bw) {
  d <- as.vector(stats::dist(P))
  h <- if (is.null(bw)) {
    s <- stats::sd(d)
    iqr <- unname(diff(stats::quantile(d, c(0.25, 0.75), type = 7)))
    0.9 * (if (iqr > 0) min(s, iqr / 1.34) else s) * length(d)^(-0.2)
  } else {
    bw
  }
  cst <- 1 / (length(d) * h * sqrt(2 * pi))
  list(K = vapply(r, function(x) cst * sum(exp(-0.5 * ((x - d) / h)^2) + exp(-0.5 * ((x + d) / h)^2)), 0), h = h)
}

#' @rdname EllisonGlaeser
#' @export
DurantonOverman <- function(coords, r, sites = NULL, n_sim = 0, bandwidth = NULL, level = 0.05, seed = 1) {
  P <- as.matrix(coords)
  kd <- .do_kd(P, r, bandwidth)
  out <- list(r = r, K = kd$K, bandwidth = kd$h)
  if (!is.null(sites) && n_sim > 0) {
    S <- as.matrix(sites)
    n <- nrow(P)
    m <- nrow(S)
    sims <- t(vapply(seq_len(n_sim) - 1, function(b) {
      u <- .morie_random_uniform(n, seed = seed, stream = b)
      idx <- seq_len(m)
      for (t in seq_len(n)) {
        j <- t + floor(u[t] * (m - t + 1))
        tmp <- idx[t]
        idx[t] <- idx[j]
        idx[j] <- tmp
      }
      .do_kd(S[idx[seq_len(n)], , drop = FALSE], r, kd$h)$K
    }, numeric(length(r))))
    lo <- apply(sims, 2, stats::quantile, probs = level / 2, type = 7, names = FALSE)
    hi <- apply(sims, 2, stats::quantile, probs = 1 - level / 2, type = 7, names = FALSE)
    out <- c(out, list(lower = lo, upper = hi, localized = kd$K > hi, dispersed = kd$K < lo))
  }
  out
}
