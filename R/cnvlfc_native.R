# morie.fn -- R arm for the cnvlfc module (rootcoder007/morie).
#
# Convergent cross mapping (Sugihara, May, Ye, Hsieh, Deyle, Fogarty &
# Munch 2012, Science 338(6106), 496-500, doi:10.1126/science.1227079):
# recover a "driver" from the delay-embedding manifold of a
# "response", and check whether cross-map skill rises with library
# length. Skill recovering X from M_Y supports "X drives Y". The
# convention is the opposite of what one might expect, and is the
# usual place the method is misread, so both directions are always
# scored and labelled with the causal statement each supports.
#
# This is a native-R translation of /home/rootcoder/work/morie/src/morie/fn/cnvlfc.py.
# No external packages are used; RNG helpers are SplitMix64 (.ghc_rng)
# which is bit-identical to numpy's default_rng.

#' .cnvlfc_embed
#'
#' A step of the cnvlfc_native implementation. Called by \code{.cnvlfc_ccm}, \code{.cnvlfc_cross_map}, \code{cnvlfc_embed}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param series Coerced to numeric by the body, with \code{as.numeric}.
#' @param E Coerced to integer by the body, with \code{as.integer}. Defaults to \code{2}.
#' @param tau Coerced to integer by the body, with \code{as.integer}. Defaults to \code{1}.
#' @return A list with \code{points}, \code{index}, \code{E}, \code{tau}.
#' @export
.cnvlfc_embed <- function(series, E = 2, tau = 1) {
    v <- as.numeric(series)
    e <- as.integer(E)
    t <- as.integer(tau)
    if (e < 1L) stop("cnvlfc: the embedding dimension must be at least 1")
    if (t < 1L) stop("cnvlfc: the delay must be at least 1")
    need <- (e - 1L) * t
    if (length(v) <= need + 1L) {
        stop(sprintf("cnvlfc: %d points cannot support dimension %d at delay %d",
                     length(v), e, t))
    }
    idx <- (need + 1L):length(v)               # 1-based inclusive
    # build points: row i is (v[i], v[i-t], ..., v[i-(E-1)*t])
    m <- length(idx)
    pts <- matrix(NA_real_, nrow = m, ncol = e)
    for (i in seq_len(m)) {
        ii <- idx[i]
        for (k in seq_len(e)) {
            pts[i, k] <- v[ii - (k - 1L) * t]
        }
    }
    # index is REPORTED, so it follows the Python 0-based
    # convention; idx itself stays 1-based for the subsetting above.
    list(points = pts, index = idx - 1L, E = e, tau = t)
}

#' .cnvlfc_corr
#'
#' A step of the cnvlfc_native implementation. Called by \code{.cnvlfc_cross_map}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param a A vector; its length is taken.
#' @param b Numeric; passed to \code{sum}.
#' @return A numeric value.
#' @export
.cnvlfc_corr <- function(a, b) {
    n <- length(a)
    if (n < 2L) return(NaN)
    ma <- sum(a) / n
    mb <- sum(b) / n
    sa <- sum((a - ma)^2)
    sb <- sum((b - mb)^2)
    if (sa <= 0 || sb <= 0) return(NaN)
    cov <- sum((a - ma) * (b - mb))
    cov / sqrt(sa * sb)
}

#' .cnvlfc_euclid
#'
#' A step of the cnvlfc_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param target A matrix; indexed by row and column.
#' @param candidates A matrix; indexed by row and column.
#' @param exclude_idx A vector; indexed elementwise.
#' @param target_idx Numeric; combined arithmetically in the body.
#' @param exclude_window Passed to \code{<=}.
#' @return A list with \code{d}, \code{ord}, \code{keep}.
#' @export
.cnvlfc_euclid <- function(target, candidates, exclude_idx, target_idx,
                          exclude_window) {
    # returns sorted (distance, index) for first (E+1) candidates.
    k <- ncol(target)                       # dimension
    d <- vapply(candidates, function(b) {
        if (abs(exclude_idx[b] - target_idx) <= exclude_window) {
            return(Inf)
        }
        diff <- target[1L, ] - candidates[b, ]
        sqrt(sum(diff * diff))
    }, numeric(1L))
    ord <- order(d)
    # take E+1 nearest finite ones
    keep <- ord[!is.infinite(d[ord])]
    need <- as.integer(target[1L, ncol(target)])   # hidden E+1 in col? no.
    # E+1 must be passed; instead, recover from ncol of candidates.
    list(d = d, ord = ord, keep = keep)
}

#' .cnvlfc_cross_map
#'
#' A step of the cnvlfc_native implementation. Called by \code{.cnvlfc_ccm}, \code{cnvlfc_cross_map}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param driver Coerced to numeric by the body, with \code{as.numeric}.
#' @param response Coerced to numeric by the body, with \code{as.numeric}.
#' @param E Passed to \code{.cnvlfc_embed}. Defaults to \code{2L}.
#' @param tau Passed to \code{.cnvlfc_embed}. Defaults to \code{1L}.
#' @param library Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.
#' @param seed Coerced to integer by the body, with \code{as.integer}. Defaults to \code{1L}.
#' @param exclude Coerced to integer by the body, with \code{as.integer}. Defaults to \code{0L}.
#' @return A list with \code{rho}, \code{observed}, \code{predicted}, \code{n_predicted}, \code{library}, \code{E}, \code{tau}.
#' @export
.cnvlfc_cross_map <- function(driver, response, E = 2L, tau = 1L,
                              library = NULL, seed = 1L, exclude = 0L) {
    X <- as.numeric(driver)
    Y <- as.numeric(response)
    if (length(X) != length(Y)) {
        stop(sprintf("cnvlfc: the two series have %d and %d points",
                     length(X), length(Y)))
    }
    em <- .cnvlfc_embed(Y, E, tau)
    pts <- em$points
    idx <- em$index
    m   <- nrow(pts)
    k   <- as.integer(E) + 1L
    if (is.null(library)) {
        lib <- seq_len(m)
    } else {
        L <- as.integer(library)
        if (L < k + 1L) {
            stop(sprintf("cnvlfc: a library of %d points cannot supply %d neighbours",
                         L, k))
        }
        if (L > m) {
            stop(sprintf("cnvlfc: the library asks for %d points but only %d are embeddable",
                         L, m))
        }
        rng <- .ghc_rng(as.integer(seed))
        u <- .ghc_unif(rng)
        start <- (as.integer(u * (m - L + 1L))) %% (m - L + 1L)
        lib <- seq.int(start + 1L, length.out = L)    # 1-based
    }
    ex <- as.integer(exclude)

    obs  <- numeric(0)
    pred <- numeric(0)
    for (a in seq_len(m)) {
        cand <- lib[lib != a &
                    abs(idx[lib] - idx[a]) > ex]
        nc <- length(cand)
        if (nc < k) next
        # compute distances from pts[a,] to pts[cand,]
        d <- numeric(nc)
        for (cc in seq_len(nc)) {
            diff <- pts[a, ] - pts[cand[cc], ]
            d[cc] <- sqrt(sum(diff * diff))
        }
        ord <- order(d)
        d1 <- d[ord[1L]]
        if (d1 <= 0) {
            w <- numeric(k)
            w[1L] <- 1
        } else {
            w <- exp(-d[ord[seq_len(k)]] / d1)
        }
        sw <- sum(w)
        w  <- w / sw
        est <- 0
        for (j in seq_len(k)) {
            b <- cand[ord[j]]
            est <- est + w[j] * X[idx[b]]
        }
        obs  <- c(obs,  X[idx[a]])
        pred <- c(pred, est)
    }
    if (length(obs) < 3L) {
        stop("cnvlfc: too few points survived the embedding and Theiler window to score")
    }
    list(rho = .cnvlfc_corr(obs, pred),
         observed = obs, predicted = pred,
         n_predicted = length(obs),
         library = length(unique(lib)),
         E = as.integer(E), tau = as.integer(tau))
}

#' .cnvlfc_ccm
#'
#' A step of the cnvlfc_native implementation. Called by \code{cnvlfc_ccm}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x Coerced to numeric by the body, with \code{as.numeric}.
#' @param y Coerced to numeric by the body, with \code{as.numeric}.
#' @param E Passed to \code{.cnvlfc_embed}. Defaults to \code{2L}.
#' @param tau Passed to \code{.cnvlfc_embed}. Defaults to \code{1L}.
#' @param lib_sizes Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.
#' @param seed Passed to \code{.cnvlfc_cross_map}. Defaults to \code{1L}.
#' @param exclude Passed to \code{.cnvlfc_cross_map}. Defaults to \code{0L}.
#' @return A list with \code{estimate}, \code{x_causes_y}, \code{y_causes_x}, \code{lib_sizes}, \code{E}, \code{tau}, \code{n_embeddable}, \code{verdict}, \code{method}.
#' @export
.cnvlfc_ccm <- function(x, y, E = 2L, tau = 1L, lib_sizes = NULL,
                        seed = 1L, exclude = 0L) {
    X <- as.numeric(x)
    Y <- as.numeric(y)
    m <- nrow(.cnvlfc_embed(Y, E, tau)$points)
    if (is.null(lib_sizes)) {
        base <- as.integer(E) + 3L
        candidates <- unique(pmax(base, as.integer(m * c(0.05, 0.1, 0.25, 0.5, 1.0))))
        lib_sizes <- sort(candidates)
    }
    curves <- list(x_causes_y = list(), y_causes_x = list())
    for (L in lib_sizes) {
        # Y cross maps X  =>  X drives Y
        r_xy <- .cnvlfc_cross_map(X, Y, E, tau, L, seed, exclude)$rho
        r_yx <- .cnvlfc_cross_map(Y, X, E, tau, L, seed, exclude)$rho
        curves$x_causes_y <- c(curves$x_causes_y,
                               list(list(library = as.integer(L), rho = r_xy)))
        curves$y_causes_x <- c(curves$y_causes_x,
                               list(list(library = as.integer(L), rho = r_yx)))
    }
    out <- list()
    for (key in c("x_causes_y", "y_causes_x")) {
        rs <- vapply(curves[[key]], function(p) p$rho, numeric(1L))
        out[[key]] <- list(
            curve    = curves[[key]],
            rho_final = rs[length(rs)],
            rho_first = rs[1L],
            increase  = rs[length(rs)] - rs[1L],
            converges = (rs[length(rs)] - rs[1L]) > 0.05 && rs[length(rs)] > 0.3
        )
    }
    a <- out$x_causes_y$converges
    b <- out$y_causes_x$converges
    if (a && b) {
        verdict <- paste0("bidirectional coupling, or synchrony -- CCM ",
                          "cannot separate the two")
    } else if (a) {
        verdict <- "x drives y"
    } else if (b) {
        verdict <- "y drives x"
    } else {
        verdict <- "no convergent cross mapping in either direction"
    }
    list(
        estimate     = list(x_causes_y = out$x_causes_y$rho_final,
                            y_causes_x = out$y_causes_x$rho_final),
        x_causes_y   = out$x_causes_y,
        y_causes_x   = out$y_causes_x,
        lib_sizes    = as.integer(lib_sizes),
        E            = as.integer(E),
        tau          = as.integer(tau),
        n_embeddable = as.integer(m),
        verdict      = verdict,
        method       = paste0("convergent cross mapping (Sugihara et al. 2012); ",
                              "skill recovering X from the manifold of Y ",
                              "supports X causing Y")
    )
}

#' .cnvlfc_coupled_logistic
#'
#' A step of the cnvlfc_native implementation. Called by \code{cnvlfc_coupled_logistic}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param n Coerced to integer by the body, with \code{as.integer}.
#' @param rx Numeric; combined arithmetically in the body. Defaults to \code{3.8}.
#' @param ry Numeric; combined arithmetically in the body. Defaults to \code{3.5}.
#' @param bxy Numeric; combined arithmetically in the body. Defaults to \code{0}.
#' @param byx Numeric; combined arithmetically in the body. Defaults to \code{0.1}.
#' @param x0 Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.4}.
#' @param y0 Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.2}.
#' @param burn Coerced to integer by the body, with \code{as.integer}. Defaults to \code{300L}.
#' @return A list with \code{x}, \code{y}, \code{bxy}, \code{byx}.
#' @export
.cnvlfc_coupled_logistic <- function(n, rx = 3.8, ry = 3.5, bxy = 0.0,
                                     byx = 0.1, x0 = 0.4, y0 = 0.2,
                                     burn = 300L) {
    x <- as.numeric(x0)
    y <- as.numeric(y0)
    nb <- as.integer(burn)
    nn <- as.integer(n)
    X <- numeric(nn)
    Y <- numeric(nn)
    total <- nb + nn
    for (i in seq_len(total)) {
        xn <- x * (rx - rx * x - bxy * y)
        yn <- y * (ry - ry * y - byx * x)
        x <- xn
        y <- yn
        if (!is.finite(x) || !is.finite(y)) {
            stop(sprintf("cnvlfc: the coupled map diverged at step %d; ",
                         "the parameters are outside the bounded regime", i - 1L))
        }
        if (i > nb) {
            X[i - nb] <- x
            Y[i - nb] <- y
        }
    }
    list(x = X, y = Y, bxy = as.numeric(bxy), byx = as.numeric(byx))
}

#' cnvlfc_embed
#'
#' A step of the cnvlfc_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param series Passed to \code{.cnvlfc_embed}.
#' @param E Passed to \code{.cnvlfc_embed}. Defaults to \code{2}.
#' @param tau Passed to \code{.cnvlfc_embed}. Defaults to \code{1}.
#' @return A list with \code{points}, \code{index}, \code{E}, \code{tau}.
#' @export
cnvlfc_embed <- function(series, E = 2, tau = 1) {
    em <- .cnvlfc_embed(series, E, tau)
    list(points = lapply(seq_len(nrow(em$points)),
                         function(i) em$points[i, ]),
         index = em$index,
         E = em$E, tau = em$tau)
}

#' cnvlfc_cross_map
#'
#' A step of the cnvlfc_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param driver Passed to \code{.cnvlfc_cross_map}.
#' @param response Passed to \code{.cnvlfc_cross_map}.
#' @param E Passed to \code{.cnvlfc_cross_map}. Defaults to \code{2L}.
#' @param tau Passed to \code{.cnvlfc_cross_map}. Defaults to \code{1L}.
#' @param library Passed to \code{.cnvlfc_cross_map}.
#' @param seed Passed to \code{.cnvlfc_cross_map}. Defaults to \code{1L}.
#' @param exclude Passed to \code{.cnvlfc_cross_map}. Defaults to \code{0L}.
#' @return The value of \code{.cnvlfc_cross_map}.
#' @export
cnvlfc_cross_map <- function(driver, response, E = 2L, tau = 1L,
                             library = NULL, seed = 1L, exclude = 0L) {
    .cnvlfc_cross_map(driver, response, E, tau, library, seed, exclude)
}

#' cnvlfc_ccm
#'
#' A step of the cnvlfc_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x Passed to \code{.cnvlfc_ccm}.
#' @param y Passed to \code{.cnvlfc_ccm}.
#' @param E Passed to \code{.cnvlfc_ccm}. Defaults to \code{2L}.
#' @param tau Passed to \code{.cnvlfc_ccm}. Defaults to \code{1L}.
#' @param lib_sizes Passed to \code{.cnvlfc_ccm}.
#' @param seed Passed to \code{.cnvlfc_ccm}. Defaults to \code{1L}.
#' @param exclude Passed to \code{.cnvlfc_ccm}. Defaults to \code{0L}.
#' @return The value of \code{.cnvlfc_ccm}.
#' @export
cnvlfc_ccm <- function(x, y, E = 2L, tau = 1L, lib_sizes = NULL,
                       seed = 1L, exclude = 0L) {
    .cnvlfc_ccm(x, y, E, tau, lib_sizes, seed, exclude)
}

#' cnvlfc_coupled_logistic
#'
#' A step of the cnvlfc_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param n Passed to \code{.cnvlfc_coupled_logistic}.
#' @param rx Passed to \code{.cnvlfc_coupled_logistic}. Defaults to \code{3.8}.
#' @param ry Passed to \code{.cnvlfc_coupled_logistic}. Defaults to \code{3.5}.
#' @param bxy Passed to \code{.cnvlfc_coupled_logistic}. Defaults to \code{0}.
#' @param byx Passed to \code{.cnvlfc_coupled_logistic}. Defaults to \code{0.1}.
#' @param x0 Passed to \code{.cnvlfc_coupled_logistic}. Defaults to \code{0.4}.
#' @param y0 Passed to \code{.cnvlfc_coupled_logistic}. Defaults to \code{0.2}.
#' @param burn Passed to \code{.cnvlfc_coupled_logistic}. Defaults to \code{300L}.
#' @return The value of \code{.cnvlfc_coupled_logistic}.
#' @export
cnvlfc_coupled_logistic <- function(n, rx = 3.8, ry = 3.5, bxy = 0.0,
                                    byx = 0.1, x0 = 0.4, y0 = 0.2,
                                    burn = 300L) {
    .cnvlfc_coupled_logistic(n, rx, ry, bxy, byx, x0, y0, burn)
}

#' cnvlfc_convergent_cross_mapping
#'
#' A step of the cnvlfc_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param x Carried through into a list the body builds.
#' @param y Carried through into a list the body builds.
#' @param E Carried through into a list the body builds. Defaults to \code{2L}.
#' @param tau Carried through into a list the body builds. Defaults to \code{1L}.
#' @param ... Passed through.
#' @return The value of \code{do.call}.
#' @export
cnvlfc_convergent_cross_mapping <- function(x, y, E = 2L, tau = 1L, ...) {
    dots <- list(...)
    do.call(.cnvlfc_ccm,
            c(list(x = x, y = y, E = E, tau = tau), dots))
}

#' @rdname cnvlfc_embed
#' @export
morie_cnvlfc <- cnvlfc_embed






















