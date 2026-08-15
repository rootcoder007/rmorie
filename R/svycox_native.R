# morie.fn -- function file (rootcoder007/morie)
# Survey-weighted Cox regression, and why the variance is the hard
# part.
#
# **The point estimate is easy.** Weight each subject's contribution to
# the partial likelihood by its sampling weight. The score becomes
#
# .. math:: U(\beta) = \sum_i w_i \delta_i
#           \Bigl\{ x_i - \frac{\sum_{j \in R_i} w_j x_j
#           e^{x_j'\beta}}{\sum_{j \in R_i} w_j e^{x_j'\beta}}
#           \Bigr\}
#
# and Newton-Raphson on that gives :math:`\hat\beta`. This estimates
# the coefficient of the Cox model that would be fitted to the whole
# finite population -- a *census parameter*, which is what a survey
# analyst usually wants, and which is well defined whether or not the
# proportional hazards model is true.
#
# **The variance is not.** The inverse information :math:`I^{-1}` is
# the variance of a maximum likelihood estimator under independent
# sampling from the model. Under a survey design the observations are
# neither independent nor identically weighted, and :math:`I^{-1}`
# understates the variance whenever the design clusters and overstates
# it when it stratifies on something predictive. Binder's answer is a
# sandwich built from the *design*:
#
# .. math:: \hat{V}(\hat\beta) = I^{-1}
#           \, \hat{V}_{\text{design}}\Bigl(\sum_i w_i \hat{U}_i\Bigr)
#           \, I^{-1}
#
# where :math:`\hat{U}_i` is subject :math:`i`'s score residual (its
# own contribution to :math:`U` plus the contribution it makes to the
# risk sets of every earlier failure). The middle term is the
# design-based variance of a total -- ordinary stratified,
# between-cluster variance -- so the survey machinery is applied to the
# residuals rather than to the coefficient.
#
# **What that buys, concretely.** Clustering inflates the variance and
# the model-based standard error does not see it. The anchor builds a
# design where subjects within a cluster share their covariate, and
# shows Binder's standard error rising with the intra-cluster
# correlation while the model-based one does not move.
#
# **Ties.** Breslow's approximation, which is what weighting the
# partial likelihood naturally yields: each tied failure sees the full
# risk set. :mod:`morie.fn.coxph` uses Efron's approximation instead,
# so the two agree exactly when failure times are distinct and diverge
# slightly when they are not. The anchor exhibits both facts rather
# than only the agreeable one.
#
# **Two checks that must hold exactly.** With every weight equal to one
# and no design structure, :math:`\hat\beta` must equal the unweighted
# Cox estimate; and integer weights must give the same
# :math:`\hat\beta` as physically replicating those rows. Both are
# identities, and both are anchored.
#
# References
# ----------
# Binder, D. A. (1992) "Fitting Cox's proportional hazards models from
# survey data", *Biometrika* 79(1), 139-147,
# doi:10.1093/biomet/79.1.139. The weighted estimating equation above,
# the interpretation of :math:`\hat\beta` as a finite-population census
# parameter, and the sandwich variance with the design-based variance
# of the weighted score residuals in the middle.
#
# Cox, D. R. (1972) "Regression models and life-tables", *Journal of
# the Royal Statistical Society: Series B* 34(2), 187-202,
# doi:10.1111/j.2517-6161.1972.tb00899.x, for the partial likelihood
# being weighted; see :mod:`morie.fn.coxph` for the unweighted fit this
# must reduce to.

.svycox_prep <- function(time, event, X, weights, strata, cluster) {
    T <- as.numeric(time)
    E <- as.integer(event)
    n <- length(T)
    if (length(E) != n) {
        stop(sprintf("svycox: %d times but %d event indicators",
                     n, length(E)))
    }
    if (n < 2L) {
        stop("svycox: need at least two subjects")
    }
    if (any(E != 0L & E != 1L)) {
        stop("svycox: the event indicator must be 0 or 1")
    }
    if (any(T < 0)) {
        stop("svycox: a survival time cannot be negative")
    }
    if (is.data.frame(X)) {
        X <- as.matrix(X)
    }
    if (is.matrix(X)) {
        if (nrow(X) != n) {
            stop(sprintf("svycox: %d covariate rows but %d subjects",
                         nrow(X), n))
        }
        M <- lapply(seq_len(n), function(i) as.numeric(X[i, ]))
    } else {
        if (length(X) != n) {
            stop(sprintf("svycox: %d covariate rows but %d subjects",
                         length(X), n))
        }
        M <- lapply(X, function(row) as.numeric(unlist(row)))
    }
    p <- length(M[[1L]])
    if (p == 0L || any(sapply(M, length) != p)) {
        stop("svycox: the covariate matrix is ragged or empty")
    }
    if (is.null(weights)) {
        w <- rep(1.0, n)
    } else {
        w <- as.numeric(weights)
    }
    if (length(w) != n) {
        stop(sprintf("svycox: %d weights but %d subjects", length(w), n))
    }
    if (any(w <= 0)) {
        stop("svycox: sampling weights must be positive")
    }
    if (sum(E) == 0L) {
        stop("svycox: no events, so nothing is estimable")
    }
    if (is.null(strata)) {
        h <- rep("1", n)
    } else {
        h <- as.character(strata)
    }
    if (is.null(cluster)) {
        c_id <- as.character(seq_len(n) - 1L)
    } else {
        c_id <- as.character(cluster)
    }
    if (length(h) != n || length(c_id) != n) {
        stop("svycox: strata and cluster need one entry per subject")
    }
    return(list(T = T, E = E, M = M, w = w, h = h, c = c_id,
                n = n, p = p))
}

.svycox_score_and_info <- function(T, E, M, w, beta, n, p) {
    eta <- numeric(n)
    for (i in seq_len(n)) {
        Mi <- M[[i]]
        s <- 0.0
        for (k in seq_len(p)) {
            s <- s + Mi[k] * beta[k]
        }
        eta[i] <- s
    }
    r <- numeric(n)
    for (i in seq_len(n)) {
        r[i] <- w[i] * exp(eta[i])
    }
    U <- numeric(p)
    I_mat <- matrix(0.0, p, p)
    resid <- matrix(0.0, n, p)
    ord <- order(T)
    for (i_ord in seq_len(n)) {
        i <- ord[i_ord]
        if (E[i] == 0L) {
            next
        }
        risk <- ord[i_ord:n]
        s0 <- 0.0
        for (j in risk) {
            s0 <- s0 + r[j]
        }
        if (s0 <= 0) {
            next
        }
        s1 <- numeric(p)
        for (k in seq_len(p)) {
            ss <- 0.0
            for (j in risk) {
                ss <- ss + r[j] * M[[j]][k]
            }
            s1[k] <- ss
        }
        xbar <- s1 / s0
        Mi <- M[[i]]
        for (k in seq_len(p)) {
            U[k] <- U[k] + w[i] * (Mi[k] - xbar[k])
            resid[i, k] <- resid[i, k] + (Mi[k] - xbar[k])
        }
        for (k in seq_len(p)) {
            for (l in seq_len(p)) {
                s2 <- 0.0
                for (j in risk) {
                    s2 <- s2 + r[j] * M[[j]][k] * M[[j]][l]
                }
                I_mat[k, l] <- I_mat[k, l] +
                    w[i] * (s2 / s0 - xbar[k] * xbar[l])
            }
        }
        for (j in risk) {
            f <- w[i] * r[j] / s0
            denom <- max(w[j], 1e-300)
            Mj <- M[[j]]
            for (k in seq_len(p)) {
                resid[j, k] <- resid[j, k] -
                    f * (Mj[k] - xbar[k]) / denom
            }
        }
    }
    return(list(U = U, I = I_mat, resid = resid))
}

.svycox_solve <- function(A, b) {
    p <- length(b)
    Ab <- matrix(0.0, p, p + 1L)
    for (i in seq_len(p)) {
        for (j in seq_len(p)) {
            Ab[i, j] <- A[i, j]
        }
        Ab[i, p + 1L] <- b[i]
    }
    for (c in seq_len(p)) {
        piv <- c
        max_val <- abs(Ab[c, c])
        if (c < p) {
            for (r in (c + 1L):p) {
                if (abs(Ab[r, c]) > max_val) {
                    max_val <- abs(Ab[r, c])
                    piv <- r
                }
            }
        }
        if (abs(Ab[piv, c]) < 1e-14) {
            stop("svycox: the information matrix is singular. Either "
                 "a covariate is constant or collinear among the "
                 "failures, or the groups are completely separated -- "
                 "one always failing before the other -- in which "
                 "case the partial likelihood is monotone and no "
                 "finite estimate exists")
        }
        if (piv != c) {
            tmp <- Ab[c, ]
            Ab[c, ] <- Ab[piv, ]
            Ab[piv, ] <- tmp
        }
        for (r in seq_len(p)) {
            if (r == c) {
                next
            }
            f <- Ab[r, c] / Ab[c, c]
            for (k in c:(p + 1L)) {
                Ab[r, k] <- Ab[r, k] - f * Ab[c, k]
            }
        }
    }
    d <- diag(Ab)
    return(Ab[seq_len(p), p + 1L] / d)
}

.svycox_inverse <- function(A) {
    p <- nrow(A)
    out <- matrix(0.0, p, p)
    for (j in seq_len(p)) {
        e <- numeric(p)
        e[j] <- 1.0
        col <- .svycox_solve(A, e)
        for (i in seq_len(p)) {
            out[i, j] <- col[i]
        }
    }
    return(out)
}

morie_svycox_score_residuals <- function(time, event, X, beta,
                                          weights = NULL) {
    prep <- .svycox_prep(time, event, X, weights, NULL, NULL)
    T <- prep$T
    E <- prep$E
    M <- prep$M
    w <- prep$w
    n <- prep$n
    p <- prep$p
    beta <- as.numeric(beta)
    return(.svycox_score_and_info(T, E, M, w, beta, n, p)$resid)
}

.svycox_design_variance <- function(contrib, w, h, c_id, p) {
    cells <- list()
    for (i in seq_along(w)) {
        key <- paste(h[i], c_id[i], sep = "\r")
        if (is.null(cells[[key]])) {
            cells[[key]] <- numeric(p)
        }
        for (k in seq_len(p)) {
            cells[[key]][k] <- cells[[key]][k] + w[i] * contrib[i, k]
        }
    }
    by_h <- list()
    for (key in names(cells)) {
        parts <- strsplit(key, "\r", fixed = TRUE)[[1]]
        hh <- parts[1]
        if (is.null(by_h[[hh]])) {
            by_h[[hh]] <- list()
        }
        by_h[[hh]][[length(by_h[[hh]]) + 1L]] <- cells[[key]]
    }
    V <- matrix(0.0, p, p)
    for (hh in names(by_h)) {
        vs <- by_h[[hh]]
        nh <- length(vs)
        if (nh < 2L) {
            stop(sprintf("svycox: stratum %s has a single cluster, "
                         "so its variance contribution is not "
                         "estimable", hh))
        }
        mean_v <- numeric(p)
        for (k in seq_len(p)) {
            s <- 0.0
            for (v in vs) {
                s <- s + v[k]
            }
            mean_v[k] <- s / nh
        }
        f <- nh / (nh - 1)
        for (v in vs) {
            for (k in seq_len(p)) {
                for (l in seq_len(p)) {
                    V[k, l] <- V[k, l] +
                        f * (v[k] - mean_v[k]) * (v[l] - mean_v[l])
                }
            }
        }
    }
    return(V)
}

morie_svycox <- function(time, event, X, weights = NULL, strata = NULL,
                         cluster = NULL, max_iter = 100, tol = 1e-9) {
    prep <- .svycox_prep(time, event, X, weights, strata, cluster)
    T <- prep$T
    E <- prep$E
    M <- prep$M
    w <- prep$w
    h <- prep$h
    c_id <- prep$c
    n <- prep$n
    p <- prep$p
    beta <- numeric(p)
    hist <- numeric(0)
    converged <- FALSE
    for (iter in seq_len(as.integer(max_iter))) {
        si <- .svycox_score_and_info(T, E, M, w, beta, n, p)
        U <- si$U
        I_mat <- si$I
        step <- .svycox_solve(I_mat, U)
        beta <- beta + step
        cur <- max(abs(step))
        hist <- c(hist, cur)
        if (cur < tol) {
            converged <- TRUE
            break
        }
    }
    if (!converged) {
        stop(sprintf("svycox: Newton-Raphson did not converge in %d "
                     "iterations (last step %.3g)",
                     as.integer(max_iter), hist[length(hist)]))
    }
    si <- .svycox_score_and_info(T, E, M, w, beta, n, p)
    U <- si$U
    I_mat <- si$I
    resid <- si$resid
    Iinv <- .svycox_inverse(I_mat)
    Vd <- .svycox_design_variance(resid, w, h, c_id, p)
    V <- Iinv %*% Vd %*% Iinv
    se <- numeric(p)
    se_model <- numeric(p)
    de <- numeric(p)
    z <- numeric(p)
    hr <- numeric(p)
    for (k in seq_len(p)) {
        if (V[k, k] > 0) {
            se[k] <- sqrt(V[k, k])
        } else {
            se[k] <- NaN
        }
        if (Iinv[k, k] > 0) {
            se_model[k] <- sqrt(Iinv[k, k])
        } else {
            se_model[k] <- NaN
        }
        if (se_model[k] > 0) {
            de[k] <- (se[k] / se_model[k])^2
        } else {
            de[k] <- NaN
        }
        if (se[k] > 0) {
            z[k] <- beta[k] / se[k]
        } else {
            z[k] <- NaN
        }
        hr[k] <- exp(beta[k])
    }
    return(list(
        estimate = beta,
        coefficients = beta,
        hazard_ratios = hr,
        std_errors = se,
        model_std_errors = se_model,
        vcov = V,
        information = I_mat,
        score = U,
        design_effect = de,
        z = z,
        n = n,
        n_events = sum(E),
        n_iterations = length(hist),
        ties = "breslow",
        method = "Binder (1992) weighted partial likelihood with a "
                 "design-based sandwich variance"
    ))
}

morie_svycox_survey_cox <- function(time, event, X, weights = NULL,
                                    strata = NULL, cluster = NULL, ...) {
    morie_svycox(time, event, X, weights, strata, cluster, ...)
}
