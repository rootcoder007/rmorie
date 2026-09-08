# morie.fn -- function file (rootcoder007/morie)
# r"""Causal inference in longitudinal network-dependent data.
#
# Almost all causal inference assumes :math:`n` independent units that
# are not causally connected: the intervention on one unit cannot affect
# another's outcome, so the causal model only has to describe relations
# *within* a unit, and inference rests on :math:`n` independent
# realisations. In many cluster trials and in observational studies of a
# few communities, the number of independent units is simply not large
# enough for that limit to mean anything.
#
# **The extreme case is the one worth stating.** One community of
# causally connected individuals. Can the effect of a
# community-level intervention on a community-level outcome -- the
# average of individual outcomes, say -- still be evaluated? The chapter's
# answer is yes, but only if the causal model covers **all** units at
# once and identifiability is established *without* appealing to
# asymptotics in a number of independent units.
#
# **Where the replication comes from instead.** Individuals within the
# community, whose dependence is restricted by the known network: unit
# :math:`i` is influenced only by its friends :math:`F_i`, and
# :math:`|F_i|/N \to 0`. That is what makes an average over individuals
# behave like an average over weakly dependent terms, so a central limit
# theorem applies in :math:`N` even with a single community. The
# condition is a real one and can fail: a hub connected to a constant
# fraction of the network breaks it, and ``network_summary`` reports the
# maximum degree share for exactly that reason.
#
# **Longitudinally**, the dependence compounds: a unit's covariates at
# time :math:`t` may respond to its friends' treatments at
# :math:`t-1`. The intervention is on the whole network at each time,
# and the estimand is the mean community outcome under that policy.
#
# References
# ----------
# van der Laan, M. J. & Rose, S. (2018) *Targeted Learning in Data
# Science*, Springer, doi:10.1007/978-3-319-65304-4. Chap. 20 (Sofrygin
# & van der Laan): existing causal inference assumes n independent,
# causally unconnected units, so causal models need only describe
# relations within a unit and inference rests on n independent
# realisations; in many cluster randomized trials or observational
# studies of few communities the number of independent units is not
# large enough for limit-distribution inference; the extreme case of a
# single community of causally connected individuals and whether a
# community-level intervention's effect on a community-level outcome can
# still be evaluated; and the requirement that causal models incorporate
# all units and that identifiability be established under minimal
# assumptions WITHOUT relying on asymptotics in a number of independent
# units. Chap. 21 (the restriction of dependence to the known network
# and the condition that |F_i|/N vanishes).
#
# Sofrygin, O. & van der Laan, M. J. (2017) "Semi-Parametric Estimation
# and Inference for the Mean Outcome of the Single Time-Point
# Intervention in a Causally Connected Population", *Journal of Causal
# Inference* 5(1), 20160003, doi:10.1515/jci-2016-0003.
#
# Ogburn, E. L. & VanderWeele, T. J. (2014) "Causal Diagrams for
# Interference", *Statistical Science* 29(4), 559-578,
# doi:10.1214/14-STS501.
# """

.tlnetlg_EPS <- 1e-12

# Private helpers (prefixed with .tlnetlg_ to avoid name collisions)
#' Private helpers (prefixed with .tlnetlg_ to avoid name collisions)
#'
#' A step of the tlnetlg_native implementation. Called by
#' \code{morie_tlnetlg_exposure_summary}, \code{morie_tlnetlg_network_variance}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param x Optional; may be \code{NULL}. A list; the body checks with \code{is.list}.
#' @return One of two values, depending on the branch taken.
#' @export
#' @examples
#' x <- c(1.2, 2.4, 3.1, 4.8, 5.3, 6.7, 7.1, 8.9)
#' res <- .tlnetlg_vec(x = x)
#' res
.tlnetlg_vec <- function(x) {
  if (is.null(x)) return(numeric(0))
  if (is.list(x) && !is.data.frame(x)) {
    unlist(lapply(x, as.numeric))
  } else {
    as.numeric(x)
  }
}

#' .tlnetlg_mat
#'
#' A step of the tlnetlg_native implementation. Called by
#' \code{morie_tlnetlg_community_estimand},
#' \code{morie_tlnetlg_longitudinal_network_gcomp}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param W A matrix; passed to \code{as.matrix}.
#' @return The value of \code{do.call}.
#' @export
#' @examples
#' x <- c(1.2, 2.4, 3.1, 4.8, 5.3, 6.7, 7.1, 8.9)
#' res <- .tlnetlg_mat(W = x)
#' res
.tlnetlg_mat <- function(W) {
  if (is.matrix(W)) {
    storage.mode(W) <- "double"
    return(W)
  }
  if (is.data.frame(W)) return(as.matrix(W))
  rows <- lapply(W, function(r) {
    if (is.list(r)) unlist(lapply(r, as.numeric)) else as.numeric(r)
  })
  do.call(rbind, rows)
}

#' morie_tlnetlg_network_summary
#'
#' A step of the tlnetlg_native implementation. Called by
#' \code{morie_tlnetlg_longitudinal_network_gcomp}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param friends A vector; its length is taken and its elements indexed.
#' @return A list with \code{N}, \code{degrees}, \code{max_degree}, \code{mean_degree},
#' \code{max_share}, \code{sparse}, \code{note}.
#' @export
morie_tlnetlg_network_summary <- function(friends) {
  N <- length(friends)
  if (N < 2) stop("tlnetlg: at least 2 units are needed")

  friends <- lapply(friends, function(f) as.integer(unlist(f)))

  deg <- vapply(seq_len(N), function(i) {
    length(setdiff(friends[[i]], i))
  }, integer(1))

  mx <- max(deg)
  Nf <- as.numeric(N)

  list(
    N = as.integer(N),
    degrees = as.integer(deg),
    max_degree = as.integer(mx),
    mean_degree = sum(deg) / Nf,
    max_share = mx / Nf,
    sparse = (mx / Nf) < 0.25,
    note = "dependence must be limited to the known network and |F_i|/N must vanish"
  )
}

#' morie_tlnetlg_exposure_summary
#'
#' A step of the tlnetlg_native implementation. Called by \code{morie_tlnetlg_community_estimand}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param A Passed to \code{.tlnetlg_vec}.
#' @param friends A vector; its length is taken and its elements indexed.
#' @param kind One of \code{"any"}, \code{"count"}, \code{"fraction"}. Defaults to
#' \code{"fraction"}.
#' @return A list with \code{summary}, \code{kind}, \code{note}.
#' @export
morie_tlnetlg_exposure_summary <- function(A, friends, kind = "fraction") {
  a <- .tlnetlg_vec(A)
  N <- length(a)
  if (length(friends) != N) {
    stop(sprintf("tlnetlg: %d treatments but %d friend sets",
                 N, length(friends)))
  }

  friends <- lapply(friends, function(f) as.integer(unlist(f)))

  out <- vector("list", N)
  for (i in seq_len(N)) {
    f <- sort(setdiff(friends[[i]], i))
    s <- 0.0
    if (length(f) > 0L) {
      if (kind == "fraction") {
        s <- sum(a[f]) / length(f)
      } else if (kind == "count") {
        s <- sum(a[f])
      } else if (kind == "any") {
        s <- if (any(a[f] == 1.0)) 1.0 else 0.0
      } else {
        stop(sprintf("tlnetlg: kind must be fraction, count or any, got %s",
                     kind))
      }
    }
    out[[i]] <- list(a[i], s)
  }

  list(
    summary = out,
    kind = kind,
    note = "own treatment plus a fixed-dimensional summary of the friends'"
  )
}

#' morie_tlnetlg_community_estimand
#'
#' A step of the tlnetlg_native implementation. Called by
#' \code{morie_tlnetlg_longitudinal_network_gcomp}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param Q_fn Accepted by the signature and not used anywhere in the body.
#' @param friends A vector; its length is taken.
#' @param W Passed to \code{.tlnetlg_mat}.
#' @param policy Accepted by the signature and not used anywhere in the body.
#' @return A list with \code{psi}, \code{assigned}, \code{individual}, \code{N}.
#' @export
morie_tlnetlg_community_estimand <- function(Q_fn, friends, W, policy) {
  rows <- .tlnetlg_mat(W)
  N <- nrow(rows)
  if (length(friends) != N) {
    stop(sprintf("tlnetlg: %d covariate rows but %d friend sets",
                 N, length(friends)))
  }

  friends <- lapply(friends, function(f) as.integer(unlist(f)))

  a <- numeric(N)
  for (i in seq_len(N)) {
    a[i] <- as.numeric(policy(i, rows))
  }

  es <- morie_tlnetlg_exposure_summary(a, friends)[["summary"]]

  vals <- numeric(N)
  for (i in seq_len(N)) {
    vals[i] <- as.numeric(Q_fn(es[[i]][[1]], es[[i]][[2]], rows[i, ]))
  }

  list(
    psi = sum(vals) / N,
    assigned = a,
    individual = vals,
    N = as.integer(N)
  )
}

#' morie_tlnetlg_network_variance
#'
#' A step of the tlnetlg_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param ic Passed to \code{.tlnetlg_vec}.
#' @param friends A vector; its length is taken and its elements indexed.
#' @return A list with \code{se}, \code{se_naive}, \code{n_dependent_pairs},
#' \code{ratio}, \code{note}.
#' @export
morie_tlnetlg_network_variance <- function(ic, friends) {
  v <- .tlnetlg_vec(ic)
  N <- length(v)
  if (length(friends) != N) {
    stop(sprintf("tlnetlg: %d influence values but %d friend sets",
                 N, length(friends)))
  }

  friends <- lapply(friends, function(f) as.integer(unlist(f)))

  m <- sum(v) / N
  var <- sum((v - m) ^ 2) / N
  cov <- 0.0
  pairs <- 0L
  for (i in seq_len(N)) {
    fj <- setdiff(friends[[i]], i)
    for (j in fj) {
      cov <- cov + (v[i] - m) * (v[j] - m)
      pairs <- pairs + 1L
    }
  }

  total <- (var + cov / N) / N
  naive <- var / N

  se <- sqrt(max(total, 0.0))
  se_naive <- sqrt(naive)

  ratio <- if (naive > .tlnetlg_EPS) sqrt(max(total, 0.0) / naive) else NaN

  list(
    se = se,
    se_naive = se_naive,
    n_dependent_pairs = as.integer(pairs),
    ratio = ratio,
    note = "only CONNECTED pairs contribute covariance; treating units as independent drops them"
  )
}

#' morie_tlnetlg_longitudinal_network_gcomp
#'
#' A step of the tlnetlg_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param Q_seq A vector; its length is taken and its elements indexed.
#' @param friends Passed to \code{morie_tlnetlg_community_estimand}.
#' @param W Passed to \code{.tlnetlg_mat}.
#' @param policy Passed to \code{morie_tlnetlg_community_estimand}.
#' @param T Coerced to integer by the body, with \code{as.integer}.
#' @return A list with \code{estimate}, \code{psi}, \code{path}, \code{T},
#' \code{network}, \code{method}, \code{note}.
#' @export
morie_tlnetlg_longitudinal_network_gcomp <- function(Q_seq, friends, W, policy, T) {
  T_int <- as.integer(T)
  if (T_int < 1L) stop("tlnetlg: need at least one time point")
  if (length(Q_seq) != T_int) {
    stop(sprintf("tlnetlg: %d regressions for %d time points",
                 length(Q_seq), T_int))
  }

  cur <- .tlnetlg_mat(W)
  path <- numeric(T_int)

  for (t in seq_len(T_int)) {
    r <- morie_tlnetlg_community_estimand(Q_seq[[t]], friends, cur, policy)
    path[t] <- r$psi
    new_cur <- matrix(0, nrow = nrow(cur), ncol = ncol(cur) + 1L)
    for (i in seq_len(nrow(cur))) {
      new_cur[i, ] <- c(r$individual[i], cur[i, ])
    }
    cur <- new_cur
  }

  list(
    estimate = path[T_int],
    psi = path[T_int],
    path = path,
    T = T_int,
    network = morie_tlnetlg_network_summary(friends),
    method = "longitudinal network g-computation; van der Laan & Rose (2018) Chap. 20",
    note = "replication comes from weakly dependent INDIVIDUALS, not from independent communities"
  )
}

#' morie_tlnetlg_cheatsheet
#'
#' A step of the tlnetlg_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @return A character value.
#' @export
morie_tlnetlg_cheatsheet <- function() {
  paste("tlnetlg: standard causal inference assumes n independent,",
        "causally unconnected units -- useless when you observe",
        "ONE community of connected individuals. Model ALL units",
        "jointly and establish identifiability WITHOUT asymptotics",
        "in independent units. Replication comes from individuals",
        "whose dependence is restricted to the known network, with",
        "|F_i|/N -> 0; a hub connected to a constant fraction",
        "breaks that and no N repairs it. Variance must add the",
        "covariance of CONNECTED pairs.")
}

# compact alias per ledger/NAMING.md
morie_tlnetlg_networklongitudinal <- morie_tlnetlg_longitudinal_network_gcomp

# Main entry point
morie_tlnetlg <- morie_tlnetlg_longitudinal_network_gcomp
