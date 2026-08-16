# Measuring fairness in ranked outputs.
# Sources: Yang, K. & Stoyanovich, J. (2017) "Measuring Fairness in
# Ranked Outputs", *Proceedings of the 29th International Conference
# on Scientific and Statistical Database Management (SSDBM '17)*,
# doi:10.1145/3085504.3085526, arXiv:1610.08559. Sec. 3 (statistical
# parity for rankings; set-based fairness computed at discrete
# cut-offs with a logarithmic discount; normalisation to [0,1] with 0
# the fairest; rND, rKL and rRD definitions) and the discussion of
# Figures 3-5 (rND and rKL treat the two groups symmetrically and
# are best when the top-i share matches the population share, while
# rRD is only applicable when the protected group is the minority).
# Jarvelin, K. & Kekalainen, J. (2002) "Cumulated gain-based
# evaluation of IR techniques", *ACM Transactions on Information
# Systems* 20(4), 422-446, doi:10.1145/582415.582418, for the
# logarithmic discount.
#
# Native implementation mirroring Python morie.fn.fairRC exactly: the
# same cutoffs at multiples of step from step to N, the same 1/log2(i)
# discount, the same rND (top-i share vs population share), rKL
# (Bernoulli KL with 1e-12 clamp) and rRD (ratio of S+ to S-)
# formulas, the same normaliser (protected group placed entirely
# last), the same rejection of single-group rankings, and the same
# caveat attached to rRD when the protected group is the majority.

.FAIRRC_EPS <- 1e-12
.FAIRRC_MEASURES <- c("rND", "rKL", "rRD")

#' cutoffs
#'
#' Part of the fairRC_native implementation; see the file header for the
#' source it follows.
#'
#' @param N See Usage.
#' @param step Defaults to \code{10}.
#' @return The value of \code{seq}.
#' @export
cutoffs <- function(N, step = 10) {
  n <- as.integer(N); s <- as.integer(step)
  if (n < s)
    stop("fairRC: the ranking of ", n, " is shorter than the first ",
         "cut-off ", s)
  seq(s, n, by = s)
}

.shares <- function(protected, i) {
  sum(protected[seq_len(i)]) / as.numeric(i)
}

.raw <- function(protected, measure, step) {
  N <- length(protected)
  P <- sum(protected) / as.numeric(N)
  tot <- 0
  for (i in cutoffs(N, step)) {
    p <- .shares(protected, i)
    w <- 1 / log(i, base = 2)
    if (measure == "rND") {
      tot <- tot + w * abs(p - P)
    } else if (measure == "rKL") {
      a <- min(max(p, .FAIRRC_EPS), 1 - .FAIRRC_EPS)
      b <- min(max(P, .FAIRRC_EPS), 1 - .FAIRRC_EPS)
      tot <- tot + w *
        (a * log(a / b) + (1 - a) * log((1 - a) / (1 - b)))
    } else {
      npos <- sum(protected[seq_len(i)])
      nneg <- i - npos
      r1 <- if (nneg == 0 || npos == 0) 0 else npos / as.numeric(nneg)
      NP <- sum(protected); NN <- N - NP
      r2 <- if (NN == 0 || NP == 0) 0 else NP / as.numeric(NN)
      tot <- tot + w * abs(r1 - r2)
    }
  }
  tot
}

#' Z: the value of the worst arrangement. The protected group placed
#'
#' entirely last maximises the deviation at every cut-off.
#'
#' @param protected See Usage.
#' @param measure Defaults to \code{"rND"}.
#' @param step Defaults to \code{10}.
#' @return One of two values, depending on the branch taken.
#' @export
normalizer <- function(protected, measure = "rND", step = 10) {
  # Z: the value of the worst arrangement. The protected group placed
  # entirely last maximises the deviation at every cut-off.
  n <- length(protected)
  npos <- sum(as.integer(protected))
  worst <- c(rep(0L, n - npos), rep(1L, npos))
  z <- .raw(worst, measure, step)
  if (z > .FAIRRC_EPS) z else 1
}

.measure <- function(protected, measure, step, normalize,
                      caveat = NULL) {
  p <- as.integer(as.logical(protected))
  if (!(measure %in% .FAIRRC_MEASURES))
    stop("fairRC: measure must be one of ",
         paste(.FAIRRC_MEASURES, collapse = ", "), ", got ",
         deparse(measure))
  if (length(p) == 0L)
    stop("fairRC: the ranking is empty")
  sp <- sum(p)
  if (sp == 0L || sp == length(p))
    stop("fairRC: fairness is undefined when every item is in one ",
         "group")
  raw <- .raw(p, measure, step)
  z <- if (normalize) normalizer(p, measure, step) else 1
  pay <- list(
    estimate = raw / z,
    value = raw / z,
    raw = raw,
    normalizer = z,
    measure = measure,
    protected_share = sp / as.numeric(length(p)),
    cutoffs = cutoffs(length(p), step),
    method = "Yang & Stoyanovich (2017) Sec. 3",
    note = paste0("0 is fairest; the best value is reached when the ",
                  "top-i share matches the POPULATION share, not ",
                  "50/50")
  )
  if (!is.null(caveat)) pay$caveat <- caveat
  pay
}

#' Normalised discounted difference
#'
#' Part of the fairRC_native implementation; see the file header for the
#' source it follows.
#'
#' @param protected See Usage.
#' @param step Defaults to \code{10}.
#' @param normalize Defaults to \code{TRUE}.
#' @return The value of \code{.measure}.
#' @export
rND <- function(protected, step = 10, normalize = TRUE) {
  # Normalised discounted difference.
  .measure(protected, "rND", step, normalize)
}

#' Normalised discounted KL divergence
#'
#' Part of the fairRC_native implementation; see the file header for the
#' source it follows.
#'
#' @param protected See Usage.
#' @param step Defaults to \code{10}.
#' @param normalize Defaults to \code{TRUE}.
#' @return The value of \code{.measure}.
#' @export
rKL <- function(protected, step = 10, normalize = TRUE) {
  # Normalised discounted KL divergence.
  .measure(protected, "rKL", step, normalize)
}

#' Normalised discounted ratio. Only meaningful when the protected
#'
#' group is the minority -- it does not treat the two groups
#' symmetrically.
#'
#' @param protected See Usage.
#' @param step Defaults to \code{10}.
#' @param normalize Defaults to \code{TRUE}.
#' @return The value of \code{.measure}.
#' @export
rRD <- function(protected, step = 10, normalize = TRUE) {
  # Normalised discounted ratio. Only meaningful when the protected
  # group is the minority -- it does not treat the two groups
  # symmetrically.
  p <- as.integer(as.logical(protected))
  cav <- NULL
  if (sum(p) > 0.5 * length(p))
    cav <- paste0("rRD is NOT APPLICABLE here: the protected group is ",
                  "the MAJORITY, and rRD does not treat the two ",
                  "groups symmetrically")
  .measure(p, "rRD", step, normalize, cav)
}

.fairRC_cheatsheet <- function() {
  paste0("fairRC: statistical parity for RANKINGS -- did group ",
         "membership influence POSITION. Set-based fairness at ",
         "top-10, top-20, ... with a 1/log2(i) discount, so ",
         "unfairness at the top costs more (the nDCG idea). rND uses ",
         "|share_top_i - share_population|, rKL the KL divergence, ",
         "rRD the ratio of S+ to S-. All in [0,1], 0 is fairest, and ",
         "best when the top-i share matches the POPULATION share -- ",
         "20% of the population is fairly served by 20%, not 50%. rRD ",
         "is asymmetric and applies only when the protected group is ",
         "the minority.")
}

# compact alias per ledger/NAMING.md
fairranking <- rND

# public names resolved by fn/_lazy_map.json
fairness_rec <- rND
fairnessrec <- rND

# morie entry point
#' Morie entry point
#'
#' Part of the fairRC_native implementation; see the file header for the
#' source it follows.
#'
#' @param protected See Usage.
#' @param measure Defaults to \code{"rND"}.
#' @param step Defaults to \code{10}.
#' @param normalize Defaults to \code{TRUE}.
#' @return One of two values, depending on the branch taken.
#' @export
morie_fairRC <- function(protected, measure = "rND", step = 10,
                         normalize = TRUE) {
  if (!(measure %in% .FAIRRC_MEASURES))
    stop("fairRC: measure must be one of ",
         paste(.FAIRRC_MEASURES, collapse = ", "), ", got ",
         deparse(measure))
  if (measure == "rND") rND(protected, step, normalize)
  else if (measure == "rKL") rKL(protected, step, normalize)
  else rRD(protected, step, normalize)
}
