# morie.fn -- function file (rootcoder007/morie)
# R arm of snmtst (identified_set, sensitivity_curve, breakdown_value,
# fixed_length_ci).
# Sources:
#   Rambachan, A. & Roth, J. (2023) "A More Credible Approach to
#   Parallel Trends", Review of Economic Studies 90(5), 2555-2591,
#   doi:10.1093/restud/rdad018. Sec. 2.1 (event-study coefficients,
#   incl. Example 2 for staggered designs), Assumption 1 (the
#   tau/delta decomposition), Sec. 2.3 (Delta^SD and Delta^RM),
#   Sec. 3 (the identified set and fixed-length confidence sets), and
#   the breakdown value.
#   Callaway, B. & Sant'Anna, P. H. C. (2021) "Difference-in-
#   Differences with multiple time periods", Journal of Econometrics
#   225(2), 200-230, doi:10.1016/j.jeconom.2020.12.001. The
#   staggered-adoption event-study coefficients this sensitivity
#   analysis is applied to in Example 2.
#   Wager, S. (2025) Causal Inference: A Statistical Learning Approach,
#   Stanford University, draft of 26 November 2025. Chapter 13,
#   Assumption 13.2, for the parallel-trends assumption being relaxed
#   here.

.SNMTST_EPS <- 1e-12
.SNMTST_FAMILIES <- c("SD", "RM")

#' .snmtst_split
#'
#' A step of the snmtst_native implementation. Called by \code{identified_set}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param beta Coerced to numeric by the body, with \code{as.numeric}.
#' @param n_pre A count; the body uses it as \code{seq_len(...)}.
#' @param n_post A count; the body uses it as \code{seq_len(...)}.
#' @return A list with \code{pre}, \code{post}.
#' @export
.snmtst_split <- function(beta, n_pre, n_post) {
  b <- as.numeric(beta)
  if (length(b) != as.integer(n_pre) + as.integer(n_post))
    stop(sprintf("snmtst: %d coefficients but n_pre + n_post = %d",
                 length(b), as.integer(n_pre) + as.integer(n_post)))
  if (n_pre < 1L || n_post < 1L)
    stop(sprintf("snmtst: need at least one pre and one post period, got %d and %d",
                 n_pre, n_post))
  list(pre = b[seq_len(n_pre)], post = b[n_pre + seq_len(n_post)])
}

#' .snmtst_target
#'
#' A step of the snmtst_native implementation. Called by \code{identified_set}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param post A vector; its length is taken.
#' @param l_vec Optional; may be \code{NULL}. Coerced to numeric by the body, with
#' \code{as.numeric}.
#' @return The value of \code{lv}, as built in the body.
#' @export
.snmtst_target <- function(post, l_vec) {
  Tp <- length(post)
  if (is.null(l_vec))
    return(c(1.0, rep(0.0, Tp - 1L)))
  lv <- as.numeric(l_vec)
  if (length(lv) != Tp)
    stop(sprintf("snmtst: the target vector has %d entries for %d post periods",
                 length(lv), Tp))
  lv
}

#' identified_set
#'
#' A step of the snmtst_native implementation. Called by \code{breakdown_value},
#' \code{fixed_length_ci}, \code{sensitivity_curve}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param beta Passed to \code{.snmtst_split}.
#' @param n_pre Passed to \code{.snmtst_split}.
#' @param n_post Passed to \code{.snmtst_split}.
#' @param M Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0}.
#' @param family Compared against \code{"SD"}. Defaults to \code{"SD"}.
#' @param l_vec Passed to \code{.snmtst_target}.
#' @param grid Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.
#' @return A list with \code{lower}, \code{upper}, \code{estimate},
#' \code{pre_max_change}, \code{bound}, \code{M}, \code{family}, \code{width},
#' \code{note}.
#' @export
identified_set <- function(beta, n_pre, n_post, M = 0.0, family = "SD",
                           l_vec = NULL, grid = NULL) {
  if (!(family %in% .SNMTST_FAMILIES))
    stop(sprintf("snmtst: family must be SD or RM, got %r", family))
  Mv <- as.numeric(M)
  if (Mv < 0.0)
    stop(sprintf("snmtst: M must be non-negative, got %r", M))
  sp <- .snmtst_split(beta, n_pre, n_post)
  pre <- sp$pre
  post <- sp$post
  lv <- .snmtst_target(post, l_vec)
  Tp <- length(post)
  if (family == "SD") {
    if (length(pre) < 2L)
      stop(sprintf("snmtst: Delta^SD needs at least 2 pre-periods to define a slope, got %d",
                   length(pre)))
    base <- pre[length(pre)]
    slope <- pre[length(pre)] - pre[length(pre) - 1L]
    lin <- vapply(seq_len(Tp), function(t)
      base + slope * t, numeric(1))
    dev <- vapply(seq_len(Tp), function(t)
      Mv * (t + 1L) * (t + 2L) / 2.0, numeric(1))
    c_coef <- vapply(seq_len(Tp), function(j) {
      ts <- seq.int(j - 1L, Tp - 1L)
      sum(lv[ts + 1L] * (ts - (j - 1L) + 2L))
    }, numeric(1))
    point <- sum(lv * (post - lin))
    if (is.null(grid)) {
      half <- Mv * sum(abs(c_coef))
      lo <- point - half
      hi <- point + half
    } else {
      br <- .snmtst_brute(point, c_coef, Mv, as.integer(grid),
                          post = post, lin = lin, lv = lv)
      lo <- br$lo
      hi <- br$hi
    }
    return(list(lower = lo, upper = hi, estimate = point,
                linear_path = lin, max_deviation = dev,
                coefficients = c_coef, M = Mv, family = "SD",
                width = hi - lo,
                note = "M = 0 is exactly linear extrapolation from the pre-period"))
  }
  if (length(pre) < 2L)
    stop(sprintf("snmtst: Delta^RM needs at least 2 pre-periods to form a first difference, got %d",
                 length(pre)))
  scale <- max(abs(diff(pre)))
  bound <- Mv * scale
  centre <- sum(lv * post)
  hi <- centre + bound * sum(abs(lv))
  lo <- centre - bound * sum(abs(lv))
  list(lower = lo, upper = hi, estimate = centre,
       pre_max_change = scale, bound = bound, M = Mv, family = "RM",
       width = hi - lo,
       note = "M = 1 says the post violation is no larger than the largest observed pre-period change")
}

#' .snmtst_brute
#'
#' A step of the snmtst_native implementation. Called by \code{identified_set}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param point Numeric; combined arithmetically in the body.
#' @param c A vector; its length is taken.
#' @param M Numeric; combined arithmetically in the body.
#' @param grid A count; the body uses it as \code{seq_len(...)}.
#' @param post Optional; may be \code{NULL}. Numeric; combined arithmetically in the body.
#' @param lin Optional; may be \code{NULL}. Numeric; combined arithmetically in the body.
#' @param lv Optional; may be \code{NULL}. Numeric; combined arithmetically in the body.
#' @return A list with \code{lo}, \code{hi}.
#' @export
.snmtst_brute <- function(point, c, M, grid, post = NULL, lin = NULL,
                          lv = NULL) {
  if (grid < 2L)
    stop(sprintf("snmtst: the brute-force grid needs at least 2 points per coordinate, got %d",
                 grid))
  p <- length(c)
  if (grid ^ p > 2000000L)
    stop(sprintf("snmtst: a %d-point grid over %d coordinates is %d evaluations -- refuse rather than hang",
                 grid, p, grid ^ p))
  steps <- vapply(seq_len(grid) - 1L,
                  function(j) -M + 2.0 * M * j / (grid - 1L),
                  numeric(1))
  idx <- rep(0L, p)
  lo <- NULL
  hi <- NULL
  direct <- !is.null(post) && !is.null(lin) && !is.null(lv)
  repeat {
    if (direct) {
      r <- steps[idx + 1L]
      e_prev2 <- 0.0
      e_prev <- 0.0
      e <- numeric(p)
      for (t in seq_len(p)) {
        cur <- 2.0 * e_prev - e_prev2 + r[t]
        e[t] <- cur
        e_prev2 <- e_prev
        e_prev <- cur
      }
      val <- sum(lv * (post - (lin + e)))
    } else {
      val <- point - sum(c * steps[idx + 1L])
    }
    if (is.null(lo)) { lo <- val
    hi <- val }
    else { lo <- min(lo, val)
    hi <- max(hi, val) }
    q <- p
    while (q >= 1L) {
      idx[q] <- idx[q] + 1L
      if (idx[q] < grid) break
      idx[q] <- 0L
      q <- q - 1L
    }
    if (q < 1L) break
  }
  list(lo = lo, hi = hi)
}

#' sensitivity_curve
#'
#' A step of the snmtst_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param beta Passed to \code{identified_set}.
#' @param n_pre Passed to \code{identified_set}.
#' @param n_post Passed to \code{identified_set}.
#' @param Ms Iterated over elementwise, with \code{lapply}.
#' @param family Carried through into a list the body builds. Defaults to \code{"SD"}.
#' @param l_vec Passed to \code{identified_set}.
#' @return A list with \code{curve}, \code{family}, \code{M}, \code{width}.
#' @export
sensitivity_curve <- function(beta, n_pre, n_post, Ms, family = "SD",
                              l_vec = NULL) {
  curve <- lapply(Ms, function(M) {
    s <- identified_set(beta, n_pre, n_post, M = M, family = family,
                         l_vec = l_vec)
    list(M = s$M, lower = s$lower, upper = s$upper, width = s$width)
  })
  list(curve = curve, family = family,
       M = vapply(curve, function(o) o$M, numeric(1)),
       width = vapply(curve, function(o) o$width, numeric(1)))
}

#' breakdown_value
#'
#' A step of the snmtst_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param beta Passed to \code{identified_set}.
#' @param n_pre Passed to \code{identified_set}.
#' @param n_post Passed to \code{identified_set}.
#' @param family Carried through into a list the body builds. Defaults to \code{"SD"}.
#' @param l_vec Passed to \code{identified_set}.
#' @param sign One of \code{"negative"}, \code{"positive"}. Defaults to \code{"positive"}.
#' @param M_max Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{10}.
#' @param tol Passed to \code{>}. Defaults to \code{1e-09}.
#' @return A list with \code{breakdown}, \code{family}, \code{sign}, \code{status}.
#' @export
breakdown_value <- function(beta, n_pre, n_post, family = "SD",
                            l_vec = NULL, sign = "positive",
                            M_max = 10.0, tol = 1e-9) {
  if (!(sign %in% c("positive", "negative")))
    stop(sprintf("snmtst: sign must be positive or negative, got %r", sign))
  holds <- function(M) {
    s <- identified_set(beta, n_pre, n_post, M = M, family = family,
                        l_vec = l_vec)
    if (sign == "positive") s$lower > 0.0 else s$upper < 0.0
  }
  if (!holds(0.0))
    return(list(breakdown = 0.0, family = family, sign = sign,
                status = "the conclusion fails even at M = 0"))
  if (holds(as.numeric(M_max)))
    return(list(breakdown = as.numeric(M_max), family = family,
                sign = sign,
                status = "survives the whole search range; increase M_max to find the true breakdown"))
  lo <- 0.0
  hi <- as.numeric(M_max)
  while (hi - lo > tol) {
    mid <- 0.5 * (lo + hi)
    if (holds(mid)) lo <- mid else hi <- mid
  }
  list(breakdown = lo, family = family, sign = sign,
       status = "interior")
}

#' fixed_length_ci
#'
#' A step of the snmtst_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param beta Passed to \code{identified_set}.
#' @param sigma Coerced to numeric by the body, with \code{as.numeric}.
#' @param n_pre Passed to \code{identified_set}.
#' @param n_post Passed to \code{identified_set}.
#' @param M Passed to \code{identified_set}. Defaults to \code{0}.
#' @param family Carried through into a list the body builds. Defaults to \code{"SD"}.
#' @param l_vec Passed to \code{identified_set}.
#' @param level Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.95}.
#' @return A list with \code{estimate}, \code{lower}, \code{upper},
#' \code{identified_lower}, \code{identified_upper}, \code{M}, \code{family},
#' \code{level}, \code{conservative}, \code{method}.
#' @export
fixed_length_ci <- function(beta, sigma, n_pre, n_post, M = 0.0,
                            family = "SD", l_vec = NULL, level = 0.95) {
  s <- identified_set(beta, n_pre, n_post, M = M, family = family,
                      l_vec = l_vec)
  if (as.numeric(sigma) < 0.0)
    stop(sprintf("snmtst: sigma must be non-negative, got %r", sigma))
  if (!(as.numeric(level) > 0.0 && as.numeric(level) < 1.0))
    stop(sprintf("snmtst: level must be in (0, 1), got %r", level))
  z <- qnorm(0.5 + as.numeric(level) / 2.0)
  list(estimate = s$estimate,
       lower = s$lower - z * as.numeric(sigma),
       upper = s$upper + z * as.numeric(sigma),
       identified_lower = s$lower, identified_upper = s$upper,
       M = s$M, family = family, level = as.numeric(level),
       conservative = TRUE,
       method = paste0("identified set (Rambachan & Roth 2023 Sec. 2.3) ",
                       "widened by the normal critical value; the exact ",
                       "fixed-length construction of their Sec. 3 is ",
                       "tighter"))
}

#' .snmtst_cheatsheet
#'
#' A step of the snmtst_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
#' @examples
#' res <- .snmtst_cheatsheet()
#' res
.snmtst_cheatsheet <- function() {
  paste0("snmtst: honest DiD. beta = tau + delta with tau_pre = 0, ",
         "so the PRE coefficients estimate the violation. Instead ",
         "of assuming delta = 0, bound it: Delta^SD(M) caps the ",
         "SECOND differences (M=0 IS linear extrapolation), ",
         "Delta^RM(Mbar) caps the post violation at Mbar times the ",
         "largest pre-period change. Report the BREAKDOWN value -- ",
         "the M at which the sign flips -- not a pre-trends ",
         "p-value, which has low power exactly where it matters.")
}

# ledger/NAMING.md compact aliases
sensitivitytest <- breakdown_value
sensitivity_did <- breakdown_value
sensitivitydid <- breakdown_value

morie_snmtst <- list(identified_set = identified_set,
                     sensitivity_curve = sensitivity_curve,
                     breakdown_value = breakdown_value,
                     fixed_length_ci = fixed_length_ci,
                     cheatsheet = .snmtst_cheatsheet,
                     sensitivitytest = sensitivitytest,
                     sensitivity_did = sensitivity_did,
                     sensitivitydid = sensitivitydid)
