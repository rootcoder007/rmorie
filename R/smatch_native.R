# morie.fn -- function file (rootcoder007/morie)
# R arm of smatch (poisson_design, sccs_poisson_fit, sample_size, power,
# relative_efficiency).
# Sources:
#   Whitaker, H. J., Farrington, C. P., Spiessens, B. & Musonda, P.
#   (2006) "Tutorial in biostatistics: The self-controlled case series
#   method", Statistics in Medicine 25, 1768-1797, doi:10.1002/sim.2302.
#   Sec. 4 (the associated Poisson model with an individual factor and
#   a log-time offset), Sec. 7.3-7.5 (risk-period choice, covariates,
#   relative efficiency) and Sec. 7.6 (the sample size expression
#   implemented here).
#   Farrington, C. P. (1995) "Relative Incidence Estimation from Case
#   Series for Vaccine Safety Evaluation", Biometrics 51(1), 228-235,
#   JSTOR https://www.jstor.org/stable/2533328. The conditional
#   likelihood the Poisson form reproduces.
#   Musonda, P., Farrington, C. P. & Whitaker, H. J. (2006) "Sample
#   sizes for self-controlled case series studies", Statistics in
#   Medicine 25(15), 2618-2631. The age-varying case (not implemented).

.smatch_EPS <- 1e-12

#' Symmetric positive-definite solve via base R\'s chol
#'
#' Part of the smatch_native implementation; see the file header for the
#' source it follows.
#'
#' @param M See Usage.
#' @param b See Usage.
#' @return A vector, from \code{as.numeric}.
#' @export
.smatch_cholsolve <- function(M, b) {
  # Symmetric positive-definite solve via base R's chol.
  L <- chol(M)
  y <- forwardsolve(t(L), b)
  as.numeric(backsolve(L, y))
}

#' .smatch_build_intervals
#'
#' Part of the smatch_native implementation; see the file header for the
#' source it follows.
#'
#' @param start See Usage.
#' @param end See Usage.
#' @param exposure See Usage.
#' @param events See Usage.
#' @param rp See Usage.
#' @param ab See Usage.
#' @return The value of \code{lapply}.
#' @export
.smatch_build_intervals <- function(start, end, exposure, events, rp,
                                    ab) {
  # Python's smatch imports build_intervals from sccsno rather than defining
  # one; delegating keeps the two in step instead of drifting apart again.
  cells <- morie_sccsno_build_intervals(start, end, exposure, events, rp, ab)
  as_cell <- function(band, risk, e, n) {
    list(age = as.integer(band), risk = as.integer(risk),
         exposure = as.numeric(e), n = as.numeric(n))
  }
  if (is.matrix(cells)) {
    return(lapply(seq_len(nrow(cells)), function(q)
      as_cell(cells[q, 1L], cells[q, 2L], cells[q, 3L], cells[q, 4L])))
  }
  lapply(cells, function(cl)
    as_cell(cl[[1L]], cl[[2L]], cl[[3L]], cl[[4L]]))
}

#' morie_smatch_poisson_design
#'
#' Part of the smatch_native implementation; see the file header for the
#' source it follows.
#'
#' @param cases See Usage.
#' @param risk_periods See Usage.
#' @param age_breaks Defaults to \code{numeric(0)}.
#' @return A list with \code{y}, \code{offset}, \code{X}, \code{n_risk}, \code{n_age}, \code{n_people}, \code{n_rows}.
#' @export
morie_smatch_poisson_design <- function(cases, risk_periods, age_breaks = numeric(0)) {
  rp <- lapply(risk_periods, function(r) c(as.numeric(r[1]), as.numeric(r[2])))
  ab <- as.numeric(age_breaks)
  n_risk <- length(rp)
  n_age <- length(ab) + 1L
  people <- list()
  for (c in cases) {
    ev <- as.numeric(c$events)
    if (length(ev) == 0L) next
    cells <- .smatch_build_intervals(as.numeric(c$start),
                                     as.numeric(c$end), c$exposure,
                                     ev, rp, ab)
    if (length(cells) > 0L) people[[length(people) + 1L]] <- cells
  }
  if (length(people) == 0L)
    stop("smatch: no case contributed an event")
  P <- length(people)
  ncol <- n_risk + (n_age - 1L) + P
  y <- c(); off <- c(); X <- list()
  for (i in seq_along(people)) {
    cells <- people[[i]]
    for (cell in cells) {
      j <- cell[[1L]]; r_idx <- cell[[2L]]; e <- cell[[3L]]
      n <- cell[[4L]]
      if (e <= .smatch_EPS) next
      row <- rep(0.0, ncol)
      if (r_idx > 0L) row[r_idx] <- 1.0
      if (j > 0L) row[n_risk + j] <- 1.0
      row[n_risk + n_age - 1L + i] <- 1.0
      X[[length(X) + 1L]] <- row
      y <- c(y, as.numeric(n))
      off <- c(off, log(e))
    }
  }
  if (length(y) == 0L) stop("smatch: no case contributed an event")
  Xm <- do.call(rbind, X)
  list(y = y, offset = off, X = Xm, n_risk = n_risk, n_age = n_age,
       n_people = P, n_rows = length(y))
}

#' morie_smatch_sccs_poisson_fit
#'
#' Part of the smatch_native implementation; see the file header for the
#' source it follows.
#'
#' @param cases See Usage.
#' @param risk_periods See Usage.
#' @param age_breaks Defaults to \code{numeric(0)}.
#' @param iters Defaults to \code{200}.
#' @param tol Defaults to \code{1e-12}.
#' @param ridge Defaults to \code{1e-09}.
#' @return A list with \code{estimate}, \code{relative_incidence}, \code{log_ri}, \code{age_effects}, \code{individual_effects}, \code{coef}, \code{converged}, \code{iterations}, \code{n_rows}, \code{n_people}, \code{method}, \code{identical_to}.
#' @export
morie_smatch_sccs_poisson_fit <- function(cases, risk_periods, age_breaks = numeric(0),
                             iters = 200, tol = 1e-12, ridge = 1e-9) {
  d <- morie_smatch_poisson_design(cases, risk_periods, age_breaks = age_breaks)
  y <- d$y; off <- d$offset; X <- d$X
  p <- ncol(X)
  beta <- rep(0.0, p)
  conv <- FALSE; it <- 0L
  for (it in seq_len(as.integer(iters))) {
    eta <- off + as.numeric(X %*% beta)
    eta <- pmin(pmax(eta, -500), 500)
    mu <- exp(eta)
    W <- pmax(mu, 1e-12)
    z <- eta - off + (y - mu) / pmax(mu, 1e-12)
    XtWX <- crossprod(X, X * W) + diag(ridge, p)
    XtWz <- as.numeric(crossprod(X, W * z))
    nb <- tryCatch(.smatch_cholsolve(XtWX, XtWz),
                   error = function(e) {
                     stop("smatch: the Poisson design is singular ",
                          "-- an interval has no exposure time or ",
                          "an individual has no variation")
                   })
    mx <- max(abs(nb - beta))
    beta <- nb
    if (mx < tol) { conv <- TRUE; break }
  }
  nr <- d$n_risk
  list(estimate = exp(beta[seq_len(nr)]),
       relative_incidence = exp(beta[seq_len(nr)]),
       log_ri = beta[seq_len(nr)],
       age_effects = beta[(nr + 1L):(nr + d$n_age - 1L)],
       individual_effects =
         beta[(nr + d$n_age):length(beta)],
       coef = beta, converged = conv, iterations = it,
       n_rows = d$n_rows, n_people = d$n_people,
       method = paste0("associated Poisson model with a per-individual ",
                       "factor and log-time offset; Whitaker et al. ",
                       "(2006) Sec. 4"),
       identical_to = "the conditional multinomial fit of sccsno")
}

#' .smatch_qnorm
#'
#' Part of the smatch_native implementation; see the file header for the
#' source it follows.
#'
#' @param p See Usage.
#' @return The value of \code{qnorm}.
#' @export
.smatch_qnorm <- function(p) qnorm(p)

#' .smatch_pnorm
#'
#' Part of the smatch_native implementation; see the file header for the
#' source it follows.
#'
#' @param z See Usage.
#' @return The value of \code{pnorm}.
#' @export
.smatch_pnorm <- function(z) pnorm(z)

#' morie_smatch_sample_size
#'
#' Part of the smatch_native implementation; see the file header for the
#' source it follows.
#'
#' @param log_ri See Usage.
#' @param r See Usage.
#' @param p_exposed See Usage.
#' @param alpha Defaults to \code{0.05}.
#' @param power Defaults to \code{0.8}.
#' @return A list with \code{n_events}, \code{n_events_ceiling}, \code{rho}, \code{A}, \code{B}, \code{C}, \code{z_alpha_2}, \code{z_power}, \code{log_ri}, \code{r}, \code{p_exposed}, \code{assumes}, \code{method}.
#' @export
morie_smatch_sample_size <- function(log_ri, r, p_exposed, alpha = 0.05, power = 0.8) {
  b <- as.numeric(log_ri)
  rr <- as.numeric(r)
  p <- as.numeric(p_exposed)
  if (b == 0.0)
    stop("smatch: the sample size is unbounded at a log relative incidence of 0")
  if (!(rr > 0.0 && rr < 1.0))
    stop(sprintf(paste0("smatch: r must lie strictly in (0, 1), got ",
                        "%r -- it is the risk period as a fraction ",
                        "of the observation period"), r))
  if (!(p > 0.0 && p <= 1.0))
    stop(sprintf("smatch: p_exposed must lie in (0, 1], got %r", p_exposed))
  if (!(as.numeric(alpha) > 0.0 && as.numeric(alpha) < 1.0))
    stop("smatch: alpha must lie in (0, 1)")
  if (!(as.numeric(power) > 0.0 && as.numeric(power) < 1.0))
    stop("smatch: power must lie in (0, 1)")
  eb <- exp(b)
  den <- rr * eb + 1.0 - rr
  rho <- rr * eb / den
  A <- 2.0 * (rho * b - log(den))
  if (A <= .smatch_EPS)
    stop(sprintf(paste0("smatch: the information A is non-positive ",
                        "(%.3e) -- the design carries no signal here"),
                 A))
  B <- b * b * rho * (1.0 - rho) / A
  C <- 1.0 + (1.0 - p) / (p * den)
  za <- .smatch_qnorm(1.0 - as.numeric(alpha) / 2.0)
  zg <- .smatch_qnorm(as.numeric(power))
  n <- (C / A) * (za + zg * sqrt(B)) ^ 2
  list(n_events = n, n_events_ceiling = as.integer(ceiling(n)),
       rho = rho, A = A, B = B, C = C,
       z_alpha_2 = za, z_power = zg,
       log_ri = b, r = rr, p_exposed = p,
       assumes = "age effects negligible; see Musonda, Farrington & Whitaker (2006) otherwise",
       method = "Whitaker et al. (2006) Sec. 7.6")
}

#' morie_smatch_power
#'
#' Part of the smatch_native implementation; see the file header for the
#' source it follows.
#'
#' @param n_events See Usage.
#' @param log_ri See Usage.
#' @param r See Usage.
#' @param p_exposed See Usage.
#' @param alpha Defaults to \code{0.05}.
#' @return A list with \code{power}, \code{z_power}, \code{n_events}, \code{A}, \code{B}, \code{C}.
#' @export
morie_smatch_power <- function(n_events, log_ri, r, p_exposed, alpha = 0.05) {
  s <- morie_smatch_sample_size(log_ri, r, p_exposed, alpha = alpha, power = 0.5)
  A <- s$A; B <- s$B; C <- s$C
  za <- s$z_alpha_2
  root <- sqrt(max(as.numeric(n_events) * A / C, 0.0))
  zg <- if (B > .smatch_EPS) (root - za) / sqrt(B) else Inf
  list(power = pnorm(zg), z_power = zg,
       n_events = as.numeric(n_events), A = A, B = B, C = C)
}

#' morie_smatch_relative_efficiency
#'
#' Part of the smatch_native implementation; see the file header for the
#' source it follows.
#'
#' @param r See Usage.
#' @param log_ri See Usage.
#' @return A list with \code{rho}, \code{efficiency}, \code{r}, \code{log_ri}, \code{interpretation}.
#' @export
morie_smatch_relative_efficiency <- function(r, log_ri) {
  rr <- as.numeric(r); b <- as.numeric(log_ri)
  if (!(rr > 0.0 && rr < 1.0))
    stop("smatch: r must lie strictly in (0, 1)")
  eb <- exp(b)
  den <- rr * eb + 1.0 - rr
  rho <- rr * eb / den
  list(rho = rho, efficiency = 1.0 - rho,
       r = rr, log_ri = b,
       interpretation = paste0("the fraction of cases falling in the ",
                               "risk period is rho; the marginal ",
                               "information lost grows with it, so a ",
                               "SHORT risk period keeps efficiency ",
                               "high (Sec. 7.5)"))
}

#' .smatch_cheatsheet
#'
#' Part of the smatch_native implementation; see the file header for the
#' source it follows.
#'
#' @return A character value.
#' @export
.smatch_cheatsheet <- function() {
  paste0("smatch: the case series fitted as a POISSON model -- ",
         "counts n_ijk, offset log(e_ijk), factors for age, ",
         "exposure AND one per individual. The individual factors ",
         "force the fitted totals to match the observed ones, ",
         "which IS the conditioning, so this is the same fit as ",
         "the multinomial, not an approximation. Sample size ",
         "(Sec. 7.6): rho = re^b/(re^b+1-r), A = 2{rho b - ",
         "log(re^b+1-r)}, B = b^2 rho(1-rho)/A -> 1 as b -> 0, ",
         "C = 1 + (1-p)/(p(re^b+1-r)), n = (C/A)(z_a2 + z_g sqrt ",
         "B)^2. p is the POPULATION exposed fraction, not the ",
         "cases.")
}

# ledger/NAMING.md compact alias
morie_smatch_selfcontrolledcaseseries <- morie_smatch_sccs_poisson_fit
morie_smatch_sccs_design <- morie_smatch_sccs_poisson_fit
morie_smatch_sccsdesign <- morie_smatch_sccs_poisson_fit

morie_smatch <- morie_smatch_poisson_design
