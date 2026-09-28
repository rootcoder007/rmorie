.sf_design <- function(t, daily, weekly, annual, trend) {
  yr <- 365.25 * 24
  cols <- list(rep(1, length(t)))
  for (k in seq_len(daily)) cols <- c(cols, list(sin(2 * pi * k * t / 24), cos(2 * pi * k * t / 24)))
  for (k in seq_len(weekly)) cols <- c(cols, list(sin(2 * pi * k * t / 168), cos(2 * pi * k * t / 168)))
  for (k in seq_len(annual)) cols <- c(cols, list(sin(2 * pi * k * t / yr), cos(2 * pi * k * t / yr)))
  if (trend) cols <- c(cols, list(t / yr))
  do.call(cbind, cols)
}

#' Calls-for-service demand and queueing-based staffing
#'
#' \code{CallDemandModel}: Poisson harmonic regression of hourly calls with
#' daily, weekly and annual cycles and a trend (IRLS with the \code{glm}
#' convergence rule), with forecasts. \code{ErlangC}: probability of waiting.
#' \code{StaffingRequirement}: minimal units per period for a waiting
#' probability, service level or mean-wait target, with optional square-root
#' staffing. \code{PatrolCarRequirement}: PCAM-style maximum of queueing,
#' response-time and patrol-frequency requirements. Identical to the Python
#' arm \code{morie.fn.staffing}.
#'
#' @param counts Hourly call counts.
#' @param hours Hour indices.
#' @param daily,weekly,annual Numbers of harmonics.
#' @param trend Include a linear trend.
#' @param new_hours Hours to forecast.
#' @param maxit,epsilon IRLS controls.
#' @param c Number of servers.
#' @param a Offered load.
#' @param arrival_rates,arrival_rate Calls per hour.
#' @param handle_time Mean handling time (hours).
#' @param target \code{"wait_prob"}, \code{"service_level"} or \code{"asa"}.
#' @param level Target value.
#' @param threshold Service-level answer time.
#' @param beta Square-root staffing parameter.
#' @param area Beat area.
#' @param response_speed Response speed.
#' @param target_response Target travel time.
#' @param street_miles Street length to patrol.
#' @param patrol_speed Patrol speed.
#' @param patrol_frequency Required passes per hour.
#' @param target_wait Target waiting probability.
#' @param metric \code{"euclidean"} or \code{"rectilinear"}.
#' @return List or numeric.
#' @references Green, L. V., Kolesar, P. J. and Whitt, W. (2007). Coping with
#'   time-varying demand when setting staffing requirements for a service
#'   system. Production and Operations Management 16, 13-39.
#'
#'   Chaiken, J. M. and Dormont, P. (1978). A patrol car allocation model:
#'   background. Management Science 24, 1280-1290.
#'
#'   Wilson, J. M. and Weiss, A. (2012). A Performance-Based Approach to
#'   Police Staffing and Allocation. U.S. Department of Justice, COPS Office.
#' @examples
#' ErlangC(3, 2)
#' StaffingRequirement(c(1, 4), 1, level = 0.2)$units
#' @export
CallDemandModel <- function(counts, hours, daily = 3, weekly = 2, annual = 2, trend = TRUE, new_hours = NULL,
                            maxit = 100, epsilon = 1e-10) {
  y <- counts
  X <- .sf_design(hours, daily, weekly, annual, trend)
  mu <- y + 0.1
  eta <- log(mu)
  dev_old <- Inf
  for (it in seq_len(maxit)) {
    z <- eta + (y - mu) / mu
    XtWX <- crossprod(X * mu, X)
    beta <- as.vector(solve(XtWX, crossprod(X * mu, z)))
    eta <- as.vector(X %*% beta)
    mu <- exp(eta)
    dev <- 2 * sum(ifelse(y > 0, y * log(y / mu), 0) - (y - mu))
    if (abs(dev - dev_old) / (abs(dev) + 0.1) < epsilon) break
    dev_old <- dev
  }
  cov <- solve(crossprod(X * mu, X))
  out <- list(coefficients = beta, fitted = mu, deviance = dev, iterations = it, se = sqrt(diag(cov)))
  if (!is.null(new_hours)) {
    Xn <- .sf_design(new_hours, daily, weekly, annual, trend)
    out$forecast <- as.vector(exp(Xn %*% beta))
    out$forecast_log_se <- sqrt(rowSums((Xn %*% cov) * Xn))
  }
  out
}

#' @rdname CallDemandModel
#' @export
ErlangC <- function(c, a) {
  if (a >= c) return(NaN)
  b <- 1
  for (k in seq_len(c)) b <- a * b / (k + a * b)
  c * b / (c - a * (1 - b))
}

#' @rdname CallDemandModel
#' @export
StaffingRequirement <- function(arrival_rates, handle_time, target = "wait_prob", level = 0.2, threshold = 0,
                                beta = NULL) {
  units <- integer(0)
  perf <- numeric(0)
  for (lam in arrival_rates) {
    a <- lam * handle_time
    cc <- max(1, floor(a) + 1)
    repeat {
      C <- ErlangC(cc, a)
      val <- switch(target,
        wait_prob = C,
        service_level = 1 - C * exp(-(cc - a) * threshold / handle_time),
        asa = C * handle_time / (cc - a),
        stop("target must be wait_prob, service_level or asa")
      )
      ok <- if (target == "service_level") val >= level else val <= level
      if (ok) break
      cc <- cc + 1
    }
    units <- c(units, cc)
    perf <- c(perf, val)
  }
  out <- list(units = units, performance = perf)
  if (!is.null(beta)) {
    a <- arrival_rates * handle_time
    out$square_root_units <- ifelse(a > 0, ceiling(a + beta * sqrt(a)), 0)
  }
  out
}

#' @rdname CallDemandModel
#' @export
PatrolCarRequirement <- function(arrival_rate, handle_time, area, response_speed, target_response,
                                 street_miles = 0, patrol_speed = 1, patrol_frequency = 0, target_wait = 0.2,
                                 metric = "euclidean") {
  a <- arrival_rate * handle_time
  q <- StaffingRequirement(arrival_rate, handle_time, "wait_prob", target_wait)$units
  cm <- if (metric == "euclidean") 0.5 else sqrt(2 * pi) / 4
  resp <- ceiling(a + area * (cm / (response_speed * target_response))^2 - 1e-12)
  pat <- if (patrol_frequency > 0) ceiling(a + patrol_frequency * street_miles / patrol_speed - 1e-12) else 0
  list(offered_load = a, queue = q, response = resp, patrol = pat, units = max(q, resp, pat))
}

.sf_feasible <- function(b, L, tot) {
  n <- length(b)
  E <- NULL
  for (t in seq_len(n)) E <- rbind(E, c(t, t - 1, 0))
  E <- rbind(E, c(0, n, tot), c(n, 0, -tot))
  for (h in seq_len(n)) {
    lo <- h - L
    E <- rbind(E, if (lo >= 0) c(h, lo, -b[h]) else c(h, lo + n, tot - b[h]))
  }
  dist <- numeric(n + 1)
  for (it in seq_len(n + 1)) {
    changed <- FALSE
    for (k in seq_len(nrow(E))) {
      u <- E[k, 1] + 1
      v <- E[k, 2] + 1
      if (dist[u] + E[k, 3] < dist[v] - 1e-9) {
        dist[v] <- dist[u] + E[k, 3]
        changed <- TRUE
      }
    }
    if (!changed) return(dist - dist[1])
  }
  NULL
}

#' Shift scheduling, relief factors and workload staffing
#'
#' \code{ShiftSchedule}: fewest officers on cyclic consecutive-period shifts
#' covering every period's requirement (difference constraints and binary
#' search; Bartholdi, Orlin and Ratliff 1980). \code{ReliefFactor}:
#' \code{days_per_year / (days_per_year - days_off)}. \code{WorkloadStaffing}:
#' Wilson and Weiss (2012) obligated time, officers on duty and assigned.
#'
#' @param requirements Integer requirement per period.
#' @param shift_length Shift length in periods.
#' @param days_off,days_per_year Leave days and year length.
#' @param calls,handle_minutes Calls and handling minutes by call type.
#' @param units_per_call Units per call by type.
#' @param call_share Fraction of a shift for calls for service.
#' @param shift_hours Shift length (hours).
#' @param relief Shift-relief factor.
#' @return List or numeric.
#' @references Bartholdi, J. J., Orlin, J. B. and Ratliff, H. D. (1980).
#'   Cyclic scheduling via integer programs with circular ones. Operations
#'   Research 28, 1074-1085.
#' @examples
#' ShiftSchedule(c(2, 2, 3, 5, 5, 4), 3)$total
#' ReliefFactor(151)
#' @export
ShiftSchedule <- function(requirements, shift_length) {
  b <- round(requirements)
  n <- length(b)
  L <- shift_length
  if (L < 1 || L > n) stop("shift_length must be between 1 and the number of periods")
  lo <- max(b)
  hi <- sum(b)
  while (lo < hi) {
    mid <- (lo + hi) %/% 2
    if (!is.null(.sf_feasible(b, L, mid))) hi <- mid else lo <- mid + 1
  }
  S <- .sf_feasible(b, L, lo)
  starts <- round(diff(S))
  cov <- vapply(seq_len(n) - 1, function(h) sum(starts[((h - seq_len(L) + 1) %% n) + 1]), 0)
  list(starts = starts, total = sum(starts), coverage = cov)
}

#' @rdname ShiftSchedule
#' @export
ReliefFactor <- function(days_off, days_per_year = 365) days_per_year / (days_per_year - days_off)

#' @rdname ShiftSchedule
#' @export
WorkloadStaffing <- function(calls, handle_minutes, units_per_call = 1, call_share = 0.5, shift_hours = 8, relief = 1) {
  obl <- sum(calls * handle_minutes / 60 * units_per_call)
  on <- obl / (call_share * shift_hours)
  list(obligated_hours = obl, on_duty = on, assigned = on * relief, on_duty_ceiling = ceiling(on - 1e-12))
}
