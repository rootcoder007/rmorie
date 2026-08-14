# Randomized interventional direct and indirect effects.
# Sources: Didelez, V., Dawid, A. P. & Geneletti, S. (2006) "Direct and
# Indirect Effects of Sequential Treatments", UAI 2006, 138-146,
# arXiv:1206.6840. Dawid & Didelez (2010) Statistics Surveys 4, 184-231.
# Robins (1986) Mathematical Modelling 7(9-12) (the g-formula). Native R
# mirroring morie.fn.randIE: same regime-indicator / psi(a, a*) form,
# same algebraic identity, same g-formula and weighting routes, same
# own_mediator_mean diagnostic.

.EPS <- 1e-12
.ROUTES <- c("gformula", "weighting")

.labels <- function(v, name) {
  out <- as.character(v)
  if (length(out) == 0L) stop(paste0("randIE: ", name, " is empty"))
  out
}

morie_randIE_mediator_distribution <- function(A, M, C = NULL, laplace = 0) {
  a <- .labels(A, "A"); m <- .labels(M, "M"); n <- length(a)
  if (length(m) != n)
    stop(paste0("randIE: ", n, " treatments but ", length(m),
                " mediator values"))
  c <- if (is.null(C)) rep("*", n) else .labels(C, "C")
  if (length(c) != n)
    stop(paste0("randIE: ", length(c), " strata for ", n, " units"))
  levels.v <- sort(unique(m))
  cells <- list()
  for (i in seq_len(n)) {
    key <- paste0(a[i], "\r", c[i])
    if (is.null(cells[[key]])) cells[[key]] <- character(0)
    cells[[key]] <- c(cells[[key]], m[i])
  }
  out <- list()
  for (k in names(cells)) {
    vals <- cells[[k]]; tot <- length(vals) + laplace * length(levels.v)
    parts <- strsplit(k, "\r", fixed = TRUE)[[1]]
    key2 <- c(parts[1], parts[2])
    out[[length(out) + 1L]] <- list(key = key2,
                                    p = vapply(levels.v, function(lv)
                                      (sum(vals == lv) + laplace) / tot,
                                      numeric(1)))
  }
  out.p <- list()
  for (e in out) out.p[[paste0(e$key[1], "\r", e$key[2])]] <- e$p
  names(out.p) <- paste0(
    vapply(out, function(e) e$key[1], character(1)), "\r",
    vapply(out, function(e) e$key[2], character(1)))
  list(p = out.p, levels = levels.v,
       strata = sort(unique(c)),
       arms = sort(unique(a)),
       n = n)
}

morie_randIE_interventional_mean <- function(Y, A, M, C = NULL, a = "1",
                                             a.star = "0",
                                             route = "gformula",
                                             laplace = 0) {
  if (!(route %in% .ROUTES))
    stop(paste0("randIE: route must be gformula or weighting, got ",
                route))
  y <- as.numeric(Y)
  av <- .labels(A, "A"); mv <- .labels(M, "M"); n <- length(y)
  if (!(length(av) == length(mv) && length(av) == n))
    stop(paste0("randIE: Y, A and M must agree in length (",
                n, ", ", length(av), ", ", length(mv), ")"))
  cv <- if (is.null(C)) rep("*", n) else .labels(C, "C")
  if (length(cv) != n)
    stop(paste0("randIE: ", length(cv), " strata for ", n, " units"))
  a <- as.character(a); a.star <- as.character(a.star)
  if (!(a %in% av))
    stop(paste0("randIE: treatment arm '", a,
                "' not observed; arms are ", paste(sort(unique(av)),
                                                    collapse = ", ")))
  if (!(a.star %in% av))
    stop(paste0("randIE: treatment arm '", a.star,
                "' not observed; arms are ", paste(sort(unique(av)),
                                                    collapse = ", ")))
  md <- morie_randIE_mediator_distribution(av, mv, cv, laplace = laplace)
  strata <- md$strata; levels.v <- md$levels
  pc <- vapply(strata, function(s)
    sum(cv == s) / n, numeric(1))
  ybar <- list(); cnt <- list()
  for (i in seq_len(n)) {
    key <- paste0(av[i], "\r", mv[i], "\r", cv[i])
    ybar[[key]] <- if (is.null(ybar[[key]])) 0 else ybar[[key]]
    ybar[[key]] <- ybar[[key]] + y[i]
    cnt[[key]] <- if (is.null(cnt[[key]])) 0 else cnt[[key]] + 1
  }
  for (k in names(ybar)) ybar[[k]] <- ybar[[k]] / cnt[[k]]
  total <- 0; missing <- list()
  for (s.idx in seq_along(strata)) {
    s <- strata[s.idx]
    pmk <- paste0(a.star, "\r", s)
    pm <- md$p[[pmk]]
    if (is.null(pm)) { missing[[length(missing) + 1L]] <- list("mediator", a.star, s); next }
    for (lv in levels.v) {
      w <- pm[lv == md$levels]
      if (is.na(w) || w <= 0) next
      key <- paste0(a, "\r", lv, "\r", s)
      if (is.null(ybar[[key]])) {
        missing[[length(missing) + 1L]] <- list("outcome", a, lv, s); next
      }
      total <- total + pc[s.idx] * w * ybar[[key]]
    }
  }
  if (length(missing) > 0L)
    stop(paste0("randIE: ", length(missing),
                " cell(s) needed by the g-formula are empty, e.g. ",
                paste(unlist(missing[[1]]), collapse = ","),
                " -- psi(", a, ", ", a.star,
                ") is not identified from this sample"))
  if (route == "weighting") {
    num <- 0; den <- 0
    for (i in seq_len(n)) {
      if (av[i] != a) next
      pkey.star <- paste0(a.star, "\r", cv[i])
      pkey.obs <- paste0(a, "\r", cv[i])
      p.star <- if (is.null(md$p[[pkey.star]])) 0
        else md$p[[pkey.star]][mv[i] == md$levels]
      p.obs <- if (is.null(md$p[[pkey.obs]])) 0
        else md$p[[pkey.obs]][mv[i] == md$levels]
      if (is.na(p.obs) || p.obs <= .EPS) next
      w <- if (is.na(p.star)) 0 else p.star / p.obs
      num <- num + w * y[i]; den <- den + w
    }
    if (den <= .EPS)
      stop(paste0("randIE: the mediator-density ratio put no weight on arm '", a, "'"))
    total <- num / den
  }
  own <- y[av == a]
  list(estimate = total, a = a, a.star = a.star, route = route,
       own.mediator.mean = if (length(own) > 0L)
         mean(own) else NaN,
       n.arm = length(own), n = n,
       note = "psi(a, a) is not the observed arm mean unless the mediator is degenerate: drawing M from p(m|a,c) breaks the individual-level M-Y dependence")
}

morie_randIE_randomized_interventional_effect <- function(Y, A, M, C = NULL,
                                                         treated = "1",
                                                         control = "0",
                                                         route = "gformula",
                                                         laplace = 0) {
  psi <- function(a, a.star)
    morie_randIE_interventional_mean(Y, A, M, C, a = a, a.star = a.star,
                                     route = route, laplace = laplace)$estimate
  t <- as.character(treated); c <- as.character(control)
  p11 <- psi(t, t); p10 <- psi(t, c); p00 <- psi(c, c)
  p01 <- tryCatch(psi(c, t), error = function(e) NULL)
  list(estimate = p11 - p00,
       total = p11 - p00,
       direct = p10 - p00,
       indirect = p11 - p10,
       direct.control.arm = if (is.null(p01)) NULL else p00 - p01,
       psi = list("11" = p11, "10" = p10, "01" = p01, "00" = p00),
       route = route, treated = t, control = c,
       identity = "total = direct + indirect holds exactly by construction; it is not evidence the estimator is correct",
       method = "randomized interventional direct/indirect effects, Didelez, Dawid & Geneletti (2006) Secs. 3-4")
}

morie_randIE_decompose <- function(result) {
  tot <- result$total; d <- result$direct; i <- result$indirect
  list(total = tot, direct = d, indirect = i,
       residual = tot - (d + i),
       proportion.mediated = if (abs(tot) > .EPS) i / tot else NaN)
}

# house entry point: the package exports one morie_<module>
morie_randIE <- morie_randIE_mediator_distribution
