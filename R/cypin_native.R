# SPDX-License-Identifier: AGPL-3.0-or-later
# R arm of cypin -- cytochrome P450 inhibition descriptors and a
# caller-fitted logistic model. Mirrors src/morie/fn/cypin.py operation
# for operation, on the SMILES parser in R/avalon_native.R, the
# pharmacophore typing in R/scfhop_native.R and the shared numerics in
# R/aaa_helpers_w3num.R.
#
# Most drugs are cleared by five cytochrome P450 isozymes -- 1A2, 2C9,
# 2C19, 2D6 and 3A4 -- and a compound that inhibits one of them will
# change the blood level of every other drug that isozyme clears. That
# is where drug-drug interactions come from. Veith and colleagues
# measured it directly: seventeen thousand compounds against all five
# isozymes in quantitative high-throughput screen, which is a DATASET,
# and then a model fitted to that dataset.
#
# The distinction matters, because the two halves are not equally
# available. The assay's design and its findings are published. The
# fitted model's coefficients are not published as a table anyone can
# copy, and this module will not invent them.
#
#   THE DESCRIPTORS. Computed exactly from the molecular graph, with
#   nothing fitted and nothing approximated: molecular weight from the
#   IUPAC standard atomic weights, heavy-atom count, rings and aromatic
#   rings, rotatable bonds by their definition, hydrogen-bond donors and
#   acceptors and basic nitrogens from the pharmacophore typing, the
#   halogen count, the polar-atom count, and the fraction of carbons
#   that are saturated. Every one is checkable by hand and several are
#   checked by hand in the anchors.
#
#   THE MODEL. A logistic regression fitted by iteratively reweighted
#   least squares, which the CALLER trains on their own inhibition data.
#
# Called with no model, the function returns the descriptors, a NULL
# prediction and a reason saying in words that no coefficients were
# supplied and that the paper does not publish a set to default to. A
# refusal that names what is missing is worth more than a number nobody
# can trace, and it is anchored.
#
# What the paper does establish, and what the descriptors carry, is that
# inhibition rises with lipophilicity and with aromatic ring count, that
# 2C19 and 3A4 are the most promiscuous of the five, and that 2D6
# responds to a basic nitrogen. Those are directions, not coefficients,
# and they are stated here as directions.
#
# References
#   Veith, H. et al. (2009) "Comprehensive characterization of
#     cytochrome P450 isozyme selectivity across chemical libraries."
#     Nature Biotechnology 27(11), 1050-1055. doi:10.1038/nbt.1581.
#   Meija, J. et al. (2016) "Atomic weights of the elements 2013."
#     Pure and Applied Chemistry 88(3), 265-291.
#   McCullagh, P. and Nelder, J.A. (1989) "Generalized Linear Models",
#     2nd edition, Chapman and Hall.

# The five isozymes Veith et al. screened, in the order the paper lists.
.cypin_isozymes <- c("1A2", "2C9", "2C19", "2D6", "3A4")

# IUPAC standard atomic weights. Where the element has a published
# interval rather than a single value, the conventional value is used
# and that is a documented choice, not a rounding.
.cypin_mass <- c(H = 1.008, B = 10.81, C = 12.011, N = 14.007,
                 O = 15.999, F = 18.998403163, P = 30.973761998,
                 S = 32.06, Cl = 35.45, Br = 79.904, I = 126.90447)

# The descriptor block, in a fixed order, because a coefficient vector
# is meaningless without knowing which slot is which.
.cypin_names <- c("mw", "heavy_atoms", "n_rings", "n_aromatic_rings",
                  "n_rotatable", "hbd", "hba", "n_basic_n", "n_halogen",
                  "n_polar", "fsp3", "formal_charge")

#' The descriptor block, computed exactly from the graph
#'
#' Rotatable bonds are single bonds outside a ring whose two ends each
#' have more than one heavy neighbour -- the standard definition, which
#' excludes a terminal methyl because spinning it changes nothing, and
#' excludes ring bonds because they cannot turn.
#'
#' The saturated fraction counts carbons with four single bonds and no
#' aromaticity, over all carbons; a molecule with no carbon has no such
#' fraction and reports zero rather than dividing by nothing.
#'
#' @param smiles The compound.
#' @return A numeric vector in the order given by the module's names.
#' @export
morie_cypin_descriptors <- function(smiles) {
  g <- morie_avalon_parse(smiles)
  el <- g$el; arom <- g$arom; chg <- g$chg; bonds <- g$bonds
  n <- length(el)
  adj <- .avalon_adj(n, bonds)
  nh <- morie_avalon_h(el, arom, chg, g$hexp, bonds)
  rr <- morie_avalon_rings(n, bonds, g$closures)
  rings <- rr$rings
  ty <- morie_scfhop_types(smiles)

  mass <- numeric(n)
  for (i in seq_len(n)) {
    m <- .cypin_mass[el[i]]
    if (is.na(m)) stop("no standard atomic weight for ", el[i])
    mass[i] <- m + nh[i] * .cypin_mass[["H"]]
  }
  mw <- .w3_csum(mass)

  ringbond <- character(0)
  for (r in rings) for (k in seq_along(r)) {
    a <- r[k]; b <- r[if (k == length(r)) 1L else k + 1L]
    ringbond <- c(ringbond, if (a < b) sprintf("%d-%d", a, b)
                            else sprintf("%d-%d", b, a))
  }
  rot <- 0L
  for (bd in bonds) {
    if (bd[3] != 1) next
    a <- bd[1]; b <- bd[2]
    key <- if (a < b) sprintf("%d-%d", a, b) else sprintf("%d-%d", b, a)
    if (key %in% ringbond) next
    if (length(adj[[a + 1L]]) > 1L && length(adj[[b + 1L]]) > 1L)
      rot <- rot + 1L
  }

  naro <- 0L
  for (r in rings) {
    allaro <- TRUE
    for (v in r) if (arom[v + 1L] != 1L) allaro <- FALSE
    if (allaro) naro <- naro + 1L
  }

  hbd <- 0L; hba <- 0L; basic <- 0L
  for (i in seq_len(n)) {
    if ("D" %in% ty[[i]]) hbd <- hbd + 1L
    if ("A" %in% ty[[i]]) hba <- hba + 1L
    if ("P" %in% ty[[i]]) basic <- basic + 1L
  }
  hal <- sum(el %in% c("F", "Cl", "Br", "I"))
  pol <- sum(el %in% c("N", "O"))

  ncarb <- 0L; nsp3 <- 0L
  for (i in seq_len(n)) {
    if (el[i] != "C") next
    ncarb <- ncarb + 1L
    if (arom[i] == 1L) next
    allsingle <- TRUE
    for (e in adj[[i]]) if (e[2] != 1) allsingle <- FALSE
    if (allsingle && length(adj[[i]]) + nh[i] == 4L) nsp3 <- nsp3 + 1L
  }

  c(mw, n, length(rings), naro, rot, hbd, hba, basic, hal, pol,
    if (ncarb > 0L) nsp3 / ncarb else 0, sum(chg))
}

#' The logistic function, written so it cannot overflow either way
#'
#' A large positive argument would overflow the exponential in the naive
#' form and a large negative one would overflow the other; branching on
#' the sign keeps both exponentials below one.
#'
#' @param z A numeric scalar.
#' @return A number strictly between zero and one.
#' @export
morie_cypin_logistic <- function(z) {
  if (z >= 0) return(1 / (1 + exp(-z)))
  e <- exp(z)
  e / (1 + e)
}

#' Logistic regression by iteratively reweighted least squares
#'
#' An intercept is prepended, so the returned vector is one longer than
#' a descriptor row and its first entry is the intercept.
#'
#' The ridge is a small quadratic penalty on the slopes and not on the
#' intercept. It is there because inhibition data is routinely separable
#' -- every compound above some lipophilicity inhibits -- and a
#' separable logistic fit has no finite maximum, so without it the
#' coefficients run off to infinity and the iteration reports a number
#' that only means "it kept going". The penalty is a parameter and it is
#' reported back.
#'
#' @param X One descriptor row per compound.
#' @param y The inhibition labels.
#' @param ridge The penalty on the slopes.
#' @param iters The iteration cap.
#' @param tol The convergence tolerance on the coefficient step.
#' @return A list with the coefficients, the deviance, the iteration
#'   count and the score.
#' @export
morie_cypin_fit <- function(X, y, ridge = 1e-6, iters = 50L,
                            tol = 1e-12) {
  n <- length(X)
  if (n == 0L) stop("a fit needs data")
  if (length(y) != n) stop("one label per compound")
  p <- length(X[[1]]) + 1L
  D <- lapply(X, function(row) c(1, as.numeric(row)))
  yy <- as.numeric(ifelse(as.numeric(y) != 0, 1, 0))
  b <- numeric(p)
  it <- 0L
  for (it in seq_len(as.integer(iters))) {
    eta <- vapply(seq_len(n), function(i) .w3_dot(D[[i]], b), numeric(1))
    mu <- vapply(eta, morie_cypin_logistic, numeric(1))
    w <- pmax(mu * (1 - mu), 1e-10)
    z <- eta + (yy - mu) / w
    A <- matrix(0, p, p)
    for (a in seq_len(p)) for (cc in seq_len(p))
      A[a, cc] <- .w3_csum(vapply(seq_len(n), function(i)
        D[[i]][a] * w[i] * D[[i]][cc], numeric(1)))
    if (p >= 2L) for (a in 2:p) A[a, a] <- A[a, a] + ridge
    rhs <- vapply(seq_len(p), function(a)
      .w3_csum(vapply(seq_len(n), function(i) D[[i]][a] * w[i] * z[i],
                      numeric(1))), numeric(1))
    nb <- .w3_solve_chol(.w3_chol(A), rhs)
    step <- max(abs(nb - b))
    b <- nb
    if (step < tol) break
  }
  eta <- vapply(seq_len(n), function(i) .w3_dot(D[[i]], b), numeric(1))
  mu <- vapply(eta, morie_cypin_logistic, numeric(1))
  terms <- vapply(seq_len(n), function(i)
    yy[i] * log(if (mu[i] > 1e-300) mu[i] else 1e-300) +
      (1 - yy[i]) * log(if (mu[i] < 1 - 1e-300) 1 - mu[i] else 1e-300),
    numeric(1))
  dev <- -2 * .w3_csum(terms)
  # The score, which is zero at an unpenalised optimum. Reported rather
  # than asserted, because with a ridge it is zero only up to the
  # penalty -- and a caller who sees it large knows the fit did not
  # converge whatever the iteration count says.
  score <- vapply(seq_len(p), function(a)
    .w3_csum(vapply(seq_len(n), function(i) D[[i]][a] * (yy[i] - mu[i]),
                    numeric(1))), numeric(1))
  list(coefficients = b, deviance = dev, iterations = it, score = score,
       ridge = as.numeric(ridge), n = n, p = p)
}

#' The inhibition probability of one descriptor row
#'
#' @param x A descriptor row.
#' @param coefficients The intercept followed by one slope per
#'   descriptor.
#' @return A probability.
#' @export
morie_cypin_predict <- function(x, coefficients) {
  if (length(coefficients) != length(x) + 1L)
    stop("the model must have one coefficient per descriptor plus an ",
         "intercept")
  morie_cypin_logistic(coefficients[1] +
                         .w3_dot(coefficients[-1], as.numeric(x)))
}

#' Descriptors for a compound against one P450 isozyme
#'
#' @param smiles The compound.
#' @param isozyme One of 1A2, 2C9, 2C19, 2D6, 3A4.
#' @param model Coefficients from the fit, or the list the fit returns.
#'   NULL means no prediction is made and the reason says so.
#' @return A list with the descriptors, named; the probability if a
#'   model was given; and otherwise the reason there is none.
#' @export
morie_cypin <- function(smiles, isozyme, model = NULL) {
  if (!(isozyme %in% .cypin_isozymes))
    stop("the isozyme is one of ",
         paste(.cypin_isozymes, collapse = ", "))
  x <- morie_cypin_descriptors(smiles)
  named <- as.list(x)
  names(named) <- .cypin_names
  coef <- NULL
  if (!is.null(model))
    coef <- if (is.list(model)) model$coefficients else model
  if (is.null(coef)) {
    pred <- NULL
    reason <- paste0(
      "no coefficients were supplied, and none are shipped: the screen ",
      "of Veith et al. is published as a dataset and its fitted model ",
      "is not published as a table, so any default here would be ",
      "invented. Fit one on inhibition data with this module's fit ",
      "function and pass it as model.")
  } else {
    pred <- morie_cypin_predict(x, coef)
    reason <- ""
  }
  list(descriptors = x, names = .cypin_names, named = named,
       predicted = pred,
       inhibits = if (is.null(pred)) NULL else pred >= 0.5,
       reason = reason, isozyme = isozyme, n_descriptors = length(x),
       has_model = !is.null(coef),
       method = paste0("P450 inhibition descriptors with a ",
                       "caller-fitted logistic model"))
}

#' One-line summary of the cypin module
#'
#' @return A character scalar.
#' @export
morie_cypin_cheatsheet <- function()
  paste0("cypin: P450 inhibition for 1A2/2C9/2C19/2D6/3A4. Exact graph ",
         "descriptors plus a logistic model the caller fits; no ",
         "coefficients are shipped because none are published")
