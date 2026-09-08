# SPDX-License-Identifier: AGPL-3.0-or-later
# R arm of halft -- plasma half-life from volume of distribution and
# clearance. Mirrors src/morie/fn/halft.py operation for operation.
#
# Half-life is a derived quantity, not a measured one, and that is the
# whole point. Clearance is the body's capacity to eliminate the drug
# and volume of distribution is how widely it spreads; half-life is what
# falls out of the two:
#
#     t_half = ln(2) * V / CL
#
# Reading it the other way round -- treating half-life as the primitive
# and inferring clearance from it -- is the standard mistake, because a
# long half-life can mean poor clearance OR wide distribution, and those
# have opposite consequences. A drug with a huge volume and good
# clearance has a long half-life and still leaves the body efficiently.
#
# Which V, and therefore which half-life, depends on the model:
#
#   "one_compartment"  V is the single apparent volume. One exponential,
#                      one half-life. Almost no real drug behaves this
#                      way after an intravenous dose, and using it
#                      anyway is how the distribution phase gets
#                      mistaken for elimination.
#
#   "two_compartment"  Central volume V1, peripheral V2, elimination
#                      clearance CL and inter-compartmental clearance Q.
#                      The concentration is a sum of two exponentials
#                      whose rate constants alpha and beta are the roots
#                      of
#                          lambda^2 - (k10 + k12 + k21) lambda
#                                   + k10 k21 = 0
#                      with k10 = CL/V1, k12 = Q/V1, k21 = Q/V2. The
#                      TERMINAL half-life ln(2)/beta is the one usually
#                      quoted, and on its own it misleads: if the
#                      terminal phase carries only a few per cent of the
#                      area it says almost nothing about accumulation.
#
#   "effective"        ln(2) * Vss / CL, the mean-residence-time
#                      half-life. This is the one that predicts
#                      accumulation on repeated dosing -- the question
#                      half-life is usually being asked to answer. It is
#                      reported alongside the terminal value for the
#                      two-compartment route precisely so the gap
#                      between them is visible.
#
# The smiles argument is carried through untouched. The identity is a
# statement about V and CL and does not involve the structure; the
# argument exists because the ledger's signature has it, and it is
# recorded in the result for provenance rather than quietly ignored. A
# structure-based PREDICTION of V or CL is a different module and should
# not be hidden inside this one.
#
# References
#   Rowland, M. and Tozer, T.N. (2011) "Clinical Pharmacokinetics and
#     Pharmacodynamics: Concepts and Applications," 4th edition. Wolters
#     Kluwer.
#   Gibaldi, M. and Perrier, D. (1982) "Pharmacokinetics," 2nd edition.
#     Marcel Dekker, chapter 2.
#   Boxenbaum, H. and Battle, M. (1995) "Effective half-life in clinical
#     pharmacokinetics." Journal of Clinical Pharmacology 35(8),
#     763-766.

.HALFT_ROUTES <- c("one_compartment", "two_compartment", "effective")
.HALFT_LN2 <- 0.6931471805599453

#' alpha and beta, the roots of the disposition quadratic
#'
#' Solved with the numerically stable form of the quadratic formula --
#' the naive (-b +- sqrt(disc)) / 2 cancels catastrophically when the two
#' rates are far apart, which is exactly the case where the terminal
#' half-life matters most.
#'
#' @param V1 Central volume.
#' @param V2 Peripheral volume.
#' @param CL Elimination clearance.
#' @param Q Inter-compartmental clearance.
#' @return A list with alpha, beta and the micro rate constants.
#' @export
morie_halft_rates <- function(V1, V2, CL, Q) {
  if (V1 <= 0 || V2 <= 0 || CL <= 0 || Q <= 0)
    stop("volumes and clearances must be positive")
  k10 <- CL / V1
  k12 <- Q / V1
  k21 <- Q / V2
  b <- k10 + k12 + k21
  cc <- k10 * k21
  disc <- b * b - 4 * cc
  if (disc < 0) disc <- 0
  root <- sqrt(disc)
  # The larger root first, then the smaller by c / alpha: computing the
  # small root as (b - root) / 2 subtracts two nearly equal numbers and
  # loses most of its digits.
  alpha <- 0.5 * (b + root)
  beta <- if (alpha > 0) cc / alpha else 0
  list(alpha = alpha, beta = beta, k10 = k10, k12 = k12, k21 = k21)
}

#' Plasma half-life from volume of distribution and clearance
#'
#' @param smiles Structure, carried through for provenance. Not used in
#'   the arithmetic; see the file header.
#' @param Vd Volume of distribution, in litres. The steady-state volume
#'   for the one-compartment and effective routes.
#' @param Cl Clearance, in litres per hour.
#' @param route A member of the route list.
#' @param V1 Central volume, for the two-compartment route.
#' @param V2 Peripheral volume.
#' @param Q Inter-compartmental clearance.
#' @param dose An intravenous dose; when supplied the result carries the
#'   biexponential coefficients.
#' @return A list with the half-life, the elimination rate constant,
#'   mean residence time, and for the two-compartment route both phase
#'   half-lives, the fraction of area in each and the effective
#'   half-life.
#' @export
morie_halft <- function(smiles = NULL, Vd = NULL, Cl = NULL,
                        route = "one_compartment", V1 = NULL, V2 = NULL,
                        Q = NULL, dose = NULL) {
  if (!(route %in% .HALFT_ROUTES))
    stop("route must be one of ", paste(.HALFT_ROUTES, collapse = ", "))
  if (is.null(Cl) || as.numeric(Cl) <= 0) stop("Cl must be positive")
  CL <- as.numeric(Cl)

  if (route == "two_compartment") {
    if (is.null(V1) || is.null(V2) || is.null(Q))
      stop("the two-compartment route needs V1, V2 and Q")
    V1 <- as.numeric(V1)
    V2 <- as.numeric(V2)
    Q <- as.numeric(Q)
    r <- morie_halft_rates(V1, V2, CL, Q)
    a <- r$alpha
    b <- r$beta
    vss <- if (is.null(Vd)) V1 + V2 else as.numeric(Vd)
    # MRT is the primitive and the effective half-life is ln(2) times
    # it. Writing the latter as LN2 * vss / CL instead groups the
    # arithmetic as (LN2 * vss) / CL, which differs from
    # LN2 * (vss / CL) in the last bit -- and the identity between the
    # two reported numbers then fails by an ulp.
    mrt <- vss / CL
    # Coefficients of the unit-dose biexponential, from the standard
    # partial fractions: C(t) = A exp(-alpha t) + B exp(-beta t).
    Aunit <- if (a != b) (a - r$k21) / (V1 * (a - b)) else 0
    Bunit <- if (a != b) (r$k21 - b) / (V1 * (a - b)) else 0
    auc_a <- if (a > 0) Aunit / a else 0
    auc_b <- if (b > 0) Bunit / b else 0
    auc <- auc_a + auc_b
    payload <- list(
      estimate = .HALFT_LN2 / b, half_life = .HALFT_LN2 / b,
      terminal_half_life = .HALFT_LN2 / b,
      distribution_half_life = .HALFT_LN2 / a,
      effective_half_life = .HALFT_LN2 * mrt,
      alpha = a, beta = b, k10 = r$k10, k12 = r$k12, k21 = r$k21,
      A_unit = Aunit, B_unit = Bunit, auc_unit = auc,
      fraction_area_terminal = if (auc > 0) auc_b / auc else NaN,
      Vss = vss, V1 = V1, V2 = V2, Q = Q,
      mean_residence_time = mrt)
    if (!is.null(dose)) {
      payload$A <- as.numeric(dose) * Aunit
      payload$B <- as.numeric(dose) * Bunit
      payload$auc <- as.numeric(dose) * auc
      payload$dose <- as.numeric(dose)
    }
  } else {
    if (is.null(Vd) || as.numeric(Vd) <= 0) stop("Vd must be positive")
    V <- as.numeric(Vd)
    k <- CL / V
    mrt <- V / CL
    payload <- list(estimate = .HALFT_LN2 / k, half_life = .HALFT_LN2 / k,
                    terminal_half_life = .HALFT_LN2 / k,
                    effective_half_life = .HALFT_LN2 * mrt,
                    k_elimination = k, Vss = V,
                    mean_residence_time = mrt)
    if (!is.null(dose)) {
      payload$A_unit <- 1 / V
      payload$auc_unit <- 1 / CL
      payload$auc <- as.numeric(dose) / CL
      payload$dose <- as.numeric(dose)
    }
  }

  payload$Cl <- CL
  payload$route <- route
  # Assigned through [ ] rather than $ so a NULL structure is STORED as
  # an element rather than deleting the field -- otherwise the payload
  # silently loses a key whenever no structure was supplied.
  payload["smiles"] <- list(smiles)
  payload$se <- NaN
  payload$method <- "plasma half-life from volume and clearance"
  payload
}

#' One-line summary of the halft module
#'
#' @return A character scalar.
#' @export
morie_halft_cheatsheet <- function()
  paste0("halft: plasma half-life. routes ",
         paste(.HALFT_ROUTES, collapse = ", "))
