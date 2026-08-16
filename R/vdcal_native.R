# SPDX-License-Identifier: AGPL-3.0-or-later
# R arm of vdcal -- steady-state volume of distribution by the Oie-Tozer
# model. Mirrors src/morie/fn/vdcal.py operation for operation, on the
# shared numerics in R/aaa_helpers_w3num.R.
#
# Volume of distribution is not a volume. It is the proportionality
# constant between the amount of drug in the body and its concentration
# in plasma, so a drug that hides in fat has a "volume" of several
# hundred litres in a seventy-kilogram person and one that stays in the
# blood has about three. What the Oie-Tozer model does is take that
# number apart into physiology and chemistry:
#
#     Vss = Vp (1 + Re_i)
#           + fu Vp (Ve/Vp - Re_i)
#           + Vr (fu / fut)
#
# Three terms, and each one is a different place the drug can be. The
# first is plasma plus the protein that leaks out of it -- Re_i is the
# ratio of extravascular to intravascular albumin, and it is there
# whether the drug binds anything or not. The second is the drug free in
# plasma distributing into extracellular water, which is why it carries
# fu and why its sign depends on whether extracellular water is bigger
# than the albumin term. The third is everything else: tissue. It
# carries the ratio of the free fractions, and it is the term that makes
# volumes of distribution large, because fut for a lipophilic base can
# be a thousandth.
#
# The physiological constants for a human are Vp = 0.0436, Ve = 0.151
# and Vr = 0.380 litres per kilogram, with Re_i = 1.4. Those four
# numbers alone, with fu = fut = 1, give Vss = 0.5746 l/kg -- which is
# total body water to two figures, and is the check that the arithmetic
# is assembled right rather than merely plausible.
#
# The equation runs both ways and both directions are here. Forward,
# from the two free fractions to the volume. Backward, from a MEASURED
# volume to fut, which is the use that makes the model interesting: fut
# is not measurable in a person, and solving for it turns a
# pharmacokinetic observation into a statement about tissue binding. The
# inverse is closed form, not a search, so the round trip is exact.
#
# What this module does NOT do is predict fut from structure. Lombardo
# and colleagues do that with a regression on ElogD(7.4), the ionised
# fraction at pH 7.4 and fu, and the coefficients of that regression are
# data -- they belong to the papers, not here. The route exists and
# takes the coefficients from the caller; with none supplied it says so
# rather than inventing them.
#
# References
#   Oie, S. and Tozer, T.N. (1979) "Effect of altered plasma protein
#     binding on apparent volume of distribution." Journal of
#     Pharmaceutical Sciences 68(9), 1203-1205.
#     doi:10.1002/jps.2600680948.
#   Lombardo, F., Obach, R.S., Shalaeva, M.Y. and Gao, F. (2002)
#     "Prediction of volume of distribution values in humans for neutral
#     and basic drugs using physicochemical measurements and plasma
#     protein binding data." Journal of Medicinal Chemistry 45(13),
#     2867-2876.
#   Waters, N.J. and Lombardo, F. (2010) "Use of the Oie-Tozer model in
#     understanding mechanisms and determinants of drug distribution."
#     Drug Metabolism and Disposition 38(7), 1094-1102.
#     doi:10.1124/dmd.110.032722.
#   Lombardo, F., Berellini, G. and Obach, R.S. (2019) "An accurate in
#     vitro prediction of human VDss based on the Oie-Tozer equation and
#     primary physicochemical descriptors. 3." Drug Metabolism and
#     Disposition 47(12), 1380-1387.

.VDCAL_DIRECTIONS <- c("vss", "fut")

# Human physiology, in litres per kilogram, with the extravascular to
# intravascular albumin ratio. Waters and Lombardo (2010).
.VDCAL_HUMAN <- list(Vp = 0.0436, Ve = 0.151, Vr = 0.380, Re_i = 1.4)

.vdcal_phys <- function(par) {
  p <- .VDCAL_HUMAN
  if (!is.null(par)) for (nm in names(par)) p[[nm]] <- par[[nm]]
  for (k in c("Vp", "Ve", "Vr"))
    if (p[[k]] <= 0) stop(k, " must be positive")
  if (p$Re_i < 0) stop("the albumin ratio cannot be negative")
  p
}

#' The volume of distribution, in litres per kilogram
#'
#' Returns the total and the three terms separately, because the
#' interesting question about a large volume is almost always WHICH term
#' made it large.
#'
#' @param fu The plasma free fraction.
#' @param fut The tissue free fraction.
#' @param par Overrides for the physiological constants, or NULL.
#' @return A list with the total and the plasma, extracellular and
#'   tissue terms.
#' @export
morie_vdcal_oie_tozer <- function(fu, fut, par = NULL) {
  p <- .vdcal_phys(par)
  fu <- as.numeric(fu); fut <- as.numeric(fut)
  if (!(fu > 0 && fu <= 1))
    stop("the plasma free fraction must lie in (0, 1]")
  if (!(fut > 0 && fut <= 1))
    stop("the tissue free fraction must lie in (0, 1]")
  plasma <- p$Vp * (1 + p$Re_i)
  extra <- fu * p$Vp * (p$Ve / p$Vp - p$Re_i)
  tissue <- p$Vr * (fu / fut)
  list(total = .w3_csum(c(plasma, extra, tissue)), plasma = plasma,
       extra = extra, tissue = tissue)
}

#' Solve the model backwards for the tissue free fraction
#'
#' Closed form, not a search: the volume is linear in fu/fut, so
#' inverting it is arithmetic. A measured volume smaller than the
#' plasma-and-albumin floor cannot be produced by this model at any
#' tissue binding, and that is an error rather than a negative fut.
#'
#' @param vss The measured volume, in litres per kilogram.
#' @param fu The plasma free fraction.
#' @param par Overrides for the physiological constants, or NULL.
#' @return The implied tissue free fraction.
#' @export
morie_vdcal_fut <- function(vss, fu, par = NULL) {
  p <- .vdcal_phys(par)
  fu <- as.numeric(fu)
  if (!(fu > 0 && fu <= 1))
    stop("the plasma free fraction must lie in (0, 1]")
  plasma <- p$Vp * (1 + p$Re_i)
  extra <- fu * p$Vp * (p$Ve / p$Vp - p$Re_i)
  rest <- as.numeric(vss) - plasma - extra
  if (rest <= 0)
    stop("this volume is below what plasma and extracellular water ",
         "alone account for; no tissue binding can produce it")
  fut <- p$Vr * fu / rest
  if (fut > 1)
    stop("the implied tissue free fraction exceeds one, which would ",
         "mean the tissue concentrates the drug less than water does")
  fut
}

# The Lombardo descriptor route, on caller-supplied coefficients:
#   log(1/fut) = a + b ElogD(7.4) + c fi(7.4) + d log(1/fu)
# The four coefficients are fitted quantities that belong to the papers.
# This module will not guess them: pass them, or use the measured route.
.vdcal_fut_descriptors <- function(elogd, fi, fu, coefficients) {
  if (is.null(coefficients) || !length(coefficients))
    stop("the descriptor route needs the regression coefficients; they ",
         "are fitted values from Lombardo et al. and are not shipped here")
  cc <- list(a = 0, b = 0, c = 0, d = 0)
  for (nm in names(coefficients)) cc[[nm]] <- coefficients[[nm]]
  fu <- as.numeric(fu)
  if (!(fu > 0 && fu <= 1))
    stop("the plasma free fraction must lie in (0, 1]")
  y <- .w3_csum(c(cc$a, cc$b * as.numeric(elogd), cc$c * as.numeric(fi),
                  cc$d * log(1 / fu)))
  fut <- exp(-y)
  if (fut <= 0 || fut > 1)
    stop("the fitted tissue free fraction fell outside (0, 1]; the ",
         "coefficients and the descriptors do not belong to the same model")
  fut
}

#' Volume of distribution at steady state, forwards or backwards
#'
#' @param smiles Carried through untouched. The model is physiological,
#'   not structural: nothing here reads the structure, and pretending
#'   otherwise would be the fabrication this module exists to avoid.
#' @param ppb Plasma protein binding as the FREE fraction, in (0, 1]. A
#'   drug quoted as 99 percent bound has fu = 0.01.
#' @param fut The tissue free fraction, for the forward direction, or
#'   NULL.
#' @param vss A measured volume in litres per kilogram, for the inverse
#'   direction, or NULL.
#' @param direction A member of the direction list.
#' @param weight Body mass, used only to report the volume in litres as
#'   well as litres per kilogram.
#' @param par Overrides for the physiological constants, or NULL.
#' @param elogd The lipophilicity descriptor, or NULL.
#' @param fi The ionised fraction at pH 7.4, or NULL.
#' @param coefficients The fitted regression coefficients, or NULL.
#' @return A list with the volume in both units, the three terms, the
#'   tissue free fraction used or solved for, and the route taken.
#' @export
morie_vdcal <- function(smiles, ppb, fut = NULL, vss = NULL,
                        direction = "vss", weight = 70, par = NULL,
                        elogd = NULL, fi = NULL, coefficients = NULL) {
  if (!(direction %in% .VDCAL_DIRECTIONS))
    stop("direction must be one of ",
         paste(.VDCAL_DIRECTIONS, collapse = ", "))
  fu <- as.numeric(ppb)
  p <- .vdcal_phys(par)
  route <- "given"
  if (direction == "fut") {
    if (is.null(vss)) stop("the inverse direction needs a measured volume")
    ft <- morie_vdcal_fut(vss, fu, par)
    route <- "inverse"
  } else {
    if (is.null(fut)) {
      if (is.null(elogd) || is.null(fi))
        stop("give a tissue free fraction, or the descriptors and ",
             "coefficients to fit one")
      ft <- .vdcal_fut_descriptors(elogd, fi, fu, coefficients)
      route <- "descriptors"
    } else {
      ft <- as.numeric(fut)
    }
  }
  ot <- morie_vdcal_oie_tozer(fu, ft, par)
  w <- as.numeric(weight)
  if (w <= 0) stop("body mass must be positive")
  list(vss = ot$total, vss_litres = ot$total * w, estimate = ot$total,
       se = NaN, plasma_term = ot$plasma, extracellular_term = ot$extra,
       tissue_term = ot$tissue, fu = fu, fut = ft, binding_ratio = fu / ft,
       weight = w, Vp = p$Vp, Ve = p$Ve, Vr = p$Vr, Re_i = p$Re_i,
       smiles = smiles, direction = direction, route = route,
       method = "Oie-Tozer steady-state volume of distribution")
}

#' One-line summary of the vdcal module
#'
#' @return A character scalar.
#' @export
morie_vdcal_cheatsheet <- function()
  paste0("vdcal: Oie-Tozer steady-state volume of distribution. ",
         "directions ", paste(.VDCAL_DIRECTIONS, collapse = ", "),
         "; human Vp 0.0436, Ve 0.151, Vr 0.380 l/kg, Re/I 1.4")
