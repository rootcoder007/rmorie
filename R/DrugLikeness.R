#' Oral drug-likeness rule filters
#'
#' \code{LipinskiRuleOfFive}: rule of five with at most one violation.
#' \code{VeberRules}: rotatable bonds and polar surface area (or H-bond count).
#' \code{EganEgg}: PSA and logP limits of the absorption egg.
#' \code{OralBioavailabilityRules}: all three together. Identical to the Python
#' arm \code{morie.fn.drugrules}.
#'
#' @param mw Molecular weight.
#' @param logp Octanol-water logP.
#' @param hbd,hba H-bond donors and acceptors.
#' @param max_violations Allowed Lipinski violations.
#' @param rotatable_bonds Number of rotatable bonds.
#' @param psa Topological polar surface area (square Angstrom).
#' @param hbond_total Donors plus acceptors (alternative to psa).
#' @return A list.
#' @references Lipinski, C. A. et al. (1997). Advanced Drug Delivery Reviews
#'   23, 3-25.
#'
#'   Veber, D. F. et al. (2002). Journal of Medicinal Chemistry 45, 2615-2623.
#'
#'   Egan, W. J., Merz, K. M. and Baldwin, J. J. (2000). Journal of Medicinal
#'   Chemistry 43, 3867-3877.
#' @examples
#' LipinskiRuleOfFive(520, 5.6, 2, 7)$passes
#' OralBioavailabilityRules(350, 2.5, 2, 5, 6, 80)$n_passed
#' @export
LipinskiRuleOfFive <- function(mw, logp, hbd, hba, max_violations = 1) {
  crit <- list(mw = mw <= 500, logp = logp <= 5, hbd = hbd <= 5, hba = hba <= 10)
  v <- sum(!unlist(crit))
  list(violations = v, passes = v <= max_violations, criteria = crit)
}

#' @rdname LipinskiRuleOfFive
#' @export
VeberRules <- function(rotatable_bonds, psa = NULL, hbond_total = NULL) {
  if (is.null(psa) && is.null(hbond_total)) stop("give psa or hbond_total")
  polar <- if (!is.null(psa)) psa <= 140 else hbond_total <= 12
  list(passes = rotatable_bonds <= 10 && polar, rotatable_ok = rotatable_bonds <= 10, polar_ok = polar)
}

#' @rdname LipinskiRuleOfFive
#' @export
EganEgg <- function(psa, logp) {
  list(passes = psa <= 131.6 && logp >= -1 && logp <= 5.88, psa_ok = psa <= 131.6, logp_ok = logp >= -1 && logp <= 5.88)
}

#' @rdname LipinskiRuleOfFive
#' @export
OralBioavailabilityRules <- function(mw, logp, hbd, hba, rotatable_bonds, psa) {
  lip <- LipinskiRuleOfFive(mw, logp, hbd, hba)$passes
  veb <- VeberRules(rotatable_bonds, psa)$passes
  egg <- EganEgg(psa, logp)$passes
  n <- lip + veb + egg
  list(lipinski = lip, veber = veb, egan = egg, n_passed = n, passes = n == 3)
}
