# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Research P15: sentencing disparity decompositions (research/lean/P15Decomposition.lean;
# Oaxaca 1973; Blinder 1973; Oaxaca & Ransom 1999).
#
#   Research.P15.twofold_B / twofold_A    gap = explained (reference prices) + unexplained (other group's means)
#   Research.P15.threefold                gap = endowments + coefficients + interaction, exactly
#   Research.P15.reference_dependence     explained_A - explained_B = interaction; equal iff interaction = 0
#   Research.P15.attribution_shift        shifting a covariate moves c (beta_A - beta_B)_j between attribution lines

#' Oaxaca-Blinder decomposition of a sentencing gap, with its two references and the interaction
#'
#' Fits \code{formula} separately in the two groups by least squares (so each
#' fitted line passes through its group means, \code{Research.P15.Groups.yA_eq})
#' and decomposes the gap in mean outcomes exactly, three ways: explained at
#' group B's coefficients plus unexplained at A's means (\code{twofold_B}), the
#' same with the roles swapped (\code{twofold_A}), and endowments plus
#' coefficients plus interaction (\code{threefold}). The two explained parts
#' differ by exactly the interaction (\code{reference_dependence}), so the
#' share of a gap "explained by legal factors" is a choice of reference. The
#' per-variable attribution of the unexplained part moves when a covariate is
#' re-centred (\code{attribution_shift}); the function reports that shift for
#' each covariate so a reader sees how far the lines can move.
#' @param formula Outcome on covariates, e.g. \code{sentence ~ offence_score + priors}.
#' @param data Data frame.
#' @param group Name of the two-level grouping variable (character or factor).
#' @param reference Level treated as group B (the "reference prices"); the other is A.
#' @param shift Recentring amount used to report the attribution shift per covariate.
#' @return A list with \code{levels}, \code{means} (outcome means), \code{gap},
#'   \code{twofold_B}, \code{twofold_A} (each with \code{explained},
#'   \code{unexplained}), \code{threefold} (\code{endowments},
#'   \code{coefficients}, \code{interaction}), \code{by_variable} (a data frame
#'   with each covariate's explained and unexplained contributions under the
#'   B reference, and \code{unexplained_shift} = \code{shift * (beta_A - beta_B)}),
#'   \code{identity_checks} and \code{theorems}.
#' @examples
#' set.seed(3)
#' n <- 400
#' g <- rep(c("A", "B"), each = n / 2)
#' score <- rnorm(n, ifelse(g == "A", 5, 4)); priors <- rpois(n, ifelse(g == "A", 2, 1))
#' sentence <- 2 + 0.8 * score + 0.5 * priors + ifelse(g == "A", 1, 0) + rnorm(n)
#' d <- data.frame(sentence, score, priors, g)
#' r <- morie_disparity_decomposition(sentence ~ score + priors, d, group = "g", reference = "B")
#' c(gap = r$gap, explained_B = r$twofold_B[["explained"]], interaction = r$threefold[["interaction"]])
#' @export
morie_disparity_decomposition <- function(formula, data, group, reference, shift = 1) {
  gv <- as.character(data[[group]])
  lv <- sort(unique(gv))
  if (length(lv) != 2L) stop("group must take exactly two values", call. = FALSE)
  if (!reference %in% lv) stop("reference must be one of the two group levels", call. = FALSE)
  B <- as.character(reference); A <- setdiff(lv, B)
  fit <- function(l) stats::lm(formula, data = data[gv == l, , drop = FALSE])
  fA <- fit(A); fB <- fit(B)
  bA <- stats::coef(fA); bB <- stats::coef(fB)
  if (anyNA(bA) || anyNA(bB)) stop("the covariates are collinear within a group", call. = FALSE)
  xA <- colMeans(stats::model.matrix(fA)); xB <- colMeans(stats::model.matrix(fB))
  yA <- mean(stats::model.response(stats::model.frame(fA))); yB <- mean(stats::model.response(stats::model.frame(fB)))
  explained_B <- sum((xA - xB) * bB); unexplained_A <- sum(xA * (bA - bB))
  explained_A <- sum((xA - xB) * bA); unexplained_B <- sum(xB * (bA - bB))
  interaction <- sum((xA - xB) * (bA - bB))
  by_var <- data.frame(variable = names(bA), explained_B = (xA - xB) * bB, unexplained_A = xA * (bA - bB),
                       unexplained_shift = shift * (bA - bB), row.names = NULL)
  list(levels = c(A = A, B = B), means = c(A = yA, B = yB), gap = yA - yB,
       twofold_B = c(explained = explained_B, unexplained = unexplained_A),
       twofold_A = c(explained = explained_A, unexplained = unexplained_B),
       threefold = c(endowments = explained_B, coefficients = unexplained_B, interaction = interaction),
       by_variable = by_var,
       identity_checks = c(twofold_B = (yA - yB) - (explained_B + unexplained_A),
                           twofold_A = (yA - yB) - (explained_A + unexplained_B),
                           threefold = (yA - yB) - (explained_B + unexplained_B + interaction),
                           reference = (explained_A - explained_B) - interaction),
       theorems = c("Research.P15.twofold_B", "Research.P15.twofold_A", "Research.P15.threefold",
                    "Research.P15.reference_dependence", "Research.P15.explained_eq_iff", "Research.P15.attribution_shift"))
}
