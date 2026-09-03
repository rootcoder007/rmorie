# SPDX-License-Identifier: AGPL-3.0-or-later

#' Decomposition of the total AlphaFold training loss
#'
#' Supplement equation (7) of Jumper et al. (2021), p. 32.  A fixed
#' weighted sum of the FAPE loss, the structure module's auxiliary loss,
#' the distogram and masked-MSA cross entropies and the confidence loss,
#' with the experimentally-resolved and violation terms added only during
#' fine-tuning.  The coefficients are the published ones, not free
#' parameters.
#'
#' @param fape,aux,dist,msa,conf The five always-present loss terms.
#' @param expres,viol The two fine-tuning-only terms.
#' @param phase Either "training" or "finetuning".
#' @param ncrop Optional residue count after cropping; the total is then
#'   multiplied by its square root, the reweighting described just below
#'   equation (7).
#' @return A list with the weighted \code{terms}, the total
#'   \code{estimate}, the \code{unscaled} total, \code{phase}, \code{scale}
#'   and \code{method}.
#' @references Jumper et al (2021) Nature 596:583-589, Suppl. equation (7)
#' @examples
#' rmorie:::Alfloss(fape = c(1, 2, 3, 4, 5, 6, 7, 8), aux = c(1, 2, 3, 4, 5, 6, 7, 8),
#' dist = c(1, 2, 3, 4, 5, 6, 7, 8), msa = c(1, 2, 3, 4, 5, 6, 7, 8), conf = 0.5)
Alfloss <- function(fape, aux, dist, msa, conf, expres = 0, viol = 0,
                    phase = "training", ncrop = NULL) {
  if (!phase %in% c("training", "finetuning")) {
    stop("phase must be 'training' or 'finetuning'")
  }
  terms <- list(
    fape = 0.5 * fape, aux = 0.5 * aux, dist = 0.3 * dist,
    msa = 2.0 * msa, conf = 0.01 * conf
  )
  if (phase == "finetuning") {
    terms$expres <- 0.01 * expres
    terms$viol <- 1.0 * viol
  }
  total <- sum(unlist(terms))
  scale <- if (is.null(ncrop)) 1 else sqrt(as.numeric(ncrop))
  list(
    terms = terms, estimate = total * scale, unscaled = total,
    phase = phase, scale = scale,
    method = "AlphaFold total loss decomposition"
  )
}
