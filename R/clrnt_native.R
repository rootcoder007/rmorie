# In vitro to in vivo prediction of hepatic intrinsic clearance.
# Sources: Wood, F. L., Houston, J. B. & Hallifax, D. (2017) "Clearance
# Prediction Methodology Needs Fundamental Improvement: Trends Common
# to Rat and Human Hepatocytes/Microsomes and Implications for
# Experimental Methodology", *Drug Metabolism and Disposition* 45(11),
# 1178-1188, equations 1-8 and the physiological scaling constants in
# its Methods. The ledger's second citation, Pirmohamed 2019, does
# not exist and is not used.

# Native implementation mirroring Python morie.fn.clrnt exactly: the
# same Hallifax-Houston binding term for microsomes and hepatocytes,
# the same physiological scaling constants, the same well-stirred
# and parallel-tube back-solves, the same AFE / RMSE / ESF / 2-fold
# accuracy summary, and the same blood-from-plasma bookkeeping with
# the paper's R_b defaults by charge.

.CLRNT_CONSTANTS <- list(
  human = list(microsomes_pbsf = 40.0,
               hepatocytes_pbsf = 120e6,
               liver_weight = 21.4,
               qh = 20.7),
  rat = list(microsomes_pbsf = 60.0,
             hepatocytes_pbsf = 120e6,
             liver_weight = 40.0,
             qh = 100.0)
)

#' .clrnt_binding_term
#'
#' A step of the clrnt_native implementation. Called by \code{fu_hepatocytes}, \code{fu_microsomes}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param log_pd Coerced to numeric by the body, with \code{as.numeric}.
#' @return A numeric value.
#' @export
.clrnt_binding_term <- function(log_pd) {
  x <- as.numeric(log_pd)
  10 ^ (0.072 * x * x + 0.067 * x - 1.126)
}

#' fu_microsomes
#'
#' A step of the clrnt_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param log_pd Passed to \code{.clrnt_binding_term}.
#' @param protein Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.
#' @return A numeric value.
#' @export
fu_microsomes <- function(log_pd, protein = 1.0) {
  if (as.numeric(protein) <= 0)
    stop("clrnt: microsomal protein concentration must be positive")
  1.0 / (1.0 + as.numeric(protein) * .clrnt_binding_term(log_pd))
}

#' fu_hepatocytes
#'
#' A step of the clrnt_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param log_pd Passed to \code{.clrnt_binding_term}.
#' @param volume_ratio Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.005}.
#' @return A numeric value.
#' @export
fu_hepatocytes <- function(log_pd, volume_ratio = 0.005) {
  if (as.numeric(volume_ratio) <= 0)
    stop("clrnt: the volume ratio must be positive")
  1.0 / (1.0 + 125.0 * as.numeric(volume_ratio) *
           .clrnt_binding_term(log_pd))
}

#' blood_from_plasma
#'
#' A step of the clrnt_native implementation. Called by \code{clrnt}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param cl_plasma Coerced to numeric by the body, with \code{as.numeric}.
#' @param fu_plasma Coerced to numeric by the body, with \code{as.numeric}.
#' @param blood_plasma_ratio Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.
#' @param charge One of \code{"acidic"}, \code{"basic"}, \code{"neutral"}. Defaults to \code{"neutral"}.
#' @return A list with \code{cl_blood}, \code{fu_blood}, \code{rb}.
#' @export
blood_from_plasma <- function(cl_plasma, fu_plasma,
                               blood_plasma_ratio = NULL,
                               charge = "neutral") {
  if (!(charge %in% c("acidic", "basic", "neutral")))
    stop("clrnt: charge must be acidic, basic or neutral")
  rb <- if (is.null(blood_plasma_ratio))
    (if (charge == "acidic") 0.55 else 1.0) else as.numeric(blood_plasma_ratio)
  if (rb <= 0)
    stop("clrnt: the blood/plasma ratio must be positive")
  list(cl_blood = as.numeric(cl_plasma) / rb,
       fu_blood = as.numeric(fu_plasma) / rb,
       rb = rb)
}

#' scale_to_liver
#'
#' A step of the clrnt_native implementation. Called by \code{clrnt}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param clint_in_vitro Coerced to numeric by the body, with \code{as.numeric}.
#' @param fu_incubation Coerced to numeric by the body, with \code{as.numeric}.
#' @param system One of \code{"hepatocytes"}, \code{"microsomes"}. Defaults to \code{"hepatocytes"}.
#' @param species The body requires: clrnt: species must be 'human' or 'rat'. Defaults to \code{"human"}.
#' @param pbsf Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.
#' @param liver_weight Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.
#' @return A numeric value.
#' @export
scale_to_liver <- function(clint_in_vitro, fu_incubation,
                           system = "hepatocytes", species = "human",
                           pbsf = NULL, liver_weight = NULL) {
  if (!(species %in% names(.CLRNT_CONSTANTS)))
    stop("clrnt: species must be 'human' or 'rat'")
  if (!(system %in% c("hepatocytes", "microsomes")))
    stop("clrnt: system must be 'hepatocytes' or 'microsomes'")
  fu <- as.numeric(fu_incubation)
  if (!(fu > 0 && fu <= 1))
    stop("clrnt: the incubational unbound fraction must lie in (0, 1]")
  c <- .CLRNT_CONSTANTS[[species]]
  p <- if (is.null(pbsf)) {
    v <- if (system == "hepatocytes") c$hepatocytes_pbsf
         else c$microsomes_pbsf
    if (system == "hepatocytes") v / 1e6 else v
  } else as.numeric(pbsf)
  lw <- if (is.null(liver_weight)) c$liver_weight
        else as.numeric(liver_weight)
  as.numeric(clint_in_vitro) * p * lw / fu
}

#' observed_clint_u
#'
#' A step of the clrnt_native implementation. Called by \code{clrnt}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param cl_h Coerced to numeric by the body, with \code{as.numeric}.
#' @param fu_blood Coerced to numeric by the body, with \code{as.numeric}.
#' @param species The body requires: clrnt: species must be 'human' or 'rat'. Defaults to \code{"human"}.
#' @param qh Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.
#' @param liver_model One of \code{"parallel_tube"}, \code{"well_stirred"}. Defaults to \code{"well_stirred"}.
#' @return One of two values, depending on the branch taken.
#' @export
observed_clint_u <- function(cl_h, fu_blood, species = "human",
                             qh = NULL,
                             liver_model = "well_stirred") {
  if (!(species %in% names(.CLRNT_CONSTANTS)))
    stop("clrnt: species must be 'human' or 'rat'")
  if (!(liver_model %in% c("well_stirred", "parallel_tube")))
    stop("clrnt: liver_model must be 'well_stirred' or 'parallel_tube'")
  q <- if (is.null(qh)) .CLRNT_CONSTANTS[[species]]$qh else as.numeric(qh)
  cl <- as.numeric(cl_h)
  fu <- as.numeric(fu_blood)
  if (!(fu > 0 && fu <= 1))
    stop("clrnt: the unbound fraction must lie in (0, 1]")
  if (cl <= 0)
    stop("clrnt: hepatic clearance must be positive")
  if (cl >= q)
    stop(sprintf("clrnt: hepatic clearance cannot reach or exceed hepatic blood flow (%.4g >= %.4g ml/min/kg)",
                 cl, q))
  if (liver_model == "well_stirred")
    cl / (fu * (1.0 - cl / q))
  else
    -q * log(1.0 - cl / q) / fu
}

#' prediction_accuracy
#'
#' A step of the clrnt_native implementation. Called by \code{clrnt}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param predicted Coerced to numeric by the body, with \code{as.numeric}.
#' @param observed Coerced to numeric by the body, with \code{as.numeric}.
#' @param fold Numeric; combined arithmetically in the body. Defaults to \code{2}.
#' @return A list with \code{afe}, \code{fold_underprediction}, \code{rmse}, \code{esf}, \code{average_esf}, \code{within_fold}, \code{beyond_fold}, \code{n}, \code{fold}.
#' @export
prediction_accuracy <- function(predicted, observed, fold = 2.0) {
  p <- as.numeric(predicted)
  o <- as.numeric(observed)
  n <- length(p)
  if (n == 0L || n != length(o))
    stop("clrnt: need one observed value per prediction")
  if (any(p <= 0) || any(o <= 0))
    stop("clrnt: clearances must be positive to take logs")
  afe <- 10 ^ (sum(log10(p / o)) / n)
  rmse <- sqrt(sum((p - o)^2) / n)
  esf <- o / p
  avg_esf <- 10 ^ (sum(log10(o / p)) / n)
  within <- mean((1.0 / fold <= p / o) & (p / o <= fold))
  list(afe = afe, fold_underprediction = 1.0 / afe, rmse = rmse,
       esf = esf, average_esf = avg_esf,
       within_fold = within, beyond_fold = 1.0 - within,
       n = n, fold = as.numeric(fold))
}

#' clrnt
#'
#' A step of the clrnt_native implementation. Called by \code{morie_clrnt}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param clint_in_vitro A vector; its length is taken.
#' @param cl_h Passed to \code{spread}.
#' @param fu_blood Passed to \code{spread}.
#' @param log_pd The body requires: clrnt: give either fu_incubation or log_pd so equations 1-2 can estimate it.
#' @param fu_incubation The body requires: clrnt: give either fu_incubation or log_pd so equations 1-2 can estimate it.
#' @param system Compared against \code{"microsomes"}. Defaults to \code{"hepatocytes"}.
#' @param species Carried through into a list the body builds. Defaults to \code{"human"}.
#' @param liver_model Carried through into a list the body builds. Defaults to \code{"well_stirred"}.
#' @param protein Iterated over elementwise, with \code{vapply}. Defaults to \code{1}.
#' @param volume_ratio Iterated over elementwise, with \code{vapply}. Defaults to \code{0.005}.
#' @param cl_plasma Passed to \code{spread}.
#' @param fu_plasma The body requires: clrnt: plasma clearance needs fu_plasma too.
#' @param blood_plasma_ratio Passed to \code{spread}.
#' @param charge Passed to \code{blood_from_plasma}. Defaults to \code{"neutral"}.
#' @param fold Passed to \code{prediction_accuracy}. Defaults to \code{2}.
#' @return The value of \code{out}, as built in the body.
#' @export
clrnt <- function(clint_in_vitro, cl_h = NULL, fu_blood = NULL,
                  log_pd = NULL, fu_incubation = NULL,
                  system = "hepatocytes", species = "human",
                  liver_model = "well_stirred", protein = 1.0,
                  volume_ratio = 0.005, cl_plasma = NULL,
                  fu_plasma = NULL, blood_plasma_ratio = NULL,
                  charge = "neutral", fold = 2.0) {
  single <- length(clint_in_vitro) == 1L
  cl_in <- as.numeric(clint_in_vitro)
  n <- length(cl_in)

  spread <- function(v, name) {
    if (is.null(v)) return(NULL)
    if (length(v) == 1L) return(rep(as.numeric(v), n))
    if (length(v) != n)
      stop(sprintf("clrnt: %s must have one entry per compound", name))
    as.numeric(v)
  }

  lp <- spread(log_pd, "log_pd")
  fu_inc <- spread(fu_incubation, "fu_incubation")
  clh <- spread(cl_h, "cl_h")
  fub <- spread(fu_blood, "fu_blood")
  clp <- spread(cl_plasma, "cl_plasma")
  fup <- spread(fu_plasma, "fu_plasma")
  rbs <- spread(blood_plasma_ratio, "blood_plasma_ratio")

  rb_used <- NULL
  if (is.null(clh) && !is.null(clp)) {
    if (is.null(fup))
      stop("clrnt: plasma clearance needs fu_plasma too")
    clh <- numeric(n)
    fub <- numeric(n)
    rb_used <- numeric(n)
    for (i in seq_len(n)) {
      a <- blood_from_plasma(clp[i], fup[i],
                             if (is.null(rbs)) NULL else rbs[i],
                             charge)
      clh[i] <- a$cl_blood
      fub[i] <- a$fu_blood
      rb_used[i] <- a$rb
    }
  }

  if (is.null(fu_inc)) {
    if (is.null(lp))
      stop("clrnt: give either fu_incubation or log_pd so equations 1-2 can estimate it")
    fu_inc <- if (system == "microsomes")
      vapply(lp, fu_microsomes, numeric(1), protein = protein)
    else
      vapply(lp, fu_hepatocytes, numeric(1), volume_ratio = volume_ratio)
  }

  predicted <- vapply(seq_len(n),
                      function(i) scale_to_liver(cl_in[i], fu_inc[i],
                                                 system, species),
                      numeric(1))

  out <- list(estimate = if (single) predicted[1L] else predicted,
              predicted = if (single) predicted[1L] else predicted,
              fu_incubation = if (single) fu_inc[1L] else fu_inc,
              system = system, species = species,
              liver_model = liver_model,
              constants = .CLRNT_CONSTANTS[[species]],
              blood_plasma_ratio = rb_used,
              note = paste("predictions of this kind are systematically",
                           "LOW and the shortfall grows with in vivo",
                           "clearance (Wood, Houston & Hallifax 2017);",
                           "the accuracy block is how you measure it on",
                           "your own data"),
              method = paste("in vitro to in vivo CLint,u prediction",
                             "(Wood, Houston & Hallifax 2017)"))
  if (!is.null(clh) && !is.null(fub)) {
    obs <- vapply(seq_len(n),
                  function(i) observed_clint_u(clh[i], fub[i], species,
                                               NULL, liver_model),
                  numeric(1))
    out$observed <- if (single) obs[1L] else obs
    out$cl_h <- if (single) clh[1L] else clh
    out$fu_blood <- if (single) fub[1L] else fub
    out$accuracy <- prediction_accuracy(predicted, obs, fold)
  }
  out
}

hepatic_clearance_prediction <- clrnt
hepatic_clearance <- clrnt
clearance_intrinsic <- clrnt

#' morie_clrnt
#'
#' A step of the clrnt_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param clint_in_vitro Passed to \code{clrnt}.
#' @param cl_h Passed to \code{clrnt}.
#' @param fu_blood Passed to \code{clrnt}.
#' @param log_pd Passed to \code{clrnt}.
#' @param fu_incubation Passed to \code{clrnt}.
#' @param system Passed to \code{clrnt}. Defaults to \code{"hepatocytes"}.
#' @param species Passed to \code{clrnt}. Defaults to \code{"human"}.
#' @param liver_model Passed to \code{clrnt}. Defaults to \code{"well_stirred"}.
#' @param protein Passed to \code{clrnt}. Defaults to \code{1}.
#' @param volume_ratio Passed to \code{clrnt}. Defaults to \code{0.005}.
#' @param cl_plasma Passed to \code{clrnt}.
#' @param fu_plasma Passed to \code{clrnt}.
#' @param blood_plasma_ratio Passed to \code{clrnt}.
#' @param charge Passed to \code{clrnt}. Defaults to \code{"neutral"}.
#' @param fold Passed to \code{clrnt}. Defaults to \code{2}.
#' @return The value of \code{clrnt}.
#' @export
morie_clrnt <- function(clint_in_vitro, cl_h = NULL, fu_blood = NULL,
                       log_pd = NULL, fu_incubation = NULL,
                       system = "hepatocytes", species = "human",
                       liver_model = "well_stirred", protein = 1.0,
                       volume_ratio = 0.005, cl_plasma = NULL,
                       fu_plasma = NULL, blood_plasma_ratio = NULL,
                       charge = "neutral", fold = 2.0) {
  clrnt(clint_in_vitro, cl_h = cl_h, fu_blood = fu_blood,
        log_pd = log_pd, fu_incubation = fu_incubation,
        system = system, species = species,
        liver_model = liver_model, protein = protein,
        volume_ratio = volume_ratio, cl_plasma = cl_plasma,
        fu_plasma = fu_plasma, blood_plasma_ratio = blood_plasma_ratio,
        charge = charge, fold = fold)
}

#' .clrnt_cheatsheet
#'
#' A step of the clrnt_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
.clrnt_cheatsheet <- function() {
  paste("clrnt: in vitro to in vivo CLint,u (Wood, Houston & Hallifax",
        "2017). fu in the incubation from eq.1 (microsomes) or eq.2",
        "(hepatocytes) when unmeasured; scale by PBSF x liver weight",
        "over fu (eq.3) -- 40 mg/g human microsomes, 60 rat, 120e6",
        "cells/g both, 21.4 g/kg human liver, 40 g/kg rat; observed",
        "from CLh/(fub[1 - CLh/Qh]) (eq.4, well-stirred; parallel",
        "tube also available), Qh 20.7 human, 100 rat. Accuracy by",
        "AFE (eq.5), RMSE (eq.6), ESF = observed/predicted (eq.7)",
        "and its log average (eq.8), plus the 2-fold count. The",
        "paper's finding is that this pipeline UNDERPREDICTS, worse",
        "as clearance rises. The ledger's second citation,",
        "Pirmohamed 2019, does not exist.")
}
