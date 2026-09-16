# SPDX-License-Identifier: AGPL-3.0-or-later
#' Temperature-scaled knowledge-distillation loss
#'
#' p_i = exp(z_i/T)/sum_j exp(z_j/T) (eq. 1); the soft cross entropy
#' CE(p, q) is multiplied by T^2 and blended with the hard-label cross
#' entropy taken at T = 1.
#'
#' @param teacher Teacher logits, length k.
#' @param student Student logits, length k.
#' @param temperature T > 0.
#' @param label One-based index of the correct class, or NULL.
#' @param alpha Weight on the soft objective, in \[0, 1\].
#'
#' @return List with softce, kl, hardce, total, teacherprob, studentprob,
#'   temperature, k.
#' @references Hinton, Vinyals and Dean (2015), arXiv:1503.02531, Sect. 2
#'   and Equation (1).  Read from the ar5iv rendering; the same paper is
#'   in the local corpus.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Distilkl(V, V)
Distilkl <- function(teacher, student, temperature = 2, label = NULL,
                     alpha = 0.5) {
  t <- .t1_vec(teacher)
  s <- .t1_vec(student)
  T <- as.numeric(temperature)
  if (length(t) != length(s)) {
    stop("teacher and student logits must have equal length")
  }
  if (T <= 0) stop("temperature must be strictly positive")
  a <- as.numeric(alpha)
  if (a < 0 || a > 1) stop("alpha must lie in [0, 1]")
  k <- length(t)
  sm <- function(z, TT) {
    e <- exp((z - max(z)) / TT)
    e / sum(e)
  }
  p <- sm(t, T)
  q <- sm(s, T)
  ce <- -sum(p * log(q))
  kl <- sum(ifelse(p > 0, p * log(p / q), 0))
  hard <- NA_real_
  total <- T * T * ce
  if (!is.null(label)) {
    j <- as.integer(label)
    if (j < 1L || j > k) stop("label out of range")
    hard <- -log(sm(s, 1)[j])
    total <- a * T * T * ce + (1 - a) * hard
  }
  .t1_result(
    softce = ce, kl = kl, hardce = hard, total = total,
    teacherprob = p, studentprob = q, temperature = T, k = k,
    method = "Temperature-scaled distillation loss (Hinton et al. 2015 Sect. 2)"
  )
}
