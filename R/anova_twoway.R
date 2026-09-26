# SPDX-License-Identifier: AGPL-3.0-or-later
#' Two-way ANOVA with sequential (Type I) sums of squares
#'
#' Fits y = mu + alpha_i + delta_j (Hedderich, Sachs & Reynarowych 2023, eq
#' 8.47) or, with `interaction = TRUE`, adds gamma_ij (eq 8.48), and gives
#' SS(A), SS(B | A) and SS(AB | A, B) as drops in the residual sum of squares
#' as each block of dummy columns is added: anova(lm(y ~ a + b)) or
#' anova(lm(y ~ a * b)), balanced or not.
#'
#' @param data Data frame.
#' @param y,a,b Column names of the outcome and the two factors.
#' @param interaction Include the A x B interaction.
#' @return Named list: F, p_value, df (factor A), ss_a, ss_b, ss_resid, df_b,
#'   df_resid, f_b, p_b and, with interaction, ss_ab, df_ab, f_ab, p_ab.
#' @references Hedderich, J., Sachs, L. & Reynarowych, Z. (2023). Applied
#'   Statistics: Methods Using R. Eqs (8.47)-(8.48).
#' @examples
#' d <- data.frame(y = c(3, 4, 5, 3.5, 5, 6, 2, 3, 4.5, 2.5, 3.2, 4), a = rep(1:2, each = 6),
#'                 b = rep(1:3, 4))
#' morie_anova_twoway(d, interaction = TRUE)$f_ab
#' @export
morie_anova_twoway <- function(data, y = "y", a = "a", b = "b", interaction = FALSE) {
  d <- stats::na.omit(data[, c(y, a, b)])
  yv <- as.numeric(d[[y]])
  fa <- factor(d[[a]])
  fb <- factor(d[[b]])
  n <- length(yv)
  dum <- function(f) {
    lv <- levels(f)
    if (length(lv) < 2L) return(matrix(numeric(0), n, 0))
    vapply(lv[-1L], function(l) as.numeric(f == l), numeric(n))
  }
  da <- matrix(dum(fa), n)
  db <- matrix(dum(fb), n)
  dab <- if (interaction && ncol(da) && ncol(db)) {
    do.call(cbind, lapply(seq_len(ncol(da)), function(i) da[, i] * db))
  } else {
    matrix(numeric(0), n, 0)
  }
  rss <- function(X) sum(stats::lm.fit(X, yv)$residuals^2)
  r0 <- rss(matrix(1, n, 1))
  r1 <- rss(cbind(1, da))
  r2 <- rss(cbind(1, da, db))
  r3 <- if (interaction) rss(cbind(1, da, db, dab)) else r2
  df_a <- nlevels(fa) - 1
  df_b <- nlevels(fb) - 1
  df_ab <- if (interaction) df_a * df_b else 0
  df_resid <- n - 1 - df_a - df_b - df_ab
  if (df_resid <= 0) stop("Not enough observations for two-way ANOVA", call. = FALSE)
  ms <- r3 / df_resid
  ft <- function(ss, d) if (d > 0 && ms > 0) c(ss / d / ms, stats::pf(ss / d / ms, d, df_resid, lower.tail = FALSE)) else c(0, 1)
  ta <- ft(r0 - r1, df_a)
  tb <- ft(r1 - r2, df_b)
  out <- list(F = ta[1L], p_value = ta[2L], df = df_a, ss_a = r0 - r1, ss_b = r1 - r2, ss_resid = r3,
              df_b = df_b, df_resid = df_resid, f_b = tb[1L], p_b = tb[2L])
  if (interaction) {
    tab <- ft(r2 - r3, df_ab)
    out <- c(out, list(ss_ab = r2 - r3, df_ab = df_ab, f_ab = tab[1L], p_ab = tab[2L]))
  }
  out
}
