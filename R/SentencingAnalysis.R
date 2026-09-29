#' Mandatory minimum sentence analysis
#'
#' R arm of \code{morie.fn.sntmn}: shares of cases sentenced at, below and
#' above the applicable mandatory minimum and the mean excess over the minimum,
#' overall and by offense.
#'
#' @param df Data frame or list with the three columns.
#' @param offense_col,sentence_col,min_col Column names.
#' @return A named list (the Python result's fields).
#' @references United States Sentencing Commission (2017). Mandatory Minimum Penalties in the Federal Criminal Justice System. Washington, DC.
#' @examples
#' Sntmn(list(offense = c("a", "a", "b"), sentence_days = c(60, 90, 30),
#'   mandatory_min_days = c(60, 60, 60)))$value
#' @export
Sntmn <- function(df, offense_col = "offense", sentence_col = "sentence_days", min_col = "mandatory_min_days") {
  d <- df[[sentence_col]] - df[[min_col]]
  off <- df[[offense_col]]
  summ <- function(v) {
    ab <- v[v > 0]
    list(pct_at_minimum = mean(v == 0), pct_below_minimum = mean(v < 0), pct_above_minimum = mean(v > 0),
         mean_above_minimum = if (length(ab)) mean(ab) else 0, n = length(v))
  }
  out <- summ(d)
  labs <- unique(off)
  out$by_offense <- stats::setNames(lapply(labs, function(o) summ(d[off == o])), labs)
  c(list(value = out$pct_at_minimum), out)
}
