% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Oddsrat}
\alias{Oddsrat}
\title{Odds ratio by Woolf\'s logit method (Woolf 1955) -- NOT from Gibbons}
\usage{
Oddsrat(table, alpha = 0.05, cc = 0)
}
\description{
Odds ratio by Woolf\'s logit method (Woolf 1955) -- NOT from Gibbons
}
\value{
A list with \code{estimate}, \code{log_or}, \code{se}, \code{lower}, \code{upper}, \code{statistic}, \code{df}, \code{p_value}.
}
\keyword{internal}
\examples{
Oddsrat(rbind(c(30, 10), c(15, 25)))
}
