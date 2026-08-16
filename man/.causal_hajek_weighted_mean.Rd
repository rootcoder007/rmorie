% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.causal_hajek_weighted_mean}
\alias{.causal_hajek_weighted_mean}
\title{Hajek estimator for a population mean}
\usage{
.causal_hajek_weighted_mean(y, weights)
}
\arguments{
\item{y}{Response values.}
\item{weights}{Survey weights \eqn{w_i = 1/\pi_i}, all positive.}
}
\description{
The ratio (Hajek) estimator \eqn{\bar{y}_H = \sum_i w_i y_i / \sum_i w_i}, which normalises the Horvitz-Thompson estimator by the ESTIMATED population size and is therefore insensitive to the overall scale of the weights.  The standard error uses Taylor (delta-method) linearisation under the with-replacement approximation. Mirrors Python \code{morie.survey.hajek_mean}; this is the SURVEY Hajek mean, a different estimator from the Hajek IPW average treatment effect in \code{\link{morie_estimate_ate}}, which is a DIFFERENCE of two such weighted means.
}
\value{
A list with \code{mean}, \code{se}, \code{ci_lower},   \code{ci_upper}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.causal_hajek_weighted_mean(V, V)
}
