% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_fairness_result}
\alias{.morie_fairness_result}
\title{.morie_fairness_result}
\usage{
.morie_fairness_result(title, summary_lines = list(), tables = list(), sections = list(), warnings = character(0), interpretation = "", payload = list())
}
\arguments{
\item{title}{See Usage.}
\item{summary_lines}{Defaults to \code{list()}.}
\item{tables}{Defaults to \code{list()}.}
\item{sections}{Defaults to \code{list()}.}
\item{warnings}{Defaults to \code{character(0)}.}
\item{interpretation}{Defaults to \code{""}.}
\item{payload}{Defaults to \code{list()}.}
}
\description{
A step of the fairness_metrics implementation. Called by \code{morie_fairness_average_odds_difference}, \code{morie_fairness_bias_amplification}, \code{morie_fairness_demographic_parity} and 4 others in the module. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
