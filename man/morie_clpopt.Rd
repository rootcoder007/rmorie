% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_clpopt}
\alias{morie_clpopt}
\title{Main entry point: solve an inequality-form linear program}
\usage{
morie_clpopt(c, A_ub = NULL, b_ub = NULL, A_eq = NULL, b_eq = NULL, upper = NULL, rule = "bland", maximise = FALSE, max_iter = 10000)
}
\arguments{
\item{c}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{A_ub}{Passed to \code{standard_form}.}
\item{b_ub}{Passed to \code{standard_form}.}
\item{A_eq}{Passed to \code{standard_form}.}
\item{b_eq}{Passed to \code{standard_form}.}
\item{upper}{Passed to \code{standard_form}.}
\item{rule}{Passed to \code{simplex}. Defaults to \code{"bland"}.}
\item{maximise}{A flag; the body branches on it. Defaults to \code{FALSE}.}
\item{max_iter}{Passed to \code{simplex}. Defaults to \code{10000}.}
}
\description{
Mirrors clpopt.linprog.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
