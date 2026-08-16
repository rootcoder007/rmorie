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
\item{c}{See Usage.}
\item{A_ub}{Defaults to \code{NULL}.}
\item{b_ub}{Defaults to \code{NULL}.}
\item{A_eq}{Defaults to \code{NULL}.}
\item{b_eq}{Defaults to \code{NULL}.}
\item{upper}{Defaults to \code{NULL}.}
\item{rule}{Defaults to \code{"bland"}.}
\item{maximise}{Defaults to \code{FALSE}.}
\item{max_iter}{Defaults to \code{10000}.}
}
\description{
Mirrors clpopt.linprog.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
