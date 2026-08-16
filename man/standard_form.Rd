% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{standard_form}
\alias{standard_form}
\title{Internal: convert an inequality-form program to Ax = b, x >= 0}
\usage{
standard_form(c, A_ub = NULL, b_ub = NULL, A_eq = NULL, b_eq = NULL, upper = NULL)
}
\arguments{
\item{c}{See Usage.}
\item{A_ub}{Optional; may be \code{NULL}. Passed to \code{.clpopt_mat}.}
\item{b_ub}{Defaults to \code{NULL}.}
\item{A_eq}{Optional; may be \code{NULL}. Passed to \code{.clpopt_mat}.}
\item{b_eq}{Defaults to \code{NULL}.}
\item{upper}{Optional; may be \code{NULL}. A vector; its length is taken and its elements indexed.}
}
\description{
Mirrors clpopt.standard_form.
}
\value{
A list with \code{A}, \code{b}, \code{c}, \code{n_original}, \code{n_slack}, \code{row_kinds}.
}
\keyword{internal}
