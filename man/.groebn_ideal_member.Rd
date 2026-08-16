% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.groebn_ideal_member}
\alias{.groebn_ideal_member}
\title{.groebn_ideal_member}
\usage{
.groebn_ideal_member(f, F, order = "lex", basis = NULL)
}
\arguments{
\item{f}{Passed to \code{.groebn_normal_form}.}
\item{F}{Passed to \code{.groebn_buchberger}.}
\item{order}{Passed to \code{.groebn_buchberger}. Defaults to \code{"lex"}.}
\item{basis}{Optional; may be \code{NULL}. Passed to \code{is.null}.}
}
\description{
Ideal membership ---------------------------------------------------------------------------
}
\value{
A list with \code{estimate}, \code{member}, \code{remainder}, \code{order}, \code{basis}, \code{method}.
}
\keyword{internal}
