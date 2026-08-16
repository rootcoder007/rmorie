% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.groebn_normal_form}
\alias{.groebn_normal_form}
\title{.groebn_normal_form}
\usage{
.groebn_normal_form(f, G, order = "lex")
}
\arguments{
\item{f}{Passed to \code{.groebn_divide}.}
\item{G}{Passed to \code{.groebn_divide}.}
\item{order}{Passed to \code{.groebn_divide}. Defaults to \code{"lex"}.}
}
\description{
A step of the groebn_native implementation. Called by \code{.groebn_buchberger}, \code{.groebn_ideal_member}, \code{.groebn_reduce_basis}. See the file header for the source the module follows.
}
\value{
The value of \code{$}.
}
\keyword{internal}
