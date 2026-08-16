% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_ihh_side}
\alias{.morie_ihh_side}
\title{Voight et al. 2006, Materials and Methods "Calculation of iHS":}
\usage{
.morie_ihh_side(pos, ehh, core, side, min_ehh)
}
\arguments{
\item{pos}{A vector; its length is taken and its elements indexed.}
\item{ehh}{A vector; indexed elementwise.}
\item{core}{Numeric; combined arithmetically in the body.}
\item{side}{Passed to \code{>}.}
\item{min_ehh}{Passed to \code{<}.}
}
\description{
trapezoid area outward from the core, INCLUDING the segment that ends at the first marker below min_ehh.
}
\value{
A list with \code{area}, \code{truncated}.
}
\keyword{internal}
