% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.mor_ihh_one_side}
\alias{.mor_ihh_one_side}
\title{Trapezoid integral of an EHH curve out from the core on one side,}
\usage{
.mor_ihh_one_side(pos, ehh, core, side, min_ehh)
}
\arguments{
\item{pos}{A vector; its length is taken and its elements indexed.}
\item{ehh}{A vector; indexed elementwise.}
\item{core}{Numeric; combined arithmetically in the body.}
\item{side}{See Usage.}
\item{min_ehh}{See Usage.}
}
\description{
stopping once the curve drops below min_ehh.  `truncated` stays TRUE when the curve never dropped that far before the data ran out, which is exactly the case where the integral understates iHH.
}
\value{
A list with \code{area}, \code{truncated}.
}
\keyword{internal}
