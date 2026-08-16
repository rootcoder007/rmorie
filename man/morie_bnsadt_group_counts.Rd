% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_bnsadt_group_counts}
\alias{morie_bnsadt_group_counts}
\title{Published studies falling in each free group's region}
\usage{
morie_bnsadt_group_counts(x, sigma, family)
}
\arguments{
\item{x}{Published point estimates.}
\item{sigma}{Their standard errors.}
\item{family}{A name of the selection family.}
}
\description{
A group with NO observations is not identified from below: nothing in the data distinguishes "such studies are published one time in ten" from "one time in a billion", because none were seen either way. The likelihood is then monotone in that beta all the way to zero, and a maximiser sent after it walks off into the denormals -- which is exactly what the two arms did differently before this existed.
}
\value{
An integer count per free group.
}
\keyword{internal}
