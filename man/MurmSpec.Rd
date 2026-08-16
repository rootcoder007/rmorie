% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{MurmSpec}
\alias{MurmSpec}
\title{MurmSpec: heart murmur frequency analysis; Rangayyan (2024) eq.}
\usage{
MurmSpec(pcg, fs, f1 = 25, f2 = 75, f3 = 150)
}
\arguments{
\item{pcg}{Passed to \code{.bsapsd}.}
\item{fs}{Numeric; combined arithmetically in the body.}
\item{f1}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{25}.}
\item{f2}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{75}.}
\item{f3}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{150}.}
}
\description{
(6.45) Part of the rangayyan_phys implementation; see the file header for the source it follows.
}
\value{
A vector, from \code{c}.
}
\keyword{internal}
