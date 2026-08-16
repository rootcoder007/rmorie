% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_qrs_dft}
\alias{.morie_qrs_dft}
\title{Plain O(n^2) DFT.  The angle is formed as w * j with w = -2 pi k / n,}
\usage{
.morie_qrs_dft(x)
}
\arguments{
\item{x}{See Usage.}
}
\description{
NOT as -2 pi k j / n: the two differ in the last bits and the Python arm uses the former.
}
\value{
A list with \code{re}, \code{im}.
}
\keyword{internal}
