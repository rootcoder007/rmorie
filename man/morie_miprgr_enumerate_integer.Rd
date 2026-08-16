% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_miprgr_enumerate_integer}
\alias{morie_miprgr_enumerate_integer}
\title{Brute-force enumeration over a small integer box}
\usage{
morie_miprgr_enumerate_integer(A, b, c, integer_vars, upper = 10, maximise = TRUE)
}
\arguments{
\item{A,b,c,integer_vars,maximise}{As in \code{branch_and_bound}.}
\item{upper}{Inclusive upper bound of every variable.}
}
\description{
Brute-force enumeration over a small integer box
}
\value{
A list with value, x, note.
}
\keyword{internal}
