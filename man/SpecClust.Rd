% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{SpecClust}
\alias{SpecClust}
\title{L_sym = I - D^-1/2 A D^-1/2.  The clustering lives in the SMALLEST}
\usage{
SpecClust(a, k = 2L) 
}
\arguments{
\item{a}{Passed to \code{.morie_spx_chkw}.}
\item{k}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{2L}.}
}
\description{
eigenvalues, so power iteration runs on 2I - L_sym and the values are mapped back; running it on L_sym and taking the top vectors gets this exactly backwards.  Ng, Jordan & Weiss (2001).  NOT in Schabenberger & Gotway.  A zero-degree node RAISES rather than being quietly assigned.
}
\value{
A list with \code{labels}, \code{sizes}, \code{eigenvalues}, \code{fiedler}, \code{degree}, \code{smallest_eigenvalues_not_largest}, \code{k}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
A <- matrix(0, 6, 6)
A[1, 2] <- A[2, 3] <- A[1, 3] <- 1
A[4, 5] <- A[5, 6] <- A[4, 6] <- 1
A[3, 4] <- 0.05
A <- A + t(A)
SpecClust(A, k = 2L)
}
