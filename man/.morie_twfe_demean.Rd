% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_twfe_demean}
\alias{.morie_twfe_demean}
\title{Internal helper: alternating-projection two-way demeaning}
\usage{
.morie_twfe_demean(M, f1, f2, tol = 1e-11, max_iter = 500L)
}
\description{
Removes unit and time group means from every column of `M` by alternating projections (Guimaraes-Portugal). Exact in one pass for balanced panels; iterates to `tol` otherwise.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
