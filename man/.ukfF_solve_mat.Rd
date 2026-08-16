% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.ukfF_solve_mat}
\alias{.ukfF_solve_mat}
\title{Solve A X = B for X (small systems, partial pivoting, Gauss-Jordan)}
\usage{
.ukfF_solve_mat(a, b_cols)
}
\arguments{
\item{a}{A matrix; passed to \code{nrow}.}
\item{b_cols}{A matrix; passed to \code{ncol}.}
}
\description{
A step of the ukfF_mixedcase_native implementation. Called by \code{morie_ukfF}. See the file header for the source the module follows.
}
\value{
The value of \code{[}.
}
\keyword{internal}
