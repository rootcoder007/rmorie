% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Basmat}
\alias{Basmat}
\title{Basis matrix of the observed curves (MVSML eq. 14.8)}
\usage{
Basmat(t, n_basis, kind = "fourier", period = NULL) 
}
\description{
Psi is the m x L2 matrix with entry (j, o) equal to psi_o(t_j): rows are the times at which the covariate curve was observed, columns the L2 basis functions.  It is what turns a discretely sampled curve into basis coefficients through (14.7).
}
\arguments{
\item{t}{Argument \code{t}; see Usage.}
\item{n_basis}{Argument \code{n_basis}; see Usage.}
\item{kind}{Argument \code{kind}; see Usage.}
\item{period}{Argument \code{period}; see Usage.}
}
\value{
A list with \code{Psi}, \code{m}, \code{L2}, \code{PsiTPsi}.
}
\keyword{internal}
\examples{
Basmat(t = c(1, 2, 3, 4, 5, 6, 7, 8), n_basis = 5L)
}
