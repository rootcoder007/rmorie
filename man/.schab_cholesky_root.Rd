% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_cholesky_root}
\alias{.schab_cholesky_root}
\title{Lower-triangular L with L L\\' = Sigma. The book writes the root as an}
\usage{
.schab_cholesky_root(cov, jitter = 1e-10)
}
\arguments{
\item{cov}{A matrix; passed to \code{nrow}.}
\item{jitter}{Numeric; combined arithmetically in the body. Defaults to \code{1e-10}.}
}
\description{
upper triangular U with Sigma = U\\'U; L is that U\\'.
}
\value{
A matrix, from \code{t}.
}
\keyword{internal}
\examples{
.schab_cholesky_root(cov = matrix(c(4, 1, 0, 1, 3, 1, 0, 1, 2), nrow = 3))
}
