% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_error_contrasts}
\alias{.schab_error_contrasts}
\title{A matrix K of error contrasts: full row rank, K X = 0. Sec. 4.5.2}
\usage{
.schab_error_contrasts(X)
}
\arguments{
\item{X}{See Usage.}
}
\description{
builds K explicitly for the intercept-only case and notes, citing Harville (1974), that the choice of K does not matter for estimation. Taking the orthogonal complement of the column space of X gives one such K for any linear mean structure.
}
\value{
A matrix, from \code{t}.
}
\keyword{internal}
