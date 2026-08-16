% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Svm}
\alias{Svm}
\title{Section 10.4.5.  Only the patterns with a_i > 0 -- the SUPPORT}
\usage{
Svm(X, y, C = 1, maxiter = 2000, tol = 1e-06)
}
\arguments{
\item{X}{A matrix; passed to \code{as.matrix}.}
\item{y}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{C}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.}
\item{maxiter}{Passed to \code{.morie_rg_smo}. Defaults to \code{2000}.}
\item{tol}{Passed to \code{.morie_rg_smo}. Defaults to \code{1e-06}.}
}
\description{
VECTORS -- enter the solution, so the boundary is set by the samples nearest it and is untouched by the bulk of the data.  That is the strength on small samples and the weakness against one mislabelled point near the boundary, which C controls: small C tolerates violations, large C insists on separating and contorts around an outlier.
}
\value{
A list with \code{w}, \code{b}, \code{alpha}, \code{support_vectors}, \code{n_support}, \code{margin}, \code{C}, \code{iterations}, \code{converged}, \code{training_accuracy}, \code{boundary_set_by_the_support_vectors_only}, \code{large_c_contorts_around_outliers}, \code{method}.
}
\keyword{internal}
