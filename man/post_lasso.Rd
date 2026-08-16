% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{post_lasso}
\alias{post_lasso}
\title{Refit by least squares on the selected support}
\usage{
post_lasso(X, y, lam)
}
\arguments{
\item{X}{Design matrix.}
\item{y}{Outcome vector.}
\item{lam}{Lasso regularisation.}
}
\description{
Refit by least squares on the selected support
}
\value{
A list with \code{support}, \code{coef}, \code{intercept},   \code{predict}, \code{lasso_beta}, \code{selected_by},   \code{note}.
}
\keyword{internal}
\examples{
post_lasso(X = c(1, 2, 3, 4, 5, 6, 7, 8), y = c(1, 2, 3, 4, 5, 6, 7, 8), lam = 5L)
}
