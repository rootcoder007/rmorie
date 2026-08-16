% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.lcwphr_logit_irls}
\alias{.lcwphr_logit_irls}
\title{Logistic regression by IRLS; the ridge is scaled to the design}
\usage{
.lcwphr_logit_irls(X, y, max_iter = 100L, tol = 1e-11, ridge_rel = 1e-08)
}
\arguments{
\item{X}{A matrix; passed to \code{nrow}.}
\item{y}{Numeric; combined arithmetically in the body.}
\item{max_iter}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{100L}.}
\item{tol}{Passed to \code{<}. Defaults to \code{1e-11}.}
\item{ridge_rel}{Numeric; combined arithmetically in the body. Defaults to \code{1e-08}.}
}
\description{
A step of the lcwphr_native implementation. Called by \code{morie_lcwphr_latent_class_weighted}. See the file header for the source the module follows.
}
\value{
A list with \code{beta}, \code{fitted}.
}
\keyword{internal}
\examples{
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
.lcwphr_logit_irls(M, M)
}
