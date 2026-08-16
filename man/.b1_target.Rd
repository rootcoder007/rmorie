% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.b1_target}
\alias{.b1_target}
\title{.b1_target}
\usage{
.b1_target(Y, A, QAW, Q1W, Q0W, g1W, gbound = 0.025, iters = 100, tol = 1e-12)
}
\arguments{
\item{Y}{A vector; its length is taken.}
\item{A}{Numeric; combined arithmetically in the body.}
\item{QAW}{Passed to \code{.b1_logit}.}
\item{Q1W}{Passed to \code{.b1_logit}.}
\item{Q0W}{Passed to \code{.b1_logit}.}
\item{g1W}{Passed to \code{.b1_bound}.}
\item{gbound}{Numeric; combined arithmetically in the body. Defaults to \code{0.025}.}
\item{iters}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{100}.}
\item{tol}{Passed to \code{<}. Defaults to \code{1e-12}.}
}
\description{
A step of the b1_tmle implementation. Called by \code{Comptml}, \code{Tmleboot}, \code{Tmleor} and 3 others in the module. See the file header for the source the module follows.
}
\value{
A list with \code{epsilon}, \code{QAstar}, \code{Q1star}, \code{Q0star}, \code{g1}, \code{g0}, \code{H1}, \code{H0}.
}
\keyword{internal}
\examples{
.b1_target(Y = c(1, 2, 3, 4, 5, 6, 7, 8), A = c(1, 2, 3, 4, 5, 6, 7, 8), QAW = c(1, 2, 3, 4, 5, 6, 7, 8), Q1W = c(1, 2, 3, 4, 5, 6, 7, 8), Q0W = c(1, 2, 3, 4, 5, 6, 7, 8), g1W = c(1, 2, 3, 4, 5, 6, 7, 8))
}
