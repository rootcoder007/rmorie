% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_msm_weighted_glm}
\alias{morie_msm_weighted_glm}
\title{morie_msm_weighted_glm}
\usage{
morie_msm_weighted_glm(y, X, weights = NULL, family = "gaussian", offset = NULL, n_iter = 60, 
    tol = 1e-10) 
}
\description{
morie_msm_weighted_glm
}
\arguments{
\item{y}{Argument \code{y}; see Usage.}
\item{X}{Argument \code{X}; see Usage.}
\item{weights}{Argument \code{weights}; see Usage.}
\item{family}{Argument \code{family}; see Usage.}
\item{offset}{Argument \code{offset}; see Usage.}
\item{n_iter}{Argument \code{n_iter}; see Usage.}
\item{tol}{Argument \code{tol}; see Usage.}
}
\value{
A list with \code{beta}, \code{fitted}, \code{eta}, \code{weights}, \code{family}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_msm_weighted_glm(V, V)
}
