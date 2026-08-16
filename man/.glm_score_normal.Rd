% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.glm_score_normal}
\alias{.glm_score_normal}
\title{.glm_score_normal}
\usage{
.glm_score_normal(p0, p1, sd)
}
\arguments{
\item{p0}{Numeric; combined arithmetically in the body.}
\item{p1}{Numeric; combined arithmetically in the body.}
\item{sd}{A vector; its length is taken.}
}
\description{
A step of the glm_native implementation. Called by \code{morie_glr_test}. See the file header for the source the module follows.
}
\value{
A list with \code{z}, \code{kl}.
}
\keyword{internal}
