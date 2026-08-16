% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.glm_score_bernoulli}
\alias{.glm_score_bernoulli}
\title{.glm_score_bernoulli}
\usage{
.glm_score_bernoulli(p0, p1)
}
\arguments{
\item{p0}{A vector; its length is taken.}
\item{p1}{A vector; its length is taken.}
}
\description{
A step of the glm_native implementation. Called by \code{morie_glr_test}. See the file header for the source the module follows.
}
\value{
A list with \code{z}, \code{kl}.
}
\keyword{internal}
