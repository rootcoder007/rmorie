% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_glm_predict}
\alias{morie_glm_predict}
\title{morie_glm_predict}
\usage{
morie_glm_predict(fit, X, add_intercept = TRUE, type = c("response", "link"), offset = NULL)
}
\arguments{
\item{fit}{See Usage.}
\item{X}{See Usage.}
\item{add_intercept}{Defaults to \code{TRUE}.}
\item{type}{Defaults to \code{c("response", "link")}.}
\item{offset}{Defaults to \code{NULL}.}
}
\description{
Part of the glm implementation; see the file header for the source it follows.
}
\value{
The value of \code{.morie_glm_families()[[fit$family]]$linkinv}.
}
\keyword{internal}
