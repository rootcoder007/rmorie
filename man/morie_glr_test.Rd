% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_glr_test}
\alias{morie_glr_test}
\title{morie_glr_test}
\usage{
morie_glr_test(x, p0, p1, threshold = NULL, family = "bernoulli", sd = 1)
}
\arguments{
\item{x}{Optional; may be \code{NULL}. A vector; its length is taken.}
\item{p0}{A vector; its length is taken.}
\item{p1}{A vector; its length is taken.}
\item{threshold}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{family}{Coerced to character by the body, with \code{as.character}. Defaults to \code{"bernoulli"}.}
\item{sd}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.}
}
\description{
A step of the glm_native implementation. Called by \code{glrtest}, \code{page_cusum}. See the file header for the source the module follows.
}
\value{
The value of \code{payload}, as built in the body.
}
\keyword{internal}
