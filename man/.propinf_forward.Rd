% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.propinf_forward}
\alias{.propinf_forward}
\title{.propinf_forward}
\usage{
.propinf_forward(net, x)
}
\arguments{
\item{net}{A vector; its length is taken and its elements indexed.}
\item{x}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the propinf_native implementation. Called by \code{morie_propinf_fcnn_predict}, \code{morie_propinf_train_fcnn}. See the file header for the source the module follows.
}
\value{
A list with \code{acts}, \code{pre}.
}
\keyword{internal}
