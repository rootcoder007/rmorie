% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.csrnn_window}
\alias{.csrnn_window}
\title{Internal: normalise `window` to a d x 2 matrix of (min, max) rows}
\usage{
.csrnn_window(window, P)
}
\arguments{
\item{window}{Optional; may be \code{NULL}. A matrix; passed to \code{nrow}.}
\item{P}{A matrix; passed to \code{ncol}.}
}
\description{
A step of the csrnn implementation. Called by \code{morie_csr_nn_test}, \code{morie_ripley_csr_test}. See the file header for the source the module follows.
}
\value{
The value of \code{bounds}, as built in the body.
}
\keyword{internal}
