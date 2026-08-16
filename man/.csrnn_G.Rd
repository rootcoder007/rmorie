% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.csrnn_G}
\alias{.csrnn_G}
\title{Internal: G-hat(y) = #(y_i <= y) / n, evaluated on `grid`}
\usage{
.csrnn_G(nn, grid)
}
\arguments{
\item{nn}{A vector; its length is taken.}
\item{grid}{See Usage.}
}
\description{
A step of the csrnn implementation. Called by \code{morie_csr_nn_test}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
