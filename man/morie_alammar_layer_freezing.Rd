% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_alammar_layer_freezing}
\alias{morie_alammar_layer_freezing}
\title{morie_alammar_layer_freezing}
\usage{
morie_alammar_layer_freezing(n_layers, n_stages = NULL) 
}
\arguments{
\item{n_layers,n_stages}{Schedule sizes.}
}
\description{
morie_alammar_layer_freezing
}
\value{
A list with \code{masks}, \code{n_stages}, \code{trainable_per_stage}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
morie_alammar_layer_freezing(n_layers = 5L)
}
