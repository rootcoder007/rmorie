% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{elmo_representation}
\alias{elmo_representation}
\title{elmo_representation}
\usage{
elmo_representation(X, layers, raw_weights = NULL, gamma = 1)
}
\arguments{
\item{X}{See Usage.}
\item{layers}{See Usage.}
\item{raw_weights}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{gamma}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.}
}
\description{
A step of the elmo_native implementation. Called by \code{elmo}, \code{elmorepresentation}, \code{morie_elmo}. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{elmo}, \code{layers}, \code{weights}, \code{gamma}, \code{n_layers}, \code{L}, \code{d}, \code{top_layer}, \code{method}.
}
\keyword{internal}
