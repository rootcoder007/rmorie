% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_burkov_layer2_output}
\alias{morie_burkov_layer2_output}
\title{Second-layer scalar output (Burkov Eq 1.7)}
\usage{
morie_burkov_layer2_output(W_2, y_1, b_2_1, phi = "identity")
}
\arguments{
\item{W_2}{Weight row.}
\item{y_1}{Layer-1 output.}
\item{b_2_1}{Bias.}
\item{phi}{Activation.}
}
\description{
Second-layer scalar output (Burkov Eq 1.7)
}
\value{
A list with \code{estimate}, \code{preactivation}, \code{n}, \code{method}.
}
\keyword{internal}
