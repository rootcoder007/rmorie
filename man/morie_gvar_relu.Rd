% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_gvar_relu}
\alias{morie_gvar_relu}
\title{ReLU on a reverse-mode tape node}
\usage{
morie_gvar_relu(x)
}
\arguments{
\item{x}{A tape node. @return A tape node.}
}
\description{
ReLU on a reverse-mode tape node
}
\value{
The value of \code{.morie_gvar}.
}
\keyword{internal}
\examples{
morie_gvar_relu(.morie_gvar(-1.5))$value
}
