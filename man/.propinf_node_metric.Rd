% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.propinf_node_metric}
\alias{.propinf_node_metric}
\title{.propinf_node_metric}
\usage{
.propinf_node_metric(layer, i)
}
\arguments{
\item{layer}{A list; the body reads \code{$W} from it.}
\item{i}{See Usage.}
}
\description{
A step of the propinf_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
.propinf_node_metric(D, V)
}
