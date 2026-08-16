% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_gr_log_softmax}
\alias{.morie_gr_log_softmax}
\title{Log-softmax of a vector, overflow safe}
\usage{
.morie_gr_log_softmax(z)
}
\arguments{
\item{z}{Numeric; passed to \code{max}.}
}
\description{
A step of the geron_ml_native implementation. Called by \code{morie_geron_dalle_autoregressive_token}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_gr_log_softmax(V)
}
