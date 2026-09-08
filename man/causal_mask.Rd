% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{causal_mask}
\alias{causal_mask}
\title{causal_mask}
\usage{
causal_mask(n) 
}
\arguments{
\item{n}{Coerced to integer by the body, with \code{as.integer}.}
}
\description{
A step of the sasRec_native implementation. Called by \code{self_attention}. See the file header for the source the module follows.
}
\value{
The value of \code{mask}, as built in the body.
}
\keyword{internal}
\examples{
causal_mask(n = 5L)
}
