% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{query_tokens}
\alias{query_tokens}
\title{query_tokens}
\usage{
query_tokens(n_queries, dim, seed = 0, scale = 0.02) 
}
\arguments{
\item{n_queries}{Coerced to integer by the body, with \code{as.integer}.}
\item{dim}{Coerced to integer by the body, with \code{as.integer}.}
\item{seed}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{0}.}
\item{scale}{Numeric; combined arithmetically in the body. Defaults to \code{0.02}.}
}
\description{
A step of the blip2v_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A matrix, from \code{matrix}.
}
\keyword{internal}
\examples{
query_tokens(n_queries = 5L, dim = 5L)
}
