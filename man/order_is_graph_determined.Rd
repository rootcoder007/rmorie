% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{order_is_graph_determined}
\alias{order_is_graph_determined}
\title{order_is_graph_determined}
\usage{
order_is_graph_determined(features, adj, perm, k_keep, tol = 1e-09)
}
\arguments{
\item{features}{A matrix; the body checks with \code{is.matrix}.}
\item{adj}{See Usage.}
\item{perm}{A vector; indexed elementwise.}
\item{k_keep}{See Usage.}
\item{tol}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1e-09}.}
}
\description{
A step of the sortP_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{max_deviation}, \code{invariant}, \code{note}.
}
\keyword{internal}
