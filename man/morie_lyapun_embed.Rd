% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_lyapun_embed}
\alias{morie_lyapun_embed}
\title{morie_lyapun_embed}
\usage{
morie_lyapun_embed(y, m, tau)
}
\arguments{
\item{y}{A vector; its length is taken and its elements indexed.}
\item{m}{A count; the body uses it as \code{seq_len(...)}.}
\item{tau}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the lyapun_native implementation. Called by \code{divergence_curve}, \code{morie_lyapun}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
