% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.dpoF_plackett_luce}
\alias{.dpoF_plackett_luce}
\title{.dpoF_plackett_luce}
\usage{
.dpoF_plackett_luce(logp, logp_ref, beta)
}
\arguments{
\item{logp}{Optional; may be \code{NULL}. A matrix; passed to \code{dim}.}
\item{logp_ref}{Optional; may be \code{NULL}. A matrix; passed to \code{dim}.}
\item{beta}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the dpoF_native implementation. Called by \code{morie_dpoF}. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{loss}, \code{losses}, \code{rewards}, \code{beta}, \code{n}, \code{model}, \code{method}.
}
\keyword{internal}
