% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.htpfourier}
\alias{.htpfourier}
\title{Phi_1..phi_L evaluated at s: the alternating basis of p.584}
\usage{
.htpfourier(s, L, P)
}
\arguments{
\item{s}{Numeric; combined arithmetically in the body.}
\item{L}{A count; the body uses it as \code{seq_len(...)}.}
\item{P}{Numeric; passed to \code{sqrt}.}
}
\description{
A step of the htpfn implementation. Called by \code{Htpfn}. See the file header for the source the module follows.
}
\value{
The value of \code{[}.
}
\keyword{internal}
