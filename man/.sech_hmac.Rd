% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.sech_hmac}
\alias{.sech_hmac}
\title{.sech_hmac}
\usage{
.sech_hmac(key, msg)
}
\arguments{
\item{key}{A vector; its length is taken.}
\item{msg}{See Usage.}
}
\description{
A step of the sechsh_native implementation. Called by \code{morie_sechsh_chain_entry}. See the file header for the source the module follows.
}
\value{
The value of \code{.sech_sha256}.
}
\keyword{internal}
