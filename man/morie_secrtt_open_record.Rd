% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_secrtt_open_record}
\alias{morie_secrtt_open_record}
\title{Decrypt a record; fails closed}
\usage{
morie_secrtt_open_record(sealed, dek)
}
\arguments{
\item{sealed}{List with \code{ciphertext}, \code{nonce}, \code{tag},   optional \code{aad}.}
\item{dek}{Raw DEK.}
}
\description{
Decrypt a record; fails closed
}
\value{
Raw plaintext.
}
\keyword{internal}
