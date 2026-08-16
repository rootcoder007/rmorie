% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_seckdf_extract}
\alias{morie_seckdf_extract}
\title{HKDF Extract}
\usage{
morie_seckdf_extract(ikm, salt = NULL)
}
\arguments{
\item{ikm}{Raw input keying material.}
\item{salt}{Optional raw salt; default HashLen zeros.}
}
\description{
PRK = HMAC(salt, IKM). The salt is the HMAC KEY and the IKM is the MESSAGE; the order is the way round implementations get backwards.
}
\value{
List with \code{prk}, \code{salt_supplied}, \code{note}.
}
\keyword{internal}
