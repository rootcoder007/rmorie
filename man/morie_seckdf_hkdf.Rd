% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_seckdf_hkdf}
\alias{morie_seckdf_hkdf}
\title{HKDF: extract then expand, or expand alone on uniform input}
\usage{
morie_seckdf_hkdf(ikm, salt = NULL, info = raw(), length = 32L, skip_extract = FALSE)
}
\arguments{
\item{ikm}{Raw input keying material.}
\item{salt}{Optional raw salt.}
\item{info}{Optional raw context info.}
\item{length}{Output length in bytes.}
\item{skip_extract}{Logical; skip the Extract step.}
}
\description{
\code{skip_extract = TRUE} is the documented case for input that is already a uniformly random key, not a shortcut.
}
\value{
List with \code{okm}, \code{okm_hex} (==\code{estimate}),   \code{prk}, \code{prk_hex}, \code{length}, \code{blocks},   \code{salt_supplied}, \code{extract_skipped}, \code{method},   \code{note}.
}
\keyword{internal}
