% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_seckdf_derive_context_keys}
\alias{morie_seckdf_derive_context_keys}
\title{Derive one key per context from a single PRK}
\usage{
morie_seckdf_derive_context_keys(ikm, contexts, salt = NULL, length = 32L)
}
\arguments{
\item{ikm}{Raw input keying material.}
\item{contexts}{Character vector of context labels.}
\item{salt}{Optional raw salt.}
\item{length}{Output length per key.}
}
\description{
Deriving an encryption key and a MAC key from the same secret is safe precisely because the contexts differ.
}
\value{
List with \code{keys}, \code{hex}, \code{prk},   \code{all_distinct}, \code{note}.
}
\keyword{internal}
