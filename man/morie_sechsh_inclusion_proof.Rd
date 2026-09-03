% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_sechsh_inclusion_proof}
\alias{morie_sechsh_inclusion_proof}
\title{Merkle audit path for one leaf}
\usage{
morie_sechsh_inclusion_proof(leaves, index) 
}
\arguments{
\item{leaves}{List of raw-byte leaves.}
\item{index}{Zero-based leaf index.}
}
\description{
Returns \code{log2(n)} sibling hashes, top-down; the verifier folds them in reverse to recompute the root.
}
\value{
List with \code{path}, \code{path_hex}, \code{length},   \code{index}, \code{size}, \code{note}.
}
\keyword{internal}
\examples{
if (morie_crypto_sodium_available()) {
  morie_sechsh_inclusion_proof(leaves = c(1, 2, 3, 4, 5, 6, 7, 8), index = 5L)
}
}
