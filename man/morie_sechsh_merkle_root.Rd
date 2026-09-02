% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_sechsh_merkle_root}
\alias{morie_sechsh_merkle_root}
\title{RFC 6962 Merkle tree root}
\usage{
morie_sechsh_merkle_root(leaves) 
}
\arguments{
\item{leaves}{List of raw-byte leaves.}
}
\description{
Leaves are prefixed 0x00 and interior nodes 0x01, so a node can never be presented as a leaf; the split is at the largest power of two strictly less than the length.
}
\value{
Raw 32-byte root.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_sechsh_merkle_root(V)
}
