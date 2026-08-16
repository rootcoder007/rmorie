% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_sechsh_verify_inclusion}
\alias{morie_sechsh_verify_inclusion}
\title{Verify a Merkle audit path}
\usage{
morie_sechsh_verify_inclusion(leaf, index, size, path, root)
}
\arguments{
\item{leaf}{Raw leaf.}
\item{index}{Zero-based leaf index.}
\item{size}{Total leaf count.}
\item{path}{List of sibling hashes (top-down).}
\item{root}{Raw root.}
}
\description{
Reconstructs the head from the leaf and the path alone. The path is recorded top-down by \code{inclusion_proof} but must be folded bottom-up; consuming it in the written order combines the wrong sibling at the wrong level.
}
\value{
List with \code{root}, \code{root_hex}, \code{valid},   \code{path_used}.
}
\keyword{internal}
