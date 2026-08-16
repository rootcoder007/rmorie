% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.alfmpv_chain_table}
\alias{.alfmpv_chain_table}
\title{.alfmpv_chain_table}
\usage{
.alfmpv_chain_table(chain, idx)
}
\arguments{
\item{chain}{A list; the body reads \code{$species} from it.}
\item{idx}{See Usage.}
}
\description{
A step of the alfmpv_native implementation. Called by \code{morie_alfmpv_msa_pairing}. See the file header for the source the module follows.
}
\value{
A list with \code{species}, \code{evalue}, \code{identity}, \code{gaps}, \code{coverage}, \code{n}.
}
\references{
  Evans, R. et al. (2022) "Protein complex prediction with
    AlphaFold-Multimer." bioRxiv 2021.10.04.463034v2, section 2.1,
    doi:10.1101/2021.10.04.463034
  Mirdita, M. et al. (2022) "ColabFold: making protein folding
    accessible to all." Nature Methods 19(6), 679-682,
    doi:10.1038/s41592-022-01488-1
  Bryant, P., Pozzati, G. & Elofsson, A. (2022) "Improved prediction of
    protein-protein interactions using AlphaFold2." Nature
    Communications 13, 1265, doi:10.1038/s41467-022-28865-w
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.alfmpv_chain_table(V, V)
}
