% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.metsem_indeg}
\alias{.metsem_indeg}
\title{.metsem_indeg}
\usage{
.metsem_indeg(g, v)
}
\arguments{
\item{g}{A list; the body reads \code{$inc} from it.}
\item{v}{Passed to \code{\%in\%}.}
}
\description{
A step of the metsem_native implementation. Called by \code{.metsem_walk}, \code{morie_metsem}, \code{morie_metsem_unitigs}. See the file header for the source the module follows.
}
\value{
One of two values, depending on the branch taken.
}
\references{
  Nurk, S., Meleshko, D., Korobeynikov, A. and Pevzner, P.A. (2017)
    "metaSPAdes: a new versatile metagenomic assembler." Genome
    Research 27(5), 824-834. doi:10.1101/gr.213959.116.
  Pevzner, P.A., Tang, H. and Waterman, M.S. (2001) "An Eulerian path
    approach to DNA fragment assembly." PNAS 98(17), 9748-9753.
  Bankevich, A. et al. (2012) "SPAdes: a new genome assembly
    algorithm and its applications to single-cell sequencing."
    Journal of Computational Biology 19(5), 455-477.
    doi:10.1089/cmb.2012.0021.
}
\keyword{internal}
\examples{
ABUNDANT <- "ACGTACGGTCAGTTGCAAGT"
RARE <- "TTGCCATGAACCGGTTAACA"
READS <- c(rep(ABUNDANT, 4L), RARE)
K <- 7L
g <- morie_metsem_graph(READS, K)
morie_metsem_unitigs(g)
.metsem_indeg(g = g, v = K)
}
