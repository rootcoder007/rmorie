% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.metsem_drop}
\alias{.metsem_drop}
\title{.metsem_drop}
\usage{
.metsem_drop(g, es)
}
\arguments{
\item{g}{A list; the body reads \code{$edges}, \code{$inc}, \code{$k}, \code{$nodes}, \code{$out} from it.}
\item{es}{See Usage.}
}
\description{
A step of the metsem_native implementation. Called by \code{morie_metsem}. See the file header for the source the module follows.
}
\value{
The value of \code{g}, as built in the body.
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
.metsem_drop(g = g, es = K)
}
