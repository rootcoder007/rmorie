% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.metsem_index}
\alias{.metsem_index}
\title{.metsem_index}
\usage{
.metsem_index(edges, k)
}
\arguments{
\item{edges}{See Usage.}
\item{k}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the metsem_native implementation. Called by \code{.metsem_drop}, \code{morie_metsem_graph}. See the file header for the source the module follows.
}
\value{
A list with \code{out}, \code{inc}, \code{nodes}.
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
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
.metsem_index(D, V)
}
