% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{phmmsr_search_pipeline}
\alias{phmmsr_search_pipeline}
\title{phmmsr_search_pipeline}
\usage{
phmmsr_search_pipeline(sequences, profile, msv_threshold = 0.02, mu = 10, lam = 0.7, full_score = NULL)
}
\arguments{
\item{sequences}{A vector; its length is taken and its elements indexed.}
\item{profile}{Passed to \code{phmmsr_msv_score}.}
\item{msv_threshold}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.02}.}
\item{mu}{Passed to \code{phmmsr_gumbel_pvalue}. Defaults to \code{10}.}
\item{lam}{Passed to \code{phmmsr_msv_score}. Defaults to \code{0.7}.}
\item{full_score}{Optional; may be \code{NULL}. Passed to \code{is.null}.}
}
\description{
A step of the phmmsr_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{passed}, \code{msv_scores}, \code{discarded}, \code{survivor_fraction}, \code{full_scores}, \code{method}, \code{note}.
}
\references{
----------
Eddy, S. R. (2011) "Accelerated Profile HMM Searches", *PLoS
Computational Biology* 7(10), e1002195,
doi:10.1371/journal.pcbi.1002195. The MSV algorithm computing an
optimal sum of multiple ungapped local alignment segments by a striped
vector-parallel approach; MSV scores following the same statistical
distribution as gapped optimal local alignment scores, allowing rapid
evaluation of significance and use as a heuristic filter; the 20-fold
acceleration of Forward/Backward by sparse rescaling; the pipeline in
which high-scoring MSV hits are reanalysed with the full HMM; and the
benchmarks showing negligible sensitivity sacrificed, with HMMER3
100-1000 fold faster than HMMER2 and about as fast as BLAST for
protein searches.

Farrar, M. (2007) "Striped Smith-Waterman speeds database searches six
times over other SIMD implementations", *Bioinformatics* 23(2),
156-161, doi:10.1093/bioinformatics/btl582. The striped layout reused.

Durbin, R., Eddy, S. R., Krogh, A. & Mitchison, G. (1998)
*Biological Sequence Analysis*, Cambridge University Press,
doi:10.1017/CBO9780511790492. Profile HMMs.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
phmmsr_search_pipeline(V, V)
}
