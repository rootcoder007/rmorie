% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{phmmsr_gumbel_pvalue}
\alias{phmmsr_gumbel_pvalue}
\title{phmmsr_gumbel_pvalue}
\usage{
phmmsr_gumbel_pvalue(score, mu, lam)
}
\arguments{
\item{score}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{mu}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{lam}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the phmmsr_native implementation. Called by \code{phmmsr_search_pipeline}. See the file header for the source the module follows.
}
\value{
One of two values, depending on the branch taken.
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
