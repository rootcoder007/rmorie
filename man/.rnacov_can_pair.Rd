% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.rnacov_can_pair}
\alias{.rnacov_can_pair}
\title{.rnacov_can_pair}
\usage{
.rnacov_can_pair(a, b)
}
\arguments{
\item{a}{Passed to \code{==}.}
\item{b}{Passed to \code{==}.}
}
\description{
A step of the rnacov_native implementation. Called by \code{morie_rnacov_nussinov}. See the file header for the source the module follows.
}
\value{
A logical value.
}
\references{
  Eddy, S.R. and Durbin, R. (1994) "RNA sequence analysis using
    covariance models." Nucleic Acids Research 22(11), 2079-2088.
    doi:10.1093/nar/22.11.2079.
  Nawrocki, E.P. and Eddy, S.R. (2013) "Infernal 1.1: 100-fold faster
    RNA homology searches." Bioinformatics 29(22), 2933-2935.
  Rivas, E., Clements, J. and Eddy, S.R. (2017) "A statistical test
    for conserved RNA structure shows lack of evidence for structure
    in lncRNAs." Nature Methods 14(1), 45-48.
  Nussinov, R. and Jacobson, A.B. (1980) "Fast algorithm for
    predicting the secondary structure of single-stranded RNA." PNAS
    77(11), 6309-6313.
  Miller, G.A. (1955) "Note on the bias of information estimates." In
    Information Theory in Psychology, 95-100.
}
\keyword{internal}
