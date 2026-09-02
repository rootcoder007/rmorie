% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_alfmpv}
\alias{morie_alfmpv}
\title{AlphaFold-Multimer chain pairing}
\usage{
morie_alfmpv(chains = NULL, msas = NULL, mode = "multimer", ...) 
}
\arguments{
\item{chains}{alias for msas, for the older call shape.}
\item{msas}{one entry per chain; see morie_alfmpv_msa_pairing.}
\item{mode}{multimer, colabfold or folddock.}
\item{...}{passed to morie_alfmpv_msa_pairing.}
}
\description{
AlphaFold-Multimer chain pairing
}
\value{
see morie_alfmpv_msa_pairing.
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
