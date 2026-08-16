% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_alfmpv_msa_pairing}
\alias{morie_alfmpv_msa_pairing}
\title{MSA pairing for a protein complex}
\usage{
morie_alfmpv_msa_pairing(msas, mode = "multimer", min_coverage = 0.5, max_gap = 0.9, copies = NULL, max_pairs = NULL)
}
\arguments{
\item{msas}{one entry per chain: either a plain vector of species   identifiers, or a list with a species element and any of evalue,   identity, gaps and coverage as parallel vectors.}
\item{mode}{multimer, colabfold or folddock.}
\item{min_coverage}{colabfold only: least fraction of the query the   alignment must cover. 0.5 is the published value.}
\item{max_gap}{folddock only: most gaps a hit may be, as a fraction.   0.9 is the published value.}
\item{copies}{copy count per chain for a homo-oligomer. ColabFold   copies the alignment per component rather than searching again, so   the copies share one chain's hits.}
\item{max_pairs}{stop after this many paired rows.}
}
\description{
MSA pairing for a protein complex
}
\value{
a list with paired, species_paired, unpaired, n_paired,   n_unpaired, n_rows, n_chains, chain_source, n_filtered, mode,   pairing_rule and method.
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
