% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.alfmpv_keep}
\alias{.alfmpv_keep}
\title{Which hits survive the route\\'s own filter, in order}
\usage{
.alfmpv_keep(tab, mode, min_coverage, max_gap)
}
\arguments{
\item{tab}{A list; the body reads \code{$coverage}, \code{$gaps}, \code{$n} from it.}
\item{mode}{One of \code{"colabfold"}, \code{"folddock"}.}
\item{min_coverage}{Passed to \code{>=}.}
\item{max_gap}{Passed to \code{<=}.}
}
\description{
A step of the alfmpv_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{which}.
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
