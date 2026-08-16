% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.alfmpv_order}
\alias{.alfmpv_order}
\title{Lower sorts first. Ties break on the hit\\'s position in the}
\usage{
.alfmpv_order(tab, mode, idx, pos)
}
\arguments{
\item{tab}{A list; the body reads \code{$evalue}, \code{$identity} from it.}
\item{mode}{One of \code{"colabfold"}, \code{"multimer"}.}
\item{idx}{See Usage.}
\item{pos}{Numeric; passed to \code{order}.}
}
\description{
alignment, so the order is total and neither arm can wander off on a tie.
}
\value{
The value of \code{order}.
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
.alfmpv_order(tab = c(1, 2, 3, 4, 5, 6, 7, 8), mode = 5L, idx = c(1, 2, 3, 4, 5, 6, 7, 8), pos = c(1, 2, 3, 4, 5, 6, 7, 8))
}
