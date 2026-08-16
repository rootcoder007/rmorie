% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.alfmpv_field}
\alias{.alfmpv_field}
\title{One hit attribute as a vector of length n, defaulted when absent: a}
\usage{
.alfmpv_field(chain, name, n, default)
}
\arguments{
\item{chain}{A vector; indexed elementwise.}
\item{name}{See Usage.}
\item{n}{A count; the body uses it as \code{rep(...)}.}
\item{default}{A count; the body uses it as \code{rep(...)}.}
}
\description{
caller with no E-values should still be able to run the multimer route, which never looks at them.
}
\value{
The value of \code{v}, as built in the body.
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
