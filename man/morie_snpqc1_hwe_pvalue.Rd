% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_snpqc1_hwe_pvalue}
\alias{morie_snpqc1_hwe_pvalue}
\title{morie_snpqc1_hwe_pvalue}
\usage{
morie_snpqc1_hwe_pvalue(n_hom_minor, n_het, n_hom_major, test = "exact")
}
\arguments{
\item{n_hom_minor}{Coerced to integer by the body, with \code{as.integer}.}
\item{n_het}{Coerced to integer by the body, with \code{as.integer}.}
\item{n_hom_major}{Coerced to integer by the body, with \code{as.integer}.}
\item{test}{One of \code{"chisq"}, \code{"exact"}. Defaults to \code{"exact"}.}
}
\description{
A step of the snpqc1_native implementation. Called by \code{morie_snpqc1}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
