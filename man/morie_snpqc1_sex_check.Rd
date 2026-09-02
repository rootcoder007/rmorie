% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_snpqc1_sex_check}
\alias{morie_snpqc1_sex_check}
\title{morie_snpqc1_sex_check}
\usage{
morie_snpqc1_sex_check(x_genotypes, reported_sex = NULL, male_min = 0.8, female_max = 0.2) 
}
\arguments{
\item{x_genotypes}{Passed to \code{.snpqc1_check}.}
\item{reported_sex}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
\item{male_min}{Passed to \code{>}. Defaults to \code{0.8}.}
\item{female_max}{Passed to \code{<}. Defaults to \code{0.2}.}
}
\description{
A step of the snpqc1_native implementation. Called by \code{morie_snpqc1}. See the file header for the source the module follows.
}
\value{
The value of \code{res}, as built in the body.
}
\keyword{internal}
\examples{
S <- c("a", "b", "c")
morie_snpqc1_sex_check(S)
}
