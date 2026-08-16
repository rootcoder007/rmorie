% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.psy_spearman_brown}
\alias{.psy_spearman_brown}
\title{Spearman-Brown: reliability of a mean of k measurements. Always at}
\usage{
.psy_spearman_brown(icc1, k)
}
\arguments{
\item{icc1}{Numeric; combined arithmetically in the body.}
\item{k}{Numeric; combined arithmetically in the body.}
}
\description{
least the single-measure value, which is why every average-measure ICC exceeds its single-measure counterpart.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
.psy_spearman_brown(icc1 = 5L, k = 5L)
}
