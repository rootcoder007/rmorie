% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.rfmdi_imp}
\alias{.rfmdi_imp}
\title{Mean decrease in impurity: Imp(X_j) = (1/B) sum_b sum_{t: split on}
\usage{
.rfmdi_imp(trees, p)
}
\arguments{
\item{trees}{See Usage.}
\item{p}{See Usage.}
}
\description{
X_j} (n_t/n) [ i(t) - (n_L/n_t) i(t_L) - (n_R/n_t) i(t_R) ], i the within-node sum of squares.  The stored drop is already in raw sum-of-squares units.
}
\value{
A numeric value.
}
\keyword{internal}
