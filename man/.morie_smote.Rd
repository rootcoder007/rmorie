% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_smote}
\alias{.morie_smote}
\title{.morie_smote}
\usage{
.morie_smote(X, y_chr, k)
}
\arguments{
\item{X}{A matrix; indexed by row and column.}
\item{y_chr}{See Usage.}
\item{k}{Passed to \code{.morie_knn_index}.}
}
\description{
SMOTE (Chawla et al. 2002): synthesize minority-class points by linear interpolation towards random minority k-NN until classes balance. Returns rows to append (X_new, y_new).
}
\value{
A list with \code{X_new}, \code{y_new}.
}
\keyword{internal}
