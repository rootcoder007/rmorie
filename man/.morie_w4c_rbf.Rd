% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_w4c_rbf}
\alias{.morie_w4c_rbf}
\title{.morie_w4c_rbf}
\usage{
.morie_w4c_rbf(A, B, gamma)
}
\arguments{
\item{A}{A matrix; passed to \code{\%*\%}.}
\item{B}{A matrix; passed to \code{t}.}
\item{gamma}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the geron_w4c_native implementation. Called by \code{morie_geron_one_class_svm}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
