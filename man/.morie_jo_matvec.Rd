% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_jo_matvec}
\alias{.morie_jo_matvec}
\title{.morie_jo_matvec}
\usage{
.morie_jo_matvec(w, v)
}
\arguments{
\item{w}{See Usage.}
\item{v}{A matrix; passed to \code{\%*\%}.}
}
\description{
A step of the ts_joseph implementation. Called by \code{.morie_jo_glu}, \code{.morie_jo_resblock}, \code{morie_itrans} and 4 others in the module. See the file header for the source the module follows.
}
\value{
A vector, from \code{as.numeric}.
}
\keyword{internal}
