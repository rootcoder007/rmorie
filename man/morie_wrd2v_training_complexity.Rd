% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_wrd2v_training_complexity}
\alias{morie_wrd2v_training_complexity}
\title{morie_wrd2v_training_complexity}
\usage{
morie_wrd2v_training_complexity(architecture, D, V, N = NULL, C = NULL, hierarchical = TRUE) 
}
\arguments{
\item{architecture}{Compared against \code{"cbow"}.}
\item{D}{Numeric; combined arithmetically in the body.}
\item{V}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{N}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{C}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{hierarchical}{A flag; the body branches on it. Defaults to \code{TRUE}.}
}
\description{
A step of the wrd2v_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
morie_wrd2v_training_complexity(architecture = "skip-gram", D = 100L,
                                V = 10000L, C = 5L)
}
