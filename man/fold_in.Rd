% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{fold_in}
\alias{fold_in}
\title{fold_in}
\usage{
fold_in(query, model) 
}
\arguments{
\item{query}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{model}{A list; the body reads \code{$S}, \code{$T} from it.}
}
\description{
A step of the lsa_native implementation. Called by \code{morie_lsa}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
X <- matrix(c(2, 0, 1, 0, 3, 1, 1, 1, 0, 0, 2, 1), nrow = 3, byrow = TRUE)
model <- lsa_decompose(X, k_dim = 2)
q_hat <- fold_in(c(1, 0, 2), model)
length(q_hat)
}
