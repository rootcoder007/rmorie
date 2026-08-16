% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_alammar_setfit_twostep}
\alias{morie_alammar_setfit_twostep}
\title{morie_alammar_setfit_twostep}
\usage{
morie_alammar_setfit_twostep(embeddings, labels)
}
\description{
morie_alammar_setfit_twostep
}
\value{
A list with \code{positive_pairs}, \code{negative_pairs}, \code{n_positive}, \code{n_negative}, \code{head_train_accuracy}, \code{head_predictions}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
morie_alammar_setfit_twostep(rbind(c(0, 0), c(0.1, 0), c(5, 5), 
    c(5.1, 5)), c(0, 0, 1, 1))
}
