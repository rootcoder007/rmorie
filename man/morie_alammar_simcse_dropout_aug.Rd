% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_alammar_simcse_dropout_aug}
\alias{morie_alammar_simcse_dropout_aug}
\title{morie_alammar_simcse_dropout_aug}
\usage{
morie_alammar_simcse_dropout_aug(embeddings_dropout1, embeddings_dropout2, tau = 0.05) 
}
\arguments{
\item{embeddings_dropout1,embeddings_dropout2}{Two dropout passes of   the same sentences.}

\item{tau}{Argument \code{tau}; see Usage.}
}
\description{
morie_alammar_simcse_dropout_aug
}
\value{
A list with \code{estimate}, \code{losses}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_alammar_simcse_dropout_aug(V, V)
}
