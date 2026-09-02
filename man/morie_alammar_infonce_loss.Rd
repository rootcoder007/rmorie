% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_alammar_infonce_loss}
\alias{morie_alammar_infonce_loss}
\title{morie_alammar_infonce_loss}
\usage{
morie_alammar_infonce_loss(anchor, positive, negatives, tau = 0.07) 
}
\arguments{
\item{negatives}{Matrix of negative embeddings.}
\item{tau}{Temperature.}

\item{anchor}{Argument \code{anchor}; see Usage.}

\item{positive}{Argument \code{positive}; see Usage.}
}
\description{
morie_alammar_infonce_loss
}
\value{
A list with \code{estimate}, \code{positive_similarity}, \code{negative_similarities}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
morie_alammar_infonce_loss(anchor = c(1, 2, 3, 4, 5, 6, 7, 8), positive = c(1, 2, 3, 4, 5, 6, 7, 8), negatives = c(1, 2, 3, 4, 5, 6, 7, 8))
}
