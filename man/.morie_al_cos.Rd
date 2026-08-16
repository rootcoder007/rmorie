% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_al_cos}
\alias{.morie_al_cos}
\title{.morie_al_cos}
\usage{
.morie_al_cos(a, b)
}
\arguments{
\item{a}{Numeric; combined arithmetically in the body.}
\item{b}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the alammar_llm_native implementation. Called by \code{morie_alammar_cosine_similarity_loss}, \code{morie_alammar_infonce_loss}, \code{morie_alammar_multiple_negatives_ranking} and 1 others in the module. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_al_cos(V, V)
}
