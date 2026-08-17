% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{scaling_comparison}
\alias{scaling_comparison}
\title{Rank configurations by Gflops.}
\usage{
scaling_comparison(configs)
}
\arguments{
\item{configs}{list of 5-tuples (name, latent, patch, depth, width).}
}
\description{
Each entry is c(name, latent, patch, depth, width). Three axes (depth, width, token count) all move Gflops; only the first two move parameters.
}
\value{
list with ranked entries and a note.
}
\keyword{internal}
\examples{
configs <- list(list("small", 224, 16, 6, 384),
                list("base", 224, 16, 12, 768))
scaling_comparison(configs)
}
