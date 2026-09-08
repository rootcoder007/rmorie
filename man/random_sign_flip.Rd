% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{random_sign_flip}
\alias{random_sign_flip}
\title{Random sign flip on the positional encoding}
\usage{
random_sign_flip(pe, rng) 
}
\arguments{
\item{pe}{Positional encoding matrix (n x d).}
\item{rng}{Generator environment.}
}
\description{
Random sign flip on the positional encoding
}
\value{
Sign-flipped encoding.
}
\keyword{internal}
\examples{
random_sign_flip(pe = c(1, 2, 3, 4, 5, 6, 7, 8), rng = list(a = 1, b = 2))
}
