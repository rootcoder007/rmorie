% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_meglt}
\alias{morie_meglt}
\title{morie_meglt}
\usage{
morie_meglt(op, ...) 
}
\arguments{
\item{op}{A vector; its length is taken.}
\item{...}{Passed through.}
}
\description{
A step of the meglt_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{switch}.
}
\keyword{internal}
\examples{
set.seed(2)
U <- matrix(rnorm(10), 5, 2)
V <- matrix(rnorm(8), 4, 2)
M <- U \%*\% t(V)
obs <- list()
for (i in 0:4) for (j in 0:3) if (runif(1) < 0.7)
  obs[[length(obs) + 1]] <- c(i, j)
r <- morie_meglt("svt", M, obs, tau = 1, iters = 100)
str(r, max.level = 1)
}
