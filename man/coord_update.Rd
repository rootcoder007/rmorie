% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{coord_update}
\alias{coord_update}
\title{Eq. (4): x_i + C sum_j (x_i - x_j) phi_x(m_\{ij\})}
\usage{
coord_update(X, M, phi_x, C = NULL) 
}
\arguments{
\item{X}{A vector; its length is taken and its elements indexed.}
\item{M}{A vector; indexed elementwise.}
\item{phi_x}{Accepted by the signature and not used anywhere in the body.}
\item{C}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the egnnL_native implementation. Called by \code{egcl}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
set.seed(1)
X <- lapply(1:3, function(i) rnorm(3))
phi_e <- function(hi, hj, d2, a) c(hi + hj, d2)
phi_x <- function(m) sum(m) * 0.01
M <- lapply(seq_len(3), function(i) vector("list", 3))
for (i in 1:3) for (j in 1:3) if (i != j)
  M[[i]][[j]] <- edge_message(rnorm(2), rnorm(2), X[[i]], X[[j]], phi_e)
Xn <- coord_update(X, M, phi_x)
length(Xn)
}
