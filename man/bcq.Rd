% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{bcq}
\alias{bcq}
\title{bcq}
\usage{
bcq(dataset, states = NULL, actions = NULL, tau = 0.3, gamma = 0.99, lr = 0.5, iters = 2000, loss = "huber", huber_c = 1, behavior = NULL, tol = 1e-12)
}
\arguments{
\item{dataset}{See Usage.}
\item{states}{Optional; may be \code{NULL}. Coerced to list by the body, with \code{as.list}.}
\item{actions}{Optional; may be \code{NULL}. Coerced to list by the body, with \code{as.list}.}
\item{tau}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.3}.}
\item{gamma}{Numeric; combined arithmetically in the body. Defaults to \code{0.99}.}
\item{lr}{Numeric; combined arithmetically in the body. Defaults to \code{0.5}.}
\item{iters}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{2000}.}
\item{loss}{One of \code{"huber"}, \code{"squared"}. Defaults to \code{"huber"}.}
\item{huber_c}{Numeric; combined arithmetically in the body. Defaults to \code{1}.}
\item{behavior}{Optional; may be \code{NULL}. A vector; indexed elementwise.}
\item{tol}{Defaults to \code{1e-12}.}
}
\description{
A step of the bcq_native implementation. Called by \code{morie_bcq}. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{q}, \code{policy}, \code{allowed}, \code{behavior}, \code{value}, \code{n_eliminated}, \code{bellman_error}, \code{tau}, \code{gamma}, \code{n_transitions}, \code{method}.
}
\keyword{internal}
\examples{
D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
bcq(D)
}
