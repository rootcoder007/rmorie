% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_safrl_cmdp_returns}
\alias{morie_safrl_cmdp_returns}
\title{morie_safrl_cmdp_returns}
\usage{
morie_safrl_cmdp_returns(policy, states, actions, step, reward, costs, gamma = 0.9, start = NULL, iters = 5000, tol = 1e-14)
}
\arguments{
\item{policy}{Accepted by the signature and not used anywhere in the body.}
\item{states}{Coerced to list by the body, with \code{as.list}.}
\item{actions}{Coerced to list by the body, with \code{as.list}.}
\item{step}{Accepted by the signature and not used anywhere in the body.}
\item{reward}{Carried through into a list the body builds.}
\item{costs}{Coerced to list by the body, with \code{as.list}.}
\item{gamma}{Numeric; combined arithmetically in the body. Defaults to \code{0.9}.}
\item{start}{Optional; may be \code{NULL}. A function; the body checks with \code{is.function}.}
\item{iters}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{5000}.}
\item{tol}{Defaults to \code{1e-14}.}
}
\description{
A step of the safrl_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{J}, \code{J_C}, \code{gamma}, \code{method}.
}
\keyword{internal}
