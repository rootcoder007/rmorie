% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{optimise_branch}
\alias{optimise_branch}
\title{optimise_branch}
\usage{
optimise_branch(make_tree, seqs, pi = NULL, u = 1, lo = 1e-06, hi = 10, tol = 1e-10, max_iter = 200)
}
\arguments{
\item{make_tree}{A function; the body checks with \code{is.function}.}
\item{seqs}{Passed to \code{morie_phylml}.}
\item{pi}{Passed to \code{morie_phylml}.}
\item{u}{Passed to \code{morie_phylml}. Defaults to \code{1}.}
\item{lo}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1e-06}.}
\item{hi}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{10}.}
\item{tol}{Passed to \code{<}. Defaults to \code{1e-10}.}
\item{max_iter}{Passed to \code{:}. Defaults to \code{200}.}
}
\description{
A step of the phylml_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{result}, as built in the body.
}
\keyword{internal}
