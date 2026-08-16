% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.alfbnp_fit_linear}
\alias{.alfbnp_fit_linear}
\title{Denoising score matching, solved exactly. At each noise level the}
\usage{
.alfbnp_fit_linear(clean, sigmas, draws, ridge)
}
\arguments{
\item{clean}{A vector; indexed elementwise.}
\item{sigmas}{A vector; its length is taken and its elements indexed.}
\item{draws}{A vector; its length is taken and its elements indexed.}
\item{ridge}{Numeric; combined arithmetically in the body.}
}
\description{
optimal linear map is a ridge regression with a closed form; descending to it would only add a tolerance for the two arms to disagree about.
}
\value{
The value of \code{coefs}, as built in the body.
}
\keyword{internal}
