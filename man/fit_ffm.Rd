% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{fit_ffm}
\alias{fit_ffm}
\title{fit_ffm}
\usage{
fit_ffm(rows, labels, fields, n_features, n_fields, k_dim = 4, eta = 0.1, lam = 2e-05, epochs = 10, seed = 0)
}
\arguments{
\item{rows}{A vector; its length is taken and its elements indexed.}
\item{labels}{A vector; its length is taken and its elements indexed.}
\item{fields}{A vector; indexed elementwise.}
\item{n_features}{See Usage.}
\item{n_fields}{See Usage.}
\item{k_dim}{Defaults to \code{4}.}
\item{eta}{Numeric; combined arithmetically in the body. Defaults to \code{0.1}.}
\item{lam}{Numeric; combined arithmetically in the body. Defaults to \code{2e-05}.}
\item{epochs}{Defaults to \code{10}.}
\item{seed}{Defaults to \code{0}.}
}
\description{
A step of the ffmFM_native implementation. Called by \code{field_aware_fm}, \code{fieldawarefm}, \code{morie_ffmFM}. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{W}, \code{loss_history}, \code{final_loss}, \code{k}, \code{n_parameters}, \code{n_parameters_fm}, \code{method}, \code{caveat}.
}
\keyword{internal}
