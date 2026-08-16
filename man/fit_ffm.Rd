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
\item{rows}{See Usage.}
\item{labels}{See Usage.}
\item{fields}{See Usage.}
\item{n_features}{See Usage.}
\item{n_fields}{See Usage.}
\item{k_dim}{Defaults to \code{4}.}
\item{eta}{Defaults to \code{0.1}.}
\item{lam}{Defaults to \code{2e-05}.}
\item{epochs}{Defaults to \code{10}.}
\item{seed}{Defaults to \code{0}.}
}
\description{
Part of the ffmFM_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{W}, \code{loss_history}, \code{final_loss}, \code{k}, \code{n_parameters}, \code{n_parameters_fm}, \code{method}, \code{caveat}.
}
\keyword{internal}
