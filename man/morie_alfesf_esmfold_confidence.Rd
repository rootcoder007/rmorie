% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_alfesf_esmfold_confidence}
\alias{morie_alfesf_esmfold_confidence}
\title{morie_alfesf_esmfold_confidence}
\usage{
morie_alfesf_esmfold_confidence(lddt_logits = NULL, pae_logits = NULL, features = NULL, weights = NULL, lddt = NULL, chain_id = NULL, temperature = 1, l2 = 0.001, iters = 300L, lr = 0.5, pae_bin_width = 0.5)
}
\arguments{
\item{lddt_logits}{Defaults to \code{NULL}.}
\item{pae_logits}{Defaults to \code{NULL}.}
\item{features}{Defaults to \code{NULL}.}
\item{weights}{Defaults to \code{NULL}.}
\item{lddt}{Defaults to \code{NULL}.}
\item{chain_id}{Defaults to \code{NULL}.}
\item{temperature}{Defaults to \code{1}.}
\item{l2}{Defaults to \code{0.001}.}
\item{iters}{Defaults to \code{300L}.}
\item{lr}{Defaults to \code{0.5}.}
\item{pae_bin_width}{Defaults to \code{0.5}.}
}
\description{
Part of the alfesf_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{plddt}, \code{plddt_mean}, \code{ptm}, \code{iptm}, \code{pae}, \code{d0}, \code{temperature}, \code{weights}, \code{route}, \code{n_lddt_bins}, \code{n_pae_bins}, \code{method}, \code{note}.
}
\keyword{internal}
