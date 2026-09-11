% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_alfesf_esmfold_confidence}
\alias{morie_alfesf_esmfold_confidence}
\title{morie_alfesf_esmfold_confidence}
\usage{
morie_alfesf_esmfold_confidence(lddt_logits = NULL, pae_logits = NULL, features = NULL, weights = NULL, 
    lddt = NULL, chain_id = NULL, temperature = 1, l2 = 0.001, iters = 300L, 
    lr = 0.5, pae_bin_width = 0.5) 
}
\arguments{
\item{lddt_logits}{Optional; may be \code{NULL}. A matrix; indexed by row and column.}
\item{pae_logits}{Optional; may be \code{NULL}. Passed to \code{.alfesf_rows}.}
\item{features}{Optional; may be \code{NULL}. Passed to \code{.alfesf_rows}.}
\item{weights}{Optional; may be \code{NULL}. A list; the body reads \code{$b}, \code{$W} from it.}
\item{lddt}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{chain_id}{Optional; may be \code{NULL}. Coerced to vector by the body, with \code{as.vector}.}
\item{temperature}{Compared against \code{"fit"}. Defaults to \code{1}.}
\item{l2}{Passed to \code{.alfesf_fit_multinomial}. Defaults to \code{0.001}.}
\item{iters}{Passed to \code{.alfesf_fit_multinomial}. Defaults to \code{300L}.}
\item{lr}{Passed to \code{.alfesf_fit_multinomial}. Defaults to \code{0.5}.}
\item{pae_bin_width}{Passed to \code{.alfesf_pae_centres}. Defaults to \code{0.5}.}
}
\description{
A step of the alfesf_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{plddt}, \code{plddt_mean}, \code{ptm}, \code{iptm}, \code{pae}, \code{d0}, \code{temperature}, \code{weights}, \code{route}, \code{n_lddt_bins}, \code{n_pae_bins}, \code{method}, \code{note}.
}
\keyword{internal}
\examples{
# decode pLDDT logits: the score is the binned expectation
set.seed(2)
lddt_logits <- matrix(rnorm(6 * 50), nrow = 6, ncol = 50)
res <- morie_alfesf_esmfold_confidence(lddt_logits = lddt_logits)
round(res$plddt, 2)
round(res$plddt_mean, 2)
# aligned-error logits, one row per residue pair, give PAE and pTM
pae_logits <- matrix(rnorm(36 * 8), nrow = 36, ncol = 8)
pae <- morie_alfesf_esmfold_confidence(pae_logits = pae_logits)
dim(pae$pae)
round(pae$ptm, 4)
}
