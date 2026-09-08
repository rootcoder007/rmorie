% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_ncfRS_neumf}
\alias{morie_ncfRS_neumf}
\title{morie_ncfRS_neumf}
\usage{
morie_ncfRS_neumf(p_gmf, q_gmf, p_mlp, q_mlp, Ws, bs, h) 
}
\arguments{
\item{p_gmf}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{q_gmf}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{p_mlp}{Passed to \code{morie_ncfRS_mlp_layers}.}
\item{q_mlp}{Passed to \code{morie_ncfRS_mlp_layers}.}
\item{Ws}{Passed to \code{morie_ncfRS_mlp_layers}.}
\item{bs}{Passed to \code{morie_ncfRS_mlp_layers}.}
\item{h}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the ncfRS_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{score}, \code{gmf_part}, \code{mlp_part}, \code{note}.
}
\references{
----------
He, X., Liao, L., Zhang, H., Nie, L., Hu, X. & Chua, T.-S. (2017)
"Neural Collaborative Filtering", *Proceedings of the 26th
International Conference on World Wide Web (WWW \'17)*, 173-182,
doi:10.1145/3038912.3052569. Sec. 3.1 (the general NCF framework and
the log loss with sampled negatives for implicit data), Sec. 3.2
(GMF: eq. (9), and the demonstration that MF is recovered when a_out
is the identity and h is uniform), Sec. 3.3 (MLP over the
concatenation), and Sec. 3.4 (NeuMF: separate GMF and MLP embeddings
fused in the last layer, and why sharing one embedding would limit
the fused model).

Koren, Y., Bell, R. & Volinsky, C. (2009) "Matrix Factorization
Techniques for Recommender Systems", *Computer* 42(8), 30-37,
doi:10.1109/MC.2009.263. The inner-product model being generalised.
}
\keyword{internal}
\examples{
set.seed(1)
p_gmf <- rnorm(4); q_gmf <- rnorm(4)
p_mlp <- rnorm(4); q_mlp <- rnorm(4)
Ws <- list(matrix(rnorm(4 * 8, 0, 0.3), 4, 8),
           matrix(rnorm(2 * 4, 0, 0.3), 2, 4))
bs <- list(rep(0, 4), rep(0, 2))
h <- rnorm(6, 0, 0.3)
s <- morie_ncfRS_neumf(p_gmf, q_gmf, p_mlp, q_mlp, Ws, bs, h)
is.numeric(s)
}
