% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{m_step}
\alias{m_step}
\title{m_step}
\usage{
m_step(n_dw, post, K) 
}
\arguments{
\item{n_dw}{Passed to \code{.plsa_m_step}.}
\item{post}{Passed to \code{.plsa_m_step}.}
\item{K}{Passed to \code{.plsa_m_step}.}
}
\description{
A step of the plsa_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{.plsa_m_step}.
}
\references{
----------
Hofmann, T. (1999) "Probabilistic Latent Semantic Analysis",
Proceedings of the Fifteenth Conference on Uncertainty in
Artificial Intelligence (UAI 1999), 289-296, arXiv:1301.6705.
Sec. 2 (LSA by SVD and the assessment of its theoretical
foundation), Sec. 3 (the aspect model, eq. (1) asymmetric and
eq. (2) symmetric parameterisations, the conditional independence
of d and w given z, and z as a bottleneck), and Sec. 3.2 (EM:
eq. (3) for the E step and eqs. (4)-(6) for the M step; maximum
likelihood as minimisation of the cross entropy or KL divergence
against the empirical distribution).

NOTE: the text layer of the local PDF is garbled across the equation
region; the equations above were recovered by OCR (ocrpg.sh, 300 dpi
+ tesseract) rather than by pdftotext.

Deerwester, S., Dumais, S. T., Furnas, G. W., Landauer, T. K. &
Harshman, R. (1990) "Indexing by latent semantic analysis", Journal
of the American Society for Information Science 41(6), 391-407. The
LSA method this gives a probabilistic footing to.

Blei, D. M., Ng, A. Y. & Jordan, M. I. (2003) "Latent Dirichlet
Allocation", Journal of Machine Learning Research 3, 993-1022. The
Dirichlet prior that removes the per-document parameter growth;
implemented in lda.
}
\keyword{internal}
\examples{
m_step(n_dw = c(1, 2, 3, 4, 5, 6, 7, 8),
  post = data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9)), K = 5L)
}
