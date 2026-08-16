% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_saigeg}
\alias{morie_saigeg}
\title{morie_saigeg}
\usage{
morie_saigeg(y, G, X = NULL, mu = NULL, ratio = 1, two_sided = TRUE)
}
\arguments{
\item{y}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{G}{Passed to \code{.saigeg_score_statistic}.}
\item{X}{Passed to \code{.saigeg_fit_null}.}
\item{mu}{Optional; may be \code{NULL}. Passed to \code{.saigeg_score_statistic}.}
\item{ratio}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.}
\item{two_sided}{Passed to \code{.saigeg_normal_pvalue}. Defaults to \code{TRUE}.}
}
\description{
A step of the saigeg_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{p_value}, \code{p_normal}, \code{score}, \code{variance}, \code{z}, \code{case_control_ratio}, \code{n_cases}, \code{n_controls}, \code{variance_ratio}, \code{saddlepoint}, \code{method}, \code{why}.
}
\references{
----------
Zhou, W., Nielsen, J. B., Fritsche, L. G., Dey, R., Gabrielsen, M. E.,
Wolford, B. N. et al. (2018) "Efficiently controlling for case-control
imbalance and sample relatedness in large-scale genetic association
studies", Nature Genetics, doi:10.1038/s41588-018-0184-y. The
motivation (unbalanced case-control ratios in biobank PheWAS, the
failure of LMM and of the Gaussian score test), the use of the
saddlepoint approximation to calibrate the logistic mixed-model score
test, and the computational strategy for large N. Volume and pages are
not printed in the accepted-article file held locally.

Dey, R., Schmidt, E. M., Abecasis, G. R. & Lee, S. (2017) "A fast and
accurate algorithm to test for binary phenotypes and its application
to PheWAS", The American Journal of Human Genetics 101(1), 37-49,
doi:10.1016/j.ajhg.2017.05.014. The saddlepoint-approximation score
test for unrelated samples that SAIGE extends to mixed models.

Lugannani, R. & Rice, S. (1980) "Saddle point approximation for the
distribution of the sum of independent random variables", Advances
in Applied Probability 12(2), 475-490, doi:10.2307/1426607. The tail
formula implemented in saddlepoint_pvalue.

Chen, H., Wang, C., Conomos, M. P., Stilp, A. M., Li, Z., Sofer, T. et
al. (2016) "Control for population structure and relatedness for
binary traits in genetic association studies via logistic mixed
models", The American Journal of Human Genetics 98(4), 653-666,
doi:10.1016/j.ajhg.2016.02.012. GMMAT, the Gaussian-approximation
predecessor whose calibration SAIGE repairs.
}
\keyword{internal}
