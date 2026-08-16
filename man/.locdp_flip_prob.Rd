% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.locdp_flip_prob}
\alias{.locdp_flip_prob}
\title{Private helper: compute Warner flip probability for a given epsilon}
\usage{
.locdp_flip_prob(epsilon)
}
\arguments{
\item{epsilon}{See Usage.}
}
\description{
p = 1 / (1 + exp(epsilon)) -> achieves epsilon-local-DP.
}
\value{
A numeric value.
}
\references{
----------
Kasiviswanathan, S. P., Lee, H. K., Nissim, K., Raskhodnikova, S., &
    Smith, A. (2011). What can we learn privately? *SIAM Journal on
    Computing*, 40(3), 793-826. (Local model, section 1; randomized
    response as the basic local protocol.)
Warner, S. L. (1965). Randomized response: a survey technique for
    eliminating evasive answer bias. *JASA*, 60(309), 63-69.
Dwork, C., & Roth, A. (2014). *FnT-TCS*, 9(3-4), section 3.2.
    Local source: /run/media/rootcoder/WD_BLACK/library/pdf/fetched-wave3/dwork-roth-2014-algorithmic-foundations-differential-privacy.pdf
}
\keyword{internal}
