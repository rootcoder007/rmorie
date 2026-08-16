% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.ucfR_predict_rating}
\alias{.ucfR_predict_rating}
\title{.ucfR_predict_rating}
\usage{
.ucfR_predict_rating(target, others, item, min_common = 2, top_k = NULL, significance = FALSE)
}
\arguments{
\item{target}{Passed to \code{.ucfR_neighbours}.}
\item{others}{A vector; indexed elementwise.}
\item{item}{See Usage.}
\item{min_common}{Passed to \code{.ucfR_neighbours}. Defaults to \code{2}.}
\item{top_k}{Passed to \code{.ucfR_neighbours}.}
\item{significance}{Passed to \code{.ucfR_neighbours}. Defaults to \code{FALSE}.}
}
\description{
A step of the ucfR_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{prediction}, \code{naive_weighted_mean}, \code{user_mean}, \code{n_neighbours}, \code{fell_back}, \code{method}, \code{note}.
}
\references{
----------
Resnick, P., Iacovou, N., Suchak, M., Bergstrom, P. & Riedl, J.
(1994) "GroupLens: An Open Architecture for Collaborative Filtering of
Netnews", *Proceedings of the 1994 ACM Conference on Computer
Supported Cooperative Work (CSCW \'94)*, 175-186,
doi:10.1145/192844.192905. [PDF supplied by Vee.] The premise that
people who agreed in the past are likely to agree again and that
predictions can therefore be made from correlated users\' ratings with
no content analysis; the Pearson correlation computed over co-rated
items as the neighbour weight; and the prediction as the user\'s own
mean plus a weighted average of the neighbours\' deviations from their
means, normalised by the sum of the absolute weights.

Herlocker, J. L., Konstan, J. A., Borchers, A. & Riedl, J. (1999)
"An Algorithmic Framework for Performing Collaborative Filtering",
*SIGIR \'99*, 230-237, doi:10.1145/312624.312682. Significance
weighting for small overlaps.

Sarwar, B., Karypis, G., Konstan, J. & Riedl, J. (2001)
"Item-based Collaborative Filtering Recommendation Algorithms",
*WWW \'01*, 285-295, doi:10.1145/371920.372071. The item-based
transpose of this.
}
\keyword{internal}
