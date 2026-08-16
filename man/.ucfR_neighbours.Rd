% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.ucfR_neighbours}
\alias{.ucfR_neighbours}
\title{.ucfR_neighbours}
\usage{
.ucfR_neighbours(target, others, min_common = 2, top_k = NULL, significance = FALSE)
}
\arguments{
\item{target}{Passed to \code{.ucfR_pearson}.}
\item{others}{A vector; its length is taken and its elements indexed.}
\item{min_common}{Passed to \code{.ucfR_pearson}. Defaults to \code{2}.}
\item{top_k}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
\item{significance}{Passed to \code{.ucfR_pearson}. Defaults to \code{FALSE}.}
}
\description{
A step of the ucfR_native implementation. Called by \code{.ucfR_predict_rating}. See the file header for the source the module follows.
}
\value{
A list with \code{neighbours}, \code{n}, \code{note}.
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
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.ucfR_neighbours(V, V)
}
