% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.ucfR_co_rated}
\alias{.ucfR_co_rated}
\title{.ucfR_co_rated}
\usage{
.ucfR_co_rated(ratings_a, ratings_b)
}
\arguments{
\item{ratings_a}{See Usage.}
\item{ratings_b}{See Usage.}
}
\description{
Part of the ucfR_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{items}, \code{n}, \code{a}, \code{b}, \code{note}.
}
\references{
----------
Resnick, P., Iacovou, N., Suchak, M., Bergstrom, P. & Riedl, J.
(1994) "GroupLens: An Open Architecture for Collaborative Filtering of
Netnews", *Proceedings of the 1994 ACM Conference on Computer
Supported Cooperative Work (CSCW \\'94)*, 175-186,
doi:10.1145/192844.192905. [PDF supplied by Vee.] The premise that
people who agreed in the past are likely to agree again and that
predictions can therefore be made from correlated users\\' ratings with
no content analysis; the Pearson correlation computed over co-rated
items as the neighbour weight; and the prediction as the user\\'s own
mean plus a weighted average of the neighbours\\' deviations from their
means, normalised by the sum of the absolute weights.

Herlocker, J. L., Konstan, J. A., Borchers, A. & Riedl, J. (1999)
"An Algorithmic Framework for Performing Collaborative Filtering",
*SIGIR \\'99*, 230-237, doi:10.1145/312624.312682. Significance
weighting for small overlaps.

Sarwar, B., Karypis, G., Konstan, J. & Riedl, J. (2001)
"Item-based Collaborative Filtering Recommendation Algorithms",
*WWW \\'01*, 285-295, doi:10.1145/371920.372071. The item-based
transpose of this.
}
\keyword{internal}
