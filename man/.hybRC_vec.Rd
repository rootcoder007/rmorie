% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.hybRC_vec}
\alias{.hybRC_vec}
\title{Private helpers (prefixed to avoid R/ environment collisions)}
\usage{
.hybRC_vec(x)
}
\arguments{
\item{x}{Optional; may be \code{NULL}. Passed to \code{is.null}.}
}
\description{
A step of the hybRC_native implementation. Called by \code{weighted}. See the file header for the source the module follows.
}
\value{
A vector, from \code{as.numeric}.
}
\references{
----------
Burke, R. (2002) "Hybrid Recommender Systems: Survey and
Experiments", *User Modeling and User-Adapted Interaction* 12(4),
331-370, doi:10.1023/A:1021240730564. [PDF supplied by Vee.] The
taxonomy of seven hybridisation methods -- weighted, switching, mixed,
feature combination, cascade, feature augmentation and meta-level --
with the weighted hybrid\'s implicit assumption that the relative value
of the techniques is more or less uniform across the space of possible
items; the note that switching hybrids introduce additional complexity
because the switching criteria must be determined, adding another
level of parameterisation, in exchange for sensitivity to the
components\' strengths and weaknesses; and the observation that four
techniques -- weighted, mixed, switching and feature combination --
are order-insensitive, so a CN/CF mixed system is no different from a
CF/CN one.

Balabanovic, M. & Shoham, Y. (1997) "Fab: content-based,
collaborative recommendation", *Communications of the ACM* 40(3),
66-72, doi:10.1145/245108.245124. An early hybrid.

Resnick, P. et al. (1994) "GroupLens", *CSCW \'94*, 175-186,
doi:10.1145/192844.192905. The collaborative half; implemented in
:mod:`ucfR`.
"""
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.hybRC_vec(V)
}
