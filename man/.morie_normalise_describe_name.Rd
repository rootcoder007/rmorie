% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_normalise_describe_name}
\alias{.morie_normalise_describe_name}
\title{Normalise a name for describe-corpus lookup.}
\usage{
.morie_normalise_describe_name(name)
}
\arguments{
\item{name}{Character scalar, the user-supplied lookup key.}
}
\description{
Strips the leading `morie_` prefix and the legacy `describe_` prefix; also strips a trailing `.md` extension if present.
}
\value{
Normalised name (lowercase mnemonic).
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_normalise_describe_name(V)
}
