% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.siu_apply_canonical_overrides}
\alias{.siu_apply_canonical_overrides}
\title{Internal helper: Siu Apply Canonical Overrides}
\usage{
.siu_apply_canonical_overrides(df, overrides)
}
\description{
Internal helper: Siu Apply Canonical Overrides
}
\value{
The value of \code{df}, as built in the body.
}
\keyword{internal}
\examples{
idx <- morie_siu_index(lang = "en")
head(idx)
# How many drids are English vs French vs unknown?
table(morie_siu_index()$`_language`)
# Unique-case index (English-preferred)
canon <- morie_siu_index(canonical_only = TRUE)
nrow(canon)
.siu_apply_canonical_overrides(df = canon, overrides = idx)
}
