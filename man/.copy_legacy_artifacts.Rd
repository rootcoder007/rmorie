% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.copy_legacy_artifacts}
\alias{.copy_legacy_artifacts}
\title{Internal helper: Copy Legacy Artifacts}
\usage{
.copy_legacy_artifacts(relative_paths, output_dir, root = file.path(.legacy_reference_root(), "six", "outputs"))
}
\description{
Internal helper: Copy Legacy Artifacts
}
\value{
The value of \code{copied}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.copy_legacy_artifacts(V, V)
}
