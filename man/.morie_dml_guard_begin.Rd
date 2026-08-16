% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_dml_guard_begin}
\alias{.morie_dml_guard_begin}
\title{Internal guard for DoubleML cross-fitting via {future}}
\usage{
.morie_dml_guard_begin()
}
\description{
DoubleML cross-fits through \pkg{future}. Three failure modes are guarded here, uniformly, for every `$fit()` call site: 1. Worker launch can fail outright on constrained machines    (`FutureLaunchError`). The fits are small, so parallel workers buy    nothing: evaluate sequentially in-process. 2. future\'s connection-misuse diagnostic can segfault R uncatchably    on some builds (`diff_connections()` inside `FutureResult`). 3. future raises an "UNRELIABLE VALUE ... future.seed=TRUE"    RNG-misuse condition; call sites seed explicitly via    `set.seed(random_state)`, so it is a false alarm that would    otherwise trip R CMD check under `stop_on_warning`. Usage at a call site (restores everything on exit): \preformatted{   .gst <- .morie_dml_guard_begin()   on.exit(.morie_dml_guard_end(.gst), add = TRUE) }
}
\value{
An opaque state list for `.morie_dml_guard_end`.
}
\keyword{internal}
\examples{
.morie_dml_guard_begin()
}
