% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.otis_alert_volatility_frame}
\alias{.otis_alert_volatility_frame}
\title{Internal helper: Otis Alert Volatility Frame}
\usage{
.otis_alert_volatility_frame(df)
}
\description{
Internal helper: Otis Alert Volatility Frame
}
\value{
The value of \code{base}, as built in the body.
}
\keyword{internal}
\examples{
df <- morie_synth_otis("b01", n = 200L, seed = 1L)
grid <- morie_otis_causal_grid(df)
names(grid)
.otis_alert_volatility_frame(df = df)
}
