% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.tqipb_pow4}
\alias{.tqipb_pow4}
\title{4^b by repeated multiplication. Written out because R\\\'s `^` on an}
\usage{
.tqipb_pow4(b)
}
\arguments{
\item{b}{See Usage.}
}
\description{
integer exponent is repeated squaring while Python\\\'s `**` calls libm pow(); the module does not rely on the two agreeing.
}
\value{
The value of \code{p}, as built in the body.
}
\keyword{internal}
