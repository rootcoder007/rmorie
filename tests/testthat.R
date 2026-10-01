library(testthat)
# covr merges one trace file per R process; under parallel testthat a worker
# that exits before its trace is written silently drops every line its test
# files touched, so the instrumented run stays in one process.
if (nzchar(Sys.getenv("R_COVR"))) Sys.setenv(TESTTHAT_PARALLEL = "FALSE")
# On Windows a parallel testthat worker dies with an access violation
# (exit code -1073741819) part-way through a file, which R-universe reports
# as a failed check; the suite runs in one process there.
if (.Platform$OS.type == "windows") Sys.setenv(TESTTHAT_PARALLEL = "FALSE")
library(rmorie)

test_check("rmorie")
