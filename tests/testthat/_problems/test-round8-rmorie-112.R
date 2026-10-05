# Extracted from test-round8-rmorie.R:112

# test -------------------------------------------------------------------------
skip_on_os("windows")
d <- tempfile("rocache")
dir.create(d)
Sys.chmod(d, "0555")
on.exit(Sys.chmod(d, "0755"), add = TRUE)
skip_if(file.access(d, 2L) == 0L, "running as root: directories are always writable")
withr::local_envvar(MORIE_CACHE_DIR = d)
expect_message(rmorie:::.morie_cache_store_soft(data.frame(a = 1), "t1"), "cache skipped for t1")
