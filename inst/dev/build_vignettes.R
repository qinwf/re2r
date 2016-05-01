Sys.setenv(OS="Linux")
Sys.setenv(RUN_RE2R_BENCHMARK="TRUE")
library(devtools)
build_vignettes()
