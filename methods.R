################################################################################
# file: methods.R
#
# Source shared helpers and individual via_* implementations.
################################################################################

source("R/common.R")

method_files <- c(
  "R/via_tidyverse.R",
  "R/via_data.table.R",
  "R/via_stats_reshape.R",
  "R/via_reduce_merge.R",
  "R/via_base_matrix.R"
)

for (path in method_files) {
  source(path)
}
