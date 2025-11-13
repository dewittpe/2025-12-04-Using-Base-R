for (f in list.files(path = "R", full.names = TRUE)) {
  source(f)
}

cargs <- commandArgs(trailingOnly = TRUE)
if (length(cargs) == 0) {
  cargs <- "benchmarks/via_base_matrix/100/1.rds"
} else if (length(cargs) > 1) {
  stop("expected commandArgs of length 0 or 1")
}
cargs <- strsplit(cargs, split = "/")[[1]]
BENCHMARKSDIR <- cargs[1]
METHOD <- cargs[2]
N <- as.integer(cargs[3])
ITERATION <- as.integer(sub("\\.rds", "", cargs[4]))

# create output directory if needed
outdir <- file.path(BENCHMARKSDIR, METHOD, N)
if (!dir.exists(outdir)) {
  dir.create(path = outdir, recursive = TRUE)
}

# set seed and build data
set.seed(ITERATION)
data <- generate_data(n.patients = N)
if (METHOD == "via_data.table") {
  data.table::setDT(data)
}

# apply the method
tic <- Sys.time()
x <-
  do.call(
    what = METHOD,
    args = list(data = data, id.vars = c("pid", "eid"), conditions = "condition")
  )
toc <- Sys.time()

# find the time and save that to disk
seconds <- as.numeric(difftime(toc, tic, units = "secs"))
outfile <- file.path(outdir, paste0(ITERATION, ".dput"))
dput(seconds, file = outfile)
