for (f in list.files(path = "R", full.names = TRUE)) {
  source(f)
}


set.seed(42)
data <- generate_data(n.patients = 2)
TBL <- tibble::as_tibble(data)
DT  <- data.table::copy(data)
data.table::setDT(DT)

rtn_tidy <-
  via_tidyverse(TBL, id.vars = c("pid", "eid"), conditions = "condition")

rtn_dt <-
  via_data.table(DT, id.vars = c("pid", "eid"), conditions = "condition")

rtn_reshape <-
  via_stats_reshape(data, id.vars = c("pid", "eid"), conditions = "condition")

rtn_reduce_merge <-
  via_reduce_merge(data, id.vars = c("pid", "eid"), conditions = "condition")

rtn_base_matrix  <-
  via_base_matrix(data, id.vars = c("pid", "eid"), conditions = "condition")

stopifnot(
  isTRUE(all.equal(rtn_tidy, rtn_dt,           check.class = FALSE, check.attributes = FALSE)),
  isTRUE(all.equal(rtn_tidy, rtn_reshape,      check.class = FALSE, check.attributes = FALSE)),
  isTRUE(all.equal(rtn_tidy, rtn_reduce_merge, check.class = FALSE, check.attributes = FALSE)),
  isTRUE(all.equal(rtn_tidy, rtn_base_matrix,  check.class = FALSE, check.attributes = FALSE))
)

print("Success!")
