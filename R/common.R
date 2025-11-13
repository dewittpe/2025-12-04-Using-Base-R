################################################################################
# Common helpers shared across via_* implementations
################################################################################

generate_data <- function(n.patients = 10L) {
  # for each patient, generate a random number of encounters (at least one
  # encounter for each patient)
  encounters <- rpois(n = n.patients, lambda = 4.5) + 1L

  # for each encounter generate a set of random "conditions"
  n.conditions <- rpois(n = sum(encounters), lambda = 2)

  # generate the conditions
  conditions <- lapply(n.conditions, sample, x = LETTERS, replace = TRUE)
  conditions <- lapply(conditions, function(x) { if (length(x)) x else "" })
  n.conditions <- lapply(conditions, length)

  pid <- rep.int(seq_len(n.patients), times = encounters)
  pid <- rep.int(pid, times = n.conditions)

  eid <- do.call(c, lapply(encounters, seq_len))
  eid <- rep.int(eid, times = n.conditions)

  data.frame(pid = pid, eid = eid, condition = do.call(c, conditions))
}

check_inputs <- function(data, id.vars, conditions) {
  stopifnot(inherits(data, "data.frame"))
  stopifnot(inherits(id.vars, "character"), length(id.vars) > 0L)
  stopifnot(inherits(conditions, "character"), length(conditions) == 1L)
  stopifnot(all(id.vars %in% names(data)), conditions %in% names(data))
  invisible(TRUE)
}
