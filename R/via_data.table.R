via_data.table <- function(data, id.vars, conditions) {
  check_inputs(data, id.vars, conditions)
  stopifnot(inherits(data, 'data.table'))

  cond_levels <- LETTERS[1:12]
  cond_col <- conditions
  keep_cols <- c(id.vars, cond_col)

  # stay within data.table to avoid unnecessary copies
  `%chin%` <- get("%chin%", asNamespace("data.table"))
  dt <- data[get(cond_col) %chin% cond_levels, ..keep_cols]
  dt[, (cond_col) := factor(get(cond_col), levels = cond_levels)]
  dt <- unique(dt, by = c(id.vars, cond_col))
  dt[, flag := 1L]

  cast_formula <- as.formula(paste(paste(id.vars, collapse = "+"), "~", cond_col))

  rtn <- data.table::dcast(
    data = dt,
    formula = cast_formula,
    value.var = "flag",
    fun.aggregate = max,
    fill = 0L,
    drop = FALSE
  )

  iddf <- unique(data[, ..id.vars], by = id.vars)
  data.table::setkeyv(rtn, id.vars)
  rtn <- rtn[iddf, on = id.vars]

  data.table::setnafill(rtn, cols = cond_levels, fill = 0L)
  data.table::setcolorder(rtn, c(id.vars, cond_levels))

  rtn
}

via_data.table_threads1 <- function(data, id.vars, conditions) {
  via_data.table_with_threads(data, id.vars, conditions, threads = 1L)
}

via_data.table_threads2 <- function(data, id.vars, conditions) {
  via_data.table_with_threads(data, id.vars, conditions, threads = 2L)
}

via_data.table_threads4 <- function(data, id.vars, conditions) {
  via_data.table_with_threads(data, id.vars, conditions, threads = 4L)
}

via_data.table_with_threads <- function(data, id.vars, conditions, threads = NULL) {
  if (is.null(threads)) {
    return(via_data.table(data, id.vars, conditions))
  }

  old_threads <- data.table::getDTthreads()
  on.exit(data.table::setDTthreads(old_threads), add = TRUE)
  data.table::setDTthreads(threads)
  via_data.table(data, id.vars, conditions)
}
