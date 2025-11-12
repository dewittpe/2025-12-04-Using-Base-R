################################################################################
# file: methods.R
#
# define several methods for use in a presentation to the Devner R User Group on
# using tidyverse, data.table, and base R method for building a inidcatior 
################################################################################

generate_data <- function(n.patients = 10L) {
  # for each patient, generate a random number of encounters (at least one
  # encounter for each patient)
  encounters <- rpois(n = n.patients, lambda = 4.5) + 1L

  # for each encounter generate a set of random "conditions"
  n.conditions <- rpois(n = sum(encounters), lambda = 2)

  # generate the conditions
  conditions <- lapply(n.conditions, sample, x = LETTERS, replace = TRUE)
  conditions <- lapply(conditions, function(x) {if (length(x)) x else ""})
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

via_tidyverse <- function(data, id.vars, conditions) {
  check_inputs(data, id.vars, conditions)
  stopifnot(!inherits(data, "data.table"))

  rtn <-
    tidyr::pivot_wider(
      data        = dplyr::filter(data, .data[[conditions]] %in% LETTERS[1:12]),
      id_cols     = tidyselect::all_of(id.vars),
      names_from  = tidyselect::all_of(conditions),
      values_from = tidyselect::all_of(conditions),
      values_fill = 0L,
      values_fn   = function(x) { as.integer(length(x) > 0) }
    )

  # verify all conditions have been flagged
  for (j in LETTERS[which(!(LETTERS[1:12] %in% names(rtn)))]) {
    rtn[[j]] <- rep(0L, nrow(rtn))
  }

  iddf <- dplyr::distinct(dplyr::select(data, tidyselect::all_of(id.vars)))
  rtn <- dplyr::left_join(x = iddf, y = rtn, by = id.vars)

  rtn <-
    dplyr::mutate(rtn, dplyr::across(tidyselect::everything(), ~ tidyr::replace_na(.x, 0L)))

  rtn <-
    dplyr::select(rtn, tidyselect::all_of(id.vars), tidyselect::all_of(LETTERS[1:12]))

  rtn
}

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

via_stats_reshape <- function(data, id.vars, conditions) {
  check_inputs(data, id.vars, conditions)

  rtn <-
    stats::reshape(
      data = unique(subset(data, data[[conditions]] %in% LETTERS[1:12])),
      idvar = id.vars,
      timevar = conditions,
      v.names = conditions,
      direction = "wide"
    )

  for (j in names(rtn)[-which(names(data) %in% id.vars)]) {
    flag <- as.integer(!is.na(rtn[[j]]))
    if (length(flag) == 0) {
      flag <- rep(0L, nrow(rtn))
    }
    rtn[[j]] <- flag
  }

  names(rtn) <- sub(pattern = paste0(conditions, "."), "", names(rtn), fixed = TRUE)

  # verify all conditons have been flagged
  for (j in LETTERS[which(!(LETTERS[1:12] %in% names(rtn)))]) {
    rtn[[j]] <- rep(0L, nrow(rtn))
  }

  colorder <- c(id.vars, LETTERS[1:12])

  rtn <-
    merge(
      x = unique(data[, id.vars, drop = FALSE]),
      y = rtn[, colorder, drop = FALSE],
      all.x = TRUE,
      by = id.vars
    )

  rtn[is.na(rtn)] <- 0L
  rtn
}

via_reduce_merge <- function(data, id.vars, conditions) {
  check_inputs(data, id.vars, conditions)
  iddf <- unique(data[, id.vars, drop = FALSE])

  cnds <- lapply(LETTERS[1:12], function(x) {
    idx <- data[[conditions]] == x
    rtn <- unique(data[idx, id.vars, drop = FALSE])
    rtn[[x]] <- rep(1L, nrow(rtn))
    rtn
    })

  cnds <- Reduce(
    f = function(x, y) { merge(x, y, all = TRUE, by = id.vars) },
    x = cnds,
    init = iddf
  )

  cnds[is.na(cnds)] <- 0L
  cnds
}

via_base_matrix <- function(data, id.vars, conditions) {
  check_inputs(data, id.vars, conditions)

  iddf <- unique(data[, id.vars, drop = FALSE])
  cnds <- unique(subset(data, data[[conditions]] %in% LETTERS[1:12]))

  X <- matrix(0L, nrow = nrow(iddf), ncol = 12)
  colnames(X) <- LETTERS[1:12]

  key_iddf <- do.call(paste, c(iddf, sep = "\r"))
  key_cnds <- do.call(paste, c(cnds[, id.vars, drop = FALSE], sep = "\r"))

  ri <- match(key_cnds, key_iddf)
  ci <- match(cnds[[conditions]], LETTERS[1:12])
  keep <- !(is.na(ri) | is.na(ci))
  if (any(keep)) {
    X[cbind(ri[keep], ci[keep])] <- 1L
  }

  cbind(iddf, as.data.frame(X, check.names = FALSE))
}



