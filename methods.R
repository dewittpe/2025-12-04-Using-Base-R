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

  cond_levels <- LETTERS[1:12]

  prep <- dplyr::filter(data, .data[[conditions]] %in% cond_levels)
  prep[[conditions]] <- factor(prep[[conditions]], levels = cond_levels)
  prep[["flag"]] <- 1L

  rtn <-
    tidyr::pivot_wider(
      data        = prep,
      id_cols     = tidyselect::all_of(id.vars),
      names_from  = tidyselect::all_of(conditions),
      values_from = "flag",
      values_fill = 0L,
      values_fn   = max,
      names_expand = TRUE
    )

  iddf <- dplyr::distinct(
    data,
    dplyr::across(tidyselect::all_of(id.vars))
  )
  rtn <- dplyr::left_join(x = iddf, y = rtn, by = id.vars)

  rtn <-
    dplyr::mutate(rtn, dplyr::across(tidyselect::all_of(cond_levels), ~ tidyr::replace_na(.x, 0L)))

  rtn <-
    dplyr::select(rtn, tidyselect::all_of(id.vars), tidyselect::all_of(cond_levels))

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

via_stats_reshape <- function(data, id.vars, conditions) {
  check_inputs(data, id.vars, conditions)

  cond_levels <- LETTERS[1:12]
  iddf <- unique(data[, id.vars, drop = FALSE])

  idx <- data[[conditions]] %in% cond_levels
  cond_data <- data[idx, c(id.vars, conditions), drop = FALSE]
  cond_data <- cond_data[!duplicated(cond_data), , drop = FALSE]

  if (!nrow(cond_data)) {
    zeros <- as.data.frame(matrix(0L, nrow = nrow(iddf), ncol = length(cond_levels)))
    names(zeros) <- cond_levels
    return(cbind(iddf, zeros))
  }

  cond_data[[conditions]] <- factor(cond_data[[conditions]], levels = cond_levels)
  cond_data[["flag"]] <- 1L

  rtn <-
    stats::reshape(
      data = cond_data,
      idvar = id.vars,
      timevar = conditions,
      v.names = "flag",
      direction = "wide"
    )

  cond_cols <- paste0("flag.", cond_levels)
  missing <- cond_cols[!cond_cols %in% names(rtn)]
  if (length(missing)) {
    rtn[missing] <- 0L
  }

  rtn[cond_cols] <- lapply(
    cond_cols,
    function(nm) {
      col <- rtn[[nm]]
      col[is.na(col)] <- 0L
      as.integer(col)
    }
  )

  names(rtn)[match(cond_cols, names(rtn))] <- cond_levels

  rtn <-
    merge(
      x = iddf,
      y = rtn[, c(id.vars, cond_levels), drop = FALSE],
      all.x = TRUE,
      by = id.vars
    )

  rtn[cond_levels] <- lapply(rtn[cond_levels], function(x) { x[is.na(x)] <- 0L; x })
  rtn
}

via_reduce_merge <- function(data, id.vars, conditions) {
  check_inputs(data, id.vars, conditions)
  cond_levels <- LETTERS[1:12]
  iddf <- unique(data[, id.vars, drop = FALSE])

  cond_data <- data[data[[conditions]] %in% cond_levels, c(id.vars, conditions), drop = FALSE]
  cond_data <- cond_data[!duplicated(cond_data), , drop = FALSE]

  if (!nrow(cond_data)) {
    zeros <- as.data.frame(matrix(0L, nrow = nrow(iddf), ncol = length(cond_levels)))
    names(zeros) <- cond_levels
    return(cbind(iddf, zeros))
  }

  cond_split <- split(cond_data[, id.vars, drop = FALSE], cond_data[[conditions]])
  cond_split <- cond_split[cond_levels]

  cnds <- lapply(seq_along(cond_split), function(i) {
    chunk <- cond_split[[i]]
    if (is.null(chunk) || !nrow(chunk)) {
      return(NULL)
    }
    chunk[[cond_levels[i]]] <- 1L
    chunk
  })

  cnds <- cnds[!vapply(cnds, is.null, logical(1))]

  if (!length(cnds)) {
    zeros <- as.data.frame(matrix(0L, nrow = nrow(iddf), ncol = length(cond_levels)))
    names(zeros) <- cond_levels
    return(cbind(iddf, zeros))
  }

  cnds <- Reduce(
    f = function(x, y) { merge(x, y, all = TRUE, by = id.vars) },
    x = cnds,
    init = iddf
  )

  cond_missing <- cond_levels[!cond_levels %in% names(cnds)]
  if (length(cond_missing)) {
    cnds[cond_missing] <- 0L
  }

  cnds[cond_levels] <- lapply(cnds[cond_levels], function(x) { x[is.na(x)] <- 0L; x })
  cnds[, c(id.vars, cond_levels), drop = FALSE]
}

via_base_matrix <- function(data, id.vars, conditions) {
  check_inputs(data, id.vars, conditions)

  cond_levels <- LETTERS[1:12]
  iddf <- unique(data[, id.vars, drop = FALSE])

  idx <- data[[conditions]] %in% cond_levels
  cond_data <- data[idx, c(id.vars, conditions), drop = FALSE]
  cond_data <- cond_data[!duplicated(cond_data), , drop = FALSE]

  n_id <- nrow(iddf)
  zero_mat <- matrix(0L, nrow = n_id, ncol = length(cond_levels))
  colnames(zero_mat) <- cond_levels

  if (!nrow(cond_data)) {
    return(cbind(iddf, as.data.frame(zero_mat, check.names = FALSE)))
  }

  key_iddf <- interaction(iddf[, id.vars, drop = FALSE], drop = TRUE, sep = "\r")
  key_cnds <- interaction(cond_data[, id.vars, drop = FALSE], drop = TRUE, sep = "\r")

  ri <- match(key_cnds, key_iddf)
  ci <- match(cond_data[[conditions]], cond_levels)
  keep <- !(is.na(ri) | is.na(ci))
  if (any(keep)) {
    zero_mat[cbind(ri[keep], ci[keep])] <- 1L
  }

  cbind(iddf, as.data.frame(zero_mat, check.names = FALSE))
}
