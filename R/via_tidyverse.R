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
