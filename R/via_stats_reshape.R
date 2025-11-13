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
