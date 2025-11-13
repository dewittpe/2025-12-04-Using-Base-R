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
