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
