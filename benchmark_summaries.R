library(data.table)

times <-
  c("via_base_matrix",
    "via_data.table",
    "via_reduce_merge",
    "via_tidyverse",
    "via_stats_reshape"
    )
times <- lapply(times, list.files, recursive = TRUE, full.names = TRUE)
times <- unlist(times)
times <- sapply(times, dget)
times <- data.table::data.table(file = names(times), seconds = times)

times[, c("method", "N", "iteration") := tstrsplit(file, "/")]
times[, N := as.integer(N)]
times[, iteration := as.integer(sub("\\.dput", "", iteration))]


g1 <-
  ggplot2::ggplot(times) +
  ggplot2::aes(x = seconds, fill = method) +
  ggplot2::geom_density(alpha = 0.8) +
  ggplot2::xlab("Seconds") +
  ggplot2::theme(
    axis.ticks.y = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_blank()
  ) +
  ggplot2::facet_wrap(~ N)

g2 <-
  ggplot2::ggplot(times) +
  ggplot2::aes(x = factor(N), y = seconds, fill = method, color = method) +
  ggplot2::geom_boxplot(alpha = 0.8) +
  ggplot2::xlab("Patients") +
  ggplot2::ylab("Seconds") +
  ggplot2::coord_flip()
g2

g3 <- g1 + ggplot2::scale_x_log10() + ggplot2::annotation_logticks(sides = "b")
g4 <- g2 + ggplot2::scale_y_log10() + ggplot2::annotation_logticks(sides = "b")

ggplot2::ggsave(
  plot =
    ggpubr::ggarrange(
      g1, g3, g2, g4,
      nrow = 2,
      ncol = 2,
      align = "h",
      common.legend = TRUE
    ),
  filename = paste0("benchmarks_n", N, ".png"),
  width = 6.2,
  height = 3.5
  )
