library(data.table)

times <- lapply("benchmarks", list.files, recursive = TRUE, full.names = TRUE)
times <- unlist(times)
times <- sapply(times, dget)
times <- data.table::data.table(file = names(times), seconds = times)

times[, c("method", "N", "iteration") := tstrsplit(file, "/")[-1]]
times[, N := as.integer(N)]
times[, iteration := as.integer(sub("\\.dput", "", iteration))]

g1 <-
  ggplot2::ggplot(times) +
  ggplot2::theme_bw() +
  ggplot2::aes(x = N, y = seconds, color = method) +
  ggplot2::geom_point(alpha = 0.2) +
  ggplot2::scale_x_log10(name = "Patients", labels = scales::label_comma()) +
  ggplot2::scale_y_log10(name = "Seconds", labels = scales::label_number()) +
  ggplot2::annotation_logticks(sides = "bl") +
  ggplot2::stat_smooth(formula = y ~ x, method = "loess") +
  ggplot2::theme(
    panel.grid.minor.y = ggplot2::element_blank(),
    legend.position = "bottom"
  )

if (interactive()) {
  g1
} else {
  ggplot2::ggsave(plot = g1, filename = "benchmark_summaries.png")
}
