library(data.table)

times <- lapply("benchmarks", list.files, recursive = TRUE, full.names = TRUE)
times <- unlist(times)
times <- sapply(times, dget)
times <- data.table::data.table(file = names(times), seconds = times)

times[, c("method", "N", "iteration") := tstrsplit(file, "/")[-1]]
times[, N := as.integer(N)]
times[, iteration := as.integer(sub("\\.dput", "", iteration))]

summaries <-
  times[,
    .(lwr = quantile(seconds, p = 0.25), seconds = median(seconds), upr = quantile(seconds, p = 0.95)),
    by = .(N, method)
    ]

g1 <-
  ggplot2::ggplot(
    summaries[!startsWith(method, "via_data.table") | method == "via_data.table_threads1"]
  ) +
  ggplot2::theme_bw() +
  ggplot2::aes(x = N, y = seconds, ymin = lwr, ymax = upr, fill = method, color = method) +
  ggplot2::geom_point() +
  ggplot2::geom_line() +
  ggplot2::geom_ribbon(alpha = 0.2) +
  ggplot2::scale_x_log10(name = "Patients", labels = scales::label_comma()) +
  ggplot2::scale_y_log10(name = "Seconds", labels = scales::label_number()) +
  ggplot2::annotation_logticks(sides = "bl") +
  ggplot2::theme(
    panel.grid.minor.y = ggplot2::element_blank()
  )

g2 <-
  ggplot2::ggplot(
    summaries[startsWith(method, "via_data.table")]
  ) +
  ggplot2::theme_bw() +
  ggplot2::aes(x = N, y = seconds, ymin = lwr, ymax = upr, fill = method, color = method) +
  ggplot2::geom_point() +
  ggplot2::geom_line() +
  ggplot2::geom_ribbon(alpha = 0.2) +
  ggplot2::scale_x_log10(name = "Patients", labels = scales::label_comma()) +
  ggplot2::scale_y_log10(name = "Seconds", labels = scales::label_number()) +
  ggplot2::annotation_logticks(sides = "bl") +
  ggplot2::theme(
    panel.grid.minor.y = ggplot2::element_blank()
  )
g2

if (!interactive()) {
  ggplot2::ggsave(plot = g1, filename = "benchmark_summaries.png", width = 6, height = 3.5)
  ggplot2::ggsave(plot = g2, filename = "benchmark_dt_summaries.png", width = 6, height = 3.5)
}
