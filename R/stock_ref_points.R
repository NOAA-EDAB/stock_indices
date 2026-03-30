#' plot b/bmsy and f/fmsy over time by species
#'
#' use stocksmart database
#'

# pull all northeast stock
northeast_stocks <- stocksmart::stock_assessment_summary |>
  dplyr::filter(grepl("Northeast", regional_ecosystem)) |>
  dplyr::distinct(stock_name, stock_id, itis, jurisdiction, science_center)


all_bio <- NULL
all_mort <- NULL
# loop over all stocks ato pull timeseries of b/bmsy and f/fmsy
for (istock in 1:nrow(northeast_stocks)) {
  species_stock <- northeast_stocks[istock, ]
  bio <- stocksmart::get_reference_points(
    stock = species_stock$stock_id,
    ref_point = "b_over_bmsy"
  ) |>
    dplyr::mutate(stock = species_stock$stock_name)
  fmort <- stocksmart::get_reference_points(
    stock = species_stock$stock_id,
    ref_point = "f_over_fmsy"
  ) |>
    dplyr::mutate(stock = species_stock$stock_name)

  # store all data in tidy format
  all_bio <- rbind(all_bio, bio)
  all_mort <- rbind(all_mort, fmort)
}


## Make plots
# B/BMSY by species over time
p <- ggplot2::ggplot(data = all_bio) +
  ggplot2::geom_line(
    ggplot2::aes(x = assessment_year, y = b_over_bmsy),
    linewidth = 1
  ) +
  ggplot2::facet_wrap(~stock) +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)
  ) +
  ggplot2::ggtitle("B/BMSY for Managed stocks in the Northeast") +
  ggplot2::geom_hline(yintercept = 1)

p
p1 <- plotly::ggplotly(p)

# F/FMSY
p <- ggplot2::ggplot(data = all_mort) +
  ggplot2::geom_line(
    ggplot2::aes(x = assessment_year, y = f_over_fmsy),
    linewidth = 1
  ) +
  ggplot2::facet_wrap(~stock) +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)
  ) +
  ggplot2::ggtitle("F/FMSY for Managed stocks in the Northeast") +
  ggplot2::geom_hline(yintercept = 1, linetype = "dashed")

p
p2 <- plotly::ggplotly(p)
p2
