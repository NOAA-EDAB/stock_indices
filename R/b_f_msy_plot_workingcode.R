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

# LINE PLOTS

######### F/Fmsy ##########

# Filter out missing values
mort_clean <- all_mort |>
  dplyr::filter(!is.na(f_over_fmsy))

# Plot
ggplot2::ggplot(
  mort_clean,
  ggplot2::aes(x = assessment_year, y = f_over_fmsy)
) +
  ggplot2::geom_hline(
    yintercept = 1,
    linetype = "dashed",
    color = "gray",
    size = 0.8
  ) +
  ggplot2::geom_point(color = "black", size = 2) +
  ggplot2::stat_summary(fun = mean, geom = "line", color = "red", size = 1) +
  ggplot2::stat_summary(fun = mean, geom = "point", color = "red", size = 2.5) +
  ggplot2::labs(
    title = "F/Fmsy for Managed Species",
    x = NULL,
    y = "F/Fmsy"
  ) +
  ggplot2::scale_x_continuous(breaks = scales::pretty_breaks()) +
  ggplot2::theme_classic() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(size = 14, face = "plain", hjust = 0),
    legend.title = ggplot2::element_blank(),
    axis.title.y = ggplot2::element_text(size = 12, color = "black"),
    axis.text = ggplot2::element_text(size = 11, color = "black"),
    panel.border = ggplot2::element_rect(color = "black", fill = NA, size = 1),
    axis.line = ggplot2::element_blank() # Border takes place of axis lines
  )

######## B/Bmsy #########

# Filter out missing values
bio_clean <- all_bio |>
  dplyr::filter(!is.na(b_over_bmsy))

# Plot
ggplot2::ggplot(bio_clean, ggplot2::aes(x = assessment_year, y = b_over_bmsy)) +
  ggplot2::geom_hline(
    yintercept = 1,
    linetype = "dashed",
    color = "gray",
    size = 0.8
  ) +
  ggplot2::geom_point(color = "black", size = 2) +
  ggplot2::stat_summary(fun = mean, geom = "line", color = "red", size = 1) +
  ggplot2::stat_summary(fun = mean, geom = "point", color = "red", size = 2.5) +
  ggplot2::labs(
    title = "B/Bmsy for Managed Species",
    x = NULL,
    y = "B/Bmsy"
  ) +
  ggplot2::scale_x_continuous(breaks = scales::pretty_breaks()) +
  ggplot2::theme_classic() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(size = 14, face = "plain", hjust = 0),
    legend.title = ggplot2::element_blank(),
    axis.title.y = ggplot2::element_text(size = 12, color = "black"),
    axis.text = ggplot2::element_text(size = 11, color = "black"),
    panel.border = ggplot2::element_rect(color = "black", fill = NA, size = 1),
    axis.line = ggplot2::element_blank() # Border takes place of axis lines
  )
