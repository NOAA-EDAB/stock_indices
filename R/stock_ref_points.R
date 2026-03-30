#' Find all species in the northeast
#'
#'

northeast_stocks <- stocksmart::stock_assessment_summary |>
  dplyr::filter(grepl("Northeast", regional_ecosystem)) |>
  dplyr::distinct(stock_name, stock_id, itis, jurisdiction, science_center)
