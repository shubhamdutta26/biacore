#' Plot biacore SPR data
#'
#' @param file A txt file containing SPR data from biacore T200
#' @param raw_color A character string containing color information in hex for
#' the raw data
#' @param fitted_color A character string containing color information in hex for
#' the fitted data
#'
#' @return A ggplot2 object
#' @export
#'
#' @examples
#' file <- system.file("extdata", "aCD16_3G8_Mouse.txt", package = "biacore")
#' plot_biacore(file)
plot_biacore <- function(file,
                         raw_color = "black",
                         fitted_color = "red") {
  df <- readr::read_delim(
    file = file,
    delim = "\t",
    escape_double = FALSE,
    trim_ws = TRUE,
    show_col_types = FALSE) |>
    stats::na.omit()

  # Remove : and spaces replaced with _
  colnames(df) <- gsub("\\s+", "_", gsub(":", "", colnames(df)))

  # Remove _X and _Y
  removed_xy <- gsub("_X$", "", gsub("_Y$", "", colnames(df)))

  # Generate names of each sample without Fitted_
  samples <- removed_xy[!grepl("^Fitted_", removed_xy)]

  # Create the ggplot
  p <- ggplot2::ggplot(df) +
    ggplot2::xlab("Time (s)") +
    ggplot2::ylab("Response units")

  # Add black and red lines using a loop
  for (each_sample in samples) {
    p <- p +
      ggplot2::geom_line(ggplot2::aes(
        x = !!rlang::sym(paste0(each_sample, "_X")),
        y = !!rlang::sym(paste0(each_sample, "_Y"))),
        color = raw_color) +
      ggplot2::geom_line(ggplot2::aes(
        x = !!rlang::sym(paste0("Fitted_", each_sample, "_X")),
        y = !!rlang::sym(paste0("Fitted_", each_sample, "_Y"))),
        color = fitted_color)
  }

  # Print the plot
  return(p)
}
