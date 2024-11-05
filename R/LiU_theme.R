#' LiU Theme
#'
#' A custom theme based on the Linköping University style.
#' This function applies both a custom ggplot theme and a manual color scale.
#'
#' @return A list containing the custom theme and color scale.
#' @export LiU_theme
#' @importFrom ggplot2 theme_grey
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 element_rect
#' @importFrom ggplot2 scale_color_manual

LiU_theme <- function() {
  # Return the list
  return(list(
    ggplot2::theme_grey() +
      ggplot2::theme(
        panel.background = ggplot2::element_rect(fill = "#bde6f6", color = NA),
        plot.background = ggplot2::element_rect(fill = "#d2eef9", color = NA),
        legend.background = ggplot2::element_rect(fill = "#bde6f6", color = NA)
      ),
    ggplot2::scale_color_manual(values = c("#ff6442", "#8781d3", "#fcf05f", "#687f91"))
  ))
}
