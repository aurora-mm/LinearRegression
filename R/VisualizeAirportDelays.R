#' Visualize Mean Airport Delays by Location
#'
#' Creates a scatter plot that visualizes the mean departure delay of flights
#' at different destination airports across the United States. The plot shows
#' each airport's mean delay by longitude and latitude, with both color and size
#' representing the average delay in minutes.
#'
#' @return A ggplot object representing the mean delays at airports across the United States.
#'
#' @export visualize_airport_delays
#' @importFrom dplyr filter group_by summarize inner_join %>%
#' @importFrom ggplot2 ggplot aes geom_point scale_color_viridis_c scale_size_continuous labs theme_minimal theme
#' @importFrom rlang .data
visualize_airport_delays <- function() {
  # Join the flights and airports datasets
  delay_data <- nycflights13::flights %>%
    filter(!is.na(.data$dep_delay)) %>%
    group_by(.data$dest) %>%
    summarize(mean_delay = mean(.data$dep_delay, na.rm = TRUE)) %>%
    inner_join(nycflights13::airports, by = c("dest" = "faa"))

  # Create the plot
  ggplot(delay_data, aes(x = .data$lon, y = .data$lat, size = .data$mean_delay, color = .data$mean_delay)) +
    geom_point(alpha = 0.7) +
    scale_color_viridis_c(option = "C", name = "Mean Delay (min)") +
    scale_size_continuous(name = "Mean Delay (min)") +
    labs(
      title = "Mean Flight Delays by Destination Airport",
      x = "Longitude", y = "Latitude"
    ) +
    theme_minimal() +
    theme(legend.position = "right")
}
