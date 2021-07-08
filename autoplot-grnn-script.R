my_colours <- function(name) {
  col_l <- list("blue" = "#000099",
                "red" = "#CC0000",
                "green" = "#339900",
                "orange" = "#CC79A7"
  )
  return(col_l[[name]])
}

autoplot.grnnForecast <- function(forecast, highlight = c("points")) {
  highlight = match.arg(highlight)
  
  # extract the time series
  timeS <- data.frame(
    x = as.vector(stats::time(forecast$orig_timeS)),
    y = as.vector(forecast$orig_timeS)
  )
  
  # extract the forecast
  pred <- data.frame(
    x = as.vector(stats::time(forecast$prediction)),
    y = as.vector(forecast$prediction)
  )
  
  p <- ggplot2::ggplot(timeS, ggplot2::aes_string('x', 'y'))
  p <- p + ggplot2::geom_line(ggplot2::aes(colour = "Original"))
  p <- p + ggplot2::geom_line(ggplot2::aes(colour = "Forecast"), data = pred)
  if (highlight == "points") {
    p <- p + ggplot2::geom_point(ggplot2::aes(colour = "Original"))
    p <- p + ggplot2::geom_point(ggplot2::aes(colour = "Forecast"), data = pred)
  }
  breaks <- c("Original", "Forecast")
  colours <- c("Original" = "black", "Forecast" = my_colours("blue"))
  p <- p + ggplot2::scale_colour_manual(values = colours, breaks = breaks)
  p <- p + ggplot2::labs(x = "Time", y = NULL, colour = "Time series")
  p
}




#backcast
my_colours <- function(name) {
  col_l <- list("blue" = "#000099",
                "red" = "#CC0000",
                "green" = "#339900",
                "orange" = "#CC79A7"
  )
  return(col_l[[name]])
}

autoplot.grnnBackcast <- function(forecast, highlight = c("points")) {
  highlight = match.arg(highlight)
  
  # extract the time series
  timeS <- data.frame(
    x = as.vector(stats::time(forecast$orig_timeS)),
    y = rev(as.vector(forecast$orig_timeS))
  )
  
  # extract the forecast
  pred <- data.frame(
    x = as.vector(stats::time(forecast$prediction)),
    y = as.vector(forecast$prediction)
  )
  
  p <- ggplot2::ggplot(timeS, ggplot2::aes_string('x', 'y'))
  p <- p + ggplot2::geom_line(ggplot2::aes(colour = "Original"))
  p <- p + ggplot2::geom_line(ggplot2::aes(colour = "Backcast"), data = pred)
  if (highlight == "points") {
    p <- p + ggplot2::geom_point(ggplot2::aes(colour = "Original"))
    p <- p + ggplot2::geom_point(ggplot2::aes(colour = "Backcast"), data = pred)
  }
  breaks <- c("Original", "Backcast")
  colours <- c("Original" = "black", "Backcast" = my_colours("blue"))
  p <- p + ggplot2::scale_colour_manual(values = colours, breaks = breaks)
  p <- p + ggplot2::labs(x = "Time", y = NULL, colour = "Time series")
  p
}