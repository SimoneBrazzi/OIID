library(tidyverse)
library(ggplot2)
library(plotly)
library(scales)
library(roxygen2)
library(lubridate)
library(rsdmx)
library(owidR)
library(reticulate)
library(gt)
library(janitor)
library(shiny)
library(readr)  # For reading and writing CSV files
library(R.utils)

# functions.R

#' Title: Time Series Plotting Function
#' 
#' @description This function creates a time series plot of the given data if x is not empty, null, or zero.
#' @param t A data frame containing the time series data.
#' @param x A numeric vector for the x-axis. If x is empty, null, or zero, the function will not plot.
#' @param y A numeric vector for the y-axis.
#' @param colour A factor variable for coloring the lines/points.
#' @param palette A character string specifying the color palette.
#' @param title A character string for the plot title.
#' @param subtitle A character string for the plot subtitle.
#' @param xlabs A character string for the x-axis label.
#' @param ylabs A character string for the y-axis label.
#' @param legend_title A character string for the legend title.
#' @return A Plotly object of the time series plot if x is valid.
#' @examples
#' \dontrun{
#' plot_time_series(
#'   t = data.frame(time = as.Date('2021-01-01') + 0:9, value = rnorm(10)),
#'   x = as.Date('2021-01-01') + 0:9, 
#'   y = rnorm(10), 
#'   colour = as.factor(rep(1, 10)),
#'   palette = "Set1", 
#'   title = "Time Series Plot", 
#'   subtitle = "Example Plot", 
#'   xlabs = "Time", 
#'   ylabs = "Value", 
#'   legend_title = "Series"
#' )
#' }
#' @export
plot_time_series <- function(
    t, x, y, colour, palette, title, subtitle, xlabs, ylabs, legend_title
) {
  
  if (is.null(x) || length(x) == 0 || all(x == 0)) {
    message("x is empty, null, or zero. No plot will be generated.")
    return(print("Non ci sono dati disponibili."))
  }
  
  line_plot <- ggplot(t) +
    geom_line(aes(x = x, y = y, colour = colour)) +
    geom_point(aes(x = x, y = y, colour = colour)) +
    theme_classic() +
    scale_color_brewer(palette = palette) +
    labs(
      title = title,
      subtitle = subtitle,
      x = xlabs,
      y = ylabs,
      color = legend_title
    ) +
    scale_x_date(breaks = pretty_breaks(n = 10)) +
    scale_y_continuous(
      breaks = pretty_breaks(n = 10),
      labels = number_format(big.mark = ",")
    )
  
  line_plotly <- ggplotly(line_plot) %>% 
    layout(hovermode="x unified")
  
  return(line_plotly)
}


#' Read and Save Data from SDMX
#'
#' This function reads data from the SDMX service provided by ISTAT and saves it as an RDS file for future use. 
#' If the RDS file already exists, it loads the data from the file.
#'
#' @param id The ID of the data flow.
#' @param dsd The data structure definition.
#' @param path The path where the RDS file will be saved.
#' @param version The version of the data (currently unused in the function).
#'
#' @return A data frame containing the data read from the SDMX service or the RDS file.
#' @import readr
#' @import stringr
#' @import SDMX
#' @export
#'
#' @examples
#' \dontrun{
#'   df <- read_db("dataflow_id", "dsd_id", "path/to/save/", "version")
#' }
read_db <- function(id, dsd, path, version) {
  
  rds_path <- str_c(path, id, ".rds")
  
  if (file.exists(rds_path)) {
    # If the RDS file exists, read data from the RDS file
    df <- readRDS(rds_path)
  } else {
    # If the RDS file does not exist, read data using readSDMX and save to RDS
    df <- readSDMX(
      providerId = "ISTAT",
      resource = "data", 
      flowRef = id,
      dsd = dsd
    ) %>% 
      as.data.frame(labels = TRUE) %>% 
      mutate(year = ymd(str_c(obsTime, "-01-01"))) %>%
      janitor::clean_names() %>%
      rename(value = obs_value) %>%
      select(year, matches("_it"), value) %>% 
      rename_with(~ str_remove(.x, "_label_it$"))
    
    # Save data to RDS file
    saveRDS(df, rds_path)
  }
  
  return(df)
}



