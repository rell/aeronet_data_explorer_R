library(
  ggplot2,
  here
  )

# Define the function
download_ggplot <- function(plot, name) {
  print(paste("downloading ", format(name),".png", sep = ''))
  ggsave(
    plot = plot,
    file = here("graphs", paste(name, ".png", sep = "")),
    dpi = 400,
    width = 7,
    height = 8,
    units = "in"
  )
}

