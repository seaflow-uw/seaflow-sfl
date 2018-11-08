library(tidyverse)
library(plotly)

setwd("~/Documents/DATA/Codes/seaflow-sfl/")

read_sfl <- function(x){
  df <- read_delim(x, delim="\t")
  cruise <- sub(".sfl", "", basename(x))
  df$cruise <- cruise
  return(df)
}

list.sfl <- list.files("curated", pattern=".sfl", full.names=T)
sfl <- do.call(rbind, lapply(list.sfl, function(x) read_sfl(x)))

sfl2 <- sfl %>%
        group_by(cruise, DATE= cut(DATE, breaks="1 hour")) %>%
        summarize(LAT = mean(LAT), LON = mean(LON))



geo <- list(
  showland = TRUE,
  showlakes = TRUE,
  showcountries = TRUE,
  showocean = TRUE,
  countrywidth = 0.5,
  landcolor = toRGB("grey90"),
  lakecolor = toRGB("white"),
  oceancolor = toRGB("white"),
  projection = list(
    type = 'orthographic',
    rotation = list(
      lon = -100,
      lat = 40,
      roll = 0
    )
  ),
  lonaxis = list(
    showgrid = TRUE,
    gridcolor = toRGB("gray40"),
    gridwidth = 0.5
  ),
  lataxis = list(
    showgrid = TRUE,
    gridcolor = toRGB("gray40"),
    gridwidth = 0.5
  )
)

p <- plot_geo(sfl2, lat = ~LAT, lon = ~LON, color = ~cruise) %>%
  layout(
    showlegend = FALSE, geo = geo
  )



plotly_IMAGE(p, format = "png", out_file = "cruise-track.png")
