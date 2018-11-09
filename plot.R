library(tidyverse)
library(plotly)
library(viridis)
library(googlesheets)

setwd("~/Documents/DATA/Codes/seaflow-sfl/")

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


# Get official cruise ID
seaflow.meta <- gs_read(gs_title("SeaFlow\ instrument\ log", verbose = FALSE))



read_sfl <- function(x){
  df <- read_delim(x, delim="\t")

    #parse cruise name and serial number of instrument
    exp <- unlist(list(strsplit(sub(".sfl", "", basename(x)),"_")))

      if(length(exp) > 2) { cruise <- paste(exp[1],exp[2],sep="_")
      } else if(length(exp) ==2) cruise <- exp[1]
      inst <-  sub(".sfl","",exp[length(exp)])
      cruise.id <- seaflow.meta[which(seaflow.meta$cruise == cruise),'Cruise ID']


  df$cruise <- unlist(cruise.id)
  df$inst <- inst
  return(df)
}





# load SFL
list.sfl <- list.files("curated", pattern=".sfl", full.names=T)
sfl <- do.call(rbind, lapply(list.sfl, function(x) read_sfl(x)))

# Bin data by "1 hour"
sfl2 <- sfl %>%
        group_by(cruise, DATE= cut(DATE, breaks="1 hour")) %>%
        summarize(LAT = mean(LAT, na.rm=T), LON = mean(LON, na.rm=T))

# order cruise list chronologically
sfl2 <- sfl2[order(sfl2$DATE),]
sfl2$cruise <- factor(sfl2$cruise, levels = unique(sfl2$cruise))

#plot
p <- plot_geo(sfl2, lat = ~LAT, lon = ~LON, color = ~cruise, colors = viridis_pal(option = "D")(100), alpha=0.5) %>%
  layout(showlegend=T, legend = list(orientation='h'), geo = geo)
p
#save static plot
plotly_IMAGE(p, format = "png", out_file = "cruise-track.png", width = 1000, height = 1000)
