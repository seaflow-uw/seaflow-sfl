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
      print(cruise)

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
        summarise(LAT = mean(LAT, na.rm=T), LON = mean(LON, na.rm=T))

# order cruise list chronologically
sfl2 <- sfl2[order(sfl2$DATE),]
sfl2$cruise <- factor(sfl2$cruise, levels = unique(sfl2$cruise))

#plot
p <- plot_geo(sfl2, lat = ~LAT, lon = ~LON, color = ~cruise, colors = viridis_pal(option = "D")(100), alpha=0.5) %>%
  layout(showlegend=T, legend = list(orientation='h', alpha=1), geo = geo)
p
#save static plot (png)
plotly_IMAGE(p, format = "png", out_file = "cruise-track.png", width = 1000, height = 1000)

#save dynamic plot (html)
htmlwidgets::saveWidget(ggplotly(p), file = "cruise-track.html")












### DATA shared on Zenodo (as published in ScientificData)


sfl2 <- subset(sfl, cruise == "TN248" |
                    cruise == "CN11ID" |
                    cruise == "TN271" |
                    cruise == "TN280" |
                    cruise == "CN12ID" |
                    cruise == "TN292" |
                    cruise == "CN13ID" |
                    cruise == "KM1427" |
                    cruise == "KM1427" |
                    cruise == "KM1502" |
                    cruise == "KM1508" |
                    cruise == "KM1510" |
                    cruise == "KM1512" |
                    cruise == "KOK1512" |
                    cruise == "KOK1515" |
                    cruise == "KM1518" |
                    cruise == "KM1601" |
                    cruise == "KM1602" |
                    cruise == "KM1603" |
                    cruise == "KOK1604" |
                    cruise == "HOE-Legacy 4" |
                    cruise == "KOK1608" |
                    cruise == "KOK1609" |
                    cruise == "KM1708" |
                    cruise == "KM1709" |
                    cruise == "KOK1806"
                  )


# Number of data file
print(paste(nrow(sfl2), "data files collected"))

# Distance covered
library(geosphere)

D <- NULL
for(i in 2:nrow(sfl2)){
message(round(100*i/nrow(sfl2)), "% completed \r", appendLF=FALSE)
d <- distm(sfl2[(i-1):i,c("LON","LAT")], fun = distHaversine)[1,2]
D <- c(D, d)
flush.console()
}

# Sum distance along track
id <- which(20*D*0.00053996 > 20) # 1 m = 0.00053996 knots (nautical mile / h)
print(paste(round(sum(D[-id]/1000, na.rm=T)), "km covered"))
hist(D[-id]/1000)

# Speed of ship while underway
id2 <- which(20*D*0.00053996 > 20 | D/1000 < 0.2)
print(paste(round(mean(D[-id2]/1000, na.rm=T),1), "km covered in 3 minutes"))
print(paste(round(sum(D[-id2]/1000, na.rm=T)), "km covered while underway"))
print(paste(round(mean(20*D[-id2]*0.00053996, na.rm=T),1), "knots on average"))


# files collected per cruise
sfl4 <- sfl2 %>%
        group_by(cruise=cruise) %>%
        summarise(samples=length(cruise))
sum(sfl4$samples)

# number of samples per degree LAT/LON
sfl3 <- sfl2 %>%
        group_by(LAT=round(LAT), LON=round(LON)) %>%
        summarise(datafiles=length(LAT))

      p <- sfl3 %>%
          ggplot() + geom_point(ggplot2::aes_string('LON', 'LAT', col='datafiles'), size=2, pch=15, alpha=1,show.legend=T) +
          borders('world', fill = 'gray80') +
          labs(x='Longitude (E)', y= 'Latitude (N)') +
          coord_fixed(ratio = 1, xlim = c(-170,-110), ylim=c(10,60)) +
          scale_color_gradientn(colours=viridis::viridis(100), trans='log10') +
          theme_bw() +
          geom_point(aes(x=-158, y=23), col='red3',size=2, pch=0)
      p

      ggsave("Figure1.png", width=6, height=6, unit='in', dpi=300)
