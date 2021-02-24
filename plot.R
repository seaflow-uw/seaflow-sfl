#install.packages("googlesheets4")

library(tidyverse)
library(plotly)
library(viridis)
library(googlesheets4)
options(browser="/usr/bin/firefox")

setwd("~/Documents/Codes/seaflow-sfl/")

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
seaflow.meta <- read_sheet("https://docs.google.com/spreadsheets/d/1Tsi7OWIZWfCQJqLDpId2aG_i-8Cp-p63PYjjvDkOtH4/edit?usp=sharing")



read_sfl <- function(x){
  df <- read_delim(x, delim="\t")

    #parse cruise name and serial number of instrument
    exp <- unlist(list(strsplit(sub(".sfl", "", basename(x)),"_")))

      if(length(exp) > 2) { cruise <- paste(exp[1],exp[2],sep="_")
      } else if(length(exp) ==2) cruise <- exp[1]
      print(cruise)
      inst <-  sub(".sfl","",exp[length(exp)])
      cruise.id <- seaflow.meta[which(seaflow.meta$cruise == cruise),'Cruise ID']

  df$cruise <- unlist(cruise.id)
  df$inst <- inst
  return(df)
}




################
### load SFL ###
################
list.sfl <- list.files("curated", pattern=".sfl", full.names=T)
sfl <- do.call(rbind, lapply(list.sfl, function(x) read_sfl(x)))


#################
### FUN FACTS ###
#################
# Number of cruises
print(paste(length(unique(sfl$cruise)), "cruises"))

# Number of data files
print(paste(length(unique(sfl$DATE)), "data files collected"))

# Hours of observations
sfl$PAR <- as.numeric(sfl$PAR)
df <- sfl %>%
            group_by(cruise, DATE= cut(DATE, breaks="1 hour")) %>%
            summarise_all(mean)

print(paste(length(unique(df$DATE)), "hours of observations"))

write_csv(df[,c("DATE","LAT","LON","PAR")], "~/Desktop/SeaFlow_coordinates.csv")

# Distance covered
library(geosphere)
    D <- NULL
    for(i in 2:nrow(sfl)){
    message(round(100*i/nrow(sfl)), "% completed \r", appendLF=FALSE)
    d <- distm(sfl[(i-1):i,c("LON","LAT")], fun = distHaversine)[1,2]
    D <- c(D, d)
    flush.console()
    }
    # Sum distance along track
    id <- which(20*D*0.00053996 > 20) # 1 m = 0.00053996 knots (nautical mile / h)
    print(paste(round(sum(D[-id]/1000, na.rm=T)), "km covered"))

    # Speed of ship while underway
    id2 <- which(20*D*0.00053996 > 20 | D/1000 < 0.2)
    print(paste(round(mean(D[-id2]/1000, na.rm=T),1), "km covered in 3 minutes"))
    print(paste(round(sum(D[-id2]/1000, na.rm=T)), "km covered while underway"))
    print(paste(round(mean(20*D[-id2]*0.00053996, na.rm=T),1), "knots on average"))


# number of samples per degree LAT/LON
sfl3 <- sfl %>%
        group_by(LAT=round(LAT), LON=round(LON)) %>%
        summarise(datafiles=length(LAT))

# files collected per cruise
sfl4 <- sfl %>%
  group_by(cruise=cruise) %>%
  summarise(samples=length(cruise))
sum(sfl4$samples)

# number of cruises over time
sfl5 <- sfl %>%
  group_by(cruise) %>%
  summarise(DATE=mean(DATE))
sfl5 <- sfl5[order(sfl5$DATE),]
plot(sfl5$DATE, 1:nrow(sfl5), pch=21, bg='red3', cex=2, type="o", lty=2, ylab="# Cruises", xlab="year")  


## CHANGE POINT detection (Corinne's paper)

sfl2 <- sfl %>% filter(cruise == "DeepDOM" | 
              cruise == "KM1712" | 
              cruise == "KM1713" | 
              cruise == "MGL1704" | 
              cruise == "SCOPE_16" | 
              cruise == "SCOPE_2" | 
              cruise == "Thompson_12" | 
              cruise == "Thompson_1" | 
              cruise == "Thompson_9" |
              cruise == "Tokyo_1" | 
              cruise == "Tokyo_2" | 
              cruise == "Tokyo_3") %>%
              filter(SALINITY < 50 & SALINITY > 20) %>%
              ggplot(aes(SALINITY)) + 
              geom_histogram(na.rm=T)



#### PLOTTING
df <- sfl %>%
            group_by(LAT=round(LAT,1), LON=round(LON,1), cruise) %>%
            summarise_all(mean)

# order cruise list chronologically
df <- df[order(df$DATE),]
df$cruise <- factor(df$cruise, levels = unique(df$cruise))

#plot
p <- plot_geo(df, lat = ~LAT, lon = ~LON, color = ~cruise, colors = viridis_pal(option = "D")(100), alpha=0.5) %>%
  layout(showlegend=T, legend = list(orientation='h', alpha=1), geo = geo)
p

#save static plot (png)
Sys.setenv("plotly_username" = "ribalet")
Sys.setenv("plotly_api_key" = "svt75uksF9i1jgIljK63")

plotly_IMAGE(p, format = "png", out_file = "cruise-track.png", width = 1000, height = 1000)

#save dynamic plot (html)
htmlwidgets::saveWidget(ggplotly(p), file = "cruise-track.html")









