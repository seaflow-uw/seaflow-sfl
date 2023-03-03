#install.packages("googlesheets4")

library(tidyverse)
library(plotly)
library(viridis)
library(googlesheets4)

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
googlesheets4::gs4_deauth()
seaflow.meta <- googlesheets4::read_sheet('https://docs.google.com/spreadsheets/d/1Tsi7OWIZWfCQJqLDpId2aG_i-8Cp-p63PYjjvDkOtH4')



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



#### PLOTTING
df <- sfl %>%
            group_by(LAT=round(LAT,1), LON=round(LON,1), cruise) %>%
            summarise_all(mean)

# order cruise list chronologically
df <- df[order(df$DATE),]
df$cruise <- factor(df$cruise, levels = unique(df$cruise))

#plot
p <- plot_geo(df, lat = ~LAT, lon = ~LON, color = ~cruise, colors = viridis_pal(option = "D")(100), alpha=0.5) %>%
  layout(showlegend=F, legend = list(orientation='h', alpha=1), geo = geo)
p

#save static plot (png)
Sys.setenv("plotly_username" = "ribalet")
Sys.setenv("plotly_api_key" = "svt75uksF9i1jgIljK63")

plotly_IMAGE(p, format = "png", out_file = "cruise-track.png", width = 1000, height = 1000)

#save dynamic plot (html)
htmlwidgets::saveWidget(ggplotly(p), file = "cruise-track.html")





##### ADD BACKGROUND TO IMAGE

library(ncdf4)
path <- "~/Documents/Projects/SF_GRADIENTS/Gradient-1.0/"

file.chl <- list.files(path, pattern="CHL", full.name=T)
nc <- open.nc(file.chl[2])
    dat <- read.nc(nc)
    z <- dat$chlor_a
    z <- dat$chl_oc3
    ylat <-rev(dat$lat)
    xlon <-dat$lon
    z <- z[,length(ylat):1]
    z[which(z > 30)] <- 30
    z[which( z < 0.01)] <- 0.01

plot_geo(df, lat = ~LAT, lon = ~LON, color = ~cruise, colors= "red3", alpha=0.5) %>%
  layout(showlegend=F, legend = list(orientation='h', alpha=1), geo = geo) %>%
  add_trace(data=z, y= ylat, x= xlon)

plot_ly(z = z, type = "surface") %>% 
  add_trace(data = df, x = x, y = y, z = z, mode = "markers", type = "scatter3d", 
            marker = list(size = 5, color = "red", symbol = 104))










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

# number of samples per degree LAT/LON
sfl3 <- sfl %>%
        group_by(LAT=round(LAT), LON=round(LON)) %>%
        summarise(datafiles=length(LAT))

# samples collected
sfl4 <- sfl %>%
  group_by(cruise=cruise) %>%
  summarise(samples=length(cruise))
print(paste(sum(sfl4$samples), " samples collected"))

# number of cruises over time
sfl5 <- sfl %>%
  group_by(cruise) %>%
  summarise(DATE=mean(DATE))
sfl5 <- sfl5[order(sfl5$DATE),]
plot(sfl5$DATE, 1:nrow(sfl5), pch=21, bg='red3', cex=2, type="o", lty=2, ylab="# Cruises", xlab="year")  


# Total number of EVT particles counted
sum(sfl[,c("EVENT RATE")] * 180) * 10^-9

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

