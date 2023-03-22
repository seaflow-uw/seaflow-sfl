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






#################
### FUN FACTS ###
#################
max_distance_3min <- 14 / (0.53996 * 20) # top speed 14 knots (1 km = 0.53996 knots (nautical mile / h) or ~ 26 km / h, equivalent to 1.3 km / 3 min

sfl_fun <- sfl %>% 
  filter(!is.na(LON)) %>%
  mutate(lat = LAT,
         lon = case_when(LON <= 0 ~ LON + 360,
                         TRUE ~ LON)) %>%
  filter(lon > 100) %>% # Bad GPS coordinates
  arrange(DATE) %>%
  mutate(raw_distance = c(0, geosphere::distHaversine(as.matrix(sfl[,c("lon","lat")]))/1000), # in km
         # to prevent distance to exceed max distance
         distance = case_when(raw_distance > max_distance_3min ~ max_distance_3min, 
                              TRUE ~ raw_distance), 
        total_distance = cumsum(distance)) %>%
  mutate(`EVENT RATE` = case_when(`EVENT RATE` < 3000 ~ median(`EVENT RATE`), 
                                  TRUE ~ `EVENT RATE`),
         particles = `EVENT RATE` * 180,
         total_particles = cumsum(particles) / 10^9,
         total_samples = row_number(),
         total_time = total_samples * 3 / 60)


print(paste(length(unique(sfl_fun$cruise)), "cruises"))
print(paste(round(max(sfl_fun$total_distance)), "km travelled"))
print(paste(max(sfl_fun$total_samples), "samples collected"))
print(paste(round(max(sfl_fun$total_time)), "hours of observations"))
print(paste(round(max(sfl_fun$total_particles)), "x 10^9 particles"))


colourCount = length(unique(sfl_fun$cruise))
getPalette = colorRampPalette(rev(RColorBrewer::brewer.pal(9, "Set1")))

a <- sfl_fun %>% ggplot() +
  geom_point(aes(DATE, total_distance, col = cruise)) +
  theme_bw() +
  scale_color_manual(values = getPalette(colourCount)) +
  ylab("Distance travelled (km)") +
  xlab("time")  

b <- sfl_fun %>% ggplot() +
  geom_point(aes(DATE, total_samples / 1000, col = cruise)) +
  theme_bw() +
  scale_color_manual(values = getPalette(colourCount)) +
  ylab(expression(paste("Files collected (x 10"^{3},")"))) + 
  xlab("time")  

c <- sfl_fun %>% ggplot() +
  geom_point(aes(DATE, total_time, col = cruise)) +
  theme_bw() +
  scale_color_manual(values = getPalette(colourCount)) +
  ylab("Hours of Observations (h)") +
  xlab("time")  

d <- sfl_fun %>% ggplot() +
  geom_point(aes(DATE, total_particles, col = cruise)) +
  theme_bw() +
  scale_color_manual(values = getPalette(colourCount)) +
  ylab(expression(paste("Particles measured (x 10"^{9},")"))) + 
  xlab("time")  

png("sfl_funfacts.png",  width = 4000, height = 2000, res = 300)
ggpubr::ggarrange(a, c, b ,d, ncol = 2, nrow = 2, common.legend = TRUE, legend="right")
dev.off()

