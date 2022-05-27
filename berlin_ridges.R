library(nominatimlite)
library(elevatr)
library(terra)
library(sf)
library(dplyr)

berlin <- geo_lite("Berlin, Germany")

berlin_sf <- reverse_geo_lite_sf(berlin$lat, berlin$lon, points_only = FALSE,
                                 full_results = TRUE,
                                 custom_query = list(zoom = 5)) 

plot(st_geometry(berlin_sf %>% st_buffer(2000)))
plot(st_geometry(berlin_sf), add=TRUE)

rast_berlin <- get_elev_raster(berlin_sf, z = 7) %>% 
  rast() %>% 
  crop(vect(st_buffer(berlin_sf, 1000))) %>% 
  mask(vect(berlin_sf))


plot(rast_berlin)

rast_berlin[is.na(rast_berlin)] <- 0



plot(rast_berlin)

nrow(rast_berlin)



library(ggplot2)
library(ggridges)

ggplot(berlin_sf) +
  geom_sf()



df <- as.data.frame(rast_berlin, xy=TRUE)
df$val <- df[ , 3]

# Add colors
ff <- quantile(df$y, c(.25, .75))

d <- df %>% mutate(col = ifelse(y < ff[1] | y > ff[2], "B", "A"))



berlin_ridges <- ggplot(d) +
  # geom_sf(data=berlin_sf, fill=NA, color=NA) +
  geom_density_ridges(aes(x=x, y=y, group=y, height=val, color=col),
                      stat = "identity", scale=8, fill="black",
                      size=0.75) +
   coord_sf(crs=4326) +
  theme_void() +
  theme(plot.background = element_rect(fill="black"),
        plot.margin = margin(t=40, b=40, l=20, r=20)) 

berlin_ridges
ggsave("berlin.png", berlin_ridges, width = 4200, height = 4200, units = "px", dpi=300)
