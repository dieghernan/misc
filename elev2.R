library(giscoR)
library(tidyverse)
library(sf)
library(nominatimlite)
library(elevatr)
library(terra)

box <- nominatimlite::bbox_to_poly(c(
  2377294, 1313597, 7453440,
  5628510
),
crs = 3035
)

cntries <- gisco_get_countries(
  year = 2020,
  epsg = 3035,
  resolution = 3,
  region = c("Europe", "Asia", "Africa")
)

elev_rast <- get_elev_raster(box, z=5, verbose = TRUE, neg_to_na = TRUE)


rend <- rast(elev_rast)



warp <- terra::vect(box) %>% terra::rast(
  resolution = 2500
)

f2 <- terra::project(rend, warp,
  filename = "elev.tiff",
  overwrite = TRUE
)



# Cut to shape
cutshape <- cntries %>%
  st_geometry() %>%
  st_crop(box) %>%
  st_union() %>%
  st_transform(st_crs(f2))


f2 <- terra::crop(f2, vect(box))

f2 <- terra::mask(f2, vect(cutshape))

ncell(f2)

pol <- as.polygons(f2)

pol_sf <- st_as_sf(pol) %>% st_make_valid()






# Raised ----

pol_sf_end <- pol_sf


# Get countries
cntries <- cntries %>%
  st_filter(box)


raise <- 50

st_geometry(pol_sf_end) <- "geometry"
names(pol_sf_end) <- c("elev", "geometry")
sf_filtered <- pol_sf_end %>%
  filter(elev > raise) %>%
  mutate(elev = elev - raise)



pplaces <- rnaturalearth::ne_download(type = "populated_places", returnclass = "sf")



colcity <- colorspace::darken("darkgreen", 0.4)


cntryiso <- unique(cntries$ISO3_CODE)


pplaces2 <- pplaces %>%
  filter(ADM0_A3 %in% cntryiso) %>%
  filter(ADM0CAP == 1) %>%
  st_transform(3035)




coltitle <- colorspace::darken(hcl.colors(10, "Terrain")[1], 0.3)
seacol <- colorspace::darken("#C6ECFF", 0.7)
colsub <- seacol
coltitle <- seacol


box <- nominatimlite::bbox_to_poly(c(
  2377294, 1313597, 7453440,
  5628510
),
crs = 3035
)



sysfonts::font_add("noto",
                   regular = "NotoSerif-Regular.ttf",
                   bold = "NotoSerif-Bold.ttf"
)

showtext::showtext_auto()

cntries2 <- gisco_get_countries(
  year = 2020,
  epsg = 3035,
  resolution = 3,
  region = c("Europe", "Asia")
)


elev <- ggplot(sf_filtered) +
#  geom_sf(data=cntries2, fill="white", color="white") +
  geom_sf(aes(fill = elev), color = NA, size = 0.00001) +
  #geom_sf(data = rest, fill = seacol, color = seacol, size = 0.00001) +
  geom_sf(data = cntries, fill = NA, color = "grey90", size = 0.2, alpha=0.8) +
  geom_sf_text(
    data = pplaces2,
    aes(label = "\u2605"),
    color = "white",
    alpha = 0.9, size = 10
  ) +
  coord_sf(
    xlim = c(2377294, 7453440),
    ylim = c(1313597, 5628510),
    expand = FALSE
  ) +
  scale_fill_gradientn(
    colours = hcl.colors(50, "terrain"),
    breaks = c(0, 100, 500, 1000, 2000, 3000)
  ) +
  guides(fill = guide_colorsteps(
    title = "meters over the sea level (after raise)",
    direction = "horizontal",
    title.position = "top",
    show.limits = FALSE,
    barwidth = 50,
    barheight = .8,
    title.hjust = 0,
    label.hjust = .5,
    nrow = 1,
    byrow = T,
    reverse = F,
    label.position = "bottom"
  )) +
  theme_void() +
  theme(
    panel.background = element_rect(fill=seacol),
    legend.position = "bottom",
    text = element_text(family = "noto"),
    plot.title = element_text(
      color = coltitle,
      size = 110,
      hjust = .5,
      margin =margin(b=20, t=10)
    ),
    plot.subtitle = element_text(
      color = colsub,
      hjust = .5,
      size = 60,
      margin = margin(t = 20)
    ),
    plot.caption = element_text(
      size = 50, color = "grey60",
      hjust = 0.5,
      lineheight = .5,
      margin = margin(t = 30, b = 20)
    ),
    legend.title = element_text(color = "grey20", size = 50, 
                                margin = margin(t= 20, b = -20)),
    legend.text = element_text(
      color = "grey20", size = 70,
      margin = margin(t = -20)
    )
  ) +
  labs(
    fill = "",
    y = "",
    x = "",
    title = paste(
      "Europe with the sea level",
      raise, "meters higher"
    ),
    caption = "©2022 Diego Hernangómez https://dieghernan.github.io\nData: elevatr package https://registry.opendata.aws/terrain-tiles/, © EuroGeographics for the administrative boundaries"
  )

ggsave("elevsea.png", elev,
       height = 4200,
       width = 4000,
       dpi = 300,
       units = "px",
       bg = "white"
)
