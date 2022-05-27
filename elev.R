library(giscoR)
library(tidyverse)
library(sf)


# munis <- gisco_get_grid(resolution = 10)
#
# # Make chunks!
# cnt_ids <- unique(munis$CNTR_ID) %>%
#   paste(collapse = "-") %>% strsplit("-") %>%
#   unlist() %>% unique()
#
#
# chunk_cnt <- gisco_get_countries(year = 2020,
#                                  epsg = 3035,
#                                  country = cnt_ids,
#                                  resolution = 10
#                                  )
#
# ct <- "AT"
#
# for (ct in cnt_ids){
#   cntry <- chunk_cnt %>% filter(CNTR_ID == ct)
#
#   grid_chunk <- st_intersection(munis, cntry)
#
#   grid_chunk$CNTR_ID_CHUNK <- ct
#   grid_chunk <- st_make_valid(grid_chunk) %>%
#     select(-FID)
#   filename <- paste0(ct, ".gpkg")
#
#   st_write(grid_chunk, file.path("gridchunk", filename), append = FALSE)
#
#
# }
#


# Read Chunks

f <- list.files("gridchunk",
  pattern = ".gpkg$",
  full.names = TRUE
)

all <- lapply(f, st_read, quiet = TRUE)
all <- bind_rows(all)



# Get countries
cntries <- gisco_get_countries(
  year = 2020,
  epsg = 3035,
  resolution = 3,
) %>%
  filter(CNTR_ID %in% unique(all$CNTR_ID_CHUNK))

# Download DEMs
#
# cnt <- cntries %>% st_drop_geometry() %>%
#   mutate(iso = countrycode::countrycode(CNTR_ID,
#                                         "eurostat",
#                                         "iso3c")) %>%
#   pull(iso) %>% sort()
#
#
#
# for (ct in cnt){
#
#
#     try(p <- raster::getData("alt", country = ct,
#                          path="dem2", mask=FALSE))
#
# }
#



# allrast <- list.files("dem2", pattern = "grd$", full.names = TRUE)
# 
# rend <- raster::raster(allrast[1])
# rend <- terra::rast(rend)
# names(rend) <- "elev"
# 
# rest <- allrast[-1]
# 
# 
# for (r in rest) {
#   message(r)
# 
#   r1 <- raster::raster(r)
#   r1 <- terra::rast(r1)
# 
#   rend <- terra::merge(rend, r1)
# }
# 
# 
# # Warp
# library(terra)
# 
# box <- nominatimlite::bbox_to_poly(c(
#   2377294, 1313597, 7453440,
#   5628510
# ),
# crs = 3035
# )
# 
# warp <- terra::vect(box) %>% terra::rast(
#   resolution = c(2500, 2500)
# )
# 
# 
# ncell(warp)
# 
# 
# f2 <- terra::project(rend, warp,
#   method = "min",
#   filename = "elev.tiff",
#   overwrite = TRUE
# )
# 
# 
# 
# # To pols and sf
# pol <- as.polygons(f2)
# 
# 
# writeVector(pol, "elev.gpkg", overwrite = TRUE)


# Read on sf
pol_sf <- st_read("elev.gpkg") %>% st_make_valid()


# Cut to shape
cutshape <- cntries %>%
  st_geometry() %>%
  st_union()
pol_sf_end <- st_intersection(
  pol_sf,
  cutshape
)



# Raised ----

raise <- 10

sf_filtered <- pol_sf_end %>%
  filter(elev > raise) %>%
  mutate(elev = elev - raise)

rest <- pol_sf_end %>%
  filter(elev <= raise) %>%
  st_geometry() %>%
  st_union()


pplaces <- rnaturalearth::ne_download(type = "populated_places", returnclass = "sf")


names(pplaces)


colcity <- colorspace::darken("darkgreen", 0.4)

scales::show_col(colcity)

cntryiso <- unique(cntries$ISO3_CODE)


pplaces2 <- pplaces %>%
  filter(ADM0_A3 %in% cntryiso) %>%
  filter(ADM0CAP == 1) %>%
  st_transform(3035)


#
# ggplot(sf_filtered, aes(x=elev)) +
#   geom_histogram()


# ggplot(sf_filtered, aes(x=elev)) +
#   geom_histogram()



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
  geom_sf(data=cntries2, fill="white", color="white") +
  geom_sf(aes(fill = elev), color = NA, size = 0.00001) +
  geom_sf(data = rest, fill = seacol, color = seacol, size = 0.00001) +
  geom_sf(data = cntries, fill = NA, color = "grey90", size = 0.2) +
  geom_sf_text(
    data = pplaces2,
    aes(label = "\u2605"),
    color = "red",
    alpha = 0.9, size = 15
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
      margin =margin(b=20)
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
    caption = "©2022 Diego Hernangómez https://dieghernan.github.io\nData: DIVA-GIS https://diva-gis.org/gdata, © EuroGeographics for the administrative boundaries"
  )

ggsave("elevsea.png", elev,
  height = 4200,
  width = 4000,
  dpi = 300,
  units = "px"
)
