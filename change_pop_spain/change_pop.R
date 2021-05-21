library(mapSpain)
library(readxl)
library(sf)
library(tmap)
library(dplyr)

# Descargamos datos INE --------

url <- "https://www.ine.es/pob_xls/pobmun.zip"
tmpzip <- tempfile(fileext = ".zip")
download.file(url, tmpzip)

# Inspeccionamos los archivos que hay dentro
unzip(tmpzip, list = TRUE)

# Extraemos del zip al directorio temporal y cargamos archivos
unzip(tmpzip, exdir = tempdir())

# Cargamos archivos 2000 y 2020

ine2000 <-
  read_excel(file.path(tempdir(), "pobmun00.xls"), skip = 1) %>%
  mutate(LAU_CODE = paste0(CPRO, CMUN),
         pop = POB00) %>%
  select(LAU_CODE, NOMBRE, pop)

ine2020 <-
  read_excel(file.path(tempdir(), "pobmun20.xlsx"), skip = 1) %>%
  mutate(LAU_CODE = paste0(CPRO, CMUN),
         pop = POB20) %>%
  select(LAU_CODE, NOMBRE, pop)


# Mapas 2000--------------------

# Seleccionamos shapefiles y unimos informacion


## Tomamos la fecha mas antigua de municipios disponible, 2005
munic_00 <- esp_get_munic_siane(2005, epsg = 3857)

## Unimos por LAU_CODE, que es cpro y cmun concatenado
munic_00_ine <- munic_00 %>% left_join(ine2000)

# Area

municarea_00 <-
  as.double(st_area(st_transform(munic_00_ine, 3035)) / 1000000)
munic_00_ine$area <- municarea_00

munic_00_ine$dens <- munic_00_ine$pop / munic_00_ine$area

br <-
  c(0,
    10,
    25,
    100,
    200,
    500,
    1000,
    5000,
    10000,
    Inf)


# Usamos una imagen de fondo

mapabase <-
  esp_getTiles(munic_00_ine,
               "IGNBase.Gris",
               verbose = TRUE,
               zoom = 5)

map00 <- tm_shape(mapabase) +
  tm_rgb() +
  tm_shape(munic_00_ine) +
  tm_fill(
    "dens",
    breaks = br,
    alpha = 0.8,
    title = "Pop. per km2",
    palette = "-inferno",
    showNA = FALSE,
    colorNA = "#000004"
  ) +
  tm_layout(main.title = "Year 2000")

map00
# Guardamos
tmap_save(map00, filename = "./change_pop_spain/map00.png")



# Mapas 2020--------------------

# Seleccionamos shapefiles y unimos informacion


munic_20 <- esp_get_munic_siane(2020, epsg = 3857)

## Unimos por LAU_CODE, que es cpro y cmun concatenado
munic_20_ine <- munic_20 %>% left_join(ine2020)

# Area

municarea_20 <-
  as.double(st_area(st_transform(munic_20_ine, 3035)) / 1000000)
munic_20_ine$area <- municarea_20

munic_20_ine$dens <- munic_20_ine$pop / munic_20_ine$area

# Ya lo tenemos de antes
# br <-
#   c(
#     0,
#     10,
#     25,
#     100,
#     200,
#     500,
#     1000,
#     5000,
#     10000,
#     Inf
#   )


# Usamos una imagen de fondo
# Ya la hemos descargado antes
# mapabase <- esp_getTiles(munic_20_ine, "IGNBase.Gris", verbose = TRUE, zoom = 5)

map20 <- tm_shape(mapabase) +
  tm_rgb() +
  tm_shape(munic_20_ine) +
  tm_fill(
    "dens",
    breaks = br,
    alpha = 0.8,
    title = "Pop. per km2",
    palette = "-inferno",
    showNA = FALSE,
    colorNA = "#000004"
  ) +
  tm_layout(main.title = "Year 2020")

map20
# Guardamos
tmap_save(map20, filename = "./change_pop_spain/map20.png")


# Facetas

munic_00_ine$year <- 2000
munic_20_ine$year <- 2020

munic_unif <- bind_rows(munic_00_ine,
                        munic_20_ine)

facet <- tm_shape(munic_unif) +
  tm_fill(
    "dens",
    breaks = br,
    alpha = 0.8,
    title = "Pop. per km2",
    palette = "-inferno",
    showNA = FALSE,
    colorNA = "#000004"
  ) +
  tm_facets(by = "year", ncol = 1)

facet

tmap_save(facet, filename = "./change_pop_spain/facets.png")


# Variaciones ----

ine2000_cross <-
  ine2000 %>% mutate(pop00 = pop) %>% select(LAU_CODE, pop00)

inevars <- ine2020 %>% left_join(ine2000_cross) %>%
  mutate(var = 100 * (pop / pop00 - 1))

# Reemplazo NAs por 1
inevars[is.na(inevars$var), "var"] <- 100

# Pegar al mapa 2020
munic_var <- munic_20 %>% left_join(inevars)

summary(munic_var$var)

br_var <-
  unique(c(seq(-100, 0, 10), seq(0, 100, 25), 500, 1000, 1500, 2000, Inf))

munic_var$cuts <- cut(munic_var$var, br_var, dig.lab = 5)

outline <- esp_get_country(epsg = 3857)


mapvar <- tm_shape(munic_var) +
  tm_fill(
    col = "cuts",
    title = "Ratio of change in pop\n(2000-2020)",
    showNA = FALSE,
    palette = hcl.colors(20, "RdYlBu", rev = TRUE)
  ) +
  tm_shape(outline) +
  tm_borders() +
  tm_layout(
    legend.outside = TRUE,
    frame = FALSE,
    legend.text.size = 0.8,
    legend.position =  c("right", "center")
  )


mapvar

tmap_save(
  mapvar,
  filename = "./change_pop_spain/variations.png",
  asp = 0,
  width = 8,
  height = 4
)

