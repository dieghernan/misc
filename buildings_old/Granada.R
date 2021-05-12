#https://dominicroye.github.io/es/2019/visualizar-el-crecimiento-urbano/


library(feedeR)
library(sf)
library(fs)
library(tidyverse)
library(lubridate)
library(classInt)
library(ggplot2)
library(rvest)
library(tidygeocoder)


url <-
  "http://www.catastro.minhap.es/INSPIRE/buildings/ES.SDGC.bu.atom.xml"

# importamos los RSS con enlaces de provincias
prov_enlaces <- feed.extract(url)
str(prov_enlaces) # estructura es lista

# extraemos la tabla con los enlaces
prov_enlaces_tab <- as_tibble(prov_enlaces$items) %>%
  mutate(title = repair_encoding(title))


# filtramos la provincia y obtenemos la url RSS
val_atom <-
  filter(prov_enlaces_tab, str_detect(title, "Granada")) %>% pull(link)


# importamos la RSS
val_enlaces <- feed.extract(val_atom)

# obtenemos la tabla con los enlaces de descarga
val_enlaces_tab <- val_enlaces$items
val_enlaces_tab <- mutate(val_enlaces_tab,
                          title = repair_encoding(title),
                          link = repair_encoding(link))


# filtramos la tabla con el nombre de la ciudad
val_link <-
  filter(val_enlaces_tab, str_detect(title, "18900")) %>% pull(link)
val_link


# creamos un archivo temporal
temp <- tempfile()

# descargamos los datos
download.file(URLencode(val_link), temp)

# descomprimimos a una carpeta llamda buildings
unzip(temp, exdir = "buildings_old")

# obtenemos la ruta con el archivo
file_val <- dir_ls("buildings_old", regexp = "18900.building.gml")

# importamos los datos
buildings_val <- st_read(file_val)


buildings_val <- mutate(buildings_val,
                        beginning = str_replace(beginning, "^-", "0000") %>%
                          ymd_hms() %>% as_date())

# limitamos al periodo posterior a 10
filter(buildings_val, beginning >= "10-01-01") %>%
  ggplot(aes(beginning)) +
  geom_density(fill = "#2166ac", alpha = 0.7) +
  scale_x_date(date_breaks = "100 year",
               date_labels = "%Y") +
  theme_minimal() +
  theme(
    title = element_text(family = "Montserrat"),
    axis.text = element_text(family = "Montserrat")
  ) +
  labs(y = "", x = "", title = "Evolución del desarrollo urbano")


# map ----

point_bf <- geo("Granada, Spain") %>%
  st_as_sf(coords = c("long", "lat"), crs = st_crs(4326)) %>%
  st_transform(25830) %>%
  st_buffer(1500) %>%
  st_transform(st_crs(buildings_val))

buildings_val25 <- st_intersection(buildings_val, point_bf)

buildings_val25$year <-
  year(buildings_val25$beginning) %>% as.integer()

# encontrar breaks

custom_breaks <- c(0, seq(1880, 2020, 10))

br <-
  classIntervals(buildings_val25$year, 20, "fixed", fixedBreaks = custom_breaks)

br

lab <- names(print(
  br,
  under = "<",
  over = ">",
  cutlabels = FALSE
))
lab



# categorizar el año
buildings_val25 <- mutate(buildings_val25,
                          yr_cl = cut(year, br$brks, labels = lab, include.lowest = TRUE))

# Get levels and add "Sin Dato"
levels <- levels(buildings_val25$yr_cl)


levels[length(levels) + 1] <- "Sin Dato"

#descarga de familia tipográfica
#sysfonts::font_add_google("Montserrat", "Montserrat")

#usar showtext para familias tipográficas
showtext::showtext_auto()

buildings_val25$yr_cl <-
  factor(buildings_val25$yr_cl, levels = levels)
buildings_val25$yr_cl[is.na(buildings_val25$yr_cl)] <- "Sin Dato"

g2 <- ggplot(buildings_val25) +
  geom_sf(aes(fill = yr_cl), col = NA) +
  #geom_sf(data = point_bf, fill = NA, col = "grey20", alpha = .9) +
  scale_fill_manual(
    values = c(hcl.colors(length(lab), palette = "Spectral"), "grey50"),
    guide = guide_legend(keywidth = .5, keyheight = .7),
    na.value = "grey50",
    drop = FALSE
  ) +
  theme_void() +
  labs(title = "Granada",
       fill = "",
       caption = " Code based on Dominic Roye (@dr_xeo) | Data: Catastro") +
  theme(
    plot.title = element_text(hjust = .5, colour = "white",
                              family = "Montserrat",
                              size=100,
                              margin = margin(t = 10)),
    plot.background = element_rect(fill = "black", colour = NA),
    panel.background = element_rect(fill = "black"),
    plot.margin=grid::unit(c(0,0,0,0), "mm"),
    plot.caption = element_text(
      colour = "white",
      hjust = 0.5,
      size=20,
      family = "Montserrat",
      margin = margin(b = 10)
    ),
    legend.text = element_text(
      colour = "white",
      margin = margin(r = 10),
      size = 30,
      family = "Montserrat"
    )
  )


g2

ggsave("./buildings_old/granada.png", g2, width = 7, height = 7)
