#*** Spatial approach **
#*** Matías Carrasco Jiménez **

# Preamble --------------------------------------------------------------------

remove(list = ls())
set.seed(123)

if (!require(pacman)) install.packages("pacman") 
p_load("sf", "dplyr", "readr", "paletteer", "ggthemes", "ggplot2", "tidyr",
       "scales", "gstat", "stars", "readxl", "spdep", "stringr", "purrr"
)

MUNICIPIOS_SHP <- "inputs/spatial_data/mun/mun.shp"

mun <- read_sf(MUNICIPIOS_SHP)
cat(sprintf("  Municipalities : %s polygons\n", format(nrow(mun), big.mark = ",")))
cat(sprintf("  CRS            : %s\n", st_crs(mun)$input))

# I: Average temperature ----

## Loading ----

TEMPERATURA_SHP <- "inputs/spatial_data/temp/temp.shp"

temp <- read_sf(TEMPERATURA_SHP)
colnames(temp) <- c("temp", "FC", "geometry")

# Assign CRS manually — same projection as 00mun.shp (MEXICO_ITRF_2008_LCC)
# temp.shp has no .prj file but was produced by INEGI in the same projection
temp <- st_set_crs(temp, st_crs(mun))

# Filter non-temperature values (H2O = water bodies, P/E = precip/evap zones)
temp <- temp %>%
  filter(!temp %in% c("H2O", "P/E")) %>%
  mutate(temp = as.numeric(temp))

cat(sprintf("  Isolines       : %s (after filtering H2O and P/E)\n",
            format(nrow(temp), big.mark = ",")))
cat(sprintf("  Temp range     : %d–%d °C\n", min(temp$temp), max(temp$temp)))

## Spatial intersection ----

intersection <- st_intersection(
  temp %>% dplyr::select(temp),
  mun  %>% dplyr::select(CVEGEO, CVE_ENT, CVE_MUN, NOMGEO)
)

intersection <- intersection %>%
  mutate(seg_length_m = as.numeric(st_length(geometry)))

cat(sprintf("  Intersection segments: %s\n", format(nrow(intersection), big.mark = ",")))

temp_municipios <- intersection %>%
  st_drop_geometry() %>%
  group_by(CVEGEO, CVE_ENT, CVE_MUN, NOMGEO) %>%
  summarise(
    avg_temp     = weighted.mean(temp, w = seg_length_m, na.rm = TRUE),
    temp_min_C   = min(temp, na.rm = TRUE),
    temp_max_C   = max(temp, na.rm = TRUE),
    n_isolines   = n(),
    total_isoline_length_m = sum(seg_length_m, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(ubica_geo = CVEGEO)

cat(sprintf("  Municipalities with temperature : %s\n",
            format(nrow(temp_municipios), big.mark = ",")))

## Nearest-isoline fallback  ----

n_missing <- nrow(mun) - nrow(temp_municipios)
cat(sprintf("  Municipalities with no isoline  : %d", n_missing))

if (n_missing > 0) {
  cat("\n  Running nearest-isoline fallback...\n")
  
  missing_mun <- mun %>%
    filter(!CVEGEO %in% temp_municipios$ubica_geo)
  
  nearest_idx  <- st_nearest_feature(st_centroid(missing_mun), temp)
  nearest_temp <- temp$temp[nearest_idx]
  
  fallback <- missing_mun |>
    st_drop_geometry() |>
    mutate(
      ubica_geo              = CVEGEO,
      avg_temp     = nearest_temp,
      temp_min_C             = nearest_temp,
      temp_max_C             = nearest_temp,
      n_isolines             = 0L,
      total_isoline_length_m = 0
    ) |>
    select(ubica_geo, CVE_ENT, CVE_MUN, NOMGEO,
           avg_temp, temp_min_C, temp_max_C,
           n_isolines, total_isoline_length_m)
  
  temp_mun <- bind_rows(temp_municipios, fallback) %>%
    dplyr::select(ubica_geo, avg_temp)
  cat(sprintf("  Fallback applied to %d municipalities\n", nrow(fallback)))
} else {
  temp_mun <- temp_municipios %>%
    dplyr::select(ubica_geo, avg_temp)
}

## Plots ----

temp_map <- ggplot() +
  geom_sf(data = mun, fill = "gray95", color = "gray80", linewidth = 0.05) +
  geom_sf(data = temp, aes(color = temp), linewidth = 0.4) +
  scale_color_gradientn(
    name = "",
    colors = paletteer_c("grDevices::Blue-Red 3", 30),
    limits = c(min(temp$temp), max(temp$temp, na.rm = TRUE)),
    breaks = seq(min(temp$temp), max(temp$temp, na.rm = TRUE), 
                 by = (max(temp$temp, na.rm = TRUE)-min(temp$temp, na.rm = TRUE))/4),
    labels = c("2°C", "9°C", "16°C", "23°C", "30°C"),
    oob    = squish)+
  theme_void(base_size = 12)+
  theme(legend.position = "bottom",
        legend.key.width = unit(3, "cm"),
        legend.text = element_text(size = 17))

ggsave("outputs/plots/temp1.jpeg", plot = temp_map, height = 6, width = 8.5, dpi = 300)

mun_plot1 <- mun %>%
  left_join(temp_mun %>% select(ubica_geo, avg_temp),
            by = c("CVEGEO" = "ubica_geo"))

map <- ggplot(mun_plot1) +
  geom_sf(aes(fill = avg_temp), color = NA) +
  scale_fill_gradientn(
    name = "",
    colors = paletteer_c("grDevices::Blue-Red 3", 30),
    limits = c(min(mun_plot1$avg_temp), max(mun_plot1$avg_temp, na.rm = TRUE)),
    breaks = seq(min(mun_plot1$avg_temp), max(mun_plot1$avg_temp, na.rm = TRUE), 
                 by = (max(mun_plot1$avg_temp, na.rm = TRUE)-min(mun_plot1$avg_temp, na.rm = TRUE))/4),
    labels = c("8°C", "13°C", "17°C", "23°C", "30°C"),
    oob    = squish
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.caption      = element_text(hjust = 0.5, size = 7, color = "black"),
    legend.position   = "bottom",
    legend.text = element_text(size = 17),
    legend.key.width = unit(3, "cm"),
    plot.background   = element_rect(fill = "white", color = NA),
    plot.margin       = margin(10, 10, 10, 10)
  )

ggsave("outputs/plots/temp2.jpeg", plot = map, height = 6, width = 8.5, dpi = 300)

rm(list = setdiff(ls(), c("mun", "temp_mun")))

# II: Average max-min temperature ----

## Meteorological stations  ----

est_SHP <- "inputs/spatial_data/temp/est08.shp"

est <- read_sf(est_SHP) %>%
  st_transform(st_crs(mun)) %>%
  mutate(NR_CLIM = case_when(
    NR_CLIM == "Centro" ~ "C",
    NR_CLIM == "Centro Sur" ~ "CS",
    NR_CLIM == "Golfo de California" ~ "GC",
    NR_CLIM == "Golfo de M\xe9xico" ~ "GM",
    NR_CLIM == "Noreste" ~ "NE",
    NR_CLIM == "Noroeste" ~ "NW",
    NR_CLIM == "Norte" ~ "No",
    NR_CLIM == "Pac\xedfico Central" ~ "CP",
    NR_CLIM == "Pac\xedfico Sur" ~ "SP",
    NR_CLIM == "Pen\xednsula de Yucat\xe1n" ~ "YP",
    NR_CLIM == "Sureste" ~ "SE"
  ))

stations <- ggplot() +
  geom_sf(data = mun, fill = "gray95", color = "gray80", linewidth = 0.05) +
  geom_sf(data = est, aes(color = NR_CLIM), size = 1, shape = 17) +
  scale_color_manual(values = c(
    "C" = "#1b9e77",
    "CS" = "#d95f02",
    "GM" = "black",
    "GC" = "#e7298a",
    "NE" = "#66a61e",
    "NW" = "#e6ab02",
    "No" = "#a6761d",
    "CP" = "#666666",
    "SP" = "red2",
    "YP" = "purple4",
    "SE" = "#7570b3"
  )) +
  theme_void(base_size = 12)+
  theme(legend.position = "bottom",
        legend.text = element_text(size = 17),
        legend.title = element_blank())+
  guides(color = guide_legend(nrow = 2, byrow = TRUE, override.aes = list(size = 2)))

ggsave("outputs/plots/stations.jpeg",
       plot = stations, height = 6, width = 8, dpi = 300)

## Loading ----

TMAX_SHP <- "inputs/spatial_data/temp/temp_max.shp"
TMIN_SHP <- "inputs/spatial_data/temp/temp_min.shp"

tmax_sta <- read_sf(TMAX_SHP) %>%
  st_transform(st_crs(mun))   # same projection as municipal shapefile

tmin_sta <- read_sf(TMIN_SHP) %>%
  st_transform(st_crs(mun))

cat(sprintf("  Stations loaded: %d\n", nrow(tmax_sta)))

# Monthly column names (months 1-12 only)
tmax_cols <- c(paste0("TMAXMEAP_", 1:9), paste0("TMAXMEAP", 10:12))
tmin_cols <- c(paste0("TMINMEAP_", 1:9), paste0("TMINMEAP", 10:12))

# ── 2. Compute station-level annual summaries ──────────────────

tmax_sta <- tmax_sta %>%
  mutate(
    tmax_warmest = pmax(!!!syms(tmax_cols), na.rm = TRUE),  # warmest month max
    tmax_annual  = rowMeans(across(all_of(tmax_cols)), na.rm = TRUE)
  )

tmin_sta <- tmin_sta %>%
  mutate(
    tmin_coldest = pmin(!!!syms(tmin_cols), na.rm = TRUE),  # coldest month min
    tmin_annual  = rowMeans(across(all_of(tmin_cols)), na.rm = TRUE)
  )

cat(sprintf("  tmax_warmest range: %.1f -- %.1f °C\n",
            min(tmax_sta$tmax_warmest, na.rm=TRUE),
            max(tmax_sta$tmax_warmest, na.rm=TRUE)))
cat(sprintf("  tmin_coldest range: %.1f -- %.1f °C\n",
            min(tmin_sta$tmin_coldest, na.rm=TRUE),
            max(tmin_sta$tmin_coldest, na.rm=TRUE)))

# ── 3. IDW interpolation to municipality centroids ────────────

mun_centroids <- mun %>%
  st_centroid() %>%
  select(CVEGEO)

idw_interpolate <- function(stations, value_col, centroids, nmax = 10, idp = 2) {
  
  sta_clean <- stations %>%
    filter(!is.na(.data[[value_col]])) %>%
    select(all_of(value_col))
  
  model <- gstat(
    formula = as.formula(paste(value_col, "~ 1")),
    data    = sta_clean,
    nmax    = nmax,
    set     = list(idp = idp)
  )
  
  pred <- predict(model, newdata = centroids) %>%
    st_drop_geometry() %>%
    transmute(
      ubica_geo = centroids$CVEGEO,
      !!value_col := var1.pred
    )
  
  return(pred)
}
cat("  Running IDW for tmax_warmest...\n")
pred_tmax_warmest <- idw_interpolate(tmax_sta, "tmax_warmest", mun_centroids)

cat("  Running IDW for tmax_annual...\n")
pred_tmax_annual  <- idw_interpolate(tmax_sta, "tmax_annual",  mun_centroids)

cat("  Running IDW for tmin_coldest...\n")
pred_tmin_coldest <- idw_interpolate(tmin_sta, "tmin_coldest", mun_centroids)

cat("  Running IDW for tmin_annual...\n")
pred_tmin_annual  <- idw_interpolate(tmin_sta, "tmin_annual",  mun_centroids)

## Spatial Intersection ----

temp_station_mun <- pred_tmax_warmest %>%
  left_join(pred_tmax_annual,  by = "ubica_geo") %>%
  left_join(pred_tmin_coldest, by = "ubica_geo") %>%
  left_join(pred_tmin_annual,  by = "ubica_geo")

## Plots ----

mun_plot_tmax <- mun %>%
  left_join(temp_station_mun %>% select(ubica_geo, tmax_warmest),
            by = c("CVEGEO" = "ubica_geo"))

map_tmax <- ggplot(mun_plot_tmax) +
  geom_sf(aes(fill = tmax_warmest), color = NA) +
  scale_fill_gradientn(
    colors = paletteer_c("grDevices::Blue-Red 3", 30),
    limits = c(min(mun_plot_tmax$tmax_warmest), max(mun_plot_tmax$tmax_warmest, na.rm = TRUE)),
    breaks = seq(min(mun_plot_tmax$tmax_warmest), max(mun_plot_tmax$tmax_warmest, na.rm = TRUE), 
                 by = (max(mun_plot_tmax$tmax_warmest, na.rm = TRUE)-min(mun_plot_tmax$tmax_warmest, na.rm = TRUE))/4),
    labels = c("22°C", "26°C", "31°C", "35°C", "42°C"),
    oob    = squish,
    name   = ""
  ) +
  theme_void(base_size = 12)+
  theme(legend.position = "bottom",
        legend.key.width = unit(3, "cm"),
        legend.text = element_text(size = 17))

ggsave("outputs/plots/tmax_warmest.jpeg",
       plot = map_tmax, height = 6, width = 8, dpi = 300)

mun_plot_tmin <- mun %>%
  left_join(temp_station_mun %>% select(ubica_geo, tmin_coldest),
            by = c("CVEGEO" = "ubica_geo"))

map_tmin <- ggplot(mun_plot_tmin) +
  geom_sf(aes(fill = tmin_coldest), color = NA) +
  scale_fill_gradientn(
    colors = rev(paletteer_c("grDevices::Blue-Red 3", 30, direction = -1)),
    limits = c(min(mun_plot_tmin$tmin_coldest), max(mun_plot_tmin$tmin_coldest, na.rm = TRUE)),
    breaks = seq(min(mun_plot_tmin$tmin_coldest), max(mun_plot_tmin$tmin_coldest, na.rm = TRUE), 
                 by = (max(mun_plot_tmin$tmin_coldest, na.rm = TRUE)-min(mun_plot_tmin$tmin_coldest, na.rm = TRUE))/4),
    labels = c("-5°C", "1°C", "8°C", "14°C", "21°C"),
    oob    = squish,
    name   = ""
  ) +
  theme_void(base_size = 12)+
  theme(legend.position = "bottom",
        legend.key.width = unit(3, "cm"),
        legend.text = element_text(size = 17))

ggsave("outputs/plots/tmin_coldest.jpeg",
       plot = map_tmin, height = 6, width = 8, dpi = 300)

mun_plot_tmax2 <- mun %>%
  left_join(temp_station_mun %>% select(ubica_geo, tmax_annual),
            by = c("CVEGEO" = "ubica_geo"))

map_tma2 <- ggplot(mun_plot_tmax2) +
  geom_sf(aes(fill = tmax_annual), color = NA) +
  scale_fill_gradientn(
    colors = paletteer_c("grDevices::Blue-Red 3", 30),
    limits = c(min(mun_plot_tmax2$tmax_annual), max(mun_plot_tmax2$tmax_annual, na.rm = TRUE)),
    breaks = seq(min(mun_plot_tmax2$tmax_annual), max(mun_plot_tmax2$tmax_annual, na.rm = TRUE), 
                 by = (max(mun_plot_tmax2$tmax_annual, na.rm = TRUE)-min(mun_plot_tmax2$tmax_annual, na.rm = TRUE))/4),
    labels = c("19°C", "23°C", "27°C", "31°C", "35°C"),
    oob    = squish,
    name   = ""
  ) +
  theme_void(base_size = 12)+
  theme(legend.position = "bottom",
        legend.key.width = unit(3, "cm"),
        legend.text = element_text(size = 17))

ggsave("outputs/plots/tmax_annual.jpeg",
       plot = map_tma2, height = 6, width = 8, dpi = 300)

mun_plot_tmin2 <- mun %>%
  left_join(temp_station_mun %>% select(ubica_geo, tmin_annual),
            by = c("CVEGEO" = "ubica_geo"))

map_tmin2 <- ggplot(mun_plot_tmin2) +
  geom_sf(aes(fill = tmin_annual), color = NA) +
  scale_fill_gradientn(
    colors = rev(paletteer_c("grDevices::Blue-Red 3", 30, direction = -1)),
    limits = c(min(mun_plot_tmin2$tmin_annual), max(mun_plot_tmin2$tmin_annual, na.rm = TRUE)),
    breaks = seq(min(mun_plot_tmin2$tmin_annual), max(mun_plot_tmin2$tmin_annual, na.rm = TRUE), 
                 by = (max(mun_plot_tmin2$tmin_annual, na.rm = TRUE)-min(mun_plot_tmin2$tmin_annual, na.rm = TRUE))/4),
    labels = c("2°C", "7°C", "12°C", "17°C", "22°C"),
    oob    = squish,
    name   = ""
  ) +
  theme_void(base_size = 12)+
  theme(legend.position = "bottom",
        legend.key.width = unit(3, "cm"),
        legend.text = element_text(size = 17))

ggsave("outputs/plots/tmin_annual.jpeg",
       plot = map_tmin2, height = 6, width = 8, dpi = 300)

## Station-level warm/cold day counts (supporting maps) ----
# Folded in from the former standalone stations.R (August 2026), which could not
# run on its own because it depended on `mun` and on this script's loaded
# packages. Plots the raw station observations of SU25_P (summer days above
# 25 C) and FDO_P (frost days) from annual.shp. These two figures are NOT
# currently cited in essay_I.tex; they are retained as supporting evidence for
# the climate-zone thresholds. Set MAKE_STATION_DAY_MAPS <- FALSE to skip.

MAKE_STATION_DAY_MAPS <- TRUE

if (MAKE_STATION_DAY_MAPS) {

  annual_SHP <- "inputs/spatial_data/temp/annual.shp"
  annual <- read_sf(annual_SHP) %>% st_transform(st_crs(mun))

  day_count_map <- function(var, palette, labels) {
    v <- annual[[var]]
    rng <- range(v, na.rm = TRUE)
    ggplot(annual) +
      geom_sf(data = mun, fill = "gray95", color = "gray80", linewidth = 0.05) +
      geom_sf(aes(color = .data[[var]]), size = 1) +
      scale_color_gradientn(
        colors = paletteer_c(palette, 30, direction = -1),
        limits = rng,
        breaks = seq(rng[1], rng[2], by = diff(rng) / 4),
        labels = labels,
        oob    = squish,
        name   = ""
      ) +
      theme_void(base_size = 12) +
      theme(legend.position  = "bottom",
            legend.key.width = unit(3, "cm"),
            legend.text      = element_text(size = 17))
  }

  warm_days <- day_count_map("SU25_P", "grDevices::Reds 3",
                             c("<1", "90", "180", "270", "365"))
  ggsave("outputs/plots/warm_days_st.jpeg",
         plot = warm_days, height = 6, width = 8, dpi = 300)

  cold_days <- day_count_map("FDO_P", "grDevices::Blues 3",
                             c("0", "45", "90", "130", "175"))
  ggsave("outputs/plots/cold_days_st.jpeg",
         plot = cold_days, height = 6, width = 8, dpi = 300)

  cat("  Station day-count maps written to outputs/plots/\n")
}

# Export ----

spatial_char <- temp_mun %>%
  left_join(temp_station_mun, by = "ubica_geo") %>%
  mutate(weather = case_when(
    tmax_annual >= 30 & avg_temp >= 24 ~ 1,  # hot
    tmin_annual <= 10 & avg_temp <= 12 ~ 3,  # cold
    TRUE ~ 2                                 # temperate
  ))

cat(sprintf("  Hot municipalities  (weather=1): %d\n",
            sum(spatial_char$weather == 1)))
cat(sprintf("  Temperate mun.      (weather=2): %d\n",
            sum(spatial_char$weather == 2)))
cat(sprintf("  Cold municipalities (weather=3): %d\n",
            sum(spatial_char$weather == 3)))


rm(list = setdiff(ls(), c("spatial_char")))

save.image("outputs/intermediate/spatial.RData")


