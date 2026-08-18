#*** Energy Poverty (plots) **

rm(list = ls())
options(scipen = 999)

if (!require(pacman)) install.packages("pacman")
p_load("sf", "dplyr", "readr", "paletteer", "ggthemes", "ggplot2", "tidyr", "scales",
       "scales", "gstat", "stars", "readxl", "spdep", "stringr", "purrr", "latex2exp",
       "data.table"
)

source("0_legend_helpers.R")

load("outputs/intermediate/dt.RData")

# Figures 1-2 ----

dt16 <- dt_list$dt2016 %>%
  mutate(year = 2016)
dt18 <- dt_list$dt2018 %>%
  mutate(year = 2018)
dt20 <- dt_list$dt2020 %>%
  mutate(year = 2020)
dt22 <- dt_list$dt2022 %>%
  mutate(year = 2022)
dt24 <- dt_list$dt2024 %>%
  mutate(year = 2024)

dt_pooled <- rbindlist(list(dt16, dt18, dt20, dt22, dt24),
                       use.names = TRUE, fill = TRUE)

colors <- c(
  "2016"  = "#6EB665",
  "2018"  = "#2A9D8F",
  "2020"  = "#F4D35E",
  "2022"  = "#6A4C93",
  "2024"  = "#E76F51"
)

fig1 <- ggplot(dt_pooled %>% filter(ictpc_l_r >0))+
  geom_density(aes(x = ictpc_l_r, 
                   color = as.factor(year),
                   weight = factor),
               adjust = 2)+
  #geom_density(aes(x = log(ach_ictpc_r), 
  #                 color = as.factor(year),
  #                 weight = factor),
  #             linetype = "dashed",
  #             adjust = 2)+
  geom_vline(data = dt24 %>% filter(ictpc_l_r >0),
             aes(xintercept = weighted.mean(ictpc_l_r, w=factor), 
                 color = as.factor(year)), linetype = "dashed") +
  geom_vline(data = dt22 %>% filter(ictpc_l_r >0),
             aes(xintercept = weighted.mean(ictpc_l_r, w=factor), 
                 color = as.factor(year)), linetype = "dashed") +
  geom_vline(data = dt20 %>% filter(ictpc_l_r >0),
             aes(xintercept = weighted.mean(ictpc_l_r, w=factor), 
                 color = as.factor(year)), linetype = "dashed") +
  geom_vline(data = dt18 %>% filter(ictpc_l_r >0),
             aes(xintercept = weighted.mean(ictpc_l_r, w=factor), 
                 color = as.factor(year)), linetype = "dashed") +
  geom_vline(data = dt16 %>% filter(ictpc_l_r >0),
             aes(xintercept = weighted.mean(ictpc_l_r, w=factor), 
                 color = as.factor(year)), linetype = "dashed") +
  labs(y="Density",
       x="Per Capita Household Income (log)")+
  scale_color_manual(values = colors, name = "Year") +
  theme_minimal()+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.text = element_text(size = 18))+
  scale_x_continuous(limits = c(4, 12))
ggsave("outputs/plots/income_density.jpeg", plot = fig1, width = 8, height = 6)

fig2 <- ggplot(dt_pooled %>% filter(energy_exp_r > 0 & quintiles == 1)) +
  geom_density(aes(x = log(energy_exp_r),
                   color = as.factor(year),
                   weight = factor), 
               adjust = 2) +
  geom_vline(data = dt24 %>% filter(energy_exp_r > 0),
             aes(xintercept = weighted.mean(log(energy_exp_r), w=factor), 
            color = as.factor(year)), linetype = "dashed") +
  geom_vline(data = dt22 %>% filter(energy_exp_r > 0),
             aes(xintercept = weighted.mean(log(energy_exp_r), w=factor), 
                 color = as.factor(year)), linetype = "dashed") +
  geom_vline(data = dt20 %>% filter(energy_exp_r > 0),
             aes(xintercept = weighted.mean(log(energy_exp_r), w=factor), 
                 color = as.factor(year)), linetype = "dashed") +
  geom_vline(data = dt18 %>% filter(energy_exp_r > 0),
             aes(xintercept = weighted.mean(log(energy_exp_r), w=factor), 
                 color = as.factor(year)), linetype = "dashed") +
  geom_vline(data = dt16 %>% filter(energy_exp_r > 0),
             aes(xintercept = weighted.mean(log(energy_exp_r), w=factor), 
                 color = as.factor(year)), linetype = "dashed") +
  labs(y = "Density",
       x = "Expenditure in energy services (log)") +
  scale_color_manual(values = colors, name = "") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.text = element_text(size = 18)) 
ggsave("outputs/plots/energy_density.jpeg", plot = fig2, width = 8, height = 6)

# Centiles ----

rm(list = ls())

load("outputs/final/estimations_inc.RData")

colors <- c(
  "2016"  = "#6EB665",
  "2018"  = "#2A9D8F",
  "2020"  = "#F4D35E",
  "2022"  = "#6A4C93",
  "2024"  = "#E76F51"
)

fig3 <- ggplot()+
  geom_line(data = results_df_inc %>% filter(year == 2016 & subgroup == "centiles"), 
             aes(x=as.numeric(term), y=log(ictpc_r), color="2016"),
              alpha = 0.9, size = 0.7)+
  geom_line(data = results_df_inc %>% filter(year == 2018 & subgroup == "centiles"), 
             aes(x=as.numeric(term), y=log(ictpc_r), color="2018"),
              alpha = 0.9, size = 0.7)+
  geom_line(data = results_df_inc %>% filter(year == 2020 & subgroup == "centiles"), 
             aes(x=as.numeric(term), y=log(ictpc_r), color="2020"),
              alpha = 0.9, size = 0.7)+
  geom_line(data = results_df_inc %>% filter(year == 2022 & subgroup == "centiles"), 
             aes(x=as.numeric(term), y=log(ictpc_r), color="2022"),
              alpha = 0.9, size = 0.7)+
  geom_line(data = results_df_inc %>% filter(year == 2024 & subgroup == "centiles"), 
             aes(x=as.numeric(term), y=log(ictpc_r), color="2024"),
              alpha = 0.9, size = 0.7)+
  labs(y="Per Capita Household Income (log)",
       x="Centiles")+
  scale_color_manual(values = colors, name = "") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.text = element_text(size = 18)) 

ggsave("outputs/plots/ictpc_cen.jpeg", plot = fig3, width = 8, height = 6)

fig4 <- ggplot()+
  geom_point(data = results_df_inc %>% filter(year == 2016 & 
                                                subgroup == "centiles"), 
             aes(x=as.numeric(term), y=H, color="2016"),
             shape = 15, alpha = 0.9)+
  geom_point(data = results_df_inc %>% filter(year == 2018 & 
                                                subgroup == "centiles"), 
             aes(x=as.numeric(term), y=H, color="2018"),
             shape = 16, alpha = 0.9)+
  geom_point(data = results_df_inc %>% filter(year == 2020 & 
                                                subgroup == "centiles"), 
             aes(x=as.numeric(term), y=H, color="2020"),
             shape = 17, alpha = 0.9)+
  geom_point(data = results_df_inc %>% filter(year == 2022 & 
                                                subgroup == "centiles"), 
             aes(x=as.numeric(term), y=H, color="2022"),
             shape = 18, alpha = 0.9)+
  geom_point(data = results_df_inc %>% filter(year == 2024 & 
                                                subgroup == "centiles"), 
             aes(x=as.numeric(term), y=H, color="2024"),
             shape = 20, alpha = 0.9)+
  labs(y="H",
       x="Centiles")+
  scale_color_manual(values = colors, name = "") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.text = element_text(size = 18)) 

ggsave("outputs/plots/H_cen.jpeg", plot = fig4, width = 8, height = 6)

fig5 <- ggplot()+
  geom_point(data = results_df_inc %>% filter(year == 2016 & 
                                                subgroup == "centiles"), 
             aes(x=as.numeric(term), y=A, color="2016"),
             shape = 15, alpha = 0.9)+
  geom_point(data = results_df_inc %>% filter(year == 2018 & 
                                                subgroup == "centiles"), 
             aes(x=as.numeric(term), y=A, color="2018"),
             shape = 16, alpha = 0.9)+
  geom_point(data = results_df_inc %>% filter(year == 2020 & 
                                                subgroup == "centiles"), 
             aes(x=as.numeric(term), y=A, color="2020"),
             shape = 17, alpha = 0.9)+
  geom_point(data = results_df_inc %>% filter(year == 2022 & 
                                                subgroup == "centiles"), 
             aes(x=as.numeric(term), y=A, color="2022"),
             shape = 18, alpha = 0.9)+
  geom_point(data = results_df_inc %>% filter(year == 2024 & 
                                                subgroup == "centiles"), 
             aes(x=as.numeric(term), y=A, color="2024"),
             shape = 20, alpha = 0.9)+
  labs(y="A",
       x="Centiles")+
  scale_color_manual(values = colors, name = "") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.text = element_text(size = 18)) 

ggsave("outputs/plots/A_cen.jpeg", plot = fig5, width = 8, height = 6)

fig4_1 <- ggplot()+
  geom_smooth(data = results_df_inc %>% filter(year == 2016 & 
                                                subgroup == "centiles"), 
             aes(x=as.numeric(term), y=log(energy_exp_r), color="2016"),
              alpha = 0.9, se = F, size = 0.7, span = 0.1)+
  geom_smooth(data = results_df_inc %>% filter(year == 2018 & 
                                                subgroup == "centiles"), 
             aes(x=as.numeric(term), y=log(energy_exp_r), color="2018"),
              alpha = 0.9, se = F, size = 0.7, span = 0.1)+
  geom_smooth(data = results_df_inc %>% filter(year == 2020 & 
                                                subgroup == "centiles"), 
             aes(x=as.numeric(term), y=log(energy_exp_r), color="2020"),
              alpha = 0.9, se = F, size = 0.7, span = 0.1)+
  geom_smooth(data = results_df_inc %>% filter(year == 2022 & 
                                                subgroup == "centiles"), 
             aes(x=as.numeric(term), y=log(energy_exp_r), color="2022"),
              alpha = 0.9, se = F, size = 0.7, span = 0.1)+
  geom_smooth(data = results_df_inc %>% filter(year == 2024 & 
                                                subgroup == "centiles"), 
             aes(x=as.numeric(term), y=log(energy_exp_r), color="2024"),
              alpha = 0.9, se = F, size = 0.7, span = 0.1)+
  labs(y="Expenditure in energy services (log)",
       x="Centiles")+
  scale_color_manual(values = colors, name = "") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.text = element_text(size = 18)) 

ggsave("outputs/plots/energy_cen_1.jpeg", plot = fig4_1, width = 8, height = 6)

cent_data16 <- results_df_inc %>% filter(subgroup == "centiles") %>%
  filter(year == 2016) %>%
  select(H, ictpc_r, energy_exp_r, term) %>%
  rename(H_2016 = H, 
         energy_2016 = energy_exp_r,
         ictpc_r_2016 = ictpc_r)

cent_data24 <- results_df_inc %>% filter(subgroup == "centiles") %>%
  filter(year == 2024) %>%
  select(H, ictpc_r, energy_exp_r, term) %>%
  rename(H_2024 = H, 
         energy_2024 = energy_exp_r,
         ictpc_r_2024 = ictpc_r)

cent_data_delta <- cent_data16 %>%
  left_join(cent_data24, by = "term") %>%
  mutate(delta_H = H_2024 - H_2016, 
         gg_ictpc = (ictpc_r_2024/ictpc_r_2016)-1,
         gg_energy = (energy_2024/energy_2016)-1)

fig5b <- ggplot()+
  geom_point(data = cent_data_delta,
             aes(x=as.numeric(term), y=delta_H*100), color="red",
             shape = 16, alpha = 0.9)+
  geom_hline(yintercept = 0, linetype = "dotted", 
             color = "black", alpha = 0.8)+
  geom_vline(xintercept = 50, linetype = "dotted", 
             color = "black", alpha = 0.8)+
  geom_smooth(data = cent_data_delta, aes(x=as.numeric(term), y=delta_H*100), 
              method = "lm", se = TRUE, color = "blue", size = 1, 
              linetype = "dashed") +
  labs(y = latex2exp::TeX("$\\Delta H$ (pp)"),
       x = "Centiles")+
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.text = element_text(size = 18)) 

ggsave("outputs/plots/delta_H_cen.jpeg", plot = fig5b, width = 8, height = 6)

fig6 <- ggplot()+
  geom_point(data = cent_data_delta, 
             aes(x=as.numeric(term), y=gg_ictpc*100), color="red",
             shape = 16, alpha = 0.9)+
  geom_hline(yintercept = 0, linetype = "dotted", 
             color = "black", alpha = 0.8)+
  geom_vline(xintercept = 50, linetype = "dotted", 
             color = "black", alpha = 0.8)+
  geom_smooth(data = cent_data_delta, aes(x=as.numeric(term), y=gg_ictpc*100), 
              method = "lm", se = TRUE, color = "blue", size = 0.5, 
              linetype = "dashed") +
  labs(y = "Per Capita Household Income (growth rate)",
       x = "Centiles")+
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.text = element_text(size = 18)) 

ggsave("outputs/plots/gg_ictpc_cen.jpeg", plot = fig6, width = 8, height = 6)

fig7 <- ggplot()+
  geom_point(data = cent_data_delta, 
             aes(x=as.numeric(term), y=gg_energy*100), color="red",
             shape = 16, alpha = 0.9)+
  geom_hline(yintercept = 0, linetype = "dotted", 
             color = "black", alpha = 0.8)+
  geom_vline(xintercept = 50, linetype = "dotted", 
             color = "black", alpha = 0.8)+
  geom_smooth(data = cent_data_delta, aes(x=as.numeric(term), y=gg_energy*100), 
              method = "lm", se = TRUE, color = "blue", size = 0.5, 
              linetype = "dashed") +
  labs(y = "Expenditure in energy services (growth rate)",
       x = "Centiles")+
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.text = element_text(size = 18)) 

ggsave("outputs/plots/gg_energy_cen.jpeg", plot = fig7, width = 8, height = 6)

# Maps ----

rm(list = ls())
# Re-sourced deliberately: the rm() above clears the environment, and every
# auto_legend()/fmt_*() call in this file lives below this line. The earlier
# source() near the top of the script covers the (currently helper-free) blocks
# above it; this is not a duplicate.
source("0_legend_helpers.R")

load("outputs/final/estimations_means.RData")

ents_shp <- "inputs/spatial_data/ent/ent.shp"
ent <- read_sf(ents_shp) %>%
  select(NOMGEO, CVE_ENT, geometry) %>%
  mutate(CVE_ENT = as.factor(CVE_ENT))

{
  state_data <- results_df %>%
    filter(subgroup == "ent_name") %>%
    rename(ent_name = term) %>%
    mutate(CVE_ENT = as.factor(case_when( ent_name == "Aguascalientes" ~ "01",
                                ent_name == "Baja California" ~ "02",
                                ent_name == "Baja California Sur" ~ "03",
                                ent_name == "Campeche" ~ "04",
                                ent_name == "Coahuila" ~ "05",
                                ent_name == "Colima" ~ "06",
                                ent_name == "Chiapas" ~ "07",
                                ent_name == "Chihuahua" ~ "08",
                                ent_name == "Ciudad de México" ~ "09",
                                ent_name == "Durango" ~ "10",
                                ent_name == "Guanajuato" ~ "11",
                                ent_name == "Guerrero" ~ "12",
                                ent_name == "Hidalgo" ~ "13",
                                ent_name == "Jalisco" ~ "14",
                                ent_name == "México" ~ "15",
                                ent_name == "Michoacán" ~ "16",
                                ent_name == "Morelos" ~ "17",
                                ent_name == "Nayarit" ~ "18",
                                ent_name == "Nuevo León" ~ "19",
                                ent_name == "Oaxaca" ~ "20",
                                ent_name == "Puebla" ~ "21",
                                ent_name == "Querétaro" ~ "22",
                                ent_name == "Quintana Roo" ~ "23",
                                ent_name == "San Luis Potosí" ~ "24",
                                ent_name == "Sinaloa" ~ "25",
                                ent_name == "Sonora" ~ "26",
                                ent_name == "Tabasco" ~ "27",
                                ent_name == "Tamaulipas" ~ "28",
                                ent_name == "Tlaxcala" ~ "29",
                                ent_name == "Veracruz" ~ "30",
                                ent_name == "Yucatán" ~ "31",
                                ent_name == "Zacatecas" ~ "32")))
  
  state_data16 <- state_data %>%
    filter(year == 2016) %>%
    select(H, A, M0, CVE_ENT, ictpc, ictpc_r) %>%
    rename(H_2016 = H, M0_2016 = M0, A_2016 = A, 
           ictpc_2016 = ictpc, ictpc_r_2016 = ictpc_r)
  
  state_data24 <- state_data %>%
    filter(year == 2024) %>%
    select(H, A, M0, CVE_ENT, ictpc, ictpc_r) %>%
    rename(H_2024 = H, M0_2024 = M0, A_2024 = A, 
           ictpc_2024 = ictpc, ictpc_r_2024 = ictpc_r)
  
  stata_data_delta <- state_data16 %>%
    left_join(state_data24, by = c("CVE_ENT")) %>%
    mutate(delta_H = H_2024 - H_2016,
           delta_A = A_2024 - A_2016,
           delta_M0 = M0_2024 - M0_2016, 
           gg_ictpc = (ictpc_r_2024/ictpc_r_2016)-1)
}

ent <- ent %>%
  left_join(stata_data_delta, by = "CVE_ENT") 

lg_H <- auto_legend(ent$H_2024, formatter = fmt_pct)
H_2024 <- ggplot(data = ent) +
  geom_sf(fill = "gray95", color = "gray80", size = 0.1) +
  geom_sf(aes(fill = H_2024), size = 0.5) +
  scale_fill_gradientn(
    name   = "",
    limits = lg_H$limits,
    breaks = lg_H$breaks,
    labels = lg_H$labels,
    oob    = squish,
    colors = paletteer_c("grDevices::Red-Purple", 32, direction = -1)
  )+
  theme_void(base_size = 12) +
  theme(
    legend.position  = "bottom",
    legend.text = element_text(size = 18),
    legend.key.width = unit(2, "cm"),
    plot.background  = element_rect(fill = "white", color = NA),
    plot.margin      = margin(10, 10, 10, 10)
  )
ggsave("outputs/plots/H_2024.jpeg", plot = H_2024, height = 6, width = 8, dpi = 300)

lg_A <- auto_legend(ent$A_2024, formatter = fmt_pct)
A_2024 <- ggplot() +
  geom_sf(data = ent, fill = "gray95", color = "gray80", size = 0.1) +
  geom_sf(data = ent, aes(fill = A_2024), size = 0.5) +
  scale_fill_gradientn(
    name   = "",
    limits = lg_A$limits,
    breaks = lg_A$breaks,
    labels = lg_A$labels,
    oob    = squish,
    colors = paletteer_c("grDevices::Red-Purple", 32, direction = -1)
  )+
  theme_void(base_size = 12) +
  theme(
    legend.position  = "bottom",
    legend.text = element_text(size = 18),
    legend.key.width = unit(2, "cm"),
    plot.background  = element_rect(fill = "white", color = NA),
    plot.margin      = margin(10, 10, 10, 10)
  )
ggsave("outputs/plots/A_2024.jpeg", plot = A_2024, height = 6, width = 8, dpi = 300)

lg_ic <- auto_legend(ent$ictpc_2024, formatter = fmt_money)
ictpc_2024 <- ggplot() +
  geom_sf(data = ent, fill = "gray95", color = "gray80", size = 0.1) +
  geom_sf(data = ent, aes(fill = ictpc_2024), size = 0.5) +
  scale_fill_gradientn(
    name   = "",
    limits = lg_ic$limits,
    breaks = lg_ic$breaks,
    labels = lg_ic$labels,
    oob    = squish,
    colors = paletteer_c("grDevices::Red-Purple", 32, direction = -1)
  )+
  theme_void(base_size = 12) +
  theme(
    legend.position  = "bottom",
    legend.text = element_text(size = 18),
    legend.key.width = unit(2, "cm"),
    plot.background  = element_rect(fill = "white", color = NA),
    plot.margin      = margin(10, 10, 10, 10)
  )
ggsave("outputs/plots/ictpc_2024.jpeg", plot = ictpc_2024, height = 6, width = 8, dpi = 300)

lg_dH <- auto_legend(ent$delta_H * 100, formatter = fmt_pp)  # H stored 0–1 → ×100 to pp
delta_H <- ggplot() +
  geom_sf(data = ent, fill = "gray95", color = "gray80", size = 0.1) +
  geom_sf(data = ent, aes(fill = delta_H * 100), size = 0.5) +
  scale_fill_gradientn(
    name   = "",
    limits = lg_dH$limits,
    breaks = lg_dH$breaks,
    labels = lg_dH$labels,
    colors = paletteer_c("grDevices::Greens 3", 30)
  )+
  theme_void(base_size = 12) +
  theme(
    legend.position  = "bottom",
    legend.text = element_text(size = 18),
    legend.key.width = unit(2, "cm"),
    plot.background  = element_rect(fill = "white", color = NA),
    plot.margin      = margin(10, 10, 10, 10)
  )
ggsave("outputs/plots/delta_H.jpeg", plot = delta_H, height = 6, width = 8, dpi = 300)

lg_dA <- auto_legend(ent$delta_A * 100, formatter = fmt_pp)
delta_A <- ggplot() +
  geom_sf(data = ent, fill = "gray95", color = "gray80", size = 0.1) +
  geom_sf(data = ent, aes(fill = delta_A * 100), size = 0.5) +
  scale_fill_gradientn(
    name   = "",
    limits = lg_dA$limits,
    breaks = lg_dA$breaks,
    labels = lg_dA$labels,
    oob    = squish,
    colors = paletteer_c("grDevices::Tropic", 30)
  )+
  theme_void(base_size = 12) +
  theme(
    legend.position  = "bottom",
    legend.text = element_text(size = 18),
    legend.key.width = unit(2, "cm"),
    plot.background  = element_rect(fill = "white", color = NA),
    plot.margin      = margin(10, 10, 10, 10)
  )
ggsave("outputs/plots/delta_A.jpeg", plot = delta_A, height = 6, width = 8, dpi = 300)

lg_gg <- auto_legend(ent$gg_ictpc, formatter = fmt_growth)
gg_ictpc <- ggplot() +
  geom_sf(data = ent, fill = "gray95", color = "gray80", size = 0.1) +
  geom_sf(data = ent, aes(fill = gg_ictpc), size = 0.5) +
  scale_fill_gradientn(
    name   = "",
    limits = lg_gg$limits,
    breaks = lg_gg$breaks,
    labels = lg_gg$labels,
    oob    = squish,
    colors =paletteer_c("ggthemes::Temperature Diverging", 30, direction =-1)
  )+
  theme_void(base_size = 12) +
  theme(
    legend.position  = "bottom",
    legend.text = element_text(size = 18),
    legend.key.width = unit(2, "cm"),
    plot.background  = element_rect(fill = "white", color = NA),
    plot.margin      = margin(10, 10, 10, 10)
  )
ggsave("outputs/plots/gg_ictpc.jpeg", plot = gg_ictpc, height = 6, width = 8, dpi = 300)

# Dimensional contribution ----

rm(list = ls())
load("outputs/final/dimensional_decomposition.RData")

plot_data <- final_output %>%
  filter(dimension != "Total") %>% 
  pivot_longer(
    cols = starts_with("relative_j_"),
    names_to = "year",
    values_to = "share"
  ) %>%
  mutate(
    year = str_extract(year, "\\d{4}"),
    year = as.numeric(year)
  )

labels_dim <- c(
  dp_hill  = "Af",
  dp_light = "Ea",
  dp_cook  = "Ck",
  dp_refri = "Rf",
  dp_entr  = "Et",
  dp_com   = "Cm",
  dp_wt    = "Wt",
  dp_clim  = "Tc"
)

colors_dim <- c(
  dp_wt    = "#1b9e77",
  dp_light = "#d95f02",
  dp_cook  = "#7570b3",
  dp_refri = "#e7298a",
  dp_entr  = "#66a61e",
  dp_com   = "#e6ab02",
  dp_hill  = "#a6761d",
  dp_clim  = "#666666"
)

plot_data <- plot_data %>%
  group_by(dimension) %>%
  mutate(order_2024 = share[year == 2024]) %>%
  ungroup() %>%
  mutate(dimension = reorder(dimension, -order_2024))

dim <- ggplot(plot_data, aes(x = year, y = share, fill = dimension)) +
  geom_area(position = "stack", alpha = 0.70) +
  scale_x_continuous(breaks = c(2016, 2018, 2020, 2022, 2024)) +
  scale_fill_manual(
    values = colors_dim,
    labels = labels_dim
  ) +
  labs(
    x = "Year",
    y = latex2exp::TeX("$M_0$ (%)")
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    legend.text = element_text(size = 18)
  )

ggsave("outputs/plots/dim.jpeg", plot = dim, width = 8, height = 6, dpi = 300)

# k sensitivity figures ----

rm(list = ls())
load("outputs/final/k_sensitivity.RData")

k_colors <- c(
  "0.1"  = "#6EB665",
  "0.15" = "#2A9D8F",
  "0.2"  = "#F4D35E",
  "0.25" = "#6A4C93",
  "0.3"  = "#E76F51"
)

k_colors2 <- c(
  "0.125" = "#6EB665",
  "0.25"  = "#2A9D8F",
  "0.375" = "#F4D35E",
  "0.5"   = "#6A4C93",
  "0.625" = "#E76F51"
)


ksens_all <- bind_rows(
  k_sensitivity      %>% mutate(scheme = "NB weights"),
  k_sensitivity_mca  %>% mutate(scheme = "MCA weights"),
  k_sensitivity_eq   %>% mutate(scheme = "Equal weights"),
  k_sensitivity_freq %>% mutate(scheme = "Frequency-based weights")
) %>%
  mutate(
    k_chr  = as.character(k),
    scheme = factor(scheme, levels = c("NB weights", "MCA weights",
                                       "Equal weights",
                                       "Frequency-based weights"))
  )

## Plotting helper for k-sensitivity panels --------------------------
.ksens_panel <- function(df, y_var, y_lab, palette) {
  df <- df %>% mutate(
    .y    = .data[[y_var]],
    .se   = .data[[paste0("se_", y_var)]],
    .ymin = .y - 1.96 * .se,
    .ymax = .y + 1.96 * .se
  )
  ggplot(df, aes(x = year, y = .y, color = k_chr, fill = k_chr)) +
    geom_ribbon(aes(ymin = .ymin, ymax = .ymax), alpha = 0.3, color = NA) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 1.8) +
    scale_x_continuous(breaks = c(2016, 2018, 2020, 2022, 2024)) +
    scale_color_manual(values = palette, name = NULL) +
    scale_fill_manual( values = palette, name = NULL) +
    labs(x = "Year", y = y_lab) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      axis.title.x = element_text(size = 16),
      axis.title.y = element_text(size = 16),
      legend.text  = element_text(size = 18)
    )
}

## Generate one H + one M0 panel per scheme ---------------------------
schemes <- list(
  list(label = "NB weights",              tag = "NB",  palette = k_colors),
  list(label = "MCA weights",             tag = "MCA", palette = k_colors),
  list(label = "Equal weights",           tag = "EQ",  palette = k_colors2),
  list(label = "Frequency-based weights", tag = "FQ",  palette = k_colors)
)

for (s in schemes) {
  df_s <- ksens_all %>% filter(scheme == s$label)

  ggsave(sprintf("outputs/plots/k_sens_%s.jpeg", s$tag),
         plot = .ksens_panel(df_s, "M0", latex2exp::TeX("$M_0$ (%)"), s$palette),
         width = 8, height = 6, dpi = 300)
}

