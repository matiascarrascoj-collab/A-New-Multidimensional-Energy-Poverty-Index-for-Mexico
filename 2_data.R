#*** Energy Poverty (dataset construction) **
# Preamble --------------------------------------------------------------------

rm(list = ls())
options(scipen = 999)
options(survey.lonely.psu = "adjust") 

if (!require(pacman)) install.packages("pacman") 
p_load("dplyr", "rstudioapi","dineq", "haven", 
       "data.table", "stringr", "zoo", "data.table", 
       "tidyverse", "srvyr", "bit64", "survey",
       "convey", "scales", "sf", "readr", "broom", "FactoMineR"
)

# I: Dataset construction ----

years <- c(2016, 2018, 2020, 2022, 2024)

dt_list <- list()

ach_cols <- c("ach_light", "ach_cook", "ach_refri", 
              "ach_entr", "ach_com", "ach_wt", "ach_clim")

energia_claves1 <- c("G009", # Gas LP
                     "G010", # Oil
                     "G011", # Diesel
                     "G012", # Coal
                     "G013", # Wood
                     "G014", # Heating fuel
                     "G015", # Candles 
                     "G016", # Other fuels
                     "R001", # Electricity
                     "R003"  # Natural gas
                     )

energia_claves2 <-  c("045222", # Stationary LP gas 
                      "045300", # Liquid fuels
                      "045410", # Coal-charcoal 
                      "045420", # Wood-based fuels
                      "045430", # Other biomass fuels
                      "045490", # Other solid fuels
                      "05619A", # Candles 
                      "045100", # Electricity
                      "045210", # Natural gas
                      "045221"  # Non-stationary LP gas
                      )

deflators <- list(
  `2016` = list(d07 = 1.0017314941, d08 = 1, d09 = 0.9978188915, d10 = 1.0133832055),
  `2018` = list(d07 = 0.9975063146, d08 = 1, d09 = 1.0064969411, d10 = 1.0203890181),
  `2020` = list(d07 = 0.9963402865, d08 = 1, d09 = 1.0008217634, d10 = 1.0170085903),
  `2022` = list(d07 = 0.9998142481, d08 = 1, d09 = 0.9978682753, d10 = 1.0031577830),
  `2024` = list(d07 = 0.9974870947, d08 = 1, d09 = 1.0023425388, d10 = 1.0112186313)
)

lineas <- list(
  `2016` = list(urbana = 2903.91, rural = 2030.14),
  `2018` = list(urbana = 3325.40, rural = 2316.53),
  `2020` = list(urbana = 3559.53, rural = 2519.95),
  `2022` = list(urbana = 4157.63, rural = 2970.19),
  `2024` = list(urbana = 4564.97, rural = 3296.92)
)

load("outputs/intermediate/spatial.RData")

for (i in years) {
  message(format(Sys.time(), "%H:%M:%S"), " | Running year ", i)
  
  ## I-1 Viviendas table ---- 
  
  viviendas_path <- sprintf("inputs/viviendas/viviendas%d.csv", i)
  viviendas <- fread(viviendas_path, colClasses = list(character = "folioviv")) %>% rename_all(tolower) %>%
    mutate(folioviv = ifelse(nchar(folioviv) == 9, paste0("0", folioviv), folioviv))

  if (i %in% c(2016, 2018, 2020, 2022)) {
    viviendas <- viviendas %>% mutate(focos=focos_inca+focos_ahor) %>%
      dplyr::rename(combus=combustible, fogon_chi=estufa_chi)
  } 
  
  viviendas <- viviendas %>% 
    mutate(ach_light = ifelse(disp_elect <= 4, 1, 0),
           combus=as.numeric(combus),
           estufa=as.numeric(fogon_chi),
           ach_cook = (case_when((combus==1 | combus==2) & estufa==2 ~ 0,
                                 (combus==1 | combus==2) & estufa==1 ~ 1,
                                 combus>=3 & combus<=7 ~ 1)),
           water_heat = ifelse((calent_sol== 1 | calent_gas == 1), 1 , 0),
           aire_acond = ifelse(aire_acond==1, 1, 0),
           calefacc = ifelse(calefacc==1, 1, 0))
   
  viviendas <- viviendas %>% dplyr::select(folioviv, ach_light, ach_cook,
                                           water_heat, aire_acond, calefacc)
  
  # I-2 Hogares table ----

  hogares_path <- sprintf("inputs/hogares/hogares%d.csv", i)
  hogares   <- fread(hogares_path,   colClasses = list(character = c("folioviv", "foliohog"))) %>% 
    rename_all(tolower)
  
  if (i %in% c(2016, 2018, 2020)) {
    hogares <- hogares %>% 
      mutate(num_tva=ifelse(num_tva== -1, NA, num_tva),
             num_tvd=ifelse(num_tvd== -1, NA, num_tvd),
             num_tv = num_tva + num_tvd)
  } else {
    hogares <- hogares %>% 
      mutate(num_tva=ifelse(num_tva == "&", NA, num_tva),
             num_tvd=ifelse(num_tvd == "&", NA, num_tvd),
             num_tv = num_tva + num_tvd)
  }

   hogares <- hogares %>%
    mutate(folioviv = ifelse(nchar(folioviv) == 9, paste0("0", folioviv), folioviv),
           ach_refri = ifelse(num_refri >= 1, 1, 0),
           ach_entr = ifelse((num_tv>=1  | (num_compu>=1 & conex_inte == 1) | num_radio>=1), 1, 0),
           ach_com = ifelse(celular == 1 | telefono == 1, 1, 0),
           estufa = ifelse(num_estuf >= 1, 1, 0)) %>%
    dplyr::select(folioviv, foliohog, ach_refri, ach_entr, ach_com,
                  estufa, num_venti, num_tv)
  
  # I-3 Población table ----
  
  poblacion_path <- sprintf("inputs/poblacion/poblacion%d.csv", i)
  poblacion <- fread(poblacion_path, colClasses = list(character = c("folioviv", "foliohog"))) %>%
    rename_all(tolower) %>%
    mutate(folioviv = ifelse(nchar(folioviv) == 9, paste0("0", folioviv), folioviv),
           children_1 = ifelse(edad <= 5, 1, 0),
           children_2 = ifelse(edad > 5 & edad <= 11, 1, 0),
           children_3 = ifelse(edad > 11 & edad <= 17, 1, 0),
           children = ifelse(edad <= 17, 1, 0),
           elderly = ifelse(edad >= 65, 1, 0),
           women = ifelse(sexo == 2, 1, 0)) 
  
  if (i %in% c(2016, 2018)) {
    poblacion <- poblacion %>% mutate(discap=ifelse((disc1 != "&" | disc1 == "8"), 1, 0))
  } else {
    poblacion <- poblacion %>%
      mutate(discap=case_when((disc_camin>="1" & disc_camin<="2") ~1, 
                              (disc_ver  >="1" & disc_ver  <="2") ~1, 
                              (disc_brazo>="1" & disc_brazo<="2") ~1, 
                              (disc_apren>="1" & disc_apren<="2") ~1, 
                              (disc_oir  >="1" & disc_oir  <="2") ~1, 
                              (disc_vest >="1" & disc_vest <="2") ~1, 
                              (disc_habla>="1" & disc_habla<="2") ~1,  
                              (disc_acti>="1" & disc_acti<="2")  ~1,  
                              (disc_camin=="&" & disc_ver=="&" &
                               disc_brazo=="&" & disc_apren=="&" &
                               disc_oir=="&" & disc_vest=="&" &
                               disc_habla=="&" & disc_acti=="&") ~ NA_real_,
                               TRUE ~0))
  }
  
  ### Same sex households----
  
  hogares_same_sex <- poblacion %>%
    group_by(folioviv, foliohog) %>%
    summarise(same_sex_hh = as.integer(any(parentesco == "205")), .groups = "keep")
  
  poblacion <- poblacion %>%
    left_join(hogares_same_sex, by = c("folioviv", "foliohog")) %>%
    mutate(
      homosexual = ifelse(parentesco %in% c("101", "205"), same_sex_hh, 0L)
    ) %>%
  dplyr::select(folioviv, foliohog, numren, madre_id, 
                padre_id, alfabetism, children_1, children_2, 
                children_3, children, elderly, discap, women, homosexual,
                parentesco, same_sex_hh, hablaind, edad, etnia)
  
  ### Indigenous identification ----
  
  parentesco_inpi <- c(
    "101",     # jefe/a
    "201", "202", "203", "204", "205",  # cónyuge / pareja
    "601",     # madre, padre, madrastra, padrastro
    "606",     # abuelo/a
    "607",     # bisabuelo/a
    "608",     # tatarabuelo/a
    "615"      # suegro/a
  )
  
  # Household identification 
  hogares_indigenas <- poblacion %>%
    filter(parentesco %in% parentesco_inpi) %>%
    group_by(folioviv, foliohog) %>%
    summarise(
      hogar_indigena = as.integer(any(hablaind == 1, na.rm = TRUE)),
      .groups = "drop"
    )
  
  poblacion <- poblacion %>%
    left_join(hogares_indigenas, by = c("folioviv", "foliohog")) %>%
    mutate(hogar_indigena = replace_na(hogar_indigena, 0))
  
  hablaind_lookup <- poblacion %>%
    mutate(numren = as.character(numren)) %>%
    select(folioviv, foliohog, numren, hablaind_lookup = hablaind)
  
  poblacion <- poblacion %>%
    # Traer hablaind de la madre
    left_join(
      hablaind_lookup %>% rename(hablaind_madre = hablaind_lookup,
                                 madre_id       = numren),
      by = c("folioviv", "foliohog", "madre_id")
    ) %>%
    # Traer hablaind del padre
    left_join(
      hablaind_lookup %>% rename(hablaind_padre = hablaind_lookup,
                                 padre_id       = numren),
      by = c("folioviv", "foliohog", "padre_id")
    ) %>%
    mutate(
      indigenous = case_when(
        hogar_indigena == 1                          ~ 1,
        edad < 3 & is.na(hablaind) & is.na(etnia) &
          (hablaind_madre == 1 |
             hablaind_padre == 1)                ~ 1,
        edad < 3 & is.na(hablaind) & is.na(etnia) &
          hogar_indigena == 1                  ~ 1,
        TRUE                                   ~ 0
      )
    ) %>%
    select(-hablaind_madre, -hablaind_padre) %>%
    dplyr::select(
      folioviv, foliohog, numren, alfabetism, children_1, children_2, children_3, 
      children, elderly, indigenous, women, homosexual, same_sex_hh, discap,
      hogar_indigena, edad
    )
  
  # I-4 Pobreza table ----
  
  pobreza_path <- sprintf("inputs/pobreza/pobreza%d.csv", i)
  pobreza <- fread(pobreza_path, colClasses = list(character = c("folioviv", "foliohog"))) %>%
    rename_all(tolower) %>%
    mutate(folioviv = ifelse(nchar(folioviv) == 9, paste0("0", folioviv), folioviv),
           ubica_geo = ifelse(nchar(ubica_geo) == 4, paste0("0", ubica_geo), ubica_geo),
           ubica_geo = as.character(ubica_geo),
           quintiles = as.factor(ntiles.wtd(x=ictpc, n=5, weights=factor)),
           deciles = as.factor(ntiles.wtd(x=ictpc, n=10, weights=factor)),
           centiles = as.factor(ntiles.wtd(x=ictpc, n=100, weights=factor)),
           rururb = as.factor(rururb),
           ictpc_l = log(ictpc)) %>%
    dplyr::select(-parentesco, -edad) 
  
  if (i %in% c(2020, 2022, 2024)) {
    pobreza <- pobreza %>% dplyr::select(-discap)
  }
  
  if (i == 2016) {
    pobreza <- pobreza %>% mutate(ubica_geo = ifelse(nchar(ubica_geo) == 8, paste0("0", ubica_geo), ubica_geo),
                                  ubica_geo = stringr::str_sub(ubica_geo, 1, 5))
  }
  
  # I-5 Energy expenditures ----
  
  yr <- as.character(i)
  defl <- deflators[[yr]]

  path_ghog <- sprintf("inputs/gastos/gastoshogar%d.csv", i)
  path_gper <- sprintf("inputs/gastos/gastospersona%d.csv", i)
  
  ghog <- fread(path_ghog, colClasses = list(character = c("folioviv", "foliohog"))) %>% 
    rename_all(tolower) %>% mutate(base = 1)
  gper <- fread(path_gper, colClasses = list(character = c("folioviv", "foliohog"))) %>% 
    rename_all(tolower) %>% mutate(base = 2)
  
  gasto <- bind_rows(ghog, gper) %>%
    mutate(
      folioviv_raw = as.character(folioviv),
      folioviv     = str_pad(folioviv_raw, 10, "left", pad = "0"),
      decena       = as.integer(str_sub(folioviv, 8, 8)), 
    ) 
  
  if(i == 2024){
    energia_claves_i <- energia_claves2
  } else{
    energia_claves_i <- energia_claves1
  }
  
  energia_mon <- data.table(gasto)[
    
    tipo_gasto == "G1"
    
  ][clave %in% energia_claves_i
    
  ][, gasto_mensual := gasto_tri / 3
    
  ][, gasto_mensual_def := fcase(
    decena %in% c(1L, 2L),     gasto_mensual / defl$d07,
    decena %in% c(3L, 4L, 5L), gasto_mensual / defl$d08,
    decena %in% c(6L, 7L, 8L), gasto_mensual / defl$d09,
    decena %in% c(9L, 0L),     gasto_mensual / defl$d10,
    default = gasto_mensual
  )
  
  ][, .(energy_exp = sum(gasto_mensual_def, na.rm = TRUE)),
    by = .(folioviv, foliohog)]
  
  # I-6 Concentrado table
  
  concentrado_path <- sprintf("inputs/concentrado/concentradohogar%d.csv", i)
  concentrado <- fread(concentrado_path, colClasses = list(character = c("folioviv", "foliohog"))) %>% 
    rename_all(tolower) %>%
    mutate(folioviv = ifelse(nchar(folioviv) == 9, paste0("0", folioviv), folioviv)) %>%
    dplyr::select(folioviv, foliohog, tot_integ)
  
  # I-6 Merge ----
  
  linea = lineas[[yr]]
  urban_line <- linea$urbana
  rural_line <- linea$rural
  
  dt_i <- poblacion %>%
    left_join(viviendas, by = "folioviv") %>%
    left_join(hogares, by = c("folioviv", "foliohog")) %>%
    left_join(energia_mon, by = c("folioviv", "foliohog")) %>%
    left_join(concentrado, by = c("folioviv", "foliohog")) %>%
    left_join(pobreza, by = c("folioviv", "foliohog", "numren")) %>%
    filter(!is.na(ubica_geo)) %>%
    filter(ictpc_l != -Inf) %>%
    mutate(energy_exp  = replace_na(energy_exp, 0))
  
  dt_i <- dt_i %>% 
    left_join(spatial_char, by = "ubica_geo") %>%
    mutate(
      ach_wt       = ifelse(water_heat == 1 | estufa == 1, 1, 0),
      ratio_venti = case_when(
        tot_integ > 0 & (num_venti / tot_integ) >= 0.5 ~ 1,
        TRUE ~ 0
      ),
      ach_clim = case_when(
        weather == 1 & (aire_acond == 1 | ratio_venti == 1) ~ 1,
        weather == 3 & calefacc == 1 ~ 1,
        weather == 2 ~ NA_real_,
        TRUE ~ 0
      ),
      ict_ad = ict-energy_exp,
      ach_ictpc = ict_ad/tamhogesc,
      dp_hill =case_when(ach_ictpc <urban_line  & rururb==0  ~ 1,
                         ach_ictpc>=urban_line  & rururb==0 & !is.na(ictpc) ~ 0,
                         ach_ictpc <rural_line  & rururb==1  ~ 1,
                         ach_ictpc>=rural_line  & rururb==1 & !is.na(ictpc) ~ 0),
      dp_light = ifelse(ach_light == 0, 1, 0), 
      dp_cook = ifelse(ach_cook == 0, 1, 0), 
      dp_refri = ifelse(ach_refri == 0, 1, 0), 
      dp_entr = ifelse(ach_entr == 0, 1, 0),
      dp_com = ifelse(ach_com == 0, 1, 0),
      dp_wt = ifelse(ach_wt == 0, 1, 0), 
      dp_clim = ifelse(ach_clim == 1, 0, 1),
      upm       = as.factor(upm),
      est_dis   = as.factor(est_dis),
      factor    = as.numeric(factor),
            ent = as.factor(ent),
       ent_name = case_when(ent == 1  ~ "Aguascalientes",
                                ent == 02  ~ "Baja California",
                                ent == 03  ~ "Baja California Sur",
                                ent == 04  ~ "Campeche",
                                ent == 05  ~ "Coahuila",
                                ent == 06  ~ "Colima",
                                ent == 07  ~ "Chiapas",
                                ent == 08  ~ "Chihuahua",
                                ent == 09  ~ "Ciudad de México",
                                ent == 10 ~ "Durango",
                                ent == 11 ~ "Guanajuato",
                                ent == 12 ~ "Guerrero",
                                ent == 13 ~ "Hidalgo",
                                ent == 14 ~ "Jalisco",
                                ent == 15 ~ "México",
                                ent == 16 ~ "Michoacán",
                                ent == 17 ~ "Morelos",
                                ent == 18 ~ "Nayarit",
                                ent == 19 ~ "Nuevo León",
                                ent == 20 ~ "Oaxaca",
                                ent == 21 ~ "Puebla",
                                ent == 22 ~ "Querétaro",
                                ent == 23 ~ "Quintana Roo",
                                ent == 24 ~ "San Luis Potosí",
                                ent == 25 ~ "Sinaloa",
                                ent == 26 ~ "Sonora",
                                ent == 27 ~ "Tabasco",
                                ent == 28 ~ "Tamaulipas",
                                ent == 29 ~ "Tlaxcala",
                                ent == 30 ~ "Veracruz",
                                ent == 31 ~ "Yucatán",
                                ent == 32 ~ "Zacatecas"),
           reg_num = case_when(
             (ent == 26) | (ent == 25) | (ent == 02) | (ent == 03) | (ent == 18) ~ 1,
             (ent == 05) | (ent == 08) | (ent == 10) | (ent == 32) | (ent == 24) ~ 2,
             (ent == 28) | (ent == 19) ~ 3,
             (ent == 01) | (ent == 14) | (ent == 11) | (ent == 06) | (ent == 16) ~ 4,
             (ent == 22) | (ent == 15) | (ent == 09) | (ent == 13) | (ent == 17) | (ent == 29) | (ent == 21) ~ 5,
             (ent == 12) | (ent == 20) | (ent == 07) ~ 6,
             (ent == 27) | (ent == 30) ~ 7,
             (ent == 04) | (ent == 23) | (ent == 31) ~ 8),
           reg_name = case_when(
             reg_num == 1 ~ "Northwest",
             reg_num == 2 ~ "North",
             reg_num == 3 ~ "Northeast",
             reg_num == 4 ~ "Center-West",
             reg_num == 5 ~ "Center-East",
             reg_num == 6 ~ "South",
             reg_num == 7 ~ "East",
             reg_num == 8 ~ "Peninsula"),
           macro_num = case_when( 
             (ent == 26) | (ent == 25) | (ent == 02) | (ent == 03) | (ent == 18) | 
               (ent == 05) | (ent == 08) | (ent == 10) | (ent == 32) | (ent == 24) |
               (ent == 28) | (ent == 19) ~ 1,
             (ent == 01) | (ent == 14) | (ent == 11) | (ent == 06) | (ent == 16) |
               (ent == 22) | (ent == 15) | (ent == 09) | (ent == 13) | (ent == 17) | 
               (ent == 29) | (ent == 21) ~ 2,
             (ent == 12) | (ent == 20) | (ent == 07) ~ 3,
             (ent == 27) | (ent == 30) | (ent == 04) | (ent == 23) | (ent == 31) ~ 4),
           macro_name = case_when(
             macro_num == 1 ~ "Northern",
             macro_num == 2 ~ "Central",
             macro_num == 3 ~ "South",
             macro_num == 4 ~ "Eastern"))
  
  dt_list[[paste0("dt", i)]] <- dt_i
  
}

rm(list = setdiff(ls(), c("dt_list", "years")))

# II: Time-comparable MEP weights ----

dp_cols <- c("dp_hill", "dp_light", "dp_cook", "dp_refri",
             "dp_entr", "dp_com", "dp_wt", "dp_clim")

dt_pooled <- map_dfr(years, function(i) {
  dt_list[[paste0("dt", i)]] %>%
    select(all_of(dp_cols), est_dis, upm, factor, weather) %>%
    mutate(
      year       = as.integer(i),
      # Make strata and PSU unique across years
      strata_pool = paste(i, est_dis, sep = "_"),
      psu_pool    = paste(i, upm,     sep = "_")
    )
}) %>%
  # Remove temperate climate (dp_clim undefined)
  filter(weather %in% c(1, 3)) %>%
  # Remove any remaining NAs across all 7 dimensions
  filter(if_all(all_of(dp_cols), ~ !is.na(.x)))

# Sanity check
dt_pooled %>%
  group_by(year) %>%
  summarise(
    n_individuals = n(),
    w_total       = sum(factor, na.rm = TRUE),
    dp_hill       = weighted.mean(dp_hill,  factor, na.rm = TRUE)*100,
    dp_light      = weighted.mean(dp_light, factor, na.rm = TRUE)*100,
    dp_cook       = weighted.mean(dp_cook,  factor, na.rm = TRUE)*100,
    dp_refri      = weighted.mean(dp_refri, factor, na.rm = TRUE)*100,
    dp_entr       = weighted.mean(dp_entr,  factor, na.rm = TRUE)*100,
    dp_com        = weighted.mean(dp_com,  factor, na.rm = TRUE)*100,
    dp_wt         = weighted.mean(dp_wt,    factor, na.rm = TRUE)*100,
    dp_clim       = weighted.mean(dp_clim,  factor, na.rm = TRUE)*100
  ) %>%
  print()

## Weighted MCA on pooled individual-level data ----

# Normalize individual survey weights to sum to 1
w_norm <- dt_pooled$factor / sum(dt_pooled$factor, na.rm = TRUE)

# Prepare deprivation matrix as factors
dep_matrix <- dt_pooled %>%
  select(all_of(dp_cols)) %>%
  mutate(across(everything(), ~ factor(.x, levels = c(0, 1),
                                       labels = c("0", "1")))) %>%
  as.data.frame()

# Run weighted MCA keeping 2 axes for diagnostics
mca_res <- MCA(dep_matrix,
               row.w  = w_norm,
               ncp    = 5,
               graph  = FALSE)
summary(mca_res)

# ── Diagnostics ────────────────────────────────────────────────

cat("Variance explained:\n")
print(round(mca_res$eig, 4))

cat("\nColumn coordinates on axis 1 (all modalities):\n")
print(round(mca_res$var$coord[, 1], 4))

cat("\nv.test on axis 1:\n")
print(round(mca_res$var$v.test[, 1], 3))

cat("\nContribution to axis 1 variance (%):\n")
print(round(mca_res$var$contrib[, 1], 3))

## Extract fixed weight vector from axis 1 (MCA — robustness scheme) ----

# Extract coordinates for deprived modality ("_1") only
coords  <- mca_res$var$coord[, 1]
dep_idx <- str_detect(names(coords), "_1$")
coords_dep <- coords[dep_idx]

cat("Raw coordinates for deprived modalities on axis 1:\n")
print(round(sort(coords_dep, decreasing = TRUE), 4))

# Normalize to sum to 1 (MCA weights — used in robustness comparisons)
w_j_mca <- abs(coords_dep) / sum(abs(coords_dep))
names(w_j_mca) <- str_remove(names(w_j_mca), "_1$")

cat("\nNormalized MCA weights (w_j_mca), sum =", round(sum(w_j_mca), 6), ":\n")
print(round(sort(w_j_mca, decreasing = TRUE), 6))

cat("\nmin(w_j_mca):\n")
print(round(min(w_j_mca), 4))

cat("\nmax(w_j_mca):\n")
print(round(max(w_j_mca), 4))

## MCA diagnostics (saved for reporting) ----
mca_diagnostics <- list(
  eig      = as.data.frame(round(mca_res$eig[seq_len(min(8L, nrow(mca_res$eig))), ], 4)),
  loadings = as.data.frame(round(mca_res$var$coord[, 1:2, drop = FALSE], 4)),
  contrib  = as.data.frame(round(mca_res$var$contrib[, 1, drop = FALSE], 4)),
  v_test   = as.data.frame(round(mca_res$var$v.test[, 1, drop = FALSE], 4))
)

## Main weighting scheme: Nussbaumer-style three-tier expert judgment (NB) ----
# Tier 1 (basic energy services — heat, light, cooking, water heating): Ea, Ck, Wt
# Tier 2 (modern services contingent on electricity): Rf, Et, Cm
# Tier 3 (conditional / contextual — climate-dependent or income-dependent): Tc, Af
#
# Ratio between adjacent tiers: w_t / w_{t+1} = 0.20 / 0.13 ≈ 1.538  (Nussbaumer et al., 2012)
# Tier 3 weight is set strictly below the poverty cutoff k = 0.10, so that a single
# Tier-3 deprivation can never single-handedly identify a household as energy poor —
# Tier-3 dimensions (Af, Tc) only add to intensity once the household is already poor
# on other dimensions. This addresses the well-known concern that the Hills (2011, 2012)
# affordability indicator is essentially income poverty conflated with energy expenditures.
#
# Solving 3 w_1 + 3 w_2 + 2 w_3 = 1 with w_1 / w_2 = w_2 / w_3 = 1.538:
nb_ratio <- 0.20 / 0.13
nb_w2    <- 1 / (3 * nb_ratio + 3 + 2 / nb_ratio)   # ≈ 0.1122
nb_w1    <- nb_ratio * nb_w2                         # ≈ 0.1726
nb_w3    <- nb_w2 / nb_ratio                         # ≈ 0.0729

w_j <- c(
  dp_light = nb_w1,   # Ea — Tier 1
  dp_cook  = nb_w1,   # Ck — Tier 1
  dp_wt    = nb_w1,   # Wt — Tier 1
  dp_refri = nb_w2,   # Rf — Tier 2
  dp_entr  = nb_w2,   # Et — Tier 2
  dp_com   = nb_w2,   # Cm — Tier 2
  dp_hill  = nb_w3,   # Af — Tier 3
  dp_clim  = nb_w3    # Tc — Tier 3
)
# Re-normalize to ensure exact sum to 1
w_j <- w_j / sum(w_j)

cat("\nMain (Nussbaumer-style three-tier) weights w_j, sum =", round(sum(w_j), 6), ":\n")
print(round(sort(w_j, decreasing = TRUE), 6))

# 7-dimension renormalization for temperate climate (drop dp_clim)
w_j_7dim <- w_j[names(w_j) != "dp_clim"]
w_j_7dim <- w_j_7dim / sum(w_j_7dim)

## Alternative weight vectors (robustness) ----

# 1. MCA 7-dim renormalization for temperate
w_j_mca_7 <- w_j_mca[names(w_j_mca) != "dp_clim"]
w_j_mca_7 <- w_j_mca_7 / sum(w_j_mca_7)

# 2. Equal weights (1/8 per dim for hot/cold; 1/7 for temperate)
w_j_eq   <- setNames(rep(1/8, 8), names(w_j))
w_j_eq_7 <- setNames(rep(1/7, 7), names(w_j_eq)[names(w_j_eq) != "dp_clim"])

# 3. Frequency-based weights: w_j ∝ (1 − p̄_j)
#    p̄_j = pooled weighted deprivation rate (hot/cold sample, same as MCA pool)
p_j_w    <- sapply(dp_cols, function(col)
  weighted.mean(dt_pooled[[col]], dt_pooled$factor, na.rm = TRUE))
w_j_freq   <- (1 - p_j_w) / sum(1 - p_j_w)
w_j_freq_7 <- w_j_freq[names(w_j_freq) != "dp_clim"]
w_j_freq_7 <- w_j_freq_7 / sum(w_j_freq_7)

cat("\nDeprivation rates used for frequency weights (p̄_j):\n")
print(round(sort(p_j_w, decreasing = TRUE), 4))
cat("\nFrequency-based weights (w_j_freq):\n")
print(round(sort(w_j_freq, decreasing = TRUE), 6))

# 4. NB three-tier scheme excluding the affordability dimension entirely (NB-noAf).
#    Used in Section 4 to demonstrate that the headline trends are not artifacts of
#    the Hills-style affordability indicator (reviewer concern: circularity with
#    CONEVAL's monetary poverty line).
#    Configuration: 3 Tier-1 (Ea, Ck, Wt) + 3 Tier-2 (Rf, Et, Cm) + 1 Tier-3 (Tc)
#    Solving 3 w_1 + 3 w_2 + w_3 = 1 with w_1/w_2 = w_2/w_3 = 1.538:
nb_noaf_w2 <- 1 / (3 * nb_ratio + 3 + 1 / nb_ratio)   # ≈ 0.1210
nb_noaf_w1 <- nb_ratio * nb_noaf_w2                    # ≈ 0.1861
nb_noaf_w3 <- nb_noaf_w2 / nb_ratio                    # ≈ 0.0786

w_j_noaf <- c(
  dp_light = nb_noaf_w1,   # Ea — Tier 1
  dp_cook  = nb_noaf_w1,   # Ck — Tier 1
  dp_wt    = nb_noaf_w1,   # Wt — Tier 1
  dp_refri = nb_noaf_w2,   # Rf — Tier 2
  dp_entr  = nb_noaf_w2,   # Et — Tier 2
  dp_com   = nb_noaf_w2,   # Cm — Tier 2
  dp_hill  = 0,            # Af — DROPPED
  dp_clim  = nb_noaf_w3    # Tc — Tier 3
)
w_j_noaf <- w_j_noaf / sum(w_j_noaf)

# Temperate (no Tc, no Af): just 3 + 3 = 6 dims, both tiers above k.
nb_noaf_t_w2 <- 1 / (3 * nb_ratio + 3)
nb_noaf_t_w1 <- nb_ratio * nb_noaf_t_w2
w_j_noaf_7 <- c(
  dp_light = nb_noaf_t_w1, dp_cook  = nb_noaf_t_w1, dp_wt    = nb_noaf_t_w1,
  dp_refri = nb_noaf_t_w2, dp_entr  = nb_noaf_t_w2, dp_com   = nb_noaf_t_w2,
  dp_hill  = 0
)
w_j_noaf_7 <- w_j_noaf_7 / sum(w_j_noaf_7)

cat("\nNB-noAf weights (Af dropped), 8-dim configuration:\n")
print(round(sort(w_j_noaf, decreasing = TRUE), 6))

p_load("srvyr")

## MEP scoring helper (supports multiple weight schemes) ----
.add_mep <- function(dt, w8, w7, k, sfx = "") {
  sc  <- if (nchar(sfx) == 0L) "mep_score"     else paste0("mep_score_", sfx)
  po  <- if (nchar(sfx) == 0L) "mep"           else paste0("mep_",       sfx)
  raw <- if (nchar(sfx) == 0L) "mep_score_raw" else paste0("mep_score_", sfx, "_raw")

  score <- case_when(
    dt$weather %in% c(1L, 3L) ~
      dt$dp_hill  * w8["dp_hill"]  + dt$dp_light * w8["dp_light"] +
      dt$dp_cook  * w8["dp_cook"]  + dt$dp_refri * w8["dp_refri"] +
      dt$dp_entr  * w8["dp_entr"]  + dt$dp_com   * w8["dp_com"]   +
      dt$dp_wt    * w8["dp_wt"]    + dt$dp_clim  * w8["dp_clim"],
    dt$weather == 2L ~
      dt$dp_hill  * w7["dp_hill"]  + dt$dp_light * w7["dp_light"] +
      dt$dp_cook  * w7["dp_cook"]  + dt$dp_refri * w7["dp_refri"] +
      dt$dp_entr  * w7["dp_entr"]  + dt$dp_com   * w7["dp_com"]   +
      dt$dp_wt    * w7["dp_wt"],
    TRUE ~ NA_real_
  )

  dt[[po]]  <- ifelse(score >= k, 1, 0)
  dt[[sc]]  <- ifelse(dt[[po]] == 1, score, 0)
  dt[[raw]] <- score   # Continuous score (no k applied) — used by k sensitivity
  dt
}

# III: Final dataset ----

cat("Main (Nussbaumer-style) weights for 8-dim households (hot/cold climate):\n")
print(round(sort(w_j, decreasing = TRUE), 4))

cat("\nRenormalized weights for 7-dim households (temperate):\n")
print(round(sort(w_j_7dim, decreasing = TRUE), 4))

# Cutoffs
k  <- 0.10

MEP <- map_dfr(years, function(i) {

  yr <- paste0("dt", i)
  dt <- dt_list[[yr]]

  dt <- .add_mep(dt, w_j,        w_j_7dim,    k)            # NB weights (MAIN)
  dt <- .add_mep(dt, w_j_mca,    w_j_mca_7,   k, "mca")     # MCA (robustness)
  dt <- .add_mep(dt, w_j_eq,     w_j_eq_7,    k, "eq")      # Equal weights
  dt <- .add_mep(dt, w_j_freq,   w_j_freq_7,  k, "freq")    # Frequency-based
  dt <- .add_mep(dt, w_j_noaf,   w_j_noaf_7,  k, "noaf")    # Af dropped (Section 4)
  dt <- dt %>% mutate(mep_u = ifelse(mep_score >= 0, 1, 0))

  dt_list[[yr]] <<- dt

})

## Deflation ----

inpc <- read.csv("inputs/additional/inpc.csv") %>%
  select(Año_int, Mes, Deflator) %>%
  rename(year = Año_int, mes = Mes)

aug_deflators <- inpc %>%
  filter(mes == 8) %>%
  select(year, Deflator) 

for (i in years) {
  yr   <- paste0("dt", i)
  defl <- aug_deflators %>% filter(year == i) %>% pull(Deflator)
  
  dt_list[[yr]] <- dt_list[[yr]] %>%
    mutate(
      ictpc_r      = ictpc      / defl,
      ach_ictpc_r  = ach_ictpc  / defl,
      energy_exp_r = energy_exp / defl,
      ictpc_l_r    = log(ictpc_r)
    )
}

rm(list = setdiff(ls(), c("dt_list",
                           "w_j",       "w_j_7dim",
                           "w_j_mca",   "w_j_mca_7",
                           "w_j_eq",    "w_j_eq_7",
                           "w_j_freq",  "w_j_freq_7",
                           "w_j_noaf",  "w_j_noaf_7",
                           "mca_diagnostics")))

save(dt_list,
     w_j,        w_j_7dim,
     w_j_mca,    w_j_mca_7,
     w_j_eq,     w_j_eq_7,
     w_j_freq,   w_j_freq_7,
     w_j_noaf,   w_j_noaf_7,
     mca_diagnostics,
     file = "outputs/intermediate/dt.RData")
