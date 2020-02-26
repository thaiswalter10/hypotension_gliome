setwd("/Users/louisperdriel/OneDrive - UNIVERSITE PARIS DESCARTES/Troisieme_cycle/Recherche_clinique/Hypotension_per_op_ischemie/Donnees")
data <- read.csv2("gliome_HD.csv", na.string="NA")

### ---- Librairies -----------
library(lubridate)
library(chron)
library (tidyverse)
library(dplyr)

### ---- Sous bases de données ----

# Patients dont j'ai pu récupérer les données anesthésiques per-op
data %>%
  filter (!is.na(heure_fin), !is.na(asa)) -> data

### ---- Variables d'intérêt ------------------------

# Recodage variable ML...IOT
data <- data %>% mutate_at("ML...IOT", as.character)
data <- data %>% 
  mutate(IOT
     = case_when(
      ML...IOT != "VS" ~ "IOT", TRUE~"VS"
    )
  )

# variables psychotropes selon Etienne
data$psychotrope <- 0
data$psychotrope[is.na(data$benzodiazepines)==F & data$benzodiazepines=="oui"] <- 1
data$psychotrope[is.na(data$anti_epileptiques)==F & data$anti_epileptiques=="oui"] <- 1
data$psychotrope[is.na(data$neurolpetiques_1G)==F & data$neurolpetiques_1G=="oui"] <- 1
data$psychotrope[is.na(data$neurolpetiques_2G)==F & data$neurolpetiques_2G=="oui"] <- 1
data$psychotrope[is.na(data$thymoregulateurs)==F & data$thymoregulateurs=="oui"] <- 1
data$psychotrope[is.na(data$opioides)==F & data$opioides=="oui"] <- 1
  
# Création d'une variable age 
data$date_naissance_D <-  chron (as.character(data$date_naissance), format = c(dates = "d.m.y"))
data$date_chirurgie_D <-  chron (as.character(data$date_chirugie), format = c(dates = "d.m.y"))
data$age <- trunc((data$date_chirurgie_D - data$date_naissance_D)/365.25)

# Création de la variable asa = 2 permettant de comparer les cohortes pour mon tableau description de la population
data <- data %>%
  mutate (asa_2 = case_when (asa == 2 ~ 1, TRUE ~ 0))
# Separation de la variable date_chirurgie_D en année et mois séparés
data <- data %>% 
  mutate (date_chirurgie_D_datetime = dmy(date_chirurgie_D)) %>%
  mutate (year = year(date_chirurgie_D_datetime), month = month(date_chirurgie_D_datetime))

# Définition d'une variable anti-hypertenseur
data <- data %>%
  mutate (anti_hypertenseur = case_when(inh_calc == "oui" | bb == "oui" | diuretique == "oui" | iec == "oui" ~ "oui", TRUE ~ "non"))

# Recodage variable HTA
data$hta [data$hta %in% c('oui')] <- 'OUI'
data$hta [data$hta %in% c(NA)] <- 'NON'

# Recodage de la variable sexe
data$gender <- ifelse (data$sexe_1F_0H == 1,2,1)

# Conversion créatininémie de µmol/L en mg/dL
data$creat_preop_mg <- data$creat_preop / 88.4

#Calcul DFG 
data$kappa <- ifelse(data$gender==1,0.9,0.7)
data$alpha <- ifelse(data$gender==2,-0.329,-0.411)
data$fact <- ifelse(data$gender==2,1.018,1)

data$dfg_ckdepi <- 141 * pmin (data[,"creat_preop_mg"]/data[,"kappa"] ,1) ^ data[,"alpha"] *
  pmax(data[,"creat_preop_mg"]/data[,"kappa"],1)^-1.209 * 0.993^data[,"age"] * data[,"fact"]

data$dfg_ckdepi <- as.double(data$dfg_ckdepi)


# Classification des patients en fonction du stade d'insuffisance rénale pré-opératoire
data$dfg_ckdepi_std <- data$dfg_ckdepi
data$dfg_ckdepi_std <- ifelse((data$dfg_ckdepi_std >= 90 & data$dfg_ckdepi_std <= 230), "normale", data$dfg_ckdepi_std)
data$dfg_ckdepi_std <- ifelse((data$dfg_ckdepi_std >= 60 & data$dfg_ckdepi_std < 90), "légère", data$dfg_ckdepi_std)
data$dfg_ckdepi_std <- ifelse((data$dfg_ckdepi_std >= 30  & data$dfg_ckdepi_std < 60), "moderee", data$dfg_ckdepi_std)
data$dfg_ckdepi_std <- ifelse((data$dfg_ckdepi_std >= 15 & data$dfg_ckdepi_std < 30), "severe", data$dfg_ckdepi_std)
data$dfg_ckdepi_std <- ifelse((data$dfg_ckdepi_std >= 0 & data$dfg_ckdepi_std < 15), "normale", data$dfg_ckdepi_std)


#recodage variable tbc
data$intox_tabac <- ifelse ((is.na(data$intox_tabac) == T | data$intox_tabac == "non"), "non", "oui")

#recodage variable diabete
data$diabete_t2 <- ifelse ((is.na(data$dt_insulinoreq) == T | data$diabete_t2 == "non"), "non", "oui")

### ---- Recodage de tous les traitements psychotropes afin que les NA deviennent des "non" ----
# Recodage variable benzodiazepines 
data <- data %>% 
  mutate(
    benzodiazepines = case_when(
      is.na(benzodiazepines) == TRUE ~ "non", 
      TRUE ~ "oui"
    )
  )

# Recodage variable anti_epileptiques
data <- data %>% 
  mutate(
    anti_epileptiques = case_when(
      is.na(anti_epileptiques) == TRUE ~ "non", 
      TRUE ~ "oui"
    )
  )

# Recodage variable anti_h1
data <- data %>% 
  mutate(
    anti_h1 = case_when(
      is.na(anti_h1) == TRUE ~ "non", 
      TRUE ~ "oui"
    )
  )

# Recodage variable antidepresseurs
data <- data %>% 
  mutate(
    antidepresseurs = case_when(
      is.na(antidepresseurs) == TRUE ~ "non", 
      TRUE ~ "oui"
    )
  )

# Recodage variable neurolpetiques_1G
data <- data %>% 
  mutate(
    neurolpetiques_1G = case_when(
      is.na(neurolpetiques_1G) == TRUE ~ "non", 
      TRUE ~ "oui"
    )
  )

# Recodage variable neurolpetiques_2G
data <- data %>% 
  mutate(
    neurolpetiques_2G = case_when(
      is.na(neurolpetiques_2G) == TRUE ~ "non", 
      TRUE ~ "oui"
    )
  )

# Recodage variable thymoregulateurs
data <- data %>% 
  mutate(
    thymoregulateurs = case_when(
      is.na(thymoregulateurs) == TRUE ~ "non", 
      TRUE ~ "oui"
    )
  )

# Recodage variable opioides
data <- data %>% 
  mutate(
    opioides = case_when(
      is.na(opioides) == TRUE ~ "non", 
      TRUE ~ "oui"
    )
  )



### ---- Gestion péri-opératoire ------------------------
# La quantité d'expansion volémique est en pochon de 500mL. Je crée une nouvelle variable rapportant l'exp_vol en mL
data <- data %>%
  mutate(exp_volem_mL = Exp_volem*500)

# Création d'une variable transufions per-op oui non 
data <-  data %>%
  mutate (transfu_y1_n0 = case_when(transfu == 0 ~ 0, TRUE ~ 1))

# Création d'une variable: recours à un agent vaso-constricteur
data <- data %>%
  mutate (
    vaso_c_drug = case_when(v_max_nad == 0 & ephedrine == 0 ~ FALSE, TRUE ~ TRUE),
    nad_b = case_when(v_max_nad == 0 ~ 0, TRUE ~ 1),
    ephedrine_b = case_when(ephedrine == 0 ~ 0, TRUE~1)
  )

# Dose moyenne max en microgramme kilo minute
data$max_nad_microg_kg_min <- data$v_max_nad*5/data$pds/60

# Microscope
data <- data %>%
  mutate(microscope = case_when(is.na(microscope) ~ 0, TRUE ~ 1))

# Durée chirurgie binaire
data$duree_interv_min <- as.numeric(as.character(data$duree_interv_min))
data <- data %>% mutate(short_surg = case_when(duree_interv_min < 350 ~ 1, TRUE ~ 0))

### ---- Conversion factor in numeric ----
data$grade <- as.character(data$grade)

### ---- Déficit cognitif significatif ------------------------
# Déficit cognitif pré-opératoire
# Définition pour chacun des 7 tests cognitifs d'une variable var, définie par le résultat 
# du test cog pré-op moins le résultat du test cog post-op en déviation standard
# Si var est > 1,5 on dit que le patient présente un déficit cognitif post-opératoire
# Définition dans un second temps d'une variable var_b prenant 1 si var >= 1,5, 0 sinon

data <- data %>%
  mutate (
    age_old_b = case_when((age >=45)~1, TRUE ~ 0),
    
    def_cog_preop = case_when (span_endroit_SD_pre < -1.65 | span_envers_SD_pre < -1.65 |FLUs_SD_pre_EM < -1.65 | 
                                 FLUp_SD_pre_EM < -1.65 | TMTA_SD_pre_EM < -1.65 | TMTB_SD_pre_EM < -1.65 |
                                 TMTB_A_SD_pre_EM < -1.65 ~ 1, 
                               span_endroit_SD_pre > -1.65 & span_envers_SD_pre > -1.65 & FLUs_SD_pre_EM > -1.65 & 
                                 FLUp_SD_pre_EM > -1.65 & TMTA_SD_pre_EM > -1.65 & TMTB_SD_pre_EM > -1.65 &
                                 TMTB_A_SD_pre_EM > -1.65 ~ 0, 
                               TRUE ~ NA_real_),
    var_span_endroit_SD = span_endroit_SD_pre - span_endroit_SD_post,
    var_span_envers_SD = span_envers_SD_pre - span_envers_SD_post,
    var_FLUs_SD = FLUs_SD_pre_EM - FLUs_SD_post_EM,
    var_FLUp_SD = FLUp_SD_pre_EM - FLUs_SD_post_EM,
    var_TMTA_SD = TMTA_SD_pre_EM - TMTA_SD_post_EM,
    var_TMTB_SD = TMTB_SD_pre_EM - TMTB_SD_post_EM,
    var_TMTB_A_SD = TMTB_A_SD_pre_EM - TMTB_A_SD_post_EM,

    var_span_endroit_SD_b = case_when (var_span_endroit_SD >= 1.5 ~ 1, var_span_endroit_SD < 1.5 ~ 0, TRUE ~ NA_real_),
    var_span_envers_SD_b = case_when (var_span_envers_SD >= 1.5 ~ 1, var_span_envers_SD <1.5 ~ 0, TRUE ~ NA_real_),
    var_FLUs_SD_b = case_when (var_FLUs_SD >= 1.5 ~ 1, var_FLUs_SD < 1.5 ~ 0, TRUE ~ NA_real_),
    var_FLUp_SD_b = case_when (var_FLUp_SD >= 1.5 ~ 1, var_FLUp_SD < 1.5 ~ 0, TRUE ~ NA_real_),
    var_TMTA_SD_b = case_when (var_TMTA_SD >= 1.5 ~ 1, var_TMTA_SD < 1.5 ~ 0, TRUE ~ NA_real_),
    var_TMTB_SD_b = case_when (var_TMTB_SD >= 1.5 ~ 1, var_TMTB_SD < 1.5 ~ 0, TRUE ~ NA_real_),
    var_TMTB_A_SD_b = case_when (var_TMTB_A_SD >= 1.5 ~ 1, var_TMTB_A_SD < 1.5 ~ 0, TRUE ~ NA_real_),
    
    cognitif_test_total_score = var_span_endroit_SD_b + var_span_envers_SD_b + var_FLUs_SD_b +
      var_FLUp_SD_b + var_TMTA_SD_b + var_TMTB_SD_b + var_TMTB_A_SD_b,
    cog_bin_outcome = case_when (cognitif_test_total_score >=1 ~ 1, TRUE ~ 0),

    all_cog_test_available = case_when(!is.na(span_endroit_SD_pre) &
                               !is.na(span_endroit_SD_post) &
                               !is.na(span_envers_SD_pre) &
                               !is.na(span_envers_SD_post) &
                               !is.na(FLUp_SD_pre_EM) &
                               !is.na(FLUp_SD_post_EM) &
                               !is.na(FLUs_SD_pre_EM) &
                               !is.na(FLUs_SD_post_EM) &
                               !is.na(TMTA_SD_pre_EM) & 
                               !is.na(TMTA_SD_post_EM) & 
                               !is.na(TMTB_SD_pre_EM) & 
                               !is.na(TMTB_SD_post_EM) &
                               !is.na(TMTB_A_SD_pre_EM) &
                               !is.na(TMTB_A_SD_post_EM)~1, TRUE ~ 0),
    span_endroit_available = case_when(is.na(var_span_endroit_SD_b) ~ 0, TRUE ~ 1),
    span_envers_available = case_when(is.na(var_span_envers_SD_b) ~ 0, TRUE ~ 1),
    FLUp_available = case_when(is.na(var_FLUp_SD_b) ~ 0, TRUE ~ 1),
    FLUs_available = case_when(is.na(var_FLUs_SD_b) ~ 0, TRUE ~ 1),
    TMTA_available = case_when(is.na(var_TMTA_SD_b) ~ 0, TRUE ~ 1),
    TMTB_available = case_when(is.na(var_TMTB_SD_b) ~ 0, TRUE ~ 1),
    TMTB_A_available = case_when(is.na(var_TMTB_A_SD_b) ~ 0, TRUE ~ 1),
    
    am_fct_cog = case_when(var_span_endroit_SD<= -1.5 | var_span_envers_SD<= -1.5 | 
                                       var_FLUs_SD<= -1.5 | var_FLUp_SD<= -1.5 | var_TMTA_SD<= -1.5 | 
                                       var_TMTB_SD<= -1.5 | var_TMTB_A_SD<= -1.5 ~ 1, 
                                     var_span_endroit_SD> -1.5 & var_span_envers_SD> -1.5 & 
                                       var_FLUs_SD> -1.5 & var_FLUp_SD> -1.5 & var_TMTA_SD> -1.5 & 
                                       var_TMTB_SD> -1.5 & var_TMTB_A_SD> -1.5 ~ 0, TRUE ~ NA_real_),
    
    am_var_span_endroit_SD_b = case_when(is.na(var_span_endroit_SD) ~ NA_real_, var_span_endroit_SD <= -1.5 ~ 1, TRUE ~ 0),
    am_var_span_envers_SD_b = case_when(is.na(var_span_envers_SD) ~ NA_real_, var_span_envers_SD <= -1.5 ~ 1, TRUE ~ 0),
    am_var_FLUs_SD_b = case_when(is.na(var_FLUs_SD) ~ NA_real_, var_FLUs_SD <= -1.5 ~ 1, TRUE ~ 0),
    am_var_FLUp_SD_b = case_when(is.na(var_FLUp_SD) ~ NA_real_, var_FLUp_SD <= -1.5 ~ 1, TRUE ~ 0),
    am_var_TMTA_SD_b = case_when(is.na(var_TMTA_SD) ~ NA_real_, var_TMTA_SD <= -1.5 ~ 1, TRUE ~ 0), 
    am_var_TMTB_SD_b = case_when(is.na(var_TMTB_SD) ~ NA_real_, var_TMTB_SD <= -1.5 ~ 1, TRUE ~ 0), 
    am_var_TMTB_A_SD_b = case_when(is.na(var_TMTB_A_SD) ~ NA_real_, var_TMTB_A_SD <= -1.5 ~ 1, TRUE ~ 0), 
    
    hypotension_b = case_when(nomb_pas_inf_85p > 0 ~ 1, TRUE ~ 0),
    hypo_longue = case_when (duree_pas_inf_85p>100 ~ 1, TRUE ~ 0),
    
    nomb_recidive_b = case_when(nomb_recidive >= 1 ~ 1, TRUE ~ 0),
    
    grade_b = case_when( grade == "GBM" ~ "haut", TRUE ~ "bas")
  )


### ---- Tableau données d'intérêt ----
data_1 <- data %>%
  select(nom, nip, age, age_old_b, date_chirurgie_D, span_endroit_SD_pre, span_envers_SD_pre, FLUp_SD_pre_EM, FLUs_SD_pre_EM, TMTA_SD_pre_EM, TMTB_SD_pre_EM, TMTB_A_SD_pre_EM, span_endroit_SD_post, span_envers_SD_post, FLUp_SD_post_EM, FLUs_SD_post_EM, TMTA_SD_post_EM, TMTB_SD_post_EM, TMTB_A_SD_post_EM, var_span_endroit_SD_b, var_span_envers_SD_b, var_FLUs_SD_b, var_FLUp_SD_b, var_TMTA_SD_b, var_TMTB_SD_b, var_TMTB_A_SD_b, span_endroit_available, span_envers_available, FLUp_available, FLUs_available, TMTA_available, TMTB_available, TMTB_A_available, all_cog_test_available, def_cog_preop, cog_bin_outcome, am_fct_cog, date_naissance:duree_pas_inf_85p, short_surg, hta, v_max_nad:diabete_t2, bpco,corticoides:demence, epilepsie_2R:creat_preop_mg,
   
  dfg_ckdepi, dfg_ckdepi_std, grade_b, hypo_longue, psychotrope, benzodiazepines, anti_epileptiques, anti_h1, antidepresseurs, neurolpetiques_1G, neurolpetiques_2G, thymoregulateurs, opioides,
  
   exp_volem_mL, 
   transfu_y1_n0, vaso_c_drug, nad_b, ephedrine_b, max_nad_microg_kg_min, month, year, nomb_recidive_b, hypotension_b, asa_2, IOT)

write.csv2(data_1, file="gliome_HD_tidy_150220_v2.csv")

