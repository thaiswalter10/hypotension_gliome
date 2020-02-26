### Ce script me permet de regrouper toutes mes fonctions nécessaire à l'analyse descriptive et d'association de la 
### base de donnée gliome_HD

### ------------------- Libraries ------------------------------------------------------

library(tidyverse)


### ------------------- Functions ------------------
# Am besten hätte ich so eine function: 
stat_des <- function(df, column_name){
  df %>%
    summarise(column_name_mean = mean(column_name), column_name_sd = sd(column_name))
}

# Statdessen musste ich ein function für jede Variabel schreiben, nur das data frame konnte 
#als Variable genommen worden. 
age <- function (df, na.rm = TRUE) {
  df %>%
    summarise(age_mean = mean(age), age_sd = sd(age))
}

age_old <- function(df, na.rm = TRUE){
  df %>%
    mutate (older_45 = ifelse (age >= 45, "yes", "no")) %>%
    group_by(older_45) %>% 
    summarise(age_mean = mean(age), age_sd = sd(age))
}

age_old_b <- function(df){
  df <- df %>%
    mutate(age_old_b = case_when((age >=45)~1, TRUE ~ 0))
  df$age_old_b
}


age_old_prop <- function (df, na.rm = TRUE){
  df %>%
    group_by (older_than_45 = age >= 45) %>%
    summarise (n = n()) %>%
    mutate (prop = n/sum(n))
}

age_old_pr_htest <-  function(df){
  df %>%
    filter(age >= 45) %>%
    return(age)
}

sex <- function (df) { 
  df %>%
    group_by(sexe_1F_0H) %>%
    summarise(n = n()) %>%
    mutate (prop = n/sum(n))
}

height <- function (df, na.rm = TRUE) {
  df %>%
    filter(!is.na(taille)) %>%
    summarise(height_mean = mean(taille), height_sd = sd(taille))
}

imc <- function (df, na.rm = TRUE) {
  df %>%
    filter(!is.na(imc)) %>%
    summarise(imc_mean = mean(imc), imc_sd = sd(imc))
}

asa <- function (df) { 
  df %>%
    filter(!is.na(asa)) %>%
    group_by(asa) %>%
    summarise(tot = n()) %>%
    mutate (prop = tot/sum(tot))
}

asa_2 <- function (df){
  df <- df %>%
    mutate (asa_2 = case_when ((asa == 2)~1, TRUE~0))
  df$asa_2
}

pas_bloc <- function (df) {
  df %>%
    summarise (pas_bloc_mean = mean (pas_arrivee_bloc), pas_bloc_sd = sd(pas_arrivee_bloc))
}

hta <- function (df) { 
  df %>%
    group_by(hta) %>%
    summarise(tot = n()) %>%
    mutate (prop = tot/sum(tot))
}

creat <- function (df) {
  df %>%
    summarise (creat_mean = mean(creat_preop, na.rm = TRUE), creat_sd = sd(creat_preop, na.rm = TRUE))
}

dfg <- function (df) {
  df %>%
    filter(!is.na(dfg_ckdepi)) %>%
    summarise (dfg_mean = mean (dfg_ckdepi), dfg_sd = sd(dfg_ckdepi))
}

anti_hypertenseur <- function (df) { 
  df %>%
    group_by(anti_hypertenseur) %>%
    summarise(n = n()) %>%
    mutate (prop = n/sum(n))
}

bb <- function (df) { 
  df %>%
    group_by(bb) %>%
    summarise(n = n()) %>%
    mutate (prop = n/sum(n))
}

diuretique <- function (df) { 
  df %>%
    group_by(diuretique) %>%
    summarise(n = n()) %>%
    mutate (prop = n/sum(n))
}

iec <- function (df) { 
  df %>%
    group_by(iec) %>%
    summarise(n = n()) %>%
    mutate (prop = n/sum(n))
}

epilepsie_2R <- function (df) { 
  df %>%
    group_by(epilepsie_2R) %>%
    summarise(n = n()) %>%
    mutate (prop = n/sum(n))
}

localisation <- function (df) { 
  df %>%
    group_by(localisation) %>%
    summarise(n = n()) %>%
    mutate (prop = n/sum(n))
}

nomb_recidive_b <- function (df){
  df %>%
    group_by(nomb_recidive_b) %>%
    summarise(n = n()) %>%
    mutate (prop = n/sum(n))
}

anapath <- function (df){
  df %>%
    group_by(anapath) %>%
    summarise(n = n()) %>%
    mutate (prop = n/sum(n))
}

preop_cog_def <- function(df){
  df %>%
    mutate ()
}
  
analyse_descriptive <- function(df){
  age <- age(df)
  age_old <- age_old(df)
  age_old_prop <- age_old_prop(df)
  sex <- sex(df)
  height <- height(df)
  imc <- imc(df)
  asa <- asa(df)
  asa_2 <- asa_2(df)
  pas_bloc <- pas_bloc(df)
  hta <- hta(df)
  creat <- creat(df)
  dfg <- dfg(df)
  anti_hypertenseur <- anti_hypertenseur(df)
  bb <- bb(df)
  diuretique <- diuretique(df)
  iec <- iec(df)
  nomb_recidive_b <- nomb_recidive_b (df)
  analyse_descriptive <- list(age, age_old, age_old_prop, sex, height, imc, asa, asa_2, pas_bloc, hta, creat, dfg, anti_hypertenseur, bb, diuretique, iec, nomb_recidive_b)
  return(analyse_descriptive)
}


### -------------------- Fonction stats des ttt psychotrope --------------

# Compter combien de patients ont au moins un traitement psychotrope
# Ne fonctionne pas
psychotrope <- function (df) { 
  df %>%
    group_by(psychotrope) %>%
    summarise(n = n()) %>%
    mutate (prop = n/sum(n))
}

benzodiazepines <- function (df) { 
  df %>%
    group_by(benzodiazepines) %>%
    summarise(n = n()) %>%
    mutate (prop = n/sum(n))
}

anti_epileptiques <- function (df) { 
  df %>%
    group_by(anti_epileptiques) %>%
    summarise(n = n()) %>%
    mutate (prop = n/sum(n))
}

anti_h1 <- function (df) { 
  df %>%
    group_by(anti_h1) %>%
    summarise(n = n()) %>%
    mutate (prop = n/sum(n))
}


antidepresseurs <- function (df) { 
  df %>%
    group_by(antidepresseurs) %>%
    summarise(n = n()) %>%
    mutate (prop = n/sum(n))
}


opioides <- function (df) { 
  df %>%
    group_by(opioides) %>%
    summarise(n = n()) %>%
    mutate (prop = n/sum(n))
}

anal_descript_psychotrope <- function (df) {
  psychotrope <- psychotrope(df)
  benzodiazepines <- benzodiazepines(df)
  anti_epileptiques <- anti_epileptiques(df)
  anti_h1 <- anti_h1(df)
  antidepresseurs <- antidepresseurs(df)
  opioides <- opioides(df)
  anal_descript_psychotrope <- list(psychotrope, benzodiazepines, anti_epileptiques, anti_h1, antidepresseurs, opioides)
  return(anal_descript_psychotrope)
}

cog_test_availability_prop <-function(data){
  data %>%
    select("var_span_endroit_SD_b", "var_span_envers_SD_b", "var_FLUs_SD_b", "var_FLUp_SD_b", "var_TMTA_SD_b", "var_TMTB_SD_b", 
           "var_TMTB_A_SD_b") %>% 
    summarise_all(funs(round(sum(!is.na(.))/nrow(data)*100,1)))
} 

cog_test_availability <-function(data){
  data %>%
    select("var_span_endroit_SD_b", "var_span_envers_SD_b", "var_FLUs_SD_b", "var_FLUp_SD_b", "var_TMTA_SD_b", "var_TMTB_SD_b", 
           "var_TMTB_A_SD_b") %>% 
    summarise_all(funs(sum(!is.na(.))))
} 

all_cog_test <-  function(df) {
  df %>%
    filter(  !is.na(span_endroit_SD_pre) &
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
               !is.na(TMTB_A_SD_post_EM)
    )
}
