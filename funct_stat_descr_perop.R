### Ce script me permet de regrouper toutes mes fonctions nécessaire à l'analyse descriptive du manamagement per-opératoire 
### des patients contenus au sein de la base de donnée gliome_HD

### ------- Fonctions descriptives -------
# Expansion volémique
exp_volem <-  function (df) {
  summary(df$exp_volem_mL)
}

blood_products <- function (df) {
  df %>%
    group_by(transfu_y1_n0) %>%
    summarise(n = n()) %>%
    mutate (prop = n/sum(n))
}

blood_loss <- function (df) {
  summary(df$pertes_sang)  
}

pas_arrivee_bloc <- function (df){
  df %>%
    summarise(pas_arr_mean = mean(pas_arrivee_bloc), pas_arr_sd = sd(pas_arrivee_bloc))
}

duree_pas_inf_85p <- function(df) {
  summary (df$duree_pas_inf_85p) 
}

vaso_c_drug <- function (df) {
  df %>%
    group_by(vaso_c_drug) %>%
    summarise (n = n()) %>%
    mutate (prop = n/sum(n))
}

norepinephrine <- function (df){
  df %>%
    group_by(nad_b) %>%
    summarise (n = n()) %>%
    mutate (prop = n/sum(n))
}

nad_max <- function (df) {
  summary(df$max_nad_microg_kg_min)
}

ephedrine_b <- function (df) {
    df %>%
      group_by(ephedrine_b) %>%
      summarise(n = n()) %>%
      mutate(prop = n/sum(n))
}

ephedrine_iqr <- function (df) {
  summary(df$ephedrine)
}

duration_surg <- function (df) {
  summary(df$duree_interv_min)
}

microscope <- function (df){
  df %>%
    filter (!is.na(microscope)) %>%
    group_by(microscope) %>%
    summarise(n = n()) %>%
    mutate (prop = n/sum(n))
}

chir_eveil_1oui_0non <- function(df){
  df %>%
    group_by(chir_eveil_1oui_0non) %>%
    summarise(n=n()) %>%
    mutate(prop=n/sum(n))
}

IOT <- function (df){
  df %>% 
    group_by(chir_eveil_1oui_0non, IOT) %>% 
    summarise(n=n()) %>% 
    mutate (prop= n/sum(n))
}


hypotension_b <- function(df){
  df %>%
    mutate () %>%
    group_by(hypotension_b) %>%
    summarise(n=n()) %>%
    mutate(prop = n/sum(n))
}

hypo_longue <- function(df){
  df %>% 
    group_by(hypo_longue) %>% 
    summarise (n=n()) %>% 
    mutate (prop = n/sum(n))
}

### ------- Fonction descriptive résumée -----

analyse_descriptive_perop <- function (df) {
  hypo_longue <- hypo_longue(df)
  exp_volem <- exp_volem (df)
  blood_products <- blood_products(df)
  blood_loss <- blood_loss(df)
  pas_arrivee_bloc <- pas_arrivee_bloc(df)
  duree_pas_inf_85p <- duree_pas_inf_85p (df)
  vaso_c_drug <- vaso_c_drug(df)
  norepinephrine <- norepinephrine(df)
  nad_max <- nad_max(df)
  ephedrine_b <- ephedrine_b(df)
  ephedrine_iqr <- ephedrine_iqr(df)
  duration_surg <- duration_surg(df)
  microscope <- microscope(df)
  chir_eveil_1oui_0non <- chir_eveil_1oui_0non(df)
  IOT <- IOT(df)
  analyse_descriptive_perop <- list(hypo_longue= hypo_longue, exp_volem=exp_volem, blood_products=blood_products, blood_loss=blood_loss, 
                                    pas_arrivee_bloc=pas_arrivee_bloc, duree_pas_inf_85p=duree_pas_inf_85p, vaso_c_drug=vaso_c_drug, 
                                    norepinephrine=norepinephrine, nad_max=nad_max, ephedrine_b=ephedrine_b, ephedrine_iqr=ephedrine_iqr, 
                                    duration_surg=duration_surg, microscope, chir_eveil_1oui_0non, IOT)
  return(analyse_descriptive_perop)
}

