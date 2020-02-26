setwd("/Users/louisperdriel/OneDrive - UNIVERSITE PARIS DESCARTES/Troisieme_cycle/Recherche_clinique/Hypotension_per_op_ischemie/Donnees")
data <- read.csv2("gliome_HD_tidy_120319_v1.csv", na.string="NA")

### -------------------- Librairies ---------------------------------------------------
library(tidyverse)
library(chron)
library(Hmisc)
library(gmodels)

### -------------------- Charger mes fonctions sources -----------------------
setwd("/Users/louisperdriel/OneDrive - UNIVERSITE PARIS DESCARTES/Troisieme_cycle/Recherche_clinique/Hypotension_per_op_ischemie/Scripts")
source(file="funct_stat_descr_pop.R", encoding ="UTF-8")
source(file="funct_stat_descr_perop.R", encoding ="UTF-8")
setwd("/Users/louisperdriel/OneDrive - UNIVERSITE PARIS DESCARTES/Pro/Code/micro_scripts")
source(file="confidence_interval.R", encoding = "UTF-8")

### -------------------- Sous bases de données ----
# Base de données avec les patients ayant TOUS les résultats de leurs tests cognitifs
data_all_cog_test <- all_cog_test(data)
data_all_cog_test_def <- data_all_cog_test %>% filter(cog_bin_outcome == 1)
data_all_cog_test_no_def <- data_all_cog_test %>% filter(cog_bin_outcome == 0)
data_all_cog_test_old <- data_all_cog_test %>% filter (age >=45)

#Base de données ne comprenant que les patients ayant au moins un résultat de test cog 
data_at_least_one_cog_test <-  
  data %>%
  filter(!is.na(var_span_endroit_SD_b) |
             !is.na(var_span_envers_SD_b) |
             !is.na(var_FLUp_SD_b) |
             !is.na(var_FLUs_SD_b) | 
             !is.na(var_TMTA_SD_b) | 
             !is.na(var_TMTB_SD_b) | 
             !is.na(var_TMTB_A_SD_b)
  )

data_ischemie <- data %>% filter(isch_postop_0abs_1pres==1)

data_non_ischemie <- data %>% filter(isch_postop_0abs_1pres==0)

data_ischemie_all_cog_test <- all_cog_test(data_ischemie)
data_non_ischemie_all_cog_test <- all_cog_test(data_non_ischemie)

data_eveille <- data %>% filter(chir_eveil_1oui_0non == 1)
data_endormis <- data %>% filter(chir_eveil_1oui_0non == 0)
data_jeune <- data %>% filter (age<45)
data_ages <- data %>% filter(age>=45)
data_femme <- data %>% filter(sexe_1F_0H == 1)
data_homme <- data %>% filter(sexe_1F_0H == 0)
data_court <- data %>% filter(duree_interv_min<350)
data_long <- data %>% filter (duree_interv_min>=350)
data_ht_grade <- data %>% filter (grade_b == "haut")
data_bs_grade <- data %>% filter (grade_b == "bas")

### -------------------- Caractéristiques de la population complète ------------------------------------

# Analyse descriptive des caractéristiques de ma cohorte complète
analyse_descriptive(data)
anal_descript_psychotrope(data)
data %>% select(age) %>% filter (age >= 45) %>% arrange(desc(age)) %>% filter(age < quantile(age,0.8)) 
data %>% group_by(def_cog_preop) %>% summarise(n=n())

# Analyse descriptive des caractérisitiques de la cohorte avec une ischémie post-op
analyse_descriptive(data_ischemie)
anal_descript_psychotrope(data_ischemie)
none_missing_value_isch <- data_ischemie %>% summarise_all(funs(sum(!is.na(.))))
data_ischemie %>% group_by(def_cog_preop) %>% summarise(n=n()) %>% mutate (prop= n/sum(n))
cog_test_availability(data_ischemie)
cog_test_availability_prop(data_ischemie)
nrow(all_cog_test(data_ischemie))
data_ischemie %>% group_by(grade_b) %>% summarise(n=n()) %>% mutate (prop= n/sum(n))

# Analyse descriptive des caractérisitiques de la cohorte sans ischémie post-op
analyse_descriptive(data_non_ischemie)
anal_descript_psychotrope(data_non_ischemie)
none_missing_value_no_isch <- data_non_ischemie %>% summarise_all(funs(sum(!is.na(.))))
data_non_ischemie %>% group_by(def_cog_preop) %>% summarise(n=n()) %>% mutate (prop= n/sum(n))
cog_test_availability(data_non_ischemie)
cog_test_availability_prop(data_non_ischemie)
nrow(all_cog_test(data_non_ischemie))
data_non_ischemie %>% group_by(grade_b) %>% summarise(n=n()) %>% mutate (prop= n/sum(n))

# Tests d'homogénéité: résultats des tests cognitifs disponibles
CrossTable(data$all_cog_test_available, data$isch_postop_0abs_1pres, chisq=T, fisher=T)
CrossTable(data$span_endroit_available, data$isch_postop_0abs_1pres, chisq=T, fisher=T)
CrossTable(data$span_envers_available, data$isch_postop_0abs_1pres, chisq=T, fisher=T)
CrossTable(data$FLUs_available, data$isch_postop_0abs_1pres, chisq=T, fisher=T)
CrossTable(data$FLUp_available, data$isch_postop_0abs_1pres, chisq=T, fisher=T)
CrossTable(data$TMTA_available, data$isch_postop_0abs_1pres, chisq=T, fisher=T)
CrossTable(data$TMTB_available, data$isch_postop_0abs_1pres, chisq=T, fisher=T)
CrossTable(data$TMTB_A_available, data$isch_postop_0abs_1pres, chisq=T, fisher=T)
CrossTable(data$def_cog_preop, data$isch_postop_0abs_1pres, chisq=T, fisher=T)

# Test d'homogénéité: ts les tests cognitifs dispo et tumeur de bas ou de haut grade
CrossTable(data$all_cog_test_available, data$grade_b, chisq=T, fisher=T )

# Test d'homo: GFR
data <- data %>% mutate (DFG_av = case_when(is.na(dfg_ckdepi)~0, TRUE ~1))
CrossTable(data$DFG_av, data$isch_postop_0abs_1pres, chisq=T, fisher=T)

# Duration of surgery and tumour grade
wilcox.test(data$duree_interv_min ~ data$grade_b)

#ATCD vasculaires
table(data$insuf_cardiaque)
table( data$cardio_ische)
table(data$dfg_ckdepi_std)
table(data$avc_ait)
table(data$intox_tabac)
table(data$dt_insulinoreq)
table(data$diabete_t2)
table(data$insuf_ren_chro)

# Tests d'homogénéité
wilcox.test(data$age ~ data$isch_postop_0abs_1pres)
wilcox.test(data$age_old ~ data$isch_postop_0abs_1pres)
CrossTable(data$age_old_b, data$isch_postop_0abs_1pres, chisq=T, fisher=T)
CrossTable(data$sex, data$isch_postop_0abs_1pres, chisq=T, fisher=T)
wilcox.test(data$imc ~ data$isch_postop_0abs_1pres)
CrossTable(data$asa_2, data$isch_postop_0abs_1pres, chisq=T, fisher=T)
wilcox.test(data$pas_arrivee_bloc ~ data$isch_postop_0abs_1pres)
CrossTable(data$hta, data$isch_postop_0abs_1pres, chisq=T, fisher=T)
wilcox.test(data$creat_preop ~ data$isch_postop_0abs_1pres)
wilcox.test(data$dfg_ckdepi ~ data$isch_postop_0abs_1pres)
CrossTable(data$anti_hypertenseur, data$isch_postop_0abs_1pres, chisq=T, fisher=T)
CrossTable(data$psychotrope, data$isch_postop_0abs_1presE, chisq=T, fisher=T)
CrossTable(data$benzodiazepines, data$isch_postop_0abs_1pres, chisq=T, fisher=T)
CrossTable(data$anti_epileptiques, data$isch_postop_0abs_1pres, chisq=T, fisher=T)
CrossTable(data$nomb_recidive_b, data$isch_postop_0abs_1pres, chisq=T, fisher=T)
CrossTable(data$grade_b, data$isch_postop_0abs_1pres, chisq=T, fisher=T)

#### Table 2: Management peropératoire
analyse_descriptive_perop(data)
analyse_descriptive_perop(data_ischemie)
analyse_descriptive_perop(data_non_ischemie)

# Les patients ayant un glioblastome ont une durée de chirurgie inférieure à ceux ayant un gliome
wilcox.test(data$duree_interv_min ~ data$grade_b)

#### Tests statistiques d'homogénéité
hypo_longue_test <- CrossTable(data$hypo_longue, data$isch_postop_0abs_1pres, chisq=T, fisher=T)
exp_volem_mL_test <- wilcox.test(data$exp_volem_mL ~ data$isch_postop_0abs_1pres) 
transfu_y1_n0_test <- CrossTable (data$transfu_y1_n0, data$isch_postop_0abs_1pres, chisq=T, fisher=T)
pertes_sang_test <- wilcox.test(data$pertes_sang ~ data$isch_postop_0abs_1pres)
duree_pas_inf_85p_test <- wilcox.test(data$hypo~data$isch_postop_0abs_1presE)
vaso_c_drug_test <- CrossTable(data$vaso_c_drug, data$isch_postop_0abs_1pres, chisq=T, fisher=T)
nad_b_test <- CrossTable (data$nad_b, data$isch_postop_0abs_1pres, chisq=T, fisher=T)
max_nad_µ_kg_min_test <- wilcox.test(data$max_nad_µ_kg_min~data$isch_postop_0abs_1pres)
ephedrine_b_test <- CrossTable (data$ephedrine_b, data$isch_postop_0abs_1pres, chisq=T, fisher=T)
ephedrine_test <- wilcox.test(data$ephedrine~data$isch_postop_0abs_1pres)
duree_interv_min_test <- wilcox.test(data$duree_interv_min ~ data$isch_postop_0abs_1pres)
microscope_test <- CrossTable (data$microscope, data$isch_postop_0abs_1pres, chisq=T, fisher=T)
chir_eveil_1oui_0non_test <- CrossTable (data$chir_eveil_1oui_0non, data$isch_postop_0abs_1pres, chisq=T, fisher=T)
hypotension_b <- CrossTable(data$hypotension_b, data$isch_postop_0abs_1pres, chisq=T, fisher=T)
htest_result_brut <- list(exp_volem_mL_test, transfu_y1_n0_test, pertes_sang_test, duree_pas_inf_85p_test,
                          nad_b_test, max_nad_µ_kg_min_test, ephedrine_b_test, ephedrine_test, duree_interv_min_test)

# Disponibilité des résultats
data <- data %>% mutate(pertes_sang_av = case_when(is.na(pertes_sang)~0, TRUE ~1))
CrossTable(data$pertes_sang_av, data$isch_postop_0abs_1pres, chisq=T, fisher=T)


### -------------------- Caractéristiques pop cognitive test results all available ----
# Postoperative cognitive deficit
nrow(data_all_cog_test_def)
analyse_descriptive(data_all_cog_test_def)
anal_descript_psychotrope(data_all_cog_test_def)
data_all_cog_test_def %>% group_by(grade_b) %>% summarise(n=n()) %>% mutate (prop= n/sum(n))
data_all_cog_test_def %>% group_by(nomb_recidive_b) %>% summarise(n=n()) %>% mutate (prop= n/sum(n))

# No postoperative cognitive deficit
nrow(data_all_cog_test_no_def)
analyse_descriptive(data_all_cog_test_no_def)
anal_descript_psychotrope(data_all_cog_test_no_def)
data_all_cog_test_no_def %>% group_by(grade_b) %>% summarise(n=n()) %>% mutate (prop= n/sum(n))
data_all_cog_test_no_def %>% group_by(nomb_recidive_b) %>% summarise(n=n()) %>% mutate (prop= n/sum(n))

# Tests d'homogénéité
wilcox.test(data_all_cog_test$age ~ data_all_cog_test$cog_bin_outcome)
wilcox.test(data_all_cog_test_old$age ~ data_all_cog_test_old$cog_bin_outcome)
wilcox.test(data_all_cog_test$age_old_b ~ data_all_cog_test$cog_bin_outcome)
CrossTable(data_all_cog_test$sex, data_all_cog_test$cog_bin_outcome, chisq=T, fisher=T)
wilcox.test(data_all_cog_test$imc ~ data_all_cog_test$cog_bin_outcome)
CrossTable(data_all_cog_test$asa, data_all_cog_test$cog_bin_outcome, chisq=T, fisher=T)
wilcox.test(data_all_cog_test$pas_arrivee_bloc ~ data_all_cog_test$cog_bin_outcome)
CrossTable(data_all_cog_test$hta, data_all_cog_test$cog_bin_outcome, chisq=T, fisher=T)
wilcox.test(data_all_cog_test$dfg_ckdepi ~ data_all_cog_test$cog_bin_outcome)
CrossTable(data_all_cog_test$anti_hypertenseur, data_all_cog_test$cog_bin_outcome, chisq=T, fisher=T)
CrossTable(data_all_cog_test$psychotrope, data_all_cog_test$cog_bin_outcome, chisq=T, fisher=T)
CrossTable(data_all_cog_test$benzodiazepines, data_all_cog_test$cog_bin_outcome, chisq=T, fisher=T)
CrossTable(data_all_cog_test$anti_epileptiques, data_all_cog_test$cog_bin_outcome, chisq=T, fisher=T)
CrossTable(data_all_cog_test$nomb_recidive_b, data_all_cog_test$cog_bin_outcome, chisq=T, fisher=T)
CrossTable(data_all_cog_test$grade_b, data_all_cog_test$cog_bin_outcome, chisq=T, fisher=T)
CrossTable(data_all_cog_test$def_cog_preop, data_all_cog_test$cog_bin_outcome, chisq=T, fisher=T)


## Perop
analyse_descriptive_perop(data_all_cog_test)
analyse_descriptive_perop(data_all_cog_test_def)
analyse_descriptive_perop(data_all_cog_test_no_def)


## Tests d'homogénéité
wilcox.test(data_all_cog_test$duree_pas_inf_85p ~ data_all_cog_test$cog_bin_outcome)
CrossTable(data_all_cog_test$hypo_longue, data_all_cog_test$cog_bin_outcome, chisq=T, fisher=T)
wilcox.test(data_all_cog_test$exp_volem_mL ~ data_all_cog_test$cog_bin_outcome)
CrossTable(data_all_cog_test$transfu_y1_n0, data_all_cog_test$cog_bin_outcome, chisq=T, fisher=T)
wilcox.test(data_all_cog_test$pertes_sang ~ data_all_cog_test$cog_bin_outcome)
CrossTable(data_all_cog_test$vaso_c_drug, data_all_cog_test$cog_bin_outcome, chisq=T, fisher=T)
CrossTable(data_all_cog_test$nad_b, data_all_cog_test$cog_bin_outcome, chisq=T, fisher=T)
wilcox.test(data_all_cog_test$max_nad_µ_kg_min ~ data_all_cog_test$cog_bin_outcome)
CrossTable(data_all_cog_test$ephedrine_b, data_all_cog_test$cog_bin_outcome, chisq=T, fisher=T)
wilcox.test(data_all_cog_test$ephedrine ~ data_all_cog_test$cog_bin_outcome)
wilcox.test(data_all_cog_test$duree_interv_min ~ data_all_cog_test$cog_bin_outcome)

### -------------------- Data visualisation --------------------------------------------------------------------------------------
# Exploration d'une association entre le volume d'expansion volémique et la durée de l'anesthésie
ggplot (data, aes(exp_volem_mL, duree_anesth)) + 
  geom_point(aes(color = chir_eveil_1oui_0non))

# Exploration d'une association entre le volume d'expansion volémique et l'année de réalisation de l'anesthésie
data_1 <- data %>%
  group_by(year) %>%
  summarise (mean_exp_volem = mean(exp_volem_mL))

ggplot (data_1) +
    geom_point(aes(year, mean_exp_volem))

# Variables continues répartition d'allure gaussienne

# Visualisation volume d'expansion volémique
ggplot (data, aes(x=exp_volem_mL)) +
  geom_histogram()

# Preinduction systolic blood pressure
ggplot (data, aes(x=pas_arrivee_bloc)) +
  geom_histogram()

# Duration of surgery
ggplot (data, aes(x=duree_interv_min)) +
  geom_histogram()

# Variables continues répartition non gaussienne
# Blood loss
ggplot (data, aes(x=pertes_sang)) +
  geom_histogram()

# Duree pas inf à 85% pas d'entrée au bloc
ggplot (data, aes(x=duree_pas_inf_85p)) +
  geom_histogram()

# Dose max de noradrénaline / kg /min
ggplot (data, aes(x= max_nad_µ_kg_min)) +
  geom_histogram()

# Ephedrine: dose max
ggplot (data, aes(x = ephedrine)) +
  geom_histogram()

# Variables binaires:
# Use of blood product
ggplot (data, aes(x=transfu_y1_n0)) +
  geom_histogram()

# Vaso-activ drug needed
ggplot (data, aes(x=vaso_c_drug)) +
  geom_histogram(stat = 'count')

# Hypotension et probabilité d'une ischémie postop
setwd("/Users/louisperdriel/Documents/troisieme_cycle/Recherche_clinique/Hypotension_per_op_ischemie/Redaction_article")
tiff("Rcspline_ischemia_hypotension.tiff", width = 7, height = 7, units = "in", res = 100)
rcspline.plot(x=data$duree_pas_inf_85p, y=data$isch_postop_0abs_1pres, model="logistic", nk=3,
              show="prob",plotcl=TRUE, showknots=FALSE, add=FALSE,lty=1, noprint=TRUE,ylim=c(0,1), ylab="Probability of post-operative ischemia" , xlab="Cumulative time with hypotension (mins)", main="", statloc = "none")  
dev.off()

# Age et probabilité d'une ischémie postop
setwd("/Users/louisperdriel/Documents/troisieme_cycle/Recherche_clinique/Hypotension_per_op_ischemie/Redaction_article")
tiff("Rcspline_ischemia_age.tiff", width = 7, height = 7, units = "in", res = 100)
rcspline.plot(x=data$age, y=data$isch_postop_0abs_1pres, model="logistic", nk=3,
              show="prob",plotcl=TRUE, showknots=FALSE, add=FALSE,lty=1, noprint=TRUE,ylim=c(0,1), ylab="Probability of post-operative ischemia" , xlab="Age (years)", main="", statloc = "none")  
dev.off()

# % de mes patients entre 45 et 65 ans: 
data %>% select(age) %>% arrange (desc(age)) %>% mutate (age_perct = percent_rank (age))
0.93258427-0.46067416

# Surgery duration and probability of postoperative ischemic brain lesions
setwd("/Users/louisperdriel/Documents/troisieme_cycle/Recherche_clinique/Hypotension_per_op_ischemie/Redaction_article")
tiff("Rcspline_ischemia_surg_durat.tiff", width = 7, height = 7, units = "in", res = 100)
rcspline.plot(x=data$duree_interv_min, y=data$isch_postop_0abs_1pres, model="logistic", nk=3,
              show="prob",plotcl=TRUE, showknots=FALSE, add=FALSE,lty=1, noprint=TRUE,ylim=c(0,1), ylab="Probability of post-operative ischemia" , xlab="Surgical intervention duration (mins)", main="", statloc = "none")  
dev.off()

# Hypotension and probability of postoperative cognitiv deficits
setwd("/Users/louisperdriel/Documents/troisieme_cycle/Recherche_clinique/Hypotension_per_op_ischemie/Redaction_article")
tiff("Rcspline_cog_def_hypotension.tiff", width = 7, height = 7, units = "in", res = 100)
rcspline.plot(x=data$duree_pas_inf_85p, y=data$cog_bin_outcome, model="logistic", nk=3,
              show="prob",plotcl=TRUE, showknots=FALSE, add=FALSE,lty=1, noprint=TRUE,ylim=c(0,1), ylab="Probability of post-operative cognitive deficit" , xlab="Cumulative time with hypotension (mins)", main="", statloc = "none")  
dev.off()

#Corrélation entre la durée de l'hypotension per-op et la présence d'un déficit cognitif en post-op
# Pour une hypotension <90% de la PAS d'entrée
rcspline.plot(x=data$duree_pas_inf_90p,y=data$cog_bin_outcome, model="logistic", nk=3,
              show="prob",plotcl=T, showknots=FALSE, add=FALSE,lty=1, noprint=TRUE,ylim=c(0,1), ylab="Probabilité" , xlab="Temps (min)", main="Seuils absolus", statloc = "none")
# Pour une hypotension < 85% de la PAS d'entrée
tiff("Rcspline_def_cog_duration_long.tiff", width = 7, height = 7, units = "in", res = 100)
rcspline.plot(x=data$duree_pas_inf_85p,y=data$cog_bin_outcome, model="logistic", nk=3,
              show="prob",plotcl=T, showknots=FALSE, add=FALSE,lty=1, noprint=TRUE,ylim=c(0,1), ylab="Probabilité" , xlab="Cumulative time with hypotension (mins)", main="All patients", statloc = "none")
dev.off()




### -------------------- Association hypoT et ischémie -------------------------------------
#### Recherche de corrélation entre hypo_longue et présence d'une ischémie sur l'imagerie post-op
## Unadjusted
# 1) Via un test du Chi 2 et un test exact de Fisher
ct <- CrossTable (data$hypo_longue, data$isch_postop_0abs_1pres, chisq=T, fisher=T)
# Significatif

# 2) via un modèle linéaire généralisé 
fit_isch <- glm(isch_postop_0abs_1pres ~ hypo_longue,family="binomial",data=data)
unadj_summ = summary(fit_isch)
confidence_interval_pv(estimate_glm(unadj_summ), sd_glm(unadj_summ), ct)

# Effet hypo_longue après ajustement sur les facteurs confondant
fit_isch_adj <- glm(isch_postop_0abs_1pres ~ hypo_longue+I(age>40)+I(duree_interv_min>350)+sexe_1F_0H,family="binomial",data=data)
adj_summ = summary(fit_isch_adj)


### -------------------- Impact de l'hypotension per-op sur des sous-catégories de patients ---------------------------------------------
# Essai, recherche de facteurs prédicitfs per-opératoire d'une ischémie post-opératoire
CrossTable (data$localisation, data$isch_postop_0abs_1pres, chisq=T, fisher=T)

# Patients éveillés
fit_isch_eveille <- glm(isch_postop_0abs_1pres ~ hypo_longue,family="binomial", data=data_eveille)
eveil_summ = summary(fit_isch_eveille)


# Patients endormis, sous AG
fit_isch_endormis <- glm(isch_postop_0abs_1pres ~ hypo_longue,family="binomial",data=data_endormis)
ag_summ = summary(fit_isch_endormis)

# Patients jeune 
fit_isch_jeune <- glm(isch_postop_0abs_1pres ~ hypo_longue,family="binomial",data=data_jeune)
jne_summ = summary(fit_isch_jeune)

# Patients âgés
fit_isch_ages <- glm(isch_postop_0abs_1pres ~ hypo_longue,family="binomial",data=data_ages)
vx_summ = summary(fit_isch_ages)

# Patients de sexe féminins
fit_isch_femme <- glm(isch_postop_0abs_1pres ~ hypo_longue,family="binomial",data=data_femme)
fem_summ = summary(fit_isch_femme)

# Patients de sexe masculin
fit_isch_homme <- glm(isch_postop_0abs_1pres ~ hypo_longue,family="binomial",data=data_homme)
masc_summ = summary(fit_isch_homme)

# Patients dont la durée de l'intervention est < 350 mins
fit_isch_court <- glm(isch_postop_0abs_1pres ~ hypo_longue,family="binomial",data=data_court)
crt_summ = summary(fit_isch_court)

# Patients dont la durée de l'intervention est ≥ 350 mins
fit_isch_long <- glm(isch_postop_0abs_1pres ~ hypo_longue,family="binomial",data=data_long)
lg_summ = summary(fit_isch_long)

# Gliomes de haut grade 
fit_ht_grade <-  glm(isch_postop_0abs_1pres ~ hypo_longue,family="binomial",data=data_ht_grade)
ht_grade_summ = summary(fit_ht_grade)

# Gliomes bas grade
fit_bs_grade <- glm(isch_postop_0abs_1pres ~ hypo_longue,family="binomial",data=data_bs_grade)
bs_grade_summ = summary(fit_bs_grade)

sous_cat_patients <- list(data, data, data_ht_grade, data_bs_grade, data_court, data_long, data_ages, data_jeune, data_femme, data_homme, 
                          data_eveille, data_endormis)
nbr_sous_cat_patients <- lapply(sous_cat_patients, nrow)

### -------------------- Forest plot hypoT et ischémie ----------------
setwd("/Users/louisperdriel/Documents/troisieme_cycle/Recherche_clinique/Hypotension_per_op_ischemie/Redaction_article")
list_summ <- list(unadj_summ, adj_summ, ht_grade_summ, bs_grade_summ, crt_summ, lg_summ, jne_summ, vx_summ, fem_summ, masc_summ, 
                  eveil_summ, ag_summ)
estimate = list()
sd = list()
CI = list()
for (i in 1:length(list_summ)) {
  estimate [i] <- exp(list_summ[[i]]$coefficients[2,1])
  sd[i] <- list_summ[[i]]$coefficients[2,2]
  CI[i] = list(confidence_interval(log(estimate[[i]]), sd[[i]]))
}
estimate = unlist(estimate)
estimate = round(estimate,2)
sd = unlist(sd)
lower_limit = sapply(CI, min)
upper_limit = sapply(CI, max)
collection = c("All population, unadjusted", "All population adjusted", "Haut grade", "Bas grade", "Short duration", 'Long duration', 
               "Age < 45 yo", "Age > 45 yo", "Female", "Male", "Awake", "General anesthesia")
forest_plot = data.frame (collection, estimate, lower_limit, upper_limit)
forest_plot$legend <- paste(round(forest_plot$estimate,2)," [",round(forest_plot$lower_limit,2)," - ",round(forest_plot$upper_limit,2),"]",sep="")
forest_plot$n <- paste("n=", nbr_sous_cat_patients, sep="")

tiff("Forest_hypoT_ischemie.tiff", width = 12, height = 14, units = "in", res = 100)
par(mar=c(6, 19, 3, 1), bty="n")
level_legend_y = c(4, 3.5, 2, 1.5, 0.5, 0, -1, -1.5, -2.5, -3, -4, -4.5)

plot(forest_plot[1:8,2], level_legend_y[1:8], pch=15, cex=2, xlim=c(-1,8.8), ylim=c(-4.6, 4.6), xaxt="n", yaxt="n",ylab="",xlab="OR for 100 mins of hypotension [95% CI]",cex.axis=1.5,cex.lab=1.5)
points(forest_plot[10:12,2], level_legend_y[10:12], pch=15, cex=2)
axis(side= 1, at = c(0, 1, 2, 3, 4, 5, 6, 7) , labels = c(0, 1, 2, 3, 4, 5, 6, 7), cex.axis = 1.5)
axis(side= 2, at = level_legend_y,
     labels=c("All patients, unadjusted", "All patients, adjusted", "High grade", "Low grade", "Duration of surg < 350 mins", 'Duration of surg ≥ 350 mins',
              "Age < 45 yo", "Age > 45 yo", "Female", "Male", "Awake", "General Anesth"), cex.axis=1.5,las=2)


# Segment limite inf - estimate
segments(x0=as.numeric(forest_plot[1:8,3]),y0=level_legend_y[1:8],
         x1=as.numeric(forest_plot[1:8,2]),lwd=2)
segments(x0=as.numeric(forest_plot[9,3]),y0=level_legend_y[9],
         x1=6.5,lwd=2)
segments(x0=as.numeric(forest_plot[10:12,3]),y0=level_legend_y[10:12],
         x1=as.numeric(forest_plot[10:12,2]),lwd=2)

# Segment estimate - limite sup 
segments(x0=as.numeric(forest_plot[1,2]),y0=level_legend_y[1],
         x1=6.5,lwd=2)
segments(x0=as.numeric(forest_plot[2,2]),y0=level_legend_y[2],
         x1=as.numeric(forest_plot[2,4]),lwd=2)
segments(x0=as.numeric(forest_plot[3:6,2]),y0=level_legend_y[3:6],
         x1=6.5,lwd=2)
segments(x0=as.numeric(forest_plot[7,2]),y0=level_legend_y[7],
         x1=forest_plot[7,4],lwd=2)
segments(x0=as.numeric(forest_plot[8,2]),y0=level_legend_y[8],
         x1=6.5,lwd=2)
segments(x0=as.numeric(forest_plot[10:11,2]),y0=level_legend_y[10:11],
         x1=as.numeric(forest_plot[10:11,4]),lwd=2)
segments(x0=as.numeric(forest_plot[12,2]),y0=level_legend_y[12],
         x1=6.5,lwd=2)


# Barre limite inf
little_vert_bar(forest_plot[1:12,3], y0=level_legend_y)
# Barre ou flèche limite sup 
fleche(6.5, level_legend_y[1])
little_vert_bar(forest_plot[2,4], y0=level_legend_y[2])
fleche(6.5, level_legend_y[3:6])
little_vert_bar(forest_plot[7,4], y0=level_legend_y[7])
fleche(6.5, level_legend_y[8:9])
little_vert_bar(forest_plot[10:11,4], y0=level_legend_y[10:11])
fleche(6.5, level_legend_y[12])

abline(v=1,lty=2,lwd=2)

for (i in 1:length(level_legend_y)){
  text(7.8, level_legend_y[i], forest_plot$legend[i], cex = 1.25, col = "black")
} 
for (i in 1:length(level_legend_y)){
  text(-0.5, level_legend_y[i], forest_plot$n[i], cex = 1.25, col = "black")
}
text(7.8, 4.5, "OR, 95% CI", cex = 1.25, col = "black")
dev.off()

### -------------------- Association hypoT et déficit cognitif -------------------------------------  
#### Recherche de corrélation entre hypo_longue et présence d'au moins un déficit cognitif
# Test de Chi 2 hypo_longue et cog_bin_outcome
CrossTable (data_all_cog_test$hypo_longue, data_all_cog_test$cog_bin_outcome, chisq=T, fisher=T)
# Regression logistique durée hypotension < 85 et cog_bin_outcome
#fit <- glm(cog_bin_outcome ~ I(data_all_cog_test$duree_pas_inf_85p/10),family="binomial",data=data_all_cog_test)
#fit <-glm(cog_bin_outcome ~ I(data_all_cog_test$duree_pas_inf_85p>100),family="binomial",data=data_all_cog_test)

fit<- glm(cog_bin_outcome ~ duree_pas_inf_85p,family="binomial",data=data_all_cog_test)
cog_bin_outcome_summ <- summary(fit)

#### Recherche de corrélation entre hypo_longue ou durée hypo et présence d'un déficit cognitif pour chaque test 
# Span_endroit
# Test du Chi 2
CrossTable (data_all_cog_test$hypo_longue, data_all_cog_test$var_span_endroit_SD_b, chisq=T, fisher=T)
# Regression logistique
fit_span_endroit <- glm(var_span_endroit_SD_b ~ data_all_cog_test$duree_pas_inf_85p,family="binomial",data=data_all_cog_test)
span_endroit_summ <- summary(fit_span_endroit)


# Span_envers 
# Test du Chi 2
ct <- CrossTable (data_all_cog_test$var_span_envers_SD_b, data_all_cog_test$hypo_longue, chisq=T, fisher=T)
# Regression logistique
fit_span_envers<- glm(var_span_envers_SD_b ~ duree_pas_inf_85p,family="binomial",data=data_all_cog_test)
span_envers_summ <- summary(fit_span_envers)


# FLUs
# Test du Chi 2
CrossTable (data_all_cog_test$var_FLUs_SD_b, data_all_cog_test$hypo_longue, chisq=T, fisher=T)
# Regression logistique
fit_FLUs_SD <- glm(var_FLUs_SD_b ~ data_all_cog_test$duree_pas_inf_85p,family="binomial",data=data_all_cog_test)
FLUs_summ <- summary(fit_FLUs_SD)
  
# FLUp
# Test du Chi 2
CrossTable (data_all_cog_test$var_FLUp_SD_b, data_all_cog_test$hypo_longue, chisq=T, fisher=T)
# Regression logistique
fit_FLUp_SD <- glm(var_FLUp_SD_b ~ data_all_cog_test$duree_pas_inf_85p,family="binomial",data=data_all_cog_test)
FLUp_summ <- summary(fit_FLUp_SD)


# TMTA
# Test du Chi 2
CrossTable (data_all_cog_test$var_TMTA_SD_b, data_all_cog_test$hypo_longue, chisq=T, fisher=T)
# Regression logistique
fit_TMTA_SD <- glm(var_TMTA_SD_b ~ data_all_cog_test$duree_pas_inf_85p,family="binomial",data=data_all_cog_test)
TMTA_summ <- summary(fit_TMTA_SD)


# TMTB
# Test du Chi 2
CrossTable (data_all_cog_test$var_TMTB_SD_b, data_all_cog_test$hypo_longue, chisq=T, fisher=T)
# Regression logistique
fit_TMTB_SD <- glm(var_TMTB_SD_b ~ data_all_cog_test$duree_pas_inf_85p,family="binomial",data=data_all_cog_test)
TMTB_summ <- summary(fit_TMTB_SD)

# TMTBA
# Test du Chi 2
CrossTable (data_all_cog_test$var_TMTB_A_SD_b, data_all_cog_test$hypo_longue, chisq=T, fisher=T)
# Regression logistique
fit_TMTB_A_SD <- glm(var_TMTB_A_SD_b ~ data_all_cog_test$duree_pas_inf_85p,family="binomial",data=data_all_cog_test)
TMTB_A_summ <- summary(fit_TMTB_A_SD)

### -------------------- Forest plot hypotension et déficits cognitifs ----
setwd("/Users/louisperdriel/Documents/troisieme_cycle/Recherche_clinique/Hypotension_per_op_ischemie/Redaction_article")
list_summ <- list(cog_bin_outcome_summ, span_endroit_summ, span_envers_summ, FLUs_summ, FLUp_summ,
                  TMTA_summ, TMTB_summ, TMTB_A_summ)
estimate = list()
sd = list()
CI = list()
for (i in 1:length(list_summ)) {
  estimate [i] <- exp(list_summ[[i]]$coefficients[2,1])
  sd[i] <- list_summ[[i]]$coefficients[2,2]
  CI[i] = list(confidence_interval(log(estimate[[i]]), sd[[i]]))
}
estimate = unlist(estimate)
estimate=round(estimate,2)
sd = unlist(sd)
lower_limit = round(sapply(CI, min),2)
upper_limit = round(sapply(CI, max),2)
collection = c("Cognitiv deficit, unadjusted", "Verbal forward digit span", "Verbal backward digit span",
               "Categorical fluency test", "Literal fluency test", "TMT A", "TMT B", "TMT BA")

forest_plot = data.frame (collection, estimate, lower_limit, upper_limit)
forest_plot$legend <- paste(round(forest_plot$estimate,2)," [",round(forest_plot$lower_limit,2)," - ",round(forest_plot$upper_limit,2),"]",sep="")

tiff("Forest_hypoT_def_cog.tiff", width = 10, height = 12, units = "in", res = 100)
par(mar=c(5, 18, 1, 0.5), bty="n")

level_legend_y = c(1.75, 1.25, 1, 0.75, 0.5, 0.25, 0, -0.25)

plot(forest_plot[1:nrow(forest_plot),2], level_legend_y, pch=15, cex=2, xlim=c(0.92,1.2), ylim=c(-0.26, 2.01), xaxt="n", yaxt="n",ylab="",xlab="Odd-ratios for 100 mins hypotension [95% CI]",cex.axis=1.5,cex.lab=1.5)
axis(side= 1, at = c(0.92, 1, 1.1) , labels = c(0.92, 1, 1.1), cex.axis = 1.5)
axis(side= 2, at = level_legend_y, labels = collection, cex.axis=1.5,las=1.5)

segments(x0=as.numeric(forest_plot[1:nrow(forest_plot),3]),y0=level_legend_y,
         x1=as.numeric(forest_plot[1:nrow(forest_plot),2]),lwd=2)
segments(x0=as.numeric(forest_plot[1:nrow(forest_plot),2]),y0=level_legend_y,
         x1=as.numeric(forest_plot[1:nrow(forest_plot),4]),lwd=2)
little_vert_bar(as.numeric(forest_plot[1:nrow(forest_plot),3]), level_legend_y)
little_vert_bar(as.numeric(forest_plot[1:nrow(forest_plot),4]), level_legend_y)

abline(v=1,lty=2,lwd=2)

for (i in 1:length(level_legend_y)){
  text(1.15, level_legend_y[i], forest_plot$legend[i], cex = 1.25, col = "black")
}

text(1.15, 2, "OR, 95% CI", cex = 1.25, col = "black")
dev.off()

### -------------------- Ischémie post-op et déficits cognitifs ----
# Association entre déficit cognitif et ischémie post-op
CrossTable (data_all_cog_test$isch_postop_0abs_1pres, data_all_cog_test$cog_bin_outcome, chisq=T, fisher=T)
fit <- glm(cog_bin_outcome ~ isch_postop_0abs_1pres, family="binomial", data= data_all_cog_test)
cog_bin_outcome_isch_summ <- summary(fit)

# Association déficit cognitifs et ischémie postop ajustée
fit<- glm(cog_bin_outcome ~ isch_postop_0abs_1pres+I(age>40)+I(duree_interv_min>350)+sexe_1F_0H,family="binomial",data=data_all_cog_test)
cog_bin_ouctome_isch_adj_summ <- summary(fit)

### Evaluation en sous-groupe
# Span_endroit
# Test du Chi 2
CrossTable (data_all_cog_test$isch_postop_0abs_1pres, data_all_cog_test$var_span_endroit_SD_b, chisq=T, fisher=T)
# Regression logistique
fit_span_endroit <- glm(var_span_endroit_SD_b ~ isch_postop_0abs_1pres,family="binomial",data=data_all_cog_test)
span_endroit_summ <- summary(fit_span_endroit)

# Span_envers 
# Test du Chi 2
CrossTable (data_all_cog_test$isch_postop_0abs_1pres, data_all_cog_test$var_span_envers_SD_b, chisq=T, fisher=T)
# Regression logistique
fit_span_envers<- glm(var_span_envers_SD_b ~ isch_postop_0abs_1pres,family="binomial",data=data_all_cog_test)
span_envers_summ <- summary(fit_span_envers)

# FLUs
# Test du Chi 2
CrossTable (data_all_cog_test$isch_postop_0abs_1pres, data_all_cog_test$var_FLUs_SD_b, chisq=T, fisher=T)
# Regression logistique
fit_FLUs_SD <- glm(var_FLUs_SD_b ~ isch_postop_0abs_1pres,family="binomial",data=data_all_cog_test)
FLUs_summ <- summary(fit_FLUs_SD)

# FLUp
# Test du Chi 2
CrossTable (data_all_cog_test$isch_postop_0abs_1pres, data_all_cog_test$var_FLUp_SD_b, chisq=T, fisher=T)
# Regression logistique
fit_FLUp_SD <- glm(var_FLUp_SD_b ~ isch_postop_0abs_1pres,family="binomial",data=data_all_cog_test)
FLUp_summ <- summary(fit_FLUp_SD)

# TMTA
# Test du Chi 2
CrossTable (data_all_cog_test$isch_postop_0abs_1pres, data_all_cog_test$var_TMTA_SD_b, chisq=T, fisher=T)
# Regression logistique
fit_TMTA_SD <- glm(var_TMTA_SD_b ~ isch_postop_0abs_1pres,family="binomial",data=data_all_cog_test)
TMTA_summ <- summary(fit_TMTA_SD)

# TMTB
# Test du Chi 2
CrossTable (data_all_cog_test$isch_postop_0abs_1pres, data_all_cog_test$var_TMTB_SD_b, chisq=T, fisher=T)
# Regression logistique
fit_TMTB_SD <- glm(var_TMTB_SD_b ~ isch_postop_0abs_1pres,family="binomial",data=data_all_cog_test)
TMTB_summ <- summary(fit_TMTB_SD)

#TMTB A
# Chi 2
CrossTable (data_all_cog_test$isch_postop_0abs_1pres, data_all_cog_test$var_TMTB_A_SD_b, chisq=T, fisher=T)
# Regression logistique
fit_TMTB_A_SD <- glm(var_TMTB_A_SD_b ~ isch_postop_0abs_1pres,family="binomial",data=data_all_cog_test)
TMTB_A_summ <- summary(fit_TMTB_A_SD)

### -------------------- Forest plot Ischémie postop et déficits cognitifs ----
setwd("/Users/louisperdriel/Documents/troisieme_cycle/Recherche_clinique/Hypotension_per_op_ischemie/Redaction_article")
list_summ <- list(cog_bin_outcome_isch_summ, cog_bin_ouctome_isch_adj_summ, span_endroit_summ, span_envers_summ, FLUs_summ, FLUp_summ,
                  TMTA_summ, TMTB_summ, TMTB_A_summ)
estimate = list()
sd = list()
CI = list()
for (i in 1:length(list_summ)) {
  estimate [i] <- exp(list_summ[[i]]$coefficients[2,1])
  sd[i] <- list_summ[[i]]$coefficients[2,2]
  CI[i] = list(confidence_interval(log(estimate[[i]]), sd[[i]]))
}
estimate = unlist(estimate)
estimate=round(estimate,2)
sd = unlist(sd)
lower_limit = round(sapply(CI, min),2)
upper_limit = round(sapply(CI, max),2)
estimate = round(estimate,2)
collection = c("Cognitiv deficit, unadjusted", "Cognitiv deficit, adjusted", "Verbal forward digit span", "Verbal backward digit span",
               "Categorical fluency test", "Literal fluency test", "TMT A", "TMT B", "TMT BA")

forest_plot = data.frame (collection, estimate, lower_limit, upper_limit)
forest_plot$legend <- paste(round(forest_plot$estimate,2),"[",round(forest_plot$lower_limit,2)," - ",round(forest_plot$upper_limit,2),"]",sep="")
forest_plot_tibble = as.tibble(forest_plot)

tiff("Forest_isch_def_cog.tiff", width = 12, height = 10, units = "in", res = 100)
par(mar=c(6, 23, 1, 1), bty="n")

level_legend_y = c(3.4, 3, 2, 1.6, 1.2, 0.8, 0.4, 0, -0.4)

plot(forest_plot[1:nrow(forest_plot),2], level_legend_y, pch=15, cex=2, xlim=c(0, 7), ylim=c(-0.041, 3.5), xaxt="n", yaxt="n",ylab="",xlab="OR for postoperative brain ischemic lesion [95% CI]",cex.axis=1.5,cex.lab=1.5)
axis(side= 1, at = c(0, 1, 2, 3, 4, 5) , labels = c(0, 1, 2, 3, 4, 5), cex.axis = 1.5)
axis(side= 2, at = level_legend_y, labels = collection, cex.axis=1.5,las=1.5)

segments(x0=as.numeric(forest_plot[1:nrow(forest_plot),3]),y0=level_legend_y,
         x1=as.numeric(forest_plot[1:nrow(forest_plot),2]),lwd=2)
segments(x0=as.numeric(forest_plot[1:2,2]),y0=level_legend_y[1:2],
         x1=5,lwd=2)
segments(x0=as.numeric(forest_plot[3:4,2]),y0=level_legend_y[3:4],
         x1=as.numeric(forest_plot[3:4,4]),lwd=2)
segments(x0=as.numeric(forest_plot[5:9,2]),y0=level_legend_y[5:9],
         x1=5,lwd=2)

little_vert_bar(as.numeric(forest_plot[1:nrow(forest_plot),3]), level_legend_y, n=0.03)
little_vert_bar(as.numeric(forest_plot[3:4,4]), level_legend_y[3:4], n=0.03)
fleche(5, level_legend_y[1:2])
fleche(5, level_legend_y[5:9])

abline(v=1,lty=2,lwd=2)
segments(x0=-1, y0=2.5 ,x1=7,lty=3,lwd=3)

for (i in 1:length(level_legend_y)){
  text(6, level_legend_y[i], forest_plot$legend[i], cex = 1.25, col = "black")
}
text(6.5, 4.5, "OR, 95% CI", cex = 1.25, col = "black")

#for (i in 1:length(level_legend_y)){
#  text(0.905, level_legend_y[i], forest_plot$n[i], cex = 1.25, col = "black")
#}

dev.off()

### -------------------- Déf cog et déf cog préop----
# défo cog préop available
CrossTable(data$all_cog_test_available, data$isch_postop_0abs_1pres, chisq=T, fisher=T)

# Déf cog preop and def cog postop
ct <- CrossTable(data_all_cog_test$def_cog_preop, data_all_cog_test$cog_bin_outcome, chisq=T, fisher=T)
fit<- glm(cog_bin_outcome ~ def_cog_preop, family="binomial",data=data_all_cog_test)
summ <- summary(fit)
confidence_interval(log(estimate_glm(summ)), sd_glm(summ))

# Def cog preop and ischemia
ct <- CrossTable(data_all_cog_test$def_cog_preop, data_all_cog_test$isch_postop_0abs_1pres, chisq=T, fisher=T)
fit <- glm(isch_postop_0abs_1pres ~ def_cog_preop, family="binomial",data=data_all_cog_test)
summ <- summary(fit)
confidence_interval_pv(estimate_glm(summ), sd_glm(summ), ct)
### -------------------- Amélioration cognitive ----
data_all_cog_test %>% group_by(am_fct_cog) %>% summarise (n=n()) %>% mutate (prop=n/sum(n))
CrossTable(data_all_cog_test$hypo_longue, data_all_cog_test$am_fct_cog, chisq=T, fisher=T )

# amélioration fct cog et ischémie post-op
fit_am_fct_cog <- glm(am_fct_cog ~ hypo_longue, family="binomial",data=data_all_cog_test)
am_fct_cog_summ = summary(fit_am_fct_cog)
e <- estimate_glm(am_fct_cog_summ)
sd <- sd_glm(am_fct_cog_summ)
confidence_interval(e, sd)

# Amélioration fct cog et déf cog preop
ct <- CrossTable(data_all_cog_test$am_fct_cog, data_all_cog_test$def_cog_preop, chisq=T, fisher=T )
fit_am_fct_cog <- glm(am_fct_cog ~ def_cog_preop, family="binomial",data=data_all_cog_test)
am_fct_cog_summ = summary(fit_am_fct_cog)
confidence_interval_pv(estimate_glm(am_fct_cog_summ),sd_glm(am_fct_cog_summ), ct)

