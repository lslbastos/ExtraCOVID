################################################################################
##### ExtraCOVID - Analysis of COVID-19 impact on non-covid ICU patients
#####  
##### Data Preparation
##### Fernando Zampieri (@f_g_zampieri), Leonardo Bastos (@lslbastos)
#####
################################################################################


# Package Load ------------------------------------------------------------
# library(janitor)
# library(lubridate)
library(tidyverse)
library(patchwork)




# Data input --------------------------------------------------------------
load("data/BacukpData_ExtraCOVID/RDSL_ICU_admissions_2010_2020.RData")

### Loading data 
db <- 
    df_rdsl_icu_admissions %>%
    janitor::clean_names()

rm(df_rdsl_icu_admissions)
# db <- clean_names(db)


### Data preparation



# Creating Database -------------------------------------------------------

#Creates Database

# Diagnosis of special interest: Infection; Stroke; MI


## Variable preparation
db <- db %>% 
    mutate(
        unit_admission_date = lubridate::dmy(unit_admission_date),
        adm_year = lubridate::year(unit_admission_date),
        death = as.factor(ifelse(hospital_discharge_code == "D",1,0)),
        adm_year = as.factor(adm_year),
        saps = saps3points,
        saps_prob = saps3death_probability_standard_equation / 100,
        adm_type = as.factor(admission_type_name),
        adm_type = as.factor(
            ifelse(adm_type == "Clínica","Medical",ifelse(adm_type == "Cirurgia Eletiva", "eSurgery", "uSurgery"))),
        infection = as.factor(ifelse(admission_reason_name == "Infecção / sepse", 1, 0)),
        ecog = as.factor(ifelse(chronic_health_status_name == "Independente",0, 
                                ifelse(chronic_health_status_name == "Necessidade de assistência", 1, 2))),
        mv = as.factor(ifelse(is_mechanical_ventilation1h == "Verdadeiro", 1, 0)),
        vp = as.factor(ifelse(is_vasopressors1h == "Verdadeiro", 1, 0)),
        cci = charlson_comorbidity_index
    )

#Some variables
# db$unit_admission_date<-dmy(db$unit_admission_date)
# db$adm_year<-year(db$unit_admission_date)
# db$death<-as.factor(ifelse(db$hospital_discharge_code=="D",1,0))
# db$adm_year<-as.factor(db$adm_year)
# db$saps<-db$saps3points
# db$saps_prob<-db$saps3death_probability_standard_equation/100
# db$adm_type<-as.factor(db$admission_type_name)
# db$adm_type<-as.factor(
#   ifelse(db$adm_type=="Clínica","Medical",ifelse(db$adm_type=="Cirurgia Eletiva","eSurgery","uSurgery")))
# db$infection<-as.factor(ifelse(db$admission_reason_name=="Infecção / sepse",1,0))
# db$ecog<-as.factor(ifelse(db$chronic_health_status_name=="Independente",0,
#                 ifelse(db$chronic_health_status_name=="Necessidade de assistência",1,2)))
# db$mv<-as.factor(ifelse(db$is_mechanical_ventilation1h=="Verdadeiro",1,0))
# db$vp<-as.factor(ifelse(db$is_vasopressors1h=="Verdadeiro",1,0))
# db$cci<-db$charlson_comorbidity_index



# Data filter -------------------------------------------------------------

### Filter: year of admission 2011-2020 (644,644 unique admissions from 2011-2020)
db <- db %>% 
    filter(adm_year != "2008",
           adm_year != "2009",
           adm_year != "2010",
           # adm_year != "2020"
           adm_year != "2021"
           ) %>% 
    filter(adm_year %in% c("2015", "2016", "2017",
                           "2018", "2019"))

save(db , file = "data/RDSL_ICU_admissions_2015_2019_All_raw_final.RData")

### Filters: Adult patients (Age >= 18 years old) ( 636,330 admissions)
db <- db %>% 
    filter(age >= 18)


### Filters: Selecting first admissions and excluding readmissions (544,707 patients unique patients)
db$big_id <- paste(db$age, db$gender, db$medical_record, db$hospital_admission_date, db$hospital_code)

db <- db %>%
    group_by(big_id) %>%
    arrange(unit_admission_date) %>%
    filter(row_number() == 1) %>% 
    ungroup()

db <- db %>% 
    filter(is_readmission == "Falso")


### Filters: Patients with an defined outcome (535,842 patients with an outcome)
db <- db %>% 
    filter(!is.na(death))

# Exporting backup dataset with all admissions (COVID + NonCovid)
df_hosp <- read_csv2("../../COVID19_HospVariability/Analysis_COVID19_HospVariability/data/codigo_hospitais.csv")

df <- db 
    # filter(adm_year %in% c("2016", "2017", "2018", "2019", "2020")) %>% 
    # left_join(
    #     df_hosp,
    #     by = c("hospital_code" = "HospitalCode")
    #     ) %>% 
    # filter(str_detect(Grupo, "(SP|RJ)"))

# save(df , file = "data/RDSL_ICU_admissions_2010_2020_All_raw_final.RData")
# write_csv(df, "data/RDSL_ICU_admissions_2010_2020_All_raw_final.RData")
# write_csv(df, "data/RDSL_ICU_admissions_2016_2020_RJ_SP.csv.gz")
write_csv(db %>% filter(status_covid19 == "Confirmado", adm_year == "2020")
          , "data/RDSL_ICU_admissions_covid_2020.csv")



### Filters non-covid cases (514,292 patients)
# db <- db %>% 
#     filter(status_covid19 == "Negativo/Sem suspeição")




# Final and Exporting -----------------------------------------------------
# Initial date ("2011-01-01")
min(db$unit_admission_date)
# End Date ("2020-12-31")
max(db$unit_admission_date)
# Number of ICUs (165)
length(unique(db$unit_code))
# number of Hospitals (45)
length(unique(db$hospital_code))

db2 <- db %>% filter(adm_year == "2019")
# ### Exporting 'raw' dataset
# save(db2, file = "data/RDSL_ICU_admissions_filtered_2019.RData")
# 
# 











################################################################################
################################################################################
################################################################################
##### Dataset imputation



# Preparing imputation  ---------------------------------------------------

### Imputation dataset
to_imput <- db %>%
    select(age, saps, cci, ecog, adm_type, infection,mv, vp, adm_year, death)


### Missing values plot (Supplementary material)
library(naniar)
to_imput2 <- to_imput

colnames(to_imput2) <- c("Age", "SAPS 3", "CCI", "Performance Status", 
                         "Admission Type", "Infection", "MV", "Vasopressors", 
                         "Admission Year", "Death")

## Plot - Missing patterns per variables in the dataset
vis_miss(to_imput2, warn_large_data = FALSE)

## Plot - Proportion of missing values per variables
gg_miss_var(to_imput2, show_pct = TRUE)

## Plot - Proportion of missing values per variables and years
mygg_miss_var <- function(x) {gg_miss_var(x, show_pct = TRUE) + coord_flip(ylim = c(0,6))}
rmissmap <- map(to_imput2 %>% group_split(`Admission Year`), mygg_miss_var)

# Plot building - Missing values per variables and years
(rmissmap[[1]] + rmissmap[[2]] + rmissmap[[3]] + rmissmap[[2]] + rmissmap[[5]] +
        rmissmap[[6]] + rmissmap[[7]] + rmissmap[[8]] + rmissmap[[9]] + rmissmap[[10]]) +
    plot_annotation(tag_levels = list(c("2011", "2012", "2013", "2014", "2015",
                                        "2016", "2017", "2018", "2019", "2020")))


## Obtaining number of missing values per variables and year
levels(to_imput2$`Admission Year`)

ff1 <- function(x) {sum(is.na(x))}

mvalues <- to_imput %>%
    group_by(adm_year) %>%
    summarise(
        across(everything(), ff1),
        n = n()
        )


# Performing imputation ---------------------------------------------------
library(mice)

tempData <- mice(to_imput, m = 5, maxit = 50, meth = 'pmm', seed = 500)

summary(tempData)

## Checking imputation data
sapply(complete(tempData, 1), FUN = function(x){sum(is.na(x))})

tempData$loggedEvents

## Obtaining final dataset from imputation
t1 <- complete(tempData,1)

## Adding id and LOS values to final dataset
t1$unit_code <- db$unit_code

t1$icu_los <- db$unit_length_stay

## Selecting final variables for the dataset
t1 <- t1[c("age", "saps", "cci","ecog","adm_type","infection","mv","vp","icu_los","death","unit_code","adm_year")]



## Exporting 'imputed' final dataset ('t1')
# save(t1, file = "data/RDSL_ICU_admissions_2010_2020_NonCOVID_imputed_final.RData")

save(df, file = "data/RDSL_ICU_admissions_2019_2020_raw.RData")
write_csv(df, "data/RDSL_ICU_admissions_2019_2020_raw.csv")




# Finished