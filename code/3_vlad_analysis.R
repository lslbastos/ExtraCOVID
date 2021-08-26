################################################################################
##### ExtraCOVID - Analysis of COVID-19 impact on non-covid ICU patients
#####
##### Data Analysis: VLAD estimation COVID vs NonCOVID per week
##### Fernando Zampieri (@f_g_zampieri), Leonardo Bastos (@lslbastos)
#####
################################################################################
# Libraries ---------------------------------------------------------------
library(tidyverse)


# Input data --------------------------------------------------------------
## Obtaining data from data prep code
# source("code/2_Data_Analysis/0_data_preparation.R")

## Obtaining raw dataframe from object ('df' dataframe) - COVID + NonCovid
load("data/RDSL_ICU_admissions_2010_2020_NonCOVID_imputed_final.RData")






# VLAD calculation (COVID vs NonCOVID) ------------------------------------

### Obtaining dataset for VLAD calculating
vlad <- df %>%
    filter(year(unit_admission_date) == "2020") %>%
    mutate(psaps = saps3death_probability_standard_equation,
           death,
           pweek = week(unit_admission_date)) %>%
    select(death, psaps, pweek, status_covid19) %>%
    mutate(
        psaps = psaps / 100,
        covid = fct_recode(
            status_covid19,
            "COVID" = "Confirmado",
            "COVID" = "Suspeito, não confirmado",
            "Not_COVID" = "Negativo/Sem suspeição"
        )
    ) %>%
    select(-status_covid19)

# Defining VLAD points per survival/death outcome
vlad$points[vlad$death == 0] <- vlad$psaps[vlad$death == 0]
vlad$points[vlad$death == 1] <- (1 - vlad$psaps[vlad$death == 1]) * -1




## Calculating VLAD points for NonCOVID patients per week (plot)
pvladA <- vlad %>%
    filter(covid != "COVID") %>%
    group_by(pweek) %>%
    summarise(points = sum(points)) %>%
    ggplot(aes(
        x = factor(pweek),
        y = points,
        group = 1
    )) +
    geom_point() +
    geom_line() +
    geom_smooth(method = "gam",
                color = "black",
                linetype = 3) +
    labs(y = "VLAD for Non-COVID Cases") +
    theme_minimal_grid() %+replace% theme(
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(size = 12, face = "bold", angle = 90)
    )

## Calculating Total number of COVID19 patients per week (plot)
pvladB <- vlad %>%
    filter(covid == "COVID") %>%
    group_by(pweek) %>%
    summarise(n = n()) %>%
    bind_rows(data.frame(pweek = 1:7, n = 0), .) %>%
    ggplot(aes(
        x = factor(pweek),
        y = n,
        group = 1
    )) +
    geom_point() +
    geom_line() +
    geom_smooth(method = "gam",
                color = "black",
                linetype = 3) +
    labs(x = "Epidemiological Week in 2020", y = "COVID Admissions (n)") +
    theme_minimal_grid() %+replace% theme(
        axis.text.x = element_text(size = 8),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(size = 12, face = "bold", angle = 90)
    )


## Combining plots for Figure 2
library(patchwork)

## Exporting Figure 2
pdf("Figure_2.pdf", width = 12, height = 10)
(pvladA / pvladB) + plot_annotation(tag_levels = 'A')
dev.off()
