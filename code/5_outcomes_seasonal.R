################################################################################
##### ExtraCOVID - Analysis of COVID-19 impact on non-covid ICU patients
#####  
##### Analysis of outcomes during seasons of the year - COVID19
##### Fernando Zampieri (@f_g_zampieri), Leonardo Bastos (@lslbastos)
#####
################################################################################
library(tidyverse)
library(lme4)

# Input data --------------------------------------------------------------
## Obtaining data from data prep code
# source("code/2_Data_Analysis/0_data_preparation.R")

## Obtaining data from object ('t1' dataframe)
load("data/RDSL_ICU_admissions_2010_2020_NonCOVID_imputed_final.RData")
load("data/RDSL_ICU_admissions_2010_2020_NonCOVID_raw_final.RData")


# Data Analysis -----------------------------------------------------------
season <- t1 %>% select(saps, death, unit_code, adm_year)
season$adm_date <- db$unit_admission_date

save(season, file = "data/season.RData")

season$pweek <- lubridate::week(season$adm_date)



season <- season %>% filter(adm_year %in% c("2019", "2020"))
season$adm_year <- droplevels(season$adm_year)


season$period <- as.character(season$adm_year)
season$period[season$adm_year == "2020"] <-
    ifelse(
        season$pweek[season$adm_year == "2020"] <= 10,
        "before",
        ifelse(
            season$pweek[season$adm_year == "2020"] > 10 &
                season$pweek[season$adm_year == "2020"] <= 24,
            "peak1",
            ifelse(season$pweek[season$adm_year ==
                                    "2020"] > 25 & season$pweek[season$adm_year == "2020"] <= 41, "plateau",
                   "peak2")
        )
    )

season$period <- as.factor(season$period)
season$period <-
    factor(season$period,
           levels = c("2019", "before", "peak1", "plateau", "peak2"))

mseason <- glmer(
    death ~ saps * period + (period | unit_code),
    family = "binomial",
    data = season,
    nAGQ = 0,
    control = glmerControl(optCtrl = list(maxfun = 1e6))
)


summary(mseason)


pp <- lsmeans(
    mseason,
    pairwise ~ period | saps,
    type = "response",
    reverse = TRUE,
    adjust = "tukey",
    at = list(saps = c(20, 30, 40, 50, 60, 70, 80, 90, 100))
)

orplot <- as.data.frame(print(pp$contrasts))
orplot <-
    orplot %>% select(-df, -z.ratio, -null) %>% filter(str_detect(contrast, "2019"))

orplot$odds.ratio <- -log(orplot$odds.ratio)
orplot$lower <- exp(orplot$odds.ratio - 1.96 * orplot$SE)
orplot$upper <- exp(orplot$odds.ratio + 1.96 * orplot$SE)
orplot$odds.ratio <- exp(orplot$odds.ratio)

orplot %>% ggplot(aes(x = contrast, y = odds.ratio)) +
    geom_point() +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
    facet_wrap( ~ saps) +
    scale_x_discrete(labels = c(
        "Baseline/2019",
        "First Peak/2019",
        "Plateau/2019",
        "Second Peak/2019"
    )) +
    geom_hline(yintercept = 1,
               linetype = 1,
               alpha = 0.2) +
    geom_hline(
        yintercept = seq(0.5, 2, by = 0.5),
        linetype = 2,
        alpha = 0.2
    ) +
    cowplot::theme_cowplot() %+replace% theme(
        axis.text.x = element_text(angle = 90, size = 8),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12, angle = 90)
    ) +
    labs(x = "", y = "OR 95% CI")
