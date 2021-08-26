################################################################################
##### ExtraCOVID - Analysis of COVID-19 impact on non-covid ICU patients
#####  
##### Data Analysis: Models (risk, time and calibration) and VLAD
##### Fernando Zampieri (@f_g_zampieri), Leonardo Bastos (@lslbastos)
#####
################################################################################

# Libraries ---------------------------------------------------------------
library(tidyverse)
library(lme4)

# Input data --------------------------------------------------------------
## Obtaining data from data prep code
# source("code/2_Data_Analysis/0_data_preparation.R")


## Obtaining data from object ('t1' dataframe)
load("data/RDSL_ICU_admissions_2010_2020_NonCOVID_imputed_final.RData")






# Modeling ----------------------------------------------------------------


### Estimating models
m1 <- glmer(
    death ~ saps * adm_year + (1 | unit_code),
    family = "binomial",
    data = t1,
    nAGQ = 0
)
m2 <- glmer(
    death ~ saps + adm_year + (adm_year | unit_code),
    family = "binomial",
    data = t1,
    nAGQ = 0,
    control = glmerControl(optCtrl = list(maxfun = 1e6))
)
m3 <- glmer(
    death ~ saps * adm_year + (adm_year | unit_code),
    family = "binomial",
    data = t1,
    nAGQ = 0,
    control = glmerControl(optCtrl = list(maxfun = 1e6))
)
summary(m3)
anova(m1, m3)

#tt<-expand_grid(saps=30:100,adm_year=levels(t1$adm_year))
#library(sjPlot)


### Estimating margin effects per

# Using ggeffects
library(ggeffects)

ggp_random <- ggpredict(m3,
                        terms = c("saps [30:100]", "adm_year"),
                        interval = "confidence")

fig2B <- ggp_random %>%
    filter(group %in% c("2011", "2018", "2019", "2020")) %>%
    ggplot(aes(x, predicted, colour = group)) + geom_line(size = 1) +
    scale_y_continuous(labels = scales::percent) +
    ggsci::scale_color_jama(name = "Year") +
    labs(x = "SAPS 3", y = "Predicted Mortality") +
    theme_minimal_grid() %+replace% theme(
        axis.text.x = element_text(size = 8),
        axis.title.x = element_text(size =
                                        12, face = "bold"),
        axis.text.y = element_text(size =
                                       8)
    )



## Ussing emmeans to estimate OR per SAPS-3 and year (adjusted by unit) (Figure 4)
library(emmeans)

pp <-
    lsmeans(
        m3,
        pairwise ~ adm_year |
            saps,
        type = "response",
        adjust = "tukey",
        at = list(saps = c(20, 30, 40, 50, 60, 70, 80, 90, 100))
    )
pp2 <-
    emmeans(
        m3,
        pairwise ~ adm_year | saps,
        type = "response",
        adjust = "bonferroni",
        at = list(saps = c(20, 30, 40, 50, 60, 70, 80, 90, 100))
    )

orplot <- as.data.frame(print(pp$contrasts))
orplot <-
    orplot %>% filter(str_detect(contrast, "/ 2020")) %>% select(-df, -z.ratio)

orplot$odds.ratio <- -log(orplot$odds.ratio)
orplot$lower <- exp(orplot$odds.ratio - 1.96 * orplot$SE)
orplot$upper <- exp(orplot$odds.ratio + 1.96 * orplot$SE)
orplot$odds.ratio <- exp(orplot$odds.ratio)

orplot %>%
    select(saps, odds.ratio, lower, upper, p.value) %>%
    mutate_all(
        .funs = function(x) {
            round(as.numeric(x), 3)
        }
    ) %>%
    mutate(odds.ratio2 = paste0(odds.ratio, " [", lower, "-", upper, "]")) %>%
    select(-odds.ratio) %>%
    select(saps, odds.ratio2, p.value) %>%
    writexl::write_xlsx(path = "ordata.xlsx")

fig4 <- orplot %>% ggplot(aes(x = contrast, y = odds.ratio)) +
    geom_point() +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
    scale_y_continuous(breaks = seq(0.2, 2, by = 0.2)) +
    scale_x_discrete(labels = 2011:2019) +
    coord_cartesian(ylim = c(0.2, 2)) +
    geom_hline(yintercept = 1, linetype = 2) +
    facet_wrap( ~ saps) +
    cowplot::theme_cowplot() %+replace% theme(
        axis.text.x = element_text(angle = 45, size = 8),
        axis.text.y = element_text(size =
                                       8),
        axis.title.x = element_text(size =
                                        12),
        axis.title.y = element_text(size =
                                        12, angle = 90)
    ) +
    labs(x = "Comparison Year", y = "OR 95% CI")


## Exporting Figure 4
pdf("output/Figure_4.pdf", width = 10, height = 10)
fig4
dev.off()


# Subgroup analysis (ESM) -------------------------------------------------

## Estimating models for each sugroup (eSurgery, Medical, Infections)

table(t1$adm_type)

m3.surg <- glmer(
    death ~ saps * adm_year + (adm_year | unit_code),
    family = "binomial",
    data = subset(t1, adm_type == "eSurgery"),
    nAGQ = 0,
    control = glmerControl(optCtrl = list(maxfun = 1e6))
)
m3.med <- glmer(
    death ~ saps * adm_year + (adm_year | unit_code),
    family = "binomial",
    data = subset(t1, adm_type == "Medical"),
    nAGQ = 0,
    control = glmerControl(optCtrl = list(maxfun = 1e6))
)

m3.ecog0 <- glmer(
    death ~ saps * adm_year + (adm_year | unit_code),
    family = "binomial",
    data = subset(t1, ecog == "0"),
    nAGQ = 0,
    control = glmerControl(optCtrl = list(maxfun = 1e6))
)
m3.ecog1 <- glmer(
    death ~ saps * adm_year + (adm_year | unit_code),
    family = "binomial",
    data = subset(t1, ecog == "1"),
    nAGQ = 0,
    control = glmerControl(optCtrl = list(maxfun = 1e6))
)
m3.ecog2 <- glmer(
    death ~ saps * adm_year + (adm_year | unit_code),
    family = "binomial",
    data = subset(t1, ecog == "2"),
    nAGQ = 0,
    control = glmerControl(optCtrl = list(maxfun = 1e6))
)
m3.infection <- glmer(
    death ~ saps * adm_year + (adm_year | unit_code),
    family = "binomial",
    data = subset(t1, infection == "1"),
    nAGQ = 0,
    control = glmerControl(optCtrl = list(maxfun = 1e6))
)
m3.mv <- glmer(
    death ~ saps * adm_year + (adm_year | unit_code),
    family = "binomial",
    data = subset(t1, mv == "1"),
    nAGQ = 0,
    control = glmerControl(optCtrl = list(maxfun = 1e6))
)


orfig <- function(m = m3) {
    pp <-
        lsmeans(m,
                pairwise ~ adm_year |
                    saps,
                type = "response",
                at = list(saps = c(20, 30, 40, 50, 60, 70, 80, 90, 100)))
    orplot <- as.data.frame(print(pp$contrasts))
    orplot <-
        orplot %>% filter(str_detect(contrast, "/ 2020")) %>% select(-df, -z.ratio)
    
    orplot$odds.ratio <- -log(orplot$odds.ratio)
    orplot$lower <- exp(orplot$odds.ratio - 1.96 * orplot$SE)
    orplot$upper <- exp(orplot$odds.ratio + 1.96 * orplot$SE)
    orplot$odds.ratio <- exp(orplot$odds.ratio)
    
    figx <- orplot %>% ggplot(aes(x = contrast, y = odds.ratio)) +
        geom_point() +
        geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
        #    scale_y_log10(breaks=seq(0.1,1.5,by=0.1))+
        scale_x_discrete(labels = 2011:2019) +
        coord_cartesian(ylim = c(0.2, 2)) +
        geom_hline(yintercept = 1, linetype = 2) +
        facet_wrap( ~ saps) +
        cowplot::theme_cowplot() %+replace% theme(
            axis.text.x = element_text(angle = 45, size = 8),
            axis.text.y = element_text(size =
                                           6),
            axis.title.x = element_text(size =
                                            12),
            axis.title.y = element_text(size =
                                            12, angle = 90)
        ) +
        labs(x = "Comparison Year", y = "OR 95% CI")
    
    return(figx)
}

## Obtaining OR figures for sugroups
orfig(m3.mv)
