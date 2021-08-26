################################################################################
##### ExtraCOVID - Analysis of COVID-19 impact on non-covid ICU patients
#####  
##### Data Analysis: Descriptive and Secular trends and Figures 1 and 2
##### Fernando Zampieri (@f_g_zampieri), Leonardo Bastos (@lslbastos)
#####
################################################################################

# Package Load ------------------------------------------------------------
library(tidyverse)
library(emmeans)


# Input data --------------------------------------------------------------
## Obtaining data from data prep code
# source("code/2_Data_Analysis/0_data_preparation.R")

## Obtaining data from object ('t1' dataframe)
load("data/RDSL_ICU_admissions_2010_2020_NonCOVID_imputed_final.RData")




# Diagnostic plots (Figure 1) ---------------------------------------------

## Obtaining intervals
get95 <- function(x) {
    ymin = as.numeric(mean(x) - sd(x))
    ymax = as.numeric(mean(x) + sd(x))
    return(c(ymin = ymin, ymax = ymax))
}

## Obtaining median and IQR
median.quartile <- function(x) {
    out <- quantile(x, probs = c(0.25, 0.5, 0.75))
    names(out) <- c("ymin", "y", "ymax")
    return(out)
}

### Obtaining Diagnostic plots (Figure 1)

library(gghalves)

## Plot SAPS-3 and Age
tagesaps <- t1 %>% 
    select(age, saps, adm_year) %>%
    pivot_longer(cols = age:saps) %>%
    ggplot(aes(x = adm_year, y = value, color = name)) +
    stat_summary(fun.data = median.quartile,
                 geom = "pointrange",
                 position = position_dodge(width = 0.5)) +
    stat_summary(
        geom = "line",
        fun = median,
        aes(group = name),
        position = position_dodge(width = 0.5)
    ) +
    ggsci::scale_color_aaas(name = "", labels = c("Age", "SAPS 3")) +
    labs(x = "", y = "Median [IQR]") +
    coord_cartesian(ylim = c(18, 100)) +
    theme_half_open() %+replace% theme(axis.text.x = element_text(angle = 45))

## Plot "Death" / Mortality
tdeath <- t1 %>%
    group_by(adm_year) %>%
    summarise(mortality = mean(death == 1)) %>%
    ggplot(aes(x = adm_year, y = mortality)) +
    geom_bar(stat = "identity", fill = "navyblue", color = "black") +
    scale_y_continuous(labels = scales::percent) +
    theme_half_open() %+replace% theme(axis.text.x = element_text(angle = 45)) +
    labs(x = "", y = "Mortality")

## Plot Number of admissions / Proportion of COVID19 patients
tadm <- df %>%
    mutate(
        covid = ifelse( status_covid19 == "Confirmado", "Confirmed",
        ifelse(
            status_covid19 == "Suspeito, não confirmado",
            "Possible",
            "Negative"
            )
        )
        ) %>%
    mutate(covid = factor(covid, levels = c("Confirmed", "Possible", "Negative"))) %>%
    ggplot(aes(x = factor(adm_year), fill = covid)) +
    geom_bar(color = "black") +
    theme_half_open() %+replace% theme(axis.text.x = element_text(angle = 45)) +
    scale_fill_manual(values = c("darkred", "red", "navyblue"),
                      name = c("COVID Status")) +
    labs(x = "", y = "Admissions")

## Plot Type of admissionss
tadm_type <- t1 %>%
    ggplot(aes(x = adm_year, fill = adm_type)) + 
    geom_bar(position = "fill", color = "black") +
    theme_half_open() %+replace% theme(axis.text.x = element_text(angle = 45)) +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_manual(
        values = c("darkred", "navyblue", "red"),
        name = c(""),
        labels = c("Elective Surgery", "Medical", "Urgent Surgery")
    ) +
    labs(x = "", y = "% Admissions")


## Exporting Figure 1
pdf("Figure_1", width = 14, height = 8)
(tagesaps + tdeath) / (tadm + tadm_type) + plot_annotation(tag_levels = "A")
dev.off()









# Characteristics and outcomes per year -----------------------------------


### Table 1

library(gtsummary)
theme_gtsummary_journal(journal = "jama")
theme_gtsummary_compact()

table1 <- t1 %>%
    select(-unit_code) %>%
    mutate(
        adm_year = droplevels(adm_year),
        ecog = fct_recode(
            ecog,
            "Absent/Minor" = "0",
            "Moderate" = "1",
            "Severe" = "2"
        ),
        adm_type = fct_recode(
            adm_type,
            "Elective Surgery" = "eSurgery",
            "Medical" = "Medical",
            "Urgent Surgery" = "uSurgery"
        )
    ) %>%
    tbl_summary(
        by = adm_year,
        type = list(
            vp ~ "dichotomous",
            infection ~ "dichotomous",
            mv ~ "dichotomous",
            death ~ "dichotomous"
        ),
        statistic = list(
            all_continuous() ~ "{mean} ({sd})",
            all_categorical() ~ "{n} ({p}%)",
            all_dichotomous() ~ "{n} ({p}%)"
        ),
        label = list(
            age ~ "Age, years",
            saps ~ "SAPS 3, points",
            cci ~ "CCI, points",
            ecog ~ "Performance Impairment",
            adm_type ~ "Admission Type",
            infection ~ "Presence of Infection",
            mv ~ "Mechanical Ventilation at admission",
            vp ~ "Vasopressor at admission",
            death ~ "Hospital Mortality",
            icu_los ~ "ICU Length-of-stay (days)"
        )
    ) %>%
    #  add_p() %>%
    add_overall() %>%
    bold_labels()

table1 <- as_flex_table(table1)

## If you want to export the table
# library(flextable)
# setwd("/home/fernando/Desktop")
# save_as_pptx(as_flex_table(table1),path="/home/fernando/Desktop/oi.pptx")


#Checking saps linearity
t1 %>%
    mutate(saps_dec = cut(saps, breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 150))) %>%
    ggplot(aes(x = saps_dec, fill = death)) +
    geom_bar(position = "fill") +
    facet_wrap( ~ adm_year, ncol = 5) +
    theme_classic() %+replace% theme(axis.text.x = element_text(angle = 90)) +
    scale_fill_manual(values = c("white", "black"), guide = NULL) +
    geom_hline(
        yintercept = c(0.25, 0.50, 0.75),
        linetype = 2,
        alpha = 0.4
    ) +
    scale_y_continuous(labels = scales::percent) +
    labs(x = "", y = "Mortality (%)")












### Assessing calibration of curves per year caret
library(caret)
library(classifierplots)

t2 <- t1 %>% 
    select(adm_year, death)

t2$saps_prob <- db$saps3death_probability_standard_equation / 100

# Obtaining calibration per year
callme <- function(x) {
    if (x == "all") {
        year_data = subset(t2, adm_year = !"2020")
    } else {
        year_data <- t2 %>% filter(adm_year %in% x)
    }
    calresult <-
        calibration(death ~ saps_prob, class = "1", data = year_data)
    calresult2 <-
        data.frame(
            bin = calresult$data$bin,
            Percent = calresult$data$Percent,
            Lower = calresult$data$Lower,
            Upper = calresult$data$Upper,
            adm_year = as.character(x)
        )
    return(calresult2)
}



ppdata <- data.frame(x = c("[0,0.0909]", "(0.909,1]"), y = c(0, 100))



cplot <-
    lapply(list("all", 2011, 2018, 2019, 2020), callme) %>% 
    bind_rows() %>%
    mutate(adm_year = as.factor(adm_year)) %>%
    ggplot(aes(
        x = bin,
        y = Percent,
        group = adm_year,
        color = adm_year
    )) +
    geom_path(size = 1) +
    #    geom_errorbar(aes(ymin=Lower,ymax=Upper),width=0)+
    scale_x_discrete(labels = 1:11) +
    geom_line(
        inherit.aes = FALSE,
        data = ppdata,
        aes(x = x, y = y, group = 1),
        linetype = 2
    ) +
    ggsci::scale_color_jama(name = "Year",
                            labels = c("2011", "2018", "2019", "2020", "2011-2020")) +
    labs(x = "Bins", y = "Percentage (%)") +
    theme_minimal_grid() %+replace% theme(
        axis.text.x = element_text(size = 8),
        axis.title.x = element_text(size =
                                        12, face = "bold"),
        axis.text.y = element_text(size =
                                       8)
    )


# Using classifierplots
library(classifierplots)

callme <-
    function(i) {
        calibration_plot(db$death[db$adm_year == i], db$saps_prob[db$adm_year == i]) +
            theme_minimal() +
            labs(subtitle = i)
    }

cal_plots <- lapply(2011:2020, callme)



## Combining calibration plots
library(cowplot)
plot_grid(
    cal_plots[[1]],
    cal_plots[[2]],
    cal_plots[[3]],
    cal_plots[[4]],
    cal_plots[[5]],
    cal_plots[[6]],
    cal_plots[[7]],
    cal_plots[[8]],
    cal_plots[[9]],
    cal_plots[[10]],
    ncol = 5
)
