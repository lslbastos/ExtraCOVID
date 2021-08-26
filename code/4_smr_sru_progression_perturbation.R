################################################################################
##### ExtraCOVID - Analysis of COVID-19 impact on non-covid ICU patients
#####  
##### SMR and SRU progression analysis and perturbation scores
##### Fernando Zampieri (@f_g_zampieri), Leonardo Bastos (@lslbastos)
#####
################################################################################

# Library -----------------------------------------------------------------
library(tidyverse)

## Loading data 'db'
load("data/RDSL_ICU_admissions_2010_2020_All_raw_final.RData")

 
# # Units with COVID-19 vs. No COVID-19 -------------------------------------
# Selecting years
year_sel <- c("2016", "2017", "2018", "2019", "2020")
# Number of periods: 
# 2 - bimester
# 3 - quarters
time_period <- 2

### Patients admitted in 2016-2020 - Non-COVID patients
db_noncovid_years <- 
    db %>% 
    filter(
        adm_year %in% year_sel,
        status_covid19 == "Negativo/Sem suspeição"
    ) %>% 
    mutate(
        death      = as.numeric(as.character(death)),
        adm_mon    = lubridate::month(unit_admission_date),
        adm_period = ceiling(adm_mon / time_period),
        period     = case_when(
            # adm_year == "2015" ~ adm_bimon + (12 / period) * 0,
            adm_year == "2016" ~ adm_period + (12 / time_period) * 0,
            adm_year == "2017" ~ adm_period + (12 / time_period) * 1,
            adm_year == "2018" ~ adm_period + (12 / time_period) * 2,
            adm_year == "2019" ~ adm_period + (12 / time_period) * 3,
            adm_year == "2020" ~ adm_period + (12 / time_period) * 4
            ),
        hospital_code = paste0(hospital_code, unit_code)
        ) %>%
    mutate(
        SRU_Days = case_when(
            death == 0 & saps3points <= 24 ~ 2.3,
            death == 0 & between(saps3points, 25, 34) ~ 3.2,
            death == 0 & between(saps3points, 35, 44) ~ 4.3,
            death == 0 & between(saps3points, 45, 54) ~ 7.2,
            death == 0 & between(saps3points, 55, 64) ~ 11.0,
            death == 0 & between(saps3points, 65, 74) ~ 16.6,
            death == 0 & between(saps3points, 75, 84) ~ 22.2,
            death == 0 & between(saps3points, 85, 94) ~ 29.4,
            death == 0 & saps3points >= 95 ~ 39.0,
            death == 1 ~ 0
            )
        )
    


## Obtaining monthly SMR for all units per time period
df_units_noncovid_years <-
    db_noncovid_years %>% 
    group_by(hospital_code, adm_year, adm_period) %>% 
    summarise(
        admissions = n(),
        smr = sum(death) / sum(saps_prob),
        sru = sum(unit_length_stay) / sum(SRU_Days)
    ) %>% 
    ungroup()


# cardio_units <- db_noncovid_years %>% 
#     filter(str_detect(admission_reason_name, "(C|c)ardio")) %>% 
#     distinct(hospital_code)

### Filter: units with admissions in all periods
## bimester: 30 bimesters (2016-2020)
## quarter:  24 quarters (2016-2020)
df_hosp_unit_all_months <- 
    df_units_noncovid_years %>% 
    # filter(!(hospital_code %in% cardio_units$hospital_code)) %>% 
    # filter(smr > 0) %>% # filter positive SMR values
    # filter(admissions >= quantile(admissions, probs = 0.25)) %>% # filter by Q1 of admissions
    count(hospital_code) %>% 
    filter(
        n == (12 / time_period) * length(year_sel) 
    )


total_selected_units <- df_hosp_unit_all_months %>% count() %>% pull()


## Recalculating SMR per period for selected units
db_noncovid_years_selected_units <- 
    db_noncovid_years %>% 
    inner_join(
        df_hosp_unit_all_months
    ) %>%
    group_by(hospital_code, adm_year, adm_period) %>%
    summarise(
        total = n(),
        deaths = sum(death),
        smr = sum(death) / sum(saps_prob),
        sru = sum(unit_length_stay) / sum(SRU_Days),
        saps_avg = mean(saps3points)
    ) %>% 
    ungroup() %>%
    mutate(
        period = case_when(
            adm_year == "2016" ~ adm_period + (12 / time_period) * 0,
            adm_year == "2017" ~ adm_period + (12 / time_period) * 1,
            adm_year == "2018" ~ adm_period + (12 / time_period) * 2,
            adm_year == "2019" ~ adm_period + (12 / time_period) * 3,
            adm_year == "2020" ~ adm_period + (12 / time_period) * 4
        )
    ) %>% # year label for plots
    mutate(
        year_ref  = (ceiling(period / (12 / time_period)) - 1) * (12 / time_period)
    )



## Calculating average SMR per admission period
db_noncovid_years_selected_units_avg <-
    db_noncovid_years_selected_units %>%
    group_by(period, adm_year) %>%
    summarise(
        avg_smr = mean(smr),
        avg_smr_low  = mean(smr) - 1.96 * sd(smr) / sqrt(n()),
        avg_smr_high = mean(smr) + 1.96 * sd(smr) / sqrt(n()),
        avg_sru = mean(sru),
        avg_sru_low  = mean(sru) - 1.96 * sd(sru) / sqrt(n()),
        avg_sru_high = mean(sru) + 1.96 * sd(sru) / sqrt(n())
    )


## Calculating overall SMR per admission period
db_noncovid_years_selected_units_overall <-
    db_noncovid_years %>% 
    filter(
        hospital_code %in% df_hosp_unit_all_months$hospital_code
    ) %>%
    group_by(period) %>% 
    summarise(
        smr = sum(death) / sum(saps_prob),
        sru = sum(unit_length_stay) / sum(SRU_Days)
        ) 



# Year Label for plot
df_year_label <- 
    db_noncovid_years_selected_units %>% 
    distinct(adm_year, year_ref)



# min_scale <- min(pmin(db_noncovid_years_selected_units$smr, db_noncovid_years_selected_units$sru))
min_scale <- 0.03
max_scale <- max(pmax(db_noncovid_years_selected_units$smr, db_noncovid_years_selected_units$sru))


# Plot: SMR per hospital and Period
plot_noncovid_years_selected_units_smr <-
    db_noncovid_years_selected_units %>% 
    ggplot() +
    geom_vline(data = df_year_label, aes(xintercept = year_ref), 
               linetype = "solid", color = "black") +
    geom_line(aes(x = period, y = smr, group = hospital_code), color = "gray60") +
    geom_line(data = db_noncovid_years_selected_units_avg,
              aes(x = period, y = avg_smr, group = 1),
              color = "blue", size = 1.2
              ) +
    geom_line(data = db_noncovid_years_selected_units_overall, 
              aes(x = period, y = smr, group = 1), 
              color = "red", size = 1.2
    ) +
    geom_text(data = df_year_label, 
              aes(x = year_ref + 2, 
                  y = max(db_noncovid_years_selected_units$sru) + 0.2, label = adm_year)) +
    geom_hline(aes(yintercept = 1), linetype = "dashed") +
    scale_x_continuous(breaks = seq(1, (12 / time_period) * length(year_sel) , 1),
                       # labels = rep(month.abb[seq(3, 12, 3)], 5)
                       ) +
    coord_cartesian(ylim = c(min_scale, max_scale)) +
    scale_y_continuous(trans = "log10") +
    labs(
        y = "SMR", x = "Bimester"
        # title = "",
        # subtitle = paste0(total_selected_units, " units\n",
        #                   "Gray lines: ICU \nBlue line: Average SMR \nRed line: Overall SMR")
    ) +
    theme_bw() 





# Plot: SRU per hospital and Period
plot_noncovid_years_selected_units_sru <-
    db_noncovid_years_selected_units %>% 
    ggplot() +
    geom_vline(data = df_year_label, aes(xintercept = year_ref), 
               linetype = "solid", color = "black") +
    geom_line(aes(x = period, y = sru, group = hospital_code), color = "gray60") +
    geom_line(data = db_noncovid_years_selected_units_avg,
              aes(x = period, y = avg_sru, group = 1),
              color = "blue", size = 1.2
    ) +
    geom_line(data = db_noncovid_years_selected_units_overall, 
              aes(x = period, y = sru, group = 1), 
              color = "red", size = 1.2
    ) +
    geom_text(data = df_year_label, 
              aes(x = year_ref + 2, 
                  y = max(db_noncovid_years_selected_units$sru) + 0.2, label = adm_year)) +
    geom_hline(aes(yintercept = 1), linetype = "dashed") +
    scale_x_continuous(breaks = seq(1, (12 / time_period) * length(year_sel) , 1),
                       # labels = rep(month.abb[seq(3, 12, 3)], 5)
    ) +
    coord_cartesian(ylim = c(min_scale, max_scale)) +
    scale_y_continuous(trans = "log10") +
    labs(
        y = "SRU", x = "Bimester"
        # title = "",
        # subtitle = paste0(total_selected_units, " units\n",
        #                   "Gray lines: ICU \nBlue line: Average SRU \nRed line: Overall SRU")
    ) +
    theme_bw() 


library(patchwork)
plot_avg_smr_sru_time <- (
    plot_noncovid_years_selected_units_smr +
        plot_noncovid_years_selected_units_sru
)


ggsave("output/plot_avg_smr_sru_time.png", plot = plot_avg_smr_sru_time,
       width = 12, height = 4, dpi = 800)




# Only average values -----------------------------------------------------
set.seed(20210731)

df_smr_overall_CI_boot <-
    map(1:1000, function(x){
        
        df_month <-
            db_noncovid_years %>% 
            inner_join(
                df_hosp_unit_all_months
            ) %>%
            mutate(
                period = case_when(
                    adm_year == "2016" ~ adm_period + (12 / time_period) * 0,
                    adm_year == "2017" ~ adm_period + (12 / time_period) * 1,
                    adm_year == "2018" ~ adm_period + (12 / time_period) * 2,
                    adm_year == "2019" ~ adm_period + (12 / time_period) * 3,
                    adm_year == "2020" ~ adm_period + (12 / time_period) * 4
                )
            ) %>% # year label for plots
            mutate(
                year_ref  = (ceiling(period / (12 / time_period)) - 1) * (12 / time_period)
            ) %>% 
            group_by(hospital_code, period, saps3points) %>%
            sample_frac(1, replace = TRUE) %>%
            ungroup() %>% 
            group_by(hospital_code, period) %>%
            summarise(
                smr = sum(death) / sum(saps_prob),
                sru = sum(unit_length_stay) / sum(SRU_Days)
            ) %>% 
            ungroup() %>% 
            group_by(period) %>% 
            summarise(
                avg_smr = mean(smr),
                avg_sru = mean(sru)
                ) %>%
            ungroup()
        
        if (x %% 100 == 0) {print(x)}
        return(df_month)
        
    }) %>%
    bind_rows() %>%
    group_by(period) %>%
    summarise(
        avg_smr_low  = quantile(avg_smr, probs = 0.05/2),
        avg_smr_high = quantile(avg_smr, probs = 1 - 0.05/2),
        avg_sru_low  = quantile(avg_sru, probs = 0.05/2),
        avg_sru_high = quantile(avg_sru, probs = 1 - 0.05/2)
    )

# write_csv(df_smr_overall_CI_boot, "input/df_smr_overall_CI_boot.csv")



# Plot: SMR per hospital and Period
plot_noncovid_years_selected_units_smr_avg <-
    db_noncovid_years_selected_units_avg %>%
    ggplot() +
    geom_vline(data = df_year_label, aes(xintercept = year_ref),
               linetype = "dashed", color = "black") +
    # geom_line(aes(x = period, y = smr, group = hospital_code), color = "gray60") +
    # geom_smooth(aes(x = period, y = avg_smr, group = 1),
    #            color = "blue", size = 1.3
    # ) +
    geom_line(aes(x = period, y = avg_smr, group = 1),
               color = "blue", size = 0.2
    ) +
    geom_point(aes(x = period, y = avg_smr, group = 1),
              color = "blue", size = 1.3
    ) +
    geom_errorbar(data = df_smr_overall_CI_boot,
                  aes(x = period, ymin = avg_smr_low, ymax = avg_smr_high, group = 1),
              color = "blue", width = 0.3
    ) +

    # geom_line(data = db_noncovid_years_selected_units_overall,
    #           aes(x = period, y = smr, group = 1),
    #           color = "red", size = 1.2
    # ) +
    geom_text(data = df_year_label,
              aes(x = year_ref + 2,
                  y = max(db_noncovid_years_selected_units_avg$avg_smr) + 0.3, label = adm_year)) +
    # geom_hline(aes(yintercept = 1), linetype = "dashed") +
    scale_x_continuous(breaks = seq(1, (12 / time_period) * length(year_sel) , 1),
                       # labels = rep(month.abb[seq(3, 12, 3)], 5)
    ) +
    # coord_cartesian(ylim = c(min_scale, max_scale)) +
    # scale_y_continuous(trans = "log10") +
    labs(
        y = "Average SMR (95% Confidence Interval)", x = "Bimester"
        # title = "",
        # subtitle = paste0(total_selected_units, " units\n",
        #                   "Gray lines: ICU \nBlue line: Average SMR \nRed line: Overall SMR")
    ) +
    theme_bw() +
    theme(axis.title.y = element_text(size = 10))






# Plot: SRU per hospital and Period
plot_noncovid_years_selected_units_sru_avg <-
    db_noncovid_years_selected_units_avg %>%
    ggplot() +
    geom_vline(data = df_year_label, aes(xintercept = year_ref),
               linetype = "dashed", color = "black") +
    # geom_line(aes(x = period, y = sru, group = hospital_code), color = "gray60") +
    geom_point(aes(x = period, y = avg_sru, group = 1),
              color = "blue", size = 1.2
    ) +
    geom_line(aes(x = period, y = avg_sru, group = 1),
              color = "blue", size = 0.2
    ) +
    # geom_smooth(aes(x = period, y = avg_sru, group = 1),
    #             color = "blue", size = 1.3
    # ) +
    # geom_errorbar(aes(x = period, ymin = avg_sru_low, ymax = avg_sru_high, group = 1),
    #               color = "blue", width = 0.3
    # ) +
    geom_errorbar(data = df_smr_overall_CI_boot,
                  aes(x = period, ymin = avg_sru_low, ymax = avg_sru_high, group = 1),
                  color = "blue", width = 0.3
    ) +

    # geom_line(data = db_noncovid_years_selected_units_overall,
    #           aes(x = period, y = sru, group = 1),
    #           color = "red", size = 1.2
    # ) +
    geom_text(data = df_year_label,
              aes(x = year_ref + 2,
                  y = max(db_noncovid_years_selected_units_avg$avg_sru) + 0.3, label = adm_year)) +
    # geom_hline(aes(yintercept = 1), linetype = "dashed") +
    scale_x_continuous(breaks = seq(1, (12 / time_period) * length(year_sel) , 1),
                       # labels = rep(month.abb[seq(3, 12, 3)], 5)
    ) +
    # coord_cartesian(ylim = c(min_scale, max_scale)) +
    # scale_y_continuous(trans = "log10") +
    labs(
        y = "Average SRU (95% Confidence Interval)", x = "Bimester"
        # title = "",
        # subtitle = paste0(total_selected_units, " units\n",
        #                   "Gray lines: ICU \nBlue line: Average SRU \nRed line: Overall SRU")
    ) +
    theme_bw() +
    theme(axis.title.y = element_text(size = 10))





### Combining time series plots and gghalf
library(patchwork)
plot_avg_smr_sru_time <- (
    plot_noncovid_years_selected_units_smr_avg +
        plot_noncovid_years_selected_units_sru_avg
    )



strucchange::sctest(db_noncovid_years_selected_units_avg$avg_smr ~ db_noncovid_years_selected_units_avg$period, type = "Chow", point = 24)

strucchange::sctest(db_noncovid_years_selected_units_avg$avg_sru ~ db_noncovid_years_selected_units_avg$period, type = "Chow", point = 24)

# ggsave("output/plot_avg_smr_sru_time_bootstrap.png", plot = plot_avg_smr_sru_time,
#        width = 12, height = 4, dpi = 800)
# 




# df_test_smr_sru <-
#     db_noncovid_years_selected_units_avg %>% 
#     mutate(is_2020 = if_else(period > 25, 1, 0))
# 
# 
# wilcox.test(avg_smr ~ factor(is_2020), data = df_test_smr_sru)
# 
# 
# wilcox.test(avg_sru ~ factor(is_2020), data = df_test_smr_sru)
# 
# model_smr <- lm(smr ~ adm_year, data = db_noncovid_years_selected_units)
# 
# kruskal.test(smr ~ factor(is_2020), data = db_noncovid_years_selected_units)
# kruskal.test(sru ~ factor(is_2020), data = db_noncovid_years_selected_units)
# 
# 







# 
# 
# db_noncovid_years_selected_units %>% 
#     select(hospital_code, period, smr, sru) %>% 
#     pivot_longer(-c(hospital_code, period), names_to = "metric", values_to = "val") %>% 
#     ggplot() +
#     geom_vline(data = df_year_label, aes(xintercept = year_ref),
#                linetype = "solid", color = "black") +
#     geom_line(aes(x = period, y = val, group = hospital_code), color = "gray60") +
#     # geom_line(data = db_noncovid_years_selected_units_avg,
#     #           aes(x = period, y = avg_sru, group = 1),
#     #           color = "blue", size = 1.2
#     # ) +
#     # geom_line(data = db_noncovid_years_selected_units_overall, 
#     #           aes(x = period, y = sru, group = 1), 
#     #           color = "red", size = 1.2
#     # ) +
#     # geom_text(data = df_year_label, 
#     #           aes(x = year_ref + 2, 
#     #               y = max(db_noncovid_years_selected_units$sru) + 0.2, label = adm_year)) +
#     geom_hline(aes(yintercept = 1), linetype = "dashed") +
#     scale_x_continuous(breaks = seq(1, (12 / time_period) * 6, 1),
#                        # labels = rep(month.abb[seq(3, 12, 3)], 5)
#     ) +
#     scale_y_continuous(breaks = c(0, 0.03, 0.10, 0.30, 1, 3), trans = "log10") +
#     facet_wrap(. ~ metric) +
#     labs(
#         y = "SRU", x = "Bimester",
#         # title = "",
#         subtitle = paste0(total_selected_units, " units\n",
#                           "Gray lines: ICU \nBlue line: Average SRU \nRed line: Overall SRU")
#     ) +
#     theme_bw() 
# 










# Descriptive years -------------------------------------------------------
# 
# 
# tbl_summary_years <-
#     df_units_2018_2020_selected %>% 
#     mutate(
#         year = as.character(adm_year)
#     ) %>% 
#     bind_rows(
#         df_units_2018_2020_selected %>% 
#             mutate(
#                 year = as.character(adm_year)
#             ) %>% 
#             filter(as.numeric(year) < 2020) %>% 
#             mutate(
#                 year = paste0(min(year), "-", max(year))
#             )
#     ) %>%
#     select(year, smr, total, deaths, saps_avg) %>%
#     gtsummary::tbl_summary(
#         by = year, 
#         statistic = list(
#             gtsummary::all_continuous() ~ "{median} ({p25}, {p75}) [{min}, {max}]"
#         )
#     ) 
# 
# 
# library(flextable)
# save_as_docx(gtsummary::as_flex_table(tbl_summary_years), path = "output/table_years_compar.docx")
# 
# 
# 









# Dispersion test ---------------------------------------------------------

library(mdp)


# Perturbation SRU --------------------------------------------------------
# Data preparation
data_phen <-
    db_noncovid_years_selected_units %>%
    mutate(
        year = adm_year
    ) %>%
    mutate(
        year = case_when(
            adm_year == "2020" ~ "2020",
            TRUE ~ "2016_2019"
        )
    ) %>%
    group_by(hospital_code, year, adm_period) %>%
    summarise(
        smr = mean(smr)
    ) %>%
    ungroup() %>%
    mutate(hospital_code = paste0(hospital_code, "_", year))

# Genotype data
data_gen <-
    data_phen %>%
    select(-year) %>%
    pivot_wider(names_from = "hospital_code", values_from = "smr") %>%
    bind_cols() %>%
    select(-adm_period)

# Phenotype data (formatted)
data_phen <-
    data_phen %>%
    distinct(
        hospital_code, year
    )


## Estimating scores with default function
mdp_results <- 
    mdp(data = data_gen %>% as.data.frame(),
        pdata = data_phen %>%
            rename(
                Sample = hospital_code,
                Class = year
                ) %>% as.data.frame(),
        control_lab = "2016_2019", std = 0) # I used std = 0 so they do not remove "non-significant units"


## Obtaining data from Sample Scores
df_mdp_res_smr <- 
    mdp_results$sample_scores$allgenes %>% 
    as_tibble() %>% 
    select(Sample, Score, Class)



## Plots scores and time periods with GGhalves - Perturbation Scores
plot_half_score_smr <-
    df_mdp_res_smr %>% 
    mutate(
        Class = fct_recode(Class,
            "Average 2016-2019" = "2016_2019"
        )
    ) %>% 
    ggplot() +
    gghalves::geom_half_violin(aes(x = Class, y = Score, fill = Class),
                                side = "l") +
    gghalves::geom_half_dotplot(aes(x = Class, y = Score, fill = Class), 
                                method = "dotdensity", binwidth = 0.15) +
    scale_fill_manual(values = ggsci::pal_lancet()(9)[c(4, 5)], guide = "none") +
    labs(
        x = "",
        y = "Pertubation Score (SMR)",
        title = ""
    ) +
    coord_cartesian(ylim = c(0, 7)) +
    theme_bw() +
    theme(legend.position = "bottom")



# Perturbation SMR --------------------------------------------------------
### Pertubation SMR
# Data preparation
data_phen <-
    db_noncovid_years_selected_units %>%
    mutate(
        year = adm_year
    ) %>%
    mutate(
        year = case_when(
            adm_year == "2020" ~ "2020",
            TRUE ~ "2016_2019"
        )
    ) %>%
    group_by(hospital_code, year, adm_period) %>%
    summarise(
        sru = mean(sru)
    ) %>%
    ungroup() %>%
    mutate(hospital_code = paste0(hospital_code, "_", year))

# Genotype data
data_gen <-
    data_phen %>%
    select(-year) %>%
    pivot_wider(names_from = "hospital_code", values_from = "sru") %>%
    bind_cols() %>%
    select(-adm_period)

# Phenotype data (formatted)
data_phen <-
    data_phen %>%
    distinct(
        hospital_code, year
    )


## Estimating scores with default function
mdp_results <- 
    mdp(data = data_gen %>% as.data.frame(),
        pdata = data_phen %>%
            rename(
                Sample = hospital_code,
                Class = year
            ) %>% as.data.frame(),
        control_lab = "2016_2019", std = 0) # I used std = 0 so they do not remove "non-significant units"


## Obtaining data from Sample Scores
df_mdp_res_sru <- 
    mdp_results$sample_scores$allgenes %>% 
    as_tibble() %>% 
    select(Sample, Score, Class)



## Plots scores and time periods with GGhalves - Perturbation Scores
plot_half_score_sru <-
    df_mdp_res_sru %>% 
    mutate(
        Class = fct_recode(Class,
                           "Average 2016-2019" = "2016_2019"
        )
    ) %>% 
    ggplot() +
    gghalves::geom_half_violin(aes(x = Class, y = Score, fill = Class),
                               side = "l") +
    gghalves::geom_half_dotplot(aes(x = Class, y = Score, fill = Class), 
                                method = "dotdensity", binwidth = 0.15) +
    scale_fill_manual(values = ggsci::pal_lancet()(9)[c(4, 5)], guide = "none") +
    labs(
        x = "",
        y = "Pertubation Score (SRU)",
        title = ""
    ) +
    coord_cartesian(ylim = c(0, 7)) +
    theme_bw() +
    theme(legend.position = "bottom")





















### Combining time series plots and gghalf
library(patchwork)

plot_combined <-
    (plot_noncovid_years_selected_units_smr_avg + plot_noncovid_years_selected_units_sru_avg)  /
    (plot_half_score_smr + plot_half_score_sru) + plot_annotation(tag_levels = "A")


ggsave("output/Zampieri_ExtraCOVID_Figure_4_20210731.pdf", plot = plot_combined,
       width = 12, height = 8, dpi = 800)










# MDP - per bimester ------------------------------------------------------
## SMR
df_pertubation_score_bimester_smr <-
    map(1:6, function(x) {
        data_phen <-
            db_noncovid_years_selected_units %>%
            mutate(
                year = adm_year
            ) %>%
            mutate(
                year = case_when(
                    adm_year == "2020" ~ "2020",
                    TRUE ~ "2016_2019"
                )
            ) %>%
            group_by(hospital_code, year, adm_period) %>%
            summarise(
                smr = mean(smr)
            ) %>%
            ungroup() %>%
            mutate(hospital_code = paste0(hospital_code, "_", year)) %>% 
            filter(adm_period == x) 
        
        
        # Genotype data
        data_gen <-
            data_phen %>%
            select(-year) %>%
            pivot_wider(names_from = "hospital_code", values_from = "smr") %>%
            bind_cols() %>%
            select(-adm_period)
        
        # Phenotype data (formatted)
        data_phen <-
            data_phen %>%
            distinct(
                hospital_code, year
            )
        
        
        ## Estimating scores with default function
        mdp_results <- 
            mdp(data = data_gen %>% as.data.frame(),
                pdata = data_phen %>%
                    rename(
                        Sample = hospital_code,
                        Class = year
                    ) %>% as.data.frame(),
                control_lab = "2016_2019", std = 0) # I used std = 0 so they do not remove "non-significant units"
        
        
        ## Obtaining data from Sample Scores
        df_mdp_res <- 
            mdp_results$sample_scores$allgenes %>% 
            as_tibble() %>% 
            select(Sample, Score, Class) %>% 
            mutate(bimester = x)
        
        
        return(df_mdp_res)
        
    }) %>% 
    bind_rows()
    




## SRU
df_pertubation_score_bimester_sru <-
    map(1:6, function(x) {
        data_phen <-
            db_noncovid_years_selected_units %>%
            mutate(
                year = adm_year
            ) %>%
            mutate(
                year = case_when(
                    adm_year == "2020" ~ "2020",
                    TRUE ~ "2016_2019"
                )
            ) %>%
            group_by(hospital_code, year, adm_period) %>%
            summarise(
                sru = mean(sru)
            ) %>%
            ungroup() %>%
            mutate(hospital_code = paste0(hospital_code, "_", year)) %>% 
            filter(adm_period == x) 
        
        
        # Genotype data
        data_gen <-
            data_phen %>%
            select(-year) %>%
            pivot_wider(names_from = "hospital_code", values_from = "sru") %>%
            bind_cols() %>%
            select(-adm_period)
        
        # Phenotype data (formatted)
        data_phen <-
            data_phen %>%
            distinct(
                hospital_code, year
            )
        
        
        ## Estimating scores with default function
        mdp_results <- 
            mdp(data = data_gen %>% as.data.frame(),
                pdata = data_phen %>%
                    rename(
                        Sample = hospital_code,
                        Class = year
                    ) %>% as.data.frame(),
                control_lab = "2016_2019", std = 0) # I used std = 0 so they do not remove "non-significant units"
        
        
        ## Obtaining data from Sample Scores
        df_mdp_res <- 
            mdp_results$sample_scores$allgenes %>% 
            as_tibble() %>% 
            select(Sample, Score, Class) %>% 
            mutate(bimester = x)
        
        
        return(df_mdp_res)
        
    }) %>% 
    bind_rows()






## Statistical tests and plots
df_pval_comparison <-
    left_join(
        df_pertubation_score_bimester_smr %>%
            group_by(bimester) %>%
            summarise(
                p_SMR_test = if_else(wilcox.test(Score ~ Class)$p.value * 7 > 1, 1, wilcox.test(Score ~ Class)$p.value * 7)
            ),
        df_pertubation_score_bimester_sru %>%
            group_by(bimester) %>%
            summarise(
                p_SRU_test =  if_else(wilcox.test(Score ~ Class)$p.value * 7 > 1, 1, wilcox.test(Score ~ Class)$p.value * 7)
            )
    ) %>% 
    # mutate(
    #     bimester = as.character(bimester)
    # ) %>% 
    bind_rows(
        bind_cols(
            df_mdp_res_smr %>%
                # group_by(bimester) %>%
                summarise(
                    p_SMR_test =  if_else(wilcox.test(Score ~ Class)$p.value * 7 > 1, 1, wilcox.test(Score ~ Class)$p.value * 7)
                ),
            df_mdp_res_sru %>%
                # group_by(bimester) %>%
                summarise(
                    p_SRU_test = if_else(wilcox.test(Score ~ Class)$p.value * 7 > 1, 1, wilcox.test(Score ~ Class)$p.value * 7)
                )
        ) %>% 
            mutate(
                bimester = 7
            )
    ) %>% 
    mutate(
        # p_SMR_test = case_when(
        #     p_SMR_test < 0.001 ~ "P <0.001",
        #     p_SMR_test == 1 ~ "P=1.00",
        #     TRUE ~ paste0("P=", round(p_SMR_test, 3)) 
        # ),
        # p_SRU_test = case_when(
        #     p_SRU_test < 0.001 ~ "P <0.001",
        #     p_SRU_test == 1 ~ "P=1.00",
        #     TRUE ~ paste0("P=", round(p_SRU_test, 3)) 
        # )
    )


# SMR
plot_half_score_smr_bimester <-
    df_pertubation_score_bimester_smr %>% 
    bind_rows(
        df_mdp_res_smr %>% 
            mutate(
                bimester = 7
            )
    ) %>% 
    mutate(
        Class = fct_recode(Class,
                           "Average 2016-2019" = "2016_2019"
        ),
        bimester = factor(bimester)
    ) %>% 
    ggplot() +
    # geom_boxplot(aes(x = bimester, y = Score, fill = Class)) +
    gghalves::geom_half_violin(aes(x = bimester, y = Score, fill = Class),
                               side = "l") +
    gghalves::geom_half_dotplot(aes(x = bimester, y = Score, fill = Class),
                                method = "dotdensity", binwidth = 0.10, dotsize = 0.6) +
    scale_fill_manual(values = ggsci::pal_lancet()(9)[c(4, 5)], name = "") +
    scale_x_discrete(labels = c(as.character(1:6), "All")) +
    geom_label(data = df_pval_comparison, 
              aes(x = bimester, y = 7, label = p_SMR_test), size = 4) +
    labs(
        x = "Bimester",
        y = "Pertubation Score (SMR)",
        title = ""
    ) +
    coord_cartesian(ylim = c(0, 7)) +
    theme_bw() +
    theme(legend.position = "bottom")



# SRU
plot_half_score_sru_bimester <-
    df_pertubation_score_bimester_sru %>% 
    bind_rows(
        df_mdp_res_sru %>% 
            mutate(
                bimester = 7
            )
    ) %>% 
    mutate(
        Class = fct_recode(Class,
                           "Average 2016-2019" = "2016_2019"
        ),
        bimester = factor(bimester)
    ) %>% 
    ggplot() +
    # geom_boxplot(aes(x = bimester, y = Score, fill = Class)) +
    gghalves::geom_half_violin(aes(x = bimester, y = Score, fill = Class),
                               side = "l") +
    gghalves::geom_half_dotplot(aes(x = bimester, y = Score, fill = Class),
                                method = "dotdensity", binwidth = 0.1, dotsize = 0.6) +
    scale_fill_manual(values = ggsci::pal_lancet()(9)[c(4, 5)], name = "") +
    scale_x_discrete(labels = c(as.character(1:6), "All")) +
    geom_label(data = df_pval_comparison, 
               aes(x = bimester, y = 7, label = p_SRU_test), size = 4) +
    labs(
        x = "Bimester",
        y = "Pertubation Score (SRU)",
        title = ""
    ) +
    coord_cartesian(ylim = c(0, 7)) +
    theme_bw() +
    theme(legend.position = "bottom")



# plot_half_score_sru_bimester
library(patchwork)

# plot_bimester_combined <- 
#     (plot_noncovid_years_selected_units_smr_avg + plot_noncovid_years_selected_units_sru_avg) / 
#     ((plot_half_score_smr_bimester / plot_half_score_sru_bimester) +
#     plot_layout(guide = "collect") & theme(legend.position = "bottom"))

plot_bimester_combined <- 
    (plot_noncovid_years_selected_units_smr_avg +  plot_noncovid_years_selected_units_sru_avg +
         plot_half_score_smr_bimester + plot_half_score_sru_bimester) + plot_annotation(tag_levels = "A") +
         plot_layout(guide = "collect") & theme(legend.position = "bottom")



ggsave("output/Zampieri_ExtraCOVID_Figure_4_20210802.pdf", plot = plot_bimester_combined,
       width = 15.5, height = 10, dpi = 800)




