
#Package Load
library(janitor)
library(lubridate)
library(tidyverse)
library(patchwork)
library(lme4)
library(cowplot)
library(emmeans)

#Set Directory
setwd("C:/Users/fgzba/OneDrive/Manuscripts - Ongoing/ExtraCOVID/ExtraCOVID_R1/")

#Creates Database
db<-df_rdsl_icu_admissions
rm(df_rdsl_icu_admissions)
db<-clean_names(db)

#Diagnosis of special interest: Infection; Stroke; MI

#Some variables
db$unit_admission_date<-dmy(db$unit_admission_date)
db$adm_year<-year(db$unit_admission_date)
db$death<-as.factor(ifelse(db$hospital_discharge_code=="D",1,0))
db$adm_year<-as.factor(db$adm_year)
db$saps<-db$saps3points
db$saps_prob<-db$saps3death_probability_standard_equation/100
db$adm_type<-as.factor(db$admission_type_name)
db$adm_type<-as.factor(
  ifelse(db$adm_type=="Clínica","Medical",ifelse(db$adm_type=="Cirurgia Eletiva","eSurgery","uSurgery")))
db$infection<-as.factor(ifelse(db$admission_reason_name=="Infecção / sepse",1,0))
db$ecog<-as.factor(ifelse(db$chronic_health_status_name=="Independente",0,
                ifelse(db$chronic_health_status_name=="Necessidade de assistência",1,2)))
db$mv<-as.factor(ifelse(db$is_mechanical_ventilation1h=="Verdadeiro",1,0))
db$vp<-as.factor(ifelse(db$is_vasopressors1h=="Verdadeiro",1,0))
db$cci<-db$charlson_comorbidity_index

#Filter year
db<-db%>%filter(adm_year!="2021",
                adm_year!="2008",
                adm_year!="2009",
                adm_year!="2010")


#Filters minors of 18
db<-db%>%filter(age>=18)
644644-nrow(db)
(644644-nrow(db))/nrow(db)

#Selects only first admission
db$big_id<-paste(db$age,db$gender,db$medical_record,db$hospital_admission_date,db$hospital_code)
db<-db %>%
  group_by(big_id) %>%
  arrange(unit_admission_date) %>%
  filter(row_number()==1)%>%ungroup()
db<-db%>%filter(is_readmission=="Falso")
636330-nrow(db)
(636330-nrow(db))/nrow(db)

#Filtering
db<-db%>%filter(!is.na(death))
(544707-nrow(db))/nrow(db)

#Keeps df as backup
df<-db

#Filters non-covid cases
db<-db%>%filter(status_covid19=="Negativo/Sem suspeição")
535842-nrow(db)
(535842-nrow(db))/nrow(db)

#COVID-19 code = 1101711"
min(db$unit_admission_date)
max(db$unit_admission_date)
length(unique(db$unit_code))
length(unique(db$hospital_code))

#Imputation
to_imput<-db %>%
  dplyr::select(age,saps,cci,ecog,
    adm_type,infection,mv,
    vp,adm_year,death)


#Missing values plot
library(naniar)
to_imput2<-to_imput
colnames(to_imput2)<-c("Age","SAPS 3","CCI","Performance Status","Admission Type","Infection",
                       "MV", "Vasopressors","Admission Year","Death")
vis_miss(to_imput2,warn_large_data = FALSE)
gg_miss_var(to_imput2, show_pct = TRUE)
mygg_miss_var<-function(x) {gg_miss_var(x,show_pct=TRUE)+coord_flip(ylim=c(0,6))}
rmissmap<-map(to_imput2%>%group_split(`Admission Year`),mygg_miss_var)

rmissmap[[1]]+rmissmap[[2]]+rmissmap[[3]]+rmissmap[[2]]+rmissmap[[5]]+
  rmissmap[[6]]+rmissmap[[7]]+rmissmap[[8]]+rmissmap[[9]]+rmissmap[[10]]+
  plot_annotation(tag_levels = list(c("2011", "2012", "2013", "2014", "2015",
                                      "2016", "2017", "2018", "2019", "2020")))

levels(to_imput2$`Admission Year`)
ff1<-function(x){sum(is.na(x))}
mvalues<-to_imput%>%
  group_by(adm_year)%>%
  summarise(across(everything(),ff1),n=n())

library(mice)
tempData <- mice(to_imput,m=5,maxit=50,meth='pmm',seed=500)
summary(tempData)
sapply(complete(tempData,1),FUN = function(x){sum(is.na(x))})
tempData$loggedEvents
t1<-complete(tempData,1)
t1$unit_code<-db$unit_code
t1$icu_los<-db$unit_length_stay
t1 <- t1[c("age", "saps", "cci","ecog","adm_type","infection","mv","vp","icu_los","death","unit_code","adm_year")]

#Diagnostic Plots
get95<-function(x) {
  ymin=as.numeric(mean(x)-sd(x))
  ymax=as.numeric(mean(x)+sd(x))
  return(c(ymin=ymin,ymax=ymax))
}

library(gghalves)

median.quartile <- function(x){
  out <- quantile(x, probs = c(0.25,0.5,0.75))
  names(out) <- c("ymin","y","ymax")
  return(out) 
}
tagesaps<-t1%>%select(age,saps,adm_year)%>%
  pivot_longer(cols=age:saps)%>%
  ggplot(aes(x=adm_year,y=value,color=name))+
  stat_summary(fun.data = median.quartile, geom = "pointrange",
               position = position_dodge(width=0.5))+
  stat_summary(geom="line",fun=median,aes(group=name),
               position = position_dodge(width=0.5))+
  ggsci::scale_color_aaas(name="",labels=c("Age","SAPS 3"))+
  labs(x="",y="Median [IQR]")+
  coord_cartesian(ylim=c(18,100))+
  theme_half_open()%+replace%theme(axis.text.x = element_text(angle=45))

tdeath<-t1%>%
    group_by(adm_year)%>%
    summarise(mortality=mean(death==1))%>%
    ggplot(aes(x=adm_year,y=mortality))+
      geom_bar(stat="identity",fill="navyblue",color="black")+
      scale_y_continuous(labels=scales::percent)+
      theme_half_open()%+replace%theme(axis.text.x = element_text(angle=45))+
      labs(x="",y="Mortality")

tadm<-df%>%
  mutate(covid=ifelse(status_covid19=="Confirmado","Confirmed",
                      ifelse(status_covid19=="Suspeito, não confirmado","Possible","Negative")))%>%
  mutate(covid=factor(covid,levels=c("Confirmed","Possible","Negative")))%>%
  ggplot(aes(x=factor(adm_year),fill=covid))+
  geom_bar(color="black")+
  theme_half_open()%+replace%theme(axis.text.x = element_text(angle=45))+
  scale_fill_manual(values=c("darkred","red","navyblue"),name=c("COVID Status"))+
  labs(x="",y="Admissions")

tadm_type<-t1%>%
  ggplot(aes(x=adm_year,fill=adm_type))+geom_bar(position="fill",color="black")+
  theme_half_open()%+replace%theme(axis.text.x = element_text(angle=45))+
  scale_y_continuous(labels=scales::percent)+
  scale_fill_manual(values=c("darkred","navyblue","red"),
                    name=c(""),
                    labels=c("Elective Surgery","Medical","Urgent Surgery"))+
  labs(x="",y="% Admissions")

pdf("Figure_1",width=14,height=8)
(tagesaps+tdeath) / (tadm + tadm_type) + plot_annotation(tag_levels = "A")
dev.off()

#Table 1
library(gtsummary)
theme_gtsummary_journal(journal = "jama")
theme_gtsummary_compact()
table1 <- t1 %>%
          select(-unit_code)%>%
          mutate(adm_year=droplevels(adm_year),
                 ecog=fct_recode(ecog,
                                 "Absent/Minor" = "0",
                                 "Moderate" = "1",
                                 "Severe" = "2"),
                 adm_type=fct_recode(adm_type,
                                    "Elective Surgery" = "eSurgery",
                                    "Medical"="Medical",
                                    "Urgent Surgery"="uSurgery"))%>%
          tbl_summary(by = adm_year,
                      type = list(vp ~ "dichotomous",
                                  infection ~ "dichotomous",
                                  mv ~ "dichotomous",
                                  death ~"dichotomous"),
                      statistic = list(all_continuous() ~ "{mean} ({sd})",
                                       all_categorical() ~ "{n} ({p}%)",
                                       all_dichotomous()~ "{n} ({p}%)"),
                      label=list(age ~ "Age, years",
                                 saps ~ "SAPS 3, points",
                                 cci ~ "CCI, points",
                                 ecog ~ "Performance Impairment",
                                 adm_type ~ "Admission Type",
                                 infection ~ "Presence of Infection",
                                 mv ~ "Mechanical Ventilation at admission",
                                 vp ~ "Vasopressor at admission",
                                 death ~ "Hospital Mortality",
                                 icu_los ~ "ICU Length-of-stay (days)"
                                               )) %>%
          #  add_p() %>%
          add_overall() %>% 
          bold_labels()
table1
table1<-as_flex_table(table1)

#If you want to export
#library(flextable)
#setwd("/home/fernando/Desktop")
#save_as_pptx(as_flex_table(table1),path="/home/fernando/Desktop/oi.pptx")

#Checking saps linearity
t1%>%
  mutate(saps_dec=cut(saps,breaks=c(0,10,20,30,40,50,60,70,80,90,100,150)))%>%
  ggplot(aes(x=saps_dec,fill=death))+
    geom_bar(position="fill")+
    facet_wrap(~adm_year,ncol=5)+
    theme_classic()%+replace%theme(axis.text.x = element_text(angle=90))+
    scale_fill_manual(values=c("white","black"),guide=NULL)+
    geom_hline(yintercept = c(0.25,0.50,0.75),linetype=2,alpha=0.4)+
    scale_y_continuous(labels=scales::percent)+
    labs(x="",y="Mortality (%)")

#Calibration using caret
library(caret)
library(classifierplots)
t2<-t1%>%select(adm_year,death)
t2$saps_prob<-db$saps3death_probability_standard_equation/100
callme<-function(x){
  if(x=="all"){year_data=subset(t2,adm_year=!"2020")} else {year_data<-t2%>%filter(adm_year%in%x)}
  calresult<-calibration(death~saps_prob,class = "1",data=year_data)
  calresult2<-data.frame(bin=calresult$data$bin,Percent=calresult$data$Percent,
                        Lower=calresult$data$Lower,Upper=calresult$data$Upper,
                        adm_year=as.character(x))
  return(calresult2)
}
ppdata<-data.frame(x=c("[0,0.0909]","(0.909,1]"),y=c(0,100))
cplot<-lapply(list("all",2011,2018,2019,2020),callme)%>%bind_rows()%>%
  mutate(adm_year=as.factor(adm_year))%>%
  ggplot(aes(x=bin,y=Percent,group=adm_year,color=adm_year))+
    geom_path(size=1)+
#    geom_errorbar(aes(ymin=Lower,ymax=Upper),width=0)+
    scale_x_discrete(labels=1:11)+
    geom_line(inherit.aes = FALSE,data=ppdata,
              aes(x=x,y=y,group=1),linetype=2)+
  ggsci::scale_color_jama(name="Year",labels=c("2011","2018","2019","2020","2011-2020"))+
  labs(x="Bins",y="Percentage (%)")+
  theme_minimal_grid()%+replace%theme(axis.text.x = element_text(size=8),
                                      axis.title.x = element_text(size=12,face = "bold"),
                                      axis.text.y = element_text(size=8))

#Using classifierplots
library(classifierplots)
callme<-function(i){  calibration_plot(db$death[db$adm_year==i], db$saps_prob[db$adm_year==i])+
    theme_minimal()+
    labs(subtitle = i)}
cal_plots<-lapply(2011:2020,callme)
library(cowplot)
plot_grid(cal_plots[[1]],cal_plots[[2]],
          cal_plots[[3]],cal_plots[[4]],
          cal_plots[[5]],cal_plots[[6]],
          cal_plots[[7]],cal_plots[[8]],
          cal_plots[[9]],cal_plots[[10]],ncol=5)

#Final analysis
m1<-glmer(death ~ saps*adm_year + (1|unit_code),
            family="binomial",data=t1,nAGQ = 0)
m2<-glmer(death ~ saps + adm_year + (adm_year|unit_code),
          family="binomial",data=t1,nAGQ = 0,
          control=glmerControl(optCtrl=list(maxfun=1e6)))
m3<-glmer(death ~ saps*adm_year + (adm_year|unit_code),
          family="binomial",data=t1,nAGQ = 0,
          control=glmerControl(optCtrl=list(maxfun=1e6)))
summary(m3)
anova(m1,m3)

#tt<-expand_grid(saps=30:100,adm_year=levels(t1$adm_year))
#library(sjPlot)
library(ggeffects)

ggp_random <- ggpredict(m3, terms = c("saps [30:100]","adm_year"),
                        interval="confidence")

fig2B<-ggp_random%>%
  filter(group%in%c("2011","2018","2019","2020"))%>%
  ggplot(aes(x, predicted, colour = group)) + geom_line(size=1)+
  scale_y_continuous(labels=scales::percent)+
  ggsci::scale_color_jama(name="Year")+
  labs(x="SAPS 3",y="Predicted Mortality")+
  theme_minimal_grid()%+replace%theme(axis.text.x = element_text(size=8),
                                      axis.title.x = element_text(size=12,face = "bold"),
                                      axis.text.y = element_text(size=8))


library(emmeans)
pp<-lsmeans(m3, pairwise ~ adm_year | saps, type="response",adjust="tukey",at=list(saps=c(20,30,40,50,60,70,80,90,100)))  
pp2<-emmeans(m3, pairwise~adm_year|saps,type="response",adjust="bonferroni",
             at=list(saps=c(20,30,40,50,60,70,80,90,100)))

orplot<-as.data.frame(print(pp$contrasts))
orplot<-orplot%>%filter(str_detect(contrast, "/ 2020"))%>%select(-df,-z.ratio)

orplot$odds.ratio<- -log(orplot$odds.ratio)
orplot$lower<-exp(orplot$odds.ratio-1.96*orplot$SE)
orplot$upper<-exp(orplot$odds.ratio+1.96*orplot$SE)
orplot$odds.ratio<-exp(orplot$odds.ratio)

orplot%>%
  select(saps,odds.ratio,lower,upper,p.value)%>%
  mutate_all(.funs = function(x){round(as.numeric(x),3)})%>%
  mutate(odds.ratio2=paste0(odds.ratio," [",lower,"-",upper,"]"))%>%
  select(-odds.ratio)%>%
  select(saps,odds.ratio2,p.value)%>%
  writexl::write_xlsx(path = "ordata.xlsx")

fig4<-orplot%>%ggplot(aes(x=contrast,y=odds.ratio))+
  geom_point()+
  geom_errorbar(aes(ymin=lower,ymax=upper),width=0.2)+
  scale_y_continuous(breaks=seq(0.2,2,by=0.2))+
  scale_x_discrete(labels=2011:2019)+
  coord_cartesian(ylim = c(0.2,2))+
  geom_hline(yintercept = 1,linetype=2)+
  facet_wrap(~saps)+
  cowplot::theme_cowplot()%+replace%theme(axis.text.x = element_text(angle=45,size=8),
                                          axis.text.y = element_text(size=8),
                                          axis.title.x = element_text(size=12),
                                          axis.title.y = element_text(size=12,angle=90))+
  labs(x="Comparison Year",y="OR 95% CI")

pdf("Figure_4.pdf",width = 10,height = 10)
fig4
dev.off()
#Subgroups
table(t1$adm_type)
m3.surg<-glmer(death ~ saps*adm_year + (adm_year|unit_code),
          family="binomial",data=subset(t1,adm_type=="eSurgery"),nAGQ = 0,
          control=glmerControl(optCtrl=list(maxfun=1e6)))
m3.med<-glmer(death ~ saps*adm_year + (adm_year|unit_code),
               family="binomial",data=subset(t1,adm_type=="Medical"),nAGQ = 0,
               control=glmerControl(optCtrl=list(maxfun=1e6)))

m3.ecog0<-glmer(death ~ saps*adm_year + (adm_year|unit_code),
              family="binomial",data=subset(t1,ecog=="0"),nAGQ = 0,
              control=glmerControl(optCtrl=list(maxfun=1e6)))
m3.ecog1<-glmer(death ~ saps*adm_year + (adm_year|unit_code),
                family="binomial",data=subset(t1,ecog=="1"),nAGQ = 0,
                control=glmerControl(optCtrl=list(maxfun=1e6)))
m3.ecog2<-glmer(death ~ saps*adm_year + (adm_year|unit_code),
                family="binomial",data=subset(t1,ecog=="2"),nAGQ = 0,
                control=glmerControl(optCtrl=list(maxfun=1e6)))
m3.infection<-glmer(death ~ saps*adm_year + (adm_year|unit_code),
                family="binomial",data=subset(t1,infection=="1"),nAGQ = 0,
                control=glmerControl(optCtrl=list(maxfun=1e6)))
m3.mv<-glmer(death ~ saps*adm_year + (adm_year|unit_code),
                    family="binomial",data=subset(t1,mv=="1"),nAGQ = 0,
                    control=glmerControl(optCtrl=list(maxfun=1e6)))


orfig<-function(m=m3){
  pp<-lsmeans(m, pairwise ~ adm_year | saps, type="response",at=list(saps=c(20,30,40,50,60,70,80,90,100)))  
  orplot<-as.data.frame(print(pp$contrasts))
  orplot<-orplot%>%filter(str_detect(contrast, "/ 2020"))%>%select(-df,-z.ratio)
  
  orplot$odds.ratio<- -log(orplot$odds.ratio)
  orplot$lower<-exp(orplot$odds.ratio-1.96*orplot$SE)
  orplot$upper<-exp(orplot$odds.ratio+1.96*orplot$SE)
  orplot$odds.ratio<-exp(orplot$odds.ratio)
  
  figx<-orplot%>%ggplot(aes(x=contrast,y=odds.ratio))+
    geom_point()+
    geom_errorbar(aes(ymin=lower,ymax=upper),width=0.2)+
#    scale_y_log10(breaks=seq(0.1,1.5,by=0.1))+
    scale_x_discrete(labels=2011:2019)+
    coord_cartesian(ylim = c(0.2,2))+
    geom_hline(yintercept = 1,linetype=2)+
    facet_wrap(~saps)+
    cowplot::theme_cowplot()%+replace%theme(axis.text.x = element_text(angle=45,size=8),
                                            axis.text.y = element_text(size=6),
                                            axis.title.x = element_text(size=12),
                                            axis.title.y = element_text(size=12,angle=90))+
    labs(x="Comparison Year",y="OR 95% CI")
  
  return(figx)
}


orfig(m3.mv)

#VLAD
db$status_covid19
db$saps
db$death
vlad<-df%>%
  filter(year(unit_admission_date)=="2020")%>%
  mutate(psaps=saps3death_probability_standard_equation,death,pweek=week(unit_admission_date))%>%
  select(death,psaps,pweek,status_covid19)%>%
  mutate(psaps=psaps/100,
         covid=fct_recode(status_covid19,
                          "COVID" = "Confirmado",
                          "COVID" = "Suspeito, não confirmado",
                          "Not_COVID" = "Negativo/Sem suspeição"))%>%
  select(-status_covid19)
vlad$points[vlad$death==0]<-vlad$psaps[vlad$death==0]
vlad$points[vlad$death==1]<-(1-vlad$psaps[vlad$death==1])*-1

pvladA<-vlad%>%
  filter(covid!="COVID")%>%
  group_by(pweek)%>%
  summarise(points=sum(points))%>%
  ggplot(aes(x=factor(pweek),y=points,group=1))+
    geom_point()+
    geom_line()+
    geom_smooth(method="gam",color="black",linetype=3)+
    labs(y="VLAD for Non-COVID Cases")+
    theme_minimal_grid()%+replace%theme(axis.title.x = element_blank(),
                                        axis.text.x = element_blank(),
                                        axis.text.y = element_text(size=8),
                                        axis.title.y = element_text(size=12,face = "bold",angle=90))

pvladB<-vlad%>%
  filter(covid=="COVID")%>%
  group_by(pweek)%>%
  summarise(n=n())%>%
  bind_rows(data.frame(pweek=1:7,n=0),.)%>%
  ggplot(aes(x=factor(pweek),y=n,group=1))+
    geom_point()+
    geom_line()+
    geom_smooth(method="gam",color="black",linetype=3)+
    labs(x="Epidemiological Week in 2020",y="COVID Admissions (n)")+
    theme_minimal_grid()%+replace%theme(axis.text.x = element_text(size=8),
                                        axis.title.x = element_text(size=12,face = "bold"),
                                        axis.text.y = element_text(size=8),
                                        axis.title.y = element_text(size=12,face = "bold",angle=90))



library(patchwork)
getwd()
pdf("Figure_2.pdf",width=12,height = 10)
(pvladA/pvladB) + plot_annotation(tag_levels = 'A')# +  plot_layout(heights = c(1, 2))
dev.off()
