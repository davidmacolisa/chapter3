# Education Production Functions
# RoBMA PLUS
# 15 March 2021
# Davidmac O. Ekeocha
# PhD Thesis

# Loading Dataset and Packages----
rm(list = ls())
read.csv('C:/Users/david/OneDrive/Documents/ULMS/PhD/PhD_Thesis/Thesis/Experiments/MetaEPDev/eilo.csv')->eilo
as.numeric(eilo$SeEE)->eilo$SeEE
sum(is.na(eilo))


# install.packages('fastmap',
#                  'utf8',
#                  'pkgload',
#                  'Imports BayesTools (>= 0.1.3)',
#                  'runjags',
#                  'bridgesampling',
#                  'rjags',
#                  'stringi',
#                  'BayesFactor',
#                  'testthat',
#                  'vdiffr',
#                  'psych',
#                  # 'stats',
#                  # 'graphics',
#                  'extraDistr',
#                  'scales',
#                  'callr',
#                  'Rdpack',
#                  'RoBMA',
#                  'metafor',
#                  'ggplot2',
#                  devtools::install_github("FBartos/BayesTools") #fixed the initial error
# 
# install.packages(c("cluster", "MASS", "Matrix", "mgcv", "nlme", "survival"), lib="C:/R/R-4.1.3/library")

# write.csv(eiloEN_EA, file = 'EO.csv')
# write.csv(eiloEN_EA_SAttain, file = 'sttain.csv')
# write.csv(eiloEN_EA_SchEnrol, file = 'senrol.csv')
# write.csv(eiloEN_EA_SAttend, file = 'sttend.csv')
# write.csv(eiloEN_EA_SHours, file = 'shours.csv')
# write.csv(eiloEN_EA_TS, file = 'ts.csv')
# write.csv(eiloEN_EA_YearSch, file = 'year.csv')
# write.csv(eiloEN_EA_LR, file = 'lr.csv')
# write.csv(eiloEN_EA_EExp, file = 'sttain.csv')

# devtools::install_github("fbartos/RoBMA")
# install.packages('rhub')
# rhub::check(platform="windows-x86_64-devel",env_vars=c(R_COMPILE_AND_INSTALL_PACKAGES = "always"))
#devtools::check_rhub()


library(runjags)
library(RoBMA)
library(robumeta)
library(dplyr)
library(weightr)
library(rjags)
library(stringi)
library(bridgesampling)
library(BayesFactor)
library(testthat)
library(vdiffr)
library(covr)
library(knitr)
library(rstan)
library(rmarkdown)
library(coda)
library(scales)
library(psych)
library(stats)
library(graphics)
library(extraDistr)
library(callr)
library(Rdpack)
library(ggplot2)


# Filtering Outcomes by OutcomeVariable ##-----
unique(eilo$OutcomeVariable)

eilo %>%
  filter(OutcomeVariable != 'School Attainment') %>%
  filter(OutcomeVariable != 'Learning Outcomes') %>%
  filter(OutcomeVariable != 'Literacy Rate') %>%
  filter(OutcomeVariable != 'Education Expenditure') %>%
  filter(OutcomeVariable != 'School Attendance') %>%
  filter(OutcomeVariable != 'Study Hours (min/day)') %>%
  filter(OutcomeVariable != 'Student Absenteeism') %>%
  filter(OutcomeVariable != 'Unenrolled Children') %>%
  filter(OutcomeVariable != 'School Enrollment') %>%
  filter(OutcomeVariable != 'Test Scores') %>%
  filter(OutcomeVariable != 'U5C Mortality') %>%
  filter(OutcomeVariable != 'Fertility') %>%
  filter(OutcomeVariable != 'Malnutrition') %>%
  filter(OutcomeVariable != 'Maternal Mortality') %>%
  filter(OutcomeVariable != 'Infant Mortality') %>%
  filter(OutcomeVariable != 'U5C Mortality') %>%
  filter(OutcomeVariable != 'Fertility') %>%
  filter(OutcomeVariable != 'Adult Mortality') %>%
  filter(OutcomeVariable != 'Morbidity') %>%
  filter(OutcomeVariable != 'Maternal Mortality') %>%
  filter(OutcomeVariable != 'Good Health') %>%
  filter(OutcomeVariable != 'Depression') %>%
  filter(OutcomeVariable != 'Malnutrition') %>%
  filter(OutcomeVariable != 'Life Expectancy') %>%
  filter(OutcomeVariable != 'Unenrolled Children') %>%
  filter(OutcomeVariable != 'Cleaning (min/day)') %>%
  filter(OutcomeVariable != 'Food Processing (min/day)') %>%
  filter(OutcomeVariable != 'Water Collection (min/day)') %>%
  filter(EECat != 'Electricity Reliability') %>%
  filter(EECat != 'Energy Use') %>%
  filter(Setting != 'Urban')->eiloPP
unique(eiloPP$OutcomeVariable)

str(eiloPP)
sum(is.na(eiloPP))

# Data Management----
# as.numeric(eiloPP$SeEE)->eiloPP$SeEE
# str(eiloPP)
sum(is.na(eiloPP))

# Calculating SEs for NA without SDs
# https://www.youtube.com/watch?v=NpRPalNazu0
# https://stackoverflow.com/questions/8161836/how-do-i-replace-na-values-with-zeros-in-an-r-dataframe

# eiloPP$SeEE[is.na(eiloPP$SeEE)]<-0 # replacing NAs with zeros
SD = sqrt(1/2*(1-1/2)) # assuming 1/2 chance of the having significant beta coefficient
eiloPP$SeEE[7:9] = SD/sqrt(eiloPP$N[7:9])->eiloPP$SeEE[7:9]
eiloPP$SeEE[16] = SD/sqrt(eiloPP$N[16])->eiloPP$SeEE[16]
eiloPP$SeEE[18:21] = SD/sqrt(eiloPP$N[18:21])->eiloPP$SeEE[18:21]
eiloPP$SeEE[25] = SD/sqrt(eiloPP$N[25])->eiloPP$SeEE[25]
eiloPP$SeEE[29] = SD/sqrt(eiloPP$N[29])->eiloPP$SeEE[29]
eiloPP$SeEE[62] = SD/sqrt(eiloPP$N[62])->eiloPP$SeEE[62]
eiloPP$SeEE[85:86] = SD/sqrt(eiloPP$N[85:86])->eiloPP$SeEE[85:86]
eiloPP$SeEE[90] = SD/sqrt(eiloPP$N[90])->eiloPP$SeEE[90]
eiloPP$SeEE[94] = SD/sqrt(eiloPP$N[94])->eiloPP$SeEE[94]
eiloPP$SeEE[98] = SD/sqrt(eiloPP$N[98])->eiloPP$SeEE[98]
eiloPP$SeEE[102] = SD/sqrt(eiloPP$N[102])->eiloPP$SeEE[102]
eiloPP$SeEE[106] = SD/sqrt(eiloPP$N[106])->eiloPP$SeEE[106]
eiloPP$SeEE[110] = SD/sqrt(eiloPP$N[110])->eiloPP$SeEE[110]
eiloPP$SeEE[114] = SD/sqrt(eiloPP$N[114])->eiloPP$SeEE[114]
eiloPP$SeEE[283:294] = SD/sqrt(eiloPP$N[283:294])->eiloPP$SeEE[283:294]
eiloPP$SeEE[371:372] = SD/sqrt(eiloPP$N[371:372])->eiloPP$SeEE[371:372]
eiloPP$SeEE[381:392] = SD/sqrt(eiloPP$N[381:392])->eiloPP$SeEE[381:392]
eiloPP$SeEE[453:454] = SD/sqrt(eiloPP$N[468:469])->eiloPP$SeEE[468:469]
eiloPP$SeEE[509] = SD/sqrt(eiloPP$N[509])->eiloPP$SeEE[509]

sum(is.na(eiloPP))

# Cohen's D and SeD
eiloPP %>% mutate(
  t_stat = EE/SeEE,
  sd = (sqrt(N)*SeEE)*(1/2),
  cohenD = EE/sd,
  SeD = (sqrt(4/N + sd^2/(2*N))) #Standard error of cohen's D
)->eiloPP

# eiloPP %>% mutate(
#   t_stat = EE/SeEE,
#   sd = (sqrt(S_Sample)*SeEE)*(1/2),
#   cohenD = EE/sd,
#   SeD = (sqrt(4/S_Sample + sd^2/(2*S_Sample))) #Se of cohen's D
# )->eiloPP

str(eiloPP)

sum(is.na(eiloPP))

# eilo %>% select(Authors:nCitations)->eiloPP
str(eiloPP)
# eilo %>% mutate(SiD=1:634)->eilo
sum(is.na(eiloPP))

# select(eiloEN, Authors, GenYN, Design, CIssues, Country, JournalName, Q1)->Details
# write.csv(Details, file = 'details.csv')
# write.csv(StudyHours, file = 'study.rds')
# as.character(eilo$Authors)->eilo$Authors
# as.numeric(eilo$SeEE)->eilo$SeEE
# str(eilo)

# Converting EE to cohen's d and Getting Cohen's d Se.D
# eilo %>% mutate(
#   t_stat = EE/SeEE,
#   sd = (sqrt(N)*SeEE)*(1/2),
#   cohenD = EE/sd,
#   SeD = (sqrt(4/N + sd^2/(2*N))) #Se of cohen's D
# )->eilo


# Visualisations-----
EECat_OV<-ggplot(data=eiloPP, aes(x=EECat, fill=OutcomeVariable)) + 
  geom_bar(position = 'dodge', col = 'white') + 
  labs(title = 'Fig.1 Effect sizes and Poverty & Time Use',
       x= 'Types of Modern Energy Interventions',
       y='Count of beta coefficients') + 
  theme_bw()
EECat_OV

Authors_EECat<-ggplot(data=eiloPP, aes(x=EECat, fill=Authors)) + 
  geom_bar(position = 'stack', col = 'white') + 
  labs(title = 'Fig.2 Effect sizes by Authors',
       x= 'Types of Modern Energy Interventions',
       y='Count of beta coefficients') + 
  theme_bw()
Authors_EECat

Country_EECat<-ggplot(data=eiloPP, aes(x=EECat, fill=Country)) + 
  geom_bar(position = 'dodge', col = 'white') + 
  labs(title = 'Fig.3 Effect sizes by Country',
       x= 'Types of Modern Energy Interventions',
       y='Count of beta coefficients') + 
  theme_bw()
Country_EECat

Region_EECat<-ggplot(data=eiloPP, aes(x=Region, fill=OutcomeVariable)) + 
  geom_bar(position = 'dodge', col = 'white') + 
  labs(title = 'Fig.4 Effect sizes by Regions',
       x= 'Global South Regions',
       y='Count of beta coefficients') + 
  theme_bw()
Region_EECat

Design_EECat<-ggplot(data=eiloPP, aes(x=EECat, fill=Design)) + 
  geom_bar(position = 'dodge', col = 'white') + 
  labs(title = 'Fig.5 Effect sizes by Study Design',
       x= 'Types of Modern Energy Interventions',
       y='Count of beta coefficients') + 
  theme_bw()
Design_EECat

Method_EECat<-ggplot(data=eiloPP, aes(x=EECat, fill=Method)) + 
  geom_bar(position = 'stack', col = 'white') + 
  labs(title = 'Fig.6 Effect sizes by Method',
       x= 'Types of Modern Energy Interventions',
       y='Count of beta coefficients') + 
  theme_bw()
Method_EECat

EEvsSeEE = ggplot(data = eiloPP, aes(y=SeEE,x=EE, col=OutcomeVariable)) + geom_point() + 
  facet_grid(~OutcomeVariable) + xlab("Effect Sizes") + ylab("Standard Errors") +
  ggtitle("Effect Sizes vs Standard Errors") + theme_bw()
EEvsSeEE


library(ggpubr) #COMIBINING MULTIPLE GRAPHS -- http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/81-ggplot2-easy-way-to-mix-multiple-graphs-on-the-same-page/

ggarrange(EECat_OV, Authors_EECat, labels = c('A','B'), ncol=1, nrow=2)

ggarrange(Country_EECat, Region_EECat, Design_EECat, Method_EECat, labels = c('C','D','E','F'), ncol=2, nrow=2)


# Electricity Access Interventions-----
unique(eiloPP$EECat)
eiloPP %>% subset(EECat=='Electricity Access')->PovEA

# unique(eiloPP$Authors)
# unique(eiloPP$OutcomeVariable)
# unique(PovEA$OutcomeVariable)

PovEA %>% filter(OutcomeVariable=='Fuel Collection Time (min/day)')->PovEA_FuelCollect
PovEA %>% filter(OutcomeVariable=='Cooking Time (min/day)')->PovEA_CookingTime
PovEA %>% filter(OutcomeVariable=='Paid Work Hours (min/day)')->PovEA_PaidWHours
PovEA %>% filter(OutcomeVariable=='Time Savings (min/day)')->PovEA_TimeSavings
PovEA %>% filter(OutcomeVariable=='Poverty Headcount')->PovEA_PovertyHeadCount
PovEA %>% filter(OutcomeVariable=='Household Income')->PovEA_HHIncome
PovEA %>% filter(OutcomeVariable=='Household Consumption Expenditure')->PovEA_HHConsumeExp
PovEA %>% filter(OutcomeVariable=='Household Consumption Per Capita')->PovEA_HHConsumePP
sum(is.na(PovEA_HHConsumeExp))

# Solar Interventions-----
eiloPP %>% subset(EECat=='Solar PV')->PovSolar

# ICS Interventions----
eiloPP %>% subset(EECat=='ICS')->PovICS
unique(PovICS$OutcomeVariable)
PovICS %>% filter(OutcomeVariable == 'Fuel Collection Time (min/day)')->PovICS_FuelColl
PovICS %>% filter(OutcomeVariable == 'Fuel Preparation Time (min/day)')->PovICS_FuelPrep
PovICS %>% filter(OutcomeVariable == 'Cooking Time (min/day)')->PovICS_CookTime
PovICS %>% filter(OutcomeVariable == 'Water Collection (min/day)')->PovICS_WaterCol
PovICS %>%  filter(OutcomeVariable == 'Cleaning (min/day)')->PovICS_Cleaning
PovICS %>% filter(OutcomeVariable == 'Food Processing (min/day)')->PovICS_FoodProcess
PovICS %>% filter(OutcomeVariable == 'Other Kitchen Activities (min/day)')->PovICS_OtherKitA
PovICS %>% filter(OutcomeVariable == 'Time Savings (min/day)')->PovICS_TimeSavings
PovICS %>% filter(OutcomeVariable == 'Poverty Headcount')->PovICS_PovertyHeadCount
# TVR Interventions------
eiloPP %>% subset(EECat!='Electricity Access') %>%
  subset(EECat!='Solar PV') %>%
  subset(EECat!='ICS')->PovTVR


#### RoBMA of Electricity Access on Infant Mortality -- Found weak against the effect (-ve) and no pub. bias------
# RoBMA ENsemble -- priors for effect size, heterogeneity and publication bias
# eilo_InfMort$EE/100->eilo_InfMort$EE
# write.csv(eilo_InfMort, file = 'inf.csv')
# as.character(eilo_InfMort$Authors)->eilo_InfMort$Authors

check_setup(
  model_type = 'PSMA',
  priors_effect = prior(distribution = "normal", parameters = list(mean = 0, sd = 1)),
  priors_heterogeneity = prior(distribution = "invgamma", parameters = list(shape = 1, scale = 0.15)),
  priors_bias = list(prior_weightfunction(distribution = "two.sided", parameters = list(alpha = c(1, 1), steps = c(0.05)), prior_weights = 1/12),
                     prior_weightfunction(distribution = "two.sided", parameters = list(alpha = c(1, 1, 1), steps = c(0.05, 0.1)), prior_weights = 1/12),
                     prior_weightfunction(distribution = "one.sided", parameters = list(alpha = c(1, 1), steps = c(0.05)), prior_weights = 1/12),
                     prior_weightfunction(distribution = "one.sided", parameters = list(alpha = c(1, 1, 1), steps = c(0.025, 0.05)), prior_weights = 1/12),
                     prior_weightfunction(distribution = "one.sided", parameters = list(alpha = c(1, 1, 1), steps = c(0.05, 0.5)), prior_weights = 1/12),
                     prior_weightfunction(distribution = "one.sided", parameters = list(alpha = c(1, 1, 1, 1), steps = c(0.025, 0.05, 0.5)), prior_weights = 1/12),
                     prior_PET(distribution = "Cauchy", parameters = list(0, 1), truncation = list(0, Inf), prior_weights = 1/4),
                     prior_PEESE(distribution = "Cauchy", parameters = list(0, 5), truncation = list(0, Inf), prior_weights = 1/4)),
  priors_effect_null = prior(distribution = "point", parameters = list(location = 0)),
  priors_heterogeneity_null = prior(distribution = "point", parameters = list(location = 0)),
  priors_bias_null = prior_none(),
  models = F,
  silent = F
)

# Combining the effect sizes and transforming to fishers_Z. 
# combine_data(ri = eilo_Solar$EE,
#               ni = eilo_Solar$Sample,
#               study_names = 'Authors',
#               data = eilo_Solar,
#               transformation = 'fishers_z',
#              return_all = F)->eilo_Solar

# escalc(measure="ZCOR",
#        ri = EE,
#        ni = Sample,
#        data = eilo_InfMort,
#        slab(eilo_Solar$Authors, sep = (', ')))->eilo_InfMort


#Fitting the model
RoBMA(d = PovEA_HHConsumePP$cohenD,
      n = PovEA_HHConsumePP$N,
      seed = 1,
      model = 'PSMA',
      study_names = PovEA_HHConsumePP$Authors,
      parallel = T,
      transformation = 'fishers_z'
)->RoBMAPovEA_HHConsumePP

summary(RoBMAPovEA_HHConsumePP, output_scale='r', conditional = T)
# RoBMA::check_RoBMA(RoBMAeiloHH_EA_IM)
interpret(RoBMAPovEA_HHConsumePP, output_scale='r')

# summary(RoBMAeilo_InfMort, parameter='omega')
# summary(RoBMAeilo_InfMort, parameter='omega', output_scale='r') #pearson's r
summary(RoBMAeilo_InfMort, type='diagnostics') #pearson's r
summary(RoBMAeilo_InfMort, type='model', diagnostics = T) #pearson's r

diagnostics(
  RoBMAeilo_InfMort,
  parameter = 'mu',
  output_scale='r',
  type = 'chains',
  plot_type = "ggplot", #'base'
)

diagnostics(
  RoBMAeilo_InfMort,
  parameter = 'mu',
  output_scale='r',
  type = 'autocorrelation',
  plot_type = "ggplot",
  lags = 30,
)

diagnostics(
  RoBMAeilo_InfMort,
  parameter = 'mu',
  output_scale='r',
  type = 'densities',
  plot_type = "ggplot",
)

# RoBMA::forest(RoBMAhealth_eilo)
RoBMA::forest(RoBMAeilo_InfMort, plot_type = 'ggplot')
RoBMA::forest(RoBMAeilo_InfMort, conditional = T, plot_type = 'ggplot')


plot(RoBMAeilo_InfMort, parameter = "mu", prior = T, plot_type = 'ggplot')
plot(RoBMAeilo_InfMort, parameter = "tau", prior = T, plot_type = 'ggplot')
plot(RoBMAeilo_InfMort, parameter = "weightfunction", xlim = c(0, 0.05), plot_type = 'ggplot')
# plot(RoBMAeilo_InfMort, parameter = "weightfunction", output_scale = 'r', xlim = c(0, 0.05), rescale_x = T, plot_type = 'ggplot')
plot(RoBMAeilo_InfMort, parameter = "PET-PEESE", prior = T, plot_type = 'ggplot')

plot_models(RoBMAeilo_InfMort, parameter = "mu", xlim = c(-.1, 0.1), plot_type = 'ggplot')
print(RoBMAeilo_InfMort)

