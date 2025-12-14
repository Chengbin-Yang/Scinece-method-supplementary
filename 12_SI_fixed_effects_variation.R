# Master Runs
library(tidyverse)
library(devtools)
library(gets)
library(getspanel)
library(here)
library(doParallel)
library(readxl)
library(gdata)
library(countrycode)

source('00_oecd_project_functions.R')

##runs the main model with alternative fixed-effects specifications (see SI section 7.3)

# Country groupings
setwd("F:\\Desktop\\科研项目\\1.负责科研项目\\Climate Policy DID Science-厦大_北师大\\Science")

##load country groups 


#Developed economies = AC1
#Developing economies = AC6

AC6 <- read_excel("country_groupings.xlsx", sheet = 2)

AC1 <- read_excel("country_groupings.xlsx") %>% pull(Developed) %>% unlist
for(i in 1:nrow(AC6)){
  temp <- AC6 %>% slice(i) %>% pull(countries) %>% strsplit(., ", ") %>% unlist
  mv(from = "temp", to = paste0("AC6_", AC6$cat_abbrev[i]))
}

dfi <- readRDS("break_detection_regression_input.RDS")  

#exclude countries that don't have a minimum number of emissions 
excl_all <- dfi %>% group_by(country) %>% 
  summarise(excl_test = mean(log_total_emissions_co2)) %>% filter(excl_test < 10) %>% pull(country) %>% unique

df_excl <- dfi %>% filter(!(country %in% excl_all))

AC6 <- df_excl %>% filter(!(country %in% AC1)) %>% select(country)%>% distinct()
AC6 <- AC6$country
samples <- mget(c("AC1", "AC6"))


#specifcy basic model forms

tot_controls <- c(" ~ lgdp + lpop + lgdp_sq + hdd + cdd")

tot_core_deps <- c("log_buildings_co2",
                   "log_electricity_heat_co2", 
                   "log_industry_co2",
                   "log_transport_co2")
                  

tot_base_forms <- paste0(rep(tot_core_deps, each = length(tot_controls)), tot_controls)


# specify sector-specific controls (including EU dummies as we're looking at developed economies, all EU countries are in this group)
total_controls <- " ~ lgdp + lpop + lgdp_sq + hdd + cdd + Labels_Appliances_2001 + Labels_Appliances_2013 + MEPS_Appliances_2009 + MEPS_Appliances_2013 + ETS_E_2005 + ETS_E_2018 + ETS_I_2005 + ETS_I_2018 + MEPS_T_2009 + MEPS_ElectricMotors_2011 + MEPS_ElectricMotors_2015"

buildings_controls <- " ~ lgdp + lpop + lgdp_sq + hdd + cdd + Labels_Appliances_2001 + Labels_Appliances_2013 + MEPS_Appliances_2009 + MEPS_Appliances_2013"

electricity_controls <- " ~ lgdp + lpop + lgdp_sq + hdd + cdd + ETS_E_2005 + ETS_E_2018"

industry_controls <- " ~ lgdp + lpop + lgdp_sq + hdd + cdd + ETS_I_2005 + ETS_I_2018 + MEPS_ElectricMotors_2011 + MEPS_ElectricMotors_2015"

transport_controls <- " ~ lgdp + lpop + lgdp_sq + hdd + cdd + MEPS_T_2009"


eu_control_forms <- c(
  paste0("log_buildings_co2", buildings_controls),
  paste0("log_electricity_heat_co2", electricity_controls), 
  paste0("log_industry_co2", industry_controls),
  paste0("log_transport_co2", transport_controls))

#create an ensemble of models to run

tot_forms <- c(tot_base_forms, eu_control_forms)

# Incorporating linear country-specific time trends
df_trends <- dfi %>% mutate(country = as.factor(country),
                             trend = year - 1999,
              )

df_restr_trends <- df_excl %>% mutate(country = as.factor(country),
                                      trend = year - 1999,
                                      trend_sq = trend^2)



### robustness check: Add continent FE 

## this assignment automatically creates an "Americas" groups which will place Mexico with South-American countries in AC6 and the USA and Canada together in AC1
df_restr_trends_continent <- df_restr_trends %>% mutate(continent = countrycode(sourcevar = country,origin = "country.name",destination = "continent"))
df_restr_trends_continent$continent[df_restr_trends_continent$country == 'China'] = ''


df_restr_trends_continent$continent = as.factor(df_restr_trends_continent$continent)
df_restr_trends_continent$year_fact = as.factor(df_restr_trends_continent$year)

##create separate continent dummies 
df_restr_trends_continent = df_restr_trends_continent %>% mutate(var = 1) %>% spread(continent, var, fill = 0, sep = "_") %>% left_join(df_restr_trends_continent)


# Base forms
tot_forms_trends <- c(paste0(tot_forms, " + country:trend"))

tot_forms_trends_continent_AR1 <- c(paste0(tot_forms, " + country:trend + continent_Europe:year_fact+continent_Americas:year_fact+continent_Oceania:year_fact"))

tot_forms_trends_continent_AR6 <- c(paste0(tot_forms, " + country:trend  + continent_Asia:year_fact+continent_Americas:year_fact"))

##run models for developing economies
rel_forms_trends <- tot_forms_trends[1:4]

rel_forms_trends_continent <- tot_forms_trends_continent_AR6[1:4]

#run models with getspanel

results_developing <- foreach(f = rel_forms_trends, .combine = rbind, .packages = c('tidyverse', 'getspanel')) %:%
  foreach(ii = c(FALSE), .combine = rbind) %:%
  foreach(smpl = c("AC6"), .combine = rbind) %:%
  foreach(p.value = c(0.01), .combine = rbind) %dopar% {
    dat <- df_restr_trends %>% filter(country %in% samples[[smpl]])
    is <- isatpanel(
      data = dat,
      formula = as.formula(f),
      index = c("country", "year"),
      effect = "twoways",
      iis = ii,
      fesis = TRUE,
      ar = 0,
      t.pval = p.value,
      max.block.size = 20
    )
    models = tibble(source = f, 
                    country_sample = smpl, 
                    year_range = paste0(min(dat$year),":",max(dat$year)), 
                    p_val = p.value, 
                    is = list(is),
                    iis = ii,
                    b_size = 20,
                    ar = 0)
  }

saveRDS(results_developing,"Results_developing.RDS")


results_developing_continent <- foreach(f = rel_forms_trends_continent, .combine = rbind, .packages = c('tidyverse', 'getspanel')) %:%
  foreach(ii = c(FALSE), .combine = rbind) %:%
  foreach(smpl = c("AC6"), .combine = rbind) %:%
  foreach(p.value = c(0.01), .combine = rbind) %dopar% {
    dat <- df_restr_trends_continent %>% filter(country %in% samples[[smpl]])
    is <- isatpanel(
      data = dat,
      formula = as.formula(f),
      index = c("country", "year"),
      effect = "twoways",
      iis = ii,
      fesis = TRUE,
      ar = 0,
      t.pval = p.value,
      max.block.size = 20
    )
    models = tibble(source = f, 
                    country_sample = smpl, 
                    year_range = paste0(min(dat$year),":",max(dat$year)), 
                    p_val = p.value, 
                    is = list(is),
                    iis = ii,
                    b_size = 20,
                    ar = 0)
  }

#save the continent models for developing
saveRDS(results_developing_continent,"Results_developing_continent.RDS")

## run for developed economies

rel_forms_trends <- tot_forms_trends[5:8]

rel_forms_trends_continent <- tot_forms_trends_continent_AR1[5:8]


#run models with getspanel

results_developed <- foreach(f = rel_forms_trends, .combine = rbind, .packages = c('tidyverse', 'getspanel')) %:%
  foreach(ii = c(FALSE), .combine = rbind) %:%
  foreach(smpl = c("AC1"), .combine = rbind) %:%
  foreach(p.value = c(0.01), .combine = rbind) %dopar% {
    dat <- df_restr_trends %>% filter(country %in% samples[[smpl]])
    is <- isatpanel(
      data = dat,
      formula = as.formula(f),
      index = c("country", "year"),
      effect = "twoways",
      iis = ii,
      fesis = TRUE,
      ar = 0,
      t.pval = p.value,
      max.block.size = 20
    )
    models = tibble(source = f, 
                    country_sample = smpl, 
                    year_range = paste0(min(dat$year),":",max(dat$year)), 
                    p_val = p.value, 
                    is = list(is),
                    iis = ii,
                    b_size = 20,
                    ar = 0)
  }

saveRDS(results_developed,"Results_developed.RDS")


results_developed_continent <- foreach(f = rel_forms_trends_continent, .combine = rbind, .packages = c('tidyverse', 'getspanel')) %:%
  foreach(ii = c(FALSE), .combine = rbind) %:%
  foreach(smpl = c("AC1"), .combine = rbind) %:%
  foreach(p.value = c(0.01), .combine = rbind) %dopar% {
    dat <- df_restr_trends_continent %>% filter(country %in% samples[[smpl]])
    is <- isatpanel(
      data = dat,
      formula = as.formula(f),
      index = c("country", "year"),
      effect = "twoways",
      iis = ii,
      fesis = TRUE,
      ar = 0,
      t.pval = p.value,
      max.block.size = 20
    )
    models = tibble(source = f, 
                    country_sample = smpl, 
                    year_range = paste0(min(dat$year),":",max(dat$year)), 
                    p_val = p.value, 
                    is = list(is),
                    iis = ii,
                    b_size = 20,
                    ar = 0)
  }

saveRDS(results_developed_continent,"Results_developed_continent.RDS")

##run for developed economies with east-west continent split 

df_restr_trends <- df_excl %>% mutate(country = as.factor(country),
                                      trend = year - 1999,
                                      trend_sq = trend^2)


df_restr_trends_continent <- df_restr_trends %>% mutate(continent = countrycode(sourcevar = country,origin = "country.name",destination = "continent"))
df_restr_trends_continent$continent[df_restr_trends_continent$country == 'China'] = ''

##split Europe in EU 15+Switzerland+Norway and others 

EU_15 <- data.frame('country' = c("Austria", "Belgium", "Germany", "Denmark", "Spain", "Finland",
                                  "France", "United Kingdom", "Greece", "Ireland", "Italy",
                                  "Luxembourg", "Netherlands",
                                  "Portugal", "Sweden"))

df_restr_trends_continent$continent[df_restr_trends_continent$country %in% EU_15$country] = 'EU_15'


df_restr_trends_continent$continent = as.factor(df_restr_trends_continent$continent)
df_restr_trends_continent$year_fact = as.factor(df_restr_trends_continent$year)

##create separate continent dummies 
df_restr_trends_continent = df_restr_trends_continent %>% mutate(var = 1) %>% spread(continent, var, fill = 0, sep = "_") %>% left_join(df_restr_trends_continent)


# Base forms
tot_forms_trends <- c(paste0(tot_forms, " + country:trend"))

tot_forms_trends_continent_AR1 <- c(paste0(tot_forms, " + country:trend + continent_Europe:year_fact+continent_EU_15:year_fact+continent_Americas:year_fact+continent_Oceania:year_fact"))

#only keep final models
rel_forms_trends_continent <- tot_forms_trends_continent_AR1[5:8]


results_developed_continent <- foreach(f = rel_forms_trends_continent, .combine = rbind, .packages = c('tidyverse', 'getspanel')) %:%
  foreach(ii = c(FALSE), .combine = rbind) %:%
  foreach(smpl = c("AC1"), .combine = rbind) %:%
  foreach(p.value = c(0.01), .combine = rbind) %dopar% {
    dat <- df_restr_trends_continent %>% filter(country %in% samples[[smpl]])
    is <- isatpanel(
      data = dat,
      formula = as.formula(f),
      index = c("country", "year"),
      effect = "twoways",
      iis = ii,
      fesis = TRUE,
      ar = 0,
      t.pval = p.value,
      max.block.size = 20
    )
    models = tibble(source = f, 
                    country_sample = smpl, 
                    year_range = paste0(min(dat$year),":",max(dat$year)), 
                    p_val = p.value, 
                    is = list(is),
                    iis = ii,
                    b_size = 20,
                    ar = 0)
  }

saveRDS(results_developed_continent,"Results_developed_continent_west_east_europe_split.RDS")
