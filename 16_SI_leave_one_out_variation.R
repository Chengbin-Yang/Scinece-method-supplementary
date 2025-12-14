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
library(kableExtra)

# Country groupings
setwd("F:\\Desktop\\科研项目\\1.负责科研项目\\Climate Policy DID Science-厦大_北师大\\Science")

##load country groups 


#Developed economies = AC1
#Developing economies = AC6

AC6 <- read_excel("C:\\Users\\stecheme.PIKACCOUNTS\\Documents\\Break_detection\\OECD_project\\country_groupings.xlsx", sheet = 2)

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

# Base forms
tot_forms_trends <- c(paste0(tot_forms, " + country:trend"))

##run models for developing economies
rel_forms_trends <- tot_forms_trends[1:4]

#run models with getspanel
results_developing <- foreach(f = rel_forms_trends, .combine = rbind, .packages = c('tidyverse', 'getspanel')) %:%
  foreach(ii = c(FALSE), .combine = rbind) %:%
  foreach(smpl = c("AC6"), .combine = rbind) %:%
  foreach(leave_out = c('', samples$AC6), .combine=rbind) %:%
  foreach(p.value = c(0.01), .combine = rbind) %dopar% {
    dat <- df_restr_trends %>% filter(country %in% samples[[smpl]])
    dat <- dat %>% filter(country != leave_out)
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
                    left_out = leave_out,
                    year_range = paste0(min(dat$year),":",max(dat$year)), 
                    p_val = p.value, 
                    is = list(is),
                    iis = ii,
                    b_size = 20,
                    ar = 0)
  }

saveRDS(results_developing,"Break_detection_results_leave_one_out_robustness.RDS")

results_developing$sector = str_to_title(sapply(strsplit(results_developing$source, "_"), function(x) x[2]))

##extract negative breaks for each specification
leave_one_out_developing = list()
for(i in 1:nrow(results_developing)){
  out = list(get_breaks_list(results_developing$is[[i]]))
  out$sector = results_developing$sector[i]
  out$left_out = results_developing$left_out[i]
  leave_one_out_developing[[i]] = out
}

for(i in 1:nrow(results_developing)){
  out = leave_one_out_developing[[i]][[1]]
  out$sector = leave_one_out_developing[[i]]$sector
  out$left_out = leave_one_out_developing[[i]]$left_out
  leave_one_out_developing[[i]] = out
}

leave_one_out_developing_df <- bind_rows(leave_one_out_developing)

preserved_sectors = list()

counter = 1
for(sector in c('Buildings','Electricity','Industry','Transport')){
  sector_df = leave_one_out_developing_df[leave_one_out_developing_df$sector == sector,]
  sector_df = sector_df[c('id','time','left_out')]
  main = sector_df[sector_df$left_out=='',]
  
  preserved_df = data.frame("left_out" = unique(sector_df$left_out), "preserved_share" = NA, "sector" = sector)
  
  for(country in samples$AC6){
    left_out_df = sector_df[sector_df$left_out==country,]
    
    #add confidence interval around new breaks (shifts within 2-year confidence intervals are considered robust)
    left_out_expanded <- left_out_df %>%
      mutate(time = as.numeric(time)) %>%
      uncount(weights = 5, .id = "row_id") %>%
      mutate(time = time - 2 + (row_id - 1)) %>%
      select(-row_id)
    
    missing = setdiff( main[c('id','time')], left_out_expanded[c('id','time')])
    missing = missing[missing$id != country,]
    
    #count out % preserved 
    preserved_share = (nrow(main[main$id != country,])-nrow(missing))/nrow(main[main$id != country,])
    preserved_df$preserved_share[preserved_df$left_out == country] = preserved_share
  }
  preserved_sectors[[counter]] = preserved_df
  counter = counter+1
}

preserved_sectors_developing <- bind_rows(preserved_sectors)

##compute mean per sector

total_developing = preserved_sectors_developing %>% group_by(sector) %>% summarize(mean_value = mean(preserved_share, na.rm = TRUE))

developing_print = preserved_sectors_developing %>% group_by(left_out) %>% mutate(name = row_number()) %>% ungroup() %>%
  pivot_wider(id_cols = "left_out", 
              values_from = "preserved_share",
              names_prefix = "sector") %>% as.data.frame()

colnames(developing_print) = c('Country left out','Buildings','Electricity','Industry','Transport')

developing_print <- developing_print %>%
  na.omit() %>% mutate(across(where(is.numeric), ~round(., digits = 2)))


# Add the summary row to the dataframe
developing_print <- bind_rows(developing_print, developing_print %>%
                               summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%
                               mutate(across(where(is.numeric), ~round(., digits = 2))) %>%
                               mutate(`Country left out` = "Summary"))

latex_table <- kable(df_with_summary, format = "latex", booktabs = TRUE, digits = 2, row.names = FALSE) %>%
  kable_styling(latex_options = "hold_position")

write.csv(developing_print, 'leave_one_out_developing_results_final.csv')



### run for developed economies

#only keep final models
rel_forms_trends <- tot_forms_trends[5:8]

#run models with getspanel

results_developed <- foreach(f = rel_forms_trends, .combine = rbind, .packages = c('tidyverse', 'getspanel')) %:%
  foreach(ii = c(FALSE), .combine = rbind) %:%
  foreach(smpl = c("AC1"), .combine = rbind) %:%
  foreach(leave_out = c('', samples$AC1), .combine=rbind) %:%
  foreach(p.value = c(0.01), .combine = rbind) %dopar% {
    dat <- df_restr_trends %>% filter(country %in% samples[[smpl]])
    dat <- dat %>% filter(country != leave_out)
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
                    left_out = leave_out,
                    year_range = paste0(min(dat$year),":",max(dat$year)), 
                    p_val = p.value, 
                    is = list(is),
                    iis = ii,
                    b_size = 20,
                    ar = 0)
  }

saveRDS(results_developed,"Break_detection_results_leave_one_out_robustness_developed.RDS")


results_developed$sector = str_to_title(sapply(strsplit(results_developed$source, "_"), function(x) x[2]))

##extract negative breaks for each specification
leave_one_out_developed = list()
for(i in 1:nrow(results_developed)){
  out = list(get_breaks_list(results_developed$is[[i]]))[[1]]
  out$sector = results_developed$sector[i]
  out$left_out = results_developed$left_out[i]
  leave_one_out_developed[[i]] = out
}

leave_one_out_developed_df <- bind_rows(leave_one_out_developed)

write.csv(leave_one_out_developed_df, 'developed_leave_one_out_save.csv')

preserved_sectors = list()

counter = 1
for(sector in c('Buildings','Electricity','Industry','Transport')){
  sector_df = leave_one_out_developed_df[leave_one_out_developed_df$sector == sector,]
  sector_df = sector_df[c('id','time','left_out')]
  main = sector_df[sector_df$left_out=='',]
  
  preserved_df = data.frame("left_out" = unique(sector_df$left_out), "preserved_share" = NA, "sector" = sector)
  
  for(country in samples$AC1){
    left_out_df = sector_df[sector_df$left_out==country,]
    
    #add confidence interval around new breaks (shifts within 2-year confidence intervals are considered robust)
    left_out_expanded <- left_out_df %>%
      mutate(time = as.numeric(time)) %>%
      uncount(weights = 5, .id = "row_id") %>%
      mutate(time = time - 2 + (row_id - 1)) %>%
      select(-row_id)
    
    missing = setdiff( main[c('id','time')], left_out_expanded[c('id','time')])
    missing = missing[missing$id != country,]
    
    #count out % preserved 
    preserved_share = (nrow(main[main$id != country,])-nrow(missing))/nrow(main[main$id != country,])
    preserved_df$preserved_share[preserved_df$left_out == country] = preserved_share
  }
  preserved_sectors[[counter]] = preserved_df
  counter = counter+1
}

preserved_sectors_developed <- bind_rows(preserved_sectors)

##compute mean per sector

total_developed = preserved_sectors_developed %>% group_by(sector) %>% summarize(mean_value = mean(preserved_share, na.rm = TRUE))

developed_print = preserved_sectors_developed %>% group_by(left_out) %>% mutate(name = row_number()) %>% ungroup() %>%
  pivot_wider(id_cols = "left_out", 
              values_from = "preserved_share",
              names_prefix = "sector") %>% as.data.frame()

colnames(developed_print) = c('Country left out','Buildings','Electricity','Industry','Transport')

developed_print <- developed_print %>%
  na.omit() %>% mutate(across(where(is.numeric), ~round(., digits = 2)))


# Add the summary row to the dataframe
developed_print <- bind_rows(developed_print, developed_print %>%
                                summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%
                                mutate(across(where(is.numeric), ~round(., digits = 2))) %>%
                                mutate(`Country left out` = "Summary"))

latex_table <- kable(developed_print, format = "latex", booktabs = TRUE, digits = 2, row.names = FALSE) %>%
  kable_styling(latex_options = "hold_position")

write.csv(developed_print, 'leave_one_out_developed_results_final.csv')

