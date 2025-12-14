################################################################################
#### author: Ebba Mark
#### Script that performs break_detection in main specification as well as 
#### robustness checks in which parameters are varied and controls are varied. 
#### relies on "doparallel" parallel computing package.
################################################################################

library(tidyverse)
library(gets)
library(getspanel)
library(here)
library(doParallel)
library(readxl)
library(gdata)

# Country groupings
# Developing vs Developed: 4 x AC6 & AC1
# Source: https://www.oecd-ilibrary.org/sites/f0773d55-en/1/4/3/index.html?itemId=/content/publication/f0773d55-en&_csp_=5026909c969925715cde6ea16f4854ee&itemIGO=oecd&itemContentType=book
AC6 <- read_excel(here('code_archive/country_groupings.xlsx'), sheet = 2)
AC1 <- read_excel(here('code_archive/country_groupings.xlsx')) %>% pull(Developed) %>% unlist
for(i in 1:nrow(AC6)){
  temp <- AC6 %>% slice(i) %>% pull(countries) %>% strsplit(., ", ") %>% unlist
  mv(from = "temp", to = paste0("AC6_", AC6$cat_abbrev[i]))
}
AC6_HICs <- c(AC6_HICs, "Russia")

AC6_all <- c(AC6_HICs, AC6_LICs, AC6_LMICs, AC6_UMICs)

# Combine
samples <- mget(c("AC1", "AC6_all"))

dfi <- readRDS(here("code_archive/break_detection_regression_input.RDS"))  %>% 
  filter(year >= 2000)

# Exclude countries according to minimum emissions restriction
excl_all <- dfi %>% group_by(country) %>% 
  summarise(excl_test = mean(log_total_emissions_co2)) %>% filter(excl_test < 10) %>% pull(country) %>% unique

# Additional control variables for urban population and population^2 to be used in robustness checks below
additional_controls <- readRDS(here('code_archive/additional_control_variables.RDS'))

df_excl <- dfi %>% 
  filter(!(country %in% excl_all) & country != "Israel") %>% 
  # Incorporating linear country-specific time trends
  mutate(country = as.factor(country),
         trend = year - 1999) %>% 
  left_join(., additional_controls, by = c("country", "year"))

###############################################################################
# MAIN SPECIFICATION WITH PARAMETER VARIATIONS

# The following block runs the principal specification as well as the additional 
# robustness checks that vary the false detection rate, 
# impulse indicator saturation, and ghg vs co2 emissions

standard_controls <- " ~ lgdp + lpop + lgdp_sq + hdd + cdd + country:trend"

total_controls <- " + Labels_Appliances_2001 + Labels_Appliances_2013 + MEPS_Appliances_2009 + MEPS_Appliances_2013 + ETS_E_2005 + ETS_E_2018 + ETS_I_2005 + ETS_I_2018 + MEPS_T_2009 + MEPS_ElectricMotors_2011 + MEPS_ElectricMotors_2015"

buildings_controls <- " + Labels_Appliances_2001 + Labels_Appliances_2013 + MEPS_Appliances_2009 + MEPS_Appliances_2013"

electricity_controls <- " + ETS_E_2005 + ETS_E_2018"

industry_controls <- " + ETS_I_2005 + ETS_I_2018 + MEPS_ElectricMotors_2011 + MEPS_ElectricMotors_2015"

transport_controls <- " + MEPS_T_2009"

standard_forms <- c(
  paste0("log_total_emissions_co2", standard_controls, total_controls),
  paste0("log_buildings_co2", standard_controls, buildings_controls),
  paste0("log_electricity_heat_co2", standard_controls, electricity_controls), 
  paste0("log_industry_co2", standard_controls, industry_controls),
  paste0("log_transport_co2", standard_controls, transport_controls),
  paste0("log_total_emissions_ghg_co2e", standard_controls, total_controls),
  paste0("log_buildings_ghg_co2e", standard_controls, buildings_controls),
  paste0("log_electricity_heat_ghg_co2e", standard_controls, electricity_controls), 
  paste0("log_industry_ghg_co2e", standard_controls, industry_controls),
  paste0("log_transport_ghg_co2e", standard_controls, transport_controls))

cl <- makeCluster(30)
registerDoParallel(cl)
standard_models <- foreach(f = standard_forms, .combine = rbind, .packages = c('tidyverse', 'getspanel')) %:%
  foreach(ii = c(TRUE, FALSE), .combine = rbind) %:%
  foreach(smpl = c("AC6_all", "AC1"), .combine = rbind) %:%
  foreach(p.value = c(0.01, 0.001), .combine = rbind) %dopar% {
    dat <- df_excl %>% filter(country %in% samples[[smpl]])
    is <- isatpanel(
      data = dat,
      formula = as.formula(f),
      index = c("country", "year"),
      effect = "twoways",
      iis = ii,
      fesis = TRUE,
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

stopCluster(cl)

standard_models %>% 
  saveRDS(here("code_archive/main_model_result_parameter_variation.RDS"))