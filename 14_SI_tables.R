#########################
# SI REGRESSION TABLES
# The following script creates a .txt file with a table of the relevant sectoral 
# codes underlying sector-level emissions used in the analysis, 
# summary statistics tables, and all regression tables included in the supplementary materials. 
#########################

library(tidyverse)
library(devtools)
library(gets)
library(getspanel)
library(here)
library(conflicted)
library(modelsummary)
library(kableExtra)

conflict_prefer_all("dplyr", quiet = TRUE)
options(modelsummary_format_numeric_latex = "plain")

setwd("F:\\Desktop\\科研项目\\1.负责科研项目\\Climate Policy DID Science-厦大_北师大\\Science")
source('00_oecd_project_functions.R')


################################################################################
################################################################################
################### SECTOR CODES ###############################################
################################################################################
################################################################################

ltitle = here("code_archive/SI_latex_complete.txt")

cat(
  paste0(
    "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% \n",
    "%             SECTOR CODES.                                        % \n",
    "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% \n",
    "\n \n \n"),
  file = ltitle
)

# The following table outlines which sector codes are included in the 
# sector-level emissions numbers used for the central analysis.
codes <- readRDS(here("code_archive/sector_codes_final_7_3_2023.RDS")) %>% rename(category = `new category`)

sink(ltitle, append=TRUE)
codes %>%
  kable(format = "latex", booktabs = TRUE, col.names = c("IPCC Code (2006)", "Code Description", "Sector"), caption = "Sector Codes") %>% print
sink()


################################################################################
################################################################################
################### SUMMARY STATISTICS #########################################
################################################################################
################################################################################

cat(
  paste0(
    "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% \n",
    "%             SUMMARY STATISTICS                                   % \n",
    "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% \n",
    "\n \n \n"),
  file = ltitle,
  append = TRUE
)

dfi <- readRDS(here('code_archive/duplicated/break_detection_regression_input.RDS')) %>% 
  # scale variables for summary statistics
  mutate(
    # GDP in billions
    bgdp = gdp/1000000000,
    # Population in millions
    mpop = pop/1000000,
    sc_buildings_co2 = buildings_co2/1000,
    sc_electricity_heat_co2 = electricity_heat_co2/1000,
    sc_industry_co2 = industry_co2/1000,
    sc_transport_co2 = transport_co2/1000)

excl_all <- dfi %>% group_by(country) %>% 
  summarise(excl_test = mean(log_total_emissions_co2)) %>% filter(excl_test < 10) %>% pull(country) %>% unique

df_sum <- dfi %>% 
  filter(!(country %in% excl_all))

rel_vars <- c("Building Sector \n Emissions (CO2)" = "sc_buildings_co2", 
              "Electricity & Heat\n Sector Emissions (CO2)" = "sc_electricity_heat_co2", 
              "Industry Sector\n Emissions (CO2)" = "sc_industry_co2", 
              "Transport Sector\n Emissions (CO2)" = "sc_transport_co2",
              "GDP" = "bgdp",
              "Population" = "mpop",
              "Heating Degree Days\n (base temperature = 16 C)" = "hdd",
              "Cooling Degree Days\n (base temperature = 18 C)" = "cdd")

# Printing in groups of 2
sink(ltitle, append=TRUE)
ct = 1
for(i in 1:4){
  df_sum[c('country', rel_vars[ct:(ct+1)])] %>%
    rename(all_of(rel_vars[ct:(ct+1)])) %>% 
    pivot_longer(cols = !country, names_to = "dep", values_to = "value") %>% 
    rename(` ` = 3) %>% 
    datasummary(country * (` `) ~ dep*(mean + sd + min + max),
                data = .,
                col.names = c("Country", "",rep(c("Mean", "SD", "Min", "Max"), 2)), output = 'latex') %>% print
  ct = ct + 2
}
sink()

cat(" \n \n \n \n \n",
    " \n %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% \n ",
    file = ltitle,
    append = TRUE)


################################################################################
################################################################################
################### REGRESSION RESULTS #########################################
################################################################################
################################################################################

# Load standard model results
standard <- readRDS(here("code_archive/main_model_result_parameter_variation.RDS")) %>%
  trans_si %>%
  filter(gas_spec == "CO2" & dep != "Total Emissions" & p_val %in% c(0.01, 0.001))

test_mods(standard)

cat(
  paste0(
    "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% \n",
    "%                                                                  % \n",
    "%                      MODEL RESULTS                               % \n",
    "%                                                                  % \n",
    "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% \n",
    "\n \n \n"),
  file = ltitle,
  append = TRUE
)


for(sector in unique(standard$dep)){

  cat(
    paste0(
      "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% \n",
      "%  SECTOR = ", sector,       " \n",
      "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% \n",
      "\n \n "),
    file = ltitle,
    append = TRUE
  )
  
  for(country_sample in c("AC1", "AC6")){
    # Print Sample Header
    cat(
      paste0(
        "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% \n",
        "% SAMPLE = ", country_sample,             " \n",
        "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% \n",
        "\n \n "),
      file = ltitle,
      append = TRUE
    )
    
    sink(ltitle, append=TRUE)
    print(output(standard, sector, country_sample))
    sink()
    
    cat(" \n \n \n \n \n",
        " \n %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% \n ",
        file = ltitle,
        append = TRUE)
  }
}

################################################################################
################################################################################
################### ROBUSTNESS CHECKS ##########################################
################################################################################
################################################################################
results_developed_we <- readRDS(here("code_archive/duplicated/Results_developed_continent_west_east_europe_split.RDS")) %>% 
  trans_si %>% 
  mutate(mod_name = "with continent-year FE (east/west Europe split)")

results_developed <- readRDS(here("code_archive/duplicated/Results_developed_continent.RDS")) %>% 
  trans_si %>% 
  mutate(mod_name = "with continent-year FE") %>% 
  rbind(results_developed_we)

results_cont_year_fe <- readRDS(here("code_archive/duplicated/Results_developing_continent.RDS")) %>% 
  trans_si %>% 
  mutate(mod_name = "with continent-year FE") %>% 
  rbind(results_developed)


# Load robustness check results
# Correct EU Dummy version incorporated into below (as per procedure in master_cleaning.Rmd)
rob_sel <- readRDS(here("code_archive/robustness_checks_control_variation.RDS")) %>% 
  trans_si %>% 
  rbind(., results_cont_year_fe) %>% 
  mutate(mod_name = case_when(mod_name == "Principal Model (CO2)" ~ "(1) main",
                              mod_name == "Principal Model (GHG)" ~ "(2) w. GHG as outcome",
                              mod_name == "w.o GDP" ~ "(3) w.o GDP", 
                              mod_name == "w.o GDP^2" ~ "(4) w. linear GDP only" ,
                              mod_name == "w. Pop & Pop^2" ~ "(5) w. Pop & Pop^2" ,
                              mod_name == "w. Pop & Urban Pop" ~ "(6) w. Pop & Urban Pop",
                              mod_name == "w. Pop & Urban Pop (lin & sq)" ~ "(7) w. Pop & Urban Pop (lin & sq)", 
                              mod_name == "w. Urban Pop" ~ "(8) w. Urban Pop", 
                              mod_name == "with continent-year FE" ~ "(9) with continent-year FE",
                              mod_name == "with continent-year FE (east/west Europe split)" ~ "(10) with continent-year FE (east/west Europe split)"),
         model_no = case_when(mod_name ==  "(1) main" ~ 1,
                              mod_name ==  "(2) w. GHG as outcome" ~ 2,
                              mod_name ==  "(3) w.o GDP" ~ 3, 
                              mod_name ==  "(4) w. linear GDP only" ~ 4,
                              mod_name ==  "(5) w. Pop & Pop^2" ~ 5,
                              mod_name ==  "(6) w. Pop & Urban Pop" ~ 6,
                              mod_name ==  "(7) w. Pop & Urban Pop (lin & sq)" ~ 7, 
                              mod_name ==  "(8) w. Urban Pop" ~ 8, 
                              mod_name ==  "(9) with continent-year FE" ~ 9,
                              mod_name == "(10) with continent-year FE (east/west Europe split)" ~ 10))
test_mods(rob_sel)


cat(
  paste0(
    "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% \n",
    "%                                                                  % \n",
    "%                ROBUSTNESS CHECKS                                 % \n",
    "%                                                                  % \n",
    "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% \n",
    "\n \n \n"),
  file = ltitle,
  append = TRUE
)

for(sector in c("Buildings","Electricity Heat", "Industry", "Transport")){
  
  cat(
    paste0(
      "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% \n",
      "%  SECTOR = ", sector,       " \n",
      "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% \n",
      "\n \n "),
    file = ltitle,
    append = TRUE
  )
  
  for(country_sample in c("AC1", "AC6")){
    
    cat(
      paste0(
        "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% \n",
        "% SAMPLE = ", country_sample,             " \n",
        "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% \n",
        "\n \n "),
      file = ltitle,
      append = TRUE
    )
    
    
    sink(ltitle, append=TRUE)
    if(country_sample == "AC6"){tp <- rob_sel %>% filter(!grepl("EU", mod_name))}else{tp <- rob_sel}
    print(rob_output(tp, sector, country_sample))
    sink()
    
    cat(" \n \n \n \n \n",
        " \n %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% \n ",
        file = ltitle,
        append = TRUE)
  }
}

############################
# LASSO Robustness Checks
############################
# Requires using separate branch of getspanel to adequately handle lasso model formats

devtools::install_github("moritzpschwarz/getspanel", ref = "lasso")
library(getspanel)

load(here("code_archive/duplicated/20240515 Final Lasso Models.RData"))

lasso_robs <- final_lasso_modellist %>% 
  pivot_longer(cols = c(gets_model, lasso_model, threshold_lasso_model)) %>% 
  mutate(mod_name = case_when(name == "gets_model"~ "main",
                              name == "lasso_model" ~ "adaptive lasso",
                              name == "threshold_lasso_model" ~ "threshold lasso"),
         model_no = case_when(name == "gets_model"~ 1,
                              name == "lasso_model" ~ 2,
                              name == "threshold_lasso_model" ~ 3),
         dep = case_when(grepl("buildings", source) ~ "Buildings",
                         grepl("transport", source) ~ "Transport",
                         grepl("electricity", source) ~ "Electricity Heat",
                         grepl("industry", source) ~ "Industry")) %>% 
  rename(is = value)

test_mods(lasso_robs)

cat(
  paste0(
    "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% \n",
    "%                                                                  % \n",
    "%                LASSO ROBUSTNESS CHECKS                           % \n",
    "%                                                                  % \n",
    "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% \n",
    "\n \n \n"),
  file = ltitle,
  append = TRUE
)

for(sector in c("Buildings","Electricity Heat", "Industry", "Transport")){
  
  cat(
    paste0(
      "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% \n",
      "%  SECTOR = ", sector,       " \n",
      "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% \n",
      "\n \n "),
    file = ltitle,
    append = TRUE
  )
  
  for(country_sample in c("AC1", "AC6")){
    
    cat(
      paste0(
        "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% \n",
        "% SAMPLE = ", country_sample,             " \n",
        "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% \n",
        "\n \n "),
      file = ltitle,
      append = TRUE
    )
    
    
    sink(ltitle, append=TRUE)
    print(rob_output(lasso_robs, sector, country_sample))
    sink()
    
    cat(" \n \n \n \n \n",
        " \n %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% \n ",
        file = ltitle,
        append = TRUE)
  }
}

