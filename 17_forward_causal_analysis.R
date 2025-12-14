library(conflicted)
library(tidyverse)
library(gets)
library(getspanel)
library(here)
library(doParallel)
library(readxl)
library(gdata)

conflicted::conflicts_prefer(dplyr::filter)

df_excl <- readRDS("F:/Desktop/科研项目/1.负责科研项目/Climate Policy DID Science-厦大_北师大/Sciencebreak_detection_regression_input.RDS")


##forward causal step count 

setwd("F:\\Desktop\\科研项目\\1.负责科研项目\\Climate Policy DID Science-厦大_北师大\\Science")
source('oecd_project_functions.R')


#load oecd data with potentially effective policy introductions + tightenings

oecd_grouped = read.csv('OECD_data_preprocessed.csv')

oecd_grouped = oecd_grouped[oecd_grouped$year >1999,]
oecd_grouped = oecd_grouped[oecd_grouped$year <2021,]

#counts which years would have to have a step-shift indicator if 
#every policy gets one

#no confidence interval
result <- oecd_grouped %>%
  group_by(ISO, Module) %>%
  summarize(Unique_Years = toString(unique(year)),Year_Count = n_distinct(year)) %>% as.data.frame()

#function to do it with a confidence interval 
get_confidence_interval_years <- function(year_string,interval, return_counts=TRUE) {
  # Convert the year_string to a numeric vector
  years <- as.integer(unlist(strsplit(year_string, ", ")))
  
  # Initialize an empty vector to store the related years
  related_years <- c()
  
  # Loop through each year
  for (year in years) {
    # Calculate the related years based on the specified interval
    related <- seq(year - interval, year + interval)
    
    # Append the related years to the result vector
    related_years <- c(related_years, related)
  }
  
  # Keep only the unique years
  unique_years <- unique(related_years)
  unique_years <- unique_years[unique_years > 1999 & unique_years < 2021]
  if(return_counts==FALSE){
    return(paste(unique_years, collapse=' '))}else{
      return(length(unique_years))
    }
}

result$year_CI_2 = NA 

for(i in 1:nrow(result)){
  result$year_CI_2[i] = get_confidence_interval_years(result$Unique_Years[i],2)
}

##make multiple dfs for 0, 1, 2, 3 year CIs 
Count_dfs = list()

for(x in c(0,1,2,3)){
  result <- oecd_grouped %>%
    group_by(ISO, Module) %>%
    summarize(Unique_Years = toString(unique(year)),Year_Count = n_distinct(year)) %>% as.data.frame()
  result$Year_Count <- NA
  #get the year count
  for(i in 1:nrow(result)){
    result$Year_Count[i] = get_confidence_interval_years(result$Unique_Years[i],x)
  }
  #list the years
  for(i in 1:nrow(result)){
    result$Unique_Years[i] = get_confidence_interval_years(result$Unique_Years[i],x,return_counts = FALSE)
  }
  result$Confidence_interval_size = x
  Count_dfs[[x+1]] = result
}

##make all into one DF

Overall_counts <- do.call(rbind,Count_dfs)

##get some stats 

Overall_counts %>% group_by(Confidence_interval_size, Module) %>% summarize(total_years = sum(Year_Count))

### check which sectors/country combinations to not appear at all 

reference_df <- expand.grid(ISO = unique(Overall_counts$ISO), Module = unique(Overall_counts$Module))

# Check for missing combinations
original_df = Overall_counts[c('ISO','Module')]
original_df = original_df[!duplicated(original_df),]

missing_combinations <- anti_join(reference_df, original_df)

write.csv(Overall_counts, 'Forward_causal_step_shift_numbers.csv')

# Forward Causal Analysis -----------------------------------------------------

# Filtering full FESIS to sample ------------------------------------------

read_csv(here("data/temp/Forward_causal_step_shift_numbers.csv")) %>% 
  janitor::clean_names() %>% 
  select(-x1) -> step_shifts_raw


step_shifts_raw %>% 
  mutate(unique_years = as.list(unique_years),
         baseyears = map(unique_years, function(x){str_split(x," ")[[1]]})) %>% 
  unnest(baseyears) %>%
  mutate(baseyears = as.numeric(baseyears)) %>% 
  select(-unique_years,-year_count) %>% 
  mutate(baseyears_ci = map2(baseyears,confidence_interval_size, function(x,y){
    seq(from = x - y, to = x + y, by = 1) 
  })) %>% 
  unnest(baseyears_ci) %>% 
  select(-confidence_interval_size) -> years_to_use



uis_fun <- function(module_name = "Electricity", CI = TRUE, sample, dat){
  
  
  ## Create FESIS ------------------------------------------------------------
  N <- length(unique(dat$country))
  # Create a balanced data.frame
  df_balanced <- data.frame(id = rep(unique(dat$country),each = length(unique(dat$year))),time = rep(unique(dat$year),N))
  # Extract the names for the balanced data.frame
  # find the minimum time for each id
  fesis_names_mintime <- aggregate(dat$year, by = list(dat$country), min)
  names(fesis_names_mintime) <- c("id","mintime")
  # remove the mintime for each id
  fesis_names_merged <- merge(df_balanced, fesis_names_mintime, by = "id", sort = FALSE)
  fesis_names_intermed <- fesis_names_merged[fesis_names_merged$time != fesis_names_merged$mintime,c("id","time")]
  
  fesis_names <- paste0("uis.",fesis_names_intermed$id,".",fesis_names_intermed$time)
  
  sistlist <- do.call("list", rep(list(as.matrix(gets::sim(length(unique(dat$year))))), N))
  fesis_df <- as.data.frame(as.matrix(Matrix::bdiag(sistlist)))
  names(fesis_df) <- fesis_names
  
  fesis_df <- as_tibble(cbind(df_balanced,fesis_df)) %>% 
    rename(country = id, 
           year = time)
  
  
  # ---
  
  
  years_to_use %>% 
    
    {if(CI){select(.,-baseyears) %>% 
        rename(baseyears = baseyears_ci)
    } else {select(.,-baseyears_ci)}} %>% 
    distinct %>% 
    filter(module == module_name) %>% 
    select(-module) -> years_module
  
  years_module %>% 
    mutate(country = countrycode::countrycode(sourcevar = iso, origin = "iso3c", destination = "country.name")) %>% 
    
    filter(country %in% sample) %>% 
    
    rename(year = baseyears) %>% 
    mutate(name = paste0("uis.",country,".",year)) %>% 
    pull(name) -> years_module_breaks
  
  
  fesis_df %>% 
    select(country,year,any_of(years_module_breaks)) %>% 
    
    # quickly add dat to ensure full sample and correct order, then remove again
    left_join(dat,., by = join_by(country, year)) %>% 
    select(-all_of(names(dat))) -> uis_breaks
  
  return(uis_breaks)
  
}


# Full run ----------------------------------------------------------------
buildings_controls <- " ~ lgdp + lpop + lgdp_sq + hdd + cdd + Labels_Appliances_2001 + Labels_Appliances_2013 + MEPS_Appliances_2009 + MEPS_Appliances_2013"
electricity_controls <- " ~ lgdp + lpop + lgdp_sq + hdd + cdd + ETS_E_2005 + ETS_E_2018"
industry_controls <- " ~ lgdp + lpop + lgdp_sq + hdd + cdd + ETS_I_2005 + ETS_I_2018 + MEPS_ElectricMotors_2011 + MEPS_ElectricMotors_2015"
transport_controls <- " ~ lgdp + lpop + lgdp_sq + hdd + cdd + MEPS_T_2009"

ets_forms <- c(
  paste0("log_buildings_co2", buildings_controls),
  paste0("log_electricity_heat_co2", electricity_controls), 
  paste0("log_industry_co2", industry_controls),
  paste0("log_transport_co2", transport_controls))


# Base forms
tot_forms_trends <- c(paste0(ets_forms, " + country:trend"))
tot_forms_trends <- tot_forms_trends[!grepl("log_total_emissions",tot_forms_trends)]


####### Trends run
df_restr_trends <- df_excl %>% mutate(country = as.factor(country),
                                      trend = year - 1999)


cl <- makeCluster(8)
registerDoParallel(cl)

ets_restr_trends <- foreach(f = tot_forms_trends, .combine = rbind, .packages = c('tidyverse', 'getspanel')) %:%
  foreach(CI = c(FALSE, TRUE), .combine = rbind) %:%
  foreach(smpl = c("AC6_all", "AC1"), .combine = rbind) %:% 
  foreach(uis = c(TRUE, FALSE), .combine = rbind) %:%
  foreach(p.value = c(0.01), .combine = rbind) %dopar% {
    
    dat <- df_restr_trends %>% filter(country %in% samples[[smpl]]) %>% mutate(country = droplevels(country))
    
    sector <- switch(EXPR = str_extract(string = f, 'log_[A-z]+2'),
                     log_buildings_co2 = "Buildings",
                     log_electricity_heat_co2 = "Electricity",
                     log_transport_co2 = "Transport",
                     log_industry_co2 = "Industry")
    
    uis_breaks_cur <- uis_fun(module_name = sector,
                              CI = CI,
                              sample = samples[[smpl]], 
                              dat = dat)
    
    if(ncol(uis_breaks_cur) != 0){
      
      is <- isatpanel(
        data = dat,
        formula = as.formula(f),
        index = c("country", "year"),
        effect = "twoways",
        iis = FALSE,
        fesis = if(uis){FALSE}else{TRUE},
        uis = if(uis){uis_breaks_cur}else{NULL},
        ar = 0,
        t.pval = 0.01,
        max.block.size = 20
      )
      models = tibble(source = f,
                      country_sample = smpl,
                      year_range = paste0(min(dat$year),":",max(dat$year)),
                      p_val = 0.01,
                      is = list(is),
                      iis = FALSE,
                      b_size = 20,
                      CI = CI,
                      ar = 0,
                      uis = uis)
    }
  }
stopCluster(cl)

#save(ets_restr_trends, file = here("data/temp/20231017 uis_run.RData"))

#load(here("data/temp/20231017 uis_run.RData"))


# Now create the table for the SM -----------------------------------------

ets_restr_trends %>% 
  dplyr::filter(uis, !CI) %>% 
  mutate(n_input = map(is, function(x){x$inputdata %>% nrow}),
         n_post = map(is, function(x){x$isatpanel.result$n}), 
         covariates = map(is, function(x){
           mod <- lm(y ~ as.factor(time) + as.factor(id) + . -1, data = x$inputdata)
           length(mod$coefficients[!is.na(mod$coefficients)])
         }),
         uis = map(is, function(x){ncol(x$arguments$uis)})) %>% 
  unnest(c(n_input, n_post, uis, covariates, uis)) %>% 
  select(-is) %>% 
  
  mutate(full = (uis + covariates) /  n_post,
         only_uis = uis /  n_post) %>% 
  
  dplyr::mutate(mod_name = case_when(grepl('buildings', source) ~ "Buildings",
                                     grepl('electricity', source) ~ "Electricity",
                                     grepl('transport', source) ~ "Transport",
                                     grepl('industry', source) ~ "Industry")) %>% 
  
  select(mod_name, country_sample, n_post, covariates, uis, full, only_uis) -> data_ready


library(kableExtra)

data_ready %>% 
  mutate(country_sample = case_when(country_sample == "AC6_all" ~ "Developing", TRUE ~ "Developed"),
         full = scales::percent(full, accuracy = 0.1),
         only_uis = scales::percent(only_uis, accuracy = 0.01),
         full = gsub("%","\\%",full, fixed = TRUE),
         only_uis = gsub("%","\\%",only_uis, fixed = TRUE)) %>% 
  
  kable(format = "latex", booktabs = TRUE,escape = FALSE,
        caption = "Observations and Saturation with Indicators when using the insights from the climate policy database and including all known policies in the model. When including all known All Known Policies (AKCP) most models would be reduced to very few degrees of freedom or, when considering all controls incl. trends and fixed effects would be close to being fully saturated.",
        label = "si_tbl:forward_causal",
        col.names = c("Sector", "Sample", "$n$", "$k$", "AKCP", "Saturation w AKCP \\& controls", "Saturation w AKCP")) %>% 
  
  
  kable_styling(latex_options = "scale_down") %>% 
  kable_paper()




# Plot for the SM ---------------------------------------------------------


library(MetBrewer)
library(ggtext)

years_to_use %>%  
  mutate(country = countrycode::countrycode(sourcevar = iso, origin = "iso3c", destination = "country.name")) %>% 
  mutate(country = case_when(iso == "SVK" ~ "Slovak Republic", 
                             iso == "CZE" ~ "Czech Republic",
                             TRUE ~ country)) %>% 
  
  left_join(samples %>% 
              enframe %>% 
              filter(name %in% c("AC1","AC6_all")) %>% 
              unnest(value) %>% 
              rename(country = value,
                     sample = name)) %>% 
  ggplot(aes(x = baseyears, y = fct_rev(iso), fill = sample)) + 
  geom_tile(color = "grey", linewidth = 0.01) + 
  facet_wrap(~module) + 
  theme_minimal(base_size = 12) + 
  scale_fill_manual(values = c("#c27668","#7ba0b4"))+
  scale_x_continuous(expand = c(0,0)) +
  labs(x = NULL, y = NULL, title = "All Known Policies from the OECD Database", 
       subtitle = "All policies from the OECD Database for <span style = color:#c27668>Developed</span> and <span style = color:#7ba0b4>Developing</span> countries.") +
  theme(plot.subtitle = ggtext::element_markdown(size = 12),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(size = 8),
        panel.grid.major.y =  element_blank(),
        panel.grid.minor.y =  element_blank(), 
        legend.position = "none", 
        panel.border = element_rect(colour = "black", fill = NA)) -> p

ggsave(filename = here("output/forward_causal.pdf"), plot = p, dpi = 1000, bg = "white", width = 10, height = 12)



years_to_use %>%  
  mutate(country = countrycode::countrycode(sourcevar = iso, origin = "iso3c", destination = "country.name")) %>% 
  mutate(country = case_when(iso == "SVK" ~ "Slovak Republic", 
                             iso == "CZE" ~ "Czech Republic",
                             TRUE ~ country)) %>% 
  
  left_join(samples %>% 
              enframe %>% 
              filter(name %in% c("AC1","AC6_all")) %>% 
              unnest(value) %>% 
              rename(country = value,
                     sample = name)) %>% 
  ggplot(aes(x = baseyears_ci, y = fct_rev(iso), fill = sample)) + 
  geom_tile(color = "grey", linewidth = 0.01) + 
  facet_wrap(~module) + 
  theme_minimal(base_size = 12) + 
  scale_fill_manual(values = c("#c27668","#7ba0b4"))+
  scale_x_continuous(expand = c(0,0)) +
  labs(x = NULL, y = NULL, title = "All Known Policies from the OECD Database", 
       subtitle = "All policies from the OECD Database for <span style = color:#c27668>Developed</span> and <span style = color:#7ba0b4>Developing</span> countries.") +
  theme(plot.subtitle = ggtext::element_markdown(size = 12),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(size = 8),
        panel.grid.major.y =  element_blank(),
        panel.grid.minor.y =  element_blank(), 
        legend.position = "none", 
        panel.border = element_rect(colour = "black", fill = NA)) -> p

ggsave(filename = here("output/forward_causal_CI.pdf"), plot = p, dpi = 1000, bg = "white", width = 10, height = 12)



