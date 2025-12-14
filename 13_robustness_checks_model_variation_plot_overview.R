library(tidyverse)
library(devtools)
library(gets)
library(getspanel)
library(here)
library(doParallel)
library(readxl)
library(gdata)
library(countrycode)

# Country groupings
setwd("F:\\Desktop\\科研项目\\1.负责科研项目\\Climate Policy DID Science-厦大_北师大\\Science")


control_variation <- readRDS("all_robustness_checks_combined.RDS")  
developing_fe_variation <- readRDS("Results_developing_continent.RDS")
developed_fe_variation <- readRDS("Results_developed_continent.RDS")
developed_fe_variation_east_west <- readRDS("Results_developed_continent_west_east_europe_split.RDS")

results_developing <- readRDS("Results_developing.RDS")
results_developed <- readRDS("Results_developed.RDS")


## get all models for AC6 (developing) and preprocess

control_variation_developing = control_variation %>% filter(country_sample == 'AC6')
control_variation_developing = control_variation_developing %>% select(-c("dep","formula","gas_spec"))
##add model names for other runs

results_developing$mod_name = 'main'
developing_fe_variation$mod_name = 'with continent-year FE'

results_developing_compare = bind_rows(list(control_variation_developing, results_developing, developing_fe_variation))

results_developing_compare$sector = str_to_title(sapply(strsplit(results_developing_compare$source, "_"), function(x) x[2]))

results_developing_compare_out = list()
for(i in 1:nrow(results_developing_compare)){
  print(i)
  out = list(get_breaks_list(results_developing_compare$is[[i]]))[[1]]
  out$sector = results_developing_compare$sector[i]
  out$mod_name = results_developing_compare$mod_name[i]
  results_developing_compare_out[[i]] = out
}

results_developing_compare_df <- bind_rows(results_developing_compare_out)
write.csv(results_developing_compare_df,'results_developing_compare_df.csv')


results_developing_compare_df <- read.csv('results_developing_compare_df.csv')

#make sure main is not duplicated
results_developing_compare_df = results_developing_compare_df[results_developing_compare_df$mod_name!= "Principal Model (CO2)",]

#number and clarify naming of specifications
results_developing_compare_df$mod_name[results_developing_compare_df$mod_name == "main"] = "(1) main"
results_developing_compare_df$mod_name[results_developing_compare_df$mod_name == "Principal Model (GHG)"] = "(2) w. GHG as outcome" 
results_developing_compare_df$mod_name[results_developing_compare_df$mod_name == "w.o GDP"] = "(3) w.o GDP" 
results_developing_compare_df$mod_name[results_developing_compare_df$mod_name == "w.o GDP^2"] = "(4) w. linear GDP only" 
results_developing_compare_df$mod_name[results_developing_compare_df$mod_name == "w. Pop & Pop^2"] = "(5) w. Pop & Pop^2" 
results_developing_compare_df$mod_name[results_developing_compare_df$mod_name == "w. Pop & Urban Pop"] = "(6) w. Pop & Urban Pop" 
results_developing_compare_df$mod_name[results_developing_compare_df$mod_name == "w. Pop & Urban Pop (lin & sq)"] = "(7) w. Pop & Urban Pop (lin & sq)" 
results_developing_compare_df$mod_name[results_developing_compare_df$mod_name == "w. Urban Pop"] = "(8) w. Urban Pop" 
results_developing_compare_df$mod_name[results_developing_compare_df$mod_name == "with continent-year FE"] = "(9) with continent-year FE" 

preserved_sectors = list()

counter = 1
for(sector in c('Buildings','Electricity','Industry','Transport')){
  sector_df = results_developing_compare_df[results_developing_compare_df$sector == sector,]
  sector_df = sector_df[c('id','time','mod_name')]
  main = sector_df[sector_df$mod_name=='(1) main',]
  
  #expand breaks in main to include confidence intervals 
  
  main_expanded <- main %>%
    mutate(time = as.numeric(time)) %>%
    uncount(weights = 5, .id = "row_id") %>%
    mutate(time = time - 2 + (row_id - 1)) %>%
    select(-row_id)
  
  other_models = unique(sector_df$mod_name[sector_df$mod_name != '(1) main'])
  
  model_df_filtered = main
  
  for(other_model in other_models){
    other_model_df = sector_df[sector_df$mod_name == other_model,]
    s
    #filter out "extra breaks" (i.e. that are not in the confidence interval of the old breaks)
    other_model_df_filtered <- semi_join(other_model_df[c('id','time')], main_expanded[c('id','time')])
    other_model_df_filtered$mod_name = other_model
    model_df_filtered = rbind(model_df_filtered, other_model_df_filtered)
  }
  
  model_df_filtered$sector = sector
  preserved_sectors[[counter]] = model_df_filtered
  counter = counter+1
}

prep_for_plotting <- function(df){
  
  plotting_df <- df %>%
    mutate(breaks = 1) %>% 
    select(id, time, breaks, mod_name, sector) %>% 
    complete(id, time = 2000:2021, mod_name, sector) %>% 
    group_by(id) %>% 
    #    fill(breaks, .direction = "down") %>% 
    ungroup()
  
  #add confidence intervals in main 
  main = plotting_df[plotting_df$mod_name=='(1) main',]
  
  main_updated <- main %>%
    group_by(id) %>%
    mutate(
      # Identify the two years before and after a 'break' value of 1
      should_replace = ifelse(
        lag(breaks, 1) == 1 | lag(breaks, 2) == 1 | lead(breaks, 1) == 1 | lead(breaks, 2) == 1,
        1, 
        0
      )) %>% mutate(breaks = if_else(is.na(breaks) & should_replace == 1, 0, breaks))
  
  plotting_final <- rbind(main_updated[c('id','time','mod_name','sector','breaks')],plotting_df[plotting_df$mod_name != '(1) main',])
  
  #expand the confidence interval of main to plot in other color in all specifications 
  plotting_final <- plotting_final %>%
    group_by(id, time) %>%
    mutate(
      should_replace = ifelse(
        any(mod_name == "(1) main" & breaks == 0 | mod_name == '(1) main' & breaks == 1),
        1, 
        0
      )) %>% mutate(breaks = if_else(is.na(breaks) & should_replace == 1, 0, breaks))
  
  plotting_final$breaks[plotting_final$breaks == 0 & plotting_final$mod_name != '(1) main'] = 2
  
  plotting_final$breaks = as.factor(plotting_final$breaks)
  return(plotting_final)
}

plot_break_comparison <- function(df, my_color){
  plotting_df <- prep_for_plotting(df)
  
  p <- plotting_df %>%
    # makes sure that empty rows are shown as well (ie. where no breaks are detected)
    ggplot(aes(x = time, y = mod_name)) +
    geom_tile(aes(fill = breaks), na.rm = TRUE) +  
    scale_fill_manual(na.value = NA, values =c('darkgrey',my_color, 'lightgrey'))+
    scale_x_continuous(expand = c(0,0)) +
    scale_y_discrete(expand = c(0,0), limits = rev) +
    facet_grid(id~., scales = "free_y", space = "free") +
    theme_bw() +
    theme(panel.grid = element_blank(),
          panel.border = element_rect(fill = NA),
          strip.background = element_blank(),
          axis.text.y = element_text(size = 12, color = "black"), #set to 15 for all specifications except transport developed (12 there)
          axis.text.x = element_text(size = 25, color = "black"),
          strip.text.y = element_text(size = 25, angle = 0),
          plot.caption = element_text(size = 25, hjust = 0.5),
          legend.position = "none"
    ) +
    labs(x = NULL, y = NULL,title = NULL)
  
  return(p)
}


col_list = list("#eb5601", "#e7c019", "#bac36b", "#3b9ab2")

p_buildings = plot_break_comparison(preserved_sectors[[1]], col_list[[1]])

png("developing_buildings_robustness_overview.png", width     = 13.00,height    = 18.00,units     = "in",res       = 200)
p_buildings
dev.off()

p_electricity = plot_break_comparison(preserved_sectors[[2]], col_list[[2]])

png("developing_electricity_robustness_overview.png", width     = 13.00,height    = 18.00,units     = "in",res       = 200)
p_electricity
dev.off()

p_industry = plot_break_comparison(preserved_sectors[[3]], col_list[[3]])

png("developing_industry_robustness_overview.png", width     = 13.00,height    = 18.00,units     = "in",res       = 200)
p_industry
dev.off()

p_transport = plot_break_comparison(preserved_sectors[[4]], col_list[[4]])

png("developing_transport_robustness_overview.png", width     = 13.00,height    = 18.00,units     = "in",res       = 200)
p_transport
dev.off()

#replicate for developed economies 

control_variation_developed = control_variation %>% filter(country_sample == 'AC1')
control_variation_developed = control_variation_developed %>% select(-c("dep","formula","gas_spec"))

##add model names for other runs

results_developed$mod_name = 'main'
developed_fe_variation$mod_name = 'with continent-year FE'
developed_fe_variation_east_west$mod_name = 'with continent-year FE (east/west Europe split)'

results_developed_compare = bind_rows(list(control_variation_developed, results_developed, developed_fe_variation, developed_fe_variation_east_west))

results_developed_compare$sector = str_to_title(sapply(strsplit(results_developed_compare$source, "_"), function(x) x[2]))

results_developed_compare_out = list()
for(i in 1:nrow(results_developed_compare)){
  print(i)
  out = list(get_breaks_list(results_developed_compare$is[[i]]))[[1]]
  out$sector = results_developed_compare$sector[i]
  out$mod_name = results_developed_compare$mod_name[i]
  results_developed_compare_out[[i]] = out
}

results_developed_compare_df <- bind_rows(results_developed_compare_out)
write.csv(results_developed_compare_df,'results_developed_compare_df.csv')

results_developed_compare_df <- read.csv('results_developed_compare_df.csv')

results_developed_compare_df = results_developed_compare_df[results_developed_compare_df$mod_name!= "Principal Model (CO2)",]

results_developed_compare_df = results_developed_compare_df[results_developed_compare_df$mod_name!= "w. EU Dummy",]

#number specifications 

results_developed_compare_df$mod_name[results_developed_compare_df$mod_name == "main"] = "(1) main"
results_developed_compare_df$mod_name[results_developed_compare_df$mod_name == "Principal Model (GHG)"] = "(2) w. GHG as outcome" 
results_developed_compare_df$mod_name[results_developed_compare_df$mod_name == "w.o GDP"] = "(3) w.o GDP" 
results_developed_compare_df$mod_name[results_developed_compare_df$mod_name == "w.o GDP^2"] = "(4) w. linear GDP only" 
results_developed_compare_df$mod_name[results_developed_compare_df$mod_name == "w. Pop & Pop^2"] = "(5) w. Pop & Pop^2" 
results_developed_compare_df$mod_name[results_developed_compare_df$mod_name == "w. Pop & Urban Pop"] = "(6) w. Pop & Urban Pop" 
results_developed_compare_df$mod_name[results_developed_compare_df$mod_name == "w. Pop & Urban Pop (lin & sq)"] = "(7) w. Pop & Urban Pop (lin & sq)" 
results_developed_compare_df$mod_name[results_developed_compare_df$mod_name == "w. Urban Pop"] = "(8) w. Urban Pop" 
results_developed_compare_df$mod_name[results_developed_compare_df$mod_name == "with continent-year FE"] = "(9) with continent-year FE" 
results_developed_compare_df$mod_name[results_developed_compare_df$mod_name == "with continent-year FE (east/west Europe split)"] = "(10) with continent-year FE (east/west Europe split)" 

results_developed_compare_df$mod_name = factor(results_developed_compare_df$mod_name, levels = c("(1) main", "(2) w. GHG as outcome", "(3) w.o GDP", "(4) w. linear GDP only", "(5) w. Pop & Pop^2", "(6) w. Pop & Urban Pop", "(7) w. Pop & Urban Pop (lin & sq)", "(8) w. Urban Pop", "(9) with continent-year FE", "(10) with continent-year FE (east/west Europe split)"))

preserved_sectors = list()

counter = 1

for(sector in c('Buildings','Electricity','Industry','Transport')){
  sector_df = results_developed_compare_df[results_developed_compare_df$sector == sector,]
  sector_df = sector_df[c('id','time','mod_name')]
  main = sector_df[sector_df$mod_name=='(1) main',]
  
  #expand breaks in main to include confidence intervals 
  
  main_expanded <- main %>%
    mutate(time = as.numeric(time)) %>%
    uncount(weights = 5, .id = "row_id") %>%
    mutate(time = time - 2 + (row_id - 1)) %>%
    select(-row_id)
  
  other_models = unique(sector_df$mod_name[sector_df$mod_name != '(1) main'])
  
  model_df_filtered = main
  
  for(other_model in other_models){
    other_model_df = sector_df[sector_df$mod_name == other_model,]
    #filter out "extra breaks" (i.e. that are not in the confidence interval of the old breaks)
    other_model_df_filtered <- semi_join(other_model_df[c('id','time')], main_expanded[c('id','time')])
    other_model_df_filtered$mod_name = other_model
    model_df_filtered = rbind(model_df_filtered, other_model_df_filtered)
  }
  
  model_df_filtered$sector = sector
  preserved_sectors[[counter]] = model_df_filtered
  counter = counter+1
}

col_list = list("#eb5601", "#e7c019", "#bac36b", "#3b9ab2")

p_buildings = plot_break_comparison(preserved_sectors[[1]], col_list[[1]])

png("developed_buildings_robustness_overview.png", width     = 13.00,height    = 18.00,units     = "in",res       = 200)
p_buildings
dev.off()

p_electricity = plot_break_comparison(preserved_sectors[[2]], col_list[[2]])

png("developed_electricity_robustness_overview.png", width     = 13.00,height    = 18.00,units     = "in",res       = 200)
p_electricity
dev.off()

p_industry = plot_break_comparison(preserved_sectors[[3]], col_list[[3]])

png("developed_industry_robustness_overview.png", width     = 13.00,height    = 18.00,units     = "in",res       = 200)
p_industry
dev.off()

p_transport = plot_break_comparison(preserved_sectors[[4]], col_list[[4]])

png("developed_transport_robustness_overview.png", width     = 13.00,height    = 18.00,units     = "in",res       = 200)
p_transport
dev.off()
