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
library(stringr)

# Country groupings
setwd("F:\\Desktop\\科研项目\\1.负责科研项目\\Climate Policy DID Science-厦大_北师大\\Science")
source('00_oecd_project_functions.R')

Lasso_results <- readRDS("final_lasso_models.RDS")

gets_results <- subset(Lasso_results, select=-c(lasso_model,threshold_lasso_model))
colnames(gets_results)[colnames(gets_results) == 'gets_model'] = 'is'
gets_results$model = 'main'

lasso_results <- subset(Lasso_results, select=-c(gets_model,threshold_lasso_model))
colnames(lasso_results)[colnames(lasso_results) == 'lasso_model'] = 'is'
lasso_results$model = 'adaptive lasso'

lasso_threshold <- subset(Lasso_results, select=-c(gets_model,lasso_model))
colnames(lasso_threshold)[colnames(lasso_threshold) == 'threshold_lasso_model'] = 'is'
lasso_threshold$model = 'threshold lasso'

Lasso_results <- rbind(gets_results, lasso_results, lasso_threshold)


Lasso_results$sector = str_to_title(sapply(strsplit(Lasso_results$source, "_"), function(x) x[2]))

lasso_results_df = list()

for(i in 1:nrow(Lasso_results)){
  print(i)
  out = get_breaks_list(Lasso_results$is[[i]])
  out$sector = Lasso_results$sector[i]
  out$model = Lasso_results$model[i]
  out$country_sample = Lasso_results$country_sample[i]
  lasso_results_df[[i]] = out
}

lasso_results_df_combined <- bind_rows(lasso_results_df)

write.csv(lasso_results_df_combined, 'lasso_results_df_combined.csv')

lasso_results_df_combined <- read.csv("lasso_results_df_combined.csv")

## name models 
lasso_results_df_combined$model[lasso_results_df_combined$model == 'lasso'] = "adaptive lasso"
lasso_results_df_combined$model[lasso_results_df_combined$model == 'lasso_threshold'] = "threshold lasso"

lasso_results_df_combined$model <- factor(lasso_results_df_combined$model, levels = c('main','adaptive lasso','threshold lasso'))


AC1_lasso_results_df <- lasso_results_df_combined[lasso_results_df_combined$country_sample == 'AC1',]

preserved_sectors = list()

counter = 1
for(sector in c('Buildings','Electricity','Industry','Transport')){
  sector_df = AC1_lasso_results_df[AC1_lasso_results_df$sector == sector,]
  sector_df = sector_df[c('id','time','model')]
  main = sector_df[sector_df$model=='main',]
  
  #expand breaks in main to include confidence intervals 
  
  main_expanded <- main %>%
    mutate(time = as.numeric(time)) %>%
    uncount(weights = 5, .id = "row_id") %>%
    mutate(time = time - 2 + (row_id - 1)) %>%
    select(-row_id)
  
  other_models = unique(sector_df$model[sector_df$model != 'main'])
  
  model_df_filtered = main
  
  for(other_model in other_models){
    other_model_df = sector_df[sector_df$model == other_model,]
    #filter out "extra breaks" (i.e. that are not in the confidence interval of the old breaks)
    other_model_df_filtered <- semi_join(other_model_df[c('id','time')], main_expanded[c('id','time')])
    other_model_df_filtered$model = other_model
    model_df_filtered = rbind(model_df_filtered, other_model_df_filtered)
  }
  
  model_df_filtered$sector = sector
  preserved_sectors[[counter]] = model_df_filtered
  counter = counter+1
}

prep_for_plotting <- function(df){
  
  plotting_df <- df %>%
    mutate(breaks = 1) %>% 
    select(id, time, breaks, model, sector) %>% 
    complete(id, time = 2000:2021, model, sector) %>% 
    group_by(id) %>% 
    #    fill(breaks, .direction = "down") %>% 
    ungroup()
  
  #add confidence intervals in main 
  main = plotting_df[plotting_df$model=='main',]
  
  main_updated <- main %>%
    group_by(id) %>%
    mutate(
      # Identify the two years before and after a 'break' value of 1
      should_replace = ifelse(
        lag(breaks, 1) == 1 | lag(breaks, 2) == 1 | lead(breaks, 1) == 1 | lead(breaks, 2) == 1,
        1, 
        0
      )) %>% mutate(breaks = if_else(is.na(breaks) & should_replace == 1, 0, breaks))
  
  plotting_final <- rbind(main_updated[c('id','time','model','sector','breaks')],plotting_df[plotting_df$model != 'main',])
  
  #expand the confidence interval of main to plot in other color in all specifications 
  plotting_final <- plotting_final %>%
    group_by(id, time) %>%
    mutate(
      should_replace = ifelse(
        any(model == "main" & breaks == 0 | model == 'main' & breaks == 1),
        1, 
        0
      )) %>% mutate(breaks = if_else(is.na(breaks) & should_replace == 1, 0, breaks))
  
  plotting_final$breaks[plotting_final$breaks == 0 & plotting_final$model != 'main'] = 2
  
  plotting_final$breaks = as.factor(plotting_final$breaks)
  return(plotting_final)
}

plot_break_comparison <- function(df, my_color){
  plotting_df <- prep_for_plotting(df)
  
  p <- plotting_df %>%
    # makes sure that empty rows are shown as well (ie. where no breaks are detected)
    ggplot(aes(x = time, y = model)) +
    geom_tile(aes(fill = breaks), na.rm = TRUE) +  
    scale_fill_manual(na.value = NA, values =c('darkgrey',my_color, 'lightgrey'))+
    scale_x_continuous(expand = c(0,0)) +
    scale_y_discrete(expand = c(0,0), limits = rev) +
    facet_grid(id~., scales = "free_y", space = "free") +
    theme_bw() +
    theme(panel.grid = element_blank(),
          panel.border = element_rect(fill = NA),
          strip.background = element_blank(),
          axis.text.y = element_text(size = 20, color = "black"), #set to 15 for all specifications except transport developed (12 there)
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

png("developed_buildings_lasso_overview.png", width     = 13.00,height    = 18.00,units     = "in",res       = 200)
p_buildings
dev.off()

p_electricity = plot_break_comparison(preserved_sectors[[2]], col_list[[2]])

png("developed_electricity_lasso_overview.png", width     = 13.00,height    = 18.00,units     = "in",res       = 200)
p_electricity
dev.off()

p_industry = plot_break_comparison(preserved_sectors[[3]], col_list[[3]])

png("developed_industry_lasso_overview.png", width     = 13.00,height    = 18.00,units     = "in",res       = 200)
p_industry
dev.off()

p_transport = plot_break_comparison(preserved_sectors[[4]], col_list[[4]])

png("developed_transport_lasso_overview.png", width     = 13.00,height    = 18.00,units     = "in",res       = 200)
p_transport
dev.off()

#### developing 

AC6_lasso_results_df <- lasso_results_df_combined[lasso_results_df_combined$country_sample == 'AC6',]

preserved_sectors = list()

counter = 1
for(sector in c('Buildings','Electricity','Industry','Transport')){
  sector_df = AC6_lasso_results_df[AC6_lasso_results_df$sector == sector,]
  sector_df = sector_df[c('id','time','model')]
  main = sector_df[sector_df$model=='main',]
  
  #expand breaks in main to include confidence intervals 
  
  main_expanded <- main %>%
    mutate(time = as.numeric(time)) %>%
    uncount(weights = 5, .id = "row_id") %>%
    mutate(time = time - 2 + (row_id - 1)) %>%
    select(-row_id)
  
  other_models = unique(sector_df$model[sector_df$model != 'main'])
  
  model_df_filtered = main
  
  for(other_model in other_models){
    other_model_df = sector_df[sector_df$model == other_model,]
    #filter out "extra breaks" (i.e. that are not in the confidence interval of the old breaks)
    other_model_df_filtered <- semi_join(other_model_df[c('id','time')], main_expanded[c('id','time')])
    other_model_df_filtered$model = other_model
    model_df_filtered = rbind(model_df_filtered, other_model_df_filtered)
  }
  
  model_df_filtered$sector = sector
  preserved_sectors[[counter]] = model_df_filtered
  counter = counter+1
}

col_list = list("#eb5601", "#e7c019", "#bac36b", "#3b9ab2")

p_buildings = plot_break_comparison(preserved_sectors[[1]], col_list[[1]])

png("developing_buildings_lasso_overview.png", width     = 13.00,height    = 18.00,units     = "in",res       = 200)
p_buildings
dev.off()

p_electricity = plot_break_comparison(preserved_sectors[[2]], col_list[[2]])

png("developing_electricity_lasso_overview.png", width     = 13.00,height    = 18.00,units     = "in",res       = 200)
p_electricity
dev.off()

p_industry = plot_break_comparison(preserved_sectors[[3]], col_list[[3]])

png("developing_industry_lasso_overview.png", width     = 13.00,height    = 18.00,units     = "in",res       = 200)
p_industry
dev.off()

p_transport = plot_break_comparison(preserved_sectors[[4]], col_list[[4]])

png("developing_transport_lasso_overview.png", width     = 13.00,height    = 18.00,units     = "in",res       = 200)
p_transport
dev.off()
