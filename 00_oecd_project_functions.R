library(data.table)
library(dplyr)
library(tidyr)
library(xlsx)
library(stringr)
library(countrycode)
library(vtable)
library(cowplot)
library(ggplot2)
library(pheatmap)
library(cowplot)
library(gsubfn)
library(data.table)
library(tidyverse)
library(openxlsx)
library(getspanel)
library(here)
library(doParallel)
library(gridExtra)
library(conflicted)
library(viridis)
library(gplots)
library(devtools)
library(RColorBrewer)
library(Polychrome)
library(grid)
library(scales)
library("rnaturalearth")
library("rnaturalearthdata")
library(wesanderson)
library(seriation)
library(ggpubr)
library(gets)
library(modelsummary)
library(kableExtra)
library(miscTools)
library(broom)

conflict_prefer("filter", "dplyr")
conflict_prefer("first", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("here","here")
conflicts_prefer(ggpubr::get_legend)
conflicts_prefer(dplyr::summarize)
conflicts_prefer(lubridate::year)

conflict_prefer_all("dplyr", quiet = TRUE)
options(modelsummary_format_numeric_latex = "plain")


## takes an isatpanel object, uses break_uncertainty() in getspanel to format output and get uncertainty range

get_breaks_list <- function(res){
  out = break_uncertainty(res)
  out$country_code = countrycode(out$id,'country.name','iso3c')
  out[out$id=='SouthKorea','country_code'] = 'KOR'
  out[out$id=='SouthAfrica','country_code'] = 'ZAF'
  if(any(is.na(out$country_code))){
    print('Warning! Unmatched country names in out. Investigate!')
  }
  out$min_year = as.numeric(out$time)-as.numeric(out$tci)
  out$max_year = as.numeric(out$time)+as.numeric(out$tci)
  #filter negative breaks
  out = out[out$coef <0,]
  return(out)
}

## takes the preprocessed oecd data (as data), the formated break detection data (out), a sector (module) + optional parameters
## automatically extracts for each break and module the policies that fall between the max and min year
## uses the tci in out for break range unless a fixed-interval is specified (as an int)
## if tci_interval is true, it takes for each break the maximum of the specified fixed interval and the tci
## if introductions_only is true, the oecd data is filtered to only match introductions

filter_oecd <- function(data,country,min_year,max_year,module){
  data = data %>% 
    filter(ISO %in% country) %>% 
    filter(Module == module) %>%
    filter(year >= min_year & year <= max_year)
  
  return(data)
}

match_oecd_policies <- function(data,out,module,fixed_interval = 0, tci_interval = FALSE, introductions_only = FALSE){
  
  
  if(tci_interval == TRUE){
    #in this case, we keep the maximum of the tci interval and the fixed interval
    breaks_modify = which(out$tci<= fixed_interval)
    
    out$min_year[breaks_modify] = out$time[breaks_modify]-fixed_interval
    out$max_year[breaks_modify] = out$time[breaks_modify]+fixed_interval
    
  }else if(fixed_interval>0){
    out$min_year = out$time-fixed_interval
    out$max_year = out$time+fixed_interval
    
  }
  
  if(introductions_only == TRUE){
    #in this case we only want to keep introductions and add ons 
    add_ons = data[data$source == 'add-on', ]
    data = data[data$introduction == 1,]
    data = data[!is.na(data$ISO),]
    
    data = rbind(data,add_ons)
  }
  
  ##match break policies
  policy_store = data.frame()
  rownames(out) <- NULL
  for(i in 1:nrow(out)){
    policy_match = filter_oecd(data,out$country_code[i],out$min_year[i],out$max_year[i],module)
    if(nrow(policy_match)>0){
      policy_match$coeff = out$coef[i]
      policy_match$min_year = out$min_year[i]
      policy_match$max_year = out$max_year[i]
      policy_match$unique_break_identifier = paste(out$country_code[i],out$min_year[i],out$max_year[i],sep='_')}
    policy_store = rbind(policy_store,policy_match)
  }
  return(policy_store)
}

#Takes the "out" output and computes overlapping breaks. Overlapping means two breaks happen in the same sector and country
# s.t. the confidence interval of one completely contains the other break. In that case, we keep the one with the wider confidence
#interval 

##NOTE: THE BREAK OVERLAP IS DONE SOLELY ON THE TCI INTERVAL ATM AND DOES NOT CHANGE FOR FIXED CONFIDENCE INTS

filter_break_overlap <- function(df){
  df$included_count <- 0  # Initialize overlap count for each event
  
  for (i in 1:nrow(df)) {
    # Extract the current event's country and confidence interval
    current_country <- df$id[i]
    current_min_year <- df$min_year[i]
    current_max_year <- df$max_year[i]
    
    # Find other events in the same country
    same_country_events <- df[df$id == current_country, ]
    
    # Check for overlap with other events
    for (j in 1:nrow(same_country_events)) {
      if (df$time[i] != same_country_events$time[j]) {
        other_min_year <- same_country_events$min_year[j]
        other_max_year <- same_country_events$max_year[j]
        
        # Check if the intervals overlap
        if (current_min_year >= other_min_year && current_max_year <= other_max_year) {
          df$included_count[i] <- df$included_count[i] + 1
          print(paste('The intervals overlap for',as.character(current_country),as.character(current_min_year),as.character(current_max_year),'and',as.character(current_country),as.character(other_min_year),as.character(other_max_year),sep=' '))
        }
      }
    }
  }
  df_filtered <- df[df$included_count ==0,]
  return(df_filtered)
}

## PLOTTING FOR FIGS 2 + 3 

##function for model picking
f <- function(k) {
  step <- k
  function(y) seq(floor(min(y)), ceiling(max(y)), by = step)
}

#adjusted from getspanel package for this project 

plot_counterfactual <- function(x, country, out, plus_t = 5, facet.scales = "free", title = NULL, zero_line = FALSE,int_size=2){
  df <- x$estimateddata
  indicators <- x$isatpanel.result$aux$mX
  indicators <- indicators[,!colnames(indicators) %in% names(df)]
  df <- cbind(df,indicators)
  
  if(is.null(x$isatpanel.result$fit)){
    fitted <- as.numeric(x$isatpanel.result$mean.fit)
  } else {
    fitted <- as.numeric(x$isatpanel.result$fit)
  }
  df$fitted = fitted
 
  df_ident <- out
  df_ident$maxtime = max(out$max_year)
  df_ident$origtime <- df_ident$time
  
  # make sure the preceding observation collapses on the last observation
  df_ident_start <- df_ident
  df_ident_start$time <- df_ident_start$time - 1
  df_ident_start$coef <- 0
  df_ident_start$sd <- 0
  df_ident_start$tci <- NA
  
  df_ident_overall <- rbind(df_ident_start, df_ident)
  for(i in 1:plus_t){
    intermed <- df_ident
    intermed$time <- intermed$time + i
    intermed$time <- ifelse(intermed$time > intermed$maxtime, intermed$maxtime, intermed$time)
    df_ident_overall <- rbind(df_ident_overall, intermed)
  }
  df_ident_overall <- df_ident_overall[order(df_ident_overall$name, df_ident_overall$time),]
  df_ident_overall <- df_ident_overall[!duplicated(df_ident_overall),]
  
  
  effects <- merge(x$estimateddata, df_ident_overall, by = c("id","time"), all.x = TRUE)
  effects <- merge(effects,data.frame(x$estimateddata[,c("id","time")], fitted), by = c("id","time"))
  
  effects$cf <-  (effects$coef * (-1)) +  effects$fitted
  effects$cf_upr <- ((effects$coef + (1.96 * effects$sd)) * (-1)) +  effects$fitted
  effects$cf_lwr <- ((effects$coef - (1.96 * effects$sd)) * (-1)) +  effects$fitted
  effects$cf_upr99 <- ((effects$coef + (2.57 * effects$sd)) * (-1)) +  effects$fitted
  effects$cf_lwr99 <- ((effects$coef - (2.57 * effects$sd)) * (-1)) +  effects$fitted
  
  effects$start_rect <- effects$origtime - effects$tci
  effects$end_rect <- effects$origtime + effects$tci
  
  effects$cf_upr[is.na(effects$cf_upr)] <- effects$fitted[is.na(effects$cf_upr)]
  effects$cf_lwr[is.na(effects$cf_lwr)] <- effects$fitted[is.na(effects$cf_lwr)]

  df <- subset(df,id==country)
  effects <- subset(effects, id==country)
  effects_error = effects
  effects_error <- effects_error[!is.na(effects_error$start_rect),]
  effects_error <- effects_error[!duplicated(effects_error$name),]
  effects_2y <- effects_error
  effects_2y$min_year = effects_2y$time-int_size
  effects_2y$max_year = effects_2y$time+int_size
  out_plot <- subset(out, id==country)
  out_plot <- as.data.frame(out_plot)
  
  title = trimws(gsub("([A-Z])", " \\1", country, perl = TRUE))
  if(title == "Czech Republic"){
    title = "Czechia"
  }
  if(title == "Slovak Republic"){
    title = "Slovakia"
  }
  
  ggplot(df, aes_(
    x = ~time,
    y = ~fitted,
    group = ~id
  )) -> g
  
  
  
  if(zero_line){g = g + geom_hline(aes(yintercept = 0))}
  
  g +
    geom_line(aes_(y = ~y, color = "black"), size = 0.7, show.legend=FALSE) +
    
    geom_rect(data = effects_error, aes(xmin = min_year, xmax = max_year, ymin = -Inf, ymax = Inf),fill = "grey",alpha = 0.5, na.rm = TRUE, show.legend=FALSE) +
    
    geom_rect(data = effects_2y, aes(xmin = min_year, xmax = max_year, ymin = -Inf, ymax = Inf),fill = "grey",alpha = 0.3, na.rm = TRUE, show.legend=FALSE) +
    
    geom_line(aes(color = "blue"),linetype = 1, size = 0.5,show.legend = FALSE) +
    
    # fesis
    #geom_rect(data = out_plot, aes(xmin = ~min_year, xmax = ~max_year, ymin = -Inf, ymax = Inf, group = ~name),fill = "grey",alpha = 0.3, na.rm = TRUE, show.legend=FALSE)+
    
    geom_vline(data = out_plot, aes_(xintercept = ~time,color="red"),show.legend = FALSE) +
    
    geom_ribbon(data = effects, aes_(ymin = ~cf_lwr, ymax = ~cf_upr, fill = "red", group = ~name), alpha = 0.5, na.rm = FALSE,show.legend=FALSE) +
    
    geom_line(data = effects, aes_(y = ~cf, color = "red", group = ~name), na.rm = TRUE) +
    
    scale_color_identity(name = NULL,
                         breaks = c("black", "blue", "grey", "purple", "red","darkgreen", "orange"),
                         labels = c("y","Fitted","IIS","SIS","FESIS","CFESIS", "CSIS"),
                         guide = "legend") +
    
    scale_linetype(name = "Variable") +
    guides(fill = "none",color='none') +
    scale_y_continuous(breaks = pretty_breaks(n=3))+
    xlim(c(1998,max(max(out$time)+2,max(out$max_year),2022)))+
    theme(
      plot.title = element_text(size=25),
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      strip.background = element_blank(),
      panel.border = element_rect(colour = "grey",fill = NA),
      panel.background = element_blank(),
      axis.line.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.x = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line.y = element_line(),
      axis.text.y = element_text(size=15, face="bold"),
      axis.title.y = element_text(size=12, face="bold"),
      legend.key = element_rect(fill = NA),
      legend.key.size = unit(2,'cm'),
      legend.text = element_text(size=15),
    ) +
    
    labs(title = title, subtitle = NULL, y = NULL, x = NULL) -> plotoutput
  
  return(plotoutput)
  
}

#produces Fig. 2+3
plot_ts_example_with_policy <- function(country,res,out,policy_match,label_df,cube_size=5,symbol_size=3, ylim=c(0,3),policy_plot_prop=1, sector = "x",int_size=2,legend=TRUE){
  
  p <- plot_counterfactual(res,country,out,int_size=int_size)
  
  iso = countrycode(country,origin='country.name',destination='iso3c')
  
  if(country == 'SouthAfrica'){
    iso = 'ZAF'
  }
  if(country == 'SouthKorea'){
    iso = 'KOR'
  }
  policy_match_country <- policy_match[policy_match$ISO==iso,]
  if(sector != "x"){
    policy_match_country = policy_match_country[policy_match_country$Module==sector,]
  }
  
  policy_match_country <- policy_match_country %>%
    group_by(year) %>%
    mutate(enumeration = row_number()) %>% ungroup
  
  policy_match_plot <- policy_match_country[c('year','Module','Policy_name_fig_2_3',"enumeration")]
  
  
  label_df_sub = label_df[label_df$country == country & label_df$Module==sector,]
  label_df_sub = select(label_df_sub,-c('country'))
  
  if(nrow(label_df_sub)>0){
    #EU_flags = data.frame(year = unique(label_df_sub$year), indicator = 0.9, icon = "\U1F1EA\U1F1FA")
    EU_flags = data.frame(year = unique(label_df_sub$year), indicator = 0.9, icon = "EU")
    
    label_df_sub$icon <- case_when(label_df_sub$Policy_name == "EU-MEPS" ~ "\u2699",  # Manufacturing wheel icon
                                   label_df_sub$Policy_name == "EU-Labels" ~ "\U0001f3f7",  # Label icon
                                   label_df_sub$Policy_name == "EU-ETS" ~ "€",  # Euro sign icon
                                   label_df_sub$Policy_name == 'EU' ~ "EU", #EU flag
                                   TRUE ~ "")
    
    label_df_sub$family <- case_when(label_df_sub$Policy_name == "EU-MEPS" ~ "Noto",  # Manufacturing wheel icon
                                     label_df_sub$Policy_name == "EU-Labels" ~ "Noto",  # Label icon
                                     label_df_sub$Policy_name == "EU-ETS" ~ "Arial",  # Euro sign icon
                                     label_df_sub$Policy_name == 'EU' ~ "Arial", #EU flag
                                     TRUE ~ "")
    
  }
  
  ##enumerate the labels in each year s.t. they do not overlap in case of duplications, start at 2 bc we plot eu sign on 1
  label_df_sub <- label_df_sub %>%
    group_by(year) %>%
    mutate(enumeration = row_number()) %>% ungroup
  label_df_sub$enumeration = (label_df_sub$enumeration+1)-0.2
  
  
  out_sub = out[out$id == country,]
  
  
  
  p_policy <- ggplot() +
    scale_fill_manual(values = color_dict)+
    ylab('')+
    ylim(ylim)+
    scale_x_continuous(breaks=seq(2000, 2020, 10),limits = c(1998,max(max(out$time)+2,max(out$max_year),2022)))+
    theme(
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      axis.title.x = element_blank(),
      axis.text.x = element_text(size=15, face="bold"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.line.x = element_line(),
      axis.line.y = element_line(),
      axis.ticks.y = element_line(color='white'),
      axis.text.y = element_text(size=15,color='white'),
      axis.title.y = element_blank(),
      legend.key.size = unit(0.2,'cm'),
      legend.text = element_text(size=10),
      legend.title = element_blank(),
      legend.position = 'none')
  
  if(nrow(policy_match_plot)>0){
    policy_match_plot <- policy_match_plot %>%
      complete(Policy_name_fig_2_3,year = 1998:max(max(out$max_year),2022), fill = list(enumeration = 0)) %>% as.data.frame()
    p_policy <- p_policy +
      geom_rect(data = out_sub, aes(xmin = min_year, xmax = max_year, ymin = -Inf, ymax = Inf),fill = "grey",alpha = 0.5, na.rm = TRUE, show.legend=FALSE) +
      geom_rect(data = out_sub, aes(xmin = time-int_size, xmax = time+int_size, ymin = -Inf, ymax = Inf),fill = "grey",alpha = 0.3, na.rm = TRUE, show.legend=FALSE) +
      geom_point(data = policy_match_country, aes(x=year, y=enumeration, fill=Policy_name_fig_2_3),shape=22,size=cube_size)
  }
  
  if(nrow(label_df_sub) > 0){
    p_policy <- p_policy+geom_text(data = label_df_sub, aes(x = year, y = enumeration, label = icon, family = family),
                                   hjust = 0, size = symbol_size) +
      geom_text(data = EU_flags, aes(x = year, y = indicator, label = icon),
                hjust = 0, size = symbol_size*0.9)
    
  }
  
  if(legend==FALSE){
    p_policy <- p_policy + theme(legend.position="none")
  }
  
  p_combined <- cowplot::plot_grid(plotlist=list(p,NULL, p_policy), ncol=1,nrow=3,
                                   rel_heights=c(3,-0.05,policy_plot_prop), align="v",axis='bt')+theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"))
  
  
  return(p_combined)
}

##creates legend for Fig. 2 and 3

create_fig_2_3_legend <- function(key_size = 1, font_size = 15, CI_width = 2){
  
  legend_data_1 <- data.frame('x' = 1:10, 'y' = 1:10, color = 'Model fit ')
  legend_data_2 <- data.frame('x' = 1:10, 'y' = 1:10, color = 'Observed emissions')
  legend_data_3 <- data.frame('x' = 1:10, 'y' = 1:10,lower = 0:9,upper=2:11, color = 'Counterfactual emissions\nand 95% CI')
  legend_data_4 <- data.frame('x' = 1:10, 'y' = 1:10,color = 'Break with 99% CI')
  
  g_1 <- ggplot(legend_data_1)+geom_line(aes(y=y,x=x,color=color),linewidth=1)+
    geom_line(data = legend_data_2,aes(y=y,x=x,color=color),linewidth=1)+
    scale_color_manual(values=c('blue','black'),name='')+
    theme(legend.title = element_blank(),
          legend.key = element_rect(fill=NA),
          legend.key.size = unit(key_size,'cm'),
          legend.text = element_text(size=font_size))
  l_1 <- get_legend(g_1)
  g_2 <- ggplot(legend_data_3) +
    geom_ribbon(aes(ymin=lower, ymax=upper, x=x, fill=color), alpha = 0.5)+
    geom_line(aes(y=y, x=x,color = color),linewidth=1)+
    theme(legend.title = element_blank(),
          legend.key.size = unit(key_size,'cm'),
          legend.text = element_text(size=font_size))
  l_2 <- get_legend(g_2)
  g_3 <- ggplot(legend_data_4,aes(x=x,y=y,color=color)) + 
    geom_rect( aes(xmin = -1, xmax = 2, ymin = -Inf, ymax = Inf,fill = color),color=NA,alpha = 0.8, na.rm = TRUE)+
    geom_vline(aes(xintercept =1,color = color),linewidth=1)+
    scale_color_manual(values=c('red'))+scale_fill_manual(values=c('grey'))+
    theme(legend.title = element_blank(),
          legend.key.size = unit(key_size,'cm'),
          legend.text = element_text(size=font_size))
  l_3 <- get_legend(g_3)
  g_4 <- ggplot()+geom_rect(aes(xmin = -1, xmax = 2, ymin = -Inf, ymax = Inf,fill = '2 year interval'),alpha=0.3)+
    scale_fill_manual(values=c('grey'))+
    theme(legend.title = element_blank(),
          legend.key.size = unit(key_size,'cm'),
          legend.text = element_text(size=font_size),
          plot.margin = unit(c(0,0,0,0),"cm"))
  l_4 <- get_legend(g_4)
  
  my_legend = cowplot::plot_grid(plotlist = list(l_1,l_2,l_3,l_4),nrow=4,align='hv')+theme(plot.margin = unit(c(0,0,0,0),"cm"))
  
  return(my_legend)
}

##match across sectors 

sector_policy_match <- function(df, spec){
  sector_policy_match = tibble()
  counter=1
  for(s in c('Buildings','Electricity','Industry','Transport')){
    for(sp in spec){
      policy_out_sub = df[df$sector==s,]
      combined_out = rbind(policy_out_sub[1,sp][[1]][[1]],policy_out_sub[2,sp][[1]][[1]])
      
      spec_tibble = tibble(sector_policy_match = list(combined_out),
                           sector = s,
                           spec = sp)
      sector_policy_match = rbind(sector_policy_match, spec_tibble)
      
      #sector_policy_match[[counter]] = combined_out
      #counter = counter+1
    }
  }
  return(sector_policy_match)
}

##get effect size means for Fig. 4

get_effect_size_means <- function(out){
  #filter slow increase policies to avoid double counting
  out <- out[out$label!= "slow_increase",]
  #I'm using the Fig 1 policy names here because two different policies in the broader fig 4 taxation category should still count
  out <- out[!duplicated(out[, c("unique_break_identifier", "Policy_name_fig_1")]), ]
  #add a new column which indicate whether the break is single policy or not
  
  out <- out %>%
    group_by(unique_break_identifier) %>%
    mutate(Count = n()) %>%
    ungroup() %>%
    mutate(SinglePolicy = if_else(Count == 1, 1, 0))
  
  out$coef_percent <- (exp(out$coeff)-1)*100
  
  mean_df <- out %>% 
    group_by(Policy_name_fig_4, SinglePolicy, Cluster_categories) %>% 
    summarize(Average = mean(coef_percent),
              FirstQuartile = quantile(coef_percent, 0.25),
              ThirdQuartile = quantile(coef_percent, 0.75)) %>% as.data.frame()
  return(mean_df)
}

get_effect_size_means_pricing <- function(out){
  #filter slow increase policies to avoid double counting
  out <- out[out$label!= "slow_increase",]
  #I'm using the Fig 1 policy names here because two different policies in the broader fig 4 taxation category should still count
  out <- out[!duplicated(out[, c("unique_break_identifier", "Policy_name_fig_1")]), ]
  #add a new column which indicate whether the break is single policy or not
  
  out <- out %>%
    group_by(unique_break_identifier) %>%
    mutate(Count = n()) %>%
    ungroup() %>%
    mutate(SinglePolicy = if_else(Count == 1, 1, 0))
  
  #only keep mixes 
  out <- out[out$SinglePolicy == 0,]
  
  out$coef_percent <- (exp(out$coeff)-1)*100
  
  #label as pricing mix/no pricing mix 
  
  out <- out %>%
    group_by(unique_break_identifier) %>%
    mutate(Pricing_indicator = ifelse("Pricing" %in% Cluster_categories, 1,0)) %>%
    ungroup()
  
  mean_df <- out %>% 
    group_by(Policy_name_fig_4, Pricing_indicator, Cluster_categories) %>% 
    summarize(Average = mean(coef_percent),
              FirstQuartile = quantile(coef_percent, 0.25),
              ThirdQuartile = quantile(coef_percent, 0.75)) %>% as.data.frame()
  return(mean_df)
}

venn_diagram_plot_basic <- function(policy_match, sector, title, shape = 'circle'){
  
  if(sector == 'Transport' & title == 'Developed economies'){
    shape = 'ellipse'
  }
  
  policy_no_dups <- policy_match %>% group_by(unique_break_identifier) %>% distinct(Cluster_categories, .keep_all = TRUE)
  policy_no_dups$coeff <- (exp(policy_no_dups$coeff)-1)*100
  
  colors <- c('#f8f3e8','#f8dbb8','#dfebeb','#ffcbcb')
  names(colors) <- c('Subsidy','Regulation','Pricing','Information')
  
  sector_colors = c("#EB5600" , "#E7C019","#BAC36B","#3B9AB2")
  names(sector_colors) = c('Buildings','Electricity','Industry','Transport')
  
  euler_input <- policy_no_dups %>% 
    group_by(unique_break_identifier) %>% 
    arrange(unique_break_identifier, Cluster_categories) %>% 
    summarize(combination = paste0(Cluster_categories, collapse = "&"), .groups = "drop") %>% as.data.frame()
  
  ##merge in coeffs 
  coeff_sub = policy_no_dups[c('unique_break_identifier','coeff')]
  coeff_sub = coeff_sub[!duplicated(coeff_sub),]
  
  euler_input = merge(euler_input, coeff_sub,by='unique_break_identifier',all.x=TRUE)
  
  euler_input = euler_input %>% group_by(combination) %>% summarize(n = n(), mean_coeff = mean(coeff))
  
  euler_input$percent = round((euler_input$n / sum(euler_input$n))*100,1)
  
  euler_input$percent = paste(euler_input$percent,'%', sep = '')
  
  euler_input$label = euler_input$percent
  
  euler_plot <- round(euler_input$n/sum(euler_input$n),2)*100
  
  names(euler_plot) <- euler_input$combination
  
  euler_plot <- euler_plot[order(names(euler_plot))]
  
  #order the colors properly
  #get order in which categories appear
  category_order <- names(euler_plot) %>%
    paste(collapse = " ") %>%
    gsub("&", " ", .) %>%
    strsplit(" ") %>%
    unlist() %>%
    unique()
  
  
  colors_plot = colors[category_order]
  
  fit <- euler(euler_plot,
               shape = shape)
  
  labels = data.frame(combination = names(fit$fitted.values))
  labels$label = ""
  labels_store = data.frame(combination = names(fit$fitted.values))
  
  #have to do this by hand because the eulerr package sometimes changes the order of categories in the labels
  for(k in 1:nrow(labels)){
    matching_indices <- which(sapply(euler_input$combination, function(x) identical(sort(unlist(strsplit(labels$combination[k], ""))), sort(unlist(strsplit(x, ""))))))
    if(length(matching_indices)>0){
      labels$label[k] = euler_input$label[matching_indices[1]]
    }
  }
  
  
  labels <- labels[match(labels_store$combination, labels$combination), ]
  
  p <- plot(fit,labels = list(cex=1.7,padding=grid::unit(20, "mm")),quantities = list(labels = labels$label,cex = 2,padding = grid::unit(20, "mm")),fills=colors_plot,adjust_labels = TRUE)

  ##transform into ggplot object to finalize 
  
  p <- cowplot::plot_grid(plotlist=list(p)) + ggtitle(title)+theme(plot.title = element_text(size=40,face = 'bold',margin=margin(0,0,30,0)),plot.margin = unit(c(0,0,0,0), "cm"))
  
  
  return(list(p, euler_input))
}


#### SI functions (author: Ebba Mark)

# Dataframe transformation function for plotting, tables, etc.
# Names models as required in the si_output.R SI generating file
trans_si <- function(df){
  df_new <- df %>%
    separate(source, into = c("dep","formula"), sep = "[~]", remove = FALSE) %>% 
    mutate(dep = str_to_title(gsub("co2e", "", gsub("log ", "", gsub("_"," ", trimws(dep))))),
           formula = trimws(formula), 
           mod_name = case_when(grepl("lgdp + lpop + lgdp_sq + hdd + cdd + country:trend", formula, fixed = TRUE) ~ "Principal Model", 
                                grepl("^lpop +", formula) ~ "w.o GDP", 
                                grepl("lgdp + lpop + hdd + cdd + country:trend", formula, fixed = TRUE) ~ "w.o GDP^2",
                                grepl("lpop + lpop_sq + lgdp_sq", formula, fixed = TRUE) ~ "w. Pop & Pop^2",
                                grepl("lpop + l_urban_pop", formula, fixed = TRUE) ~ "w. Pop & Urban Pop",
                                grepl("lpop + lpop_sq + l_urban_pop + l_urban_pop_sq", formula, fixed = TRUE) ~ "w. Pop & Urban Pop (lin & sq)", 
                                grepl("lgdp + l_urban_pop", formula, fixed = TRUE) ~ "w. Urban Pop"),
           gas_spec = case_when(grepl("Ghg", dep) ~ "GHG",
                                grepl("Co2", dep) ~ "CO2"),
           dep = gsub(" Ghg ", "", gsub(" Co2", "", dep)),
           mod_name = case_when(mod_name == "Principal Model" ~ paste0(mod_name, " (", gas_spec, ")"),
                                TRUE ~ mod_name),
           country_sample = ifelse(country_sample == "AC6_all", "AC6", country_sample)
    )
  return(df_new)
}

# tests to make sure that all models in a dataframe are uniquely identified before plotting
test_mods <- function(df){
  stopifnot(
    # No NA Values
    "NA values present" = nrow(df[apply(df, 1, function(x) all(!is.na(x))), ]) %>% identical(nrow(df)),
    # Each row must be distinct - able to be identified from all others
    "Certain rows are not distinct" = df %>% select(-is) %>% distinct %>% nrow %>% identical(nrow(df))
  )
}

# The following is a helper function to the output functions below which returns 
# a list of the coefficients names for which negative breaks are detected 
# this result eases model labeling and filtering in the output tables that are returned from output functions below
rename_brute <- function(mods_df, all_negs = FALSE, principal_loc = 1){
  
  names <- mods_df[[principal_loc]] %>% tidy %>% filter(estimate < 0) %>% pull(term)
  
  if(all_negs){
    names <- c()
    for(k in mods_df){
      names <- k %>% tidy %>% filter(estimate < 0) %>% pull(term) %>% append(names, .)
    }
    names <- unique(names)
  }
  
  new_names <- gsub(".", ": ", gsub("fesis", "", names), fixed = TRUE)
  return(sort(setNames(new_names, names)))
}


# Function to produce output table per sector and country sample for the main specification with parameter variations.
# The following function makes sure to report the results using HAC standard errors clustered at the country level
output <- function(df, sec, sample){
  mods <- list()
  chars <- list()
  temp <- df %>%
    filter(dep == sec,
           ar == 0,
           b_size == 20,
           country_sample == sample) %>%
    arrange(-p_val, iis)
  if(nrow(temp) > 12){break}
  for(r in 1:nrow(temp)){
    temper <- temp %>% slice(r)
    temp_mod <- temper %>% pull(is) %>% first %>% robust_isatpanel(., HAC = TRUE, cluster = "group")
    mods[[paste0("(",r,")")]] = temp_mod$HAC
    chars[[r]] = paste0(temper$p_val, "; ", temper$iis)
  }
  
  subtitles <- str_split_fixed(chars, "; ", n = unique(str_count(chars, "; ")) + 1)
  text <- modelsummary(mods, keep = "fesis", coef_map = rename_brute(mods, all_negs = TRUE, principal_loc = 1),
                       gof_map = c("nobs", "bic"), stars = TRUE, 
                       output = "latex",
                       caption = paste0("Sector: ", sec, " (Country Sample: ", ifelse(sample == "AC6", "Developing economies", "Developed economies"),")")) %>% 
    add_header_above(c("Sig. Lev. in Break Det.", subtitles[,1])) %>%
    add_header_above(c("IIS Enabled", subtitles[,2])) %>% 
    column_spec(2,bold=T)
  return(text)
}


# Function to produce output tables per sector and country sample for the robustness checks in which control variables are varied.
# The following function makes sure to report the results using HAC standard errors clustered at the country level
rob_output <- function(df, sec, sample){
  mods <- list()
  chars <- list()
  temp <- df %>%
    filter(dep == sec,
           country_sample == sample) %>%
    arrange(model_no)
  if(nrow(temp) > 10){break}
  for(r in 1:nrow(temp)){
    temper <- temp %>% slice(r)
    temp_mod <- temper %>% pull(is) %>% first %>% robust_isatpanel(., HAC = TRUE, cluster = "group")
    mods[[paste0("(",r,")")]] = temp_mod$HAC #%>% tidy() %>% filter(estimate < 0) %>% as.data.frame()
    chars[r] = temper$mod_name
  }
  text <- modelsummary(mods, keep = "fesis", coef_map = rename_brute(mods, all_negs = TRUE, principal_loc = 1), 
                       gof_map = c("nobs", "bic"), stars = TRUE, 
                       output = "latex",
                       caption = paste0("Sector: ", sec, " (Country Sample: ", ifelse(sample == "AC6", "Developing economies", "Developed economies"),")")) %>%
    add_header_above(c("Model", unlist(chars))) %>% 
    column_spec(2,bold=T)
  return(text)
}


## helper functions for robustness check overviews (Annika Stechemesser)

prep_for_plotting <- function(df){
  
  plotting_df <- df %>%
    mutate(breaks = 1) %>% 
    select(id, time, breaks, model, sector) %>% 
    complete(id, time = 2000:2021, model, sector) %>% 
    group_by(id) %>% 
    #    fill(breaks, .direction = "down") %>% 
    ungroup()
  
  #add confidence intervals in main 
  main = plotting_df[plotting_df$model=='Main',]
  
  main_updated <- main %>%
    group_by(id) %>%
    mutate(
      # Identify the two years before and after a 'break' value of 1
      should_replace = ifelse(
        lag(breaks, 1) == 1 | lag(breaks, 2) == 1 | lead(breaks, 1) == 1 | lead(breaks, 2) == 1,
        1, 
        0
      )) %>% mutate(breaks = if_else(is.na(breaks) & should_replace == 1, 0, breaks))
  
  plotting_final <- rbind(main_updated[c('id','time','model','sector','breaks')],plotting_df[plotting_df$model != 'Main',])
  plotting_final$breaks = as.factor(plotting_final$breaks)
  return(plotting_final)
}

plot_break_comparison <- function(df, my_color){
  plotting_df <- prep_for_plotting(df)
  
  p <- plotting_df %>%
    # makes sure that empty rows are shown as well (ie. where no breaks are detected)
    ggplot(aes(x = time, y = model)) +
    geom_tile(aes(fill = breaks), na.rm = TRUE) +  
    scale_fill_manual(na.value = NA, values =c('grey',my_color))+
    scale_x_continuous(expand = c(0,0)) +
    scale_y_discrete(expand = c(0,0), limits = rev) +
    facet_grid(id~., scales = "free_y", space = "free") +
    theme_bw() +
    theme(panel.grid = element_blank(),
          panel.border = element_rect(fill = NA),
          strip.background = element_blank(),
          axis.text.y = element_text(size = 25, color = "black"),
          axis.text.x = element_text(size = 25, color = "black"),
          strip.text.y = element_text(size = 25, angle = 0),
          plot.caption = element_text(size = 25, hjust = 0.5),
          legend.position = "none"
    ) +
    labs(x = NULL, y = NULL,title = NULL)
  
  return(p)
}

##further SI functions (section 9)

detected_vs_missing <- function(oecd_grouped, out,sector,ISO = NA,introductions_only = FALSE){
  
  all_sector_policies = oecd_grouped[oecd_grouped$Module==sector,]
  
  if(introductions_only == TRUE){
    #in this case we only want to keep introductions and add ons 
    add_ons = all_sector_policies[all_sector_policies$source == 'add-on', ]
    all_sector_policies = all_sector_policies[all_sector_policies$introduction == 1,]
    all_sector_policies = all_sector_policies[!is.na(all_sector_policies$ISO),]
    
    all_sector_policies = rbind(all_sector_policies,add_ons)
  }
  if(is.na(ISO)==FALSE){
    all_sector_policies = all_sector_policies[all_sector_policies$ISO == ISO, ]
    out = out[out$ISO == ISO,]
  }
  
  #filter slowly increasing policies from overall policy data frame to avoid double counting 
  
  oecd_grouped = oecd_grouped[!oecd_grouped$label == 'slow_increase',]
  
  #at the breaks, check if a policy that has a slow_increase label has been matched twice. If so, only keep one.
  out$drop = 0
  for(i in 1:nrow(out)){
    if(out$label[i] == "slow_increase"){
      break_count = out[out$unique_break_identifier == out$unique_break_identifier[i] & out$Policy == out$Policy[i] ,]
      print(break_count)
      if(nrow(break_count)>1){
        out$drop[i] = 1
      }
    }
  }
  out = out[out$drop==0,]
  
  policy_count_overall = plyr::count(all_sector_policies$Policy_name_fig_4)
  policy_count_overall = policy_count_overall %>% rename(overall_count = freq)
  
  policy_count_matched = plyr::count(out$Policy_name_fig_4) 
  policy_count_matched = policy_count_matched %>% rename(detected = freq)
  
  policy_count_merged = merge(policy_count_overall,policy_count_matched,by='x',all.x=TRUE)
  policy_count_merged$detected[is.na(policy_count_merged$detected)] = 0
  
  policy_count_merged$undetected = policy_count_merged$overall_count - policy_count_merged$detected
  
  policy_count_merged$detected_percent = (policy_count_merged$detected*100)/policy_count_merged$overall_count
  
  # #add the maximum stringency 
  # max_string <- all_sector_policies %>%
  #   group_by(Broad_category) %>%
  #   filter(Value == max(Value)) %>%
  #   ungroup()
  # max_string <- max_string[c('Broad_category','Value')]
  # max_string <- max_string[!duplicated(max_string),]
  # 
  # policy_count_merged<-merge(policy_count_merged,max_string,all.x=TRUE,by.x='x',by.y='Policy_name')
  # 
  policy_count_merged$sector = sector 
  
  return(policy_count_merged)
}

##formats matrix labels

add_newline <- function(string) {
  if (nchar(string) > 10) {
    # Find the middle white space
    whitespace_positions <- gregexpr("\\s", string)[[1]]
    middle_whitespace_position <- whitespace_positions[length(whitespace_positions) / 2 + 1]
    
    # Split the string into two parts at the middle whitespace position
    string_parts <- strsplit(string, "")[[1]]
    string_parts[middle_whitespace_position] <- "\n"
    
    # Combine the string parts back into a single string
    string <- paste(string_parts, collapse = "")
  }
  
  return(string)
}


##make matrix into properly ordered df for plotting in Fig. 4

prep_matrix <- function(x){
  
  x_df <- data.frame(
    row = factor(rep(seq(nrow(
      x
    )), times = ncol(x)), levels = seq(nrow(x), 1)),
    col = factor(rep(seq(ncol(
      x
    )), each = nrow(x)), levels = seq(ncol(x))),
    x = as.vector(x)
  )
  if (!is.null(rownames(x)))
    levels(x_df[["row"]]) <- rev(rownames(x))
  if (!is.null(colnames(x)))
    levels(x_df[["col"]]) <- colnames(x)
  
  return(x_df)
}


##gsynth functions 

############# EDITED PLOT FUNCTION FOR CF FROM GSYNTH SOURCE CODE #############

#######################################################
## METHODS
#######################################################

##########
## Plot
##########
#x a gsynth object
# type of the plot; axes limits; axes labels; 
# show raw data in "counterfactual" mode # ("none","band","all")
# main: whether to show the title;
# nfactors: whose loadings to be plotted 
# id: individual plot
plot.gsynth <- function(
    x,  
    type = "gap", 
    xlim = NULL, 
    ylim = NULL,
    xlab = NULL, 
    ylab = NULL,
    legendOff = FALSE,
    raw = "none", 
    main = NULL,
    nfactors = NULL, 
    id = NULL,
    axis.adjust = FALSE,
    theme.bw = TRUE,
    shade.post = FALSE,
    ...){
  
  
  ##-------------------------------##
  ## Checking Parameters
  ##-------------------------------##  
  
  outcome <- NULL
  ATT <- NULL
  CI.lower <- NULL
  CI.upper <- NULL
  co5 <- NULL
  co95 <- NULL
  tr5 <- NULL
  tr95 <- NULL
  group <- NULL
  L1 <- NULL
  out <- NULL
  
  scaleFUN <- function(x) sprintf("%.f", x) ## integer value at x axis
  
  if (class(x)!="gsynth") {
    stop("Not a \"gsynth\" object.")
  }
  if (!type %in% c("gap","counterfactual","ct","factors","missing","loadings","raw")) {
    stop("\"type\" option misspecified.")        
  }
  if (type == "ct") {
    type <- "counterfactual"
  }
  if (is.null(x$factor) & type == "factors") {
    stop("No factors to be plotted.")
  }
  if (is.null(x$lambda.tr) & type == "factors") {
    stop("No loadings to be plotted.")
  }
  
  if (is.null(xlim)==FALSE) {
    if (is.numeric(xlim)==FALSE) {
      stop("Some element in \"xlim\" is not numeric.")
    } else {
      if (length(xlim)!=2) {
        stop("xlim must be of length 2.")
      }
    }
  }
  if (is.null(ylim)==FALSE) {
    ## if (type!="missing") {
    if (is.numeric(ylim)==FALSE) {
      stop("Some element in \"ylim\" is not numeric.")
    } else {
      if (length(ylim)!=2) {
        stop("ylim must be of length 2.")
      }
    }
    ## } else {
    ##     m.l <- length(ylim)
    ##     for (i in 1:m.l) {
    ##         if (!ylim[m.l]%in%x$id) {
    ##             stop("Some specified units are not in the data.")
    ##         }
    ##     }
    ## }
  }
  
  if (is.null(xlab)==FALSE) {
    if (is.character(xlab) == FALSE) {
      stop("\"xlab\" is not a string.")
    } else {
      xlab <- xlab[1]
    }   
  }
  if (is.null(ylab)==FALSE) {
    if (is.character(ylab) == FALSE) {
      stop("\"ylab\" is not a string.")
    } else {
      ylab <- ylab[1]
    }   
  }
  if (is.logical(legendOff) == FALSE & is.numeric(legendOff)==FALSE) {
    stop("\"legendOff\" is not a logical flag.")
  }
  if (type == "counterfactual") {
    if (! raw %in% c("none","band","all")) {
      cat("\"raw\" option misspecifed. Reset to \"none\".")
      raw <- "none" 
    }
    if (is.null(id)==FALSE) {
      if (length(id)>1) {
        stop("More than 1 element in \"id\".") 
      }
    } 
  }
  if (is.null(main)==FALSE) {
    if (is.character(main) == FALSE) {
      stop("\"main\" is not a string.")
    } else {
      main <- main[1]
    }   
  }
  if (is.null(nfactors)==FALSE) {
    if (is.numeric(nfactors)==FALSE) {
      stop("\"nfactors\" is not a positive integer.")
    } else {
      nfactors <- nfactors[1]
      if (nfactors%%1!=0 | nfactors<=0) {
        stop("\"nfactors\" is not a positive integer.")
      }  
    } 
  }
  
  if (axis.adjust==TRUE) {
    angle <- 45
    x.v <- 1
    x.h <- 1
  } else {
    angle <- 0
    x.v <- 0
    if (type=="missing") {
      x.h <- 0.5
    } else {
      x.h <- 0
    }
  }
  
  ##-------------------------------##
  ## Plotting
  ##-------------------------------##  
  
  I.tr <- x$I.tr
  D.tr <- x$D.tr
  Y.tr <- x$Y.tr
  Y.co <- x$Y.co
  Y.ct <- x$Y.ct
  tb <- x$est.att
  Yb <- x$Y.bar[,1:2] ## treated average and counterfactual average
  tr <- x$tr
  pre <- x$pre
  post <- x$post
  # I.tr <- x$I.tr
  TT <- x$T
  T0 <- x$T0 ## notice
  p <- x$p
  ## m <- x$m
  Ntr <- x$Ntr
  Nco <- x$Nco
  N <- x$N 
  force <- x$force
  F.hat <- x$factor
  L.tr <- x$lambda.tr
  
  ## time.label <- x$time
  ## T.b <- 1:TT
  if (!is.null(L.tr)) {
    r <- dim(L.tr)[2]
  } else {
    r <- 0
  }
  
  if (type!="missing") {
    if (is.null(id)==TRUE) {
      id <- x$id.tr
    }
  } else {
    if (is.null(id)==TRUE) {
      id <- colnames(x$obs.missing)
    }
    m.l <- length(id)
    for (i in 1:m.l) {
      if (!id[i]%in%colnames(x$obs.missing)) {
        stop("Some specified units are not in the data.")
      }
    }
  }
  
  ## parameters
  line.width <- c(1.2,0.5)
  
  ## color of axes
  if (theme.bw == TRUE) {
    line.color <- "#AAAAAA70"
  } else {
    line.color <- "white"
  }
  
  ## shade in the post-treatment period
  if (is.null(shade.post) == TRUE) {
    if (type %in% c("raw","counterfactual")) {
      shade.post <- TRUE
    }
    if (type %in% c("gap","factors")) {
      shade.post <- FALSE
    }    
  } else {
    if (!class(shade.post) %in% c("logical","numeric")) {
      stop("Wrong type for option \"shade.post\"")
    }
  }
  
  ## type of plots
  if (type == "raw"| type == "counterfactual" | 
      type == "factors" |  length(id) == 1 | type =="missing" | 
      type=="loadings") {
    time <- x$time
    if (!is.numeric(time[1])) {
      time <- 1:TT
    }
    
    if (type!="missing") {
      if (length(id) == 1) {
        time.bf <- time[T0[which(id == x$id.tr)]]
      } else {
        time.bf <- time[unique(T0)]
      }
    }
    
    ## periods to show
    if (length(xlim) != 0) {
      ## if(is.numeric(time[1])){
      show <- which(time>=xlim[1]& time<=xlim[2])
      ## } else {
      ##     xlim[1] <- which(x$time>=xlim[1])[1]
      ##     xlim[2] <- which(x$time<=xlim[2])[length(which(x$time<=xlim[2]))]
      ##     show <- which(time>=xlim[1]& time<=xlim[2])
      
      ## }
    } else {
      show <- 1:length(time)
    }     
  }
  
  if (type == "gap")  { ## variable treatment timing
    time <- c(1:TT) - min(T0)
    time.bf <- 0 ## before treatment
    
    if (length(xlim) != 0) {
      show <- which(time>=xlim[1]& time<=xlim[2])     
    } else {
      show <- 1:length(time)    
    }
  }
  
  nT <- length(show)
  time.label <- x$time[show]
  
  ## if (axis.adjust==FALSE) {
  ##     n.period <- length(show)
  ## } else {
  ##     n.period <- length(show) ## min(length(show),20)
  ## }
  
  ## if (axis.adjust==TRUE) {
  ##     n.period <- n.period - 1
  ##     T.n <- (nT-1)%/%n.period
  ##     T.res <- (nT-1)%%n.period
  ##     T.b <- seq(from=1,to=T.n*n.period+1,by=T.n)
  ##     if (T.res!=0) {
  ##         T.j <- 1
  ##         for(i in (n.period-T.res+2):(n.period+1)) {
  ##             T.b[i] <- T.b[i] + T.j
  ##             T.j <- T.j + 1
  ##         }
  ##     }
  ## T.b <- show[T.b]
  ## } else {
  T.b <- 1:length(show)
  ## }
  
  
  ## legend on/off
  if (legendOff == TRUE) {
    legend.pos <- "none"
  } else {
    legend.pos <- "bottom"
  }
  
  ############  START  ###############
  
  if (type == "raw") {
    ## axes labels
    if (is.null(xlab)==TRUE) {
      xlab <- x$index[2]
    } else if (xlab == "") {
      xlab <- NULL
    }
    if (is.null(ylab)==TRUE) {
      ylab <- x$Yname
    } else if (ylab == "") {
      ylab <- NULL
    }
    
    pst <- D.tr
    for (i in 1:Ntr){
      pst[T0[i],i] <- 1 ## paint the period right before treatment
    }
    time.pst <- c(pst[show,] * time[show])
    time.pst <- time.pst[which(c(pst[show,])==1)]
    Y.tr.pst <- c(Y.tr[show,])[which(pst[show,]==1)]
    id.tr.pst <- matrix(rep(1:Ntr,each=TT),TT,Ntr,byrow=FALSE)[show,]
    id.tr.pst <- c(id.tr.pst)[which(pst[show,]==1)]
    
    data <- cbind.data.frame("time" = c(rep(time[show], N), time.pst),
                             "outcome" = c(c(Y.tr[show,]),
                                           c(Y.co[show,]),
                                           Y.tr.pst),
                             "type" = c(rep("tr",(Ntr*nT)),
                                        rep("co",(Nco*nT)),
                                        rep("tr.pst",length(Y.tr.pst))),
                             "id" = c(rep(1:N,each = nT), id.tr.pst*(-1)))
    
    ## theme
    p <- ggplot(data) 
    if (theme.bw == TRUE) {
      p <- p + theme_bw()
    }
    ## labels and legend
    p <- p + xlab(xlab) +  ylab(ylab) +
      theme(legend.position = legend.pos,
            axis.text.x = element_text(angle = angle, hjust=x.h, vjust=x.h),
            plot.title = element_text(size=20,
                                      hjust = 0.5,
                                      face="bold",
                                      margin = margin(10, 0, 10, 0)))        
    
    if (x$sameT0==TRUE) {
      p <- p + geom_vline(xintercept=time.bf,colour=line.color,size = 2) 
      if (shade.post == TRUE) {
        p <- p + annotate("rect", xmin= time.bf, xmax= Inf,ymin=-Inf, ymax=Inf, alpha = .3) 
      }  
    }
    
    ## main
    p <- p + geom_line(aes(time, outcome,
                           colour = type,
                           size = type,
                           linetype = type,
                           group = id))
    
    ## legend
    set.limits = c("tr","tr.pst","co")
    set.labels = c("Treated (Pre)",
                   "Treated (Post)",
                   "Controls")
    if (theme.bw == FALSE) {
      set.colors = c("#FC8D6280","red","#99999950")
    } else {
      set.colors = c("#4671D565","#06266F","#5E5E5E50")
    }
    set.linetypes = c("solid","solid","solid")
    set.linewidth = c(0.5, 0.5, 0.5)
    
    p <- p + scale_colour_manual(limits = set.limits,
                                 labels = set.labels,
                                 values =set.colors) +
      scale_linetype_manual(limits = set.limits,
                            labels = set.labels,
                            values = set.linetypes) +
      scale_size_manual(limits = set.limits,
                        labels = set.labels,
                        values = set.linewidth) +
      guides(linetype = guide_legend(title=NULL, ncol=3),
             colour = guide_legend(title=NULL, ncol=3),
             size = guide_legend(title=NULL, ncol=3)) 
    
    if (!is.numeric(time.label)) {
      p <- p + 
        scale_x_continuous(expand = c(0, 0), breaks = show[T.b], labels = time.label[T.b])
    } else {
      p <- p + scale_x_continuous(labels=scaleFUN)
    }
    
    ## title
    if (is.null(main) == TRUE) {
      p <- p + ggtitle("Raw Data")
    } else if (main!="") {
      p <- p + ggtitle(main)
    }
    
    ## ylim
    if (is.null(ylim) == FALSE) {
      p <- p + coord_cartesian(ylim = ylim)
    }
    
    
    suppressWarnings(print(p))
    
  } else if (type == "gap") { 
    
    if (length(id) == 1 & !(id[1] %in% x$id.tr)) { ## error
      stop(paste(id,"not in the treatment group"))
    } else { ## no error
      
      ## axes labels
      if (is.null(xlab) == TRUE) {
        if (x$sameT0 == TRUE) {
          xlab <- x$index[2]
        } else {
          xlab <- paste("Time relative to Treatment")
        }
      } else if (xlab == "") {
        xlab <- NULL
      }
      if (is.null(ylab) == TRUE) {
        ylab <- "Coefficient"
      } else if (ylab == "") {
        ylab <- NULL
      }
      
      ## title
      if (length(id) == 1 && !is.null(x$est.ind)) { ## id specified
        maintext <- paste(x$index[1],"=",id) 
      }  else {
        maintext <- "Estimated ATT"
      } 
      
      ## contruct data for plotting
      if (is.null(x$est.att)==TRUE) { 
        cat("Uncertainty estimates not available.\n")
        if (length(id) == 1) { ## id specified
          data <- cbind.data.frame(time, x$eff)[show,]
          colnames(data) <- c("time","ATT")
        } else {
          data <- cbind.data.frame(time, ATT = x$att)[show,] 
        } 
      } else {
        if (length(id) == 1 && !is.null(x$est.ind)) { ## id specified
          id <- which(x$id.tr == id)
          tb <- x$est.ind[,,id]
          time.bf <- time[T0[id]] 
          time <- time - time.bf
          time.bf <- 0
          if (!is.null(tb)) {
            colnames(tb) <- c("ATT", "S.E.", "CI.lower", "CI.upper","p.value")
          } else {
            tb <- as.matrix(x$eff[,id])
            colnames(tb) <- "ATT" 
          } 
        } 
        data <- cbind.data.frame(time, tb)[show,]
      }
      
      ### plotting
      p <- ggplot(data) 
      ## black/white theme
      if (theme.bw == TRUE) {
        p <- p + theme_bw()
      }
      p <- p + geom_vline(xintercept = time.bf, colour=line.color,size = 2) +
        geom_hline(yintercept = 0, colour=line.color,size = 2) +
        xlab(xlab) +  ylab(ylab) +
        theme(legend.position = legend.pos,
              plot.title = element_text(size=20,
                                        hjust = 0.5,
                                        face="bold",
                                        margin = margin(10, 0, 10, 0)))
      if (shade.post == TRUE) {
        p <- p + annotate("rect", xmin= time.bf, xmax= Inf,ymin=-Inf, ymax=Inf, alpha = .3) 
      }  
      
      
      ## point estimates
      p <- p + geom_line(aes(time, ATT), size = 1.2)
      
      ## confidence intervals
      if (is.null(x$est.att)==FALSE || !(is.null(x$est.ind)&length(id) == 1)) {
        p <- p + geom_ribbon(aes(x = time, ymin=CI.lower, ymax=CI.upper),alpha=0.2)
      }
      
      ## title
      if (is.null(main) == TRUE) {
        p <- p + ggtitle(maintext)
      } else if (main!=""){
        p <- p + ggtitle(main)
      }
      
      ## ylim
      if (is.null(ylim) == FALSE) {
        p <- p + coord_cartesian(ylim = ylim)
      }            
      
      suppressWarnings(print(p))
    }  ## end of "gap" (in case of no id error)
    
    
  } else if (type=="counterfactual") { 
    
    if (length(id) == 1|length(x$id.tr) == 1|x$sameT0==TRUE) { 
      if (length(id)==1 & !(id[1]%in%x$id.tr)) { ## error
        
        cat(paste(id,"not in the treatment group"))
        
      } else { ## one treated unit case
        
        ## axes labels
        if (is.null(xlab)==TRUE) {
          xlab <- x$index[2]
        } else if (xlab == "") {
          xlab <- NULL
        }
        if (is.null(ylab)==TRUE) {
          ylab <- x$Yname
        } else if (ylab == "") {
          ylab <- NULL
        }
        
        if (length(id) == 1 | length(x$id.tr) == 1) { ## one treated unit
          
          if (is.null(id) == TRUE) {
            id <- x$id.tr
          }
          maintext <- paste("Treated and Counterfactual (",id,")",sep="") 
          tr.info <- Y.tr[,which(id==x$id.tr)]
          ct.info <- Y.ct[,which(id==x$id.tr)] 
          if (raw == "none") { 
            data <- cbind.data.frame("time" = rep(time[show],2),
                                     "outcome" = c(tr.info[show],
                                                   ct.info[show]),
                                     "type" = c(rep("tr",nT),
                                                rep("ct",nT)))
            ## theme
            p <- ggplot(data) 
            if (theme.bw == TRUE) {
              p <- p + theme_bw()
            }
            p <- p + xlab(xlab) +  ylab(ylab) +
              geom_vline(xintercept=time.bf,colour=line.color,size = 2) +
              theme(legend.position = legend.pos,
                    axis.text.x = element_text(angle = angle, hjust=x.h, vjust=x.v),
                    plot.title = element_text(size=20,
                                              hjust = 0.5,
                                              face="bold",
                                              margin = margin(10, 0, 10, 0))) 
            if (shade.post == TRUE) {
              p <- p + annotate("rect", xmin= time.bf, xmax= Inf,ymin=-Inf, ymax=Inf, alpha = .3) 
            }        
            
            ## main
            p <- p + geom_line(aes(time, outcome,
                                   colour = type,
                                   size = type,
                                   linetype = type)) 
            ## legend
            set.limits = c("tr","ct")
            set.labels = c("Observed emissions", "Counterfactual emissions")
            set.colors = c("black","steelblue")
            set.linetypes = c("solid","longdash")
            set.linewidth = rep(line.width[1],2)
            p <- p + scale_colour_manual(limits = set.limits,
                                         labels = set.labels,
                                         values =set.colors) +
              scale_linetype_manual(limits = set.limits,
                                    labels = set.labels,
                                    values = set.linetypes) +
              scale_size_manual(limits = set.limits,
                                labels = set.labels,
                                values = set.linewidth) +
              guides(linetype = guide_legend(title=NULL, ncol=2),
                     colour = guide_legend(title=NULL, ncol=2),
                     size = guide_legend(title=NULL, ncol=2))
            
            if (!is.numeric(time.label)) {
              p <- p + 
                scale_x_continuous(expand = c(0, 0), breaks = show[T.b], labels = time.label[T.b])
            } else {
              p <- p + scale_x_continuous(labels=scaleFUN)
            }
            
            
          } else if  (raw == "band") {
            
            Y.co.90 <- t(apply(Y.co, 1, quantile, prob=c(0.05,0.95), na.rm = TRUE)) 
            data <- cbind.data.frame("time" = rep(time[show],2),
                                     "outcome" = c(tr.info[show],
                                                   ct.info[show]),
                                     "type" = c(rep("tr",nT),
                                                rep("ct",nT)))
            
            data.band <- cbind.data.frame(time, Y.co.90)[show,]
            colnames(data.band) <- c("time","co5","co95")
            
            
            ## theme 
            p <- ggplot(data) 
            if (theme.bw == TRUE) {
              p <- p + theme_bw()
            }
            p <- p + xlab(xlab) +  ylab(ylab) +
              geom_vline(xintercept=time.bf,colour=line.color,size = 2) +
              theme(legend.position = legend.pos,
                    axis.text.x = element_text(angle = angle, hjust=x.h, vjust=x.v),
                    plot.title = element_text(size=20,
                                              hjust = 0.5,
                                              face="bold",
                                              margin = margin(10, 0, 10, 0)))
            if (shade.post == TRUE) {
              p <- p + annotate("rect", xmin= time.bf, xmax= Inf,ymin=-Inf, ymax=Inf, alpha = .3) 
            }      
            
            ## main
            p <- p + geom_line(aes(time, outcome,
                                   colour = type,
                                   size = type,
                                   linetype = type))
            
            ## band
            p <- p + geom_ribbon(data = data.band,
                                 aes(ymin = co5, ymax = co95, x=time),
                                 alpha = 0.15, fill = "steelblue")
            
            set.limits = c("tr","co.band","ct")
            set.labels = c("Treated", "Controls (5-95% Quantiles)",
                           "Estimated Y(0)")
            set.colors = c("black","#4682B480","steelblue")
            set.linetypes = c("solid","solid","longdash")
            set.linewidth = c(line.width[1],4,line.width[1])
            
            p <- p + scale_colour_manual(limits = set.limits,
                                         labels = set.labels,
                                         values =set.colors) +
              scale_linetype_manual(limits = set.limits,
                                    labels = set.labels,
                                    values = set.linetypes) +
              scale_size_manual(limits = set.limits,
                                labels = set.labels,
                                values = set.linewidth) +
              guides(linetype = guide_legend(title=NULL, ncol=3),
                     colour = guide_legend(title=NULL, ncol=3),
                     size = guide_legend(title=NULL, ncol=3)) 
            
            if (!is.numeric(time.label)) {
              p <- p + 
                scale_x_continuous(expand = c(0, 0), breaks = show[T.b], labels = time.label[T.b])
            } else {
              p <- p + scale_x_continuous(labels=scaleFUN)
            }
            
          } else if (raw == "all") { ## plot all the raw data
            
            data <- cbind.data.frame("time" = rep(time[show],(2 + Nco)),
                                     "outcome" = c(tr.info[show],
                                                   ct.info[show],
                                                   c(Y.co[show,])),
                                     "type" = c(rep("tr",nT),
                                                rep("ct",nT),
                                                rep("raw.co",(Nco * nT))),
                                     "id" = c(rep("tr",nT),
                                              rep("ct",nT),
                                              rep(c(x$id.co), each = nT)))
            
            ## theme
            p <- ggplot(data) 
            if (theme.bw == TRUE) {
              p <- p + theme_bw()
            }
            p <- p + xlab(xlab) +  ylab(ylab) +
              geom_vline(xintercept=time.bf,colour=line.color,size = 2) +
              theme(legend.position = legend.pos,
                    axis.text.x = element_text(angle = angle, hjust=x.h, vjust=x.v),
                    plot.title = element_text(size=20,
                                              hjust = 0.5,
                                              face="bold",
                                              margin = margin(10, 0, 10, 0)))
            if (shade.post == TRUE) {
              p <- p + annotate("rect", xmin= time.bf, xmax= Inf,ymin=-Inf, ymax=Inf, alpha = .3) 
            }  
            ## main
            p <- p + geom_line(aes(time, outcome,
                                   colour = type,
                                   size = type,
                                   linetype = type,
                                   group = id))
            
            ## legend
            set.limits = c("tr","raw.co","ct")
            set.labels = c("Treated","Controls","Estimated Y(0)")
            set.colors = c("black","#4682B420","steelblue")
            set.linetypes = c("solid","solid","longdash")
            set.linewidth = c(line.width[1],line.width[2],line.width[1])
            
            p <- p + scale_colour_manual(limits = set.limits,
                                         labels = set.labels,
                                         values =set.colors) +
              scale_linetype_manual(limits = set.limits,
                                    labels = set.labels,
                                    values = set.linetypes) +
              scale_size_manual(limits = set.limits,
                                labels = set.labels,
                                values = set.linewidth) +
              guides(linetype = guide_legend(title=NULL, ncol=3),
                     colour = guide_legend(title=NULL, ncol=3),
                     size = guide_legend(title=NULL, ncol=3)) 
            
            if (!is.numeric(time.label)) {
              p <- p + 
                scale_x_continuous(expand = c(0, 0), breaks = show[T.b], labels = time.label[T.b])
            } else {
              p <- p + scale_x_continuous(labels=scaleFUN)
            }                       
            
          } 
          
        } else { # begin multiple treated unit case
          maintext <- "Treated and Counterfactual Averages"
          if (raw == "none") {
            data <- cbind.data.frame("time" = rep(time[show],2),
                                     "outcome" = c(Yb[show,1],
                                                   Yb[show,2]),
                                     "type" = c(rep("tr",nT),
                                                rep("co",nT))) 
            ## theme
            p <- ggplot(data) 
            if (theme.bw == TRUE) {
              p <- p + theme_bw()
            }
            p <- p + xlab(xlab) +  ylab(ylab) +
              geom_vline(xintercept=time.bf,colour=line.color,size = 2) +
              theme(legend.position = legend.pos,
                    axis.text.x = element_text(angle = angle, hjust=x.h, vjust=x.v),
                    plot.title = element_text(size=20,
                                              hjust = 0.5,
                                              face="bold",
                                              margin = margin(10, 0, 10, 0)))
            if (shade.post == TRUE) {
              p <- p + annotate("rect", xmin= time.bf, xmax= Inf,ymin=-Inf, ymax=Inf, alpha = .3) 
            }      
            ## main
            p <- p + geom_line(aes(time, outcome,
                                   colour = type,
                                   size = type,
                                   linetype = type))
            
            ## legend
            set.limits = c("tr","co")
            set.labels = c("Treated Average",
                           "Estimated Y(0) Average")
            set.colors = c("black","steelblue")
            set.linetypes = c("solid","longdash")
            set.linewidth = rep(line.width[1],2)
            p <- p + scale_colour_manual(limits = set.limits,
                                         labels = set.labels,
                                         values =set.colors) +
              scale_linetype_manual(limits = set.limits,
                                    labels = set.labels,
                                    values = set.linetypes) +
              scale_size_manual(limits = set.limits,
                                labels = set.labels,
                                values = set.linewidth) +
              guides(linetype = guide_legend(title=NULL, ncol=2),
                     colour = guide_legend(title=NULL, ncol=2),
                     size = guide_legend(title=NULL, ncol=2)) 
            
            if (!is.numeric(time.label)) {
              p <- p + 
                scale_x_continuous(expand = c(0, 0), breaks = show[T.b], labels = time.label[T.b])
            } else {
              p <- p + scale_x_continuous(labels=scaleFUN)
            }
            
          } else if  (raw == "band") {
            
            Y.tr.90 <- t(apply(Y.tr, 1, quantile, prob=c(0.05,0.95),na.rm=TRUE))
            Y.co.90 <- t(apply(Y.co, 1, quantile, prob=c(0.05,0.95),na.rm=TRUE))
            
            data <- cbind.data.frame("time" = rep(time[show],2),
                                     "outcome" = c(Yb[show,1],
                                                   Yb[show,2]),
                                     "type" = c(rep("tr",nT),
                                                rep("co",nT)))
            
            data.band <- cbind.data.frame(time, Y.tr.90, Y.co.90)[show,]
            colnames(data.band) <- c("time","tr5","tr95","co5","co95")
            
            ## theme 
            p <- ggplot(data) 
            if (theme.bw == TRUE) {
              p <- p + theme_bw()
            }
            p <- p + xlab(xlab) +  ylab(ylab) +
              geom_vline(xintercept=time.bf,colour=line.color,size = 2) +
              theme(legend.position = legend.pos,
                    axis.text.x = element_text(angle = angle, hjust=x.h, vjust=x.v),
                    plot.title = element_text(size=20,
                                              hjust = 0.5,
                                              face="bold",
                                              margin = margin(10, 0, 10, 0)))
            if (shade.post == TRUE) {
              p <- p + annotate("rect", xmin= time.bf, xmax= Inf, ymin=-Inf, ymax=Inf, alpha = .3) 
            }      
            ## main
            p <- p + geom_line(aes(time, outcome,
                                   colour = type,
                                   size = type,
                                   linetype = type))
            ## band
            p <- p + geom_ribbon(data = data.band,
                                 aes(ymin = tr5, ymax = tr95, x=time),
                                 alpha = 0.15, fill = "black") +
              geom_ribbon(data = data.band,
                          aes(ymin = co5, ymax = co95, x=time),
                          alpha = 0.15, fill = "steelblue")
            
            set.limits = c("tr","co","tr.band","co.band")
            set.labels = c("Treated Average",
                           "Estimated Y(0) Average",
                           "Treated 5-95% Quantiles",
                           "Controls 5-95% Quantiles")
            set.colors = c("black","steelblue","#77777750","#4682B480")
            set.linetypes = c("solid","longdash","solid","solid")
            set.linewidth = c(rep(line.width[1],2),4,4)
            
            p <- p + scale_colour_manual(limits = set.limits,
                                         labels = set.labels,
                                         values =set.colors) +
              scale_linetype_manual(limits = set.limits,
                                    labels = set.labels,
                                    values = set.linetypes) +
              scale_size_manual(limits = set.limits,
                                labels = set.labels,
                                values = set.linewidth) +
              guides(linetype = guide_legend(title=NULL, ncol=2),
                     colour = guide_legend(title=NULL, ncol=2),
                     size = guide_legend(title=NULL, ncol=2)) 
            
            if (!is.numeric(time.label)) {
              p <- p + 
                scale_x_continuous(expand = c(0, 0), breaks = show[T.b], labels = time.label[T.b])
            } else {
              p <- p + scale_x_continuous(labels=scaleFUN)
            }
            
          } else if (raw == "all") { ## plot all the raw data
            
            data <- cbind.data.frame("time" = rep(time[show],(2 + N)),
                                     "outcome" = c(Yb[show,1],
                                                   Yb[show,2],
                                                   c(Y.tr[show,]),
                                                   c(Y.co[show,])),
                                     "type" = c(rep("tr",nT),
                                                rep("co",nT),
                                                rep("raw.tr",(Ntr * nT)),
                                                rep("raw.co",(Nco * nT))),
                                     "id" = c(rep("tr",nT),
                                              rep("co",nT),
                                              rep(c(x$id.tr,x$id.co),
                                                  each = nT))) 
            ## theme
            p <- ggplot(data) 
            if (theme.bw == TRUE) {
              p <- p + theme_bw()
            }
            p <- p + xlab(xlab) +  ylab(ylab) +
              geom_vline(xintercept=time.bf,colour=line.color,size = 2) +
              theme(legend.position = legend.pos,
                    axis.text.x = element_text(angle = angle, hjust=x.h, vjust=x.v),
                    plot.title = element_text(size=20,
                                              hjust = 0.5,
                                              face="bold",
                                              margin = margin(10, 0, 10, 0))) 
            if (shade.post == TRUE) {
              p <- p + annotate("rect", xmin= time.bf, xmax= Inf,ymin=-Inf, ymax=Inf, alpha = .3) 
            }      
            ## main
            p <- p + geom_line(aes(time, outcome,
                                   colour = type,
                                   size = type,
                                   linetype = type,
                                   group = id))
            ## legend
            set.limits = c("tr","co","raw.tr","raw.co")
            set.labels = c("Treated Average",
                           "Estimated Y(0) Average",
                           "Treated Raw Data",
                           "Controls Raw Data")
            set.colors = c("black","steelblue","#77777750","#4682B420")
            set.linetypes = c("solid","longdash","solid","solid")
            set.linewidth = rep(line.width,each=2)
            
            p <- p + scale_colour_manual(limits = set.limits,
                                         labels = set.labels,
                                         values =set.colors) +
              scale_linetype_manual(limits = set.limits,
                                    labels = set.labels,
                                    values = set.linetypes) +
              scale_size_manual(limits = set.limits,
                                labels = set.labels,
                                values = set.linewidth) +
              guides(linetype = guide_legend(title=NULL, ncol=2),
                     colour = guide_legend(title=NULL, ncol=2),
                     size = guide_legend(title=NULL, ncol=2)) 
            
            if (!is.numeric(time.label)) {
              p <- p + 
                scale_x_continuous(expand = c(0, 0), breaks = show[T.b], labels = time.label[T.b])
            } else {
              p <- p + scale_x_continuous(labels=scaleFUN)
            }
          }
          
        } # end multiple treated unit case
        
        ## title
        if (is.null(main) == TRUE) {
          p <- p + ggtitle(maintext)
        } else if (main!="") {
          p <- p + ggtitle(main)
        }
        
        ## ylim
        if (is.null(ylim) == FALSE) {
          p <- p + coord_cartesian(ylim = ylim)
        }                
        suppressWarnings(print(p))
      }
    } else {
      maintext <- "Treated and Counterfactual Averages"
      
      ## axes labels
      if (is.null(xlab)==TRUE) {
        xlab <- paste("Time relative to Treatment")
      } else if (xlab == "") {
        xlab <- NULL
      }
      if (is.null(ylab)==TRUE) {
        ylab <- x$Yname
      } else if (ylab == "") {
        ylab <- NULL
      }
      
      xx <- ct.adjsut(x$Y.tr, x$Y.ct, x$T0)
      
      time <- xx$timeline
      Yb <- xx$Yb
      Y.tr.aug <- xx$Y.tr.aug
      ## Y.ct.aug <- xx$Y.ct.aug
      time.bf <- 0 ## before treatment
      
      if (!is.null(xlim)) {
        show <- which(time>=xlim[1]& time<=xlim[2])
      } else {
        show <- 1:length(time)
      }
      nT <- length(show)
      
      if (raw == "none") {
        data <- cbind.data.frame("time" = rep(time[show],2),
                                 "outcome" = c(Yb[show,1],
                                               Yb[show,2]),
                                 "type" = c(rep("tr",nT),
                                            rep("co",nT))) 
        ## theme
        p <- ggplot(data) 
        if (theme.bw == TRUE) {
          p <- p + theme_bw()
        }
        p <- p  + xlab(xlab) +  ylab(ylab) +
          geom_vline(xintercept=time.bf,colour=line.color,size = 2) +
          theme(legend.position = legend.pos,
                axis.text.x = element_text(angle = angle, hjust=x.h, vjust=x.v),
                plot.title = element_text(size=20,
                                          hjust = 0.5,
                                          face="bold",
                                          margin = margin(10, 0, 10, 0)))
        if (shade.post == TRUE) {
          p <- p + annotate("rect", xmin= time.bf, xmax= Inf,ymin=-Inf, ymax=Inf, alpha = .3) 
        }      
        ## main
        p <- p + geom_line(aes(time, outcome,
                               colour = type,
                               size = type,
                               linetype = type))
        
        ## legend
        set.limits = c("tr","co")
        set.labels = c("Treated Average",
                       "Estimated Y(0) Average")
        set.colors = c("black","steelblue")
        set.linetypes = c("solid","longdash")
        set.linewidth = rep(line.width[1],2)
        p <- p + scale_colour_manual(limits = set.limits,
                                     labels = set.labels,
                                     values =set.colors) +
          scale_linetype_manual(limits = set.limits,
                                labels = set.labels,
                                values = set.linetypes) +
          scale_size_manual(limits = set.limits,
                            labels = set.labels,
                            values = set.linewidth) +
          guides(linetype = guide_legend(title=NULL, ncol=2),
                 colour = guide_legend(title=NULL, ncol=2),
                 size = guide_legend(title=NULL, ncol=2)) 
        
        if (!is.numeric(time.label)) {
          p <- p + 
            scale_x_continuous(expand = c(0, 0), breaks = show[T.b], labels = time.label[T.b])
        } else {
          p <- p + scale_x_continuous(labels=scaleFUN)
        }
        
      } else if  (raw == "band") {
        
        Y.tr.90 <- t(apply(Y.tr.aug, 1, quantile, prob=c(0.05,0.95),na.rm=TRUE))
        ## Y.co.90 <- t(apply(Y.co, 1, quantile, prob=c(0.05,0.95),na.rm=TRUE))
        
        data <- cbind.data.frame("time" = rep(time[show],2),
                                 "outcome" = c(Yb[show,1],
                                               Yb[show,2]),
                                 "type" = c(rep("tr",nT),
                                            rep("co",nT)))
        
        data.band <- cbind.data.frame(time, Y.tr.90)[show,]
        colnames(data.band) <- c("time","tr5","tr95")
        
        ## theme
        p <- ggplot(data) 
        if (theme.bw == TRUE) {
          p <- p + theme_bw()
        }
        p <- p  + xlab(xlab) +  ylab(ylab) +
          geom_vline(xintercept=time.bf,colour=line.color,size = 2) +
          theme(legend.position = legend.pos,
                axis.text.x = element_text(angle = angle, hjust=x.h, vjust=x.v),
                plot.title = element_text(size=20,
                                          hjust = 0.5,
                                          face="bold",
                                          margin = margin(10, 0, 10, 0)))
        if (shade.post == TRUE) {
          p <- p + annotate("rect", xmin= time.bf, xmax= Inf,ymin=-Inf, ymax=Inf, alpha = .3) 
        }      
        ## main
        p <- p + geom_line(aes(time, outcome,
                               colour = type,
                               size = type,
                               linetype = type))
        ## band
        p <- p + geom_ribbon(data = data.band,
                             aes(ymin = tr5, ymax = tr95, x=time),
                             alpha = 0.15, fill = "red")
        
        set.limits = c("tr","co","tr.band")
        set.labels = c("Treated Average",
                       "Estimated Y(0) Average",
                       "Treated 5-95% Quantiles")
        set.colors = c("black","steelblue","#77777750")
        set.linetypes = c("solid","longdash","solid")
        set.linewidth = c(rep(line.width[1],2),4)
        
        p <- p + scale_colour_manual(limits = set.limits,
                                     labels = set.labels,
                                     values =set.colors) +
          scale_linetype_manual(limits = set.limits,
                                labels = set.labels,
                                values = set.linetypes) +
          scale_size_manual(limits = set.limits,
                            labels = set.labels,
                            values = set.linewidth) +
          guides(linetype = guide_legend(title=NULL, ncol=2),
                 colour = guide_legend(title=NULL, ncol=2),
                 size = guide_legend(title=NULL, ncol=2))
        
        if (!is.numeric(time.label)) {
          p <- p + 
            scale_x_continuous(expand = c(0, 0), breaks = show[T.b], labels = time.label[T.b])
        } else {
          p <- p + scale_x_continuous(labels=scaleFUN)
        } 
        
      } else if (raw == "all") { ## plot all the raw data
        
        data <- cbind.data.frame("time" = rep(time[show],(2 + Ntr)),
                                 "outcome" = c(Yb[show,1],
                                               Yb[show,2],
                                               c(Y.tr.aug[show,])),
                                 "type" = c(rep("tr",nT),
                                            rep("co",nT),
                                            rep("raw.tr",(Ntr * nT))),
                                 "id" = c(rep("tr",nT),
                                          rep("co",nT),
                                          rep(c(x$id.tr),
                                              each = nT))) 
        ## theme
        p <- ggplot(data) 
        if (theme.bw == TRUE) {
          p <- p + theme_bw()
        }
        p <- p + xlab(xlab) +  ylab(ylab) +
          geom_vline(xintercept=time.bf,colour=line.color,size = 2) +
          theme(legend.position = legend.pos,
                axis.text.x = element_text(angle = angle, hjust=x.h, vjust=x.v),
                plot.title = element_text(size=20,
                                          hjust = 0.5,
                                          face="bold",
                                          margin = margin(10, 0, 10, 0))) 
        if (shade.post == TRUE) {
          p <- p + annotate("rect", xmin= time.bf, xmax= Inf,ymin=-Inf, ymax=Inf, alpha = .3) 
        }      
        ## main
        p <- p + geom_line(aes(time, outcome,
                               colour = type,
                               size = type,
                               linetype = type,
                               group = id))
        ## legend
        set.limits = c("tr","co","raw.tr")
        set.labels = c("Treated Average",
                       "Estimated Y(0) Average",
                       "Treated Raw Data")
        set.colors = c("black","steelblue","#77777750")
        set.linetypes = c("solid","longdash","solid")
        set.linewidth = c(rep(line.width[1],2),line.width[2])
        
        p <- p + scale_colour_manual(limits = set.limits,
                                     labels = set.labels,
                                     values =set.colors) +
          scale_linetype_manual(limits = set.limits,
                                labels = set.labels,
                                values = set.linetypes) +
          scale_size_manual(limits = set.limits,
                            labels = set.labels,
                            values = set.linewidth) +
          guides(linetype = guide_legend(title=NULL, ncol=2),
                 colour = guide_legend(title=NULL, ncol=2),
                 size = guide_legend(title=NULL, ncol=2)) 
        
        if (!is.numeric(time.label)) {
          p <- p + 
            scale_x_continuous(expand = c(0, 0), breaks = show[T.b], labels = time.label[T.b])
        } else {
          p <- p + scale_x_continuous(labels=scaleFUN)
        }
      }
      
      ## title
      if (is.null(main) == TRUE) {
        p <- p + ggtitle(maintext)
      } else if (main!="") {
        p <- p + ggtitle(main)
      }
      
      ## ylim
      if (is.null(ylim) == FALSE) {
        p <- p + coord_cartesian(ylim = ylim)
      }            
      suppressWarnings(print(p))
    }
    
  } else if (type=="factors") {
    
    if (x$r.cv==0) {
      cat("No factors included in the model.\n")
    } else {
      ## axes labels
      if (is.null(xlab)==TRUE) {
        xlab <- x$index[2]
      } else if (xlab == "") {
        xlab <- NULL
      }
      if (is.null(ylab)==TRUE) {
        ylab <- "Estimate"
      } else if (ylab == "") {
        ylab <- NULL
      }
      ## title
      if (is.null(main) == TRUE) {
        main <- "Latent Factors"
      } else if (main=="") {
        main <- NULL
      }
      ## prepare data
      L.co<-x$lambda.co
      norm<-sqrt(diag(t(L.co)%*%L.co)/(x$N-x$Ntr))
      data <- cbind.data.frame("time" = rep(time[show],r),
                               "factor" = c(F.hat[show,])*rep(norm,each=nT),
                               "group" = as.factor(c(rep(1:r,each=nT))))
      ## theme
      p <- ggplot(data) 
      if (theme.bw == TRUE) {
        p <- p + theme_bw()
      }
      p <- p + xlab(xlab) +  ylab(ylab) + ggtitle(main) +
        geom_hline(yintercept=0,colour=line.color,size = 2) +
        theme(legend.position = legend.pos,
              axis.text.x = element_text(angle = angle, hjust=x.h, vjust=x.v),
              plot.title = element_text(size=20,
                                        hjust = 0.5,
                                        face="bold",
                                        margin = margin(10, 0, 10, 0)))  
      ## main plot
      p <- p + geom_line(aes(time, factor,
                             colour = group,
                             group = group), size = 1.2)
      
      
      brew.colors <- c("black","steelblue","#8DD3C7","#FFFFB3","#BEBADA","#FB8072","#80B1D3","#FDB462","#B3DE69","#FCCDE5","#D9D9D9")
      set.colors = brew.colors[1:r]
      p <- p + scale_colour_manual(values =set.colors) 
      
      ## legend
      p <- p + guides(colour = guide_legend(title="Factor(s)", ncol=4)) 
      
      if (!is.numeric(time.label)) {
        p <- p + 
          scale_x_continuous(expand = c(0, 0), breaks = show[T.b], labels = time.label[T.b])
      }
      
      ## ylim
      if (is.null(ylim) == FALSE) {
        p <- p + coord_cartesian(ylim = ylim)
      }            
      suppressWarnings(print(p))
    }
    
  } else if (type=="loadings") {
    
    
    if (x$r.cv==0) {
      cat("No factors are included in the model.\n") 
    } else {
      ## number of loadings to be plotted
      if (is.null(nfactors)==TRUE) {
        nfactors<-min(x$r.cv,4) 
      } else if (nfactors>x$r.cv) {
        cat("Too many factors specified. ")
        nfactors<-min(x$r.cv,4) 
      }
      if (nfactors == 1) {
        cat("Loadings for the first factor are shown...\n")
      } else if (nfactors < x$r.cv) {
        cat(paste("Loadings for the first",nfactors,"factors are shown...\n"))
      }
      
      ## title
      if (is.null(main) == TRUE) {
        main <- "Factor Loadings"
      } else if (main=="") {
        main <- NULL
      }
      
      ## prepare data
      L.hat <- rbind(x$lambda.tr, x$lambda.co)
      Lname <- Llabel <- c()
      for (i in 1:r) {
        Lname<-c(Lname,paste("L",i,sep=""))
        Llabel<-c(Llabel,paste("Factor",i))
      }
      colnames(L.hat) <- Lname
      rownames(L.hat) <- c()
      data <- cbind.data.frame(L.hat,
                               "id"=c(x$id.tr, x$id.co),
                               "group"=as.factor(c(rep("Treated",Ntr),
                                                   rep("Control",Nco))))
      
      if (nfactors == 1) {
        p <- ggplot(data, aes(x=group, y=L1, fill = group)) +
          geom_boxplot(alpha = 0.7) +
          coord_flip() + guides(fill=FALSE) +
          xlab("") + ylab("Factor Loading")  
      } else {
        
        if (x$Ntr < 5) {
          my_dens <- function(data, mapping, ...) {
            ggplot(data = data, mapping = mapping) +
              geom_density(..., fill = "gray", alpha = 0.7, color = "gray50")
          }
          p <- GGally::ggpairs(data, mapping = aes(color = group),
                               columns = 1:nfactors,
                               columnLabels = Llabel[1:nfactors],
                               diag = list(continuous = my_dens),
                               title = main)
        } else {
          my_dens <- function(data, mapping, ...) {
            ggplot(data = data, mapping = mapping) +
              geom_density(..., alpha = 0.7, color = NA)
          }
          p <- GGally::ggpairs(data, mapping = aes(color = group, fill = group),
                               columns = 1:nfactors,
                               columnLabels = Llabel[1:nfactors],
                               diag = list(continuous = my_dens),
                               title = main) +
            theme(plot.title = element_text(hjust = 0.5))
        }
      }
      suppressWarnings(print(p))
    }
    
  } else if (type=="missing") {
    
    if (is.null(xlab)==TRUE) {
      xlab <- x$index[2]
    } else if (xlab == "") {
      xlab <- NULL
    }
    if (is.null(ylab)==TRUE) {
      ylab <- x$index[1]
    } else if (ylab == "") {
      ylab <- NULL
    }
    if (is.null(main)==TRUE) {
      main <- "Treatment Status"
    } else if (main == "") {
      main <- NULL
    }
    
    m <- x$obs.missing
    if (!is.null(id)) {
      m <- as.matrix(m[show,which(colnames(m)%in%id)])
    } else {
      m <- as.matrix(m[show,])
      ## ylim <- colnames(m)
    }
    
    all <- unique(c(m))
    col <- col2 <- breaks <- label <- NULL
    if (0%in%all) {
      col <- c(col,"#FFFFFF")
      col2 <- c(col2, "0"=NA)
      breaks <- c(breaks,0)
      label <- c(label,"Missing")
    }
    if (1%in%all) {
      col <- c(col,"#B0C4DE")
      col2 <- c(col2, "1"=NA)
      breaks <- c(breaks,1)
      label <- c(label,"Controls")
    }
    if (2%in%all) {
      col <- c(col,"#4671D5")
      col2 <- c(col2, "2"=NA)
      breaks <- c(breaks,2)
      label <- c(label,"Treated (Pre)")
    }
    if (3%in%all) {
      col <- c(col,"#06266F")
      col2 <- c(col2, "3"=NA)
      breaks <- c(breaks,3)
      label <- c(label,"Treated (Post)")
    }
    if (4%in%all) {
      col <- c(col,"#A9A9A9")
      col2 <- c(col2, "4"="red")
      breaks <- c(breaks,4)
      label <- c(label,"Treated (Removed)")
    }
    
    
    T <- dim(m)[1]
    N <- dim(m)[2]
    units <- rep(rev(1:N), each = T)
    period <- rep(1:T, N)
    res <- c(m)
    data <- cbind.data.frame(units=units, period=period, res=res)
    data[,"res"] <- as.factor(data[,"res"])
    
    N.b <- 1:N
    
    p <- ggplot(data, aes(x = period, y = units,
                          fill = res), position = "identity") 
    p <- p + geom_tile(colour="gray90", size=0.1, stat="identity") 
    
    p <- p +
      labs(x = xlab, y = ylab, 
           ## fill = "Value", 
           title=main) +
      theme_bw() + 
      scale_fill_manual(NA, breaks = breaks, values = col, labels=label)
    
    if(4%in%all) {
      p <- p + geom_point(aes(colour=res),size=0.5)
      p <- p + scale_color_manual(NA, breaks=breaks,
                                  values=col2, labels=label)
    }
    
    p <- p +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_rect(fill=NA,color="gray90", size=0.5, linetype="solid"),
            axis.line = element_blank(),
            axis.ticks = element_blank(),
            axis.text = element_text(color="black", size=14),
            axis.title=element_text(size=12),
            axis.text.x = element_text(size = 8, angle = angle, hjust=x.h, vjust=x.v),
            axis.text.y = element_text(size = 8),
            plot.background = element_rect(fill = "grey90"),
            legend.background = element_rect(fill = "grey90"),
            legend.position = legend.pos,
            legend.title=element_blank(),
            plot.title = element_text(size=20,
                                      hjust = 0.5,
                                      face="bold",
                                      margin = margin(10, 0, 10, 0))) +
      scale_x_continuous(expand = c(0, 0), breaks = T.b, labels = time.label[T.b]) +
      scale_y_continuous(expand = c(0, 0), breaks = N.b, labels = rev(sort(id)))
    
    if(length(all)>=4) {
      p <- p + guides(fill=guide_legend(nrow=2,byrow=TRUE))
    }
    suppressWarnings(print(p))
  }    
}

## counterfactual adjust
ct.adjsut <- function (Y.tr,
                       Y.ct, 
                       T0) {
  T <- dim(Y.tr)[1]
  N <- dim(Y.tr)[2]
  ## T.end <- T - min(T0)
  ## T.start <-
  T.m <- matrix(rep(1:T,N),T,N) - matrix(rep(T0,each=T),T,N)
  timeline <- min(T.m):max(T.m)
  Y.tr.aug <- matrix(NA,length(timeline),N)
  Y.ct.aug <- matrix(NA,length(timeline),N)
  for(i in 1:N) {
    Y.tr.aug[which(timeline%in%T.m[,i]),i] <- Y.tr[,i]
    Y.ct.aug[which(timeline%in%T.m[,i]),i] <- Y.ct[,i]
  }
  Y.tr.bar <- apply(Y.tr.aug, 1, mean, na.rm=TRUE)
  Y.ct.bar <- apply(Y.ct.aug, 1, mean, na.rm=TRUE)
  Yb <- cbind(Y.tr.bar,Y.ct.bar)
  return(list(timeline=timeline,
              Y.tr.aug=Y.tr.aug,
              Y.ct.aug=Y.ct.aug,
              Yb=Yb))
  
}


