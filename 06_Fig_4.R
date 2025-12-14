library(eulerr)
library("xtable")

setwd("F:\\Desktop\\科研项目\\1.负责科研项目\\Climate Policy DID Science-厦大_北师大\\Science")
source('00_oecd_project_functions.R')

#load raw policy data for computing detection shares

oecd_grouped = read.csv('OECD_data_preprocessed.csv')

#restrict to analysis period

oecd_grouped = oecd_grouped[oecd_grouped$year >1999,]
oecd_grouped = oecd_grouped[oecd_grouped$year <2021,]

#load matched policy + break data (filtered version for counting) 

policy_out_filtered <- readRDS("policy_out_filtered.RDS")

#list different confidence interval matching options
specs = c('policy_match','policy_match_2y','policy_match_3y')

#extract DFs
filtered_all <- sector_policy_match(policy_out_filtered, specs)

#we operate based on the 2y match in the main text 
filtered_all <- filtered_all[filtered_all$spec == 'policy_match_2y',]

##count out how many breaks are matched to more than 1 policy (to report in main text)

#notes: Merging them all together does not lead to the true count because unique break identifiers are not unique across sectors 
#total merged breaks: 66. Unmatched merged breaks: 6 (as only merged breaks have overlapping CIs), the 4 breaks with EU matches are not considered here either.

total_more_matches = 0
total_single_match = 0

for(i in 1:4){
  sector_match <- filtered_all$sector_policy_match[[i]]
  sector_count <- sector_match %>% group_by(unique_break_identifier) %>% count()
  
  total_more_matches = total_more_matches + nrow(sector_count[sector_count$n>1,])
  total_single_match = total_single_match + nrow(sector_count[sector_count$n<=1,])
  
  print(paste("for ", filtered_all$sector[i]," the number of breaks with at least 2 matches is ", nrow(sector_count[sector_count$n>1,]), sep = ''))
  print(paste("for ", filtered_all$sector[i]," the number of breaks with a single match is ", nrow(sector_count[sector_count$n<=1,]), sep = ''))
}

# total single match= 18, total_more_matches: 38 -> 38/56 = 0.678 -> ~68%. (without considering EU policies or unmatched breaks!)

#In addition, manually adjust the count where a break is matched with a policy + EU policy or 2 EU policies: 

#Additional breaks now matched by mix containing EU policy: Buildings Ireland 14-16; Slovakia 09-13; Slovakia 13-17; Industry BGR_2006_2010; -> +4 for mix, +4 for single
#Additoinal breaks now matched at all (by single EU policy): Buildings Czechia, Slovakia (2nd) Industry Romania, Czechia -> +4 for single

#net single stays the same. mix is +4. total breaks goes up by +4 to 60
#new total single match: 18
#new total more matches: 42
# 42/60 -> 70%

##by sector: 

#more than 2 before EU + missing: 
#Buildings: 9, Electricity:6, Industry: 6, Transport: 17
#single match before EU + missing: 
#Buildings: 9, Electricity: 3, Industry:5, Transport: 1 

#Buildings: 9+3 from EU mix; 9-3+2 for single mix. 12/20 (60% for Buildings without blank breaks)
#Transport: 17/18 -> 94% (without blank breaks)

#Electricity: 6/9 -> 67% (without blank breaks)

#Industry: 6+1,5-1+2 -> 7/13 -> 54% (without blank breaks)

### effect size bars (Fig. 4A)
sector_policy_match_2y = filtered_all$sector_policy_match[filtered_all$spec == "policy_match_2y"]

icon_links = c("Logos\\Buildings.png","Logos\\Electricity.png","Logos\\Industry.png","Logos\\Transport.png")

sector_colors = c("#eb5601","#e7c019","#bac36b","#3b9ab2")

my_mean_plots = list()

for(i in 1:4){
  
  #compute mean effect size (mix vs single policy)
  mean_df <- get_effect_size_means(sector_policy_match_2y[[i]])
  mean_df$Policy_name_fig_4[mean_df$Policy_name_fig_4=='Minimum energy performance standard'] = 'MEPS'
  
  ##get effect for pricig vs. no pricing mixes as well 
  mean_df_pricing_mixes <- get_effect_size_means_pricing(sector_policy_match_2y[[i]])
  
  #only keep mixes that display prices to plot comparison in overall mixes
  mean_df_pricing_mixes <- mean_df_pricing_mixes[mean_df_pricing_mixes$Pricing_indicator==1,]
  mean_df_pricing_mixes$Policy_name_fig_4[mean_df_pricing_mixes$Policy_name_fig_4=='Minimum energy performance standard'] = 'MEPS'
  mean_df_pricing_mixes_sub <- mean_df_pricing_mixes[c('Policy_name_fig_4','Average')]
  colnames(mean_df_pricing_mixes_sub) = c('Policy_name_fig_4','PricingMix')
  
  mean_df <- merge(mean_df,mean_df_pricing_mixes_sub,by='Policy_name_fig_4',all.x=TRUE)
  mean_df$PricingMix[mean_df$Cluster_categories=='Pricing'] = NA
  mean_df$PricingMix[mean_df$SinglePolicy==1] = NA
  mean_df$PricingMixStart = NA
  mean_df$PricingMixStart[!is.na(mean_df$PricingMix)] = 0
  
  #make a dictionary to fill in cluster categories in expanded df 
  dict_df <- mean_df[c('Policy_name_fig_4','Cluster_categories')]
  dict_df <- dict_df[!duplicated(dict_df),]
  
  ##complete for policies where there was no single policy introduction 
  mean_df <- expand.grid(Policy_name_fig_4 = unique(mean_df$Policy_name_fig_4), SinglePolicy = c(0, 1)) %>%
    left_join(mean_df, by = c("Policy_name_fig_4", "SinglePolicy"))
  
  for(j in 1:nrow(mean_df)){
    mean_df$Cluster_categories[j] = dict_df$Cluster_categories[dict_df$Policy_name_fig_4 == mean_df$Policy_name_fig_4[j]]
    
  }
  
  ##reorder according to cluster
  mean_df <- mean_df[order(mean_df$Cluster_categories,mean_df$Policy_name_fig_4),]
  
  mean_df <- mean_df %>% mutate(Cluster_categories = factor(Cluster_categories, levels = c('Pricing','Subsidy','Regulation','Information'))) %>% arrange(Cluster_categories)  
  
  mean_df$Policy_name_fig_4[mean_df$Policy_name_fig_4 == 'Public expenditure for rail'] = 'Public expenditure\nfor rail'
  
  mean_df$Policy_name_fig_4 = factor(mean_df$Policy_name_fig_4, levels = unique(mean_df$Policy_name_fig_4))
  
  mean_df$not_detected = 0
  mean_df$not_detected[is.na(mean_df$Average)] = 1
  mean_df$not_detected_val = NA
  mean_df$not_detected_val[is.na(mean_df$Average)] = 0

  p<-ggplot(mean_df, aes(x = Policy_name_fig_4, y = Average, alpha = factor(SinglePolicy))) +
    scale_alpha_manual(values = c(1,0.5), labels = c('Policy Mix','Single Policy'))+
    geom_bar(stat = "identity", position = "dodge",fill = sector_colors[i],show.legend = TRUE) +
    geom_errorbar(aes(ymin = not_detected_val, ymax = not_detected_val, group = factor(not_detected)),
                  width = 0.9,linewidth=1.5,alpha=1, position = position_dodge(0.9),color='tan')+
    geom_errorbar(aes(ymin = PricingMix, ymax = PricingMix, group = factor(SinglePolicy)),
                  width = 0.9,linewidth=1.5,alpha=1, position = position_dodge(0.9)) +
    labs(x = '', y = "") +
    scale_color_manual(values = c('black'),labels = c('Mix with Pricing'))+
    scale_fill_manual(values = c('white'))+
    guides(alpha = guide_legend(order=1,override.aes = list(linetype=c(0,0))),fill=guide_legend(override.aes = list(fill = "white", color = 'black'),order=2),color = guide_legend(override.aes = list(fill = "white",color='white',linetype = c(1), shape = c("-"))))+
    ylim(c(-40,0))+
    theme(plot.margin = margin(2, 1, 1, 3, "cm"),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.background = element_blank(),
          panel.grid.major.y = element_line( size=.1, color="grey" ),
          axis.title = element_blank(),
          axis.text = element_text(size=24),
          axis.title.y = element_text(size=25),
          legend.key.height = unit(1, "cm"),
          # legend.position = 'Bottom',
          legend.title = element_blank(),
          legend.text = element_text(size=22))
  
  legend <- get_legend(p)
  
  #make a legend for the lines as the combination of fill + line legends is super buggy in ggplot
  line_legend_plot <- ggplot(data = data.frame(x = c(1,2,3,4),y=c(1,2,3,4),group = c('Mix with Pricing','Mix with Pricing','Not detected','Not detected')),aes(x,y,color = group))+
    geom_line(linewidth=2)+
    scale_color_manual(values = c('black','tan'))+
    guides(linetype = guide_legend(override.aes = list(width = c(2,2))))+
    theme(plot.margin = margin(2, 1, 1, 3, "cm"),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.background = element_blank(),
          panel.grid.major.y = element_line( size=.1, color="grey" ),
          axis.title = element_blank(),
          axis.text = element_text(size=24),
          axis.title.y = element_text(size=25),
          legend.key = element_blank(),
          legend.background=element_blank(),
          # legend.position = 'Bottom',
          legend.title = element_blank(),
          legend.text = element_text(size=22))
  
  line_legend = get_legend(line_legend_plot)
  
  final_legend = cowplot::plot_grid(plotlist = list(legend,NULL,line_legend),nrow=3,rel_heights = c(1,-0.5,1))
  
  p = p + theme(legend.position = 'none')
  p <- cowplot::plot_grid(plotlist = list(p, final_legend),ncol=2, rel_widths = c(1,0.1))
  
  logo <- ggdraw() +
    draw_image(icon_links[i])
  
  p <- cowplot::plot_grid(plotlist=list(logo,p),ncol=2,rel_widths = c(0.07,1))
  
  my_mean_plots[[i]]<-p
}


#guide_legend(override.aes = list(fill = "white", color = 'black'))

#store means dfs 
mean_df_store = data.frame()
for(i in 1:4){
  mean_df <- get_effect_size_means(sector_policy_match_2y[[i]])
  mean_df$sector = unique(sector_policy_match_2y[[i]]$Module)[1]
  mean_df_store <- rbind(mean_df_store,mean_df)
}

write.csv(mean_df_store,'mean_dfs_fig_4b.csv')


p_mean_bars <- cowplot::plot_grid(plotlist = c(my_mean_plots,NULL),ncol=1,rel_heights = c(1,1,1,1,0.5), align='v',axis='b')

p_mean_bars <- p_mean_bars+ theme(plot.margin = margin(1,1, 1, 1, "cm"))+ geom_text(aes(x = 0.1, y = 0.5, label = 'Average effect size (%)'),
                                                                                    angle = 90,
                                                                                    hjust = 0.5,
                                                                                    vjust = -0.5,
                                                                                    size = 8,
                                                                                    color = "black")


#save subplot

png("Figs\\mean_effect_size_bars.png", width     = 35.00,height    = 15.00,units     = "in",res       = 800)
p_mean_bars
dev.off()

###create venn diagrams (Fig. 4B)

###make separate ones for developed + developing country groups 

#split the policy match data by economic development group 
developed_countries = read.csv('country_groupings.csv')
developed_countries$ISO = countrycode(developed_countries$Developed,origin='country.name',destination='iso3c')

developed_countries_filtered <- filtered_all 
developing_countries_filtered <- filtered_all


for(i in 1:4){
  df <- filtered_all$sector_policy_match[[i]]
  df$dev = 0
  df[df$ISO %in% developed_countries$ISO,'dev'] = 1
  
  developed_countries_filtered$sector_policy_match[[i]] <- df[df$dev == 1,]
  developing_countries_filtered$sector_policy_match[[i]] <- df[df$dev == 0,]
  
  print(paste('The information in developed countries in ',developed_countries_filtered$sector[i], 'comes from ', length(unique(df[df$dev==1,]$unique_break_identifier)),"breaks."))
  print(paste('The information in developing countries in ',developing_countries_filtered$sector[i], 'comes from ', length(unique(df[df$dev==0,]$unique_break_identifier)),"breaks."))
  
}


ven_diagrams_developing <- foreach(i = 1:nrow(developing_countries_filtered), .combine = rbind, .packages = c('tidyverse', 'getspanel')) %dopar% {
  #list[res,out,policy_match] <- extract_and_match(i,results,oecd_grouped)
  models = tibble(year_sample = developing_countries_filtered$spec[i],
                  sector = developing_countries_filtered$sector[i],
                  plot = list(venn_diagram_plot_basic(developing_countries_filtered$sector_policy_match[[i]],developing_countries_filtered$sector[i], title = "Developing economies")[[1]]),
                  euler_input = list(venn_diagram_plot_basic(developing_countries_filtered$sector_policy_match[[i]],developing_countries_filtered$sector[i], title = "Developing economies")[[2]]))
}

developing_vens = ven_diagrams_developing$plot


ven_diagrams_developed <- foreach(i = 1:nrow(developed_countries_filtered), .combine = rbind, .packages = c('tidyverse', 'getspanel')) %dopar% {
  #list[res,out,policy_match] <- extract_and_match(i,results,oecd_grouped)
  models = tibble(year_sample = developed_countries_filtered$spec[i],
                  sector = developed_countries_filtered$sector[i],
                  plot = list(venn_diagram_plot_basic(developed_countries_filtered$sector_policy_match[[i]],developed_countries_filtered$sector[i], title = 'Developed economies')[[1]]),
                  euler_input = list(venn_diagram_plot_basic(developed_countries_filtered$sector_policy_match[[i]],developed_countries_filtered$sector[i], title = 'Developed economies')[[2]]))
  
}

developed_vens = ven_diagrams_developed$plot

vens_combined = list()
icon_links = c("Logos\\Buildings.png","Logos\\Electricity.png","Logos\\Industry.png","Logos\\Transport.png")

for(i in 1:4){
  p <- cowplot::plot_grid(plotlist = list(developed_vens[[i]],NULL, developing_vens[[i]]), nrow=3, rel_heights = c(1,0.1,1))
  p <- p+theme(plot.margin = unit(c(0.5,0,0.5,1), "cm"), plot.background = element_rect(color = sector_colors[i],linewidth=3))
  
  #add sector logo
  logo <- ggdraw() +
    draw_image(icon_links[i])
  
  p <- ggdraw(p) +
    draw_plot(logo, -0.03, .73, .28, .28)+
    theme(plot.margin = unit(c(1,1,1,1), "cm"))
  vens_combined[[i]] <- p
  
}

both_groups_ven_panel <- cowplot::plot_grid(plotlist=list(vens_combined[[1]],NULL,vens_combined[[2]],NULL,NULL,NULL,vens_combined[[3]],NULL,vens_combined[[4]]), nrow=3,ncol=3,rel_widths = c(1,0.05,1),rel_heights = c(1,0.05,1))+theme(plot.margin = unit(c(1,1,1,1),'cm'))

#####IMPORTANT NOTE
#please note that the eulerr embedding is not fully deterministic as the algorithm computs a new embedding every time 
#which may look slightly different. Further, for Transport/Developing there is a 100% overlap between pricing and
#subsidy which does not always get picked up (notable by a larger error in the algorithm fit). The current 
#embedding should be checked against the tables (see below) to ensure correctness. For Transport/Developed
#ellipses are used as it is not possible to accurately depict the overlaps with circles due to geometric 
#restrictions. Fig. 4B is postprocessed manually to improve on the automatic label placement and ensure readability.

png("Figs\\ven_diagrams_test.png", width     = 25.00,height    = 25.00,units     = "in",res       = 800)
both_groups_ven_panel
dev.off()

##save the euler inputs as tables for SI  

developed_euler_input = data.frame()

for(i in 1:4){
  euler_input = ven_diagrams_developed$euler_input[[i]]
  euler_input$sector = ven_diagrams_developed$sector[i]
  developed_euler_input = rbind(developed_euler_input, euler_input)
}

developed_euler_input = subset(developed_euler_input, select=-c(label, n))

#print(xtable(developed_euler_input), include.rownames = FALSE, include.colnames = TRUE)

write.csv(developed_euler_input,"Fig_4B_developed.csv")

developing_euler_input = data.frame()

for(i in 1:4){
  euler_input = ven_diagrams_developing$euler_input[[i]]
  euler_input$sector = ven_diagrams_developing$sector[i]
  developing_euler_input = rbind(developing_euler_input, euler_input)
}

developing_euler_input = subset(developing_euler_input, select=-c(label,n))

#print(xtable(developing_euler_input), include.rownames = FALSE, include.colnames = TRUE)


write.csv(developing_euler_input,"Fig_4B_developing.csv")



###assemble Fig. 4

mean_bars_final <- p_mean_bars + theme(panel.border = element_rect(color = "grey", fill = NA, size = 1.5))

both_groups_ven_panel_final <- both_groups_ven_panel+theme(panel.border = element_rect(color = "grey", fill = NA, size = 1.5))+theme(plot.margin = margin(1, 1, 1, 1, "cm"))

Fig_4 <- cowplot::plot_grid(plotlist = list(mean_bars_final,both_groups_ven_panel_final),nrow=2,rel_heights = c(1.5,3))

pdf("Figs\\Fig_4.pdf", width     = 34,height    = 40)
Fig_4
dev.off()


