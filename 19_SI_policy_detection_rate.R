setwd("F:\\Desktop\\科研项目\\1.负责科研项目\\Climate Policy DID Science-厦大_北师大\\Science")
source('00_oecd_project_functions.R')

#load data

oecd_grouped = read.csv('OECD_data_preprocessed.csv')

oecd_grouped = oecd_grouped[oecd_grouped$year >1999,]
oecd_grouped = oecd_grouped[oecd_grouped$year <2021,]

policy_out <- readRDS("Policy_out.RDS")

policy_out_filtered <- readRDS("policy_out_filtered.RDS")

specs = c('policy_match','policy_match_2y','policy_match_3y')
filtered_all <- sector_policy_match(policy_out_filtered, specs)

filtered_all <- filtered_all[filtered_all$spec == 'policy_match_2y',]

sector_policy_match_2y = filtered_all$sector_policy_match[filtered_all$spec == "policy_match_2y"]
##plot as bars

oecd_grouped_adj = oecd_grouped

sectors = c('Buildings','Electricity','Industry','Transport')

sector_colors = c("#eb5601", "#e7c019", "#bac36b", "#3b9ab2")

all_across = data.frame()
for(i in 1:4){
  count_across <- detected_vs_missing(oecd_grouped_adj, sector_policy_match_2y[[i]],sectors[i],introductions_only = FALSE)
  count_across$sector = sectors[i]
  all_across <- rbind(all_across,count_across)
}

my_plots = list()
for(i in 1:4){
  count_across <- detected_vs_missing(oecd_grouped_adj, sector_policy_match_2y[[i]],sectors[i],introductions_only = FALSE)
  count_across$x[count_across$x=='Minimum energy performance standard'] = 'MEPS'
  background_bars = data.frame(x= count_across$x, detected_percent = count_across$detected_percent, y = 50)
  
  p<-ggplot(count_across, aes(x = detected_percent, y = reorder(x, detected_percent))) +
    geom_bar(stat = "identity",fill = sector_colors[i],show.legend=FALSE) +
    geom_bar(data=background_bars,aes(x=y,y=reorder(x, detected_percent)),color='black',fill=NA,stat='identity') + 
    xlim(c(0,50))+
    geom_text(aes(label=overall_count,x=50), position=position_dodge(width=0.9),size=10, hjust=-0.25)+
    #scale_fill_gradient(low = "white", high = sector_colors[sectors[i]]) +
    theme_minimal()+
    scale_x_continuous(labels = function(x) paste0(x, "%"))+
    xlab('Share of policies detected in large emission reductions')+
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.title = element_blank(),
          axis.text = element_text(size=25),
          legend.position = 'none',
          axis.title.x = element_text(size=25))
  # legend.key.height = unit(2, "cm"),
  # legend.position = 'Bottom',
  # legend.title = element_text(size=15),
  # legend.text = element_text(size=15))
  
  if(i<4){
    p <- p+theme(axis.text.x = element_blank(),axis.title.x = element_blank())
  }
  
  my_plots[[i]]<-p
  path = paste("C:\\Users\\stecheme.PIKACCOUNTS\\Documents\\Break_detection\\OECD_project\\Plots\\",sectors[i],"_overall_into_intensific_policies_detected_2y.png",sep="")
  png(path, width     = 15.00,height    = 15.00,units     = "in",res       = 800)
  print(p)
  dev.off()
}


p_final <- cowplot::plot_grid(plotlist = my_plots,ncol=1,align='v',axis='b')+ theme(plot.margin = margin(0, 1, 0, 0, "cm"))+ 
  geom_text(aes(x = 1.0, y = 0.5, label = 'Total number of policy introductions and tightenings'), angle = -90, hjust = 0.5, vjust = -0.5, size = 8, color = "black")

#right.grob <- textGrob("Number of policy introductions and tightenings", gp=gpar(fontface="bold", fontsize=25), rot=270)

pdf("plots_first_submission\\SI\\share_of_detection.pdf", width     = 18.00,height    = 15.00)
grid.arrange(arrangeGrob(p_final))

dev.off()

##count how many times pricing was implemented in electricity 
developing = oecd_grouped[oecd_grouped$High_income==0,]
developing_electricity = developing[developing$Module=='Electricity',]

plyr::count(developing_electricity$Cluster_categories)

developing_electricity_pricing = developing_electricity[developing_electricity$Cluster_categories=='Pricing',]
