###### make alternative mean bars comparing mixes with and without pricing
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

icon_links = c("C:\\Users\\stecheme.PIKACCOUNTS\\Documents\\Break_detection\\OECD_project\\Logos\\Buildings.png","C:\\Users\\stecheme.PIKACCOUNTS\\Documents\\Break_detection\\OECD_project\\Logos\\Electricity.png","C:\\Users\\stecheme.PIKACCOUNTS\\Documents\\Break_detection\\OECD_project\\Logos\\Industry.png","C:\\Users\\stecheme.PIKACCOUNTS\\Documents\\Break_detection\\OECD_project\\Logos\\Transport.png")

my_mean_plots = list()

for(i in 1:4){

  mean_df <- get_effect_size_means_pricing(sector_policy_match_2y[[i]])
  mean_df$Policy_name_fig_4[mean_df$Policy_name_fig_4=='Minimum energy performance standard'] = 'MEPS'

  #make a dictionary to fill in cluster categories in expanded df
  dict_df <- mean_df[c('Policy_name_fig_4','Cluster_categories')]
  dict_df <- dict_df[!duplicated(dict_df),]

  ##complete for policies where there was no single policy introduction
  mean_df <- expand.grid(Policy_name_fig_4 = unique(mean_df$Policy_name_fig_4), Pricing_indicator = c(1,0)) %>%
    left_join(mean_df, by = c("Policy_name_fig_4", "Pricing_indicator"))
  mean_df[is.na(mean_df)] <- 0

  for(j in 1:nrow(mean_df)){
    mean_df$Cluster_categories[j] = dict_df$Cluster_categories[dict_df$Policy_name_fig_4 == mean_df$Policy_name_fig_4[j]]

  }

  ##reorder according to cluster
  mean_df <- mean_df[order(mean_df$Cluster_categories,mean_df$Policy_name_fig_4),]

  for(j in 1:nrow(mean_df)){
    mean_df$Policy_name_fig_4[j] = add_newline(mean_df$Policy_name_fig_4[j])
  }
  mean_df$Policy_name_fig_4[mean_df$Policy_name_fig_4 == 'Public expenditure for rail'] = 'Public expenditure\nfor rail'

  mean_df$Policy_name_fig_4 = factor(mean_df$Policy_name_fig_4, levels = unique(mean_df$Policy_name_fig_4))

  p<-ggplot(mean_df, aes(x = Policy_name_fig_4, y = Average, alpha = factor(Pricing_indicator))) +
    scale_alpha_manual(values = c(1,0.5), labels = c('No Pricing',
                                                         "Pricing"))+
    geom_bar(stat = "identity", position = "dodge",fill = sector_colors[i],show.legend = TRUE) +
    #geom_errorbar(aes(ymin = FirstQuartile, ymax = ThirdQuartile),
    #              width = 0.2, position = position_dodge(0.9)) +
    labs(x = '', y = "") +
    scale_fill_discrete(name = "AppearsOnce") +
    ylim(c(-40,0))+
    theme(plot.margin = margin(2, 1, 1, 3, "cm"),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.background = element_blank(),
          panel.grid.major.y = element_line( size=.1, color="grey" ),
          axis.title = element_blank(),
          axis.text = element_text(size=24),
          axis.title.y = element_text(size=25),
          legend.key.height = unit(2, "cm"),
          # legend.position = 'Bottom',
          legend.title = element_blank(),
          legend.text = element_text(size=22))


  logo <- ggdraw() +
    draw_image(icon_links[i])
  
  p <- cowplot::plot_grid(plotlist=list(logo,p),ncol=2,rel_widths = c(0.1,1))
  
  my_mean_plots[[i]]<-p

  my_mean_plots[[i]]<-p
}


p_mean_bars <- cowplot::plot_grid(plotlist = c(my_mean_plots,NULL),ncol=1,rel_heights = c(1,1,1,1,0.5), align='v',axis='b')

p_mean_bars <- p_mean_bars+ theme(plot.margin = margin(1,1, 1, 1, "cm"))+ geom_text(aes(x = 0.1, y = 0.5, label = 'Average effect size (%)'),
                                                                                    angle = 90,
                                                                                    hjust = 0.5,
                                                                                    vjust = -0.5,
                                                                                    size = 8,
                                                                                    color = "black")




pdf("plots_first_submission\\SI\\mean_bars_no_pricing_pricing_mixes.pdf", width     = 25.00,height    = 20.00)
p_mean_bars
dev.off()

