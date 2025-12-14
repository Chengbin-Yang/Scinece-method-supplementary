

setwd("F:\\Desktop\\科研项目\\1.负责科研项目\\Climate Policy DID Science-厦大_北师大\\Science")
source('00_oecd_project_functions.R')

#load required data 

#read preprocessed oecd policy data
oecd_grouped = read.csv('OECD_data_preprocessed.csv')

#define colors

buildings_color="#eb5601"
electricity_color="#e7c019"
industry_color = "#bac36b"
transport_color = "#3b9ab2"


### Plotting for Fig. 1 

###### Fig. 1A

#Average total number of adopted policies by sector and year across countries
oecd_group_sector_country <- as.data.frame(oecd_grouped %>% group_by(Module,year) %>% summarize(n=n()))
oecd_group_sector_country <- complete(oecd_group_sector_country,Module,year)
oecd_group_sector_country[is.na(oecd_group_sector_country$n),'n'] = 0

oecd_group_sector_country <- as.data.frame(oecd_group_sector_country %>% group_by(Module) %>% mutate(csum = cumsum(n)))
number_of_countries = length(unique(oecd_grouped$ISO))
oecd_group_sector_country$csum_average = oecd_group_sector_country$csum/number_of_countries

linechart_cum_trend <- ggplot(oecd_group_sector_country,aes(x=year,y=csum_average,fill=Module,color=Module))+geom_line(linewidth=3)+scale_color_manual(values = c(buildings_color,electricity_color,industry_color,transport_color))+
  ylab('Average number of\nadopted policies &\ntightenings per country')+
  xlab('')+
  theme(plot.title = element_text(face="bold", size=15,hjust = 0.5),
        axis.title.x = element_text(size=15, angle=0, face="bold", vjust=1.1),
        axis.title.y = element_text(size=14, angle=90),
        axis.text.y = element_text(size=15, angle=0, face="bold"),
        axis.text.x = element_text(size=15, angle=0, face="bold"),
        axis.ticks.y = element_line(),
        axis.ticks.x = element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line.x = element_line(),
        axis.line.y = element_line(),
        legend.key.size = unit(1, 'cm'),
        legend.title = element_blank(), #change legend title font size
        legend.text = element_text(size=16),
        legend.position = c(0.1, 0.6),
        plot.background = element_rect(colour = "grey", fill=NA, linewidth=3))

png("Figs\\linechart.png", width     = 15,height    = 3,units     = "in",res       = 800)
linechart_cum_trend
dev.off()

pdf("Figs\\linechart.pdf", width     = 15,height    = 3)
linechart_cum_trend
dev.off()

##Count presence of different instrument types for main text 

#count command and control measures for main text 
command_and_control <- c('Building code','Air pollution standard','Performance standard (electric motors)','Performance standard (appliances)','Performance standard (cars & trucks)','Renewable portfolio standard','Energy efficiency mandate')
candc_cases <- oecd_grouped[oecd_grouped$Policy_name_fig_1 %in% command_and_control,]

##count market based vs non market based
#count which sector has most market based policies
oecd_grouped %>% group_by(Module, Market_non_market) %>% count()

##check how many subsidies are in annex 1 countries
subsidy_count <- oecd_grouped %>% group_by(Module, High_income, Policy_name_fig_4)%>% count() %>% filter(Policy_name_fig_4 == 'Subsidy')

sum(subsidy_count$n[subsidy_count$High_income==1])

sum(subsidy_count$n[subsidy_count$High_income==0])

##check carbon pricing
pricing_count = oecd_grouped %>% group_by(Policy_name_fig_1, High_income) %>% count() %>% filter(Policy_name_fig_1 %in% c("Carbon tax","Emission trading scheme"))

sum(pricing_count$n[pricing_count$High_income==1])

#pricing and subsidy count: 
oecd_grouped %>% group_by(Cluster_categories, High_income) %>% count()

###Fig. 1B

##visualize mixes

oecd_mix_names <- as.data.frame(oecd_grouped %>% group_by(Module, Policy_name_fig_1,High_income) %>% summarize(n=n()))

oecd_mix_names$High_income_label = "High_income"
oecd_mix_names$High_income_label[oecd_mix_names$High_income==0] = "Low_income" 
oecd_mix_names$High_income_label = as.factor(oecd_mix_names$High_income_label)

oecd_mix_names$High_income_label = as.factor(oecd_mix_names$High_income_label)

##plot
#source of logos: Canva (Pro Version)

my_bars <- list()
counter=1
icon_links = c("Logos\\Buildings.png","Logos\\Electricity.png","Logos\\Industry.png","Logos\\Transport.png")
colors_plot =  c(buildings_color,electricity_color,industry_color,transport_color)
for(module in c('Buildings','Electricity','Industry','Transport')){
  oecd_sub <- as.data.frame(oecd_mix_names[oecd_mix_names$Module==module,])
  oecd_sub <- oecd_sub %>% complete(High_income,Policy_name_fig_1) %>% as.data.frame()
  oecd_sub$n[is.na(oecd_sub$n)] = 0
  oecd_sub$High_income_label[oecd_sub$High_income==0] = 'Low_income'
  oecd_sub$High_income_label[oecd_sub$High_income==1] = 'High_income'
  
  #oecd_sub$Policy_name[oecd_sub$Policy_name=='Minimum energy performance standard'] = 'Min. energy performance standard'
  #oecd_sub$Policy_name[oecd_sub$Policy_name=='Ban & phase out of fossil heatings'] = 'Ban & phase out fossil heating'
  
  mix <- ggplot(oecd_sub,aes(x=reorder(Policy_name_fig_1, -n),y=n,label=Policy_name_fig_1)) +
    ylim(c(0,85))+
    geom_col(aes(group=High_income_label,alpha=High_income_label),show.legend=FALSE,fill=colors_plot[counter],color = "grey") +
    geom_text(aes(y=1.5),size = 5.3, angle = 90,position=position_dodge(width = 0.9), hjust=0)+
    #geom_text(size = 6, position=position_dodge(width = 0.9), hjust=0)+
    scale_alpha_manual(values = c(High_income = 1, Low_income = 0.5)) +
    theme_void()
  
  if(counter==1){
    mix = mix +
      ylab('Number of adopted policies and\ntightenings by policy instrument')+
      theme(axis.title.y = element_text(size=15, face="bold", angle = 90),
            axis.text.y = element_text(size=15, face="bold"),
            axis.ticks.y = element_line(),
            axis.line.y = element_line())
  }
  
  logo <- ggdraw() +
    draw_image(icon_links[counter])
  mix_logo<-cowplot::plot_grid(plotlist=list(mix,NULL,logo), nrow=3,ncol=1,rel_heights =c(1,-0.01,0.3))+theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))
  my_bars[[counter]] <- mix_logo
  counter=counter+1
}

mix_final<-cowplot::plot_grid(plotlist=my_bars, ncol=4,nrow=1)+theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))


##make legend with color shading and groups 

# Create toy df to make legend
data <- data.frame(
  x = c(1, 2, 3, 4),
  color = c(buildings_color,electricity_color,industry_color,transport_color)
)

# Create the plot
legend_dev <- ggplot(data, aes(x = x)) +
  geom_rect(aes(fill = color,xmin = x - 0.4, xmax = x + 0.4, ymin = 2, ymax = 3),alpha=0.5) +
  geom_rect(data=data,aes(color = color,fill=color,xmin = x - 0.4, xmax = x + 0.4, ymin = 2, ymax = 3)) +
  xlim(0.5, 4.5) +
  ylim(1.5, 4) +
  labs(title = "Colored Squares with Text Labels") +
  theme_minimal()+
  theme(legend.position = 'bottom',
        legend.key.size = unit(0.5,"cm"))+
  guides(nrow=2,color = guide_legend(override.aes = list(fill = c(buildings_color,electricity_color,industry_color,transport_color))))+
  scale_fill_manual(values=alpha(c(buildings_color,electricity_color,industry_color,transport_color),0),labels = c('','','','Argentina, Brazil, Chile, China, Colombia,\nIndia, Indonesia, Mexico,Peru, Russia, Saudi Arabia,\nSouth Africa, South Korea, Turkey'))+
  scale_color_manual(values=c(buildings_color,electricity_color,industry_color,transport_color),labels = c('','','','Australia, Austria, Belgium, Bulgaria, Canada, Czech Republic, Denmark, Finland, France,\nGermany, Greece, Hungary, Ireland, Italy, Japan,Netherlands, New Zealand, Norway, Poland, Portugal,\nRomania,Slovak Republic, Spain, Sweden, Switzerland, United Kingdom, United States'))+
  labs(color = 'Developed\neconomies',fill = 'Developing/Transitioning\neconomies')

legend_dev <- get_legend(legend_dev)

png("Figs\\mix_plot_with_legend.png", width     = 15,height    = 6,units     = "in",res       = 800)
cowplot::plot_grid(plotlist = list(mix_final, legend_dev),nrow=2,rel_heights = c(1,0.1))+theme(plot.background = element_rect(colour = "grey", fill=NA, linewidth=3))
dev.off() 

pdf("Figs\\mix_plot_with_legend.pdf", width     = 15,height    = 6)
cowplot::plot_grid(plotlist = list(mix_final, legend_dev),nrow=2,rel_heights = c(1,0.1))+theme(plot.background = element_rect(colour = "grey", fill=NA, linewidth=3))
dev.off() 


###### Fig. 1C

## get break counts per country to put on map 
## here the count plots and background maps are generated. The final Fig. 1C was assembled manually (i.e. placing countries on the map)

##here - as for all counts - we use the merged break count
policy_out_filtered <- readRDS("Policy_out_filtered.RDS")
final_countries = read.csv('final_countries_in_analysis.csv')


break_count = data.frame()

for(i in 1:nrow(policy_out_filtered)){
  out <- policy_out_filtered[i,]$out[[1]]
  out$sector = policy_out_filtered[i,]$sector
  break_count = rbind(break_count,out)
}
count_df <- break_count %>% dplyr::count(id, sector)

grid <- expand.grid(id = gsub(" ","",final_countries$unique.df_excl.country.), sector = c('Buildings','Electricity','Industry','Transport'))

expanded_df <- left_join(grid, count_df, by = c("id", "sector"))
expanded_df$n[is.na(expanded_df$n)] <- 0
id_order <- sort(unique(expanded_df$id))
expanded_df$id <- factor(expanded_df$id, levels = id_order)

sector_total <- expanded_df %>%
  group_by(sector) %>%
  summarise(n = sum(n)) %>%
  mutate(id = "Totals")

expanded_df$ISO = countrycode(expanded_df$id,origin='country.name',destination='iso3c')
expanded_df$ISO[expanded_df$id=='SouthAfrica']='ZAF'
expanded_df$ISO[expanded_df$id=='SouthKorea']='KOR'

##before plotting, set some colormaps: 
colors <- wesanderson::wes_palette("Zissou1", 100, type = "continuous")# View the colors

transport_color = colors[1]
industry_color = colors[40]
electricity_color = colors[60]
buildings_color = colors[90]



##save continent maps for assembling later with small country plots

world <- ne_countries(scale = "medium", returnclass = "sf")

world_oecd_grouped <- as.data.frame(merge(expanded_df, world, by.x="ISO",by.y="iso_a3",all.x=TRUE))


Europe <- ggplot(data = world) +
  geom_sf(color='grey',lwd = 0.7) +
  coord_sf(xlim = c(-15, 50), ylim = c(25, 67), expand = FALSE)+
  ggtitle('Europe and Middle East')+
  theme_void()+
  theme(plot.title = element_text(size=25,hjust=0.5),
        panel.border = element_rect(colour = "darkgrey", fill=NA, linewidth=2),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))

Americas <- ggplot(data = world) +
  geom_sf(color='grey',lwd = 0.7) +
  coord_sf(xlim = c(-125, -35), ylim = c(-60, 70), expand = FALSE)+
  theme_void()+
  ggtitle('Americas')+
  theme(plot.title = element_text(size=25,hjust=0.5),
        panel.border = element_rect(colour = "darkgrey", fill=NA, linewidth=2),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))

Asia_Oceania <- ggplot(data = world) +
  geom_sf(color='grey',lwd = 0.7) +
  coord_sf(xlim = c(60, 180), ylim = c(-50,45), expand = FALSE)+
  theme_void()+
  ggtitle('Asia and Oceania')+
  theme(plot.title = element_text(size=25,hjust=0.5),
        panel.border = element_rect(colour = "darkgrey", fill=NA, linewidth=2),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))

South_Africa <- ggplot(data = world) +
  geom_sf(color='grey',lwd = 0.7) +
  coord_sf(xlim = c(10,40), ylim = c(-20,-40), expand = FALSE)+
  ggtitle('South Africa')+
  theme_void()+
  theme(plot.title = element_text(size=35,hjust=0.5),
        panel.border = element_rect(colour = "darkgrey", fill=NA, linewidth=2),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))

png("Figs\\Europe.png", width     = 10,height    = 10,units     = "in",res       = 800)
show(Europe)
dev.off() 

pdf("Figs\\Europe.pdf", width     = 10,height    = 10)
show(Europe)
dev.off()

png("Figs\\Asia_Oceania.png", width     = 10,height    = 10,units     = "in",res       = 800)
show(Asia_Oceania)
dev.off() 

pdf("Figs\\Asia_Oceania.pdf", width     = 10,height    = 10)
show(Asia_Oceania)
dev.off() 

png("Figs\\Americas.png", width     = 10,height    = 20,units     = "in",res       = 800)
show(Americas)
dev.off() 

pdf("Figs\\Americas.pdf", width     = 10,height    = 20)
show(Americas)
dev.off() 

png("Figs\\South_Africa.png", width     = 10,height    = 7,units     = "in",res       = 800)
show(South_Africa)
dev.off() 

pdf("Figs\\South_Africa.pdf", width     = 10,height    = 7)
show(South_Africa)
dev.off()


##make count barcharts (to place on map) 

outline_df_4 = data.frame(sector = c('Buildings','Electricity','Industry','Transport'),n=c(4,4,4,4))
outline_df_3 = data.frame(sector = c('Buildings','Electricity','Industry','Transport'),n=c(3,3,3,3))
outline_df_2 = data.frame(sector = c('Buildings','Electricity','Industry','Transport'),n=c(2,2,2,2))
outline_df_1 = data.frame(sector = c('Buildings','Electricity','Industry','Transport'),n=c(1,1,1,1))

for(c in unique(expanded_df$ISO)){
  country_df = expanded_df[expanded_df$ISO==c,]
  
  break_bars <- ggplot(country_df,aes(x=sector,y=n,fill=sector))+
    geom_col(show.legend=FALSE,linewidth=2,color='white')+#ylim(0,4)+
    geom_col(data = outline_df_4,color='white',fill=NA,linewidth=2)+
    geom_col(data = outline_df_3,color='white',fill=NA,linewidth=2)+
    geom_col(data = outline_df_2,color='white',fill=NA,linewidth=2)+
    geom_col(data = outline_df_1,color='white',fill=NA,linewidth=2)+
    geom_vline(xintercept = c(0.5,1.5,2.5,3.5,4.5),linewidth=2)+
    # stat_summary(fun = sum, geom = "text", aes(label = after_stat(y)),
    #              position = position_stack(vjust = 1.1),size=30) +
    scale_fill_manual(values = c(buildings_color,electricity_color,industry_color,transport_color),
                      labels = c('Buldings','Electricity','Industry','Transport'))+
    theme_void()+
    ggtitle(c)+
    theme(plot.title = element_text(size=100,hjust=0.5),
          plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))
  
  if(c=='ZAF'){
    break_bars <- break_bars + 
      scale_y_continuous(breaks = c(1,2,3,4),labels = c("1 ","2 ","3 ","4+ "))+
      theme(axis.ticks.y = element_line(linewidth = 4),
            axis.text.y = element_text(size = 75))
  }
  
  path = paste("Figs\\Break_count_bar_charts_pdf\\",c,".pdf",sep="")
  #png(path, width     = 5,height    = 5,units     = "in",res       = 800)
  pdf(path, width     = 5,height    = 5)
  show(break_bars)
  dev.off() 
}

##make and save legend
p_legend <- ggplot(expanded_df,aes(x=ISO,fill=sector))+
  geom_bar(color='black',size = 0.02)+
  scale_fill_manual(values = c(buildings_color,electricity_color,industry_color,transport_color),
                    labels = c('Buildings','Electricity','Industry','Transport'),name='Number of detected breaks\nin CO2 emissions between\n2000 and 2020 by sector')+
  theme(legend.key.size = unit(0.7, 'cm'),
        legend.title = element_text(size=15),
        legend.text = element_text(size=15),
        legend.margin=margin(l = 3, unit='cm'),
        rect = element_rect(fill = "transparent"),
        legend.position = 'right')
legend <- get_legend(p_legend)


#myplots[[counter]] = legend
p <- cowplot::plot_grid(plotlist=list(legend))

png("Figs\\legend_break_map.png", width     = 20.00,height    = 5.00,units     = "in",res       = 800)
p
dev.off()
