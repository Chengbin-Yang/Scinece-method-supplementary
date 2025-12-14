setwd("F:\\Desktop\\科研项目\\1.负责科研项目\\Climate Policy DID Science-厦大_北师大\\Science")
source('00_oecd_project_functions.R')

oecd_grouped = read.csv('OECD_data_preprocessed.csv')

oecd_grouped = oecd_grouped[oecd_grouped$year >1999,]
oecd_grouped = oecd_grouped[oecd_grouped$year <2021,]


buildings_color = "#eb5601"
electricity_color = "#e7c019"
industry_color = "#bac36b"
transport_color = "#3b9ab2"


sector_colors = c(buildings_color,electricity_color,industry_color,transport_color)
names(sector_colors) = c('Buildings','Electricity','Industry','Transport')


matrix_plot <- function(policy_match,sector,color,legend=TRUE){
  
  matrix_match = matrix(nrow = length(unique(policy_match$Policy_name_fig_4)),ncol=length(unique(policy_match$Policy_name_fig_4)),0)
  rownames(matrix_match) = unique(policy_match$Policy_name_fig_4)
  colnames(matrix_match) = unique(policy_match$Policy_name_fig_4)
  
  matrix_match_diag = matrix_match
  
  if(nrow(matrix_match)<2){
    return(print('data input too small'))
  }
  
  for(label in rownames(matrix_match)){
    #check which breaks this label appears in 
    policy_match_sub = policy_match[policy_match$Policy_name_fig_4==label,]
    
    #to count how many times the policy is introduced by itself, set as starting value the number of breaks it appears as 
    diag_val = length(unique(policy_match_sub$unique_break_identifier))
    
    #loop over the breaks the label appears in 
    for(b in unique(policy_match_sub$unique_break_identifier)){
      
      break_sub = policy_match[policy_match$unique_break_identifier == b,]
      
      #count how often each policy appears at that break together with label
      type_count = plyr::count(break_sub$Policy_name_fig_4)
      
      if(length(unique(break_sub$Policy_name_fig_4))>1){
        diag_val = diag_val-1
      }
      
      
      #update the count in matrix match with the co-occurrence of the labels at that breaks
      for(match_lab in type_count$x){
        #if a policy matches another policy twice at one break we only count it as +1
        matrix_match[label,match_lab] = matrix_match[label,match_lab] + 1
        
      }
      matrix_match_diag[label,label] = diag_val/length(unique(policy_match_sub$unique_break_identifier))
    }
    #normalize
    matrix_match[label,] = matrix_match[label,]/nrow(policy_match[policy_match$Policy_name_fig_4==label,])
  }
  
  
  
  #diag(matrix_match) <- NA
  diss_matrix <- dist(matrix_match)
  seriated <- seriate(diss_matrix, method = "OLO")
  row_order <- get_order(seriated)
  column_order <- get_order(t(seriated))
  ordered_matrix <- matrix_match[row_order, row_order]
  #ordered_matrix <- ordered_matrix[, ncol(ordered_matrix):1]
  
  ordered_diag <- matrix_match_diag[row_order,row_order]
  # ordered_diag[lower.tri(ordered_diag)] <- NA
  #ordered_diag[upper.tri(ordered_diag)] <- NA
  
  for(i in 1:nrow(ordered_matrix)){
    rownames(ordered_matrix)[i] = add_newline(rownames(ordered_matrix)[i])
    rownames(ordered_diag)[i] = add_newline(rownames(ordered_diag)[i])
    
    colnames(ordered_matrix)[i] = add_newline(colnames(ordered_matrix)[i])
    colnames(ordered_diag)[i] = add_newline(colnames(ordered_diag)[i])
    
  }
  

  x_ordered <- prep_matrix(ordered_matrix)
  x_diag <- prep_matrix(ordered_diag)

  x_ordered <- x_ordered[x_ordered$x>0,]
  x_diag <- x_diag[x_diag$row == x_diag$col,] 
  print(x_ordered)
  print(x_diag)
  
  # Plot using ggplot2
  combined_data <- rbind(transform(x_ordered, category = "Co-occurrence"),
                         transform(x_diag, category = "Share of occurrence"))
 
  black_palette <- c("grey100","grey80","grey70","grey60","grey50","grey40","grey30","grey20","grey10","grey0")
  
  map_value_to_color <- function(value, category) {
    if (category == "Co-occurrence") {
      color <- alpha(color, alpha = value)
    } else if (category == "Share of occurrence") {
      if (value == 0) {
        color <- "grey100"
      } else {
        interval <- floor(value * 10) + 1
        color <- black_palette[interval]
      }
    } else {
      color <- NA
    }
    return(color)
  }
  
  combined_data$color = mapply(map_value_to_color, combined_data$x, combined_data$category)
  
  p_heat <- ggplot(combined_data, aes(x = col, y = row, group=category,fill = color),show.legend=TRUE) +
    geom_raster(show.legend=TRUE) +
    scale_fill_identity()+
    #scale_alpha(range=c(0.2,1),na.value=0)+
    #scale_fill_manual(values = c( color,'black'))+
    ggtitle(sector)+
    #guides(fill = guide_colourbar(barwidth = 15,barheight = 3))+
    theme(plot.title = element_text(size=27, hjust = 0.5),
          panel.grid = element_blank(),
          axis.title=element_blank(),
          aspect.ratio = nrow(ordered_matrix) / ncol(ordered_matrix),
          panel.background = element_rect(fill='white',color='grey'),
          axis.text = element_text(size=22),
          axis.text.x = element_text(size=22,angle=45,vjust=0.7),
          legend.position = "bottom",
          legend.title = element_text(size=20),
          legend.text = element_text(size=20))
  
  
  #to make the legend for the colors proper, use the range from 0.2 to 1 as used in the plot and manually add white for 0
  
  legend_color_df = data.frame(x=c(2,3,4,5,6),y=c(1,1,1,1,1),alpha=c(0.2,0.4,0.6,0.8,1), group = 'color')
  legend_color_df = rbind(legend_color_df, data.frame(x=c(1),y=c(1),alpha = c(0), group = 'white'))
  
  legend_color <- ggplot(legend_color_df,aes(x=x,y=y,alpha=alpha,fill=group))+scale_x_continuous(breaks = c(1,2,3,4,5,6), labels = c(0,0.2,0.4,0.6,0.8,1))+geom_tile(color='black')+scale_fill_manual(values=c(color,'white'))+ggtitle('Share of co-occurence (%)')+theme_void()+theme(plot.margin = margin(0,0,0,2),axis.ticks.x = element_line(),axis.text.x = element_text(size=20),plot.title = element_text(size = 20, hjust=0.5),legend.title = element_text(),legend.position = 'none')
  
  p_final <- cowplot::plot_grid(plotlist = list(p_heat,legend_color), nrow=2,ncol=1, rel_heights = c(1,0.07),align='v',axis='l')+theme(plot.margin = margin(1,1,1,1,unit='cm'))
  
  return(p_final)
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


##make an ensemble


policy_out <- readRDS("Policy_out.RDS")

policy_out_filtered <- readRDS("policy_out_filtered.RDS")


specs = c('policy_match','policy_match_2y','policy_match_3y')


filtered_all <- sector_policy_match(policy_out_filtered, specs)

sector_colors = c("#EB5600" , "#E7C019","#BAC36B","#3B9AB2")


##only keep the 2y subset 

filtered_all <- filtered_all[filtered_all$spec == 'policy_match_2y',]


maps_filtered_all <- foreach(i = 1:nrow(filtered_all), .combine = rbind, .packages = c('tidyverse', 'getspanel')) %dopar% {
  #list[res,out,policy_match] <- extract_and_match(i,results,oecd_grouped)
  models = tibble(year_sample = filtered_all$spec[i],
                  sector = filtered_all$sector[i],
                  plot = list(matrix_plot(filtered_all$sector_policy_match[[i]],filtered_all$sector[i],sector_colors[i])))
  
}

two_year_maps = maps_filtered_all$plot

matrix_panel_broad <- cowplot::plot_grid(plotlist=two_year_maps, nrow=2,ncol=2)+theme(plot.margin = margin(0.2,0.2,0.2,0.2))

legend_diag <- ggplot(data.frame(x=c(1,2,3,4,5,6,7,8,9,10),y=c(1,1,1,1,1,1,1,1,1,1),color= c("grey100","grey80","grey70", "grey60","grey50", "gray40","grey30", "gray20","grey10", "black")),aes(x=y,y=x,group=color,fill=color))+geom_tile(color = 'black')+scale_fill_identity()+scale_y_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10), labels = c('0-0.1','0.1-0.2','0.2-0.3','0.3-0.4','0.4-0.5','0.5-0.6','0.6-0.7','0.7-0.8','0.8-0.9','0.9-1'), position = 'right')+ylab('Share of individual occurrence (%)')+theme_void()+theme(plot.margin = margin(5, 0, 5, 0, "cm"),axis.ticks.y = element_line(),axis.text.y = element_text(size=25, angle=-90),axis.title.y = element_text(size=25, angle = -90),plot.title = element_text(size=15, hjust = 0.5),legend.title = element_text(),legend.position = 'none')



matrix_panel_broad_final <- cowplot::plot_grid(plotlist=list(matrix_panel_broad,legend_diag),ncol=2,rel_widths = c(1,0.05))

pdf("plots_first_submission\\SI\\matrix_panel_all.pdf", width     = 25.00,height    = 25.00)
matrix_panel_broad_final
dev.off()