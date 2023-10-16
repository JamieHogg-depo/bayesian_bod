## -----------------------------------------------------------------------------
## Age-standardised maps ## ----------------------------------------------------
## -----------------------------------------------------------------------------

source("src/ms.R")

## -----------------------------------------------------------------------------

for(j in 1:length(all_persons)){

# specs
sp <- unlist(str_split(names(all_persons)[j], "_"))
condition <- sp[1]
metric <- sp[2]
sex <- sp[3]
file_index <- paste0(condition, "_", sex, "_", metric)

# Progress
message("Condition: ", condition, "\nSex: ", sex, "\nMetric: ", metric)
  
# select temporary dataset
df_temp <- all_persons[[j]] %>% 
  mutate(cisize = upper - lower)

# create map data list
if(j %in% c(5,6)){
map_temp <- left_join(df_temp,map, by = c("lga_name16" = "LGA_NAM")) %>% 
  st_as_sf() %>%
  st_transform(4326) 
}else{
map_temp <- left_join(df_temp,map, by = c("geography_no" = "ggrphy_")) %>% 
  st_as_sf() %>%
  st_transform(4326) 
}

# LGA_CHD_Female ASYLL
year_plt_list <- list()
seq_years <- unique(df_temp$year)

## Point estimates ## ----------------------------------------------------------

if(metric == "ASYLL"){
  full_inset_plt <- createTimeMap(map_temp, "E", cut_prob = 0.04)
  cat_plot <- createCaterpillarPlot(map_temp, "E", cut_prob = 0.04)
}else if(metric == "ASYLD"){
  full_inset_plt <- createTimeMap(map_temp, "B", cut_prob = 0.04)
  cat_plot <- createCaterpillarPlot(map_temp, "B", cut_prob = 0.04)
}else{
  full_inset_plt <- createTimeMap(map_temp, "D", cut_prob = 0.04) # prevalence
  cat_plot <- createCaterpillarPlot(map_temp, "D", cut_prob = 0.04) # prevalence
}
jsave(plot = full_inset_plt, filename = paste0("map_", file_index, ".jpeg"), 
      base_folder = "plts/ForPaper", square = F,
      square_size = 1200,
      dpi = 300)
jsave(plot = cat_plot, filename = paste0("cat_", file_index, ".jpeg"), 
      base_folder = "plts/ForPaper", square = F,
      square_size = 1200,
      dpi = 300)

## Uncertainty ## --------------------------------------------------------------

# Progress
message("Width of interval")

col_range <- unname(quantile(map_temp$cisize, probs = c(0.02, 0.98)))

for(t in 1:length(seq_years)){
  
  # base map - no legend
  base <- map_temp %>% 
    filter(year == seq_years[t]) %>% 
    ggplot(aes(fill = cisize))+
    theme_void()+
    geom_sf(col = "grey", size = 0.1)+
    geom_sf(data = wa_border, aes(geometry = geometry), 
            colour = "black", fill = NA, size = 0.2)+
    scale_fill_viridis_c(begin = 0, end = 1, 
                         direction = -1,
                         option = "D",
                         limits = col_range,
                         oob = squish)+
    theme(legend.position = "none",
          text = element_text(size = 8),
          plot.title = element_text(margin = margin(0,0,2,0)),
          plot.margin = unit(c(1,1,1,1), "mm"))
  
  # base map_with legend
  temp <- base +
    labs(fill = "Width of 95% HDPI")+
    guides(fill = guide_colourbar(barwidth = 15, 
                                  title.position = "top",
                                  title.hjust = 0.5))+
    theme(legend.position = "bottom")
  base_legend <- get_legend(temp)
  
  # Base map with boxes
  #base_boxes <- base_legend + addBoxLabel(1, color = "green", size = 0.2)
  base_boxes <- base + addBoxLabel(1, color = "black", size = 0.2)
  
  # Create list of insets
  perth_inset <- base +
    xlim(lims$xmin[1], lims$xmax[1]) +
    ylim(lims$ymin[1], lims$ymax[1]) +
    theme(panel.border = element_rect(colour = "black", size=0.2, fill=NA),
          plot.title = element_text(margin = margin(0,0,2,0)),
          plot.margin = unit(c(1,1,1,1), "mm"))
  
  # create full map
  lay <- rbind(c(2,1),
               c(1,1))
  year_plt_list[[t]] = arrangeGrob(grobs = list(base_boxes, perth_inset), 
                                   layout_matrix  = lay,
                                   top = textGrob(seq_years[t],gp=gpar(fontsize=8)))
  
  # Progress 
  message("---- Year: ", seq_years[t])
  rm(lay, perth_inset, base_boxes, base, temp)
  
}

# Final plot
lay <- rbind(c(1,2,3),
             c(4,5,6),
             c(7,7,7))
full_inset_plt <- arrangeGrob(grobs = c(year_plt_list, list(base_legend)), 
                              layout_matrix  = lay)
jsave(plot = full_inset_plt, 
      filename = paste0("mapcisize_", file_index, ".jpeg"), 
      base_folder = "plts/ForPaper", square = F,
      square_size = 1200,
      dpi = 300)

## Exceedance probability ## ---------------------------------------------------

# Progress
message("Exceedance probability")

for(t in 1:length(seq_years)){
  
  # base map - no legend
  base <- map_temp %>% 
    filter(year == seq_years[t]) %>% 
    ggplot(aes(fill = EP))+
    theme_void()+
    geom_sf(col = "grey", size = 0.1)+
    geom_sf(data = wa_border, aes(geometry = geometry), 
            colour = "black", fill = NA, size = 0.2)+
    scale_fill_gradientn(colors = c("#008837", "#a6dba0", 
                                    "white","white", "white","white","white", 
                                    "#c2a5cf", "#7b3294"),
                         limits = c(-0.0000001,1.0000001),
                         #oob = squish,
                         #trans = "logit",
                         breaks = c(0,0.1,0.2,0.25,0.5,0.75,0.8,0.9,1),
                         labels = as.character(c(0,"", 0.2,"", 0.5,"", 0.8, "",1)))+
    theme(legend.position = "none",
          text = element_text(size = 8),
          plot.title = element_text(margin = margin(0,0,2,0)),
          plot.margin = unit(c(1,1,1,1), "mm"))
  
  # base map_with legend
  temp <- base +
    labs(fill = paste0("Exceedance probability (", metric, ")"))+
    guides(fill = guide_colourbar(barwidth = 15, 
                                  title.position = "top",
                                  title.hjust = 0.5))+
    theme(legend.position = "bottom")
  base_legend <- get_legend(temp)
  
  # Base map with boxes
  #base_boxes <- base_legend + addBoxLabel(1, color = "green", size = 0.2)
  base_boxes <- base + addBoxLabel(1, color = "black", size = 0.2)
  
  # Create list of insets
  perth_inset <- base +
    xlim(lims$xmin[1], lims$xmax[1]) +
    ylim(lims$ymin[1], lims$ymax[1]) +
    theme(panel.border = element_rect(colour = "black", size=0.2, fill=NA),
          plot.title = element_text(margin = margin(0,0,2,0)),
          plot.margin = unit(c(1,1,1,1), "mm"))
  
  # create full map
  lay <- rbind(c(2,1),
               c(1,1))
  year_plt_list[[t]] = arrangeGrob(grobs = list(base_boxes, perth_inset), 
                                   layout_matrix  = lay,
                                   top = textGrob(seq_years[t],gp=gpar(fontsize=8)))
  
  # Progress 
  message("---- Year: ", seq_years[t])
  rm(lay, perth_inset, base_boxes, base, temp)
  
}

# Final plot
lay <- rbind(c(1,2,3),
             c(4,5,6),
             c(7,7,7))
full_inset_plt<- arrangeGrob(grobs = c(year_plt_list, list(base_legend)), layout_matrix  = lay)

# Save 
jsave(plot = full_inset_plt, filename = paste0("mapEP_", file_index, ".jpeg"), 
      base_folder = "plts/ForPaper", square = F,
      square_size = 1200,
      dpi = 300)

}

## Modelled baseline - CHD ASYLD ## --------------------------------------------

map_temp <- left_join(CHD_ASYLD20231003,map, by = "LGA_NAM") %>% 
  cbind(.,dplyr::select(all_persons$CHD_ASYLD_Persons, year)) %>% 
  st_as_sf() %>%
  st_transform(4326) 

year_plt_list <- list()
seq_years <- unique(map_temp$year)

for(t in 1:length(seq_years)){
  
  # base map - no legend
  base <- map_temp %>% 
    filter(year == seq_years[t]) %>% 
    ggplot(aes(fill = EP))+
    theme_void()+
    geom_sf(col = "grey", size = 0.1)+
    geom_sf(data = wa_border, aes(geometry = geometry), 
            colour = "black", fill = NA, size = 0.2)+
    scale_fill_gradientn(colors = c("#008837", "#a6dba0", 
                                    "white","white", "white","white","white", 
                                    "#c2a5cf", "#7b3294"),
                         limits = c(-0.0000001,1.0000001),
                         #oob = squish,
                         #trans = "logit",
                         breaks = c(0,0.1,0.2,0.25,0.5,0.75,0.8,0.9,1),
                         labels = as.character(c(0,"", 0.2,"", 0.5,"", 0.8, "",1)))+
    theme(legend.position = "none",
          text = element_text(size = 8),
          plot.title = element_text(margin = margin(0,0,2,0)),
          plot.margin = unit(c(1,1,1,1), "mm"))
  
  # base map_with legend
  temp <- base +
    labs(fill = paste0("Exceedance probability (ASYLD)"))+
    guides(fill = guide_colourbar(barwidth = 15, 
                                  title.position = "top",
                                  title.hjust = 0.5))+
    theme(legend.position = "bottom")
  base_legend <- get_legend(temp)
  
  # Base map with boxes
  #base_boxes <- base_legend + addBoxLabel(1, color = "green", size = 0.2)
  base_boxes <- base + addBoxLabel(1, color = "black", size = 0.2)
  
  # Create list of insets
  perth_inset <- base +
    xlim(lims$xmin[1], lims$xmax[1]) +
    ylim(lims$ymin[1], lims$ymax[1]) +
    theme(panel.border = element_rect(colour = "black", size=0.2, fill=NA),
          plot.title = element_text(margin = margin(0,0,2,0)),
          plot.margin = unit(c(1,1,1,1), "mm"))
  
  # create full map
  lay <- rbind(c(2,1),
               c(1,1))
  year_plt_list[[t]] = arrangeGrob(grobs = list(base_boxes, perth_inset), 
                                   layout_matrix  = lay,
                                   top = textGrob(seq_years[t],gp=gpar(fontsize=8)))
  
  # Progress 
  message("---- Year: ", seq_years[t])
  rm(lay, perth_inset, base_boxes, base, temp)
  
}

# Final plot
lay <- rbind(c(1,2,3),
             c(4,5,6),
             c(7,7,7))
full_inset_plt<- arrangeGrob(grobs = c(year_plt_list, list(base_legend)), layout_matrix  = lay)

# Save 
jsave(plot = full_inset_plt, filename = paste0("mapEP_CHD_Persons_ASYLD_mb.jpeg"), 
      base_folder = "plts/ForPaper", square = F,
      square_size = 1200,
      dpi = 300)

## END SCRIPT ## --------------------------------------------------------------