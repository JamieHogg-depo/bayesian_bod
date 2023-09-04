

# temp data
map_temp <- full_join(raw$chd_asyld, all_persons$CHD_ASYLD_Persons, by = c("T_id", "M_id")) %>% 
  left_join(.,map, by = c("geography_no.x" = "ggrphy_")) %>% 
  st_as_sf() %>%
  st_transform(4326) #%>% 
  #mutate(raw_ASYLD = ifelse(Raw_RSE_ASYLD > 50, NA, raw_ASYLD),
         #point = ifelse(RSE > 50, NA, point))

year_plt_list <- list()
seq_years <- unique(map_temp$data_year)
seq_years <- seq_years[!is.na(seq_years)]

# range of posterior medians
cut_prob = 0.04
lower_cut_prob = cut_prob/2
col_range <- unname(quantile(map_temp$point, probs = c(lower_cut_prob, 1-lower_cut_prob)))

## Raw ## ----------------------------------------------------------------------
for(t in 1:length(seq_years)){
  
  # base map - no legend
  base <- map_temp %>% 
    filter(data_year == seq_years[t]) %>% 
    ggplot(aes(fill = raw_ASYLD))+
    theme_void()+
    geom_sf(col = "grey", size = 0.1)+
    geom_sf(data = wa_border, aes(geometry = geometry), 
            colour = "black", fill = NA, size = 0.2)+
    scale_fill_viridis_c(begin = 0, end = 1, 
                         direction = -1,
                         option = "B", limits = col_range,
                         oob = squish)+
    theme(legend.position = "none",
          text = element_text(size = 8),
          plot.title = element_text(margin = margin(0,0,2,0)),
          plot.margin = unit(c(1,1,1,1), "mm"))
  
  # base map_with legend
  temp <- base +
    labs(fill = "ASYLD")+
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
  
}

# Final plot
lay <- rbind(c(1,2,3),
             c(4,5,6),
             c(7,7,7))
out_r <- arrangeGrob(grobs = c(year_plt_list, list(base_legend)), layout_matrix  = lay)
jsave(plot = out_r, filename = paste0("map_CHD_Persons_ASYLD_raw.png"), 
      base_folder = "plts/ForPaper", square = F,
      square_size = 1200,
      dpi = 300)

## Create map ## ---------------------------------------------------------------


