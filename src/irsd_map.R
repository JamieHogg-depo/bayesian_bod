

base <- inner_join(seifa_ra, map, by = c("LGA_Code" = "geography_no")) %>% 
  ggplot(aes(fill = as.factor(IRSD_5), geometry = geometry))+
  theme_void()+
  geom_sf(col = "grey", size = 0.1)+
  geom_sf(data = wa_border, aes(geometry = geometry), 
          colour = "black", fill = NA, size = 0.2)+
  theme(legend.position = "none",
        text = element_text(size = 8),
        plot.title = element_text(margin = margin(0,0,2,0)),
        plot.margin = unit(c(1,1,1,1), "mm"))

# base map_with legend
temp <- base +
  labs(fill = "IRSD")+
  theme(legend.position = "bottom")
base_legend <- get_legend(temp)

# Base map with boxes
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
             c(1,1),
             c(3,3))
out = arrangeGrob(grobs = list(base_boxes, perth_inset, base_legend), 
                                 layout_matrix  = lay)

# Save 
jsave(plot = out, filename = "map_irsd.png", 
      base_folder = "plts/ForPaper", square = F,
      square_size = 1200,
      dpi = 300)
