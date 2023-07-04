## -----------------------------------------------------------------------------
## Asthma maps ## --------------------------------------------------------------
## -----------------------------------------------------------------------------

source("src/ms.R")

## Age-standardised YLDs ## ----------------------------------------------------

for(j in 1:length(ATHYLD_list)){
  
# specs
sp <- unlist(str_split(names(ATHYLD_list)[j], "_"))
condition <- sp[1]
metric <- sp[2]
sex <- sp[3]
file_index <- paste0(condition, "_", sex, "_", metric)

# Progress
message("Condition: ", condition, "\nSex: ", sex, "\nMetric: ", metric)
  
# select temporary dataset
df_temp <- ATHYLD_list[[j]]

# create map data list
map_temp <- left_join(df_temp,map, by = c("lga_name16" = "LGA_NAME16")) %>% 
  st_as_sf() %>%
  st_transform(4326)

# LGA_CHD_Female ASYLL
year_plt_list <- list()
seq_years <- unique(df_temp$year)

## loop over years ## ---------------------------------------------------------
for(t in 1:length(seq_years)){
  
# range of posterior medians
col_range <- range(df_temp$point)
  
# base map - no legend
base <- map_temp %>% 
   filter(year == seq_years[t]) %>% 
   ggplot(aes(fill = point))+
   theme_void()+
   geom_sf(col = "grey", size = 0.1)+
   geom_sf(data = wa_border, aes(geometry = geometry), 
           colour = "black", fill = NA, size = 0.2)+
   scale_fill_viridis_c(begin = 0, end = 1, 
                        direction = -1,
                        option = "B", limits = col_range)+
   theme(legend.position = "none",
        text = element_text(size = 10),
        plot.title = element_text(margin = margin(0,0,2,0)),
        plot.margin = unit(c(1,1,1,1), "mm"))

# base map_with legend
temp <- base +
    labs(fill = metric)+
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
year_plt_list[[t]] <- arrangeGrob(grobs = list(base_boxes, perth_inset), 
                                   layout_matrix  = lay,
                                   top = textGrob(seq_years[t],gp=gpar(fontsize=10)))

# Progress 
message("---- Year: ", seq_years[t])

}

# Final plot
lay <- rbind(c(1,2,3),
             c(4,5,6),
             c(7,7,7))
full_inset_plt <- arrangeGrob(grobs = c(year_plt_list, list(base_legend)), layout_matrix  = lay)
jsave(plot = full_inset_plt, filename = paste0("map_", file_index, ".png"), base_folder = "plts", square = F)

}

## END SCRIPT ## --------------------------------------------------------------