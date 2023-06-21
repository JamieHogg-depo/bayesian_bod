## -----------------------------------------------------------------------------
## MAPS ## ---------------------------------------------------------------------
## -----------------------------------------------------------------------------

source("src/ms.R")

## Age-standardised YLLs ## ----------------------------------------------------

for(j in 1:length(asyll_list)){

# get specs
temp <- strsplit(str_replace(names(asyll_list)[j], " ASYLL", ""), split= '_', fixed=TRUE)
condition <- temp[[1]][2]
sex <- temp[[1]][3]
file_index <- str_extract(names(asyll_list)[j], "(?<=LGA_).*?(?=\\s)")
rm(temp)

# Progress
message("Condition: ", condition, "\nSex: ", sex)

# select temporary dataset
df_temp <- asyll_list[[j]]

# create map data list
map_temp <- left_join(df_temp,map, by = "geography_no") %>% 
  st_as_sf() %>%
  st_transform(4326)

# WA outline
wa_border <- map %>% 
  summarise(geometry = st_union(geometry)) %>% 
  st_as_sf() %>%
  st_transform(4326)

# LGA_CHD_Female ASYLL
year_plt_list <- list()
seq_years <- sort(unique(df_temp$year))

## loop over years ## ---------------------------------------------------------
for(t in 1:6){
  
# base map - no legend
base <- map_temp %>% 
   filter(T_id == t) %>% 
   ggplot(aes(fill = EP))+
   theme_void()+
   geom_sf(col = "grey", size = 0.1)+
   geom_sf(data = wa_border, aes(geometry = geometry), 
           colour = "black", fill = NA, size = 0.2)+
   scale_fill_viridis_c(begin = 0, end = 1, 
                        direction = -1,
                        option = "B", limits = c(0,1))+
   theme(legend.position = "none",
        text = element_text(size = 10),
        plot.title = element_text(margin = margin(0,0,2,0)),
        plot.margin = unit(c(1,1,1,1), "mm"))

# base map_with legend
temp <- base +
    labs(fill = "Exceedance probability")+
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
                                   top = textGrob(as.character(seq_years[t]),gp=gpar(fontsize=10)))

# Progress 
message("---- Year: ", seq_years[t])

}

# Final plot
lay <- rbind(c(1,2,3),
             c(4,5,6),
             c(7,7,7))
full_inset_plt <- arrangeGrob(grobs = c(year_plt_list, list(base_legend)), layout_matrix  = lay)
jsave(plot = full_inset_plt, filename = paste0("mapEP_", file_index, "_ASYLL.png"), base_folder = "plts", square = F)

}

## END SCRIPT ## --------------------------------------------------------------