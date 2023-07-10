## -----------------------------------------------------------------------------
## MAPS ## ---------------------------------------------------------------------
## -----------------------------------------------------------------------------

source("src/ms.R")

## Age-standardised YLLs ## ----------------------------------------------------

for(j in 1:length(ASYLL6y_list)){
  
  # specs
  sp <- unlist(str_split(names(ASYLL6y_list)[j], "_"))
  condition <- sp[1]
  metric <- sp[2]
  sex <- sp[3]
  file_index <- paste0(condition, "_", sex, "_", metric)
  
  # Progress
  message("Condition: ", condition, "\nSex: ", sex, "\nMetric: ", metric)
  
  # select temporary dataset
  df_temp <- left_join(ASYLL6y_list[[j]],lga$seifa_ra, by = c("geography_no" = "LGA_Code", "year" = "Year")) 
  
  # create plot
  df_temp %>% 
    ggplot(aes(y = point,
               x = year, 
               group = M_id,
               col = EP))+
    theme_bw()+
    geom_line()+
    geom_point()+
    scale_color_gradientn(colors = c("#008837", "#a6dba0", "grey","grey","grey", "#c2a5cf", "#7b3294"),
                          limits = c(-0.0000001,1.0000001),
                          breaks = c(0,0.2,0.25,0.5,0.75,0.8,1),
                          labels = as.character(c(0,0.2,"",0.5,"",0.8,1)))+
    labs(y = "Posterior medians",
         x = "",
         col = "Exceedance\nprobability")+
    theme(legend.position = "bottom")
  
  jsave(filename = paste0("spaghetti_", file_index, "_ASYLL.png"), 
        base_folder = "plts", square = T)
  
}

## END SCRIPT ## --------------------------------------------------------------