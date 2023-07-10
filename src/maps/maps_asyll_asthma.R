## -----------------------------------------------------------------------------
## Asthma maps ## --------------------------------------------------------------
## -----------------------------------------------------------------------------

source("src/ms.R")

## -----------------------------------------------------------------------------

for(j in 1:3){
  
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

## loop over years ## ----------------------------------------------------------
full_inset_plt <- createTimeMap(map_temp, "D")
jsave(plot = full_inset_plt, filename = paste0("map_", file_index, ".png"), base_folder = "plts", square = F)

}

## END SCRIPT ## --------------------------------------------------------------