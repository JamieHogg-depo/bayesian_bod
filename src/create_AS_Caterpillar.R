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
df_temp <- left_join(asyll_list[[j]],lga$seifa_ra, by = c("geography_no" = "LGA_Code", "year" = "Year")) 

# create caterpillar plot
out <- df_temp %>% 
  group_by(year) %>% 
  mutate(x_id = order(point)) %>% ungroup() %>% 
  ggplot(aes(y = point, ymin = lower, ymax = upper,
             x = M_id))+
  theme_bw()+
  geom_errorbar(color = "grey")+
  geom_point()+
  facet_wrap(.~year)+
  labs(y = "ASYLL",
       x = "")

jsave(plot = out, filename = paste0("caterpillar_", file_index, "_ASYLL.png"), 
      base_folder = "plts", square = T)

}

## END SCRIPT ## --------------------------------------------------------------