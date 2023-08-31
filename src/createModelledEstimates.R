####
## Create ModelledEstimates
####

# create empty list
out_ll <- list()

# Wrangle each dataset
out_ll[[1]] <- all_persons$CHD_ASYLL_Persons %>% 
  dplyr::select(metric, year, geography_no, point, lower, upper, se, RSE, EP) %>% 
  mutate(condition = "Coronary heart disease") %>% 
  relocate(condition) %>% 
  rename(lga_code_16 = geography_no) %>% 
  mutate(metric = str_replace(metric, " / 100,000", " per 100000"))

out_ll[[2]] <- all_persons$CHD_ASYLD_Persons %>% 
  dplyr::select(metric, year, geography_no, point, lower, upper, se, RSE, EP) %>% 
  mutate(condition = "Coronary heart disease") %>% 
  relocate(condition) %>% 
  rename(lga_code_16 = geography_no) %>% 
  mutate(metric = str_replace(metric, "/100,000 6yr comp", " per 100000"))

out_ll[[3]] <- all_persons$Asthma_ASYLL_Persons %>% 
  dplyr::select(metric, year, geography_no, point, lower, upper, se, RSE, EP) %>% 
  mutate(condition = "Asthma") %>% 
  relocate(condition) %>% 
  rename(lga_code_16 = geography_no) %>% 
  mutate(metric = str_replace(metric, " / 100,000", " per 100000"))

out_ll[[4]] <- all_persons$Asthma_ASYLD_Persons %>% 
  left_join(.,lga$map, by = c("lga_name16" = "LGA_NAM")) %>% 
  dplyr::select(metric, year, LGA_COD, point, lower, upper, se, RSE, EP) %>% 
  mutate(lga_code_16 = as.numeric(LGA_COD)) %>% 
  mutate(condition = "Asthma") %>% 
  relocate(condition, metric, year, lga_code_16) %>% 
  dplyr::select(-LGA_COD) %>% 
  mutate(metric = str_replace(metric, "/100,000", " per 100000"))

out_ll[[5]] <- all_persons$Asthma_prev_Persons %>% 
  left_join(.,lga$map, by = c("lga_name16" = "LGA_NAM")) %>% 
  dplyr::select(metric, year, LGA_COD, point, lower, upper, se, RSE, EP) %>% 
  mutate(lga_code_16 = as.numeric(LGA_COD)) %>% 
  mutate(condition = "Asthma") %>% 
  relocate(condition, metric, year, lga_code_16) %>% 
  dplyr::select(-LGA_COD)

# Create csv
bind_rows(out_ll) %>% 
  left_join(.,pop,by = c("lga_code_16" = "LGA_Code")) %>% 
  rename(ERP = N) %>% 
  write.csv(., "ModelledEstimates.csv")

## END SCRIPT ## ---------------------------------------------------------------
