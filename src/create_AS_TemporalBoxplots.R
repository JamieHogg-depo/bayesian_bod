## -----------------------------------------------------------------------------
## MAPS ## ---------------------------------------------------------------------
## -----------------------------------------------------------------------------

source("src/ms.R")

#### ---------------------------------------------------------------------------

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
  df_temp <- left_join(asyll_list[[j]],lga$seifa_ra, 
                       by = c("geography_no" = "LGA_Code", "year" = "Year"))

# get new RA names
ra_levs <- str_replace(unique(df_temp$RA_Name), " ", "\n")

# select temporary dataset
df_temp <- df_temp %>% 
  mutate(RA_Name = factor(fct_relabel(RA_Name, ~ str_replace(., " ", "\n"))),
         RA_Name = fct_relevel(RA_Name, "Major\nCities")) %>% 
  mutate(IRSD_5 = factor(IRSD_5, 
                         levels = 1:5,
                         labels = c("Most\ndisadvantaged", 
                                    as.character(2:4), 
                                    "Least\ndisadvantaged")))

# Plot 1
irsd_plt <- df_temp %>% 
  filter(year != "2020") %>% 
  ggplot(aes(x = point, y = as.factor(year), fill = as.factor(IRSD_5)))+
  theme_bw()+
  geom_boxplot()+
  labs(y = "",
       x = "", 
       fill = "", 
       title = "Socioeconomic\nstatus")+
  scale_y_discrete(limits=rev)+
  theme(legend.position = "right",
        text = element_text(size = 10))

# Plot 2
ra_plt <- df_temp %>% 
  filter(year != "2020") %>% 
  ggplot(aes(x = point, y = as.factor(year), fill = as.factor(RA_Name)))+
  theme_bw()+
  geom_boxplot()+
  labs(y = "",
       x = "Age standardised YLLs", 
       fill = "",
       title = "Remoteness")+
  scale_y_discrete(limits=rev)+
  theme(legend.position = "right",
        text = element_text(size = 10))

# create full plot
lay <- rbind(c(1,1),
             c(2,2))
full_plot <- arrangeGrob(grobs = list(irsd_plt, ra_plt), 
                                  layout_matrix  = lay)
jsave(plot = full_plot, filename = paste0("temporalbox_", file_index, ".png"), 
      base_folder = "plts", square = F, ratio = c(9,6))

}

## END SCRIPT ## --------------------------------------------------------------