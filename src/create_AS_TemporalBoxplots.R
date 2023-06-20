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

# Plot 1
irsd_plt <- df_temp %>% 
  filter(year != "2020") %>% 
  ggplot(aes(x = point, y = as.factor(year), fill = as.factor(IRSD_5)))+
  theme_bw()+
  geom_boxplot()+
  labs(y = "",
       x = "Age standardised YLLs", 
       fill = "", 
       title = "IRSD (quintiles)")+
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
jsave(plot = full_plot, filename = paste0("temporalbox_", file_index, "_ASYLL.png"), 
      base_folder = "plts", square = F, ratio = c(9,6))

}

## END SCRIPT ## --------------------------------------------------------------