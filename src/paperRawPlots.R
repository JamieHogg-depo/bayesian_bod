## paperRawPlots

## ASYLL - both Asthma and CHD #### --------------------------------------------

bind_rows(
full_join(raw$asthma_asyll, all_persons$Asthma_ASYLL_Persons, by = c("T_id", "M_id")) %>% 
  left_join(.,pop,by = c("geography_no.x" = "LGA_Code")) %>% 
  mutate(raw_lower = raw_ASYLL.x - 1.96 * raw_SE_ASYLL.x, 
         raw_upper = raw_ASYLL.x + 1.96 * raw_SE_ASYLL.x) %>% 
  group_by(year.x) %>% 
  mutate(N_c = cut_number(N, n = 100, labels = FALSE)) %>% 
  ungroup() %>% 
  rename(raw = raw_ASYLL.x) %>% 
  dplyr::select(point, lower, upper, raw, raw_lower, raw_upper, N_c) %>% 
  mutate(model = "Asthma") %>% 
  filter(raw_upper < 1000),

full_join(raw$chd_asyll, all_persons$CHD_ASYLL_Persons, by = c("T_id", "M_id")) %>% 
  left_join(.,pop,by = c("geography_no.x" = "LGA_Code")) %>% 
  mutate(raw_lower = raw_ASYLL.x - 1.96 * raw_SE_ASYLL, 
         raw_upper = raw_ASYLL.x + 1.96 * raw_SE_ASYLL) %>% 
  group_by(year.x) %>% 
  mutate(N_c = cut_number(N, n = 100, labels = FALSE)) %>% 
  ungroup()%>% 
  rename(raw = raw_ASYLL.x) %>% 
  dplyr::select(point, lower, upper, raw, raw_lower, raw_upper, N_c) %>% 
  mutate(model = "CHD")
) %>% 
  ggplot(aes(y = point, ymin = lower, ymax = upper,
             x = raw, xmin = raw_lower, xmax = raw_upper,
             col = N_c))+
  theme_bw()+
  geom_errorbar(col = "grey")+
  geom_errorbarh(col = "grey")+
  geom_point()+
  geom_abline()+
  labs(y = "Modelled ASYLL",
       x = "Raw ASYLL",
       color = "Population\n(percentiles)")+
  scale_color_viridis_c()+
  theme(legend.position = "bottom",
        text = element_text(size = 8))+
  facet_wrap(.~model, scales = "free")
jsave(filename = paste0("compraw_ASYLL.jpeg"), 
      base_folder = "plts/ForPaper", square = F,
      square_size = 1200,
      dpi = 300)


## Asthma Prevalence ## --------------------------------------------------------

full_join(raw_asthma_3008, all_persons$Asthma_prev_Persons, by = c("lga_name16", "year")) %>% 
  left_join(.,st_drop_geometry(lga$map) %>% dplyr::select(LGA_NAM, LGA_COD), by = c("lga_name16" = "LGA_NAM")) %>% 
  mutate(LGA_COD = as.numeric(LGA_COD)) %>% 
  left_join(.,pop,by = c("LGA_COD" = "LGA_Code")) %>% 
  group_by(year) %>% 
  mutate(N_c = cut_number(N, n = 100, labels = FALSE)) %>% 
  ungroup() %>% 
  ggplot(aes(y = point, ymin = lower, ymax = upper,
             x = raw, xmin = raw_lower, xmax = raw_upper,
             col = N_c))+
  theme_bw()+
  geom_errorbar(col = "grey")+
  geom_errorbarh(col = "grey")+
  geom_point()+
  geom_abline()+
  labs(y = "Modelled prevalence",
       x = "Raw prevalence",
       color = "Population\n(percentiles)")+
  scale_color_viridis_c()+
  theme(legend.position = "bottom",
        text = element_text(size = 8))
jsave(filename = paste0("compraw_Asthma_prev.jpeg"), 
      base_folder = "plts/ForPaper", square = T,
      square_size = 1200,
      dpi = 300)

## Asthma ASYLL ## -------------------------------------------------------------

## point estimates
full_join(raw$asthma_asyll, all_persons$Asthma_ASYLL_Persons, by = c("T_id", "M_id")) %>% 
  left_join(.,pop,by = c("geography_no.x" = "LGA_Code")) %>% 
  mutate(raw_lower = raw_ASYLL.x - 1.96 * raw_SE_ASYLL.x, 
         raw_upper = raw_ASYLL.x + 1.96 * raw_SE_ASYLL.x) %>% 
  group_by(year.x) %>% 
  mutate(N_c = cut_number(N, n = 100, labels = FALSE)) %>% 
  ungroup() %>% 
  ggplot(aes(y = point, ymin = lower, ymax = upper,
             x = raw_ASYLL.x, xmin = raw_lower, xmax = raw_upper,
             col = N_c))+
  theme_bw()+
  geom_errorbar(col = "grey")+
  geom_errorbarh(col = "grey")+
  geom_point()+
  geom_abline()+
  xlim(0,1000)+ # drops 7 LGA by time points
  labs(y = "Modelled ASYLL",
       x = "Raw ASYLL",
       color = "Population\n(percentiles)")+
  scale_color_viridis_c()+
  theme(legend.position = "bottom",
        text = element_text(size = 8))
jsave(filename = paste0("compraw_Asthma_ASYLL.jpeg"), 
      base_folder = "plts/ForPaper", square = T,
      square_size = 1200,
      dpi = 300)

## RSE summary
raw$asthma_asyll %>% 
  mutate(non_zero = raw_SE_ASYLL > 0) %>% 
  group_by(year) %>% 
  summarise(p = mean(non_zero))

## RSE
full_join(raw$asthma_asyll, all_persons$Asthma_ASYLL_Persons, by = c("T_id", "M_id")) %>% 
  dplyr::select(RSE, raw_RSE_ASYLL, M_id, T_id) %>%
  pivot_longer(-c(M_id, T_id)) %>% 
  ggplot(aes(y = value, col = name, x = M_id)) + 
  geom_point()+
  facet_grid(.~T_id)

## CHD ASYLL ## ----------------------------------------------------------------

## point estimates
full_join(raw$chd_asyll, all_persons$CHD_ASYLL_Persons, by = c("T_id", "M_id")) %>% 
  left_join(.,pop,by = c("geography_no.x" = "LGA_Code")) %>% 
  mutate(raw_lower = raw_ASYLL.x - 1.96 * raw_SE_ASYLL, 
         raw_upper = raw_ASYLL.x + 1.96 * raw_SE_ASYLL) %>% 
  group_by(year.x) %>% 
  mutate(N_c = cut_number(N, n = 100, labels = FALSE)) %>% 
  ungroup() %>% 
  ggplot(aes(y = point, ymin = lower, ymax = upper,
             x = raw_ASYLL.x, xmin = raw_lower, xmax = raw_upper,
             col = N_c))+theme_bw()+
  geom_errorbar(col = "grey")+
  geom_errorbarh(col = "grey")+
  geom_point()+
  geom_abline()+
  labs(y = "Modelled ASYLL",
       x = "Raw ASYLL",
       color = "Population\n(percentiles)")+
  scale_color_viridis_c()+
  theme(legend.position = "bottom",
        text = element_text(size = 8))
jsave(filename = paste0("compraw_CHD_ASYLL.jpeg"), 
      base_folder = "plts/ForPaper", square = T,
      square_size = 1200,
      dpi = 300)

## RSE summary
raw$chd_asyll %>% 
  mutate(non_zero = raw_SE_ASYLL > 0) %>% 
  group_by(year) %>% 
  summarise(p = mean(non_zero))
# percent of non-zero ASYLL

## RSE
full_join(raw$chd_asyll, all_persons$CHD_ASYLL_Persons, by = c("T_id", "M_id")) %>% 
  dplyr::select(RSE, raw_RSE_ASYLL, M_id, year.x) %>%
  pivot_longer(-c(M_id, year.x)) %>% 
  ggplot(aes(y = value, col = name, x = M_id)) + 
  geom_point()+
  facet_wrap(.~year.x)+
  labs(y = "RSE ASYLL",
       x = "")

## CHD ASYLD ## ----------------------------------------------------------------

## point estimates
full_join(raw$chd_asyld, all_persons$CHD_ASYLD_Persons, by = c("T_id", "M_id", "year")) %>% 
  mutate(raw_lower = raw_ASYLD - 1.96 * raw_SE, 
         raw_upper = raw_ASYLD + 1.96 * raw_SE) %>% 
  mutate(raw_lower = ifelse(raw_lower < 0, 0, raw_lower)) %>% 
  group_by(year) %>% 
  mutate(N_c = cut_number(N, n = 100, labels = FALSE)) %>% 
  ungroup() %>% 
  ggplot(aes(y = point, ymin = lower, ymax = upper,
             x = raw_ASYLD, xmin = raw_lower, xmax = raw_upper,
             col = N_c))+
  theme_bw()+
  geom_abline()+
  geom_errorbar(col = "grey")+
  geom_errorbarh(col = "grey")+
  geom_point()+
  labs(y = "Modelled ASYLD",
       x = "Raw ASYLD",
       color = "Population\n(percentiles)") +
  scale_color_viridis_c()+
  theme(legend.position = "bottom",
        text = element_text(size = 8))
jsave(filename = paste0("compraw_CHD_ASYLD.jpeg"), 
      base_folder = "plts/ForPaper", square = T,
      square_size = 1200,
      dpi = 300)

## RSE summary
raw$chd_asyld %>% 
  mutate(non_zero = Raw_SE_ASYLD > 0) %>% 
  group_by(data_year) %>% 
  summarise(p = mean(non_zero))
# percent of non-zero ASYLL
  
  ## RSE
  full_join(raw$chd_asyld, all_persons$CHD_ASYLD_Persons, by = c("T_id", "M_id")) %>% 
    dplyr::select(RSE, Raw_RSE_ASYLD, M_id, data_year) %>%
    pivot_longer(-c(M_id, data_year)) %>% 
    ggplot(aes(y = log(value), col = name, x = M_id)) + 
    geom_point()+
    facet_wrap(.~data_year)+
    labs(y = "Log RSE ASYLD",
         x = "")
  
## CHD prev ## -----------------------------------------------------------------

## point estimates
full_join(all_persons$CHD_prev_Persons, raw$chd_prev, 
          by = c("geography_no", "MT_id", "year", "LGA_NAME16")) %>% 
  mutate(raw_lower = raw_prev.y - 1.96 * SE, 
         raw_upper = raw_prev.y + 1.96 * SE) %>% 
  group_by(year) %>% 
  mutate(N_c = cut_number(N.x, n = 100, labels = FALSE)) %>% 
  ungroup() %>% 
  ggplot(aes(y = point, ymin = lower, ymax = upper,
             x = raw_prev.y, xmin = raw_lower, xmax = raw_upper,
             col = N_c))+theme_bw()+
  geom_errorbar(col = "grey")+
  geom_errorbarh(col = "grey")+
  geom_point()+
  geom_abline()+
  labs(y = "Modelled prevalence",
       x = "Raw prevalence",
       color = "Population\n(percentiles)")+
  scale_color_viridis_c()+
  theme(legend.position = "bottom",
        text = element_text(size = 8))
jsave(filename = paste0("compraw_CHD_prev.jpeg"), 
      base_folder = "plts/ForPaper", square = T,
      square_size = 1200,
      dpi = 300)

## Compare all - boxplots ## ---------------------------------------------------

bind_rows(
  full_join(raw$chd_asyll, all_persons$CHD_ASYLL_Persons, by = c("T_id", "M_id")) %>% 
    dplyr::select(raw_ASYLL.x, point) %>% 
    setNames(c("Raw", "Bayesian")) %>% 
    pivot_longer(everything()) %>% 
    mutate(condition = "CHD",
           metric = "ASYLL"),
  
  full_join(raw$chd_asyld, all_persons$CHD_ASYLD_Persons, by = c("T_id", "M_id")) %>% 
    dplyr::select(raw_ASYLD, point) %>% 
    setNames(c("Raw", "Bayesian")) %>% 
    pivot_longer(everything()) %>% 
    mutate(condition = "CHD",
           metric = "ASYLD"),
  
  all_persons$CHD_prev_Persons %>% 
    dplyr::select(raw_prev, point) %>% 
    setNames(c("Raw", "Bayesian")) %>% 
    pivot_longer(everything()) %>% 
    mutate(condition = "CHD",
           metric = "Proportion"),
  
  all_persons$Asthma_ASYLD_Persons %>% 
    dplyr::select(point) %>% 
    setNames("value") %>% 
    mutate(condition = "Asthma",
           metric = "ASYLD",
           name = "Bayesian"),
  
  full_join(raw$asthma_asyll, all_persons$Asthma_ASYLL_Persons, by = c("T_id", "M_id")) %>% 
    dplyr::select(raw_ASYLL.x, point) %>% 
    setNames(c("Raw", "Bayesian")) %>% 
    pivot_longer(everything()) %>% 
    mutate(condition = "Asthma",
           metric = "ASYLL"),#%>% 
    #arrange(desc(value)) %>% slice(20:n()),
  
  full_join(raw_asthma_3008, all_persons$Asthma_prev_Persons, by = c("lga_name16", "year")) %>% 
    dplyr::select(raw, point) %>% 
    setNames(c("Raw", "Bayesian")) %>% 
    pivot_longer(everything()) %>% 
    mutate(condition = "Asthma",
           metric = "Proportion")
  
) %>% 
  ggplot(aes(y = log(value+1), x = name, fill = name))+
  geom_boxplot()+
  facet_wrap(condition~metric, scales = "free")+
  theme_bw()+
  theme(legend.position = "none",
        text = element_text(size = 8))+
  labs(y = "Point estimates (log scale)",
       x = "")
jsave(filename = paste0("compraw_ALL.jpeg"), 
      base_folder = "plts/ForPaper", square = F,
      square_size = 1200,
      dpi = 300)

## Compare CHD YLDS ## ---------------------------------------------------------

CHD_YLD_1016 %>% 
  ggplot(aes(y = point,
             x = YLD))+
  theme_bw()+
  geom_point()+
  theme(text = element_text(size = 8))+
  geom_abline()+
  labs(y = "YLDs (Bayesian)",
       x = "YLDs (Raw)")
jsave(filename = paste0("compraw_chdyld.jpeg"), 
      base_folder = "plts/ForPaper", square = F,
      square_size = 1200,
      dpi = 300)

## END SCRIPT ## ---------------------------------------------------------------