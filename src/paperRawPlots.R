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
  labs(y = "Bayesian ASYLL",
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
jsave(filename = paste0("Figure2.png"), 
      base_folder = "plts/ForPaper", square = F,
      square_size = 1200,
      dpi = 300)
jsave(filename = paste0("Figure2.tiff"), 
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
  labs(y = "Bayesian Prevalence",
       x = "Raw Prevalence",
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
  labs(y = "Bayesian ASYLL",
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

## Asthma Mortality ## ---------------------------------------------------------

## point estimates
mort3110$Asthma_Mort %>% 
  filter(raw_prev > 0) %>% 
  mutate(raw_se = (raw_prev*RSE_prev_raw)/100,
         raw_lower = raw_prev - 1.96*raw_se,
         raw_lower = ifelse(raw_lower < 0, 0, raw_lower),
         raw_upper = raw_prev + 1.96*raw_se) %>% 
  left_join(.,pop,by = c("geography_no" = "LGA_Code")) %>% 
  group_by(year) %>% 
  mutate(N_c = cut_number(N, n = 100, labels = FALSE)) %>% 
  ungroup() %>% 
  ggplot(aes(y = 100000*point, ymin = 100000*lower, ymax = 100000*upper,
             x = 100000*raw_prev, xmin = 100000*raw_lower, xmax = 100000*raw_upper,
             col = N_c))+
  theme_bw()+
  geom_errorbar(col = "grey")+
  geom_errorbarh(col = "grey")+
  geom_point()+
  geom_abline()+
  #xlim(0,10)+ # drops 7 LGA by time points
  labs(y = "Bayesian Mortality (per 100,000)",
       x = "Raw Mortality (per 100,000)",
       color = "Population\n(percentiles)")+
  scale_color_viridis_c()+
  theme(legend.position = "bottom",
        text = element_text(size = 8))
jsave(filename = paste0("compraw_Asthma_Mort.jpeg"), 
      base_folder = "plts/ForPaper", square = T,
      square_size = 1200,
      dpi = 300)

## CHD Mortality ## ---------------------------------------------------------

## point estimates
mort3110$CHD_Mort %>% 
  filter(raw_prev > 0) %>% 
  mutate(raw_se = (raw_prev*RSE_prev_raw)/100,
         raw_lower = raw_prev - 1.96*raw_se,
         raw_lower = ifelse(raw_lower < 0, 0, raw_lower),
         raw_upper = raw_prev + 1.96*raw_se) %>% 
  left_join(.,pop,by = c("geography_no" = "LGA_Code")) %>% 
  group_by(T_id) %>% 
  mutate(N_c = cut_number(N, n = 100, labels = FALSE)) %>% 
  ungroup() %>% 
  ggplot(aes(y = 100000*point, ymin = 100000*lower, ymax = 100000*upper,
             x = 100000*raw_prev, xmin = 100000*raw_lower, xmax = 100000*raw_upper,
             col = N_c))+
  theme_bw()+
  geom_errorbar(col = "grey")+
  geom_errorbarh(col = "grey")+
  geom_point()+
  geom_abline()+
  labs(y = "Bayesian Mortality (per 100,000)",
       x = "Raw Mortality (per 100,000)",
       color = "Population\n(percentiles)")+
  scale_color_viridis_c()+
  theme(legend.position = "bottom",
        text = element_text(size = 8))
jsave(filename = paste0("compraw_CHD_Mort.jpeg"), 
      base_folder = "plts/ForPaper", square = T,
      square_size = 1200,
      dpi = 300)

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
  labs(y = "Bayesian ASYLL",
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
all_persons$CHD_ASYLD_Persons %>% 
  mutate(raw = ASYLD, 
         raw_lower = raw - 1.96 * SE, 
         raw_upper = raw + 1.96 * SE) %>% 
  mutate(raw_lower = ifelse(raw_lower < 0, 0, raw_lower)) %>% 
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
  labs(y = "Bayesian ASYLD",
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
all_persons$CHD_ASYLD_Persons %>% 
  mutate(non_zero = RSE_rawASYLD > 0) %>% 
  group_by(year) %>% 
  summarise(p = mean(non_zero))
# percent of non-zero ASYLL
  
  ## RSE
all_persons$CHD_ASYLD_Persons %>% 
    dplyr::select(RSE, RSE_rawASYLD, M_id, year) %>%
    pivot_longer(-c(M_id, year)) %>% 
    ggplot(aes(y = log(value), col = name, x = M_id)) + 
    geom_point()+
    facet_wrap(.~year)+
    labs(y = "Log RSE ASYLD",
         x = "")

# Caterpillar plot comparison
all_persons$CHD_ASYLD_Persons %>% 
  mutate(raw_point = ASYLD, 
         raw_lower = raw_point - 1.96 * SE, 
         raw_upper = raw_point + 1.96 * SE) %>% 
  mutate(raw_lower = ifelse(raw_lower < 0, 0, raw_lower)) %>% 
  rename(mod_point = point,
         mod_upper = upper,
         mod_lower = lower,
         mod_rse = RSE,
         raw_rse = RSE_rawASYLD) %>% 
  mutate(N_c = cut_number(N, n = 100, labels = FALSE)) %>% 
  dplyr::select(N_c, contains(c("point", "upper", "lower", "rse"))) %>% 
  mutate(x = order(order(mod_point))) %>% 
  pivot_longer(-c(x, N_c)) %>% 
  separate(name, into = c("type", "measure")) %>% 
  pivot_wider(values_from = value, names_from = measure) %>% 
  ggplot(aes(y = point, ymin = lower, ymax = upper, 
             x = x))+theme_bw()+
  geom_errorbar(col = "grey")+
  geom_point(aes(col = N_c))+
  facet_grid(type~., labeller = as_labeller(c("mod" = "Bayesian", "raw" = "Basic stratification")))+
  labs(y = "ASYLD", x = "Ranked yearly LGAs",
       color = "Population\n(percentiles)") +
  scale_color_viridis_c()+
  theme(legend.position = "bottom",
        text = element_text(size = 8))
jsave(filename = paste0("compraw_cat_CHD_ASYLD.jpeg"), 
      base_folder = "plts/ForPaper", square = T,
      square_size = 1200,
      dpi = 300)
  
## CHD prev ## -----------------------------------------------------------------

## point estimates
all_persons$CHD_prev_Persons %>% 
  mutate(raw_lower = raw_prev - 1.96 * SE, 
         raw_upper = raw_prev + 1.96 * SE) %>% 
  group_by(year) %>% 
  mutate(N_c = cut_number(N, n = 100, labels = FALSE)) %>% 
  ungroup() %>% 
  ggplot(aes(y = point, ymin = lower, ymax = upper,
             x = raw_prev, xmin = raw_lower, xmax = raw_upper,
             col = N_c))+theme_bw()+
  geom_errorbar(col = "grey")+
  geom_errorbarh(col = "grey")+
  geom_point()+
  geom_abline()+
  labs(y = "Bayesian Prevalence",
       x = "Raw Prevalence",
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
  
  all_persons$CHD_ASYLD_Persons %>% 
    dplyr::select(ASYLD, point) %>% 
    setNames(c("Raw", "Bayesian")) %>% 
    pivot_longer(everything()) %>% 
    mutate(condition = "CHD",
           metric = "ASYLD"),
  
  mort3110$CHD_Mort %>% 
    dplyr::select(raw_prev, point) %>% 
    setNames(c("Raw", "Bayesian")) %>% 
    pivot_longer(everything()) %>% 
    mutate(condition = "CHD",
           metric = "Mortality"),
  
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
  
  mort3110$Asthma_Mort %>% 
    dplyr::select(raw_prev, point) %>% 
    setNames(c("Raw", "Bayesian")) %>% 
    pivot_longer(everything()) %>% 
    mutate(condition = "Asthma",
           metric = "Mortality"),
  
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
  #filter(metric %in% c("ASYLD", "ASYLL")) %>% 
  #filter(!metric %in% c("ASYLD", "ASYLL")) %>% 
  ggplot(aes(y = log(value+1), x = name, fill = name))+
  geom_boxplot()+
  facet_wrap(condition~metric, scales = "free", nrow = 2)+
  #facet_grid(condition~metric, scales = "free")+
  theme_bw()+
  theme(legend.position = "none",
        text = element_text(size = 8))+
  labs(y = "Point estimates (log scale)",
       x = "")
jsave(filename = paste0("compraw_ALL.jpeg"), 
      base_folder = "plts/ForPaper", square = F,
      square_size = 1200,
      dpi = 300)
jsave(filename = "Figure1.png", 
      base_folder = "plts/ForPaper", square = F,
      square_size = 1200,
      dpi = 300)
jsave(filename = "Figure1.tiff", 
      base_folder = "plts/ForPaper", square = F,
      square_size = 1200,
      dpi = 300)

jsave(filename = paste0("compraw_ALL_justBOD.jpeg"), 
      base_folder = "plts/ForPaper", square = T,
      square_size = 1200,
      dpi = 300)
jsave(filename = paste0("compraw_ALL_justMeasures.jpeg"), 
      base_folder = "plts/ForPaper", square = T,
      square_size = 1200,
      dpi = 300)

## Compare CHD YLDS ## ---------------------------------------------------------

CHD1710$CHD_YLD %>% 
  group_by(year) %>% 
  mutate(N_c = cut_number(N, n = 100, labels = FALSE)) %>% 
  ungroup() %>% 
  left_join(.,pop,by = c("geography_no" = "LGA_Code")) %>% 
  ggplot(aes(y = point,
             x = YLD, col = N_c))+
  theme_bw()+
  geom_abline()+
  geom_point()+
  labs(y = "Bayesian YLDs (per 100,000)",
       x = "Raw YLDs (per 100,000)",
       color = "Population\n(percentiles)")+
  scale_color_viridis_c()+
  theme(legend.position = "bottom",
        text = element_text(size = 8))
jsave(filename = paste0("compraw_chdyld.jpeg"), 
      base_folder = "plts/ForPaper", square = F,
      square_size = 1200,
      dpi = 300)

## END SCRIPT ## ---------------------------------------------------------------