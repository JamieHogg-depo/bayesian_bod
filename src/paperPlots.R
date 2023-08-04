# Plots for paper

# mean EP across time, socioeconomic status and remoteness ---------------------

## -----
all_persons2$CHD_ASYLL_Persons %>% 
  left_join(.,seifa_ra, by = c("geography_no" = "LGA_Code")) %>% 
  group_by(IRSD_5, year, ra) %>% 
  summarise(m_EP = mean(EP)) %>%
  filter(!is.na(ra)) %>% 
  ggplot(aes(y = m_EP, x = year, col = IRSD_5, group = IRSD_5))+
  geom_point()+
  geom_line()+
  geom_hline(yintercept = 0.8)+
  facet_wrap(.~ra)+
  labs(y = "Mean of exceedance probabilities",
       x = "",
       col = "Socioeconomic\nstatus")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "bottom")
jsave(filename = "EPtemporal_CHD_ASYLL_Persons.png", 
      base_folder = "plts", square = T)

## -----
all_persons2$Asthma_prev_Persons %>% 
  left_join(.,seifa_ra, by = c("lga_name16" = "LGA_Name")) %>% 
  group_by(IRSD_5, year, ra) %>% 
  summarise(m_EP = mean(EP)) %>%
  filter(!is.na(ra)) %>%
  ggplot(aes(y = m_EP, x = year, col = IRSD_5, group = IRSD_5))+
  geom_point()+
  geom_line()+
  geom_hline(yintercept = 0.8)+
  facet_wrap(.~ra)+
  labs(y = "Mean of exceedance probabilities",
       x = "",
       col = "Socioeconomic\nstatus")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "bottom")
jsave(filename = "EPtemporal_Asthma_prev_Persons.png", 
      base_folder = "plts", square = T)

## -----
all_persons2$Asthma_ASYLD_Persons %>% 
  left_join(.,seifa_ra, by = c("lga_name16" = "LGA_Name")) %>% 
  group_by(IRSD_5, year, ra) %>% 
  summarise(m_EP = mean(EP)) %>%
  filter(!is.na(ra)) %>% 
  ggplot(aes(y = m_EP, x = year, col = IRSD_5, group = IRSD_5))+
  geom_point()+
  geom_line()+
  geom_hline(yintercept = 0.8)+
  facet_wrap(.~ra)+
  labs(y = "Mean of exceedance probabilities",
       x = "",
       col = "Socioeconomic\nstatus")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "bottom")
jsave(filename = "EPtemporal_Asthma_ASYLD_Persons.png", 
      base_folder = "plts", square = T)

## -----
all_persons2$Asthma_ASYLL_Persons %>% 
  left_join(.,seifa_ra, by = c("geography_no" = "LGA_Code")) %>% 
  group_by(IRSD_5, year, ra) %>% 
  summarise(m_EP = mean(EP)) %>%
  filter(!is.na(ra)) %>% 
  ggplot(aes(y = m_EP, x = year, col = IRSD_5, group = IRSD_5))+
  geom_point()+
  geom_line()+
  geom_hline(yintercept = 0.8)+
  facet_wrap(.~ra)+
  labs(y = "Mean of exceedance probabilities",
       x = "",
       col = "Socioeconomic\nstatus")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "bottom")
jsave(filename = "EPtemporal_Asthma_ASYLL_Persons.png", 
      base_folder = "plts", square = T)

## Temporal random effects ## --------------------------------------------------

## -----
temporal_vecs$Asthma_YLD %>% 
  ggplot(aes(y = point, ymin = lower, ymax = upper,
             x = as.character(2015:2020),
             group = 1))+theme_bw()+
  geom_errorbar(col = "grey")+
  geom_line()+
  geom_point()+
  labs(y = "Temporal random effect",
       x = "",
       title = "WMrP")+
  theme(axis.text.x = element_text(angle = 90))
jsave(filename = "temporalRA_Asthma_YLD.png", 
      base_folder = "plts", square = T)

## -----
temporal_vecs$aCHD_YLD %>% 
  ggplot(aes(y = point, ymin = lower, ymax = upper,
             x = as.character(2015:2020),
             group = 1))+theme_bw()+
  geom_errorbar(col = "grey")+
  geom_line()+
  geom_point()+
  labs(y = "Temporal random effect",
       x = "",
       title = "SAYT model",
       subtitle = "Acute CHD - YLD")+
  theme(axis.text.x = element_text(angle = 90))
jsave(filename = "temporalRA_aCHD_YLD.png", 
      base_folder = "plts", square = T)

## -----
temporal_vecs$cCHD_YLD %>% 
  ggplot(aes(y = point, ymin = lower, ymax = upper,
             x = as.character(2015:2020),
             group = 1))+theme_bw()+
  geom_errorbar(col = "grey")+
  geom_line()+
  geom_point()+
  labs(y = "Temporal random effect",
       x = "",
       title = "SAYT model",
       subtitle = "Chronic CHD - YLD")+
  theme(axis.text.x = element_text(angle = 90))
jsave(filename = "temporalRA_cCHD_YLD.png", 
      base_folder = "plts", square = T)

## Temporal random effects ## --------------------------------------------------

