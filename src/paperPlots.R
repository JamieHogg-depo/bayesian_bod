# Plots for paper

# mean EP across time, socioeconomic status and remoteness ---------------------

## -----
all_persons$CHD_ASYLL_Persons %>% 
  left_join(.,seifa_ra, by = c("geography_no" = "LGA_Code")) %>% 
  mutate(IRSD_5 = case_when(
    IRSD_5 == 1 ~ "1 - most\ndisadvantaged",
    IRSD_5 %in% c(2,3,4) ~ "2 - 4",
    IRSD_5 == 5 ~ "5 - least\ndisadvantaged"
  )) %>% 
  group_by(IRSD_5, year, ra) %>% 
  summarise(m_EP = median(EP)) %>%
  filter(!is.na(ra)) %>% 
  ggplot(aes(y = m_EP, x = year, col = IRSD_5, group = IRSD_5))+
  geom_hline(yintercept = c(0.8,0.2),
             linetype = "dotted")+
  geom_point()+
  geom_line()+
  facet_wrap(.~ra)+
  labs(y = "Median of exceedance probabilities",
       x = "",
       col = "Socioeconomic\nstatus (IRSD)")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "right",
        text = element_text(size = 8))+
  scale_color_manual(breaks = c("1 - most\ndisadvantaged", "2 - 4",
                               "5 - least\ndisadvantaged"),
                    values = c('#e41a1c','#377eb8','#4daf4a'))+
  ylim(0,1)
jsave(filename = "EPtemporal_CHD_ASYLL_Persons.jpeg", 
      base_folder = "plts/ForPaper", square = F,
      square_size = 1200,
      dpi = 300)

# Separate lines
all_persons$CHD_ASYLL_Persons %>% 
  left_join(.,seifa_ra, by = c("geography_no" = "LGA_Code")) %>% 
  mutate(IRSD_5 = case_when(
    IRSD_5 == 1 ~ "1 - most\ndisadvantaged",
    IRSD_5 %in% c(2,3,4) ~ "2 - 4",
    IRSD_5 == 5 ~ "5 - least\ndisadvantaged"
  )) %>% 
  ggplot(aes(y = EP, x = year, col = IRSD_5, group = M_id))+
  geom_hline(yintercept = c(0.8,0.2),
             linetype = "dotted")+
  #geom_point()+
  geom_line(alpha = 0.5)+
  facet_wrap(.~ra)+
  labs(y = "Exceedance probability",
       x = "",
       col = "Socioeconomic\nstatus (IRSD)")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "right",
        text = element_text(size = 8))+
  scale_color_manual(breaks = c("1 - most\ndisadvantaged", "2 - 4",
                                "5 - least\ndisadvantaged"),
                     values = c('#e41a1c','#377eb8','#4daf4a'))+
  ylim(0,1)
jsave(filename = "EPtemporal_sep_CHD_ASYLL_Persons.jpeg", 
      base_folder = "plts/ForPaper", square = F,
      square_size = 1200,
      dpi = 300)

## -----
all_persons$CHD_ASYLD_Persons %>% 
  left_join(.,seifa_ra, by = c("geography_no" = "LGA_Code")) %>% 
  mutate(IRSD_5 = case_when(
    IRSD_5 == 1 ~ "1 - most\ndisadvantaged",
    IRSD_5 %in% c(2,3,4) ~ "2 - 4",
    IRSD_5 == 5 ~ "5 - least\ndisadvantaged"
  )) %>% 
  group_by(IRSD_5, year, ra) %>% 
  summarise(m_EP = median(EP)) %>%
  filter(!is.na(ra)) %>% 
  ggplot(aes(y = m_EP, x = year, col = IRSD_5, group = IRSD_5))+
  geom_hline(yintercept = c(0.8,0.2),
             linetype = "dotted")+
  geom_point()+
  geom_line()+
  facet_wrap(.~ra)+
  labs(y = "Median of exceedance probabilities",
       x = "",
       col = "Socioeconomic\nstatus (IRSD)")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "right",
        text = element_text(size = 8))+
  scale_color_manual(breaks = c("1 - most\ndisadvantaged", "2 - 4",
                                "5 - least\ndisadvantaged"),
                     values = c('#e41a1c','#377eb8','#4daf4a'))+
  ylim(0,1)
jsave(filename = "EPtemporal_CHD_ASYLD_Persons.jpeg", 
      base_folder = "plts/ForPaper", square = F,
      square_size = 1200,
      dpi = 300)

# separate lines
all_persons$CHD_ASYLD_Persons %>% 
  left_join(.,seifa_ra, by = c("geography_no" = "LGA_Code")) %>% 
  mutate(IRSD_5 = case_when(
    IRSD_5 == 1 ~ "1 - most\ndisadvantaged",
    IRSD_5 %in% c(2,3,4) ~ "2 - 4",
    IRSD_5 == 5 ~ "5 - least\ndisadvantaged"
  )) %>% 
  filter(!is.na(ra)) %>% 
  ggplot(aes(y = EP, x = year, col = IRSD_5, group = M_id))+
  geom_hline(yintercept = c(0.8,0.2),
             linetype = "dotted")+
  #geom_point()+
  geom_line(alpha = 0.5)+
  facet_wrap(.~ra)+
  labs(y = "Exceedance probability",
       x = "",
       col = "Socioeconomic\nstatus (IRSD)")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "right",
        text = element_text(size = 8))+
  scale_color_manual(breaks = c("1 - most\ndisadvantaged", "2 - 4",
                                "5 - least\ndisadvantaged"),
                     values = c('#e41a1c','#377eb8','#4daf4a'))+
  ylim(0,1)
jsave(filename = "EPtemporal_sep_CHD_ASYLD_Persons.jpeg", 
      base_folder = "plts/ForPaper", square = F,
      square_size = 1200,
      dpi = 300)

## -----
CHDASYLD20231003 %>% 
  cbind(.,dplyr::select(all_persons$CHD_ASYLD_Persons, year)) %>% 
  left_join(.,seifa_ra, by = c("geography_no" = "LGA_Code")) %>% 
  mutate(IRSD_5 = case_when(
    IRSD_5 == 1 ~ "1 - most\ndisadvantaged",
    IRSD_5 %in% c(2,3,4) ~ "2 - 4",
    IRSD_5 == 5 ~ "5 - least\ndisadvantaged"
  )) %>% 
  group_by(IRSD_5, year, ra) %>% 
  summarise(m_EP = median(EP)) %>%
  filter(!is.na(ra)) %>% 
  ggplot(aes(y = m_EP, x = year, col = IRSD_5, group = IRSD_5))+
  geom_hline(yintercept = c(0.8,0.2),
             linetype = "dotted")+
  geom_point()+
  geom_line()+
  facet_wrap(.~ra)+
  labs(y = "Median of exceedance probabilities",
       x = "",
       col = "Socioeconomic\nstatus (IRSD)")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "right",
        text = element_text(size = 8))+
  scale_color_manual(breaks = c("1 - most\ndisadvantaged", "2 - 4",
                                "5 - least\ndisadvantaged"),
                     values = c('#e41a1c','#377eb8','#4daf4a'))+
  ylim(0,1)
jsave(filename = "EPtemporal_CHD_ASYLD_Persons_mb.jpeg", 
      base_folder = "plts/ForPaper", square = F,
      square_size = 1200,
      dpi = 300)

# separate lines
CHDASYLD20231003 %>% 
  cbind(.,dplyr::select(all_persons$CHD_ASYLD_Persons, year)) %>% 
  left_join(.,seifa_ra, by = c("geography_no" = "LGA_Code")) %>% 
  mutate(IRSD_5 = case_when(
    IRSD_5 == 1 ~ "1 - most\ndisadvantaged",
    IRSD_5 %in% c(2,3,4) ~ "2 - 4",
    IRSD_5 == 5 ~ "5 - least\ndisadvantaged"
  )) %>% 
  filter(!is.na(ra)) %>% 
  ggplot(aes(y = EP, x = year, col = IRSD_5, group =geography_no))+
  geom_hline(yintercept = c(0.8,0.2),
             linetype = "dotted")+
  #geom_point()+
  geom_line(alpha = 0.5)+
  facet_wrap(.~ra)+
  labs(y = "Exceedance probability",
       x = "",
       col = "Socioeconomic\nstatus (IRSD)")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "right",
        text = element_text(size = 8))+
  scale_color_manual(breaks = c("1 - most\ndisadvantaged", "2 - 4",
                                "5 - least\ndisadvantaged"),
                     values = c('#e41a1c','#377eb8','#4daf4a'))+
  ylim(0,1)
jsave(filename = "EPtemporal_sep_CHD_ASYLD_Persons_mb.jpeg", 
      base_folder = "plts/ForPaper", square = F,
      square_size = 1200,
      dpi = 300)

## -----
all_persons$CHD_prev_Persons %>% 
  left_join(.,seifa_ra, by = c("geography_no" = "LGA_Code")) %>% 
  mutate(IRSD_5 = case_when(
    IRSD_5 == 1 ~ "1 - most\ndisadvantaged",
    IRSD_5 %in% c(2,3,4) ~ "2 - 4",
    IRSD_5 == 5 ~ "5 - least\ndisadvantaged"
  )) %>% 
  group_by(IRSD_5, year, ra) %>% 
  summarise(m_EP = median(EP)) %>%
  filter(!is.na(ra)) %>%
  ggplot(aes(y = m_EP, x = year, col = IRSD_5, group = IRSD_5))+
  geom_hline(yintercept = c(0.8,0.2),
             linetype = "dotted")+
  geom_point()+
  geom_line()+
  facet_wrap(.~ra)+
  labs(y = "Median of exceedance probabilities",
       x = "",
       col = "Socioeconomic\nstatus")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "right",
        text = element_text(size = 8))+
  scale_color_manual(breaks = c("1 - most\ndisadvantaged", "2 - 4",
                                "5 - least\ndisadvantaged"),
                     values = c('#e41a1c','#377eb8','#4daf4a'))+
  ylim(0,1)
jsave(filename = "EPtemporal_CHD_prev_Persons.jpeg", 
      base_folder = "plts/ForPaper", square = F,
      square_size = 1200,
      dpi = 300)

# separate lines
all_persons$CHD_prev_Persons %>% 
  left_join(.,seifa_ra, by = c("geography_no" = "LGA_Code")) %>% 
  mutate(IRSD_5 = case_when(
    IRSD_5 == 1 ~ "1 - most\ndisadvantaged",
    IRSD_5 %in% c(2,3,4) ~ "2 - 4",
    IRSD_5 == 5 ~ "5 - least\ndisadvantaged"
  )) %>% 
  filter(!is.na(ra)) %>%
  ggplot(aes(y = EP, x = year, col = IRSD_5, group = geography_no))+
  geom_hline(yintercept = c(0.8,0.2),
             linetype = "dotted")+
  #geom_point()+
  geom_line(alpha = 0.5)+
  facet_wrap(.~ra)+
  labs(y = "Exceedance probability",
       x = "",
       col = "Socioeconomic\nstatus")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "right",
        text = element_text(size = 8))+
  scale_color_manual(breaks = c("1 - most\ndisadvantaged", "2 - 4",
                                "5 - least\ndisadvantaged"),
                     values = c('#e41a1c','#377eb8','#4daf4a'))+
  ylim(0,1)
jsave(filename = "EPtemporal_sep_CHD_prev_Persons.jpeg", 
      base_folder = "plts/ForPaper", square = F,
      square_size = 1200,
      dpi = 300)

## -----
all_persons$Asthma_prev_Persons %>% 
  left_join(.,seifa_ra, by = c("lga_name16" = "LGA_Name")) %>% 
  mutate(IRSD_5 = case_when(
    IRSD_5 == 1 ~ "1 - most\ndisadvantaged",
    IRSD_5 %in% c(2,3,4) ~ "2 - 4",
    IRSD_5 == 5 ~ "5 - least\ndisadvantaged"
  )) %>% 
  group_by(IRSD_5, year, ra) %>% 
  summarise(m_EP = median(EP)) %>%
  filter(!is.na(ra)) %>%
  ggplot(aes(y = m_EP, x = year, col = IRSD_5, group = IRSD_5))+
  geom_hline(yintercept = c(0.8,0.2),
             linetype = "dotted")+
  geom_point()+
  geom_line()+
  facet_wrap(.~ra)+
  labs(y = "Median of exceedance probabilities",
       x = "",
       col = "Socioeconomic\nstatus")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "right",
        text = element_text(size = 8))+
  scale_color_manual(breaks = c("1 - most\ndisadvantaged", "2 - 4",
                                "5 - least\ndisadvantaged"),
                     values = c('#e41a1c','#377eb8','#4daf4a'))+
  ylim(0,1)
jsave(filename = "EPtemporal_Asthma_prev_Persons.jpeg", 
      base_folder = "plts/ForPaper", square = F,
      square_size = 1200,
      dpi = 300)

# separate lines
all_persons$Asthma_prev_Persons %>% 
  left_join(.,seifa_ra, by = c("lga_name16" = "LGA_Name")) %>% 
  mutate(IRSD_5 = case_when(
    IRSD_5 == 1 ~ "1 - most\ndisadvantaged",
    IRSD_5 %in% c(2,3,4) ~ "2 - 4",
    IRSD_5 == 5 ~ "5 - least\ndisadvantaged"
  )) %>% 
  filter(!is.na(ra)) %>%
  ggplot(aes(y = EP, x = year, col = IRSD_5, group = lga_name16))+
  geom_hline(yintercept = c(0.8,0.2),
             linetype = "dotted")+
  #geom_point()+
  geom_line(alpha = 0.5)+
  facet_wrap(.~ra)+
  labs(y = "Exceedance probability",
       x = "",
       col = "Socioeconomic\nstatus")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "right",
        text = element_text(size = 8))+
  scale_color_manual(breaks = c("1 - most\ndisadvantaged", "2 - 4",
                                "5 - least\ndisadvantaged"),
                     values = c('#e41a1c','#377eb8','#4daf4a'))+
  ylim(0,1)
jsave(filename = "EPtemporal_sep_Asthma_prev_Persons.jpeg", 
      base_folder = "plts/ForPaper", square = F,
      square_size = 1200,
      dpi = 300)
jsave(filename = "Figure5.png", 
      base_folder = "plts/ForPaper", square = F,
      square_size = 1200,
      dpi = 300)
jsave(filename = "Figure5.tiff", 
      base_folder = "plts/ForPaper", square = F,
      square_size = 1200,
      dpi = 300)

## -----
all_persons$Asthma_ASYLD_Persons %>% 
  left_join(.,seifa_ra, by = c("lga_name16" = "LGA_Name")) %>% 
  mutate(IRSD_5 = case_when(
    IRSD_5 == 1 ~ "1 - most\ndisadvantaged",
    IRSD_5 %in% c(2,3,4) ~ "2 - 4",
    IRSD_5 == 5 ~ "5 - least\ndisadvantaged"
  )) %>% 
  group_by(IRSD_5, year, ra) %>% 
  summarise(m_EP = median(EP)) %>%
  filter(!is.na(ra)) %>% 
  ggplot(aes(y = m_EP, x = year, col = IRSD_5, group = IRSD_5))+
  geom_hline(yintercept = c(0.8,0.2),
             linetype = "dotted")+
  geom_point()+
  geom_line()+
  facet_wrap(.~ra)+
  labs(y = "Median of exceedance probabilities",
       x = "",
       col = "Socioeconomic\nstatus")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "right",
        text = element_text(size = 8))+
  scale_color_manual(breaks = c("1 - most\ndisadvantaged", "2 - 4",
                                "5 - least\ndisadvantaged"),
                     values = c('#e41a1c','#377eb8','#4daf4a'))+
  ylim(0,1)
jsave(filename = "EPtemporal_Asthma_ASYLD_Persons.jpeg", 
      base_folder = "plts/ForPaper", square = F,
      square_size = 1200,
      dpi = 300)

# separate lines
all_persons$Asthma_ASYLD_Persons %>% 
  left_join(.,seifa_ra, by = c("lga_name16" = "LGA_Name")) %>% 
  mutate(IRSD_5 = case_when(
    IRSD_5 == 1 ~ "1 - most\ndisadvantaged",
    IRSD_5 %in% c(2,3,4) ~ "2 - 4",
    IRSD_5 == 5 ~ "5 - least\ndisadvantaged"
  )) %>% 
  filter(!is.na(ra)) %>% 
  ggplot(aes(y = EP, x = year, col = IRSD_5, group = lga_name16))+
  geom_hline(yintercept = c(0.8,0.2),
             linetype = "dotted")+
  #geom_point()+
  geom_line(alpha = 0.5)+
  facet_wrap(.~ra)+
  labs(y = "Exceedance probability",
       x = "",
       col = "Socioeconomic\nstatus")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "right",
        text = element_text(size = 8))+
  scale_color_manual(breaks = c("1 - most\ndisadvantaged", "2 - 4",
                                "5 - least\ndisadvantaged"),
                     values = c('#e41a1c','#377eb8','#4daf4a'))+
  ylim(0,1)
jsave(filename = "EPtemporal_sep_Asthma_ASYLD_Persons.jpeg", 
      base_folder = "plts/ForPaper", square = F,
      square_size = 1200,
      dpi = 300)

## -----
all_persons$Asthma_ASYLL_Persons %>% 
  left_join(.,seifa_ra, by = c("geography_no" = "LGA_Code")) %>% 
  mutate(IRSD_5 = case_when(
    IRSD_5 == 1 ~ "1 - most\ndisadvantaged",
    IRSD_5 %in% c(2,3,4) ~ "2 - 4",
    IRSD_5 == 5 ~ "5 - least\ndisadvantaged"
  )) %>% 
  group_by(IRSD_5, year, ra) %>% 
  summarise(m_EP = median(EP)) %>%
  filter(!is.na(ra)) %>% 
  ggplot(aes(y = m_EP, x = year, col = IRSD_5, group = IRSD_5))+
  geom_hline(yintercept = c(0.8,0.2),
             linetype = "dotted")+
  geom_point()+
  geom_line()+
  facet_wrap(.~ra)+
  labs(y = "Median of exceedance probabilities",
       x = "",
       col = "Socioeconomic\nstatus")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "right",
        text = element_text(size = 8))+
  scale_color_manual(breaks = c("1 - most\ndisadvantaged", "2 - 4",
                                "5 - least\ndisadvantaged"),
                     values = c('#e41a1c','#377eb8','#4daf4a'))+
  ylim(0,1)
jsave(filename = "EPtemporal_Asthma_ASYLL_Persons.jpeg", 
      base_folder = "plts/ForPaper", square = F,
      square_size = 1200,
      dpi = 300)

# separate lines
all_persons$Asthma_ASYLL_Persons %>% 
  left_join(.,seifa_ra, by = c("geography_no" = "LGA_Code")) %>% 
  mutate(IRSD_5 = case_when(
    IRSD_5 == 1 ~ "1 - most\ndisadvantaged",
    IRSD_5 %in% c(2,3,4) ~ "2 - 4",
    IRSD_5 == 5 ~ "5 - least\ndisadvantaged"
  )) %>% 
  filter(!is.na(ra)) %>% 
  ggplot(aes(y = EP, x = year, col = IRSD_5, group = geography_no))+
  geom_hline(yintercept = c(0.8,0.2),
             linetype = "dotted")+
  #geom_point()+
  geom_line(alpha = 0.5)+
  facet_wrap(.~ra)+
  labs(y = "Exceedance probability",
       x = "",
       col = "Socioeconomic\nstatus")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "right",
        text = element_text(size = 8))+
  scale_color_manual(breaks = c("1 - most\ndisadvantaged", "2 - 4",
                                "5 - least\ndisadvantaged"),
                     values = c('#e41a1c','#377eb8','#4daf4a'))+
  ylim(0,1)
jsave(filename = "EPtemporal_sep_Asthma_ASYLL_Persons.jpeg", 
      base_folder = "plts/ForPaper", square = F,
      square_size = 1200,
      dpi = 300)


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
       title = "WMrP",
       subtitle = "Asthma - YLD")+
  theme(axis.text.x = element_text(angle = 90),
        text = element_text(size = 8))
jsave(filename = "temporalRA_Asthma_YLD.jpeg", 
      base_folder = "plts/ForPaper", square = T,
      square_size = 1200,
      dpi = 300)

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
  theme(axis.text.x = element_text(angle = 90),
        text = element_text(size = 8))
jsave(filename = "temporalRA_aCHD_YLD.jpeg", 
      base_folder = "plts/ForPaper", square = T,
      square_size = 1200,
      dpi = 300)

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
  theme(axis.text.x = element_text(angle = 90),
        text = element_text(size = 8))
jsave(filename = "temporalRA_cCHD_YLD.jpeg", 
      base_folder = "plts/ForPaper", square = T,
      square_size = 1200,
      dpi = 300)

## -----
temporal_vecs$Asthma_YLL %>% 
  ggplot(aes(y = point, ymin = lower, ymax = upper,
             x = as.character(2015:2020),
             group = 1))+theme_bw()+
  geom_errorbar(col = "grey")+
  geom_line()+
  geom_point()+
  labs(y = "Temporal random effect",
       x = "",
       title = "SAYT model",
       subtitle = "Asthma - YLL")+
  theme(axis.text.x = element_text(angle = 90),
        text = element_text(size = 8))
jsave(filename = "temporalRA_Asthma_YLL.jpeg", 
      base_folder = "plts/ForPaper", square = T,
      square_size = 1200,
      dpi = 300)

## -----
temporal_vecs$CHD_YLL %>% 
  ggplot(aes(y = point, ymin = lower, ymax = upper,
             x = as.character(2015:2020),
             group = 1))+theme_bw()+
  geom_errorbar(col = "grey")+
  geom_line()+
  geom_point()+
  labs(y = "Temporal random effect",
       x = "",
       title = "SAYT model",
       subtitle = "CHD - YLL")+
  theme(axis.text.x = element_text(angle = 90),
        text = element_text(size = 8))
jsave(filename = "temporalRA_CHD_YLL.jpeg", 
      base_folder = "plts/ForPaper", square = T,
      square_size = 1200,
      dpi = 300)

## Compare EPs - raw vs modeled #### ------------------------------------------

CHDASYLD20231003 %>% 
  rename(EP_m = EP) %>% 
  cbind(.,dplyr::select(all_persons$CHD_ASYLD_Persons, EP)) %>% 
  mutate(N_c = as.factor(cut_number(N, 5, labels = FALSE))) %>% 
  ggplot(aes(y = EP_m, x = EP)) + #, col = N_c))+
  theme_bw()+
  geom_rect(xmin = 0.8, xmax = Inf, ymin = 0.8, ymax = Inf,  color = "lightgreen", fill = "lightgreen")+
  geom_rect(xmin = -Inf, xmax = 0.2, ymin = -Inf, ymax = 0.2,  color = "lightgreen", fill = "lightgreen")+
  geom_rect(xmin = 0.8, xmax = Inf, ymin = -Inf, ymax = 0.2,  color = "grey", fill = "grey")+
  geom_rect(xmin = -Inf, xmax = 0.2, ymin = 0.8, ymax = Inf,  color = "grey", fill = "grey")+
  geom_point()+
  geom_hline(yintercept = c(0.8,0.2),
             linetype = "dotted")+
  geom_vline(xintercept = c(0.8,0.2),
             linetype = "dotted")+
  labs(y = "EP (using modeled baseline)",
       x = "EP (using raw baseline)") #,
       #col = "LGA Pop.\n(quantiles)")
jsave(filename = "EPcomp_CHD_ASYLD.jpeg", 
      base_folder = "plts/ForPaper", square = T,
      square_size = 1200,
      dpi = 300)

## DALYS ## --------------------------------------------------------------------

DALYs %>% 
  left_join(.,seifa_ra, by = c("geography_no" = "LGA_Code")) %>% 
  group_by(year) %>% 
  mutate(x = order(order(ratio))) %>% 
  ungroup() %>% 
  ggplot(aes(x=x, y=ratio, col = ra)) +
  theme_bw()+
  geom_hline(yintercept = 7.768, color="blue")+
  geom_hline(yintercept = 1, linetype = "dotted")+
  geom_point()+
  labs(y = "DALY ASR ratio", 
       x= "",
       col = "")+
  facet_wrap(vars(year))+
  ylim(0,25)+
  theme(legend.position = "bottom")+
  guides(col = guide_legend(nrow = 2))
jsave(filename = "DALYs_ratio.jpeg", 
      base_folder = "plts/ForPaper", square = T,
      square_size = 1200,
      dpi = 300)

## END SCRIPT ## ---------------------------------------------------------------