## paperRawPlots

rawfiles1708 <- list(Asthma_ASYLL = read_csv("data/RawASYLL and ASYLDs20230817092950/Asthma_TotalRAW_FILE.csv"),
                     CHD_ASYLD = read_csv("data/RawASYLL and ASYLDs20230817092950/CHD_Total_raw.csv"),
                     CHD_ASYLL = read_csv("data/RawASYLL and ASYLDs20230817092950/CHD_TotalRAW_FILE.csv"))

## Asthma ASYLL ## -------------------------------------------------------------

## point estimates
full_join(rawfiles1708$Asthma_ASYLL, all_persons$Asthma_ASYLL_Persons, by = c("T_id", "M_id")) %>% 
  ggplot(aes(y = point, ymin = lower, ymax = upper,
             x = raw_ASYLL.x))+
  geom_abline()+
  geom_errorbar(col = "grey")+
  geom_point()+
  xlim(0, 100)

## RSE summary
rawfiles1708$Asthma_ASYLL %>% 
  mutate(non_zero = raw_SE_ASYLL > 0) %>% 
  group_by(year) %>% 
  summarise(p = mean(non_zero))

## RSE
full_join(rawfiles1708$Asthma_ASYLL, all_persons$Asthma_ASYLL_Persons, by = c("T_id", "M_id")) %>% 
  dplyr::select(RSE, raw_RSE_ASYLL, M_id, T_id) %>%
  pivot_longer(-c(M_id, T_id)) %>% 
  ggplot(aes(y = value, col = name, x = M_id)) + 
  geom_point()+
  facet_grid(.~T_id)

## CHD ASYLL ## ----------------------------------------------------------------

## point estimates
full_join(rawfiles1708$CHD_ASYLL, all_persons$CHD_ASYLL_Persons, by = c("T_id", "M_id")) %>% 
  mutate(raw_lower = raw_ASYLL.x - 1.96 * raw_SE_ASYLL, 
         raw_upper = raw_ASYLL.x + 1.96 * raw_SE_ASYLL) %>% 
  ggplot(aes(y = point, ymin = lower, ymax = upper,
             x = raw_ASYLL.x, xmin = raw_lower, xmax = raw_upper))+theme_bw()+
  geom_abline()+
  geom_errorbar(col = "grey")+
  geom_errorbarh(col = "grey")+
  geom_point()+
  labs(y = "Modelled ASYLL",
       x = "Raw ASYLL")

## RSE summary
rawfiles1708$CHD_ASYLL %>% 
  mutate(non_zero = raw_SE_ASYLL > 0) %>% 
  group_by(year) %>% 
  summarise(p = mean(non_zero))
# percent of non-zero ASYLL

## RSE
full_join(rawfiles1708$CHD_ASYLL, all_persons$CHD_ASYLL_Persons, by = c("T_id", "M_id")) %>% 
  dplyr::select(RSE, raw_RSE_ASYLL, M_id, year.x) %>%
  pivot_longer(-c(M_id, year.x)) %>% 
  ggplot(aes(y = value, col = name, x = M_id)) + 
  geom_point()+
  facet_wrap(.~year.x)+
  labs(y = "RSE ASYLL",
       x = "")

## CHD ASYLL ## ----------------------------------------------------------------

## point estimates
full_join(rawfiles1708$CHD_ASYLD, all_persons$CHD_ASYLD_Persons, by = c("T_id", "M_id")) %>% 
  mutate(raw_lower = raw_ASYLD - 1.96 * Raw_SE_ASYLD, 
         raw_upper = raw_ASYLD + 1.96 * Raw_SE_ASYLD) %>% 
  mutate(raw_lower = ifelse(raw_lower < 0, 0, raw_lower)) %>% 
  ggplot(aes(y = point, ymin = lower, ymax = upper,
             x = raw_ASYLD, xmin = raw_lower, xmax = raw_upper))+theme_bw()+
  geom_abline()+
  geom_errorbar(col = "grey")+
  geom_errorbarh(col = "grey")+
  geom_point()+
  labs(y = "Modelled ASYLD",
       x = "Raw ASYLD")

## RSE summary
rawfiles1708$CHD_ASYLD %>% view()
  mutate(non_zero = Raw_SE_ASYLD > 0) %>% 
  group_by(data_year) %>% 
  summarise(p = mean(non_zero))
# percent of non-zero ASYLL
  
  ## RSE
  full_join(rawfiles1708$CHD_ASYLD, all_persons$CHD_ASYLD_Persons, by = c("T_id", "M_id")) %>% 
    dplyr::select(RSE, Raw_RSE_ASYLD, M_id, data_year) %>%
    pivot_longer(-c(M_id, data_year)) %>% 
    ggplot(aes(y = log(value), col = name, x = M_id)) + 
    geom_point()+
    facet_wrap(.~data_year)+
    labs(y = "Log RSE ASYLD",
         x = "")
