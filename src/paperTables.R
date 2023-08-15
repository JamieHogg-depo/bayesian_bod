all_persons$CHD_ASYLL_Persons %>% 
  left_join(.,seifa_ra, by = c("geography_no" = "LGA_Code")) %>% 
  group_by(IRSD_5, ra, year) %>% 
  summarise(ss = median(point),
            .groups = "drop") %>% 
  ggplot(aes(y= ss, x = year, col = IRSD_5))+
  geom_point()+
  facet_wrap(.~ra)

lapply(all_persons, dim)

foo <- function(x, rr = 0){
x %>% 
  summarise(m = round(median(point), rr),
            p25 = round(quantile(point, prob = 0.25), rr),
            p75 = round(quantile(point, prob = 0.75), rr),
            below = round(100*mean(RSE < 50), rr)) %>% 
  mutate(out = paste0(m, " (", p25, ", ", p75, ")")) %>% 
  dplyr::select(out, below)
}

lapply(all_persons, foo)
foo(all_persons$Asthma_prev_Persons, 4)

all_persons$Asthma_prev_Persons
