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

## Summaries - modelled vs Raw ## ----------------------------------------------

cut_off <- 50

summ_out <- list()
foo2 <- function(x, rr = 0){
  m = round(median(x, na.rm = T), rr)
  q = round(quantile(x, probs = c(0.25, 0.75), na.rm = T), rr)
  paste0(m, " (", q[1], ", ", q[2], ")")
}
foo <- function(x, rr = 0){
  m = round(median(x, na.rm = T), rr)
  q = round(c(min(x, na.rm = T),
               max(x, na.rm = T)), rr)
  paste0(m, " (", q[1], ", ", q[2], ")")
}

## CHD_ASYLL
summ_out[[1]] <- full_join(raw$chd_asyll, all_persons$CHD_ASYLL_Persons, by = c("T_id", "M_id")) %>% 
  mutate(raw_RSE_ASYLL = ifelse(is.na(raw_RSE_ASYLL), 100, raw_RSE_ASYLL)) %>% 
  summarise(mod_sum = foo(point),
            mod_rse = 100*mean(RSE < cut_off),
            raw_sum = foo(raw_ASYLL.x),
            raw_rse = 100*mean(raw_RSE_ASYLL < cut_off, na.rm = T),
            MAD = round(mean(abs(raw_ASYLL.x - point)),1)) %>% 
  mutate(condition = "CHD",
         metric = "ASYLL") %>% 
  relocate(condition, metric)

## CHD ASYLD
summ_out[[2]] <- full_join(raw$chd_asyld, all_persons$CHD_ASYLD_Persons, by = c("T_id", "M_id")) %>% 
  mutate(raw_RSE = ifelse(is.na(raw_RSE), 100, raw_RSE)) %>% 
  summarise(mod_sum = foo(point),
            mod_rse = 100*mean(RSE < cut_off),
            raw_sum = foo(raw_ASYLD),
            raw_rse = 100*mean(raw_RSE < cut_off, na.rm = T),
            MAD = round(mean(abs(raw_ASYLD - point)),1)) %>% 
  mutate(condition = "CHD",
         metric = "ASYLD") %>% 
  relocate(condition, metric)

## CHD prev
summ_out[[3]] <- full_join(all_persons$CHD_prev_Persons, raw$chd_prev, 
          by = c("geography_no", "MT_id", "year", "LGA_NAME16")) %>% 
  mutate(raw_RSE = ifelse(is.na(RSE.y), 100, RSE.y)) %>% 
  summarise(mod_sum = foo(point, 2),
            mod_rse = 100*mean(RSE.x < cut_off),
            raw_sum = foo(raw_prev.y, 2),
            raw_rse = 100*mean(raw_RSE < cut_off, na.rm = T),
            MAD = round(mean(abs(raw_prev.y - point)), 3)) %>% 
  mutate(condition = "CHD",
         metric = "Prevalence") %>% 
  relocate(condition, metric)

## Asthma_ASYLL
summ_out[[4]] <- full_join(raw$asthma_asyll, all_persons$Asthma_ASYLL_Persons, by = c("T_id", "M_id")) %>% 
  mutate(raw_RSE_ASYLL.x = ifelse(is.na(raw_RSE_ASYLL.x), 100, raw_RSE_ASYLL.x)) %>% 
  summarise(mod_sum = foo(point),
            mod_rse = 100*mean(RSE < cut_off),
            raw_sum = foo(raw_ASYLL.x),
            raw_rse = 100*mean(raw_RSE_ASYLL.x < cut_off, na.rm = T),
            MAD = round(mean(abs(raw_ASYLL.x - point)),1)) %>% 
  mutate(condition = "Asthma",
         metric = "ASYLL") %>% 
  relocate(condition, metric)

## Asthma ASYLD
summ_out[[5]] <- all_persons$Asthma_ASYLD_Persons %>% 
  summarise(mod_sum = foo(point),
            mod_rse = 100*mean(RSE < cut_off)) %>% 
  mutate(condition = "Asthma",
         metric = "ASYLD") %>% 
  relocate(condition, metric)

## Asthma Prevalence
summ_out[[6]] <- full_join(raw_asthma_3008, all_persons$Asthma_prev_Persons, by = c("lga_name16", "year")) %>% 
  mutate(raw_RSE = ifelse(is.na(raw_RSE), 100, raw_RSE)) %>% 
  summarise(mod_sum = foo(point, 2),
            mod_rse = 100*mean(RSE < cut_off, na.rm = T),
            raw_sum = foo(raw, 2),
            raw_rse = 100*mean(raw_RSE < cut_off, na.rm = T)) %>% 
  mutate(condition = "Asthma",
         metric = "Prevalence") %>% 
  relocate(condition, metric)

## Create final table 
bind_rows(summ_out) %>% 
  mutate(mod_rse = round(mod_rse),
         raw_rse = round(raw_rse)) %>% 
  dplyr::select(-MAD) %>% 
  setNames(c("", "", "Median (min, max)", "Reliable (%)", "Median (min, max)", "Reliable (%)")) %>% 
  knitr::kable(., "latex", booktabs = TRUE)


  #dplyr::select(-c(mod_sum, raw_sum))
  #write.csv(., file = "tables/summ.csv")

## Comparison Tables ## --------------------------------------------------------

comp_out <- list()

## CHD_ASYLL
comp_out[[1]] <- full_join(raw$chd_asyll, all_persons$CHD_ASYLL_Persons, by = c("T_id", "M_id")) %>% 
  left_join(.,pop,by = c("geography_no.x" = "LGA_Code")) %>% 
  group_by(year.x) %>% 
  mutate(N_c = cut_number(N.y, n = 5, labels = FALSE)) %>% 
  ungroup() %>% 
  filter(raw_RSE_ASYLL > 0) %>% 
  mutate(r = raw_RSE_ASYLL/RSE) %>% 
  group_by(N_c) %>% 
  summarise(MAD = round(mean(abs(raw_ASYLL.x - point)),1),
            rse_r = median(r, na.rm = T), 
            rse_r_q25 = quantile(r, 0.25, na.rm = T),
            rse_r_q75 = quantile(r, 0.75, na.rm = T),
            n = n()) %>% 
  mutate(condition = "CHD",
         metric = "ASYLL") %>% 
  relocate(condition, metric, n)

## CHD ASYLD
comp_out[[2]] <- full_join(raw$chd_asyld, all_persons$CHD_ASYLD_Persons, by = c("T_id", "M_id", "year")) %>% 
  group_by(year) %>% 
  mutate(N_c = cut_number(N, n = 5, labels = FALSE)) %>% 
  ungroup() %>% 
  filter(raw_RSE > 0) %>% 
  mutate(r = raw_RSE/RSE) %>% 
  group_by(N_c) %>% 
  summarise(MAD = round(mean(abs(raw_ASYLD - point)),1),
            rse_r = median(r, na.rm = T), 
            rse_r_q25 = quantile(r, 0.25),
            rse_r_q75 = quantile(r, 0.75),
            n = n()) %>% 
  mutate(condition = "CHD",
         metric = "ASYLD") %>% 
  relocate(condition, metric, n)

## CHD prev
comp_out[[3]] <- full_join(all_persons$CHD_prev_Persons, raw$chd_prev, 
          by = c("geography_no", "MT_id", "year", "LGA_NAME16")) %>% 
  group_by(year) %>% 
  mutate(N_c = cut_number(N.x, n = 5, labels = FALSE)) %>% 
  ungroup() %>% 
  filter(RSE.y > 0) %>% 
  mutate(r = RSE.y/RSE.x) %>% 
  group_by(N_c) %>% 
  summarise(MAD = round(mean(abs(raw_prev.y - point)),3),
            rse_r = median(r, na.rm = T), 
            rse_r_q25 = quantile(r, 0.25),
            rse_r_q75 = quantile(r, 0.75),
            n = n()) %>% 
  mutate(condition = "CHD",
         metric = "Prevalence") %>% 
  relocate(condition, metric, n)

## Asthma_ASYLL
comp_out[[4]] <- full_join(raw$asthma_asyll, all_persons$Asthma_ASYLL_Persons, by = c("T_id", "M_id")) %>% 
  left_join(.,pop,by = c("geography_no.x" = "LGA_Code")) %>% 
  group_by(year.x) %>% 
  mutate(N_c = cut_number(N.y, n = 5, labels = FALSE)) %>% 
  ungroup() %>% 
  filter(raw_RSE_ASYLL.x > 0) %>% 
  mutate(r = raw_RSE_ASYLL.x/RSE) %>% 
  group_by(N_c) %>% 
  summarise(MAD = round(mean(abs(raw_ASYLL.x - point)),1),
            rse_r = median(r, na.rm = T), 
            rse_r_q25 = quantile(r, 0.25, na.rm = T),
            rse_r_q75 = quantile(r, 0.75, na.rm = T),
            n = n()) %>% 
  mutate(condition = "Asthma",
         metric = "ASYLL") %>% 
  relocate(condition, metric, n)

## Asthma Prevalence
comp_out[[5]] <- full_join(raw_asthma_3008, all_persons$Asthma_prev_Persons, by = c("lga_name16", "year")) %>% 
  left_join(.,st_drop_geometry(lga$map) %>% dplyr::select(LGA_NAM, LGA_COD), by = c("lga_name16" = "LGA_NAM")) %>% 
  mutate(LGA_COD = as.numeric(LGA_COD)) %>% 
  left_join(.,pop,by = c("LGA_COD" = "LGA_Code")) %>% 
  group_by(year) %>% 
  mutate(N_c = cut_number(N, n = 5, labels = FALSE)) %>% 
  ungroup() %>% 
  filter(raw_RSE > 0) %>% 
  mutate(r = raw_RSE/RSE) %>% 
  group_by(N_c) %>% 
  summarise(MAD = round(mean(abs(raw - point), na.rm = T),2),
            rse_r = median(r, na.rm = T), 
            rse_r_q25 = quantile(r, 0.25, na.rm = T),
            rse_r_q75 = quantile(r, 0.75, na.rm = T),
            n = n()) %>% 
  mutate(condition = "Asthma",
         metric = "Prevalence") %>% 
  relocate(condition, metric, n)

## Create final table 
bind_rows(comp_out) %>% 
  mutate(rse_r = round(rse_r, 1),
         rse_r_q25 = round(rse_r_q25, 1),
         rse_r_q75 = round(rse_r_q75, 1),
         out = paste0(rse_r, " (", rse_r_q25, ", ", rse_r_q75, ")")) %>% 
  dplyr::select(-c(rse_r, rse_r_q25, rse_r_q75)) %>% 
  setNames(c("", "", "N (valid raw RSE)", "Population (quantiles)", "MAD", "RRSE")) %>% 
  #write.csv(., file = "tables/comp.csv") %>% 
  knitr::kable(., "latex", booktabs = TRUE)

## END SCRIPT ## ---------------------------------------------------------------
