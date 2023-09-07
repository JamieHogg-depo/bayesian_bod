## -----------------------------------------------------------------------------
## Setup ## --------------------------------------------------------------------
## -----------------------------------------------------------------------------

# Packages
library(tidyverse)
library(ggpubr)
library(scales)
library(sf)
library(MASS)
library(patchwork)
library(readr)
library(readxl)
library(grid)
library(gridExtra)
rm(list = ls())

export <- FALSE

## Functions ## ----------------------------------------------------------------

source("src/moreFuns.R")

## Combinations ## -------------------------------------------------------------

## Metrics (5): YLL, YLD, DALY, ASYLL, ASYLD, ASDALY, Prev
## Sex (3): Persons, Male, Female
## Conditions (2): CHD, Asthma
## Types of figures (4): map_, mapEP, temporalbox, caterpillarbyyear

# Figure labelling system
## `figuretype`_`condition`_`metric`_`sex`
## Example: `map_Asthma_ASYLL_Persons.png`

## Notes
'
- Need different scripts for each 
'

## Load Data ## ----------------------------------------------------------------

# Load mappopData
load("data/mappopDATA_2008_2020_updated.Rdata")
rm(age_labs, hd, hr, sa2)
map <- lga$map %>% 
  rmapshaper::ms_simplify(.,keep = 0.03) %>% 
  mutate(geography_no = as.integer(LGA_COD)) 

# WA outline
wa_border <- suppressMessages(map %>% 
                                summarise(geometry = st_union(geometry)) %>% 
                                st_as_sf() %>%
                                st_transform(4326))

# convert seifa
lga$seifa_ra$LGA_Code <- as.integer(lga$seifa_ra$LGA_Code)
lga$seifa_ra$RA_Name <- str_remove(str_remove(lga$seifa_ra$RA_Name, " Australia"), " of")

# Average population
pop <- lga$pop %>% 
  group_by(Year, LGA_Code) %>% 
  summarise(N = sum(N),
            .groups = "drop") %>% 
  group_by(LGA_Code) %>% 
  summarise(N = mean(N)) %>% 
  mutate(LGA_Code = as.integer(LGA_Code))

# SEIFA and remotenesss
seifa_ra <- lga$seifa_ra %>% 
  group_by(LGA_Name, LGA_Code, RA_Name, IRSD_5) %>% 
  tally() %>% 
  ungroup() %>% 
  dplyr::select(-n) %>% 
  left_join(.,pop) %>% 
  mutate(N_c = as.factor(cut_number(N, 10, labels = FALSE)),
         ra = as.factor(RA_Name),
         ra = fct_relevel(ra, "Major Cities"))

## Updated Asthma files - 1408 ## ----------------------------------------------

loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}

Asthma1408 <- list(Asthma_ASYLD_gammavectors = loadRData("data/RE_Asthma gamma, ASYLD  and prevalence 6 year res20230814034734/WMrPST_gamma_YLD_LGA_currast.Rdata"),
                   Asthma_ASYLD = read_excel("data/RE_Asthma gamma, ASYLD  and prevalence 6 year res20230814034734/WMrPST_results_LGA_ASYLD_converted_currast_MT_6yr.xlsx"))

## Temporal vectors ## ---------------------------------------------------------

files_to_load <- list.files("data/Temporal(gamma) vectors20230803063611",
                            pattern = "*.Rdata", full.names = T)
temporal_vecs <- lapply(files_to_load, loadRData)
names(temporal_vecs) <- c("Asthma_YLD", "aCHD_YLD", "cCHD_YLD")

# convert to posterior summaries
temporal_vecs <- lapply(temporal_vecs, getResultsData)

## Updated temporal vectors - 1408 ## ------------------------------------------

files1408 <- list(Asthma_YLL = loadRData("data/TemporalVectors YLL20230814034912/LGA_Asthma_Total gamma_draws.Rdata"),
                  CHD_YLL = loadRData("data/TemporalVectors YLL20230814034912/LGA_CHD_Total gamma_draws.Rdata"),
                  Asthma_ASYLL = read_csv("data/TemporalVectors YLL20230814034912/LGA_Asthma_Total ASYLL count table 6 year.csv"),
                  CHD_ASYLL = read_csv("data/TemporalVectors YLL20230814034912/LGA_CHD_Total ASYLL count table 6 year.csv"))

# convert to posterior summaries
temporal_vecs <- c(temporal_vecs, lapply(head(files1408,2), getResultsData))

## Raw mortality and prevalence data - 1508 ## ---------------------------------

rawfiles1508 <- list(Asthma_mort = read_csv("data/Rawmortality and prevalence data20230815035038/Asthma_Totalmasked_mort.csv"),
                     CHD_mort = read_csv("data/Rawmortality and prevalence data20230815035038/CHD_Totalmasked_mort.csv"),
                     Asthma_prev = read_csv("data/Rawmortality and prevalence data20230815035038/Raw prevalence CHD Total.csv"))

## Asthma prevalance - 1708 ## -------------------------------------------------

asthma_prev_1708 <- read_excel("data/WMrPST_results_LGA_YLD_currast_6 year_prev.xlsx")

## Raw ASYLL and ASYLD - 1708 ## -----------------------------------------------

rawfiles1708 <- list(Asthma_ASYLL = read_csv("data/RawASYLL and ASYLDs20230817092950/Asthma_TotalRAW_FILE.csv"),
                     CHD_ASYLD = read_csv("data/RawASYLL and ASYLDs20230817092950/CHD_Total_raw.csv"),
                     CHD_ASYLL = read_csv("data/RawASYLL and ASYLDs20230817092950/CHD_TotalRAW_FILE.csv"))

## Raw  Asthma prevalence 3008 ## ----------------------------------------------

str_foo <- function(x){as.numeric(ifelse(x == ".", NA,x ))}
load_foo <- function(x){
  read_excel("data/Reportable Estimates for Asthma by LGA HWSS for Jamie.xlsx", sheet = x) %>% 
    rename(lga_name16 = `LGA Name`) %>% 
    mutate(raw = str_foo(Prevalence)/100,
           raw_lower = str_foo(LCI)/100,
           raw_upper = str_foo(UCI)/100,
           raw_RSE = str_foo(RSE)*100,
           year = x) %>% 
    dplyr::select(year, lga_name16, raw, raw_lower, raw_upper, raw_RSE)
}

raw_asthma_3008 <- bind_rows(lapply(as.character(2015:2020), FUN = load_foo))
rm(str_foo, load_foo)

# Join concordance
raw_asthma_3008 <- expand.grid(lga_name16 = lga$map$LGA_NAM,
                               year = as.character(2015:2020)) %>% 
  left_join(.,raw_asthma_3008) %>% 
  mutate(year = as.numeric(year))

## New CHD - 1708 ## -----------------------------------------------------------

CHD_ASYLD_1708 <- read_csv("data/CHDnew model results20230817093627/YLD_LGA_CHD_ALL_Total ASYLD count table 6year_POPCOR.csv")

## Updated Asthma YLL - 0709 ## ------------------------------------------------

Asthma_ASYLL_0709 <- read_csv("data/Updatedasthma YLL data20230907021646/LGA_Asthma_Total ASYLL count table 6 year.csv")

## Grand list ## ---------------------------------------------------------------

all_persons <- list(CHD_ASYLL_Persons = files1408$CHD_ASYLL, # downloaded on 1408
                    CHD_ASYLD_Persons = CHD_ASYLD_1708, # downloaded 1708
                    Asthma_ASYLL_Persons = Asthma_ASYLL_0709, # downloaded 0709
                    Asthma_ASYLD_Persons = Asthma1408$Asthma_ASYLD, # downloaded on 1408
                    Asthma_prev_Persons = asthma_prev_1708 # downloaded on 1708
                    )
all_persons$CHD_ASYLD_Persons <- all_persons$CHD_ASYLD_Persons %>% rename(year = data_year)
all_persons$Asthma_prev_Persons <- all_persons$Asthma_prev_Persons %>% setNames(str_remove(names(.), "mrp_"))

## Apply suppression ## --------------------------------------------------------

raw <- list()

# CHD_TotalRAW_FILE.csv
raw$chd_asyll <- rawfiles1708$CHD_ASYLL %>% 
  mutate(raw_count = as.numeric(ifelse(raw_count == "-", NA, raw_count)),
         suppressed = (raw_count < 20 | is.na(raw_count) | N < 30)) %>% 
  filter(!suppressed)

# CHD_Total_raw.csv
raw$chd_asyld <- rawfiles1708$CHD_ASYLD %>% 
  mutate(raw_estimate = as.numeric(ifelse(raw_estimate == "-", NA, raw_estimate)),
         suppressed = (raw_estimate < 20 | is.na(raw_estimate) | N < 30))%>% 
  filter(!suppressed)

# Asthma_TotalRAW_FILE.csv
raw$asthma_asyll <- rawfiles1708$Asthma_ASYLL %>% 
  mutate(raw_count = as.numeric(ifelse(raw_count == "-", NA, raw_count)),
         suppressed = (raw_count < 20 | is.na(raw_count) | N < 30))%>% 
  filter(!suppressed)

## Other code ## ---------------------------------------------------------------

# City Insets 
lims <- data.frame(
  xmin = c(152.6, 150.35, 144.5, 115.45, 138.1, 146.8, 148.6, 130.3),
  xmax = c(153.6, 151.35, 145.5, 116.45, 139.1, 147.8, 149.6, 131.3),
  ymin = -c(28, 34.4, 38.4, 32.5, 35.4, 43.4, 35.8, 13),
  ymax = -c(27, 33.4, 37.4, 31.5, 34.4, 42.4, 34.8, 12),
  city = c("Brisbane", "Sydney", "Melbourne", "Perth", "Adelaide", "Hobart", "Canberra", "Darwin"),
  position = c("r", "r", "b", "l", "b", "b", "r", "l"),
  inset_labs = c("B - Brisbane (Qld)", "S - Sydney (NSW)",
                 "M - Melbourne (Vic)", "P - Perth (WA)",
                 "A - Adelaide (SA)", "H - Hobart (Tas)",
                 "C - Canberra (ACT)", "D - Darwin (NT)")
) %>% 
  mutate(initials = str_sub(city, 1, 1)) %>% 
  filter(city == "Perth") %>% 
  mutate(xmin = 115.71,
         xmax = 116.12,
         ymin = -32.15,
         ymax = -31.74)

## END SCRIPT ## --------------------------------------------------------------
