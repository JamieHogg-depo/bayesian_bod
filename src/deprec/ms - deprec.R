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

## Six year YLLs ## ------------------------------------------------------------
files_to_load = list.files("data/CHDand Asthma YLL results by 6 year ASYLL20230707084532", 
                           pattern = "*.csv", full.names = T)
ASYLL6y_list = lapply(files_to_load, read.csv)
names(ASYLL6y_list) <- c("Asthma_ASYLL_Female",
                         "Asthma_ASYLL_Male",
                         "CHD_ASYLL_Female",
                         "CHD_ASYLL_Male",
                         "CHD_ASYLL_Persons")

## Load CHD YLLs ## ------------------------------------------------------------
files_to_load = list.files("data/YLLoutputs20CHD20230614013642", 
                           pattern = "*.csv", full.names = T)
CHDYLL_list = lapply(files_to_load, read.csv)
names(CHDYLL_list) <- c("CHD_ASYLL_Female",
                        "CHD_YLL_Female",
                        "CHD_ASYLL_Male",
                        "CHD_YLL_Male",
                        "CHD_ASYLL_Persons",
                        "CHD_YLL_Persons")

## Load Asthma YLLs ## ---------------------------------------------------------
files_to_load = list.files("data/YLLsfor%20asthma20230619125714", 
                           pattern = "*.csv", full.names = T)
ATHYLL_list = lapply(files_to_load, read.csv)
names(ATHYLL_list) <- c("Asthma_ASYLL_Female",
                        "Asthma_YLL_Female",
                        "Asthma_ASYLL_Male",
                        "Asthma_YLL_Male",
                        "Asthma_ASYLL_Persons",
                        "Asthma_YLL_Persons")

## Load Asthma YLDs ## ---------------------------------------------------------
files_to_load = list.files("data/wMrPResults LGA YLD asthma20230704021943", pattern = "*.xlsx", full.names = T)
ATHYLD_list = lapply(files_to_load, readxl::read_xlsx)
# ASYLD with all sex categories
ATHYLD_list[[1]] <- bind_rows(mutate(ATHYLD_list[[1]], sex = "Persons"), ATHYLD_list[[2]])
# YLD with all sex categories
ATHYLD_list[[3]] <- bind_rows(mutate(ATHYLD_list[[3]], sex = "Persons"), ATHYLD_list[[4]])
# Prev with all sex categories
ATHYLD_list[[5]] <- bind_rows(mutate(ATHYLD_list[[5]], sex = "Persons"), ATHYLD_list[[6]])
# set 4-6 to null
ATHYLD_list[c(2,4,6)] <- NULL
# rename
names(ATHYLD_list) <- c("Asthma_ASYLD",
                        "Asthma_YLD",
                        "Asthma_Prev")
# fix columns for Asthma_Prev
colnames(ATHYLD_list$Asthma_Prev) <- str_replace(colnames(ATHYLD_list$Asthma_Prev), 
                                                 "mrp_", "")
# split into Persons, Male and Female
x1 <- split(ATHYLD_list$Asthma_ASYLD,ATHYLD_list$Asthma_ASYLD$sex)
names(x1) <- paste0("Asthma_ASYLD_", names(x1))
x2 <- split(ATHYLD_list$Asthma_YLD,ATHYLD_list$Asthma_YLD$sex)
names(x2) <- paste0("Asthma_YLD_", names(x2))
x3 <- split(ATHYLD_list$Asthma_Prev,ATHYLD_list$Asthma_Prev$sex)
names(x3) <- paste0("Asthma_Prev_", names(x3))
ATHYLD_list <- c(x1, x2, x3); rm(x1, x2, x3)

## UPDATED ASTHMA YLL ## -------------------------------------------------------
files_to_load <- list.files("data/UpdatedAsthma YLL Results20230802064257",
pattern = "*.csv", full.names = T)
ATHYLL2_list <- lapply(files_to_load, read.csv)

# MUST USE THESE FILES

## UPDATED ASTHMA YLD - 0308 ## ------------------------------------------------

Asthma_ASYLD_Persons <- read_excel("data/WMrPST_results_LGA_ASYLD_converted_currast_MT_6yr.xlsx")

## UPDATED CHD files - 0308 ## -------------------------------------------------

files_to_load <- list.files("data/CHD",
                            pattern = "*.csv", full.names = T)
CHDYLD_list <- lapply(files_to_load, read.csv)
names(CHDYLD_list) <- c("CHD_ASYLD",
                        "CHD_ASYLD_non6",
                        "CHD_YLD")

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

## New CHD - 1708 ## -----------------------------------------------------------

CHD_ASYLD_1708 <- read_csv("data/CHDnew model results20230817093627/YLD_LGA_CHD_ALL_Total ASYLD count table 6year_POPCOR.csv")

## Grand list - deprec ## ------------------------------------------------------
df_list <- c(CHDYLL_list, ATHYLL_list)
asyll_list <- df_list[str_detect(names(df_list), "ASYLL")]
yll_list <- df_list[str_detect(names(df_list), " YLL")]

all_persons_old <- list(CHD = list(YLL = CHDYLL_list$CHD_YLL_Persons,
                                   ASYLL =ASYLL6y_list$CHD_ASYLL_Persons,
                                   YLD = list(),
                                   ASYLD = list()),
                Asthma = list(YLL = ATHYLL_list$Asthma_YLL_Persons,
                              ASYLL = ATHYLL_list$Asthma_ASYLL_Persons,
                              YLD = ATHYLD_list$Asthma_YLD_Persons,
                              ASYLD = ATHYLD_list$Asthma_ASYLD_Persons))

## Grand list ## ---------------------------------------------------------------

all_persons <- list(CHD_ASYLL_Persons = files1408$CHD_ASYLL, # downloaded on 1408
                    CHD_ASYLD_Persons = CHD_ASYLD_1708, # downloaded 1708
                    Asthma_ASYLL_Persons = files1408$Asthma_ASYLL, # downloaded 1408
                    Asthma_ASYLD_Persons = Asthma1408$Asthma_ASYLD, # downloaded on 1408
                    Asthma_prev_Persons = asthma_prev_1708 # downloaded on 1708
                    )
all_persons$CHD_ASYLD_Persons <- all_persons$CHD_ASYLD_Persons %>% rename(year = data_year)
all_persons$Asthma_prev_Persons <- all_persons$Asthma_prev_Persons %>% setNames(str_remove(names(.), "mrp_"))

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
