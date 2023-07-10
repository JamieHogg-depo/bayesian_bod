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
load("data/mappopDATA.Rdata")
rm(age_labs, hd, hr, hr_labs, sa2)
map <- lga$map %>% 
  mutate(geography_no = as.integer(LGA_CODE16)) %>% 
  rmapshaper::ms_simplify(.,keep = 0.03)

# WA outline
wa_border <- suppressMessages(map %>% 
                                summarise(geometry = st_union(geometry)) %>% 
                                st_as_sf() %>%
                                st_transform(4326))

# convert seifa
lga$seifa_ra$LGA_Code <- as.integer(lga$seifa_ra$LGA_Code)
lga$seifa_ra$RA_Name <- str_remove(str_remove(lga$seifa_ra$RA_Name, " Australia"), " of")

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

## Grand list ## --------------------------------------------------------------- 
df_list <- c(CHDYLL_list, ATHYLL_list)
asyll_list <- df_list[str_detect(names(df_list), "ASYLL")]
yll_list <- df_list[str_detect(names(df_list), " YLL")]

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
