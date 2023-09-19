# Helper script for AQLI data--------------------------------------

# metadata
# author: Aarsh Batra
# last updated: September 19, 2023
# email: aarshbatra.in@gmail.com

# libraries
library(readr)
library(dplyr)
library(stringr)
library(magrittr)
library(ggplot2)
library(readr)
library(tidytext)
library(tidyr)
library(tidyverse)
library(sf)
library(usethis)
library(devtools)
library(data.table)
library(svglite)
library(here)
library(extrafont)
library(ggtext)
library(viridis)
library(aqverse) # download using devtools::install_github("aqli-epic/aqverse")

# global variables
`%notin%` <- Negate(`%in%`)
who_guideline <- 5

# read in latest AQLI files at all 3 GADM (https://gadm.org/) levels
gadm2_aqli_2021 <- readr::read_csv(paste0(here(), "/data-raw/master.dataset/[june302023]gadm2_aqli_2021_internal_col_names.csv"))
gadm1_aqli_2021 <- readr::read_csv(paste0(here(), "/data-raw/master.dataset/[june302023]gadm1_aqli_2021_internal_col_names.csv"))
gadm0_aqli_2021 <- readr::read_csv(paste0(here(), "/data-raw/master.dataset/[june302023]gadm0_aqli_2021_internal_col_names.csv"))


# read in the shapefiles corresponding to the CSVs above
 gadm2_aqli_2021_shp <- sf::st_read(paste0(here(), "/data-raw/master.dataset/shapefiles/june302023_post_missing_data_incorp_nat_stan_adj/aqli_gadm2_final_june302023.shp"))
 gadm1_aqli_2021_shp <- sf::st_read(paste0(here(), "/data-raw/master.dataset/shapefiles/june302023_post_missing_data_incorp_nat_stan_adj/aqli_gadm1_final_june302023.shp"))
 gadm0_aqli_2021_shp <- sf::st_read(paste0(here(), "/data-raw/master.dataset/shapefiles/june302023_post_missing_data_incorp_nat_stan_adj/aqli_gadm0_final_june302023.shp"))

# Load in India state shapefile for worldview adjustment
india_state <- st_read(paste0(here() ,"/data-raw/other.important.calculations.data/india_state_shp_non_aqli/india_state.shp"))

#> join each one of these with the country continent file, so that each one of these has a continent column

# read in the country continent file
country_continent <- readr::read_csv(paste0(here(), "/data-raw/other.important.calculations.data/country_continent.csv"))

# join each of the above 3 datasets with the country_continent file using the continent column
gadm2_aqli_2021 <- gadm2_aqli_2021 %>%
  left_join(country_continent, by = "country")

gadm1_aqli_2021 <- gadm1_aqli_2021 %>%
  left_join(country_continent, by = "country")

gadm0_aqli_2021 <- gadm0_aqli_2021 %>%
  left_join(country_continent, by = "country")

#> Filling in missing continents-----------------------------------------------

# countries for which continent is NA: for these fill in the continent manually
countries_with_missing_continent <- gadm0_aqli_2021 %>% filter(is.na(continent)) %>% pull(country) %>% unique()

# continent fill in for missing coutries
continents_for_missing_countries <- c("North America", "Africa")

# [CAUTION: perform a sanity check on the above 2 vectors and how they map countries to continents before proceeding]
#creating a data frame using the above 2 vectors as columns
missing_continents_df <- tibble(country = countries_with_missing_continent,
                                continent = continents_for_missing_countries)


# adding in the missing continent information in the gadmx_aqli_2021 datasets
gadm2_aqli_2021 <- gadm2_aqli_2021 %>%
  left_join(missing_continents_df, by = "country") %>%
  mutate(continent = ifelse(is.na(continent.x), continent.y, continent.x))

gadm1_aqli_2021 <- gadm1_aqli_2021 %>%
  left_join(missing_continents_df, by = "country") %>%
  mutate(continent = ifelse(is.na(continent.x), continent.y, continent.x))

gadm0_aqli_2021 <- gadm0_aqli_2021 %>%
  left_join(missing_continents_df, by = "country") %>%
  mutate(continent = ifelse(is.na(continent.x), continent.y, continent.x))


# global variables
who_guideline <- 5
le_constant <- 0.098
latest_year <- 2021
first_year <- 1998

# global operations
`%notin%` <- Negate(`%in%`)

#> gbd results-----------

# gbd results master
gbd_results_master_2021 <- read_csv(paste0(here(), "/data-raw/gbd.calculation/GBDComparisons/results_used_in_dashboard/gbd_results_master.csv"))


# US 1970 calculation master cleaned file read
us_1970_calc_results_cleaned <- read_csv(paste0(here(), "/data-raw/other.important.calculations.data/county_pm25_foraqli_stats_cleaned.csv"))


#> other region wise defintions---------------

# central africa definition
central_african_countries <- c("Angola", "Burundi", "Cameroon",
                               "Central African Republic", "Chad",
                               "Republic of the Congo",
                               "Democratic Republic of the Congo",
                               "Equatorial Guinea", "Gabon",
                               "São Tomé and Príncipe",
                               "Rwanda")
# west africa definition
west_african_countries <- c("Benin", "Burkina Faso", "Cabo Verde",
                            "Gambia", "Ghana", "Guinea", "Guinea-Bissau",
                            "Côte d'Ivoire", "Liberia", "Mali", "Mauritania",
                            "Niger", "Nigeria", "Senegal", "Sierra Leone",
                            "Togo")

# central and west africa countries definition, combine in a single vector
central_and_west_african_countries <- c(central_african_countries, west_african_countries)

# South East Asia definition
se_asia_vec <- c("Brunei", "Myanmar", "Cambodia", "Timor-Leste", "Indonesia", "Laos", "Malaysia", "Philippines", "Singapore", "Thailand", "Vietnam")

# indo gangetic plains states

indo_gangetic_plains_states <- c("NCT of Delhi", "Uttar Pradesh", "Bihar", "Haryana",
                                 "Punjab", "Chandigarh", "West Bengal")


# european countries
european_countries <- read_csv(paste0(here(), "/data-raw/other.important.calculations.data/europe_countries.csv"))

# western european countries
western_european_countries <- c("Germany", "Switzerland", "Italy", "Monaco", "Luxembourg",
                                "Belgium", "France", "Netherlands", "Andorra", "Spain",
                                "United Kingdom", "Portugal", "Denmark", "Ireland", "Iceland", "Austria")


# European Union countries
eu_countries <- c("Belgium", "Bulgaria", "Czechia", "Denmark", "Germany", "Estonia",
                  "Ireland", "Greece", "Spain", "France",
                  "Croatia", "Italy", "Cyprus", "Latvia",
                  "Lithuania", "Luxembourg", "Hungary",
                  "Malta", "Netherlands",
                  "Austria", "Poland",
                  "Portugal", "Romania",
                  "Slovenia", "Slovakia", "Finland", "Sweden",
                  "United Kingdom")

# South Asia definition
south_asia_def <- c("Afghanistan", "Bangladesh",
                    "Bhutan", "India",
                    "Maldives", "Nepal",
                    "Pakistan", "Sri Lanka")

# latin america definition

latin_america_countries_vec <- c("México", "Guatemala", "Honduras",
                                 "El Salvador", "Nicaragua",
                                 "Costa Rica", "Panama",
                                 "Colombia", "Venezuela",
                                 "Ecuador", "Peru",
                                 "Bolivia", "Brazil",
                                 "Paraguay", "Chile",
                                 "Argentina", "Uruguay",
                                 "Cuba", "Haiti", "Dominican Republic",
                                 "Puerto Rico")

#> open AQ global landscape 2022 report and clean-------------

# read data
openaq_data <- read_csv(paste0(here(), "/data-raw/other.important.calculations.data/openAQDataGlobalLandscape2022_apr2023_continent_adj.csv"))

# # remove top rows and some columns
openaq_data <- openaq_data[7:nrow(openaq_data), ]
colnames(openaq_data) <- openaq_data[1, ]
openaq_data <- openaq_data[2:nrow(openaq_data), ]

# set column names
colnames(openaq_data) <- c("country", "evid_govt_spon_aq_mon", "evidence_ad_hoc_studies", "aq_existed_in_past_not_cur",
                           "data_in_phys_units", "transp_geog_scale_data_provided", "data_fine_temporal_scale",
                           "prog_access", "continent")


# natstandard AQLI (last updated: June 2023) raw file
natstan_aqli <- read_csv(paste0(here(), "/data-raw/other.important.calculations.data/country_annual_average_pm2.5_standards_asInJune2023.csv"))
