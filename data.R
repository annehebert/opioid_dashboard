library(shiny)
library(ggplot2)
library(urbnmapr)
library(tidyverse)
library(plotly)

###### -------------------------------------------------------------------------
#-----  Data imports
###### -------------------------------------------------------------------------

###### ---- State map data
# ---- plotly maps
df.state.map.2019 = read.delim(file = 'data_files/dashboard_interactive_map2019.txt',
                               header = TRUE, 
                               sep = '\t')
df.state.map.2019 <- df.state.map.2019 %>% 
  replace_na(list(Od.percent.85 = 0, 
                  Od.percent.75 = 0))

df.state.map.2019$Od.percent.75plus <- df.state.map.2019$Od.percent.75 + 
  df.state.map.2019$Od.percent.85
df.state.map.2019 <- df.state.map.2019 %>%
  mutate(Od.percent.75 = na_if(Od.percent.75, 0),
         Od.percent.85 = na_if(Od.percent.85, 0),
         Od.percent.75plus = na_if(Od.percent.75plus, 0))

df.state.map.2020 = read.delim(file = 'data_files/dashboard_interactive_map2020.txt', 
                               header = TRUE, 
                               sep = '\t')
df.state.map.2020 <- df.state.map.2020 %>% 
  replace_na(list(Od.percent.85 = 0, 
                  Od.percent.75 = 0))
df.state.map.2020$Od.percent.75plus <- df.state.map.2020$Od.percent.75 + 
  df.state.map.2020$Od.percent.85
df.state.map.2020 <- df.state.map.2020 %>%
  mutate(Od.percent.75 = na_if(Od.percent.75, 0),
         Od.percent.85 = na_if(Od.percent.85, 0),
         Od.percent.75plus = na_if(Od.percent.75plus, 0))




###### ---- Deaths over time 1999-2020
df.od.over.time <- read.delim(file = 'data_files/opioid_od_years.txt',
                                         header = TRUE, sep = "\t")
df.od.over.time.gender <- read.delim(file = 'data_files/opioid_od_year_gender.txt',
                              header = TRUE, sep = "\t")
df.od.over.time.gender <- df.od.over.time.gender %>%
  filter(Gender == 'Female' | Gender == 'Male')

df.od.over.time.age <- read.delim(file = 'data_files/opioid_od_year_age.txt',
                                     header = TRUE, sep = "\t")
df.od.over.time.age <- df.od.over.time.age %>%
  filter(!is.null(Ten.Year.Age.Groups)) %>%
  filter(Ten.Year.Age.Groups.Code == "<25" | Ten.Year.Age.Groups.Code == "25-34" |
           Ten.Year.Age.Groups.Code == "35-44" |  Ten.Year.Age.Groups.Code == "45-54" | 
           Ten.Year.Age.Groups.Code == "55-64" | Ten.Year.Age.Groups.Code == "65+")


df.od.over.time.race <- read.delim(file = 'data_files/opioid_od_year_race.txt',
                                     header = TRUE, sep = "\t")
df.od.over.time.race$Population <- as.integer(df.od.over.time.race$Population)
df.test1 <- aggregate(Deaths ~ Race.Eth + Year, df.od.over.time.race,sum)
df.test2 <- aggregate(Population ~ Race.Eth + Year, df.od.over.time.race,sum)
df.od.over.time.race <- df.test1 %>%
  left_join(df.test2, on = c("Race.Eth", "Year")) %>%
  mutate(Crude.Rate = Deaths/Population*100000) %>%
  filter(!is.na(Population)) %>%
  mutate(Race.Eth = recode(Race.Eth,
                          "Asian_NonHisp" = "Asian",
                          "Black_NonHisp" = "Black",
                          "Hisp" = "Hispanic",
                          "White_NonHisp" = "White",
                         "Native_NonHisp" = "Native"))
  
  

###### ---- Prescription state maps 2006-2019
prescriptions <- read.delim(file = 'data_files/Prescriptions_state.txt', 
                            header = TRUE, 
                            sep = "\t")
prescription.map <- merge(prescriptions, urbnmapr::states, by.x = "State",
                          by.y = "state_name",
                          all = TRUE)


###### ---- County map data
df.county.hex.map.2019 = read.delim(file = 'data_files/dashboard_county_hex_map2019.txt', 
                               header = TRUE, sep = '\t')
df.county.hex.map.2020 = read.delim(file = 'data_files/dashboard_county_hex_map2020.txt', 
                               header = TRUE, sep = '\t')


###### ---- Age/gender death distribution data
df.death.dist2019 = read.delim(file = 'data_files/df_death_dist2019.txt',
                           header = TRUE, sep = '\t')
df.death.hist2019 = read.delim(file = 'data_files/deaths_hist2019.txt',
                           header = TRUE, sep = '\t')
df.death.dist2020 = read.delim(file = 'data_files/df_death_dist2020.txt',
                               header = TRUE, sep = '\t')
df.death.hist2020 = read.delim(file = 'data_files/deaths_hist2020.txt',
                           header = TRUE, sep = '\t')

###### ---- Correlates of opioid burden
df.county.correlates2019 <- read.delim('data_files/county_correlates2019.txt',
                                        header = TRUE, sep = '\t')
df.county.correlates2019$X = NULL 
df.county.correlates2019$UnempRate <- df.county.correlates2019$UnempRate2019
df.county.correlates2020 <- read.delim('data_files/county_correlates2020.txt',
                                       header = TRUE, sep = '\t')
df.county.correlates2020$X = NULL 
df.county.correlates2020$UnempRate <- df.county.correlates2020$UnempRate2020

###### ---- Total prescription rates 2006-2020
prescriptions.evolution <- read.delim(file = 'data_files/Prescriptions_national.txt',
                                      header = TRUE, sep = "\t")

