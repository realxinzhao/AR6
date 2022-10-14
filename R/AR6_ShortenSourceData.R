

## Please download data from  https://data.ene.iiasa.ac.at/ar6/#/workspaces
# or from Xin' dropbox:
# https://www.dropbox.com/sh/223pixhu22uq4d2/AACD25bnA-ad6dIMl2kHqB8oa?dl=0

# Put the data under
# data/AR6/1648976687084-AR6_Scenarios_Database_World_v1.0/AR6_Scenarios_Database_World_v1.0.CSV


## Load AR6 source data ----
#
AR6 <- readr::read_csv("data/AR6/1648976687084-AR6_Scenarios_Database_World_v1.0/AR6_Scenarios_Database_World_v1.0.CSV")

require(readr, readxl)    # Load data


## Load C1-C8 category from meta----
MS_Category <- readxl::read_excel("data/AR6/1648976687084-AR6_Scenarios_Database_World_v1.0/AR6_Scenarios_Database_metadata_indicators_v1.0.xlsx",
                                  sheet = 2) %>%
  select(Model, Scenario, Category, Policy_category, Project_study)


## Variable interested ----
Var <- c("Emissions|CO2", "Price|Carbon", "GDP|MER",
         "Carbon Sequestration|CCS",
         "Carbon Sequestration|Direct Air Capture",
         "Carbon Sequestration|Enhanced Weathering",
         "Carbon Sequestration|Feedstocks",
         "Carbon Sequestration|Land Use",
         "Carbon Sequestration|Other")

#
AR6 %>%
  filter(Variable %in% Var) %>%
  mutate(Pathway = interaction(Model, Scenario)) %>%
  # filter pathways passed vetting and received a category
  filter(Pathway %in% c(MS_Category %>% distinct(interaction(Model, Scenario)) %>% pull) ) %>%
  # Join category identifer
  left_join(MS_Category, by = c("Model", "Scenario")) ->
  AR6_short

saveRDS(AR6_short, "data/AR6/RDS/AR6_short.rds")
