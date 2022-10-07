

library(dplyr)
library(tidyr)
library(ggplot2)

# Load data and libs ----
require(gcamdata) # to use an interpolation func
require(scales)

# Load rds from cache
# The original data was too large
readRDS("data/AR6/RDS/AR6_short.rds") -> AR6_short

AR6_short %>% distinct(Variable)

# Gather, clean, fill, and join ----
AR6_short %>%
  filter(Variable %in% Var) %>%
  mutate(Pathway = interaction(Model, Scenario)) %>%
  # filter pathways passed vetting and received a category
  filter(Pathway %in% c(MS_Category %>% distinct(interaction(Model, Scenario)) %>% pull) ) %>%
  # Join category identifer
  left_join(MS_Category, by = c("Model", "Scenario")) %>%
  # gather year
  gcamdata::gather_years() %>%
  # Linear interpolation to annual
  group_by(Pathway, Variable, Scenario, Model, Region, Unit) %>%
  mutate(value = gcamdata::approx_fun(year, value, rule = 2)) %>%
  ungroup() %>%
  # Only keep year > 2020 with 10 year step
  filter(year %in% seq(2020, 2100, 10)) %>%
  # Only keep C1-C4 scenarios (<2C)
  filter(Category %in% paste0("C", 1:4)) ->
  AR6_vetted

# confirm 700 C1-C4 pathways
AR6_vetted %>% distinct(Pathway) %>% nrow

AR6_vetted %>%
  # Aggregate all Carbon Sequestration|* variables
  mutate(Variable = if_else(grepl("Carbon Seq", Variable),
                            "CarbonSequestration", Variable)) %>%
  group_by_at(vars(-value)) %>% filter(!is.na(value)) %>%
  summarise(value = sum(abs(value)), .groups = "drop") %>%
  select(-Unit) %>%
  # spread variable
  spread(Variable, value) %>%
  filter(`Price|Carbon` >0, `GDP|MER` >0) %>%
  drop_na()-> AR6_vetted_clean

# Only 565/700 of the scenarios can do the calculation
# 20% of the vetted scenarios did not report all the data needed here
AR6_vetted_clean %>% distinct(Pathway) %>% nrow

# Check units
AR6_vetted %>% distinct(Variable, Unit)

# Get data ready finally ----
AR6_vetted_clean %>%
  mutate(
  # Carbon tax rev.  adjust unit to billion $
  CT_rev = `Price|Carbon`, `Emissions|CO2` /1000,
  # Carbon sequestration value adjust unit to billion $
  CCS_rev = `Price|Carbon` * CarbonSequestration /1000,
  # Ratios
  CCSrevOverGDP = CCS_rev / `GDP|MER`,
  CTrevOverGDP = CT_rev / `GDP|MER`)  %>%
  # clean model identifier
  mutate(Model = gsub("\\+ V.14E2| GECO2019| ENGAGE|_GEI|EMF33| [0-9].[0-9]$| [0-9].[0-9].[0-9]$|_[0-9].[0-9]$| [0-9].[0-9]-[0-9].[0-9]$|-$|_$",
                      "", Model)) %>%
  mutate(Model = gsub("\\+ V.14E2| EMF30| ADVANCE|ix|-Buildings|-Transport|-MAgPIE| ",
                      "", Model)) %>%
  mutate(Model = replace(Model, Model == "WITCH-GLOBIOM", "WITCH")) ->
  AR6_vetted_CarbonRev

# Plots ----

## ggplot theme ----

windowsFonts("Arial" = windowsFont("Arial"))
theme0 <- theme(
  panel.border = element_rect(colour = "black", size=1),
  text = element_text(family= "Arial", size = 15),
  axis.text.y = element_text(angle = 0, color = "black", size = 15, margin = margin(r = 10)),
  axis.text.x = element_text(angle = 0, color = "black", size = 15, margin = margin(t = 10), vjust= 0.5),
  axis.title.y = element_text(size = 15, margin = margin(t = 0, r = 10, b = 0, l = 0)),
  #axis.title.x = element_text(size = 15, margin = margin(t = 10, r = 0, b = 0, l = 0)),
  strip.background = element_rect(fill="grey95"),
  strip.text = element_text(size = 16),
  plot.margin = margin(t = 10, r = 15, b = 10, l = 10) ,
  panel.spacing = unit(1, "lines"),
  axis.title.x = element_blank()
)

## func export fig ----
outdir <- "output/"
Write_png <- function(.plot, name, w = 9000, h = 4500, r = 600){
  png(paste0(outdir,name,".png"), width = w, height = h, res = r)
  print(.plot)
  dev.off()
}

# *Fig. Carbon tax revenue in GDP ----
AR6_vetted_CarbonRev %>%
  ggplot() + facet_wrap(~Category, scales = "free_y") +
  geom_line(aes(x = year, y = CTrevOverGDP, group = Pathway, color = Model), size = 1, alpha = 0.8) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Year", y = "Share") +
  theme_bw() + theme0 +
  labs(caption = c(
    "\nData source: IPCC AR6 Scenario Database \nVariables: Price|Carbon, GDP|MER, and Emissions|CO2\n@realxinzhao"),
    title = "Share of carbon tax revenue in GDP in IPCC AR6 pathways",
    subtitle = "565 C1-C4 (< 2C) included and model version identifier simplified.") ->
  p
p %>% Write_png("CarbonRevInGDP", h = 4300, w = 6000, r = 600)



# *Fig. carbon sequestration value in GDP ----
AR6_vetted_CarbonRev %>%
  ggplot() + facet_wrap(~Category, scales = "free_y") +
  geom_line(aes(x = year, y = CCSrevOverGDP, group = Pathway, color = Model), size = 1, alpha = 0.8) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Year", y = "Share") +
  theme_bw() + theme0 +
  labs(caption = c("\nData source: IPCC AR6 Scenario Database \nVariables: Price|Carbon, GDP|MER, and Carbon Sequestration|X (first level)\n@realxinzhao"),
       title = "Share of carbon sequestration value in GDP in IPCC AR6 pathways",
       subtitle = "565 C1-C4 (< 2C) included and model version identifier simplified.") ->
  p

p %>% Write_png("CarbonCSInGDP", h = 4300, w = 6000, r = 600)

AR6_vetted_CarbonRev %>% filter(Model != "POLES") %>%
  distinct(Pathway)

# *Fig. carbon sequestration value in GDP (no POLES) ----
# remove the 64 POLES pathways
AR6_vetted_CarbonRev %>% filter(Model != "POLES") %>%
  ggplot() + facet_wrap(~Category, scales = "free_y") +
  geom_line(aes(x = year, y = CCSrevOverGDP, group = Pathway, color = Model), size = 1, alpha = 0.8) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Year", y = "Share") +
  theme_bw() + theme0 +
  labs(caption = c("\nData source: IPCC AR6 Scenario Database \nVariables: Price|Carbon, GDP|MER, and Carbon Sequestration|X (first level)\n@realxinzhao"),
       title = "Share of carbon sequestration value in GDP in IPCC AR6 pathways",
       subtitle = "501 C1-C4 (< 2C) included (POLES removed) and model version identifier simplified.") ->
  p

p %>% Write_png("CarbonCSInGDP1", h = 4300, w = 6000, r = 600)
