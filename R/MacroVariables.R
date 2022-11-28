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
  # gather year
  gcamdata::gather_years() %>%
  # Linear interpolation to annual
  group_by(Pathway, Variable, Scenario, Model, Region, Unit) %>%
  mutate(value = gcamdata::approx_fun(year, value, rule = 2)) %>%
  ungroup() %>%
  # Only keep year > 2020 with 10 year step
  filter(year %in% seq(2020, 2100, 10)) %>%
  # Only keep C1-C4 scenarios (<2C)
  filter(Category %in% paste0("C", 1:8)) ->
  AR6_vetted

# confirm 1202 C1-C4 pathways
AR6_vetted %>% distinct(Pathway) %>% nrow

AR6_vetted %>% distinct(Variable)


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
  panel.spacing = unit(1.4, "lines"),
  axis.title.x = element_blank()
)

## func export fig ----
outdir <- "output/"
Write_png <- function(.plot, name, w = 9000, h = 4500, r = 600){
  png(paste0(outdir,name,".png"), width = w, height = h, res = r)
  print(.plot)
  dev.off()
}

AR6_vetted %>% distinct(Variable, Unit)
AR6_vetted %>% filter(Variable == "Price|Carbon") %>% distinct(Model, Scenario)


AR6_vetted %>% filter(Variable == "Price|Carbon")  %>%
  # clean model identifier
  mutate(Model = gsub("\\+ V.14E2| GECO2019| ENGAGE|_GEI|EMF33| [0-9].[0-9]$| [0-9].[0-9].[0-9]$|_[0-9].[0-9]$| [0-9].[0-9]-[0-9].[0-9]$|-$|_$",
                      "", Model)) %>%
  mutate(Model = gsub("\\+ V.14E2| EMF30| ADVANCE|ix|-Buildings|-Transport|-MAgPIE| ",
                      "", Model)) %>%
  mutate(Model = replace(Model, Model == "WITCH-GLOBIOM", "WITCH")) %>%
  #filter(Model != "POLES") %>%
  ggplot() + facet_wrap(~Category, scales = "fixed", nrow = 2) +
  geom_line(aes(x = year, y = value, group = Pathway, color = Model), size = 1, alpha = 0.8) +
  #scale_y_continuous(labels = scales::percent) +
  labs(x = "Year", y = "US$2010/t CO2") +
  theme_bw() + theme0 +
  labs(caption = c(
    "\nData source: IPCC AR6 Scenario Database \nVariable(s): Price|Carbon"),
    title = "Carbon prices in IPCC AR6 pathways",
    subtitle = "C1-C8 scenarios (n = 1118 / 1202); Model version identifier omitted") ->
  p;p
p %>% Write_png("CarbonPrice1", h = 2800, w = 5000, r = 300)

# *Fig. Carbon tax revenue in GDP ----
AR6_vetted %>% filter(Variable == "Investment")  %>%
  mutate(value = value / 1000) %>%
  # clean model identifier
  mutate(Model = gsub("\\+ V.14E2| GECO2019| ENGAGE|_GEI|EMF33| [0-9].[0-9]$| [0-9].[0-9].[0-9]$|_[0-9].[0-9]$| [0-9].[0-9]-[0-9].[0-9]$|-$|_$",
                      "", Model)) %>%
  mutate(Model = gsub("\\+ V.14E2| EMF30| ADVANCE|ix|-Buildings|-Transport|-MAgPIE| ",
                      "", Model)) %>%
  mutate(Model = replace(Model, Model == "WITCH-GLOBIOM", "WITCH")) %>%
  ggplot() + facet_wrap(~Category, scales = "fixed", nrow = 2) +
  geom_line(aes(x = year, y = value, group = Pathway, color = Model), size = 1, alpha = 0.8) +
  #scale_y_continuous(labels = scales::percent) +
  labs(x = "Year", y = "Tillion US$2010 per year") +
  theme_bw() + theme0 +
  labs(caption = c(
    "\nData source: IPCC AR6 Scenario Database \nVariable(s): Investment"),
    title = "Investment (total economy wide investments) in IPCC AR6 pathways",
    subtitle = "C1-C8 scenarios (n = 433 / 1202); Model version identifier omitted") ->
  p;p
p %>% Write_png("Investment", h = 2800, w = 5000, r = 300)


AR6_vetted %>% filter(Variable == "GDP|PPP")  %>%
  mutate(value = value / 1000) %>%
  # clean model identifier
  mutate(Model = gsub("\\+ V.14E2| GECO2019| ENGAGE|_GEI|EMF33| [0-9].[0-9]$| [0-9].[0-9].[0-9]$|_[0-9].[0-9]$| [0-9].[0-9]-[0-9].[0-9]$|-$|_$",
                      "", Model)) %>%
  mutate(Model = gsub("\\+ V.14E2| EMF30| ADVANCE|ix|-Buildings|-Transport|-MAgPIE| ",
                      "", Model)) %>%
  mutate(Model = replace(Model, Model == "WITCH-GLOBIOM", "WITCH")) %>%
  ggplot() + facet_wrap(~Category, scales = "fixed", nrow = 2) +
  geom_line(aes(x = year, y = value, group = Pathway, color = Model), size = 1, alpha = 0.8) +
  #scale_y_continuous(labels = scales::percent) +
  labs(x = "Year", y = "Tillion US$2010 per year") +
  theme_bw() + theme0 +
  labs(caption = c(
    "\nData source: IPCC AR6 Scenario Database \nVariable(s): GDP|PPP"),
    title = "GDP in IPCC AR6 pathways",
    subtitle = "C1-C8 scenarios (n = 1028 / 1202); Model version identifier omitted") ->
  p;p
p %>% Write_png("GDP", h = 2800, w = 5000, r = 300)


AR6_vetted %>% filter(Variable == "Capital Stock")  %>%
  mutate(value = value / 1000) %>%
  # clean model identifier
  mutate(Model = gsub("\\+ V.14E2| GECO2019| ENGAGE|_GEI|EMF33| [0-9].[0-9]$| [0-9].[0-9].[0-9]$|_[0-9].[0-9]$| [0-9].[0-9]-[0-9].[0-9]$|-$|_$",
                      "", Model)) %>%
  mutate(Model = gsub("\\+ V.14E2| EMF30| ADVANCE|ix|-Buildings|-Transport|-MAgPIE| ",
                      "", Model)) %>%
  mutate(Model = replace(Model, Model == "WITCH-GLOBIOM", "WITCH")) %>%
  ggplot() + facet_wrap(~Category, scales = "fixed", nrow = 2) +
  geom_line(aes(x = year, y = value, group = Pathway, color = Model), size = 1, alpha = 0.8) +
  #scale_y_continuous(labels = scales::percent) +
  labs(x = "Year", y = "Tillion US$2010 per year") +
  theme_bw() + theme0 +
  labs(caption = c(
    "\nData source: IPCC AR6 Scenario Database \nVariable(s): Capital Stock"),
    title = "Capital Stock in IPCC AR6 pathways",
    subtitle = "C1-C8 scenarios (n = 124 / 1202); Model version identifier omitted") ->
  p;p
p %>% Write_png("Capital Stock", h = 2800, w = 5000, r = 300)



AR6_vetted %>% filter(Variable == "Capital Formation")  %>%
  mutate(value = value / 1000) %>%
  # clean model identifier
  mutate(Model = gsub("\\+ V.14E2| GECO2019| ENGAGE|_GEI|EMF33| [0-9].[0-9]$| [0-9].[0-9].[0-9]$|_[0-9].[0-9]$| [0-9].[0-9]-[0-9].[0-9]$|-$|_$",
                      "", Model)) %>%
  mutate(Model = gsub("\\+ V.14E2| EMF30| ADVANCE|ix|-Buildings|-Transport|-MAgPIE| ",
                      "", Model)) %>%
  mutate(Model = replace(Model, Model == "WITCH-GLOBIOM", "WITCH")) %>%
  ggplot() + facet_wrap(~Category, scales = "fixed", nrow = 2) +
  geom_line(aes(x = year, y = value, group = Pathway, color = Model), size = 1, alpha = 0.8) +
  #scale_y_continuous(labels = scales::percent) +
  labs(x = "Year", y = "Tillion US$2010 per year") +
  theme_bw() + theme0 +
  labs(caption = c(
    "\nData source: IPCC AR6 Scenario Database \nVariable(s): Capital Formation"),
    title = "Capital Formation in IPCC AR6 pathways",
    subtitle = "C1-C8 scenarios (n = 178 / 1202); Model version identifier omitted") ->
  p;p
p %>% Write_png("Capital Formation", h = 2800, w = 5000, r = 300)



AR6_vetted %>% filter(Variable == "Investment|Energy Efficiency")  %>%
  mutate(value = value / 1000) %>%
  # clean model identifier
  mutate(Model = gsub("\\+ V.14E2| GECO2019| ENGAGE|_GEI|EMF33| [0-9].[0-9]$| [0-9].[0-9].[0-9]$|_[0-9].[0-9]$| [0-9].[0-9]-[0-9].[0-9]$|-$|_$",
                      "", Model)) %>%
  mutate(Model = gsub("\\+ V.14E2| EMF30| ADVANCE|ix|-Buildings|-Transport|-MAgPIE| ",
                      "", Model)) %>%
  mutate(Model = replace(Model, Model == "WITCH-GLOBIOM", "WITCH")) %>%
  ggplot() + facet_wrap(~Category, scales = "fixed", nrow = 2) +
  geom_line(aes(x = year, y = value, group = Pathway, color = Model), size = 1, alpha = 0.8) +
  #scale_y_continuous(labels = scales::percent) +
  labs(x = "Year", y = "Tillion US$2010 per year") +
  theme_bw() + theme0 +
  labs(caption = c(
    "\nData source: IPCC AR6 Scenario Database \nVariable(s): Investment|Energy Efficiency"),
    title = "Investment-Energy Efficiency (efficiency-increasing components of energy demand technologies) in IPCC AR6 pathways",
    subtitle = "C1-C8 scenarios (n = 105 / 1202); Model version identifier omitted") ->
  p;p
p %>% Write_png("Investment_EnergyEfficiency", h = 2800, w = 5000, r = 300)

AR6_vetted %>% filter(Variable == "Investment|Energy Supply")  %>%
  mutate(value = value / 1000) %>%
  # clean model identifier
  mutate(Model = gsub("\\+ V.14E2| GECO2019| ENGAGE|_GEI|EMF33| [0-9].[0-9]$| [0-9].[0-9].[0-9]$|_[0-9].[0-9]$| [0-9].[0-9]-[0-9].[0-9]$|-$|_$",
                      "", Model)) %>%
  mutate(Model = gsub("\\+ V.14E2| EMF30| ADVANCE|ix|-Buildings|-Transport|-MAgPIE| ",
                      "", Model)) %>%
  mutate(Model = replace(Model, Model == "WITCH-GLOBIOM", "WITCH")) %>%
  ggplot() + facet_wrap(~Category, scales = "fixed", nrow = 2) +
  geom_line(aes(x = year, y = value, group = Pathway, color = Model), size = 1, alpha = 0.8) +
  #scale_y_continuous(labels = scales::percent) +
  labs(x = "Year", y = "Tillion US$2010 per year") +
  theme_bw() + theme0 +
  labs(caption = c(
    "\nData source: IPCC AR6 Scenario Database \nVariable(s): Investment|Energy Supply"),
    title = "Investment-Energy Supply (investments into the energy supply system) in IPCC AR6 pathways",
    subtitle = "C1-C8 scenarios (n = 901 / 1202); Model version identifier omitted") ->
  p;p
p %>% Write_png("Investment_EnergySupply", h = 2800, w = 5000, r = 300)


AR6_vetted %>% filter(Variable %in% c("Investment|Energy Supply", "GDP|PPP"))  %>%
  mutate(value = value / 1000) %>%
  # clean model identifier
  mutate(Model = gsub("\\+ V.14E2| GECO2019| ENGAGE|_GEI|EMF33| [0-9].[0-9]$| [0-9].[0-9].[0-9]$|_[0-9].[0-9]$| [0-9].[0-9]-[0-9].[0-9]$|-$|_$",
                      "", Model)) %>%
  mutate(Model = gsub("\\+ V.14E2| EMF30| ADVANCE|ix|-Buildings|-Transport|-MAgPIE| ",
                      "", Model)) %>%
  mutate(Model = replace(Model, Model == "WITCH-GLOBIOM", "WITCH")) %>%
  spread(Variable, value) %>%
  filter(!is.na(`GDP|PPP`), !is.na(`Investment|Energy Supply`)) %>%
  ggplot() + facet_wrap(~Category, scales = "fixed", nrow = 2) +
  geom_line(aes(x = `GDP|PPP`, y = `Investment|Energy Supply`, group = Pathway, color = Model), size = 1, alpha = 0.8) +
  #scale_y_continuous(labels = scales::percent) +
  geom_abline(slope = 0.05, intercept = 0) +
  geom_abline(slope = 0.01, intercept = 0) +
  geom_abline(slope = 0.001, intercept = 0) +
  labs(x = "Year", y = "Tillion US$2010 per year") +
  theme_bw() + theme0 +
  labs(caption = c(
    "\nData source: IPCC AR6 Scenario Database \nVariable(s): Investment|Energy Supply and GDP|PPP"),
    title = "Investment-Energy Supply vs. GDP",
    subtitle = "C1-C8 scenarios (n = 839 / 1202); Model version identifier omitted") ->
  p;p
p %>% Write_png("InvestmentES_VS_GDP", h = 2800, w = 5000, r = 300)


AR6_vetted %>% filter(Variable %in% c("Investment", "GDP|PPP"))  %>%
  mutate(value = value / 1000) %>%
  # clean model identifier
  mutate(Model = gsub("\\+ V.14E2| GECO2019| ENGAGE|_GEI|EMF33| [0-9].[0-9]$| [0-9].[0-9].[0-9]$|_[0-9].[0-9]$| [0-9].[0-9]-[0-9].[0-9]$|-$|_$",
                      "", Model)) %>%
  mutate(Model = gsub("\\+ V.14E2| EMF30| ADVANCE|ix|-Buildings|-Transport|-MAgPIE| ",
                      "", Model)) %>%
  mutate(Model = replace(Model, Model == "WITCH-GLOBIOM", "WITCH")) %>%
  spread(Variable, value) %>%
  filter(!is.na(`GDP|PPP`), !is.na(`Investment`)) %>%
  ggplot() + facet_wrap(~Category, scales = "fixed", nrow = 2) +
  geom_line(aes(x = `GDP|PPP`, y = `Investment`, group = Pathway, color = Model), size = 1, alpha = 0.8) +
  #scale_y_continuous(labels = scales::percent) +
  geom_abline(slope = 0.4, intercept = 0) +
  geom_abline(slope = 0.2, intercept = 0) +
  geom_abline(slope = 0.1, intercept = 0) +
  geom_abline(slope = 0.01, intercept = 0) +
  labs(x = "Year", y = "Tillion US$2010 per year") +
  theme_bw() + theme0 +
  labs(caption = c(
    "\nData source: IPCC AR6 Scenario Database \nVariable(s): Investment and GDP|PPP"),
    title = "Investment vs. GDP",
    subtitle = "C1-C8 scenarios (n = 384 / 1202); Model version identifier omitted") ->
  p;p
p %>% Write_png("Investment_VS_GDP", h = 2800, w = 5000, r = 300)



