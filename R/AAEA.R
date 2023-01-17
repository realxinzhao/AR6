library(dplyr)
library(ggplot2)
library(patchwork)
require(readr, readxl)    # Load data

## Please download data from  https://data.ene.iiasa.ac.at/ar6/#/workspaces
# or from Xin' dropbox:
# https://www.dropbox.com/sh/223pixhu22uq4d2/AACD25bnA-ad6dIMl2kHqB8oa?dl=0

# Put the data under
# data/AR6/1648976687084-AR6_Scenarios_Database_World_v1.0/AR6_Scenarios_Database_World_v1.0.CSV


outdir <- "output/AAEA/"
# Plots ----

## ggplot theme ----

windowsFonts("Arial" = windowsFont("Arial"))
theme0 <- theme(
  panel.grid.minor = element_blank(),
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
  #axis.title.x = element_blank()
)

## func export fig ----
outdir <- "output/"
Write_png <- function(.plot, name, w = 9000, h = 4500, r = 600){
  png(paste0(outdir,name,".png"), width = w, height = h, res = r)
  print(.plot)
  dev.off()
}

## Load AR6 source data ----
#
AR6 <- readr::read_csv("data/AR6/1648976687084-AR6_Scenarios_Database_World_v1.0/AR6_Scenarios_Database_World_v1.0.CSV")

## Load C1-C8 category from meta----
MS_Category <- readxl::read_excel("data/AR6/1648976687084-AR6_Scenarios_Database_World_v1.0/AR6_Scenarios_Database_metadata_indicators_v1.0.xlsx",
                                  sheet = 2) %>%
  select(Model, Scenario, Category, Policy_category, Project_study, SSP_family = Ssp_family) %>%
  mutate(SSP_family = paste0("SSP", SSP_family))


# Gather, clean, fill, and join
AR6 %>%
  mutate(Pathway = interaction(Model, Scenario)) %>%
  filter(Pathway %in% c(MS_Category %>% distinct(interaction(Model, Scenario)) %>% pull) ) %>%
  left_join(MS_Category, by = c("Model", "Scenario")) %>%
  gcamdata::gather_years() %>%
  group_by(Pathway, Variable, Scenario, Model, Region, Unit) %>%
  mutate(value = gcamdata::approx_fun(year, value, rule = 2)) %>%
  ungroup() %>%
  filter(year >= 2020) ->
  AR6_vetted


#readRDS("data/AR6/RDS/AR6_vetted.rds") -> AR6_vetted


# Load Variable mapping
VariableMapping <- readr::read_csv("data/AR6/VariableMapping.csv", comment = "#")

AR6_vetted %>% distinct(Variable, Unit) %>%
  left_join(VariableMapping %>% select(Variable, VarGroup), by = "Variable") %>%
  group_by(VarGroup) %>%
  summarise(n = n())->
  VarGroup

VarGroup %>% arrange(n) %>% pull(VarGroup) -> ffVarGroup

VarGroup %>% mutate(VarGroup = factor(VarGroup, levels = ffVarGroup)) %>%
  ggplot() +
  geom_bar(aes(x = VarGroup, y = n, fill = VarGroup), stat = "identity", color = 1) +
  labs(y = "Count") +
  labs(title = "(a) AR6 variables") +
  labs(x = "AR6 Variable Group") +
  theme_bw() + theme0 +
  coord_flip() +
  theme(legend.position = "none") -> p1

AR6_vetted %>% distinct(Variable, Unit) %>%
  left_join(VariableMapping %>% select(Variable, VariableL1, VarGroup), by = "Variable") %>%
  filter(VarGroup == "AgLU") %>%
  group_by(VariableL1) %>%
  summarise(n = n()) ->
  VarGroup_AgLU

VarGroup_AgLU %>% arrange(n) %>% pull(VariableL1) -> ffVarGroup_AgLU

VarGroup_AgLU %>% mutate(Var = "AgLU") %>%
  mutate(VariableL1 = factor(VariableL1, levels = ffVarGroup_AgLU)) %>%
  ggplot() +
  geom_bar(aes(x = VariableL1, y = n, fill = Var), stat = "identity", color = 1) +
  labs(y = "Count") +
  theme_bw() + theme0 + theme(legend.position = "none") +
  scale_fill_manual(values = "darkgreen") +
  labs(x = "AgLU Variable Group") +
  labs(title = "(b) AgLU variables") +
  coord_flip() -> p2


AR6_vetted %>%
  group_by(Pathway, Variable, Unit) %>%
  summarise(value = sum(value), .groups = "drop") %>%
  left_join(VariableMapping %>% select(Variable, VariableL1,VarGroup), by = "Variable") %>%
  distinct() %>%
  filter(value != 0) ->
  VarPathwayCount


VarPathwayCount %>% filter(value != 0) %>%
  group_by(VarGroup) %>%
  summarise(nVarPath = n()) %>%
  left_join(VarGroup %>% rename(nVar = n)) %>%
  mutate(n = round(nVarPath/nVar, 0) ) %>%
  mutate(VarGroup = factor(VarGroup, levels = ffVarGroup)) %>%
  ggplot() +
  geom_bar(aes(x = VarGroup, y = n, fill = VarGroup), stat = "identity", color = 1) +
  labs(y = "Count") +
  labs(title = "(c) Pathway by AR6 Variable") +
  labs(x = "AR6 Variable Group") +
  theme_bw() + theme0 +
  coord_flip() +
  theme(legend.position = "none") -> p3

VarPathwayCount %>% filter(value != 0) %>%
  filter(VarGroup == "AgLU") %>%
  group_by(VariableL1) %>%
  summarise(nVarPath = n()) %>%
  left_join(VarGroup_AgLU %>% rename(nVar = n)) %>%
  mutate(n = round(nVarPath/nVar, 0) ) %>% mutate(Var = "AgLU") %>%
  mutate(VariableL1 = factor(VariableL1, levels = ffVarGroup_AgLU)) %>%
  ggplot() +
  geom_bar(aes(x = VariableL1, y = n, fill = Var), stat = "identity", color = 1) +
  labs(y = "Count") +
  scale_fill_manual(values = "darkgreen") +
  theme_bw() + theme0 + theme(legend.position = "none") +
  labs(x = "AgLU Variable Group") +
  labs(title = "(d) Pathway by AgLU Variable") +
  coord_flip() -> p4



p1 + p2 + p3 + p4 + plot_layout(ncol = 4)-> Fig0

Fig0 %>% Write_png("Histogram1", h = 1200, w = 4000, r = 150)


VarPathwayCount %>%
  group_by(Variable, VariableL1, VarGroup) %>%
  summarise(nVarPath = n()) %>%
  mutate(VarGroup = factor(VarGroup, levels = rev(ffVarGroup))) %>%
  ggplot() +
  geom_hline(yintercept = 1202, linetype = 2) +
  geom_boxplot(aes(x = VarGroup, y = nVarPath, fill = VarGroup))+
  geom_text(data = VarPathwayCount %>%
              group_by(Variable, VariableL1, VarGroup) %>%
              summarise(nVarPath = n(), .groups = "drop") %>%
              group_by(VarGroup) %>%
              summarise(n = n()), aes(x = VarGroup, y = 1300, label = n),
            hjust = 0.5, size = 6, color = "blue", fontface = 4
            ) +
  scale_y_continuous(breaks = c(0, 500, 1000, 1202)) +
  labs(y = "Count of pathway") +
  labs(title = "(a) Variable Group") +
  labs(x = "AR6 Variable Group") +
  theme_bw() + theme0 +
  theme(legend.position = "none", plot.title = element_text(face="bold"),
        axis.text.x = element_text(angle = 35, hjust = 0.9, vjust = 1)) -> p3; p3
#p3 %>% Write_png("Histogram2", h = 800, w = 1200, r = 150)


VarPathwayCount %>% filter(VarGroup == "AgLU") %>%
  group_by(Variable, VariableL1, VarGroup) %>%
  summarise(nVarPath = n()) %>%
  mutate(VariableL1 = factor(VariableL1, levels = rev(ffVarGroup_AgLU))) %>%
  ggplot() +
  geom_hline(yintercept = 1202, linetype = 2) +
  geom_boxplot(aes(x = VariableL1, y = nVarPath, fill = VariableL1)) +
  geom_text(data = VarPathwayCount %>% filter(VarGroup == "AgLU") %>%
              group_by(Variable, VariableL1, VarGroup) %>%
              summarise(nVarPath = n(), .groups = "drop") %>%
              group_by(VariableL1) %>%
              summarise(n = n()), aes(x = VariableL1, y = 1300, label = n),
            hjust = 0.5, size = 6, color = "blue", fontface = 4
  ) +
  scale_y_continuous(breaks = c(0, 500, 1000, 1202)) +
  labs(y = "Count of pathway") +
  labs(title = "(b) AR6-AgLU Variable Group") +
  labs(x = "AR6 AgLU Variable Group") +
  theme_bw() + theme0 +
  theme(legend.position = "none", plot.title = element_text(face="bold"),
        axis.text.x = element_text(angle = 35, hjust = 0.9, vjust = 1)) -> p4; p4

#p3+p4 -> Fig1

#Fig1 %>% Write_png("Histogram3", h = 900, w = 2000, r = 150)


VarPathwayCount %>% filter(VarGroup == "AgLU") %>%
  group_by(Variable, VariableL1, VarGroup) %>%
  summarise(nVarPath = n(), .groups = "drop") %>%
  group_by(VariableL1) %>%
  mutate(totaln = n()) %>% ungroup() %>%
  arrange(-totaln, -nVarPath) -> df
df %>%
  mutate(VariableL1 = factor(VariableL1, levels = unique(df$VariableL1)),
         Variable = factor(Variable, levels = unique(df$Variable))) %>%
  ggplot() +
  geom_bar(aes(x = Variable, y = nVarPath, fill = VariableL1), stat = "identity", color = 1) +
  #scale_fill_manual(values = "darkgreen") +
  theme_bw() + theme0 + theme(legend.position = "none") +
  labs(x = "AgLU Variables", y = "Count of pathway",
       title = "(C) Histogram of AR6-AgLU Variables") +
  theme(legend.position = "none", plot.title = element_text(face="bold"),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10),
        axis.title.x = element_blank()) -> p5;p5
#p5 %>% Write_png("Histogram4", h = 900, w = 2000, r = 100)


(p3 + theme(axis.text.x = element_text(size = 12), axis.title.x = element_blank() ) +
              p4 + theme(axis.text.x = element_text(size = 12), axis.title.x = element_blank()))/p5 +
    plot_layout(heights = c(1,0.5)) -> Fig1

Fig1 %>% Write_png("Fig1", h = 1700, w = 2600, r = 200)

### Fig. 2 ----


AR6_vetted %>% filter(Category %in% paste0("C", 1:8)) %>%
  filter(Variable %in% c("Land Cover|Cropland|Energy Crops",
                         "Land Cover|Cropland",
                         "Land Cover|Forest",
                         "Land Cover|Pasture")) ->
  AR6Land



AR6Land %>% filter(year %in% c(2020, 2100)) %>%
  group_by_at(vars(-year, -value)) %>%
  mutate(value = value - value[year == 2020]) %>%
  filter(year != 2020) %>%
  mutate(Variable = gsub("Land Cover\\|", "", Variable)) %>%
  ggplot() + facet_wrap(~Variable, scales = "fixed") +
  geom_hline(yintercept = 0, linetype = 1) +
  geom_boxplot(aes(x = Category, y = value, fill = Variable)) +
  geom_text(data = AR6Land %>% filter(year %in% c(2020, 2100)) %>%
              group_by_at(vars(-year, -value)) %>%
              mutate(value = value - value[year == 2020]) %>%
              filter(year != 2020) %>%
              mutate(Variable = gsub("Land Cover\\|", "", Variable)) %>%
              group_by(Variable, Category) %>%
              summarise(n = n(), .groups = "drop") %>%
              mutate(h = if_else(grepl("Energy|Pasture", Variable), 1450, 1450)),
            aes(x = Category, y = h, label = n, color = Variable, group = Variable),
            hjust = 0.5, size = 4, fontface = 4,
            position=position_dodge(width = .9)
  ) +
  labs(y = "Mha") +
  labs(title = "(b) Land use change in 2020 - 2100") +
  labs(x = "Climate Category") +
  theme_bw() + theme0 +
  theme(plot.title = element_text(face="bold"), legend.position = "none",
        axis.text.x = element_text(angle = 0, hjust = .5, vjust = 1)) -> p6; p6



AR6_vetted %>% filter(year %in% c(2100)) %>%
  filter(Category %in% paste0("C", 1:8)) %>%
  filter(Variable %in% c("AR6 climate diagnostics|Surface Temperature (GSAT)|MAGICCv7.5.3|50.0th Percentile",
                         "Emissions|CO2",
                         "GDP|PPP",
                         "Food Demand",
                         "Price|Agriculture|Wheat|Index",
                         "Price|Carbon")) ->
  AR6tempEms

AR6tempEms  %>%
  filter(!(Variable == "Food Demand" & value >10000 )) %>%
  filter(!(Variable == "Price|Agriculture|Wheat|Index" & value >10 )) %>%
  filter(!(Variable == "Price|Carbon" & value >10000 )) %>%
  mutate(value = if_else(Variable == "GDP|PPP", value / 1000, value),
         value = if_else(Variable == "Emissions|CO2", value / 1000, value)) %>%
  mutate(Variable = factor(Variable,
                           levels = c("AR6 climate diagnostics|Surface Temperature (GSAT)|MAGICCv7.5.3|50.0th Percentile",
                                      "Emissions|CO2",
                                      "Price|Carbon",
                                      "Food Demand",
                                      "Price|Agriculture|Wheat|Index",
                                      "GDP|PPP" ),
                           labels = c("Temperature (°C)",
                                      "CO2  (GtCO2/yr)",
                                      "C prices (US$2010/tCO2)",
                                      "Food (kcal/cap/day)",
                                      "Wheat prices (2010 = 1)",
                                      "GDP (Tril US$2010/yr)")
                           ))->
  AR6tempEms1

  AR6tempEms1 %>%
  ggplot() + facet_wrap(~Variable, scales = "free_y") +
  geom_boxplot(aes(x = Category, y = value, fill = Variable)) +
  geom_text(data = AR6tempEms1 %>%
              group_by(Variable, Category) %>%
              summarise(n = n(), value = max(value), .groups = "drop") %>%
              group_by(Variable) %>%
              mutate(h = max(value)),
            aes(x = Category, y = h * 1.1, label = n, color = Variable, group = Variable),
            hjust = 0.5, size = 4, fontface = 4,
            position=position_dodge(width = .9)
  ) +
  labs(title = "(a) Key AR6 Economic and AgLU Variables in 2100") +
  labs(x = "Climate Category") +
  theme_bw() + theme0 +
  theme(plot.title = element_text(face="bold"), legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 0, hjust = .5, vjust = 1)) -> p7; p7

p7 + p6 +   plot_layout(widths = c(0.65,0.4)) -> Fig2

Fig2 %>% Write_png("Fig2", h = 1700, w = 3800, r = 200)




# AR6_vetted %>% filter(Category %in% paste0("C", 1:8)) %>%
#   filter(Variable %in% c("Population",
#                          "Emissions|CO2|AFOLU" ))
# # Check variable
# AR6_vetted %>% distinct(Variable) %>%
#   filter(grepl("Temp", Variable, ignore.case = T) == T) %>% pull






