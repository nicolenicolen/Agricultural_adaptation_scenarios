# Agricultural Productivity Index (API)

library(ggplot2)
library(tidyverse)
library(plm)
library(broom)
library(WDI)
library(countrycode)
library(data.table)
library(readstata13)
library(viridis)
library(ggthemes)
library(sandwich)
library(lmtest)
library(stargazer)
library(dplyr)
library(readxl)
library(gridExtra)
library(betareg)
library(viridis)
library(ggthemes)
library(maps)
library(rworldmap)
library(mapproj)
library(writexl)
library(scales)
library(rlang)
library(car)
library(caret)
library(glm.predict)
library(sjPlot)
library(tidyr)
library(plotly)
library(factoextra)
library(devtools)
library(ggbiplot)
library(corrplot)
library(Rcpp)
library(knitr)
library(naniar)

############### Developing the index  ##################
########################################################

############## API ############## 

# API = value of agricultural production / people employed in agriculture 

# Value of agricultural production (FAOSTAT)
# Gross Production Value (constant 2014-2016 1000 US$)

value <- read_xls("/Users/nicole/Desktop/Mechanization/Data/Mechanisation/FAOSTAT_data_AGR_VALUE_all.xls") %>%
  rename_all(tolower) %>%
  dplyr::select(area, year, value) %>%
  dplyr::rename(country = area) %>%
  dplyr::rename(value = value) %>%
  mutate(value = as.numeric(value)) %>%
  mutate(countrycode = countrycode(country, 'country.name', 'iso3c')) %>%
  dplyr::select(-country)

# Employment in agiruclture, forestry and fishing (ILO modelled estimate)
# 1000 persons (total) - divide between male and female available 

employed <- read_xls("/Users/nicole/Desktop/Mechanization/Data/Mechanisation/FAOSTAT_data_AGR_EMPLOY_ILOmodelledestimate.xls") %>%
  rename_all(tolower) %>%
  dplyr::select(area, year, sex, value) %>%
  filter(sex == "Total") %>% # filter "Total" for now
  dplyr::rename(country = area) %>%
  dplyr::rename(employed = value) %>%
  mutate(employed = as.numeric(employed)) %>%
  mutate(employed = employed*1000) %>% # unit was 1000 persons
  mutate(countrycode = countrycode(country, 'country.name', 'iso3c')) %>%
  dplyr::select(-country)
employed <- employed[!is.na(employed$countrycode), ]

# Establish the index 

api_df <- value %>%
  dplyr::left_join(employed, by = c('countrycode', 'year')) %>%
  na.omit() %>%
  dplyr::group_by(countrycode, year) %>%
  mutate(api = value/employed) %>%
  mutate(api_log = log(value/employed)) %>%
  ungroup() %>%
  filter(countrycode != "ARG") %>%
  mutate(scenario = "Baseline") 
hist(api_df$api_log)
hist(api_df$api)

########################################################
############### Plot the index  ########################
########################################################

# Average data to look at the map (1996-2005) 

dat <- api_df %>%
  dplyr::select(countrycode, year, api_log, employed, value) %>%
  filter(year %in% 2000:2010) %>%
  dplyr::group_by(countrycode) %>% 
  dplyr::mutate(average = mean(api_log, na.rm = TRUE)) %>%
  dplyr::mutate(average_value = mean(value, na.rm = TRUE)) %>%
  dplyr::mutate(average_employed = mean(employed, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(year == "2005") 

result <- api_df %>%
  dplyr::filter(countrycode == "USA", year == 2005) %>%
  dplyr::mutate(var = mean(api_log, na.rm = TRUE),
                var_value = mean(value, na.rm = TRUE),
                var_employed = mean(employed, na.rm = TRUE)) %>%
  dplyr::select(var, var_value, var_employed)

print(result)

value <- 86558132
employed <- 41662251

api_log <- log(value/employed)
print(api_log)

# Create colorscale 
colors <- c("SSP1" = "#B06D4C",  # deep sky blue
            "SSP2" = "#8AA7B1",  # pale turquoise
            "SSP4" = "#6B8F7E",  # olive green
            "SSP5" = "#46637F")  # sienna

# Create a function that generates a continuous colorscale
color_scale <- colorRampPalette(colors)
# Generate a range of colors using the color_scale function
n_colors <- 50 # number of colors in the scale
color_range <- color_scale(n_colors)

# Join data to map 

map <- joinCountryData2Map(dat, joinCode = "ISO3", nameJoinColumn = "countrycode")

map_poly <-  fortify(map) %>% 
  merge(map@data, by.x="id", by.y="ADMIN", all.x=T) %>%
  arrange(id, order) %>% 
  mutate(var %>% as.numeric())

# Plot map # color_range 

ggplot(map_poly, aes(x = long, y = lat, group = group)) +
  coord_map(projection = 'rectangular',parameters = 0, orientation=NULL, xlim = c(-180, 180), ylim = c(-55, 75)) + 
  #coord_map(projection = 'mollweide', xlim = c(-180, 180), ylim = c(-60, 75))  + # Remove antarctica
  geom_polygon(aes(fill = var)) +
  scale_fill_gradientn(colors = color_range, na.value = "#C1CDCD") + # Set the colorscale
  labs(fill = 'log(API)'
       ,title = ' '   # Change the title of the map
       ,x = NULL
       ,y = NULL) +
  theme(text = element_text(family = 'Arial', color = 'gray6')
        ,plot.title = element_text(size = 12)
        ,axis.ticks = element_blank()
        ,axis.text = element_blank()
        ,axis.line = element_blank()
        ,panel.grid = element_blank()
        ,panel.background = element_rect(fill = 'white')
        ,plot.background = element_rect(fill = 'white')
        ,legend.position = c(.08,.26)
        ,legend.background = element_blank()
        ,legend.key = element_blank()
  ) +
  annotate(geom = 'text'
           ,label = ''
           ,x = 18, y = -55
           ,size = 2
           ,family = 'Arial'
           ,color = 'gray6'
           ,hjust = 'left'
  )

#ggsave("/Users/nicole/Desktop/Mechanization/Figures/Background_Figures/API_newcolors.png", width = 8, height = 6, dpi = 300)

# short analysis 

mean_api_log <- mean(dat$api_log)
sd_api_log <- sd(dat$api_log)
highest_api_log_country <- dat$countrycode[which.max(dat$api_log)]
lowest_api_log_country <- dat$countrycode[which.min(dat$api_log)]
threshold_log <- mean_api_log # Set your desired log-transformed API threshold
above_threshold_log <- sum(dat$api_log > threshold_log)

print(mean_api_log)
print(sd_api_log)
print(highest_api_log_country)
print(lowest_api_log_country)
print(threshold_log)
print(above_threshold_log)

########################################################
#################### Regions   #########################
########################################################

regions <- read.csv("/Users/nicole/Desktop/Mechanization/Data/Regions/Regions_WB.csv")
regions_global <- read_xlsx("/Users/nicole/Desktop/Mechanization/Data/Regions/Regions_WB_global.xlsx")
income_group <- read_xlsx("/Users/nicole/Desktop/Mechanization/Data/Regions/income_group_WB.xlsx")
developed_developing <- read_xlsx("/Users/nicole/Desktop/Mechanization/Data/Regions/historical-classification-of-developed-and-developing-regions.xlsx")

########################################################
###############  Statistical analysis   ################
########################################################

# Look at the distribution of the data 

hist(api_df$api_log, 
     prob = TRUE,
     #freq = FALSE,
     main="Histogram", 
     xlab="API", 
     border="blue", 
     col="lightblue", 
     breaks = 20)
#lines(density(GAMI$limits), lwd = 2, col = 'red')
x <- seq(min(api_df$api_log), max(api_df$api_log), length = 88)
f <- dnorm(x, mean = mean(api_df$api_log), sd = sd(api_df$api_log))
lines(x, f, col = "red", lwd = 1)

########################################################
##########  Load socioeconomic variables   #############
########################################################

gdppc_hist <- read.csv("/Users/nicole/Desktop/Mechanization/Data/Socioeconomic_variables/gdppc_wb.csv", skip = 4) %>%
  rename_all(tolower) %>%
  pivot_longer(x1960:x2019, names_to = "year", values_to = "gdppc") %>%
  mutate(year = year %>% str_replace("x", "") %>% as.integer()) %>%
  dplyr::select(country.code, year, gdppc) %>%
  filter(year %in% 1991:2020) %>%
  mutate(year = as.character(year)) %>%
  mutate(gdppc_log = log(gdppc)) %>%
  mutate(gdppc_n = (gdppc-102.598)/(189422.2-102.598)) %>%
  dplyr::rename(countrycode = "country.code")

urb_hist <- read_xls('/Users/nicole/Desktop/Mechanization/Data/Socioeconomic_variables/Urbanization/SSP1_WB.xls') %>%
  gather(key = "year", value = "urbanization", 2:142) %>%
  filter(year %in% 1991:2008) %>%
  dplyr::rename(countrycode = "country or area") %>%
  mutate(year = as.character(year)) %>%
  mutate(urbanization_log = log(urbanization)) %>%
  mutate(urbanization_n = (urbanization-5.491)/(98.139-5.491)) # normalize

edu_hist <- read.csv("/Users/nicole/Desktop/Mechanization/Data/Socioeconomic_variables/education_historical_WC.csv") %>%
  dplyr::select(countrycode, post.secondary, year) %>%
  dplyr::rename(education = "post.secondary") %>%
  filter(year %in% 1970:2015) %>%
  mutate(year = as.character(year))

edu_hist <- edu_hist %>%
  dplyr::arrange(countrycode, year) %>%
  dplyr::group_by(countrycode) %>%
  dplyr::mutate(education_lag = dplyr::lag(education, n = 5))

#edu_hist <- edu_hist %>%
  #dplyr::group_by(countrycode, year) %>%
  #mutate(education_lag = dplyr::lag(education, n = 5, default = NA))

# Share labour employed in agriculture

share_labor <- read.csv("/Users/nicole/Desktop/Mechanization/Data/Risk/share-of-the-labor-force-employed-in-agriculture.csv") %>%
  rename_all(tolower) %>%
  dplyr::select(-entity) %>%
  dplyr::rename(countrycode = "code") %>%
  #filter(year %in% 1996:2005) %>%
  mutate(year = as.character(year)) %>%
  mutate(share_employed_agri_n = (share_employed_agri-0.13)/(92.37-0.13)) #

# Create master 

master <- api_df %>%
  dplyr::left_join(gdppc_hist, countrycode, by = c('countrycode', 'year')) %>% 
  dplyr::left_join(urb_hist, countrycode, by = c('countrycode', 'year')) %>% 
  dplyr::left_join(edu_hist, countrycode, by=c('countrycode', 'year')) %>%
  dplyr::left_join(share_labor, countrycode, by=c('countrycode', 'year')) %>%
  dplyr::left_join(regions_global, countrycode, by=c('countrycode')) %>%
  mutate(scenario = "Baseline") %>%
  dplyr::select(-value, -sex, -employed) 
summary(master[c("gdppc", "urbanization", "education_lag")])

plot(api_log ~ log(gdppc), pch = 19, col = "black", data = master)
plot(api_log ~ log(I(urbanization^2)), pch = 19, col = "black", data = master)
plot(api_log ~ log1p(I(education_lag^2)), pch = 19, col = "black", data = master)

# relationship between urbanization and share employed in agriculture 

plot(urbanization ~ share_employed_agri, pch = 19, col = "black", data = master)
lm_urb <- lm(urbanization ~ share_employed_agri, data = master)
summary(lm_urb)

########################################################
###########  Prep statistical analysis  ################
########################################################

plm_api <- plm(api_log ~ log(gdppc) + log(I(urbanization^2)) + log1p(I(education_lag^2)), data = master, effect = c("individual"), model = c("within"), index = c("countrycode"))
summary(plm_api) 
lmtest::bptest(plm_api)
coeftest(plm_api, vcovHC(plm_api, type = 'HC0', cluster = c('group')))

# Hausmann test 

plm_fixed <- plm(api_log ~ log(gdppc) + log(I(urbanization^2)) + log1p(I(education_lag^2)), data = master, effect = c("individual"), model = c("within"), index = c("countrycode"))
plm_random <- plm(api_log ~ log(gdppc) + log(I(urbanization^2)) + log1p(I(education_lag^2)), data = master, effect = c("individual"), model = c("random"), index = c("countrycode"))
phtest(plm_fixed, plm_random) -> Hausmann_test

# coefficients from plm()

gdppc_fit = 0.152002
urb_fit = 0.147856
edu_fit = 11.235020

# Get confidence intervals (normal)  

tidy(plm_api, conf.int = TRUE, conf.level = 0.95)

gdppc_low = 0.131
urb_low = 0.0954
edu_low = 9.36

gdppc_high = 0.173
urb_high = 0.200
edu_high = 13.1

# safe country-fixed effects 

fix_ef <- data.frame(countrycode = names(fixef(plm_api)),
                     fixef = as.vector(fixef(plm_api)))

########################################################
############# Adjust intercept manually ################
########################################################

# Select API and socioeconomic variables for last year of observations (2019) - 2008 because observations for urbanization end in 2008

api_int <- master %>%
  filter(year == "2008") %>% # 2008 because observations for urbanization end in 2008 
  dplyr::select(api_log, gdppc, urbanization, education, countrycode)

# Get new intercept (Anton's formula)

intercept <- api_int %>%
  mutate(fixef_adj = api_log - (gdppc_fit*log(gdppc)) - (urb_fit*log(urbanization)) - (edu_fit*log1p(education))) %>%
  mutate(fixef_adj_low = api_log - (gdppc_low*log(gdppc))  - (urb_low*log(urbanization)) - (edu_low*log1p(education))) %>%
  mutate(fixef_adj_high = api_log - (gdppc_high*log(gdppc)) - (urb_high*log(urbanization)) - (edu_high*log1p(education))) %>%
  dplyr::select(countrycode, fixef_adj, fixef_adj_low, fixef_adj_high)

socio_proj <- read.csv('/Users/nicole/Desktop/Mechanization/Data/Socioeconomic_variables/Projections/Pop_Edu_Gov_Gii_Urb_obs_proj.csv') %>%
  dplyr::select(countrycode, year, scenario, postsec, gdppc, urbanization) %>%
  dplyr::rename(education = "postsec") %>%
  filter(year %in% seq(2020, 2100, 5)) %>%
  filter(!countrycode %in% c("ARG", "AUT", "BHR", "BHS", "BLZ", "BRB", "COD", "COM", "CUB", "DJI", "GAB", "GRD", "GTM", "GUY", "HKG", "HTI", "KGZ", "KWT", "LBR", "LBY", "LCA", "LSO", "MRT", "PNG", "SDN", "SGP", "SLB", "SOM", "SRB", "STP", "SWZ", "SYR", "TJK", "TLS", "TON", "TUR", "UGA", "UZB", "WSM", "AGO", "BRN", "BWA", "ERI", "FJI", "ISR", "LKA", "OMN", "PSE", "TGO", "YEM"))%>%
  dplyr::left_join(intercept, countrycode, by = "countrycode")

# Apply projections 

api_proj <- socio_proj %>%
  mutate(api_proj = fixef_adj + gdppc_fit*log(gdppc) + urb_fit*log(urbanization) + edu_fit*log1p(education)) %>%
  mutate(api_low = fixef_adj_low + gdppc_low*log(gdppc)  + urb_low*log(urbanization) + edu_low*log1p(education)) %>%
  mutate(api_high = fixef_adj_high + gdppc_high*log(gdppc)  + urb_high*log(urbanization) + edu_high*log1p(education))

# Average over regions 

Summary <- api_proj %>%
  dplyr::left_join(regions, countrycode, by = "countrycode") %>%
  dplyr::group_by(region, scenario, year) %>%
  dplyr::mutate(api_fit = mean(api_proj, na.rm = T),
                api_upr = mean(api_high, na.rm = T), 
                api_lwr = mean(api_low, na.rm = T),
                exp_api_fit = mean(exp(api_proj), na.rm = T),
                exp_api_upr = mean(exp(api_high), na.rm = T), 
                exp_api_lwr = mean(exp(api_low), na.rm = T)) %>%
  dplyr::ungroup() %>%
  drop_na(region) %>%
  mutate(year = as.numeric(year))

# Baseline 

Summary_baseline <- api_df %>%
  dplyr::left_join(regions, countrycode, by=c('countrycode')) %>%
  filter(year %in% 2000:2010) %>%
  dplyr::group_by(countrycode) %>% 
  dplyr::mutate(average = mean(api_log, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(year == 2005) %>%
  dplyr::group_by(region) %>%
  dplyr::mutate(api_baseline = mean(api_log, na.rm = T)) %>%
  dplyr::mutate(exp_api_baseline = mean(exp(api_log), na.rm = T)) %>%
  mutate(year = as.numeric(year)) %>%
  dplyr::ungroup() 

# Income groups 

income_baseline <- api_df %>%
  dplyr::left_join(income_group, countrycode, by =c('countrycode')) %>%
  #filter(year %in% 2000:2010) %>%
  dplyr::group_by(income_group) %>% 
  dplyr::mutate(average = mean(api_log, na.rm = TRUE)) %>%
  ungroup() %>%
  dplyr::group_by(income_group) %>%
  dplyr::mutate(api_baseline = mean(api_log, na.rm = T)) %>%
  mutate(year = as.numeric(year)) %>%
  dplyr::ungroup() 
summary(income_baseline)

cut_offs <- income_baseline %>%
  group_by(income_group) %>%
  summarize(max_api_log = max(api_baseline),
            min_api_log = min(api_baseline)) %>%
  mutate(income_group = fct_rev(income_group)) %>%
  mutate(ymin = min_api_log, ymax = max_api_log) %>%
  select(income_group, ymin, ymax) %>%
  filter(!is.na(income_group))
print(cut_offs)

# Plot 

library(ggplot2)
library(dplyr)
library(forcats)
library(RColorBrewer)

# Define function for number formatting
f <- function(x) {
  format(round(x, 1), nsmall = 1)
}

colors <- c("SSP1" = "#B06D4C",       # deep sky blue
            "Baseline" = "#E5B76E",   # sandy brown
            "SSP2" = "#8AA7B1",       # pale turquoise
            "SSP3" = "#D38D5F",       # dark orange
            "SSP4" = "#6B8F7E",       # olive green
            "SSP5" = "#46637F")       # sienna

ggplot() +
  geom_point(data = Summary_baseline %>% filter(year == 2005),
             aes(year, api_baseline, color = scenario)) +
  geom_ribbon(data = Summary %>% filter(year > 2019),
              aes(x = year, y = api_fit, ymin = api_lwr, ymax = api_upr,
                  color = scenario, fill = scenario), alpha = .3, linetype = 0, show.legend = FALSE) +
  geom_line(data = Summary %>% filter(year > 2019) %>% filter(scenario != "SSP1"), aes(year, api_fit, color = scenario),
            size = 0.8) +
  geom_line(data = Summary %>% filter(year > 2019) %>% filter(scenario == "SSP1"), aes(year, api_fit, color = scenario),
            size = 0.8) +
  facet_wrap(~ region, ncol = 3, scales = "free") +
  scale_x_continuous(breaks = c(1990, 2000, 2020, 2040, 2060, 2080, 2095),
                     labels = c("1990", "2000", "2020", "2040", "2060", "2080", "2100")) +
  scale_y_continuous(labels = f) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  labs(x = 'Year', y = 'log(API)', title = '') +
  theme_bw() +
  theme(panel.spacing = unit(0.9, "lines"),
        strip.text = element_text(size = 10),
        strip.background = element_blank(),
        axis.text.x = element_text(angle = 90, size = 10),
        axis.text.y = element_text(size = 10),
        legend.position = 'right',
        legend.text = element_text(size = 11),
        plot.title = element_text(hjust = 0.5, size = 18))
#ggsave("/Users/nicole/Desktop/Mechanization/Figures/Background_Figures/SSP_Projections_API_WBregions.png", width = 8, height = 6, dpi = 300)

# Dataset for income group cutoffs
cut_offs <- data.frame(income_group = c("High income", "Low income", "Lower middle income", "Upper middle income"),
                       cutoff = c(3.16, -0.182, 0.79, 1.48))

ggplot() +
  geom_point(data = Summary_baseline %>% filter(year == 2005),
             aes(year, api_baseline, color = "Baseline"), shape = 19) +
  geom_ribbon(data = Summary %>% filter(year > 2019),
              aes(x = year, y = api_fit, ymin = api_lwr, ymax = api_upr,
                  color = scenario, fill = scenario), alpha = .3, linetype = 0, show.legend = FALSE) +
  geom_line(data = Summary %>% filter(year > 2019), aes(year, api_fit, color = scenario),
            size = 0.8) +
  geom_text(data = cut_offs, aes(x = Inf, y = cutoff, label = income_group),
            hjust = 1.1, vjust = 0.3, color = "#444444", size = 3) +
  facet_wrap(~ region, ncol = 3, scales = "free") +
  scale_x_continuous(breaks = c(1990, 2000, 2020, 2040, 2060, 2080, 2095),
                     labels = c("1990", "2000", "2020", "2040", "2060", "2080", "2100")) +
  scale_y_continuous(labels = f) +
  scale_color_manual(values = colors,
                     guide = guide_legend(override.aes = list(shape = c(19, NA, NA, NA, NA, NA),
                                                              linetype = c(0, 1, 1, 1, 1, 1)) )) +
  scale_fill_manual(values = colors) +
  labs(x = 'Year', y = 'API', title = '') +
  theme_bw() +
  theme(panel.spacing = unit(0.9, "lines"),
        strip.text = element_text(size = 10),
        strip.background = element_blank(),
        axis.text.x = element_text(angle = 90, size = 10),
        axis.text.y = element_text(size = 10),
        legend.position = 'right',
        legend.text = element_text(size = 11),
        plot.title = element_text(hjust = 0.5, size = 18))
#ggsave("/Users/nicole/Desktop/Mechanization/Figures/Background_Figures/SSP_Projections_API_WBregions_plusIncomegrouptext.png", width = 10, height = 8, dpi = 300)


#### FINAL 

colors <- c("SSP1" = "#6B8F7E",  # olive green
            "Baseline" = "#E5B76E",  # sandy brown
            "SSP2" = "#D38D5F",  # pale turquoise
            "SSP3" = "#8AA7B1",  # dark orange
            "SSP4" = "#B06D4C",  # deep sky blue
            "SSP5" = "#46637F")  # sienna

# Define function for number formatting
f <- function(x) {
  format(round(x, 1), nsmall = 1)
}

# Lookup table for y-axis limits 
y_limits <- data.frame(
  region = c("Central Asia", "Latin America & Caribbean", "South Asia", "East Asia & Pacific",
             "Middle East & North Africa", "Sub-Saharan Africa", "Europe", "North America"),
  ymin = c(1, 2, 0, 1.5, 2, 0, 3, 4),
  ymax = c(6, 7, 5, 7, 8, 5, 8, 8)
)

# Create the cut_offs data frame
cut_offs <- data.frame(
  region = c(
    rep("Central Asia", 3),
    rep("Latin America & Caribbean", 3),
    rep("South Asia", 3),
    rep("East Asia & Pacific", 3),
    rep("Middle East & North Africa", 3),
    rep("Sub-Saharan Africa", 3),
    rep("Europe", 3),
    rep("North America", 3)
  ),
  income_group = rep(c("High income", "Lower middle income", "Upper middle income"), 8),
cutoff = c(3.16, 0.79, 1.48)
)

# Reorder the cut_offs dataset to match the linetypes specified in scale_linetype_manual
cut_offs <- cut_offs %>%
  mutate(income_group = factor(income_group, levels = c("Lower middle income", "Upper middle income", "High income")))


# Filter out SSP4 and SSP5 from the Summary dataset
Summary_filtered <- Summary %>% 
  filter(year > 2019, !(scenario %in% c("SSP4", "SSP5")))

# Filter out SSP4 and SSP5 from the cut_offs dataset
cut_offs_filtered <- cut_offs %>% 
  filter(
    !(
      (region %in% c("Central Asia", "East Asia & Pacific", "Europe", "North America", "Latin America & Caribbean", "Middle East & North Africa") &
         income_group == "Lower middle income") |
        (region %in% c("Europe", "North America") & income_group == "Upper middle income")
    )
  )

# Create the plot
ggplot() +
  geom_blank(data = y_limits, aes(x = Inf, y = ymin, ymax = ymax)) +
  geom_point(data = Summary_baseline,
             aes(year, api_baseline, color = "Baseline"), shape = 19) +  # Add back the geom_point for the "Baseline" scenario
  geom_ribbon(data = Summary_filtered,
              aes(x = year, y = api_fit, ymin = api_lwr, ymax = api_upr,
                  color = scenario, fill = scenario), alpha = .3, linetype = 0, show.legend = FALSE) +
  geom_line(data = Summary_filtered,
            aes(year, api_fit, color = scenario),
            size = 0.8) +
  geom_hline(data = cut_offs_filtered, 
             aes(yintercept = cutoff, linetype = income_group, color = income_group)) +  # Add back the cutoff lines for income groups
  facet_wrap(~ region, ncol = 3, scales = "free_y") +
  scale_x_continuous(breaks = c(1990, 2000, 2020, 2040, 2060, 2080, 2095),
                     labels = c("1990", "2000", "2020", "2040", "2060", "2080", "2100")) +
  scale_y_continuous(labels = f) +
  scale_color_manual(values = colors[!(names(colors) %in% c("SSP4", "SSP5"))],  # Remove SSP4 and SSP5 from scale_color_manual
                     guide = guide_legend(override.aes = list(title = "Income Group"))) +
  scale_fill_manual(values = colors) +
  labs(x = 'Year', y = 'API', title = '') +
  theme_bw() +
  theme(panel.spacing = unit(0.9, "lines"),
        strip.text = element_text(size = 10),
        strip.background = element_blank(),
        axis.text.x = element_text(angle = 90, size = 10),
        axis.text.y = element_text(size = 10),
        legend.position = 'right',
        legend.text = element_text(size = 11),
        plot.title = element_text(hjust = 0.5, size = 18),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  guides(color = guide_legend(override.aes = list(shape = NA)))
#ggsave("/Users/nicole/Desktop/Mechanization/Figures/Background_Figures/SSP_Projections_API_WBregions_plusIncomegroupLINES_FINAL.png", width = 10, height = 8, dpi = 300)

###################

# exponential 
ggplot() +
  geom_point(data = Summary_baseline %>% filter(year == 2005),
             aes(year, exp_api_baseline, color = scenario)) +
  geom_ribbon(data = Summary %>% filter(year > 2019),
              aes(x = year, y = exp_api_fit, ymin = exp_api_lwr, ymax = exp_api_upr,
                  color = scenario, fill = scenario), alpha = .3, linetype = 0, show.legend = FALSE) +
  geom_line(data = Summary %>% filter(year > 2019) %>% filter(scenario != "SSP1"), aes(year, exp_api_fit, color = scenario),
            size = 0.8) +
  geom_line(data = Summary %>% filter(year > 2019) %>% filter(scenario == "SSP1"), aes(year, exp_api_fit, color = scenario),
            size = 0.8) +
  facet_wrap(~ region, ncol = 3) +
  scale_x_continuous(breaks = c(1990, 2000, 2020, 2040, 2060, 2080, 2095),
                     labels = c("1990", "2000", "2020", "2040", "2060", "2080", "2100")) +
  scale_y_continuous(labels = f) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  labs(x = 'Year', y = 'API', title = '') +
  theme_bw() +
  theme(panel.spacing = unit(0.9, "lines"),
        strip.text = element_text(size = 10),
        strip.background = element_blank(),
        axis.text.x = element_text(angle = 90, size = 10),
        axis.text.y = element_text(size = 10),
        legend.position = 'right',
        legend.text = element_text(size = 11),
        plot.title = element_text(hjust = 0.5, size = 18))
#ggsave("/Users/nicole/Desktop/Mechanization/Figures/Background_Figures/SSP_Projections_exp_API_WBregions.png", width = 8, height = 6, dpi = 300)

# per income group 

income_summary <- Summary %>% 
  dplyr::left_join(income_group, by = "countrycode") %>%
  dplyr::group_by(income_group, scenario, year) %>%
  dplyr::mutate(api_fit = mean(api_proj, na.rm = T),
                api_upr = mean(api_high, na.rm = T), 
                api_lwr = mean(api_low, na.rm = T)) %>%
  dplyr::ungroup() %>%
  drop_na(income_group) %>%
  mutate(year = as.numeric(year))

income_summary_baseline <- Summary_baseline %>%
  dplyr::left_join(income_group, by = "countrycode") %>%
  dplyr::group_by(income_group) %>%
  filter(year %in% 2000:2005) %>%
  dplyr::mutate(api_baseline = mean(api_log, na.rm = T)) %>%
  mutate(year = as.numeric(year)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(scenario = "Baseline") %>%
  filter(!is.na(income_group)) 

# plot 

# Define function for number formatting
f <- function(x) {
  format(round(x, 1), nsmall = 1)
}

ggplot() +
  geom_point(data = income_summary_baseline %>% filter(year > 2004),
             aes(year, api_baseline, color = scenario)) +
  geom_ribbon(data = income_summary %>% filter(year > 2019),
              aes(x = year, y = api_fit, ymin = api_lwr, ymax = api_upr,
                  color = scenario, fill = scenario), alpha = .3, linetype = 0, show.legend = FALSE) +
  geom_line(data = income_summary %>% filter(year > 2019) %>% filter(scenario != "SSP1"), aes(year, api_fit, color = scenario),
            size = 0.8) +
  geom_line(data = income_summary %>% filter(year > 2019) %>% filter(scenario == "SSP1"), aes(year, api_fit, color = scenario),
            size = 0.8) +
  facet_wrap(~ income_group, ncol = 4) +
  scale_x_continuous(breaks = c(1990, 2000, 2020, 2040, 2060, 2080, 2095),
                     labels = c("1990", "2000", "2020", "2040", "2060", "2080", "2100")) +
  scale_y_continuous(labels = f) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  labs(x = 'Year', y = 'log(API)', title = '') +
  theme_bw() +
  theme(panel.spacing = unit(0.9, "lines"),
        strip.text = element_text(size = 10),
        strip.background = element_blank(),
        axis.text.x = element_text(angle = 90, size = 10),
        axis.text.y = element_text(size = 10),
        legend.position = 'right',
        legend.text = element_text(size = 10),
        plot.title = element_text(hjust = 0.5, size = 18))
#ggsave("/Users/nicole/Desktop/Mechanization/Figures/Background_Figures/SSP_Projections_API_IncomeGroup.png", width = 8, height = 6, dpi = 300)

# developed / developing 

developed_summary <- Summary %>% 
  dplyr::left_join(developed_developing, by = "countrycode") %>%
  dplyr::group_by(classification, scenario, year) %>%
  dplyr::mutate(api_fit = mean(api_proj, na.rm = T),
                api_upr = mean(api_high, na.rm = T), 
                api_lwr = mean(api_low, na.rm = T)) %>%
  dplyr::ungroup() %>%
  drop_na(classification) %>%
  mutate(year = as.numeric(year))

developed_summary_baseline <- Summary_baseline %>%
  dplyr::left_join(developed_developing, by = "countrycode") %>%
  dplyr::group_by(classification) %>%
  filter(year %in% 2000:2005) %>%
  dplyr::mutate(api_baseline = mean(api_log, na.rm = T)) %>%
  mutate(year = as.numeric(year)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(scenario = "Baseline") %>%
  filter(!is.na(classification)) 

# plot 

# Define function for number formatting
f <- function(x) {
  format(round(x, 1), nsmall = 1)
}

ggplot() +
  geom_point(data = developed_summary_baseline %>% filter(year > 2004),
             aes(year, api_baseline, color = scenario)) +
  geom_ribbon(data = developed_summary %>% filter(year > 2019),
              aes(x = year, y = api_fit, ymin = api_lwr, ymax = api_upr,
                  color = scenario, fill = scenario), alpha = .3, linetype = 0, show.legend = FALSE) +
  geom_line(data = developed_summary %>% filter(year > 2019) %>% filter(scenario != "SSP1"), aes(year, api_fit, color = scenario),
            size = 0.8) +
  geom_line(data = developed_summary %>% filter(year > 2019) %>% filter(scenario == "SSP1"), aes(year, api_fit, color = scenario),
            size = 0.8) +
  facet_wrap(~ classification, ncol = 4) +
  scale_x_continuous(breaks = c(1990, 2000, 2020, 2040, 2060, 2080, 2095),
                     labels = c("1990", "2000", "2020", "2040", "2060", "2080", "2100")) +
  scale_y_continuous(labels = f) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  labs(x = 'Year', y = 'log(API)', title = '') +
  theme_bw() +
  theme(panel.spacing = unit(0.9, "lines"),
        strip.text = element_text(size = 10),
        strip.background = element_blank(),
        axis.text.x = element_text(angle = 90, size = 10),
        axis.text.y = element_text(size = 10),
        legend.position = 'right',
        legend.text = element_text(size = 10),
        plot.title = element_text(hjust = 0.5, size = 18))
#ggsave("/Users/nicole/Desktop/Mechanization/Figures/Background_Figures/SSP_Projections_API_developed_developing.png", width = 8, height = 6, dpi = 300)

# SSPs 

# Average over regions 

colors <- c("East Asia & Pacific" = "#B06D4C",  
            "South Asia" = "#E5B76E",  
            "Central Asia" = "#8AA7B1",  
            "North America" = "#D38D5F",  
            "Sub-Saharan Africa" = "#6B8F7E",  
            "Europe" = "#46637F",  
            "Middle East & North Africa" = "#993366",  
            "Latin America & Caribbean" = "#FF69B4")  

colors <- c("East Asia & Pacific" = "#B06D4C",  
            "South Asia" = "#E5B76E",  
            "Central Asia" = "#8AA7B1",  
            "North America" = "#D38D5F",  
            "Sub-Saharan Africa" = "#6B8F7E",  
            "Europe" = "#46637F",  
            "Middle East & North Africa" = "#993366",  
            "Latin America & Caribbean" = "#FF69B4")

# a single SSP

ggplot() +
  geom_point(data = Summary_baseline %>% filter(year == 2005), aes(year, api_baseline, color = region)) +
  geom_ribbon(data = Summary %>% filter(scenario == "SSP2"), aes(x = year, y=api_fit, ymin = api_lwr, ymax= api_upr, color = region, fill = region), alpha = .2, linetype = 0) +
  geom_line(data = Summary  %>% filter(scenario == "SSP2"), aes(year, api_fit, color = region), size = 0.8) +
  scale_x_continuous(breaks = c(2000, 2020, 2040, 2060, 2080, 2095),
                     labels = c("2000", "2020", "2040", "2060", "2080", "2100"))+
  theme(panel.spacing = unit(0.9, "lines"), strip.text = element_text(size=13)) +
  theme(panel.background = element_blank()) +
  labs(x = 'Year', y = 'log(API)', title = '') +
  theme(axis.text.x = element_text(angle = 90, size = 13), axis.text.y = element_text(size = 13) ) +
  theme(legend.position= 'right', text = element_text(size=13), plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors)
#ggsave("/Users/nicole/Desktop/Mechanization/Figures/Background_Figures/SSP_Projections_API_SSP2_only.png", width = 8, height = 6, dpi = 300)

# ALL SSPs 

ggplot() +
  geom_ribbon(data = Summary %>% filter(scenario != "Baseline"), aes(x = year, y = api_fit, ymin = api_lwr, ymax = api_upr, color = region, fill = region), alpha = .2, linetype = 0) +
  geom_line(data = Summary %>% filter(scenario != "Baseline"), aes(year, api_fit, color = region), size = 0.8) +
  scale_x_continuous(breaks = c(2020, 2040, 2060, 2080, 2095), labels = c("2020", "2040", "2060", "2080", "2100")) +
  theme(panel.spacing = unit(0.9, "lines"), strip.text = element_text(size = 13)) +
  theme(panel.background = element_blank()) +
  labs(x = 'Year', y = 'log(API)', title = '') +
  theme(axis.text.x = element_text(angle = 90, size = 13), axis.text.y = element_text(size = 13)) +
  theme(legend.position = 'right', text = element_text(size = 13), plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  facet_wrap(~ scenario, ncol = 3)
#ggsave("/Users/nicole/Desktop/Mechanization/Figures/Background_Figures/SSP_Projections_API_ALL_SSPs.png", width = 8, height = 6, dpi = 300)

# per developed/developing country 

colors <- c("Developed" = "#FF69B4",  
            "Developing" = "#6B8F7E")

ggplot() +
  geom_ribbon(data = developed_summary %>% filter(scenario != "Baseline"), aes(x = year, y = api_fit, ymin = api_lwr, ymax = api_upr, color = classification, fill = classification), alpha = .2, linetype = 0) +
  geom_line(data = developed_summary %>% filter(scenario != "Baseline"), aes(year, api_fit, color = classification), size = 0.8) +
  scale_x_continuous(breaks = c(2020, 2040, 2060, 2080, 2095), labels = c("2020", "2040", "2060", "2080", "2100")) +
  theme(panel.spacing = unit(0.9, "lines"), strip.text = element_text(size = 13)) +
  theme(panel.background = element_blank()) +
  labs(x = 'Year', y = 'log(API)', title = '') +
  theme(axis.text.x = element_text(angle = 90, size = 13), axis.text.y = element_text(size = 13)) +
  theme(legend.position = 'right', text = element_text(size = 13), plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  facet_wrap(~scenario, ncol = 2)
#ggsave("/Users/nicole/Desktop/Mechanization/Figures/Background_Figures/SSP_Projections_API_ALL_SSPs_developed_developing.png", width = 8, height = 6, dpi = 300)

# per income group

colors <- c("High income" = "#B06D4C",
            "Upper middle income" = "#E5B76E",
            "Lower middle income" = "#8AA7B1",
            "Low income" = "#D38D5F")

ggplot() +
  geom_ribbon(data = income_summary %>% filter(scenario != "Baseline"), aes(x = year, y = api_fit, ymin = api_lwr, ymax = api_upr, color = income_group, fill = income_group), alpha = .2, linetype = 0) +
  geom_line(data = income_summary %>% filter(scenario != "Baseline"), aes(year, api_fit, color = income_group), size = 0.8) +
  scale_x_continuous(breaks = c(2020, 2040, 2060, 2080, 2095), labels = c("2020", "2040", "2060", "2080", "2100")) +
  theme(panel.spacing = unit(0.9, "lines"), strip.text = element_text(size = 13)) +
  theme(panel.background = element_blank()) +
  labs(x = 'Year', y = 'log(API)', title = '') +
  theme(axis.text.x = element_text(angle = 90, size = 13), axis.text.y = element_text(size = 13)) +
  theme(legend.position = 'right', text = element_text(size = 13), plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  facet_wrap(~scenario, ncol = 2)
#ggsave("/Users/nicole/Desktop/Mechanization/Figures/Background_Figures/SSP_Projections_API_ALL_SSPs_income_group.png", width = 8, height = 6, dpi = 300)

###












###

# Additional data analysis for tables (main results)

regions_global <- read_xlsx("/Users/nicole/Desktop/Mechanization/Data/Regions/Regions_WB_global.xlsx")

api_global <- api_df %>%
  filter(year %in% 2000:2010) %>%
  dplyr::left_join(regions_global, by = "countrycode", multiple = "all") %>%
  dplyr::group_by(region) %>% 
  dplyr::mutate(average_2005 = mean(api_log, na.rm = TRUE)) %>%
  filter(year == "2005") %>%
  ungroup() 

print(api_global)
print(api_proj)

api_proj <- api_proj %>%
  dplyr::left_join(regions_global, by = "countrycode", multiple = "all") 

api_proj$source <- "api_proj"
api_proj$year <- as.character(api_proj$year)  # Convert to character
api_global$year <- as.character(api_global$year)  # Convert to character
api_global$source <- "api_gloabl"

combined_data <- bind_rows(api_proj, api_global) %>%
  dplyr::select(scenario, countrycode, region, year, api_proj, api_low, api_high, source, average_2005) %>%
  filter(year %in% c(2005, 2035, 2050, 2075, 2100))
print(combined_data)

# Calculate mean api_proj values for 2050 and 2100 for each countrycode
mean_values <- combined_data %>%
  group_by(region, scenario) %>%
  summarize(mean_proj_2035 = mean(api_proj[year == 2035], na.rm = TRUE),
            mean_proj_2050 = mean(api_proj[year == 2050], na.rm = TRUE),
            mean_proj_2075 = mean(api_proj[year == 2075], na.rm = TRUE),
            mean_proj_2100 = mean(api_proj[year == 2100], na.rm = TRUE))

# Merge the mean values with the average_2005 values in 2005
merged_data <- mean_values %>%
  dplyr::left_join(combined_data %>%
                     filter(year == 2005) %>%
                     dplyr::select(region, average_2005), by = "region", multiple = "all")

# Calculate the percentage difference for each region
merged_data <- merged_data %>%
  mutate(diff_2035 = (mean_proj_2035 - average_2005) / average_2005 * 100,
         diff_2050 = (mean_proj_2050 - average_2005) / average_2005 * 100,
         diff_2075 = (mean_proj_2075 - average_2005) / average_2005 * 100,
         diff_2100 = (mean_proj_2100 - average_2005) / average_2005 * 100) %>%
  #dplyr::rename(var = mean_proj_2100) %>%
  dplyr::distinct()
col_names <- colnames(merged_data)
print(col_names)
#write.table(merged_data, "/Users/nicole/Desktop/Mechanization/Data/Tables_results/Summary_api_Table_6.csv", sep = ",", row.names = FALSE)

filtered_data <- merged_data %>%
  filter(scenario %in% c("SSP1", "SSP2", "SSP3")) %>%
  dplyr::select(average_2005, diff_2035, diff_2050, diff_2075, diff_2100, region, scenario) %>%
  dplyr::mutate(
    average_2005 = round(average_2005, 2),
    diff_2035 = round(diff_2035, 2),
    diff_2050 = round(diff_2050, 2),
    diff_2075 = round(diff_2075, 2),
    diff_2100 = round(diff_2100, 2)
  )

# factor 

combined_data <- bind_rows(api_proj, api_global) %>%
  dplyr::select(scenario, countrycode, region, year, api_proj, api_low, api_high, source, average_2005) %>%
  filter(year %in% c(2005, 2035, 2050, 2075, 2100))

# Calculate mean api_proj values for 2050 and 2100 for each countrycode
mean_values <- combined_data %>%
  group_by(region, scenario) %>%
  summarize(mean_proj_2035 = mean(api_proj[year == 2035], na.rm = TRUE),
            mean_proj_2050 = mean(api_proj[year == 2050], na.rm = TRUE),
            mean_proj_2075 = mean(api_proj[year == 2075], na.rm = TRUE),
            mean_proj_2100 = mean(api_proj[year == 2100], na.rm = TRUE))

# Merge the mean values with the average_2005 values in 2005
merged_data <- mean_values %>%
  dplyr::left_join(combined_data %>%
                     filter(year == 2005) %>%
                     dplyr::select(region, average_2005), by = "region", multiple = "all")

# Calculate the factor difference for each region
merged_data <- merged_data %>%
  mutate(factor_diff_2035 = mean_proj_2035 / average_2005,
         factor_diff_2050 = mean_proj_2050 / average_2005,
         factor_diff_2075 = mean_proj_2075 / average_2005,
         factor_diff_2100 = mean_proj_2100 / average_2005) %>%
  dplyr::distinct()

col_names <- colnames(merged_data)
print(col_names)
#write.table(merged_data, "/Users/nicole/Desktop/Mechanization/Data/Tables_results/Summary_api_Table_6.csv", sep = ",", row.names = FALSE)

filtered_data <- merged_data %>%
  filter(scenario %in% c("SSP1", "SSP2", "SSP3")) %>%
  dplyr::select(average_2005, factor_diff_2035, factor_diff_2050, factor_diff_2075, factor_diff_2100, region, scenario) %>%
  dplyr::mutate(
    average_2005 = round(average_2005, 2),
    factor_diff_2035 = round(factor_diff_2035, 2),
    factor_diff_2050 = round(factor_diff_2050, 2),
    factor_diff_2075 = round(factor_diff_2075, 2),
    factor_diff_2100 = round(factor_diff_2100, 2)
  )




# testing out of sample performance 

# Convert 'year' column to numeric for sampling
master$year <- as.numeric(as.character(master$year))

# Set the number of folds for cross-validation
num_folds <- 5

# Step 1: Split the data into training and testing subsets
set.seed(123)  # For reproducibility
years <- unique(master$year)
num_chunks <- length(years) - num_folds + 1
random_chunks <- lapply(seq(num_chunks), function(i) sample(years, num_folds) + i - 1)

train_data_chunks <- lapply(random_chunks, function(chunk) master %>% filter(year < chunk[1] | year > chunk[num_folds]))
test_data_chunks <- lapply(random_chunks, function(chunk) master %>% filter(year %in% chunk))

# Convert 'year' column back to character (if needed)
master$year <- as.character(master$year)

# Step 2: Perform K-fold cross-validation
rmse_train_cv <- numeric(num_chunks)
rmse_test_cv <- numeric(num_chunks)

for (i in 1:num_chunks) {
  train_data <- train_data_chunks[[i]]
  test_data <- test_data_chunks[[i]]
  
  # Print information about the datasets for each iteration
  cat("Iteration ", i, "\n")
  cat("Training data rows: ", nrow(train_data), "\n")
  cat("Testing data rows: ", nrow(test_data), "\n")
  
  # Check if either the training or testing data is empty
  if (nrow(train_data) == 0 || nrow(test_data) == 0) {
    cat("Skipping iteration ", i, " due to empty training or testing data.\n")
    next  # Skip this iteration if no data is available
  }
  
  # Check if the number of rows in training and testing data meets a minimum threshold
  min_rows_threshold <- 50  # Set a minimum number of rows required for valid fitting
  if (nrow(train_data) < min_rows_threshold || nrow(test_data) < min_rows_threshold) {
    cat("Skipping iteration ", i, " due to insufficient data for fitting.\n")
    next  # Skip this iteration if data is insufficient for model fitting
  }
  
  # Fit the model using the training data
  plm_api <- plm(api_log ~ log(gdppc) + log(I(urbanization^2)) + log1p(I(education_lag^2)),
                 data = train_data, effect = "individual", model = "within", index = "countrycode")
  
  # Check if the model is empty (no convergence)
  if (length(coef(plm_api)) == 0 || all(is.na(coef(plm_api)))) {
    cat("Skipping iteration ", i, " due to an empty model or convergence issues.\n")
    rmse_train_cv[i] <- NA
    rmse_test_cv[i] <- NA
    next
  }
  
  # Evaluate the model's performance on the training data
  response_train <- train_data$api_log
  predictors_train <- train_data$gdppc
  urbanization_train <- train_data$urbanization
  education_lag_train <- train_data$education_lag
  predictions_train <- coef(plm_api)["log(gdppc)"] * log(predictors_train) +
    coef(plm_api)["log(I(urbanization^2))"] * log(urbanization_train^2) +
    coef(plm_api)["log1p(I(education_lag^2))"] * log1p(education_lag_train^2)
  
  valid_indices_train <- !is.na(response_train) & !is.na(predictions_train)
  response_train <- response_train[valid_indices_train]
  predictions_train <- predictions_train[valid_indices_train]
  rmse_train_cv[i] <- sqrt(mean((response_train - predictions_train)^2))
  
  # Evaluate the model's performance on the testing data
  response_test <- test_data$api_log
  predictors_test <- test_data$gdppc
  urbanization_test <- test_data$urbanization
  education_lag_test <- test_data$education_lag
  predictions_test <- coef(plm_api)["log(gdppc)"] * log(predictors_test) +
    coef(plm_api)["log(I(urbanization^2))"] * log(urbanization_test^2) +
    coef(plm_api)["log1p(I(education_lag^2))"] * log1p(education_lag_test^2)
  
  valid_indices_test <- !is.na(response_test) & !is.na(predictions_test)
  response_test <- response_test[valid_indices_test]
  predictions_test <- predictions_test[valid_indices_test]
  rmse_test_cv[i] <- sqrt(mean((response_test - predictions_test)^2))
}

# Calculate the average RMSE across all folds
mean_rmse_train_cv <- mean(rmse_train_cv, na.rm = TRUE)
mean_rmse_test_cv <- mean(rmse_test_cv, na.rm = TRUE)

# Step 3: Compare the average RMSE values
cat("Training RMSE (Average CV):", mean_rmse_train_cv, "\n")
cat("Testing RMSE (Average CV):", mean_rmse_test_cv, "\n")

# Step 4: Compare the RMSE values between training and testing data
if (mean_rmse_train_cv < mean_rmse_test_cv) {
  cat("The model performs better on the training data than on the test data.\n")
} else if (mean_rmse_train_cv > mean_rmse_test_cv) {
  cat("The model performs better on the test data than on the training data.\n")
} else {
  cat("The model performs equally on the training and test data.\n")
}
