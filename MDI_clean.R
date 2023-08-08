# Mechanization Deployment Index (MDI)

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
library(ggplot2)
library(dplyr)
library(forcats)
library(RColorBrewer)

# Load full dataset (Agricultural machinery per 100km^2)

Mechanisation_full_df <- read_xls("/Users/nicole/Desktop/Mechanization/Data/Mechanisation/Agricultural_machinery_tractors_per100sqkm.xls") 

mdi_df <- Mechanisation_full_df %>%
  gather(key = "year", value = "tractors", 6:50) %>%
  dplyr::select("Country Code","year", "tractors") %>%
  dplyr::rename(countrycode = 'Country Code') %>%
  #filter(year %in% 1996:2005) %>%
  mutate(mdi = tractors) %>%
  mutate(mdi_log = log(tractors)) %>%
  mutate(year = as.character(year))

# Plot historical data 

#mdi_regions <- mdi_df %>%
  #dplyr::left_join(regions, by = join_by(countrycode)) %>% 
  #group_by(region, year) %>%
  #dplyr::mutate(mdi_baseline = mean(mdi, na.rm = T)) %>%
  #dplyr::mutate(mdi_log_baseline = mean(mdi_log, na.rm = T)) %>%
  #mutate(year = as.numeric(year)) %>%
  #dplyr::ungroup() 
#summary(mdi_regions)  

# Create the plot
#ggplot(mdi_regions, aes(x = year, y = mdi_log_baseline, color = region)) +
  #geom_line() +
  #labs(x = "Year", y = "mdi_log_baseline", color = "Region") +
  #theme_minimal()

########################################################
############### Plot the index  ########################
########################################################

# average data to look at the map (1996-2005) 

dat <- mdi_df %>%
  dplyr::select(countrycode, year, mdi_log) %>%
  filter(year %in% 2000:2010) %>%
  dplyr::group_by(countrycode) %>% 
  dplyr::mutate(average = mean(mdi_log, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(year == "2005") %>%
  dplyr::rename(var = average) 

## Join data to map 

# Define colors for the second map
colors2 <- c("Category 1" = "#FFD700",  # gold
             "Category 2" = "#FFA500",  # orange
             "Category 3" = "#A63603",  # dark orange
             "Category 4" = "#7F4F2A")  # peru


# Create a function that generates a continuous colorscale for the second map
#color_scale2 <- colorRampPalette(colors2)
# Generate a range of colors using the color_scale2 function
#color_range2 <- color_scale2(n_colors) 

map <- joinCountryData2Map(dat, joinCode = "ISO3", nameJoinColumn = "countrycode")

map_poly <-  fortify(map) %>% 
  merge(map@data, by.x="id", by.y="ADMIN", all.x=T) %>%
  arrange(id, order) %>% 
  mutate(var %>% as.numeric())

# Plot map 

ggplot(map_poly, aes(x = long, y = lat, group = group)) +
  coord_map(projection = 'rectangular',parameters = 0, orientation=NULL, xlim = c(-180, 180), ylim = c(-55, 75)) + 
  #coord_map(projection = 'mollweide', xlim = c(-180, 180), ylim = c(-60, 75))  + # Remove antarctica
  geom_polygon(aes(fill = var)) +
  #scale_fill_viridis_c(option="plasma", limits = c(-15, 0)) +
  #scale_fill_distiller(palette = "Spectral") +
  scale_fill_gradientn(colors = colors2, na.value = "#C1CDCD") + # Set the colorscale
  labs(fill = 'log(MDI)'
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
#ggsave("/Users/nicole/Desktop/Mechanization/Figures/Background_Figures/new_MDI_newcolors.png", width = 8, height = 6, dpi = 300)

mean_mdi_log <- mean(dat$mdi_log, na.rm = TRUE)
sd_mdi_log <- sd(dat$mdi_log, na.rm = TRUE)
highest_mdi_log_country <- dat$countrycode[which.max(dat$mdi_log)]
lowest_mdi_log_country <- dat$countrycode[which.min(dat$mdi_log)]
threshold_log <- mean_mdi_log # Set your desired log-transformed mdi threshold
above_threshold_log <- sum(dat$mdi_log > threshold_log, na.rm = TRUE)

print(mean_mdi_log)
print(sd_mdi_log)
print(highest_mdi_log_country)
print(lowest_mdi_log_country)
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

# Remove missing values
mdi_log_clean <- mdi_df$mdi_log[!is.na(mdi_df$mdi_log)]
# Generate the histogram with cleaned data
hist(mdi_log_clean, 
     prob = TRUE,
     main = "Histogram", 
     xlab = "MDI", 
     border = "blue", 
     col = "lightblue", 
     breaks = 20)
# Generate the density line
x <- seq(min(mdi_log_clean), max(mdi_log_clean), length = 88)
f <- dnorm(x, mean = mean(mdi_log_clean), sd = sd(mdi_log_clean))
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
  dplyr::rename(countrycode = "country.code")

# Create master 

master <- mdi_df %>%
  dplyr::left_join(gdppc_hist, countrycode, by = c('countrycode', 'year')) %>% 
  dplyr::left_join(regions, countrycode, by=c('countrycode')) %>%
  mutate(scenario = "Baseline") 

# List of ISO-3 country codes for the countries to exclude
#exclude_countries <- c("ALB", "ARM", "BLR", "BIH", "GEO", "XKX", "MKD", "MDA", "MNE", "RUS", "SRB", "UKR",
                       #"BGR", "HRV", "CZE", "EST", "HUN", "LVA", "LTU", "POL", "ROU", "SVK", "SVN",
                       #"KAZ", "KGZ", "TJK", "TKM", "UZB",
                       #"KHM", "CHN", "LAO", "VNM",
                       #"AFR", "BWA")

# Filter out the specified countries from the 'master' data frame
#master <- master %>%
  #filter(!(countrycode %in% exclude_countries))

plot(mdi_log ~ log(gdppc), pch = 19, col = "black", data = master)
abline(plm_mdi, col = "red")

# Full socioeconomic indicators 

plm_mdi <- plm(mdi_log ~ log(gdppc), data = master, effect = c("individual"), model = c("between"), index = c("countrycode"))

# Model selected

plm_mdi <- plm(mdi_log ~ log(gdppc), data = master, effect = c("individual"), model = c("between"), index = c("countrycode"))
#plm_mdi <- plm(mdi_log ~ log(gdppc), data = master, effect = "time", model = "within", index = "countrycode") # including time-fixed effects
#plm_mdi <- plm(mdi_log ~ log(gdppc), data = master, model = "pooling") # no country-fixed effects 
#plm_mdi <- plm(mdi_log ~ log(gdppc), data = master, effect = "individual", model = "pooling") # random effects
summary(plm_mdi) 

# Perform White's test for heteroskedasticity
white_test <- bptest(plm_mdi, studentize = FALSE)
print(white_test) # heteroskedasticiy is present 

# Check country-specific heteroskedasticity 
# Install and load the required packages
library(dplyr)
library(tidyr)
library(broom)

# Filter out countrycodes with insufficient data
filtered_master <- master %>%
  group_by(countrycode) %>%
  filter(!all(is.na(mdi_log) | is.na(log(gdppc))))

# Function to perform Breusch-Pagan test and return p-value
bptest_func <- function(data) {
  htest_result <- bptest(mdi_log ~ log(gdppc), data = data)
  tidy(htest_result)$p.value
}

# Function to return the adjusted p-value using the Benjamini-Hochberg method
adjusted_pvalue <- function(p_values) {
  p.adjust(p_values, method = "BH")
}

# Perform the Breusch-Pagan test for each country
breusch_pagan_results <- filtered_master %>%
  group_by(countrycode) %>%
  do(p.value = bptest_func(.)) %>%
  ungroup()

# Create a new data frame to store the results vertically
breusch_pagan_results_tbl <- data.frame(
  CountryCode = breusch_pagan_results$countrycode,
  Original_P_Value = unlist(breusch_pagan_results$p.value)
)


#####
# Quantify standard errors 
coefficients <- coef(plm_mdi)
residuals <- resid(plm_mdi)
num_countries <- length(unique(master$countrycode))
X <- model.matrix(plm_mdi)
crossproduct <- t(X) %*% X
residual_ss <- sum(residuals^2)
robust_cov <- residual_ss / (num_countries - 2) * solve(crossproduct)
se <- sqrt(diag(robust_cov))
coefficients <- coef(plm_mdi)
coefficients_with_se <- data.frame(coefficients, se)
print(coefficients_with_se)

# Hausmann test 

plm_between <- plm(mdi ~ log(gdppc), data = master, effect = c("individual"), model = c("between"), index = c("countrycode"))
#plm_within <- plm(mdi ~ log(gdppc), data = master, effect = c("individual"), model = c("within"), index = c("countrycode"))
#phtest(plm_between, plm_within) -> Hausmann_test
#print(Hausmann_test)

# Coefficients 

gdppc_fit = 0.956268 # between 
#gdppc_fit = 0.130489 # within
#gdppc_fit = 0.141331 # excluded countries 
#gdppc_fit = 0.902115 # no country-fixed effects 
#gdppc_fit = 0.906925 # include time-fixed effects 
#gdppc_fit = 0.902115 # random effects 

# Coefficients 
gdppc_fit <- 0.956268 # between
# Get confidence intervals (normal)  
tidy(plm_mdi, conf.int = TRUE, conf.level = 0.95)
# Calculate critical value for 95% confidence level
critical_value <- qnorm(0.975) # For two-sided test
# Calculate confidence intervals for the coefficients
gdppc_low <- gdppc_fit - critical_value * 0.06368845
gdppc_high <- gdppc_fit + critical_value * 0.06368845

#gdppc_low = 0.803 # between 
#gdppc_low = 0.0874 # within
#gdppc_low = 0.0863 # excluded countries 
#gdppc_low = 0.859 # no country-fixed effects 
#gdppc_low = 0.863 # include time-fixed effects 
#gdppc_low = 0.859 # random effects 

#gdppc_high = 1.11 # between 
#gdppc_high = 0.174 # within 
#gdppc_high = 0.196 #excluded countries 
#gdppc_high = 0.945 # no country-fixed effects 
#gdppc_high = 0.951 # include time-fixed effects 
#gdppc_high = 0.945 # random 

# safe country-fixed effects 

#fix_ef <- data.frame(countrycode = names(fixef(plm_mdi)),
                     #fixef = as.vector(fixef(plm_mdi)))

########################################################
############# Adjust intercept manually ################
########################################################

# Select MDI and socioeconomic variables for last year of observations (2019) 

mdi_int <- master %>%
  filter(year == "2005") %>% # Change the year as per your requirement
  dplyr::select(mdi_log, gdppc, countrycode) %>%
  left_join(fix_ef, countrycode, by = "countrycode")

# Get new intercept (Anton's formula)
intercept <- mdi_int %>%
  mutate(fixef_adj = mdi_log - (gdppc_fit * log(gdppc))) %>%
  mutate(fixef_adj_low = mdi_log - (gdppc_low * log(gdppc))) %>%
  mutate(fixef_adj_high = mdi_log - (gdppc_high * log(gdppc))) %>%
  dplyr::select(countrycode, fixef_adj, fixef_adj_low, fixef_adj_high)

# Load socioeconomic variables 
socio_proj <- read.csv('/Users/nicole/Desktop/Mechanization/Data/Socioeconomic_variables/Projections/Pop_Edu_Gov_Gii_Urb_obs_proj.csv') %>%
  dplyr::select(countrycode, year, scenario, gdppc) %>%
  filter(year %in% seq(2020, 2100, 5)) %>%
  dplyr::left_join(intercept, countrycode, by = "countrycode")

# Apply projections 

mdi_proj <- socio_proj %>%
  mutate(mdi_proj = fixef_adj + gdppc_fit*log(gdppc)) %>%
  mutate(mdi_low = fixef_adj_low + gdppc_low*log(gdppc)) %>%
  mutate(mdi_high = fixef_adj_high + gdppc_high*log(gdppc))

# Apply projections without fixed effects 

#mdi_proj <- socio_proj %>%
  #mutate(mdi_proj = gdppc_fit*log(gdppc)) %>%
  #mutate(mdi_low = gdppc_low*log(gdppc)) %>%
  #mutate(mdi_high = gdppc_high*log(gdppc))

#mdi_proj <- mdi_proj %>%
  #dplyr::mutate(across(c(mdi_proj, mdi_low, mdi_high), exp))

# Average over regions 

Summary <- mdi_proj %>%
  left_join(regions, countrycode, by = "countrycode") %>%
  dplyr::group_by(region, scenario, year) %>%
  dplyr::mutate(mdi_fit = mean(mdi_proj, na.rm = T),
                mdi_upr = mean(mdi_high, na.rm = T), 
                mdi_lwr = mean(mdi_low, na.rm = T),
                exp_mdi_fit = mean(exp(mdi_proj), na.rm = T),
                exp_mdi_upr = mean(exp(mdi_high), na.rm = T), 
                exp_mdi_lwr = mean(exp(mdi_low), na.rm = T)) %>%
  dplyr::ungroup() %>%
  drop_na(region) %>%
  mutate(year = as.numeric(year))
#write_xlsx(Summary, "/Users/nicole/Desktop/MDI_projections_SSPs_per_region_for_table2.xlsx")

# Baseline 

Summary_baseline <- mdi_df %>%
  dplyr::left_join(regions, countrycode, by = c('countrycode')) %>%
  filter(year %in% 2000:2010) %>%
  dplyr::group_by(countrycode) %>% 
  dplyr::mutate(average = mean(mdi_log, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(year == 2005) %>%
  dplyr::group_by(region) %>%
  dplyr::mutate(mdi_baseline = mean(mdi_log, na.rm = TRUE)) %>%  # Take the mean within each region
  dplyr::mutate(exp_mdi_baseline = mean(exp(mdi_log), na.rm = TRUE)) %>%
  mutate(year = as.numeric(year)) %>%
  dplyr::mutate(scenario = "Baseline") %>%
  dplyr::ungroup() %>%
  filter(!is.na(region)) %>%
  dplyr::group_by(countrycode) %>% 
  dplyr::mutate(mdi_baseline = first(mdi_baseline)) %>%  # Take the first value within each country
  dplyr::ungroup()

# Income groups 

income_baseline <- mdi_df %>%
  dplyr::left_join(income_group, countrycode, by =c('countrycode')) %>%
  filter(year %in% 2000:2010) %>%
  dplyr::group_by(income_group) %>% 
  dplyr::mutate(average = mean(mdi_log, na.rm = TRUE)) %>%
  ungroup() %>%
  dplyr::group_by(income_group) %>%
  dplyr::mutate(mdi_baseline = mean(mdi_log, na.rm = T)) %>%
  mutate(year = as.numeric(year)) %>%
  dplyr::ungroup() 
summary(income_baseline)

cut_offs <- income_baseline %>%
  group_by(income_group) %>%
  summarize(max_mdi_log = max(mdi_baseline),
            min_mdi_log = min(mdi_baseline)) %>%
  mutate(income_group = fct_rev(income_group)) %>%
  mutate(ymin = min_mdi_log, ymax = max_mdi_log) %>%
  select(income_group, ymin, ymax) %>%
  filter(!is.na(income_group))
print(cut_offs)

# Plot 

#ggplot() +
  #geom_point(data = Summary_baseline %>% filter(year == 2005),
             #aes(year, mdi_baseline, color = scenario)) +
  #geom_ribbon(data = Summary %>% filter(year > 2019),
              #aes(x = year, y = mdi_fit, ymin = mdi_lwr, ymax = mdi_upr,
                  #color = scenario, fill = scenario), alpha = .3, linetype = 0, show.legend = FALSE) +
  #geom_line(data = Summary %>% filter(year > 2019) %>% filter(scenario != "SSP1"), aes(year, mdi_fit, color = scenario),
            #size = 0.8) +
  #geom_line(data = Summary %>% filter(year > 2019) %>% filter(scenario == "SSP1"), aes(year, mdi_fit, color = scenario),
            #size = 0.8) +
  #facet_wrap(~ region, ncol = 3, scales = "free") +
  #scale_x_continuous(breaks = c(1990, 2000, 2020, 2040, 2060, 2080, 2095),
                     #labels = c("1990", "2000", "2020", "2040", "2060", "2080", "2100")) +
  #scale_y_continuous(labels = f) +
  #scale_color_manual(values = colors) +
  #scale_fill_manual(values = colors) +
  #labs(x = 'Year', y = 'log(MDI)', title = '') +
  #theme_bw() +
  #theme(panel.spacing = unit(0.9, "lines"),
        #strip.text = element_text(size = 10),
        #strip.background = element_blank(),
        #axis.text.x = element_text(angle = 90, size = 10),
        #axis.text.y = element_text(size = 10),
        #legend.position = 'right',
        #legend.text = element_text(size = 11),
        #plot.title = element_text(hjust = 0.5, size = 18))
#ggsave("/Users/nicole/Desktop/Mechanization/Figures/Background_Figures/SSP_Projections_mdi_WBregions.png", width = 8, height = 6, dpi = 300)

library(purrr)
library(ggplot2)
library(dplyr)

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
  ymin = c(5, 5, 2, 4.5, 4.5, 2, 6, 5),
  ymax = c(9, 8, 8, 8.5, 8, 6, 8.5, 6.5)
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
  income_group = rep(c("Lower middle income", "Upper middle income", "High income"), 8),
  cutoff = c(3.57, 5.08, 6.37)
)

# Reorder the cut_offs dataset to match the linetypes specified in scale_linetype_manual
cut_offs <- cut_offs %>%
  mutate(income_group = factor(income_group, levels = c("Lower middle income", "Upper middle income", "High income")))

# ... (previous code)

# Filter out SSP4 and SSP5 from the Summary dataset
Summary_filtered <- Summary %>% 
  filter(year > 2019, !(scenario %in% c("SSP4", "SSP5")))

# Filter out SSP4 and SSP5 from the cut_offs dataset
cut_offs_filtered <- cut_offs %>% 
  filter(!(region %in% c("Central Asia", "East Asia & Pacific", "North America", "Latin America & Caribbean", "Middle East & North Africa") & income_group == "Lower middle income"))

# Create the plot
ggplot() +
  geom_blank(data = y_limits, aes(x = Inf, y = ymin, ymax = ymax)) +
  geom_point(data = Summary_baseline,
             aes(year, mdi_baseline, color = "Baseline"), shape = 19) +  # Add back the geom_point for the "Baseline" scenario
  geom_ribbon(data = Summary_filtered,
              aes(x = year, y = mdi_fit, ymin = mdi_lwr, ymax = mdi_upr,
                  color = scenario, fill = scenario), alpha = .3, linetype = 0, show.legend = FALSE) +
  geom_line(data = Summary_filtered,
            aes(year, mdi_fit, color = scenario),
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
  labs(x = 'Year', y = 'MDI', title = '') +
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
#ggsave("/Users/nicole/Desktop/Mechanization/Figures/Background_Figures/SSP_Projections_MDI_WBregions_plusIncomegroupLINES_axes_FREE.png", width = 10, height = 8, dpi = 300)

# old 

#colors <- c("SSP1" = "#6B8F7E",  # deep sky blue
            #"Baseline" = "#E5B76E",  # sandy brown
            #"SSP2" = "#D38D5F",  # pale turquoise
            #"SSP3" = "#8AA7B1",  # dark orange
            #"SSP4" = "#B06D4C",  # olive green
            #"SSP5" = "#46637F")  # sienna

# Reorder the cut_offs dataset to match the linetypes specified in scale_linetype_manual
#cut_offs <- cut_offs %>%
  #mutate(income_group = factor(income_group, levels = c("Lower middle income", "Upper middle income", "High income")))

# Create the plot
#ggplot() +
  #geom_blank(data = y_limits, aes(x = Inf, y = ymin, ymax = ymax)) +
  #geom_point(data = Summary_baseline %>% filter(year == 2005),
             #aes(year, mdi_baseline, color = "Baseline"), shape = 19) +
  #geom_ribbon(data = Summary %>% filter(year > 2019),
              #aes(x = year, y = mdi_fit, ymin = mdi_lwr, ymax = mdi_upr,
                  #color = scenario, fill = scenario), alpha = .3, linetype = 0, show.legend = FALSE) +
  #geom_line(data = Summary %>% filter(year > 2019), aes(year, mdi_fit, color = scenario),
            #size = 0.8) +
  #geom_hline(data = cut_offs %>% 
               #filter(!(region == "Europe" & income_group != "High income") &
                        #!(region %in% c("Central Asia", "East Asia & Pacific", "North America", "Latin America & Caribbean", "Middle East & North Africa") & income_group == "Lower middle income")),
             #aes(yintercept = cutoff, linetype = income_group, color = income_group)) +  # Modified to only show "Upper middle income" and "High income" 
  #facet_wrap(~ region, ncol = 3, scales = "free_y") +  # Set scales = "free_y" to respect y-axis limits for each region
  #scale_x_continuous(breaks = c(1990, 2000, 2020, 2040, 2060, 2080, 2095),
                     #labels = c("1990", "2000", "2020", "2040", "2060", "2080", "2100")) +
  #scale_y_continuous(labels = f) +
  #scale_color_manual(values = colors,
                     #guide = guide_legend(override.aes = list(shape = c(19, NA, NA, NA, NA, NA),
                                                              #linetype = c(0, 1, 1, 1, 1, 1),
                                                              #title = "Income Group"))) +
  #scale_fill_manual(values = colors) +
  #labs(x = 'Year', y = 'MDI', title = '') +
  #theme_bw() +
  #theme(panel.spacing = unit(0.9, "lines"),
        #strip.text = element_text(size = 10),
        #strip.background = element_blank(),
        #axis.text.x = element_text(angle = 90, size = 10),
        #axis.text.y = element_text(size = 10),
        #legend.position = 'right',
        #legend.text = element_text(size = 11),
        #plot.title = element_text(hjust = 0.5, size = 18),
        #panel.grid.major = element_blank(),
        #panel.grid.minor = element_blank()) +
  #guides(linetype = guide_legend(override.aes = list(shape = NA)))
#ggsave("/Users/nicole/Desktop/Mechanization/Figures/Background_Figures/LEGEND_MDI.png", width = 10, height = 8, dpi = 300)
#print(mdi_df)


#####

#ggsave("/Users/nicole/Desktop/Mechanization/Figures/Background_Figures/SSP_Projections_MDI_WBregions_plusIncomegrouptext.png", width = 10, height = 8, dpi = 300)

# exponential 
ggplot() +
  geom_point(data = Summary_baseline %>% filter(year == 2005),
             aes(year, exp_mdi_baseline, color = scenario)) +
  geom_ribbon(data = Summary %>% filter(year > 2019),
              aes(x = year, y = exp_mdi_fit, ymin = exp_mdi_lwr, ymax = exp_mdi_upr,
                  color = scenario, fill = scenario), alpha = .3, linetype = 0, show.legend = FALSE) +
  geom_line(data = Summary %>% filter(year > 2019) %>% filter(scenario != "SSP1"), aes(year, exp_mdi_fit, color = scenario),
            size = 0.8) +
  geom_line(data = Summary %>% filter(year > 2019) %>% filter(scenario == "SSP1"), aes(year, exp_mdi_fit, color = scenario),
            size = 0.8) +
  facet_wrap(~ region, ncol = 3) +
  scale_x_continuous(breaks = c(1990, 2000, 2020, 2040, 2060, 2080, 2095),
                     labels = c("1990", "2000", "2020", "2040", "2060", "2080", "2100")) +
  scale_y_continuous(labels = f) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  labs(x = 'Year', y = 'mdi', title = '') +
  theme_bw() +
  theme(panel.spacing = unit(0.9, "lines"),
        strip.text = element_text(size = 10),
        strip.background = element_blank(),
        axis.text.x = element_text(angle = 90, size = 10),
        axis.text.y = element_text(size = 10),
        legend.position = 'right',
        legend.text = element_text(size = 11),
        plot.title = element_text(hjust = 0.5, size = 18))
#ggsave("/Users/nicole/Desktop/Mechanization/Figures/Background_Figures/SSP_Projections_exp_mdi_WBregions.png", width = 8, height = 6, dpi = 300)

# per income group 

income_summary <- Summary %>% 
  dplyr::left_join(income_group, by = "countrycode") %>%
  dplyr::group_by(income_group, scenario, year) %>%
  dplyr::mutate(mdi_fit = mean(mdi_proj, na.rm = T),
                mdi_upr = mean(mdi_high, na.rm = T), 
                mdi_lwr = mean(mdi_low, na.rm = T)) %>%
  dplyr::ungroup() %>%
  drop_na(income_group) %>%
  mutate(year = as.numeric(year))

income_summary_baseline <- Summary_baseline %>%
  dplyr::left_join(income_group, by = "countrycode") %>%
  dplyr::group_by(income_group) %>%
  filter(year %in% 2000:2010) %>%
  dplyr::mutate(mdi_baseline = mean(mdi_log, na.rm = T)) %>%
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
             aes(year, mdi_baseline, color = scenario)) +
  geom_ribbon(data = income_summary %>% filter(year > 2019),
              aes(x = year, y = mdi_fit, ymin = mdi_lwr, ymax = mdi_upr,
                  color = scenario, fill = scenario), alpha = .3, linetype = 0, show.legend = FALSE) +
  geom_line(data = income_summary %>% filter(year > 2019) %>% filter(scenario != "SSP1"), aes(year, mdi_fit, color = scenario),
            size = 0.8) +
  geom_line(data = income_summary %>% filter(year > 2019) %>% filter(scenario == "SSP1"), aes(year, mdi_fit, color = scenario),
            size = 0.8) +
  facet_wrap(~ income_group, ncol = 4) +
  scale_x_continuous(breaks = c(1990, 2000, 2020, 2040, 2060, 2080, 2095),
                     labels = c("1990", "2000", "2020", "2040", "2060", "2080", "2100")) +
  scale_y_continuous(labels = f) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  labs(x = 'Year', y = 'log(MDI)', title = '') +
  theme_bw() +
  theme(panel.spacing = unit(0.9, "lines"),
        strip.text = element_text(size = 10),
        strip.background = element_blank(),
        axis.text.x = element_text(angle = 90, size = 10),
        axis.text.y = element_text(size = 10),
        legend.position = 'right',
        legend.text = element_text(size = 10),
        plot.title = element_text(hjust = 0.5, size = 18))
#ggsave("/Users/nicole/Desktop/Mechanization/Figures/Background_Figures/SSP_Projections_mdi_IncomeGroup.png", width = 8, height = 6, dpi = 300)

# developed / developing 

developed_summary <- Summary %>% 
  dplyr::left_join(developed_developing, by = "countrycode") %>%
  dplyr::group_by(classification, scenario, year) %>%
  dplyr::mutate(mdi_fit = mean(mdi_proj, na.rm = T),
                mdi_upr = mean(mdi_high, na.rm = T), 
                mdi_lwr = mean(mdi_low, na.rm = T)) %>%
  dplyr::ungroup() %>%
  drop_na(classification) %>%
  mutate(year = as.numeric(year))

developed_summary_baseline <- Summary_baseline %>%
  dplyr::left_join(developed_developing, by = "countrycode") %>%
  dplyr::group_by(classification) %>%
  filter(year %in% 2000:2010) %>%
  dplyr::mutate(mdi_baseline = mean(mdi_log, na.rm = T)) %>%
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
             aes(year, mdi_baseline, color = scenario)) +
  geom_ribbon(data = developed_summary %>% filter(year > 2019),
              aes(x = year, y = mdi_fit, ymin = mdi_lwr, ymax = mdi_upr,
                  color = scenario, fill = scenario), alpha = .3, linetype = 0, show.legend = FALSE) +
  geom_line(data = developed_summary %>% filter(year > 2019) %>% filter(scenario != "SSP1"), aes(year, mdi_fit, color = scenario),
            size = 0.8) +
  geom_line(data = developed_summary %>% filter(year > 2019) %>% filter(scenario == "SSP1"), aes(year, mdi_fit, color = scenario),
            size = 0.8) +
  facet_wrap(~ classification, ncol = 4) +
  scale_x_continuous(breaks = c(1990, 2000, 2020, 2040, 2060, 2080, 2095),
                     labels = c("1990", "2000", "2020", "2040", "2060", "2080", "2100")) +
  scale_y_continuous(labels = f) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  labs(x = 'Year', y = 'log(MDI)', title = '') +
  theme_bw() +
  theme(panel.spacing = unit(0.9, "lines"),
        strip.text = element_text(size = 10),
        strip.background = element_blank(),
        axis.text.x = element_text(angle = 90, size = 10),
        axis.text.y = element_text(size = 10),
        legend.position = 'right',
        legend.text = element_text(size = 10),
        plot.title = element_text(hjust = 0.5, size = 18))
#ggsave("/Users/nicole/Desktop/Mechanization/Figures/Background_Figures/SSP_Projections_mdi_developed_developing.png", width = 8, height = 6, dpi = 300)

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
  geom_point(data = Summary_baseline %>% filter(year == 2005), aes(year, mdi_baseline, color = region)) +
  geom_ribbon(data = Summary %>% filter(scenario == "SSP2"), aes(x = year, y=mdi_fit, ymin = mdi_lwr, ymax= mdi_upr, color = region, fill = region), alpha = .2, linetype = 0) +
  geom_line(data = Summary  %>% filter(scenario == "SSP2"), aes(year, mdi_fit, color = region), size = 0.8) +
  scale_x_continuous(breaks = c(2000, 2020, 2040, 2060, 2080, 2095),
                     labels = c("2000", "2020", "2040", "2060", "2080", "2100"))+
  theme(panel.spacing = unit(0.9, "lines"), strip.text = element_text(size=13)) +
  theme(panel.background = element_blank()) +
  labs(x = 'Year', y = 'log(MDI)', title = '') +
  theme(axis.text.x = element_text(angle = 90, size = 13), axis.text.y = element_text(size = 13) ) +
  theme(legend.position= 'right', text = element_text(size=13), plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors)
#ggsave("/Users/nicole/Desktop/Mechanization/Figures/Background_Figures/SSP_Projections_mdi_SSP2_only.png", width = 8, height = 6, dpi = 300)

# ALL SSPs 

ggplot() +
  geom_ribbon(data = Summary %>% filter(scenario != "Baseline"), aes(x = year, y = mdi_fit, ymin = mdi_lwr, ymax = mdi_upr, color = region, fill = region), alpha = .2, linetype = 0) +
  geom_line(data = Summary %>% filter(scenario != "Baseline"), aes(year, mdi_fit, color = region), size = 0.8) +
  scale_x_continuous(breaks = c(2020, 2040, 2060, 2080, 2095), labels = c("2020", "2040", "2060", "2080", "2100")) +
  theme(panel.spacing = unit(0.9, "lines"), strip.text = element_text(size = 13)) +
  theme(panel.background = element_blank()) +
  labs(x = 'Year', y = 'log(MDI)', title = '') +
  theme(axis.text.x = element_text(angle = 90, size = 13), axis.text.y = element_text(size = 13)) +
  theme(legend.position = 'right', text = element_text(size = 13), plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  facet_wrap(~ scenario, ncol = 3)
#ggsave("/Users/nicole/Desktop/Mechanization/Figures/Background_Figures/SSP_Projections_mdi_ALL_SSPs.png", width = 8, height = 6, dpi = 300)

# per developed/developing country 

colors <- c("Developed" = "#FF69B4",  
            "Developing" = "#6B8F7E")

ggplot() +
  geom_ribbon(data = developed_summary %>% filter(scenario != "Baseline"), aes(x = year, y = mdi_fit, ymin = mdi_lwr, ymax = mdi_upr, color = classification, fill = classification), alpha = .2, linetype = 0) +
  geom_line(data = developed_summary %>% filter(scenario != "Baseline"), aes(year, mdi_fit, color = classification), size = 0.8) +
  scale_x_continuous(breaks = c(2020, 2040, 2060, 2080, 2095), labels = c("2020", "2040", "2060", "2080", "2100")) +
  theme(panel.spacing = unit(0.9, "lines"), strip.text = element_text(size = 13)) +
  theme(panel.background = element_blank()) +
  labs(x = 'Year', y = 'log(MDI)', title = '') +
  theme(axis.text.x = element_text(angle = 90, size = 13), axis.text.y = element_text(size = 13)) +
  theme(legend.position = 'right', text = element_text(size = 13), plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  facet_wrap(~scenario, ncol = 2)
#ggsave("/Users/nicole/Desktop/Mechanization/Figures/Background_Figures/SSP_Projections_mdi_ALL_SSPs_developed_developing.png", width = 8, height = 6, dpi = 300)

# per income group

colors <- c("High income" = "#B06D4C",
            "Upper middle income" = "#E5B76E",
            "Lower middle income" = "#8AA7B1",
            "Low income" = "#D38D5F")

ggplot() +
  geom_ribbon(data = income_summary %>% filter(scenario != "Baseline"), aes(x = year, y = mdi_fit, ymin = mdi_lwr, ymax = mdi_upr, color = income_group, fill = income_group), alpha = .2, linetype = 0) +
  geom_line(data = income_summary %>% filter(scenario != "Baseline"), aes(year, mdi_fit, color = income_group), size = 0.8) +
  scale_x_continuous(breaks = c(2020, 2040, 2060, 2080, 2095), labels = c("2020", "2040", "2060", "2080", "2100")) +
  theme(panel.spacing = unit(0.9, "lines"), strip.text = element_text(size = 13)) +
  theme(panel.background = element_blank()) +
  labs(x = 'Year', y = 'log(MDI)', title = '') +
  theme(axis.text.x = element_text(angle = 90, size = 13), axis.text.y = element_text(size = 13)) +
  theme(legend.position = 'right', text = element_text(size = 13), plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  facet_wrap(~scenario, ncol = 2)
#ggsave("/Users/nicole/Desktop/Mechanization/Figures/Background_Figures/SSP_Projections_mdi_ALL_SSPs_income_group.png", width = 8, height = 6, dpi = 300)

# Maps percentage increase 

mdi_df <- mdi_df %>%
  filter(year %in% 1996:2015) %>%
  dplyr::group_by(countrycode) %>% 
  dplyr::mutate(average = mean(mdi_log, na.rm = TRUE)) %>%
  filter(year == "2005") %>%
  ungroup() 

mdi_proj$year <- as.integer(mdi_proj$year)

print(mdi_df)
print(mdi_proj)

mdi_proj$source <- "mdi_proj"
mdi_df$source <- "mdi_df"

# Check data types of 'year' columns
# Convert the year column in mdi_proj to character
mdi_proj$year <- as.character(mdi_proj$year)

# Now both year columns are of the same data type (character)
combined_data <- bind_rows(mdi_proj, mdi_df) %>%
  dplyr::select(countrycode, year, mdi_proj, mdi_low, mdi_high, source, mdi_log) %>%
  filter(year %in% c(2005, 2035, 2050, 2075, 2100))
print(combined_data)

# Calculate mean mdi_proj values for 2050 and 2100 for each countrycode
mean_values <- combined_data %>%
  group_by(countrycode) %>%
  summarize(mean_proj_2035 = mean(mdi_proj[year == 2035], na.rm = TRUE),
            mean_proj_2050 = mean(mdi_proj[year == 2050], na.rm = TRUE),
            mean_proj_2075 = mean(mdi_proj[year == 2075], na.rm = TRUE),
            mean_proj_2100 = mean(mdi_proj[year == 2100], na.rm = TRUE))

# Merge the mean values with the mdi_log values in 2005
merged_data <- mean_values %>%
  left_join(combined_data %>%
              filter(year == 2005) %>%
              dplyr::select(countrycode, mdi_log), by = "countrycode")

# Calculate the percentage difference for each countrycode
merged_data <- merged_data %>%
  mutate(diff_2050 = (mean_proj_2035 - mdi_log) / mdi_log * 100,
         diff_2050 = (mean_proj_2050 - mdi_log) / mdi_log * 100,
         diff_2050 = (mean_proj_2075 - mdi_log) / mdi_log * 100,
         diff_2100 = (mean_proj_2100 - mdi_log) / mdi_log * 100) %>%
  dplyr::rename(var = mean_proj_2100)

# Define colors for the second map
colors2 <- c("Category 1" = "#FFD700",  # gold
             "Category 2" = "#FFA500",  # orange
             "Category 3" = "#A63603",  # dark orange
             "Category 4" = "#7F4F2A")  # peru

map <- joinCountryData2Map(merged_data, joinCode = "ISO3", nameJoinColumn = "countrycode")

map_poly <-  fortify(map) %>% 
  merge(map@data, by.x="id", by.y="ADMIN", all.x=T) %>%
  arrange(id, order) %>% 
  mutate(var %>% as.numeric())

# Plot map 

ggplot(map_poly, aes(x = long, y = lat, group = group)) +
  coord_map(projection = 'rectangular',parameters = 0, orientation=NULL, xlim = c(-180, 180), ylim = c(-55, 75)) + 
  #coord_map(projection = 'mollweide', xlim = c(-180, 180), ylim = c(-60, 75))  + # Remove antarctica
  geom_polygon(aes(fill = var)) +
  #scale_fill_viridis_c(option="plasma", limits = c(-15, 0)) +
  #scale_fill_distiller(palette = "Spectral") +
  scale_fill_gradientn(colors = colors2, na.value = "#C1CDCD") + # Set the colorscale
  labs(fill = '% difference'
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
#ggsave("/Users/nicole/Desktop/Mechanization/Figures/Background_Figures/MDI_newcolors.png", width = 8, height = 6, dpi = 300)

# Additional data analysis for tables (main results)

regions_global <- read_xlsx("/Users/nicole/Desktop/Mechanization/Data/Regions/Regions_WB_global.xlsx")

mdi_global <- mdi_df %>%
  filter(year %in% 2000:2010) %>%
  dplyr::left_join(regions_global, by = "countrycode", multiple = "all") %>%
  dplyr::group_by(region) %>% 
  dplyr::mutate(average_2005 = mean(mdi_log, na.rm = TRUE)) %>%
  filter(year == "2005") %>%
  ungroup() 

print(mdi_global)
print(mdi_proj)

mdi_proj <- mdi_proj %>%
  dplyr::left_join(regions_global, by = "countrycode", multiple = "all") 

mdi_proj$source <- "mdi_proj"
mdi_proj$year <- as.character(mdi_proj$year)  # Convert to character
mdi_global$year <- as.character(mdi_global$year)  # Convert to character
mdi_global$source <- "mdi_gloabl"

combined_data <- bind_rows(mdi_proj, mdi_global) %>%
  dplyr::select(scenario, countrycode, region, year, mdi_proj, mdi_low, mdi_high, source, average_2005) %>%
  filter(year %in% c(2005, 2035, 2050, 2075, 2100))
print(combined_data)

# Calculate mean mdi_proj values for 2050 and 2100 for each countrycode
mean_values <- combined_data %>%
  group_by(region, scenario) %>%
  summarize(mean_proj_2035 = mean(mdi_proj[year == 2035], na.rm = TRUE),
            mean_proj_2050 = mean(mdi_proj[year == 2050], na.rm = TRUE),
            mean_proj_2075 = mean(mdi_proj[year == 2075], na.rm = TRUE),
            mean_proj_2100 = mean(mdi_proj[year == 2100], na.rm = TRUE))

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
#write.table(merged_data, "/Users/nicole/Desktop/Mechanization/Data/Tables_results/Summary_MDI_Table_6.csv", sep = ",", row.names = FALSE)

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

combined_data <- bind_rows(mdi_proj, mdi_global) %>%
  dplyr::select(scenario, countrycode, region, year, mdi_proj, mdi_low, mdi_high, source, average_2005) %>%
  filter(year %in% c(2005, 2035, 2050, 2075, 2100))

# Calculate mean api_proj values for 2050 and 2100 for each countrycode
mean_values <- combined_data %>%
  group_by(region, scenario) %>%
  summarize(mean_proj_2035 = mean(mdi_proj[year == 2035], na.rm = TRUE),
            mean_proj_2050 = mean(mdi_proj[year == 2050], na.rm = TRUE),
            mean_proj_2075 = mean(mdi_proj[year == 2075], na.rm = TRUE),
            mean_proj_2100 = mean(mdi_proj[year == 2100], na.rm = TRUE))

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

# Test out of sample performance 

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
  
  # Fit the model using the training data
  plm_mdi <- plm(mdi_log ~ log(gdppc), data = train_data, effect = "time", model = "between", index = "countrycode")
  
  # Evaluate the model's performance on the training data
  response_train <- train_data$mdi_log
  predictors_train <- train_data$gdppc
  predictions_train <- coef(plm_mdi)["log(gdppc)"] * log(predictors_train)
  valid_indices_train <- !is.na(response_train) & !is.na(predictions_train)
  response_train <- response_train[valid_indices_train]
  predictions_train <- predictions_train[valid_indices_train]
  rmse_train_cv[i] <- sqrt(mean((response_train - predictions_train)^2))
  
  # Evaluate the model's performance on the testing data
  response_test <- test_data$mdi_log
  predictors_test <- test_data$gdppc
  predictions_test <- coef(plm_mdi)["log(gdppc)"] * log(predictors_test)
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




