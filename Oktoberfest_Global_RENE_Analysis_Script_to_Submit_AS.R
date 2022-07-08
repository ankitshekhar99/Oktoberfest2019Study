## Analyzing energy and emissions statistics of Oktoberfest ##
## Analyzing RENE and NG trends for various countries ##
#' Script by Ankit Shekhar (ankit.shekhar@usys.ethz.ch)
#' Script updated date: 15th June 2022
####################### Script Start ##############################

#0.1 Loading required packages ######

library(ggplot2)
library(dplyr)
library(reshape2)
library(ggthemes)
library(ggsci)
library(RColorBrewer)
library(gridExtra)
library(readxl)
library(readr)

#0.2 General Theme of the plots
th = theme_tufte() + 
  theme(panel.background = element_rect(fill = "white", colour = "black", size = 1),
        panel.grid.major = element_line(size = 0.05, linetype = 'dashed', colour = "grey"), 
        panel.grid.minor = element_line(size = 0.05, linetype = 'dashed',colour = "grey"),
        panel.spacing = unit(0,'lines')) +
  theme(axis.text = element_text(size = 11), axis.title = element_text(size = 11, face = 'bold'),
        text = element_text(size = 11), strip.text = element_text(size = 11, face = 'bold'),
        plot.title = element_text(size = 12, face = 'bold'), panel.spacing.x = unit(0,'lines'))

#1. Oktoberfest ###########
##1.1 Importing data and calculations ######

Oktoberfest_Data = read_csv("Oktoberfest_Data_to_plot.csv")

##1.2 Common constants used in the study-------------


Hydro_EF_range = c(26/3.6,29/3.6) ## Max and minimum Emission factor of Hydro in (tCO2/TJ)
EF_CO2 = 888/3.6 ## tCO2/TJ of coal
EF_CH4 = 55 ## tCO2/TJ for Natural gas
Leakage_OkFest = 0.0136 ## estimated leakage rates of 1.36% from previous study
CF_methane = 0.000036 ## Converting CH4 (m^3) to TJ
Density_CH4 = 0.668 ## Density in kg/m^3 
CH4_GWP100 = 34 ## 100 years equivalent
CH4_GWP20 = 85 ## 20 years equivalent
CF_elect = 0.0000036 ### kWh to TJ

## from https://doi.org/10.1016/j.rser.2014.07.087
Hydro_EF_German = 13/3.6 
Wind_EF = 26/3.6 
Solar_EF = 85/3.6
GeoBio_EF = 41/3.6
Nuclear_EF = 29/3.6
Gas_EF = 499/3.6
Coal_EF = 888/3.6
Oil_EF = 733/3.6
Et = 1 ## 1 unit energy (1 TJ)
CH4_m3 = 27777.78 ### m^3 of CH4 for 1 TJ of Energy

Oktoberfest_Data = Oktoberfest_Data %>% 
  mutate(NG_emission_100 = Natural_Gas_m3*((1-Leakage_OkFest)*CF_methane*EF_CH4 + Leakage_OkFest*CH4_GWP100*Density_CH4*0.001),
         NG_emission_20 = Natural_Gas_m3*((1-Leakage_OkFest)*CF_methane*EF_CH4 + Leakage_OkFest*CH4_GWP20*Density_CH4*0.001),
         NG_emission_No_leak = Natural_Gas_m3*(1-0)*CF_methane*EF_CH4,
         Elect_emissions_high = 0.01*Hydro_EF_range[2]*RE_Share_in_percent + 0.01*(100-RE_Share_in_percent)*EF_CO2,
         Elect_emissions_low = 0.01*Hydro_EF_range[1]*RE_Share_in_percent + 0.01*(100-RE_Share_in_percent)*EF_CO2,
         Elect_emission_mean = 0.5*(Elect_emissions_high + Elect_emissions_low),
         Elect_emissions_tCo2 = Elect_emission_mean*Electricity_kWh*CF_elect) %>% 
  mutate(NG_energy_TJ = Natural_Gas_m3*CF_methane, Elect_energy_TJ = Electricity_kWh*CF_elect)

##1.3 Total energy consumption by different sources #####--------------

ggplot(Oktoberfest_Data, aes(x = Year)) + 
  geom_smooth(aes(y = NG_energy_TJ, color = 'Natural Gas',fill = 'Natural Gas'), method = 'lm') + 
  geom_point(aes(y = NG_energy_TJ, color = 'Natural Gas')) +
  geom_line(aes(y = NG_energy_TJ,color = 'Natural Gas'), linetype = 'dashed') +
  geom_smooth(aes(y = Elect_energy_TJ, color = 'Electricity',fill = 'Electricity'), method = 'lm') + 
  geom_point(aes(y = Elect_energy_TJ, color = 'Electricity')) + 
  geom_line(aes(y = Elect_energy_TJ,color = 'Electricity'), linetype = 'dashed') +
  scale_color_brewer(palette = 'Dark2') + scale_fill_brewer(palette = 'Dark2') +
  xlab('Year') +  ylab('Energy Consumption (TJ)') +
  labs(color = '', fill = '') + scale_x_continuous(breaks = seq(1989,2019,3)) +
  scale_color_brewer(palette = 'Dark2') + scale_fill_brewer(palette = 'Dark2') +
  ggtitle('Total energy consumption from sources') + th + scale_y_continuous(breaks = seq(0,12,2)) +
  theme(legend.position = c(0.2,0.88), legend.key.size = unit(0.3, "cm"))

## Saving the plots in high resolution
ggsave('Oktoberfest_energy_consumption.png', height = 2.5, width = 5, units = 'in', dpi = 700)


##1.4 Share of Renewable energy (RENE) in Oktoberfest electricity supply ####---------

ggplot(Oktoberfest_Data, aes(x = Year, y = RE_Share_in_percent)) + geom_point(color = 'darkgreen') + 
  geom_line(color = 'darkgreen') +
  ylab('Percentage share') + th + scale_x_continuous(breaks = seq(1989,2019,3)) +
  ggtitle('Percent share of Renewable energy in Electricity consumption') + 
  theme(legend.position = c(0.85,0.9))

ggsave('Oktoberfest_RENE_share1.png',b, height = 2.5, width = 5, units = 'in', dpi = 700)

##1.5 Total estimated CO2 emissions from different sources in Oktoberfest ####----------------

ggplot(Oktoberfest_Data, aes(x = Year)) + 
  geom_line(aes(y = NG_emission_No_leak, color = 'Natural Gas - without leak'), linetype = 'dashed') + 
  geom_point(aes(y = NG_emission_No_leak, color = 'Natural Gas - without leak'), shape = 1) +
  geom_line(aes(y = NG_emission_20, color = 'Natural Gas - with leakage'), linetype = 'dashed') + 
  geom_point(aes(y = NG_emission_20, color = 'Natural Gas - without leak'), shape = 1) +
  geom_line(aes(y = Elect_emissions_tCo2, color = 'Electricity')) + 
  geom_point(aes(y = Elect_emissions_tCo2, color = 'Electricity')) + 
  xlab('Year') +  ylab(expression('Tonnes of CO'[2]*'emissions')) +
  labs(color = '', fill = '') + scale_x_continuous(breaks = seq(1989,2019,3)) +
  scale_color_brewer(palette = 'Dark2') + scale_fill_brewer(palette = 'Dark2') +
  ggtitle(expression('Total CO'[2]*' emissions from sources')) + th + scale_y_continuous(breaks = seq(0,2300,300)) +
  theme(legend.position = c(0.7,0.85),legend.key.size = unit(0.3, "cm"))

ggsave('Oktoberfest_total_energy_emissions.png', height = 2.5, width = 5, units = 'in', dpi = 700)


##1.6 Figure for CO2 emissions per capita ##-----------------------------------------------

ggplot(Oktoberfest_Data, aes(x = Year)) + 
  geom_line(aes(y = NG_emission_No_leak/NG_energy_TJ, color = 'Natural Gas - without leak'), linetype = 'dashed') + 
  geom_point(aes(y = NG_emission_No_leak/NG_energy_TJ, color = 'Natural Gas - without leak'), shape = 1) +
  geom_line(aes(y = NG_emission_20/NG_energy_TJ, color = 'Natural Gas - with leak'), linetype = 'dashed') + 
  geom_point(aes(y = NG_emission_20/NG_energy_TJ, color = 'Natural Gas - with leak'), shape = 1) +
  geom_line(aes(y = Elect_emission_mean, color = 'Electricity')) + 
  geom_point(aes(y = Elect_emission_mean, color = 'Electricity')) + 
  geom_ribbon(aes(ymax = Elect_emissions_high, ymin = Elect_emissions_low, fill = 'Electricity'), alpha = 0.5) + 
  xlab('Year') +  ylab(expression('Emissions (tCO'[2]*')')) + geom_hline(yintercept = 0) +
  labs(color = '', fill = '') + scale_x_continuous(breaks = seq(1989,2019,3)) + guides(fill = FALSE) +
  scale_color_brewer(palette = 'Dark2') + scale_fill_brewer(palette = 'Dark2') +
  ggtitle(expression('CO'[2]*' Emissions per TJ of energy consumption')) + th + 
  theme(legend.position = c(0.7,0.88),legend.key.size = unit(0.3, "cm"))

ggsave('Oktoberfest_emissions_per_capita_leak_no_leak.png', height = 2.5, width = 5, units = 'in', dpi = 700) 

##1.7 Fraction of Electrical energy ##### ----------------------------------------

ggplot(Oktoberfest_Data, aes(x = Year, y = Elect_energy_TJ/(Elect_energy_TJ + NG_energy_TJ))) +
  geom_point() + geom_line(color= 'blue', linetype = 'dotted', size = 0.4) +
  geom_smooth(method = 'lm', fill = 'lightblue') + scale_x_continuous(breaks = seq(1989,2019,3)) +
  ggtitle('Ratio of electrical energy used in Oktoberfest') +
  th +  ylab('Fraction of electrical energy') + geom_hline(yintercept = 0.5, linetype = 'dashed')

ggsave('Electrical_Energy ratio.png', height = 3, width = 4.5, units = 'in', dpi = 700)

##1.8 Total Energy consumption of Oktoberfest #######-------------------------

ggplot(Oktoberfest_Data, aes(x = Year, y = (Elect_energy_TJ + NG_energy_TJ))) +
  geom_point() + geom_smooth(method = 'lm') + scale_x_continuous(breaks = seq(1989,2019,3)) +
  geom_line(linetype = 'dotted')+
  ggtitle('Trend of total energy consumption') +
  th +  ylab('Total Energy (TJ)')

ggsave('Total_Energy_Trend.png', height = 3, width = 4.5, units = 'in', dpi = 700)

##1.9 Transition phase plots for Oktoberfest ####-------------

Leakage_rate_range = seq(0,0.15,0.001) ## Varying from 0 to 15%
RENE_range = seq(0,100,1)  ## Share of Rene from 0 to 100%
summary(Oktoberfest_Data)
k = 1
NG20 = double(); NG100 = double(); Leak = double(); Rene = double()
Electr = double()
for (i in c(1:length(Leakage_rate_range)))
{
  for (j in c(1:length(RENE_range))) 
  {
    Leak[k] = Leakage_rate_range[i]
    Rene[k] = RENE_range[j]
    NG20[k] = CH4_m3*((1-Leakage_rate_range[i])*CF_methane*EF_CH4 + Leakage_rate_range[i]*CH4_GWP20*Density_CH4*0.001)
    NG100[k] = CH4_m3*((1-Leakage_rate_range[i])*CF_methane*EF_CH4 + Leakage_rate_range[i]*CH4_GWP100*Density_CH4*0.001)
    Electr[k] = 0.01*RENE_range[j]*5.555 + 0.01*(100-RENE_range[j])*EF_CO2  ## 15.444 is maximum EF of RENE ## 5.5 for Oktoberfest
    k = k+1
  }
}

RENE_leakage_DF = data.frame(NG_20 = NG20, NG_100 = NG100, LeakCH4 = Leak, RE_percent = Rene, ElectCO2 = Electr, Net_Co2_emssions = - Electr+NG100,
                             Net_Co2_emssions_20 = - Electr+NG20)

## Plotting for GWP of 100 years
ggplot(RENE_leakage_DF, aes(x = LeakCH4*100, y = RE_percent, fill = -ElectCO2+NG_100)) + geom_tile() + theme_bw() +
  xlab('Methane leakage (%)') + ylab('RENE share (%)') + 
  scale_fill_gradient2(high = 'blue',low = 'red3', n.breaks = 8, midpoint = 0) +
  th + theme(legend.position = c(0.8,0.8), legend.title = element_blank(), legend.direction = 'horizontal') + 
  labs(fill = 'Emission difference') + geom_vline(xintercept = 1.1, linetype = 'dashed') +
  theme(panel.background = element_rect(fill = "white", colour = "black", size = 1), 
        legend.direction = 'vertical', legend.key.width = unit(0.3,"cm"))

ggsave('RENE_Leakage_tiles_GWP100_OKtoberfest.png', height = 6, width = 5.8, units = 'in', dpi = 700)

## Plotting for GWP of 20 years

ggplot(RENE_leakage_DF %>% filter(LeakCH4 < 0.15, RE_percent < 101), aes(x = LeakCH4*100, y = RE_percent, fill = -ElectCO2+NG_20)) + geom_tile() + theme_bw() +
  xlab('Methane leakage (%)') + ylab('RENE share (%)') + 
  scale_fill_gradient2(high = 'blue',low = 'red3', n.breaks = 10, midpoint = 0) +
  th + theme(legend.position = c(0.8,0.8), legend.title = element_blank(), legend.direction = 'horizontal') + 
  labs(fill = 'Emission difference') + geom_vline(xintercept = 1.1, linetype = 'dashed') +
  theme(panel.background = element_rect(fill = "white", colour = "black", size = 1), 
        legend.direction = 'vertical', legend.key.width = unit(0.3,"cm"))

ggsave('RENE_Leakage_tiles_GWP200_Oktoberfest.png', height = 6, width = 5.8, units = 'in', dpi = 700)

# Oktoberfest plots done ---------------------------------------------------------------------------------------##

#2. Electricity mix from Different countries #####-----------------------------------------------------

##2.1 Importing file with yearly RENE countrywise and sector-wise ################----------------
Hydro_BP <- read_excel("RENE_data_from_BP_Statistics.xlsx",sheet = "Hydro_Consumption")
View(Hydro_BP)
str(Hydro_BP)

Solar_BP <- read_excel("RENE_data_from_BP_Statistics.xlsx",sheet = "Solar_Consumption")
View(Solar_BP)
str(Solar_BP)

Wind_BP <- read_excel("RENE_data_from_BP_Statistics.xlsx",sheet = "Wind_Consumption")
View(Wind_BP)
str(Wind_BP)

GeoBio_BP <- read_excel("RENE_data_from_BP_Statistics.xlsx",sheet = "Geothermal_Biomass_Consumption")
View(GeoBio_BP)
str(GeoBio_BP)

share_electricity_renewables <- read_csv("share-electricity-renewables.csv")
share_electricity_nuclear <- read_csv("share-electricity-nuclear.csv")

Total_Energy_BP <- read_excel("Total_Consumption.xlsx",sheet = "Primary_Consumption_EJ")
View(Total_Energy_BP)
str(Total_Energy_BP)

Nuclear_BP <- read_excel("Total_Consumption.xlsx",sheet = "Nuclear_Consumption_EJ")
View(Nuclear_BP)
str(Nuclear_BP)

##2.2 Filtering countried and additional post-processing--------------
#### Looking at interested countries -- US, Russia, China, Iran, Japan, Venezuela, Canada, Germany, Saudi Arabia, Mexico, UAE, Italy, UK, Thailand, India,
#### Uzbekistan, Turkey, Egypt, Argentina, South Korea, Indonesia, Pakistan, Qatar, Netherlands, Turkmenistan, Australia
t = data.frame(table(share_electricity_renewables$Entity))
table(share_electricity_renewables$Entity)
table(Hydro_BP$Country_EJ_38percent_efficiency)
Countries_BP = c('Argentina','Australia','Bangladesh','Belgium','Brazil','Canada','China','Egypt','France','Germany','India','Indonesia','Iran','Italy','Japan','Malaysia','Mexico',
                 'Netherlands','Pakistan','Poland','Russian Federation','South Korea','Spain','Thailand','Turkey','Ukraine','United Kingdom','US','Venezuela')

names(Hydro_BP)
Hydro_BP = melt(Hydro_BP, id.vars = 'Country_EJ_input_equi')
names(Hydro_BP) = c('Country','Year','Hydro_EJie')
Hydro_BP$Year = as.numeric(as.character(Hydro_BP$Year))
str(Hydro_BP)
Hydro_BP = Hydro_BP %>% mutate(Thermal_efficiency = if_else(Year < 2001, 0.36,
                                                            if_else(Year %in% c(2001:2017), (0.36 + 0.04*(Year-2000)/17), (0.40 + 0.05*(Year-2050)/32)))) %>% 
  mutate(Hydro_TWh = Hydro_EJie*Thermal_efficiency/0.0036) %>% select(Country, Year, Hydro_TWh) %>% filter(Country %in% Countries_BP)

table(Hydro_BP$Country)
str(Hydro_BP)


names(Solar_BP)
Solar_BP = melt(Solar_BP, id.vars = 'Country_EJ_input_equi')
names(Solar_BP) = c('Country','Year','Solar_EJie')
Solar_BP$Year = as.numeric(as.character(Solar_BP$Year))
str(Solar_BP)
Solar_BP = Solar_BP %>% mutate(Thermal_efficiency = if_else(Year < 2001, 0.36,
                                                            if_else(Year %in% c(2001:2017), (0.36 + 0.04*(Year-2000)/17), (0.40 + 0.05*(Year-2050)/32)))) %>% 
  mutate(Solar_TWh = Solar_EJie*Thermal_efficiency/0.0036) %>% select(Country, Year, Solar_TWh) %>% filter(Country %in% Countries_BP)

table(Solar_BP$Country)
str(Solar_BP)


names(Wind_BP)
Wind_BP = melt(Wind_BP, id.vars = 'Country_EJ_input_equi')
names(Wind_BP) = c('Country','Year','Wind_EJie')
Wind_BP$Year = as.numeric(as.character(Wind_BP$Year))
str(Wind_BP)
Wind_BP = Wind_BP %>% mutate(Thermal_efficiency = if_else(Year < 2001, 0.36,
                                                          if_else(Year %in% c(2001:2017), (0.36 + 0.04*(Year-2000)/17), (0.40 + 0.05*(Year-2050)/32)))) %>% 
  mutate(Wind_TWh = Wind_EJie*Thermal_efficiency/0.0036) %>% select(Country, Year, Wind_TWh) %>% filter(Country %in% Countries_BP)

table(Wind_BP$Country)
str(Wind_BP)


names(GeoBio_BP)
GeoBio_BP = melt(GeoBio_BP, id.vars = 'Country_EJ_input_equi')
names(GeoBio_BP) = c('Country','Year','GeoBio_EJie')
GeoBio_BP$Year = as.numeric(as.character(GeoBio_BP$Year))
str(GeoBio_BP)
GeoBio_BP = GeoBio_BP %>% mutate(Thermal_efficiency = if_else(Year < 2001, 0.36,
                                                              if_else(Year %in% c(2001:2017), (0.36 + 0.04*(Year-2000)/17), (0.40 + 0.05*(Year-2050)/32)))) %>% 
  mutate(GeoBio_TWh = GeoBio_EJie*Thermal_efficiency/0.0036) %>% select(Country, Year, GeoBio_TWh) %>% filter(Country %in% Countries_BP)

table(GeoBio_BP$Country)
str(GeoBio_BP)


RENE_BP_all = inner_join(Hydro_BP, inner_join(Solar_BP, inner_join(Wind_BP, GeoBio_BP)))
RENE_BP_all = RENE_BP_all %>% mutate(RENE_total = Solar_TWh + Wind_TWh + Hydro_TWh + GeoBio_TWh)
RENE_BP_all = RENE_BP_all %>% mutate(Wind_percent = 100*Wind_TWh/RENE_total, Solar_percent = 100*Solar_TWh/RENE_total,
                                     Hydro_percent = 100*Hydro_TWh/RENE_total, GeoBio_percent = 100*GeoBio_TWh/RENE_total)

table(RENE_BP_all$Country)
table(share_electricity_renewables$Entity)

### 2.2.1 Now looking at Nuclear Energy ###-------------
names(Nuclear_BP)
Nuclear_BP = melt(Nuclear_BP, id.vars = 'Nuclear_EJ')
names(Nuclear_BP) = c('Country','Year','Nuclear_EJie')
Nuclear_BP$Year = as.numeric(as.character(Nuclear_BP$Year))
str(Nuclear_BP)
Nuclear_BP = Nuclear_BP %>% mutate(Thermal_efficiency = if_else(Year < 2001, 0.36,
                                                                if_else(Year %in% c(2001:2017), (0.36 + 0.04*(Year-2000)/17), (0.40 + 0.05*(Year-2050)/32)))) %>% 
  mutate(Nuclear_TWh = Nuclear_EJie*Thermal_efficiency/0.0036) %>% select(Country, Year, Nuclear_TWh) %>% filter(Country %in% Countries_BP)

table(Nuclear_BP$Country)
str(Nuclear_BP)


names(Total_Energy_BP)  ## Total consumption not based on input equivalent
Total_Energy_BP = melt(Total_Energy_BP, id.vars = 'Primary_EJ')
names(Total_Energy_BP) = c('Country','Year','Total_Energy_EJie')
Total_Energy_BP$Year = as.numeric(as.character(Total_Energy_BP$Year))  
str(Total_Energy_BP)
Total_Energy_BP = Total_Energy_BP %>% mutate(Thermal_efficiency = 1) %>%  
  mutate(Total_Energy_TWh = Total_Energy_EJie*Thermal_efficiency/0.0036) %>% select(Country, Year, Total_Energy_TWh) %>% filter(Country %in% Countries_BP)

table(Total_Energy_BP$Country)
str(Total_Energy_BP)

Primary_Nuclear_BP = inner_join(Total_Energy_BP, Nuclear_BP)
summary(Primary_Nuclear_BP)

Primary_Nuclear_BP$Country[Primary_Nuclear_BP$Country == 'Russian Federation'] = 'Russia'
Primary_Nuclear_BP$Country[Primary_Nuclear_BP$Country == 'US'] = 'United States'
Primary_Nuclear_BP = Primary_Nuclear_BP %>% mutate(Nuclear_Share = Nuclear_TWh/Total_Energy_TWh)

ggplot(Primary_Nuclear_BP %>% filter(Country %in% Countries_final), aes(x = Year)) + 
  geom_line(aes(y = Nuclear_TWh*100/Total_Energy_TWh, color = '% of Nuclear'), show.legend = FALSE, size = 1) +
  facet_wrap(~Country, nrow = 5, scales = 'free_y') + ggtitle('Percentage of Nuclear Energy Consumption to total consumption') +
  scale_y_continuous(name = 'Share (%)') +
  theme_tufte() + scale_color_manual('', values = c('darkgreen')) +
  theme(panel.background = element_rect(fill = "white", colour = "black", size = 1),
        panel.grid.major = element_line(size = 0.05, linetype = 'dashed', colour = "grey"), 
        panel.grid.minor = element_line(size = 0.05, linetype = 'dashed',colour = "grey"),
        panel.spacing = unit(0,'lines')) + scale_x_continuous(breaks = seq(1989,2020,6)) +
  theme(axis.text.x = element_text(size = 10, angle = 90, hjust = 0.5, vjust = 0.5), axis.title.x = element_text(size = 11, face = 'bold'), 
        axis.text.y.left =  element_text(size = 10), axis.title.y.left = element_text(size = 11, face = 'bold'), 
        axis.text.y.right = element_text(size = 11,color = 'darkgreen'), 
        axis.title.y.right = element_text(size = 12, face = 'bold', color = 'darkgreen'), 
        legend.title = element_text(size = 12, face = 'bold'),
        legend.text = element_text(size = 12, face = 'bold'), legend.position = 'bottom', strip.text = element_text(size = 10, face = 'bold'),
        plot.title = element_text(size = 12, face = 'bold'), panel.spacing.x = unit(0.2,'lines'),panel.spacing.y = unit(0,'lines'))

ggsave('Nuclear_Energy_Share.png', height = 9.5, width = 12, units = 'in', dpi = 700)


Countries_RENE = c('Argentina','Australia','Bangladesh','Belgium','Brazil','Canada','China','Egypt','France','Germany','India','Indonesia','Iran','Italy','Japan','Malaysia','Mexico',
                   'Netherlands','Pakistan','Poland','Russia','South Korea','Spain','Thailand','Turkey','Ukraine','United Kingdom','United States','Venezuela')

RENE_percent = share_electricity_renewables %>% filter(Entity %in% Countries_RENE, Year %in% c(1990:2019))
Nuclear_percent = share_electricity_nuclear %>% filter(Entity %in% Countries_RENE, Year %in% c(1990:2019))

table(RENE_percent$Entity)
table(Nuclear_percent$Entity)


names(RENE_percent)[c(1,4)] = c('Country','RENE_share')
names(Nuclear_percent)[c(1,4)] = c('Country','Nuclear_share')
RENE_BP_all$Country[RENE_BP_all$Country == 'Russian Federation'] = 'Russia'
RENE_BP_all$Country[RENE_BP_all$Country == 'US'] = 'United States'
table(RENE_BP_all$Country)
table(RENE_percent$Country)
RENE_All = inner_join(RENE_BP_all, RENE_percent)
RENE_All = inner_join(RENE_All, Nuclear_percent)  ### Nuclear share in electricity added.
names(RENE_All)


###2.2.2 Plotting Nuclear share in electricity for countries ####
ggplot(RENE_All %>% filter(Country %in% Countries_final), aes(x = Year)) + 
  geom_line(aes(y = Nuclear_share, color = 'Share of Nuclear Energy'), show.legend = FALSE, size = 1) +
  facet_wrap(~Country, nrow = 5) + ggtitle('Percentage share of Nuclear Energy Consumption in Electricity') +
  scale_y_continuous(name = 'Share (%)') +
  theme_tufte() + scale_color_manual('', values = c('darkgreen')) +
  theme(panel.background = element_rect(fill = "white", colour = "black", size = 1),
        panel.grid.major = element_line(size = 0.05, linetype = 'dashed', colour = "grey"), 
        panel.grid.minor = element_line(size = 0.05, linetype = 'dashed',colour = "grey"),
        panel.spacing = unit(0,'lines')) + scale_x_continuous(breaks = seq(1989,2020,6)) +
  theme(axis.text.x = element_text(size = 10, angle = 90, hjust = 0.5, vjust = 0.5), axis.title.x = element_text(size = 11, face = 'bold'), 
        axis.text.y.left =  element_text(size = 10), axis.title.y.left = element_text(size = 11, face = 'bold'), 
        axis.text.y.right = element_text(size = 11,color = 'darkgreen'), 
        axis.title.y.right = element_text(size = 12, face = 'bold', color = 'darkgreen'), 
        legend.title = element_text(size = 12, face = 'bold'),
        legend.text = element_text(size = 12, face = 'bold'), legend.position = 'bottom', strip.text = element_text(size = 10, face = 'bold'),
        plot.title = element_text(size = 12, face = 'bold'), panel.spacing.x = unit(0.2,'lines'),panel.spacing.y = unit(0,'lines'))

ggsave('Nuclear_Energy_Share_ordered.png', height = 9.5, width = 9, units = 'in', dpi = 700)

##2.3 RENE share--------------------------
Electricity_mix_data = read_csv("electricity-prod-source-stacked.csv")
str(Electricity_mix_data)

Countries_RENE = c('Argentina','Australia','Bangladesh','Belgium','Brazil','Canada','China','Egypt','France','Germany','India','Indonesia','Iran','Italy','Japan','Malaysia','Mexico',
                   'Netherlands','Pakistan','Poland','Russia','South Korea','Spain','Thailand','Turkey','Ukraine','United Kingdom','United States','Venezuela')


Electricity_mix_data = Electricity_mix_data %>% filter(Entity %in% Countries_RENE, Year %in% c(1990:2019))
summary(Electricity_mix_data)
names(Electricity_mix_data)[4:11] = c('Elect_Coal_TWh','Elect_Gas_TWh','Elect_Hydro_TWh',
                                      'Elect_Nuclear_TWh','Elect_Oil_TWh','Elect_OtherRENE_TWh',
                                      'Elect_Solar_TWh','Elect_Wind_TWh')
names(Electricity_mix_data)[1] = 'Country'


Electricity_mix_data = Electricity_mix_data %>% 
  mutate(Total_Electricity = Elect_Coal_TWh+Elect_Gas_TWh+Elect_Hydro_TWh+Elect_Nuclear_TWh+
           Elect_Oil_TWh+Elect_OtherRENE_TWh+Elect_Solar_TWh+Elect_Wind_TWh) %>% 
  mutate(Coal_Share = Elect_Coal_TWh/Total_Electricity, Gas_Share = Elect_Gas_TWh/Total_Electricity,
         Hydro_Share = Elect_Hydro_TWh/Total_Electricity, Nuclear_Share = Elect_Nuclear_TWh/Total_Electricity,
         Oil_Share = Elect_Oil_TWh/Total_Electricity, OtherRENE_Share = Elect_OtherRENE_TWh/Total_Electricity,
         Solar_Share = Elect_Solar_TWh/Total_Electricity, Wind_Share = Elect_Wind_TWh/Total_Electricity,
         RENE_Share = Hydro_Share+Solar_Share+OtherRENE_Share+Wind_Share,
         RENE_Wind_Share = Wind_Share/RENE_Share, RENE_Solar_Share = Solar_Share/RENE_Share,
         RENE_Hydro_Share = Hydro_Share/RENE_Share, RENE_Other_Share = OtherRENE_Share/RENE_Share)
summary(Electricity_mix_data)
table(Electricity_mix_data$Year)

Electricity_mix_share = melt(Electricity_mix_data %>% 
                               select(Country, Year, Coal_Share, Gas_Share,Hydro_Share,Nuclear_Share,
                                      Oil_Share, OtherRENE_Share, Solar_Share, Wind_Share, RENE_Share),id.vars = c('Country','Year'))
names(Electricity_mix_share)[3:4] = c('Energy_Type','Percent_Share')

Countries_final = c('Argentina','Australia','Belgium','Brazil','Canada','China','Egypt','France','Germany',
                    'India','Indonesia','Iran','Italy','Japan','Malaysia','Mexico',
                    'Netherlands','Poland','Russia','Spain','Thailand',
                    'Turkey','United Kingdom','United States','Venezuela')

Electricity_mix_share$Energy_Type = factor(Electricity_mix_share$Energy_Type,
                                           levels = c('Coal_Share', 'Gas_Share','Oil_Share','Nuclear_Share','RENE_Share','Hydro_Share',
                                                      'OtherRENE_Share', 'Solar_Share', 'Wind_Share'))


Fossil = c('Coal_Share', 'Gas_Share','Oil_Share','Nuclear_Share') 
Renewables = c('RENE_Share','Hydro_Share','OtherRENE_Share', 'Solar_Share', 'Wind_Share')

Electricity_mix_share$Country = factor(Electricity_mix_share$Country,
                                       levels = c('France','Brazil','Canada','Belgium','Venezuela','United Kingdom','Spain',
                                                  'Germany','Netherlands', 'Argentina','United States','Japan','Italy','Russia','Turkey','Mexico','Egypt','Thailand',
                                                  'Iran','Malaysia','China','Australia','India','Indonesia','Poland', 'Bangladesh','South Korea',
                                                  'Pakistan','Ukraine'))

###2.3.1 Electricity mix with sources--------------------

ggplot(Electricity_mix_share %>% filter(Country %in% Countries_final, Energy_Type %in% c(Fossil,'RENE_Share')), aes(x = Year)) + 
  geom_bar(aes(y = Percent_Share*100, fill = Energy_Type), stat = 'identity', position = 'stack') +
  facet_wrap(~Country, ncol = 5) + ggtitle('Relative energy sources in Electricity mix') +
  labs(fill = 'Energy type') + ylab('Percentage Share (%)') +
  scale_fill_manual(labels = c('Coal','Gas','Oil','Nuclear','Renewables'), 
                    values = c('grey60','orangered4','tan2','lightgreen','dodgerblue3')) +
  theme(panel.background = element_rect(fill = "white", colour = "black", size = 0.3),
        panel.spacing = unit(0,'lines')) + scale_x_continuous(breaks = seq(1989,2019,6)) +
  th + 
  theme(axis.text.x = element_text(size = 9, angle = 90, hjust = 0.5, vjust = 0.5), axis.title.x = element_text(size = 12, face = 'bold'), 
        axis.text.y.left =  element_text(size = 11), axis.title.y.left = element_text(size = 12, face = 'bold'), 
        axis.text.y.right = element_text(size = 11,color = 'darkgreen'), 
        axis.title.y.right = element_text(size = 12, face = 'bold', color = 'darkgreen'), 
        legend.title = element_text(size = 12, face = 'bold'),
        legend.text = element_text(size = 12, face = 'bold'), legend.position = 'bottom', strip.text = element_text(size = 10, face = 'bold'),
        plot.title = element_text(size = 14, face = 'bold'), panel.spacing.x = unit(0.2,'lines'),panel.spacing.y = unit(0,'lines'))

ggsave('Electricity_mix_with_sources.jpg', height = 8.5, width = 8, units = 'in', dpi = 800)

###2.3.2 Relative share of renewables by sources-------------------------------------

Renewable_Electricity = melt(Electricity_mix_data %>% 
                               select(Country, Year, RENE_Hydro_Share, 
                                      RENE_Solar_Share,RENE_Other_Share, RENE_Wind_Share),id.vars = c('Country','Year'))

names(Renewable_Electricity)[3:4] = c('RENE_Type','Percent_Share')
Renewable_Electricity = inner_join(Renewable_Electricity, Electricity_mix_data %>% select(Country, Year, RENE_Share))
names(Renewable_Electricity)

Renewable_Electricity$Country = factor(Renewable_Electricity$Country,
                                       levels = c('France','Brazil','Canada','Belgium','Venezuela','United Kingdom','Spain',
                                                  'Germany','Netherlands', 'Argentina','United States','Japan','Italy','Russia','Turkey','Mexico','Egypt','Thailand',
                                                  'Iran','Malaysia','China','Australia','India','Indonesia','Poland', 'Bangladesh','South Korea',
                                                  'Pakistan','Ukraine'))

ggplot(na.omit(Renewable_Electricity) %>% filter(Country %in% Countries_final), aes(x = Year)) + 
  geom_bar(aes(y = Percent_Share*100, fill = RENE_Type), stat = 'identity', position = 'stack', alpha = 0.9) +
  geom_line(aes(y = RENE_Share*100), color = 'darkgreen', size = 1) +
  facet_wrap(~Country, ncol = 5) + ggtitle('Relative Renewable energy Sources in Electricity mix') +
  labs(fill = 'Renewable Energy type') + 
  th +
  scale_y_continuous(sec.axis = sec_axis(~.*1, name = 'Renewable energy share in Electricity (%)'), 
                     name = 'Relative percent of Renewable energy by sources (%)') +
  scale_fill_manual(labels = c('Hydropower','Solar','Geothermal/Biomass','Wind'), 
                    values = c('dodgerblue3','goldenrod2','orangered4','grey60')) +
  theme(panel.background = element_rect(fill = "white", colour = "black", size = 0.3),
        panel.grid.major = element_line(size = 0.05, linetype = 'dashed', colour = "grey"), 
        panel.grid.minor = element_line(size = 0.05, linetype = 'dashed',colour = "grey"),
        panel.spacing = unit(0,'lines')) + scale_x_continuous(breaks = seq(1989,2019,6)) +
  theme(axis.text.x = element_text(size = 9, angle = 90, hjust = 0.5, vjust = 0.5), axis.title.x = element_text(size = 12, face = 'bold'), 
        axis.text.y.left =  element_text(size = 11), axis.title.y.left = element_text(size = 12, face = 'bold'), 
        axis.text.y.right = element_text(size = 11,color = 'darkgreen'), 
        axis.title.y.right = element_text(size = 12, face = 'bold', color = 'darkgreen'), 
        legend.title = element_text(size = 12, face = 'bold'),
        legend.text = element_text(size = 12, face = 'bold'), legend.position = 'bottom', strip.text = element_text(size = 12, face = 'bold'),
        plot.title = element_text(size = 14, face = 'bold'), panel.spacing.x = unit(0.2,'lines'),panel.spacing.y = unit(0,'lines'))


#3. Separate leakage of methane for different countries---------------------------
IEA_methane_IEA <- read_csv("~/polybox/06_TUM Project/Octoberfest work/Manuscript/IEA-methane-comparison-data-source=IEA.csv")
#' Data obtained from global methane tracker: https://www.iea.org/reports/global-methane-tracker-2022

table(IEA_methane_IEA$country)
Countries_final
Countries_final_UC = toupper(Countries_final)
table(Electricity_mix_data$Country)

medianFunc = function(x,i){median(x[i])}
meanFunc = function(x,i){mean(x[i])}
madFunc = function(x,i){mad(x[i], center = mean(x), constant = 1)}
sdFunc = function(x,i){sd(x[i])}
q05Func = function(x,i){quantile(x[i], probs = 0.025, na.rm = TRUE)}
q95Func = function(x,i){quantile(x[i], probs = 0.975, na.rm = TRUE)}

q10Func = function(x,i){quantile(x[i], probs = 0.10, na.rm = TRUE)}
q90Func = function(x,i){quantile(x[i], probs = 0.9, na.rm = TRUE)}
## 3.1 Calculating methane leakage distribution plot------------
names(IEA_methane_IEA)
IEA_methane_IEA_leakage = IEA_methane_IEA %>% mutate(Country =  stringr::str_to_title(country)) %>% 
  select(Country,emissions, source, type, segment,reason, baseYear) 
table(IEA_methane_IEA_leakage$Country)

IEA_methane_IEA_leakage = IEA_methane_IEA_leakage %>% filter(reason %in% c('Fugitive','Vented'))

NG_Consumption_2019 <- read_excel("~/polybox/06_TUM Project/Octoberfest work/Manuscript/All Datasets/Data/NG_Consumption_2019.xlsx",
                                  col_types = c("text", "numeric"), sheet = 'Consumption')

table(NG_Consumption_2019$Country)
NG_Production_2019 <- read_excel("~/polybox/06_TUM Project/Octoberfest work/Manuscript/All Datasets/Data/NG_Consumption_2019.xlsx",
                                 col_types = c("text", "numeric"), sheet = 'Production')

table(NG_Production_2019$Country)

IEA_methane_IEA_leakage = IEA_methane_IEA_leakage %>% inner_join(., NG_Consumption_2019, by = 'Country')
names(IEA_methane_IEA_leakage)
length(unique(IEA_methane_IEA_leakage$Country))
table(IEA_methane_IEA_leakage$Country)
summary(IEA_methane_IEA_leakage)

IEA_methane_IEA_leakage = IEA_methane_IEA_leakage %>% left_join(., NG_Production_2019, by = 'Country')

IEA_methane_IEA_leakage = IEA_methane_IEA_leakage %>% 
  mutate(Consumption_ktNG = 18552.8757*NG_EJ_2019_Consumption, Production_ktNG = 18552.8757*NG_EJ_2019_Production) %>% 
  mutate(NG_leakage = 100*emissions/Consumption_ktNG)
summary(IEA_methane_IEA_leakage)
table(IEA_methane_IEA_leakage$Country)

IEA_methane_IEA_leakage %>% ggplot(., aes(x = Country, y = NG_leakage, fill = segment, color = reason)) +
  geom_bar(stat = 'identity', alpha = 0.7) + scale_fill_brewer(palette = 'Dark2') + th +
  scale_color_aaas() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1,  hjust = 1), 
        legend.position = 'bottom', legend.background = element_rect(fill = 'transparent')) +
  labs(y = 'Methane leakage (%)', color = '') + geom_hline(yintercept = c(1.2, 1.7,2.2), lty = 'dashed', size = 0.4, alpha = 0.8) +
  ggtitle('Methane leakage from combined fugitive and vented')

IEA_methane_IEA_leakage_Agg = IEA_methane_IEA_leakage %>% group_by(Country) %>% 
  summarise(NG = sum(NG_leakage, na.rm = TRUE), Prod_NG = mean(NG_EJ_2019_Production, na.rm =T), Cons_NG = mean(NG_EJ_2019_Consumption, na.rm = TRUE))

IEA_methane_IEA_leakage_Agg = IEA_methane_IEA_leakage_Agg %>% filter(NG<10)

##3.2 Plotting methane leakage by countries---------------------------
p = IEA_methane_IEA_leakage_Agg %>% filter(NG<10) %>% 
  mutate(C = forcats::fct_reorder(Country, NG)) %>% 
  ggplot(., aes(x = C, y = NG)) +
  geom_bar(stat = 'identity', alpha = 0.7) + th +
  scale_fill_distiller(palette = 'Spectral') + geom_hline(yintercept = 1.7, lty = 'dashed', color = 'dodgerblue') +
  theme(axis.text.x = element_text(angle = 45, vjust = 1,  hjust = 1), 
        legend.position = 'bottom', legend.background = element_rect(fill = 'transparent')) +
  labs(y = 'Methane leakage (%)', color = '') + theme(axis.title.x = element_blank()) +
  geom_text(label = '(a)', x = 2, y = 8.8, size =5)
p

## 3.3 Plotting the density of methane leakage----------------
r = ggplot() + geom_density(IEA_methane_IEA_leakage_Agg, mapping = aes(x = NG), alpha = 0.3, size = 1.3) + th +
  geom_label(label = '(c)', x = 0, y = 0, size = 5) +
  geom_rect(aes(xmin = mean(boot(IEA_methane_IEA_leakage_Agg$NG,function(x,i){quantile(x[i], probs = 0.025, na.rm = TRUE)},1000)$t), 
                xmax = mean(boot(IEA_methane_IEA_leakage_Agg$NG,function(x,i){quantile(x[i], probs = 0.975, na.rm = TRUE)},1000)$t), 
                ymin = 0, ymax = Inf, fill = '95% C.I'),alpha = 0.2, size = 0) + 
  geom_rect(aes(xmin = mean(boot(IEA_methane_IEA_leakage_Agg$NG,function(x,i){quantile(x[i], probs = 0.05, na.rm = TRUE)},1000)$t), 
                xmax = mean(boot(IEA_methane_IEA_leakage_Agg$NG,function(x,i){quantile(x[i], probs = 0.95, na.rm = TRUE)},1000)$t), 
                ymin = 0, ymax = Inf, fill = '90% C.I'),alpha = 0.2, size = 0) + 
  geom_rect(aes(xmin = mean(boot(IEA_methane_IEA_leakage_Agg$NG,function(x,i){quantile(x[i], probs = 0.1, na.rm = TRUE)},1000)$t), 
                xmax = mean(boot(IEA_methane_IEA_leakage_Agg$NG,function(x,i){quantile(x[i], probs = 0.9, na.rm = TRUE)},1000)$t), 
                ymin = 0, ymax = Inf, fill = '80% C.I'),alpha = 0.2, size = 0) + 
  # ggnewscale::new_scale_color() +
  geom_vline(aes(xintercept = mean(boot(IEA_methane_IEA_leakage_Agg$NG,meanFunc,1000)$t), 
                 color = 'Mean', lty = 'Mean'), alpha = 0.8, size = 1) +
  geom_vline(aes(xintercept = mean(boot(IEA_methane_IEA_leakage_Agg$NG,medianFunc,1000)$t), 
                 color = 'Median', lty = 'Median'), alpha = 0.8, size = 1) +
  # scale_color_uchicago() +
  scale_fill_brewer('', palette = 'Dark2') + scale_color_brewer('',palette = 'Dark2') +
  labs(x = 'Methane Leakage rate (%)', y = 'Kernel density', color = '', lty = '', fill = '') +
  # scale_fill_manual(values = c('brown', 'orange', '')) + 
  geom_hline(yintercept = 0, size = 0.2) +
  theme(legend.background = element_rect(color = NA, fill = 'transparent')) +
  theme(legend.key = element_rect(colour = NA, fill = NA),
        legend.box.background = element_blank(), legend.position = c(0.8, 0.72)) 
r

Leakage_05Q = mean(boot(IEA_methane_IEA_leakage_Agg$NG,function(x,i){quantile(x[i], probs = 0.05, na.rm = TRUE)},1000)$t)
Leakage_95Q = mean(boot(IEA_methane_IEA_leakage_Agg$NG,function(x,i){quantile(x[i], probs = 0.95, na.rm = TRUE)},1000)$t)
Leakage_50Q = mean(boot(IEA_methane_IEA_leakage_Agg$NG,function(x,i){quantile(x[i], probs = 0.50, na.rm = TRUE)},1000)$t)
Leakage_mean = mean(boot(IEA_methane_IEA_leakage_Agg$NG,function(x,i){mean(x[i], na.rm = TRUE)},1000)$t)


library(patchwork)

t = p + r
t
ggsave('Methane_leakage_details.jpg',t,height = 8, width =12, units = 'in', dpi = 500)


#4. Electricity vs NG----------

leakage_global_lower = mean(boot(IEA_methane_IEA_leakage_Agg$NG,function(x,i){quantile(x[i], probs = 0.05, na.rm = TRUE)},1000)$t)/100
leakage_global_upper = mean(boot(IEA_methane_IEA_leakage_Agg$NG,function(x,i){quantile(x[i], probs = 0.95, na.rm = TRUE)},1000)$t)/100
leakage_global = mean(boot(IEA_methane_IEA_leakage_Agg$NG,meanFunc,1000)$t)/100

Electricity_mix_data = Electricity_mix_data %>%  mutate(NG_leak = leakage_global) %>% 
  mutate(NG_leak_upper = leakage_global_upper,NG_leak_lower = leakage_global_lower)

summary(Electricity_mix_data)

##4.1 Calculating emissions from all countries--------------
Electricity_mix_data1 = Electricity_mix_data %>% 
  mutate(NG_emissions_20 = CH4_m3*((1-NG_leak)*CF_methane*EF_CH4 + NG_leak*CH4_GWP20*Density_CH4*0.001),
         NG_emissions_20_lower = CH4_m3*((1-NG_leak_lower)*CF_methane*EF_CH4 + 
                                           NG_leak_lower*CH4_GWP20*Density_CH4*0.001),
         NG_emissions_20_upper = CH4_m3*((1-NG_leak_upper)*CF_methane*EF_CH4 + 
                                           NG_leak_upper*CH4_GWP20*Density_CH4*0.001),
         NG_emissions_100 = CH4_m3*((1-NG_leak)*CF_methane*EF_CH4 + NG_leak*CH4_GWP100*Density_CH4*0.001),
         NG_emissions_no_leak = CH4_m3*((1-0)*CF_methane*EF_CH4 + 0*CH4_GWP100*Density_CH4*0.001),
         RENE_EF = (Wind_EF*RENE_Wind_Share + Solar_EF*RENE_Solar_Share + Hydro_EF*RENE_Hydro_Share + GeoBio_EF*RENE_Other_Share),
         RENE_emissions = Et*RENE_Share*RENE_EF,
         Nuclear_emissions = Et*Nuclear_Share*Nuclear_EF,
         Electr_emissions = RENE_emissions + Nuclear_emissions + Coal_Share*EF_CO2*Et + Gas_Share*Gas_EF*Et + Oil_Share*Oil_EF*Et,
         Coal_emisions = Coal_Share*EF_CO2*Et, Gas_emissions = Gas_Share*Gas_EF*Et, Oil_emissions = Oil_Share*Oil_EF*Et)
summary(Electricity_mix_data1)

NG_emission_20_DF = data.frame(Country = Countries_final) %>% 
  mutate(NG_20e = 80.97242, NG_20el = 73.18069, NG_20eu = 88.76414) %>% 
  mutate(NG_20e = if_else(Country == 'United States', 90.13915, NG_20e),
         NG_20el = if_else(Country == 'United States', 85.55578, NG_20el),
         NG_20eu = if_else(Country == 'United States', 96.25031, NG_20eu))

NG_emission_20_DF = NG_emission_20_DF %>% slice(rep(1:n(), each = 50)) %>% 
  group_by(Country) %>% mutate(Year = 1989+row_number())
NG_emission_20_DF$Country = factor(NG_emission_20_DF$Country,
                                   levels = c('France','Brazil','Canada','Belgium','Venezuela','United Kingdom','Spain',
                                              'Germany','Netherlands', 'Argentina','United States','Japan','Italy','Russia','Turkey','Mexico','Egypt','Thailand',
                                              'Iran','Malaysia','China','Australia','India','Indonesia','Poland'))

names(IEA_methane_IEA_leakage_Agg)
summary(IEA_methane_IEA_leakage_Agg)
NG_density = density(IEA_methane_IEA_leakage_Agg$NG,n = 500, from = 0, to = 9.4)
NG_density = data.frame(NG_leak = NG_density$x*0.01, density = NG_density$y)
NG_density = NG_density %>% filter(NG_leak < 0.01*Leakage_95Q)
NG_density = NG_density %>% 
  mutate(NG_emissions_20 = CH4_m3*((1-NG_leak)*CF_methane*EF_CH4 + NG_leak*CH4_GWP20*Density_CH4*0.001))
summary(NG_density)

NG_density = replicate(30, NG_density, simplify = FALSE) %>% purrr::imap_dfr(~.x %>% mutate(Year = .y + 1989))


NG_density = NG_density %>% slice(rep(1:n(), each = 50)) %>% 
  mutate(Year = 1989+row_number())


NG_density %>% ggplot(., aes(x = NG_leak, y = density)) + geom_line() + th
summary(NG_density)
str(NG_density)
pal <- wesanderson::wes_palette("Zissou1", 100, type = "continuous")

names(Electricity_mix_data1)

Electricity_mix_data1 = Electricity_mix_data1 %>% mutate(Groups = if_else(Code %in% c('BRA','CAN','TUR','ARG','VEN'), 'Group1',
                                                                          if_else(Code %in% c('DEU','ITA','GBR','ESP','USA'), 'Group2', 
                                                                                  if_else(Code %in% c('AUS','BEL','CHN','FRA','JPN',
                                                                                                      'MYS','MEX','NLD','RUS'), 'Group3','Group4'))))
table(Electricity_mix_data1$Country)

Electricity_mix_data1$Country = factor(Electricity_mix_data1$Country,
                                       levels = Electricity_mix_data_2019$Country)

names(Electricity_mix_data1)

##4.2 Plotting Electricity vs NG emissions factors------------------
p = Electricity_mix_data1 %>% filter(Country %in% Countries_final) %>% ggplot(., aes(x = Year)) + 
  geom_tile(data = NG_density, mapping = aes(x = Year, y = NG_emissions_20, fill = density), alpha = 0.3) +
  scico::scale_fill_scico(palette = "bilbao", alpha = 0.4) + # the default
  geom_line(aes(y = Electr_emissions, color = 'Electricity'), size = 1.2, lty = 'solid') + 
  facet_wrap(~Country) +
  th +
  scale_x_continuous(breaks = seq(2000,2020,5), limits = c(2000, 2020) ) +
  scale_y_continuous(breaks = seq(0,200,50), name = expression('Emission Factor (tCO'[2]*'eq/TJ)')) +
  scale_color_manual('', values = 'darkgreen') +
  scale_linetype_manual('', values = c('dashed')) +
  theme(legend.position = 'bottom',legend.key.width = unit(1,'cm'),
        axis.text.x = element_text(size = 13.5, angle = 60, hjust = 1, vjust = 1),
        axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 14.5))
p
ggsave('Electricity vs Methane1.jpg',p, height = 10.5, width =11, units = 'in', dpi = 600)


quantile(IEA_methane_IEA_leakage$NG_leakage, probs = 0.998, na.rm = TRUE)


mean(boot(Tair_15ma,meanFunc,100)$t)
mean(boot(Tair_15ma,meanFunc,100)$t)


Methane_emissions_Countries = IEA_methane_IEA %>% 
  filter(country %in% c(Countries_final, Countries_final_UC))   ## Spain, turkey and belgium missing
table(Methane_emissions_Countries$country)
Methane_emissions_Countries = Methane_emissions_Countries %>% mutate(Country =  stringr::str_to_title(country))
table(Methane_emissions_Countries$Country)
names(Methane_emissions_Countries)
Methane_emissions_Countries = Methane_emissions_Countries %>% 
  select(Country,emissions, source, type, segment,reason, baseYear)

Methane_emissions_Countries = Methane_emissions_Countries %>% group_by(Country) %>% 
  mutate(Total_emissions = sum(emissions, na.rm = TRUE)) %>% ungroup() %>% group_by(Country, reason) %>% 
  mutate(Reason_emissions = sum(emissions, na.rm = TRUE))

# 5. Phase transistion plot for all countries------------------

Electricity_mix_data = 
  Electricity_mix_data %>% 
  mutate(Low_Carbon_EF = RENE_EF+Nuclear_emissions/(RENE_Share + Nuclear_Share),
         Fossil_EF = (Coal_emisions+Gas_emissions+Oil_emissions+Nuclear_emissions)/(1-RENE_Share),
         HighC_EF = (Coal_emisions+Gas_emissions+Oil_emissions)/(1-RENE_Share-Nuclear_Share))


summary(Electricity_mix_data)
Leakage_rate_range = seq(0,0.15,0.001) ## Varying from 0 to 15%
RENE_range = seq(0,100,1)  ## Share of Rene from 0 to 100%
RENE_EF_DF = Electricity_mix_data1 %>% filter(Year == 2019, Country %in% Countries_final) %>% 
  select(Country, RENE_EF, NG_leak, Low_Carbon_EF, Gas_Share, Oil_Share, Coal_Share, Nuclear_Share, Fossil_EF, HighC_EF, RENE_Share) %>% 
  arrange(Country)

RENE_EF_DF$Country = factor(RENE_EF_DF$Country,
                            levels = Electricity_mix_data_2019$Country)

table(RENE_EF_DF$Country)

EF_RENE = double()
EF_LowC = double()   ## Lowcarbon source (Renewables + Nuclear)
EF_highC = double()
LR = double()
Nation = character()
Desired_RENE_country = double()
Min_RENE_country = double()
Upper_RENE_country = double()
Lower_RENE_country = double()
Leak_min = double()
Country_name = character()
p = list()
for (t in c(1:25)) 
{
  k = 1
  EF_RENE = RENE_EF_DF$RENE_EF[t]
  EF_LowC = RENE_EF_DF$Low_Carbon_EF[t]   ## Lowcarbon source (Renewables + Nuclear)
  LR = RENE_EF_DF$NG_leak[t]
  Nation = as.character(RENE_EF_DF$Country[t])
  NG20 = double(); Leak = double(); Rene = double(); Coal = double(); Gas = double(); Oil = double();
  Nuclear = double(); Fos_EF = double(); 
  Electr_RENE = double(); Electr_LowC = double();
  Coal = RENE_EF_DF$Coal_Share[t]
  Oil = RENE_EF_DF$Oil_Share[t]
  Gas = RENE_EF_DF$Gas_Share[t]
  Fos_EF = RENE_EF_DF$Fossil_EF[t]
  HighC_EF = RENE_EF_DF$HighC_EF[t]
  LowC_EF = RENE_EF_DF$Low_Carbon_EF[t]
  Nuclear = RENE_EF_DF$Nuclear_Share[t]
  for (i in c(1:length(Leakage_rate_range)))
  {
    for (j in c(1:length(RENE_range))) 
    {
      Leak[k] = Leakage_rate_range[i]
      Rene[k] = RENE_range[j]
      NG20[k] = CH4_m3*((1-Leakage_rate_range[i])*CF_methane*EF_CH4 + Leakage_rate_range[i]*CH4_GWP20*Density_CH4*0.001)
      Electr_RENE[k] = 0.01*RENE_range[j]*EF_RENE + 0.01*(100-RENE_range[j])*Fos_EF
      k = k+1
    }
  }
  
  RENE_leakage_DF = data.frame(NG_20 = NG20, LeakCH4 = Leak, RE_percent = Rene, ElectCO2 = Electr_RENE, Difference = -Electr_RENE+NG20)
  Desired_RENE = min(RENE_leakage_DF$RE_percent[RENE_leakage_DF$Difference >= 0 & RENE_leakage_DF$LeakCH4 == 0])
  Min_RENE = min(RENE_leakage_DF$RE_percent[RENE_leakage_DF$Difference >= 0 & RENE_leakage_DF$LeakCH4 < LR])
  Upper_RENE = min(RENE_leakage_DF$RE_percent[RENE_leakage_DF$Difference >= 0 & RENE_leakage_DF$LeakCH4 < Leakage_95Q*0.01])
  Lower_RENE = min(RENE_leakage_DF$RE_percent[RENE_leakage_DF$Difference >= 0 & RENE_leakage_DF$LeakCH4 < Leakage_05Q*0.01])
  Min_leak = min(RENE_leakage_DF$LeakCH4[RENE_leakage_DF$RE_percent == 0 & RENE_leakage_DF$Difference > 0])
  Desired_RENE_country[t] = Desired_RENE
  Min_RENE_country[t] = Min_RENE
  Upper_RENE_country[t] = Upper_RENE
  Lower_RENE_country[t] = Lower_RENE
  Leak_min[t] = Min_leak
  Country_name[t] = Nation
  
  ## Plotting for GWP of 20 years
  p[[t]] = ggplot()  + 
    geom_tile(data = RENE_leakage_DF %>% filter(RE_percent < 101), 
              mapping = aes(x = LeakCH4*100, y = RE_percent, fill = -ElectCO2+NG_20)) + 
    theme_bw() + labs(x = NULL, y = NULL) + ggtitle(Nation) +
    scale_fill_gradient2(high = 'blue',low = 'red3', n.breaks = 6, midpoint = 0) +
    scale_y_continuous('', breaks = seq(0,100,20)) + 
    th + theme(legend.position = c(0.8,0.60), legend.title = element_blank(), legend.direction = 'vertical') +
    theme(panel.background = element_rect(fill = "white", colour = "black", size = 1),
          legend.background = element_rect(fill = 'transparent'),
          plot.title = element_text(size = 13, face = 'bold'),
          legend.text = element_text(color = 'black', size = 8), legend.key.width = unit(0.3,"cm"), legend.key.height = unit(0.5,"cm")) +
    # geom_vline(xintercept = LR*100, linetype = 'dashed', color = 'red', size = 1) + 
    # geom_rect(aes(xmin = leakage_global_lower*100, xmax = leakage_global_upper*100, 
    #               ymin = 0, ymax = 100),alpha = 0.2, size = 0, fill = 'red') + 
    geom_vline(xintercept = c(Leakage_mean), linetype = 'dashed', color = 'red', size = 1) +
    # geom_hline(yintercept = Desired_RENE, linetype = 'longdash', color = 'darkgreen') +
    geom_hline(yintercept = Min_RENE, linetype = 'longdash', color = 'darkorange', size = 1) +
    geom_hline(yintercept = 100*RENE_EF_DF$RENE_Share[t], linetype = 'dashed', color = 'darkgreen',size = 0.8)
  
  
  print(t)
  
}

Desired_RENE_DF = data.frame(Country = Country_name, Desired_RENE = Desired_RENE_country, Min_RENE = Min_RENE_country,
                             Min_Leakage = Leak_min, Upper_RENE = Upper_RENE_country, Lower_RENE = Lower_RENE_country)
Desired_RENE_DF = inner_join(Desired_RENE_DF, RENE_EF_DF, by = 'Country')
Desired_RENE_DF = Desired_RENE_DF %>% mutate(Desired_Diff = Desired_RENE - 100*RENE_Share,
                                             Minimum_Diff = Min_RENE - 100*RENE_Share,
                                             Upper_Diff = Upper_RENE - 100*RENE_Share,
                                             Lower_Diff = Lower_RENE - 100*RENE_Share)




summary(Desired_RENE_DF)

args <- c(p, list(nrow = 5,ncol = 5, left = 'RENE share (%)', bottom = "Methane leakage (%)")) 
x = do.call(grid.arrange,args)
summary(Desired_RENE_DF)
ggsave('FigureS5_Phase_Transition_plot_all_countries_Updated_try.jpg',x, height = 10, width = 11, units = 'in', dpi = 500)


Desired_RENE_DF$Country = factor(Desired_RENE_DF$Country,
                                 levels = c('Belgium','Brazil','Canada','France','Spain','United Kingdom','Venezuela','Argentina',
                                            'Germany','Italy','United States','Russia','Australia','China','Iran','Japan','Mexico',
                                            'Netherlands','Poland','Thailand','Turkey','Egypt','India','Indonesia','Malaysia'))
Desired_RENE_DF$Country = reorder(Desired_RENE_DF$Country, Desired_RENE_DF$Minimum_Diff)

ggplot(melt(Desired_RENE_DF %>% select(Country, Desired_Diff, Minimum_Diff), id.vars = c('Country')), aes(x = Country)) + 
  geom_bar(aes(y = value, fill = variable), stat="identity", position = position_dodge(width = 0.5)) +
  theme_tufte() + labs(fill = '') + scale_fill_lancet(labels = c('Desired Difference','Minimum Difference')) + 
  xlab('') + ylab('RENE share (%)') +
  theme(panel.background = element_rect(fill = "white", colour = "black", size = 1),
        panel.grid.major = element_line(size = 0.2, linetype = 'dashed', colour = "grey"), 
        panel.grid.minor = element_line(size = 0.2, linetype = 'dashed',colour = "grey"),
        panel.spacing = unit(0,'lines')) + scale_y_continuous(breaks = seq(-30,60,10)) + 
  theme(axis.text.y = element_text(size = 11), axis.title = element_text(size = 11, face = 'bold'), legend.title = element_blank(), 
        axis.text.x = element_text(size = 11, angle = 45, hjust = 1, vjust = 1, face = 'bold'), legend.text = element_text(size = 11, face = 'bold'),
        text = element_text(size = 9), legend.position = 'bottom', strip.text = element_text(size = 9.5, face = 'bold'),
        plot.title = element_text(size = 11, face = 'bold'), panel.spacing.x = unit(0.2,'lines'), panel.spacing.y = unit(0,'lines'))

ggsave('Desired and Min RENE Share required1.png', height = 4, width = 6, units = 'in', dpi = 800)


##5.1  Sensitivity analysis of leakage rate wrt to RENE share for different countries ######
names(Desired_RENE_DF)
Desired_RENE_DF = Desired_RENE_DF %>% mutate(RENE_per_leakage = Desired_RENE/(Min_Leakage*100))

ggplot(melt(Desired_RENE_DF %>% select(Country, Desired_Diff, Minimum_Diff, RENE_per_leakage), 
            id.vars = c('Country','RENE_per_leakage')), aes(x = Country)) + 
  geom_bar(aes(y = value, fill = variable), stat="identity", position = position_dodge(width = 0.75), alpha = 0.70, color = 'black') +
  geom_errorbar(aes(y = value, ymin = value - RENE_per_leakage, ymax = value + RENE_per_leakage, color = variable),
                position = position_dodge(width = 0.75), size = 1, width = 0.5, show.legend = FALSE) +
  labs(fill = '') + scale_fill_lancet(labels = c('Desired Difference','Minimum Difference')) + 
  th +
  scale_color_lancet(labels = c('Desired Difference','Minimum Difference')) +
  xlab('') + ylab('RENE share (%)') + 
  ggtitle('Difference of present RENE share with Desired and Minimum RENE') +
  theme(panel.background = element_rect(fill = "white", colour = "black", size = 1),
        panel.grid.major = element_line(size = 0.2, linetype = 'dashed', colour = "grey"), 
        panel.grid.minor = element_line(size = 0.2, linetype = 'dashed',colour = "grey"),
        panel.spacing = unit(0,'lines')) + scale_y_continuous(breaks = seq(-30,60,10)) + 
  theme(axis.text.y = element_text(size = 12), axis.title = element_text(size = 12, face = 'bold'), legend.title = element_blank(), 
        axis.text.x = element_text(size = 12, angle = 45, hjust = 1, vjust = 1, face = 'bold', color = 'black'), 
        legend.text = element_text(size = 12, face = 'bold'),
        text = element_text(size = 9), legend.position = c(0.15,0.9), strip.text = element_text(size = 9.5, face = 'bold'),
        plot.title = element_text(size = 12, face = 'bold'), panel.spacing.x = unit(0.2,'lines'), panel.spacing.y = unit(0,'lines'))

ggsave('Desired and Min RENE Share required_with_error.png', height = 4, width = 8, units = 'in', dpi = 900)



##5.2 Updated Figure 8 with 90% CI----------
names(Desired_RENE_DF)
summary(Desired_RENE_DF)

ggplot(Desired_RENE_DF %>% select(Country, Minimum_Diff, Upper_Diff, Lower_Diff) %>% 
         mutate(C = forcats::fct_reorder(Country, Minimum_Diff)), aes(x = C)) + 
  geom_bar(aes(y = Minimum_Diff), stat="identity", alpha = 0.70, color = 'black', fill = 'dodgerblue3') +
  geom_errorbar(aes(y = Minimum_Diff, ymin = Upper_Diff, ymax = Lower_Diff),
                size = 0.7, width = 0.35, show.legend = FALSE) +
  th +
  xlab('') + ylab('∆RENE share (%)') + th +
  ggtitle('Difference of present RENE share and Minimum RENE share') +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1, vjust = 1, color = 'black', face = 'bold'),
        axis.title = element_text(size = 13, face = 'bold')) +
  theme(panel.background = element_rect(fill = "white", colour = "black", size = 1),
        panel.grid.major = element_line(size = 0.2, linetype = 'dashed', colour = "grey"), 
        panel.grid.minor = element_line(size = 0.2, linetype = 'dashed',colour = "grey"),
        panel.spacing = unit(0,'lines'))
ggsave('Figure8_RENEshare_difference_countries_updated.jpg', height = 3.7, width = 6.6, units = 'in', dpi = 900)
