data <- read.csv("C:/master/TFM/TFM-Laura/data/new_data.csv", sep=";")
data


hab_simple <- factor(data$hab_simple)
hab_complex <- factor(data$hab_complex)

data$adult_mass_g
adultmass <- na.omit(data$adult_mass_g)
longevity <- na.omit(data$max_longevity_d)
adultmass_log <- log10(adultmass)
longevity_log <- log10(longevity)


library(ggplot2)
# MAX LONGEVITY
ggplot(data = data, aes(x = adult_mass_g, y = max_longevity_d)) + 
  geom_point(aes(color = hab_simple)) +
  theme(legend.title = element_blank()) +
  labs(x = "Adult mass (g)", y = "Longevity (d)")

ggplot(data = data, aes(x = adult_mass_g, y = max_longevity_d)) + 
  geom_point(aes(color = hab_complex)) +
  theme(legend.title = element_blank()) +
  labs(x = "Adult mass (g)", y = "Longevity (d)")

# log_max longevity 
ggplot(data = data, aes(x = log10(adult_mass_g), y = log10(max_longevity_d))) + 
  geom_point(aes(color = hab_simple)) +
    theme(legend.title = element_blank()) +
    labs(x = "Log Adult mass (g)", y = "Log Longevity (d)")  

ggplot(data = data, aes(x = log10(adult_mass_g), y = log10(max_longevity_d))) + 
  geom_point(aes(color = hab_complex)) +
  theme(legend.title = element_blank()) +
  labs(x = "Log Adult mass (g)", y = "Log Longevity (d)")  

# MATURITY
ggplot(data = data, aes(x = adult_mass_g, y = maturity_d)) + 
  geom_point(aes(color = hab_simple)) +
  theme(legend.title = element_blank()) +
  labs(x = "Adult mass (g)", y = "Maturity (d)")

ggplot(data = data, aes(x = adult_mass_g, y = maturity_d)) + 
  geom_point(aes(color = hab_complex)) +
  theme(legend.title = element_blank()) +
  labs(x = "Adult mass (g)", y = "Maturity (d)")

# log_maturity
ggplot(data = data, aes(x = log10(adult_mass_g), y = log10(maturity_d))) + 
  geom_point(aes(color = hab_simple)) +
  theme(legend.title = element_blank()) +
  labs(x = "Log Adult mass (g)", y = "Log Maturity (d)")

ggplot(data = data, aes(x = log10(adult_mass_g), y = log10(maturity_d))) + 
  geom_point(aes(color = hab_complex)) +
  theme(legend.title = element_blank()) +
  labs(x = "Log Adult mass (g)", y = "Log Maturity (d)")

# AGE FIRT REPRODUCTION
ggplot(data = data, aes(x = adult_mass_g, y = age_first_reproduction_d)) + 
  geom_point(aes(color = hab_simple)) +
  theme(legend.title = element_blank()) +
  labs(x = "Adult mass (g)", y = "Age first reproduction (d)")

ggplot(data = data, aes(x = adult_mass_g, y = age_first_reproduction_d)) + 
  geom_point(aes(color = hab_complex)) +
  theme(legend.title = element_blank()) +
  labs(x = "Adult mass (g)", y = "Age first reproduction (d)")

# log_maturity
ggplot(data = data, aes(x = log10(adult_mass_g), y = log10(age_first_reproduction_d))) + 
  geom_point(aes(color = hab_simple)) +
  theme(legend.title = element_blank()) +
  labs(x = "Log Adult mass (g)", y = "Age first reproduction (d)")

ggplot(data = data, aes(x = log10(adult_mass_g), y = log10(age_first_reproduction_d))) + 
  geom_point(aes(color = hab_complex)) +
  theme(legend.title = element_blank()) +
  labs(x = "Log Adult mass (g)", y = "Age first reproduction (d)")

# GESTATION LENGTH
ggplot(data = data, aes(x = adult_mass_g, y = gestation_length_d)) + 
  geom_point(aes(color = hab_simple)) +
  theme(legend.title = element_blank()) +
  labs(x = "Adult mass (g)", y = "Gestation length (d)")

ggplot(data = data, aes(x = adult_mass_g, y = gestation_length_d)) + 
  geom_point(aes(color = hab_complex)) +
  theme(legend.title = element_blank()) +
  labs(x = "Adult mass (g)", y = "Gestation length (d)")

# log_gestation length
ggplot(data = data, aes(x = log10(adult_mass_g), y = log10(gestation_length_d))) + 
  geom_point(aes(color = hab_simple)) +
  theme(legend.title = element_blank()) +
  labs(x = "Log Adult mass (g)", y = "Gestation length (d)")

ggplot(data = data, aes(x = log10(adult_mass_g), y = log10(gestation_length_d))) + 
  geom_point(aes(color = hab_complex)) +
  theme(legend.title = element_blank()) +
  labs(x = "Log Adult mass (g)", y = "Gestation length (d)")

# TEAT NUMBER
ggplot(data = data, aes(x = adult_mass_g, y = teat_number_n)) + 
  geom_point(aes(color = hab_simple)) +
  theme(legend.title = element_blank()) +
  labs(x = "Adult mass (g)", y = "Teat number (n)")

ggplot(data = data, aes(x = adult_mass_g, y = teat_number_n)) + 
  geom_point(aes(color = hab_complex)) +
  theme(legend.title = element_blank()) +
  labs(x = "Adult mass (g)", y = "Teat number (n)")

# log_teat number
ggplot(data = data, aes(x = log10(adult_mass_g), y = teat_number_n)) + 
  geom_point(aes(color = hab_simple)) +
  theme(legend.title = element_blank()) +
  labs(x = "Log Adult mass (g)", y = "Teat number (n)")

ggplot(data = data, aes(x = log10(adult_mass_g), y = teat_number_n)) + 
  geom_point(aes(color = hab_complex)) +
  theme(legend.title = element_blank()) +
  labs(x = "Log Adult mass (g)", y = "Teat number (n)")



################ 

dataNA <- read.csv("C:/master/TFM/TFM-Laura/data/new_data_missingNA.csv", sep=";")
hab_simpleNA <- factor(dataNA$hab_simple)
hab_complexNA <- factor(dataNA$hab_complex)

ggplot(data = dataNA, aes(x = log10(adult_mass_g), y = log10(gestation_length_d))) + 
  geom_point(aes(color = hab_complexNA)) +
  theme(legend.title = element_blank()) +
  labs(x = "Log Adult mass (g)", y = "Gestation length (d)")