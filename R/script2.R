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
ggplot(data = data, aes(x = log10(adult_mass_g), y = max_longevity_d)) + 
  geom_point(aes(color = hab_simple)) +
    theme(legend.title = element_blank()) +
    labs(x = "Log Adult mass (g)", y = "Longevity (d)")  

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
ggplot(data = data, aes(x = log10(adult_mass_g), y = maturity_d)) + 
  geom_point(aes(color = hab_simple)) +
  theme(legend.title = element_blank()) +
  labs(x = "Log Adult mass (g)", y = "Log Maturity (d)")

ggplot(data = data, aes(x = log10(adult_mass_g), y = maturity_d)) + 
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
ggplot(data = data, aes(x = log10(adult_mass_g), y = age_first_reproduction_d)) + 
  geom_point(aes(color = hab_simple)) +
  theme(legend.title = element_blank()) +
  labs(x = "Log Adult mass (g)", y = "Age first reproduction (d)")

ggplot(data = data, aes(x = log10(adult_mass_g), y = age_first_reproduction_d)) + 
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
ggplot(data = data, aes(x = log10(adult_mass_g), y = gestation_length_d)) + 
  geom_point(aes(color = hab_simple)) +
  theme(legend.title = element_blank()) +
  labs(x = "Log Adult mass (g)", y = "Gestation length (d)")

ggplot(data = data, aes(x = log10(adult_mass_g), y = gestation_length_d)) + 
  geom_point(aes(color = hab_complex)) +
  theme(legend.title = element_blank()) +
  labs(x = "Log Adult mass (g)", y = "Gestation length (d)")

# TEAT NUMBER
ggplot(data = data, aes(x = log10(adult_mass_g), y = teat_number_n)) + 
  geom_point(aes(color = hab_simple)) +
  theme(legend.title = element_blank()) +
  labs(x = "Log Adult mass (g)", y = "Teat number (n)")

ggplot(data = data, aes(x = log10(adult_mass_g), y = teat_number_n)) + 
  geom_point(aes(color = hab_complex)) +
  theme(legend.title = element_blank()) +
  labs(x = "Log Adult mass (g)", y = "Teat number (n)")

# LITTER SIZE
ggplot(data = data, aes(x = log10(adult_mass_g), y = litter_size_n)) + 
  geom_point(aes(color = hab_simple)) +
  theme(legend.title = element_blank()) +
  labs(x = "Log Adult mass (g)", y = "Litter size (n)")

ggplot(data = data, aes(x = log10(adult_mass_g), y = litter_size_n)) + 
  geom_point(aes(color = hab_complex)) +
  theme(legend.title = element_blank()) +
  labs(x = "Log Adult mass (g)", y = "Litter size (n)")

# LITTERS PER YEAR
ggplot(data = data, aes(x = log10(adult_mass_g), y = data$litters_per_year_n)) + 
  geom_point(aes(color = hab_simple)) +
  theme(legend.title = element_blank()) +
  labs(x = "Log Adult mass (g)", y = "Litters per year (n))")

ggplot(data = data, aes(x = log10(adult_mass_g), y = data$litters_per_year_n)) + 
  geom_point(aes(color = hab_complex)) +
  theme(legend.title = element_blank()) +
  labs(x = "Log Adult mass (g)", y = "Litters per year (n))")

# INTERBIRTH INTERVAL
ggplot(data = data, aes(x = log10(adult_mass_g), y = data$interbirth_interval_d)) + 
  geom_point(aes(color = hab_simple)) +
  theme(legend.title = element_blank()) +
  labs(x = "Log Adult mass (g)", y = "Interbirth interval (d))")

ggplot(data = data, aes(x = log10(adult_mass_g), y = data$interbirth_interval_d)) + 
  geom_point(aes(color = hab_complex)) +
  theme(legend.title = element_blank()) +
  labs(x = "Log Adult mass (g)", y = "Interbirth interval (d))")

# NEONATE MASS
ggplot(data = data, aes(x = log10(adult_mass_g), y = data$neonate_mass_g)) + 
  geom_point(aes(color = hab_simple)) +
  theme(legend.title = element_blank()) +
  labs(x = "Log Adult mass (g)", y = "Neonate mass (g)))")

ggplot(data = data, aes(x = log10(adult_mass_g), y = data$neonate_mass_g)) + 
  geom_point(aes(color = hab_complex)) +
  theme(legend.title = element_blank()) +
  labs(x = "Log Adult mass (g)", y = "Neonate mass (g)")

# WEANING AGE
ggplot(data = data, aes(x = log10(adult_mass_g), y = data$weaning_age_d)) + 
  geom_point(aes(color = hab_simple)) +
  theme(legend.title = element_blank()) +
  labs(x = "Log Adult mass (g)", y = "Weaning age (d))")

ggplot(data = data, aes(x = log10(adult_mass_g), y = data$weaning_age_d)) + 
  geom_point(aes(color = hab_complex)) +
  theme(legend.title = element_blank()) +
  labs(x = "Log Adult mass (g)", y = "Weaning age (d)")

# WEANING MASS
ggplot(data = data, aes(x = log10(adult_mass_g), y = data$weaning_mass_g)) + 
  geom_point(aes(color = hab_simple)) +
  theme(legend.title = element_blank()) +
  labs(x = "Log Adult mass (g)", y = "Weaning mass (g)")

ggplot(data = data, aes(x = log10(adult_mass_g), y = data$weaning_mass_g)) + 
  geom_point(aes(color = hab_complex)) +
  theme(legend.title = element_blank()) +
  labs(x = "Log Adult mass (g)", y = "Weaning mass (g)")

# GENERATION LENGTH
ggplot(data = data, aes(x = log10(adult_mass_g), y = data$generation_length_d)) + 
  geom_point(aes(color = hab_simple)) +
  theme(legend.title = element_blank()) +
  labs(x = "Log Adult mass (g)", y = "Generation length (d)")

ggplot(data = data, aes(x = log10(adult_mass_g), y = data$generation_length_d)) + 
  geom_point(aes(color = hab_complex)) +
  theme(legend.title = element_blank()) +
  labs(x = "Log Adult mass (g)", y = "Generation length (d)")


################ 
