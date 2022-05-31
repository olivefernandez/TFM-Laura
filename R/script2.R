data <- read.csv("C:/master/TFM/TFM-Laura/data/new_data_missingNA.csv", sep=";")

hab_simple <- factor(data$hab_simple)
hab_complex <- factor(data$hab_complex)
# logs
data$Ladult_mass <- log10(data$adult_mass_g**(1/3))
data$Llongev <- log(data$max_longevity_d)
#data$Lreprodlongev <- log10(data$max_longevity_d-data$age_first_reproduction_d)
data$Lmatur <- log10(data$maturity_d)
data$Lfemale_matur <- log10(data$female_maturity_d)
data$Lmale_matur <- log10(data$male_maturity_d)
data$LAFR <- log10(data$age_first_reproduction_d)
data$Lgestation <- log10(data$gestation_length_d)
data$Lteat <- log10(data$teat_number_n)
data$Llitters_year <- log10(data$litters_per_year_n)
data$Llitter_size <- data$litter_size_n
#data$Lfecundity <- log10(data$litter_size_n*data$litters_per_year_n)
data$Lbirth_interv <- log10(data$interbirth_interval_d)
data$Lneonate_mass <- log10(data$neonate_mass_g**(1/3)+1)
data$Lweaning_age <- log10(data$weaning_age_d)
data$Lweaning_mass <- log10(data$weaning_mass_g**(1/3)+1)
data$Lgeneration <- log10(data$generation_length_d)
#data$BV <- log10(1/(data$litters_per_year_n*data$litter_size_n*(data$max_longevity_d-data$age_first_reproduction_d)))
#data$Lproductivity <- log10(data$litters_per_year_n*data$litter_size_n*data$neonate_mass)
#data$habitat_complex <- as.factor(data$hab)
#levels(data$habitat_complex) <- c("Coastal", "Aquatic", "Aquatic", "Pelagic", "Coastal", "Terrestrial")
#data$realm <- data$biogeographical_realm

   #########################
   ######    PLOTS    ###### 
   #########################

library(ggplot2)

#1  _________________________ MAX LONGEVITY _________________________ 

# geom point
ggplot(data = data, aes(x = Ladult_mass, y = Llongev)) + 
  geom_point(aes(color = hab_simple)) +
  theme(legend.title = element_blank()) +
  labs(x = "Log Adult mass (g)", y = "Log Longevity (d)") 

# BOXPLOT -> log (hab_complex)
ggplot(data, aes(x = hab_simple, y = Llongev, fill = hab_simple)) + 
  geom_boxplot(alpha = 0.9) +
  theme(legend.position ="none") +
  labs(x = "Habitat", y = "Log Longevity (d)")


#2  __________________________ MATURITY __________________________

# geom point
ggplot(data = data, aes(x = Ladult_mass, y = Lmatur)) + 
  geom_point(aes(color = hab_simple)) +
  theme(legend.title = element_blank()) +
  labs(x = "Log Adult mass (g)", y = "Log Maturity (d)")

# BOXPLOT -> log (hab_complex) 
ggplot(data, aes(x = hab_simple, y = Lmatur, fill = hab_simple)) + 
  geom_boxplot(alpha = 0.9) +
  theme(legend.position ="none") +
  labs(x = "Habitat", y = "Maturity (d)")


#2.1  _________________________ FEMALE MATURITY __________________________

# scatter plot
ggplot(data = data, aes(x = Ladult_mass, y = Lfemale_matur)) + 
  geom_point(aes(color = hab_simple)) +
  theme(legend.title = element_blank()) +
  labs(x = "Adult mass (g)", y = "Log Female Maturity (d)")

# BOXPLOT 
ggplot(data, aes(x = hab_complex, y = Lfemale_matur, fill = hab_complex)) + 
  geom_boxplot(alpha = 0.9) +
  theme(legend.position ="none") +
  labs(x = "Habitat", y = "Log Female Maturity (d)")


#2.2  ___________________________ MALE MATURITY ____________________________

# scatter plot
ggplot(data = data, aes(x = Ladult_mass, y = Lmale_matur)) + 
  geom_point(aes(color = hab_complex)) +
  theme(legend.title = element_blank()) +
  labs(x = "Adult mass (g)", y = "Log Male Maturity (d)")

# BOXPLOT 
ggplot(data, aes(x = hab_simple, y = Lmale_matur, fill = hab_simple)) + 
  geom_boxplot(alpha = 0.9) +
  theme(legend.position ="none") +
  labs(x = "Habitat", y = "Log Male Maturity (d)")


#3  _______________________ AGE FIRT REPRODUCTION _______________________

# scatter plot
ggplot(data = data, aes(x = Ladult_mass, y = LAFR)) + 
  geom_point(aes(color = hab_simple)) +
  theme(legend.title = element_blank()) +
  labs(x = "Log Adult mass (g)", y = "Log Age first reproduction (d)")

# BOXPLOT 
ggplot(data, aes(x = hab_simple, y = LAFR, fill = hab_simple)) + 
  geom_boxplot(alpha = 0.9) +
  theme(legend.position ="none") +
  labs(x = "Habitat", y = "Log Age first reproduction (d)")


#4  _________________________ GESTATION LENGTH _________________________

# scatter plot
ggplot(data = data, aes(x = Ladult_mass, y = Lgestation)) + 
  geom_point(aes(color = hab_simple)) +
  theme(legend.title = element_blank()) +
  labs(x = "Log Adult mass (g)", y = "Log Gestation length (d)")

# BOXPLOT 
ggplot(data, aes(x = hab_simple, y = Lgestation, fill = hab_simple)) + 
  geom_boxplot(alpha = 0.9) +
  theme(legend.position ="none") +
  labs(x = "Habitat", y = "Log Gestation length (d)")


#5  _________________________ TEAT NUMBER _________________________

# scatterplot
ggplot(data = data, aes(x = Ladult_mass, y = Lteat)) + 
  geom_point(aes(color = hab_simple)) +
  theme(legend.title = element_blank()) +
  labs(x = "Log Adult mass (g)", y = "Log Teat number (n)")

ggplot(data = data, aes(x = Ladult_mass, y = teat_number_n)) + 
  geom_point(aes(color = hab_simple)) +
  theme(legend.title = element_blank()) +
  labs(x = "Log Adult mass (g)", y = "Teat number (n)")

# BOXPLOT -> log (hab_complex)
ggplot(data = data, aes(x = hab_simple, y = Lteat, fill=hab_simple)) + 
  geom_boxplot(alpha=0.9) +
  theme(legend.position = "none") +
  labs(x = "Log Adult mass (g)", y = "Log Teat number (n)")

ggplot(data = data, aes(x = hab_simple, y = teat_number_n, fill=hab_simple)) + 
  geom_boxplot(alpha=0.9) +
  theme(legend.position = "none") +
  labs(x = "Log Adult mass (g)", y = "Teat number (n)")

#6  _________________________ LITTER SIZE _________________________

# scatter plot
ggplot(data = data, aes(x = Ladult_mass, y = log10(Llitter_size))) + 
  geom_point(aes(color = hab_complex)) +
  theme(legend.title = element_blank()) +
  labs(x = "Log Adult mass (g)", y = "Log Litter size (n)")

# BOXPLOT 
ggplot(data = data, aes(x = hab_simple, y = log10(Llitter_size), fill=hab_simple)) + 
  geom_boxplot(alpha=0.9) +
  theme(legend.position = "none") +
  labs(x = "Habitat", y = "Log Litter size (n)")

ggplot(data = data, aes(x = hab_complex, y = litter_size_n, fill=hab_complex)) + 
  geom_boxplot(alpha=0.9) +
  theme(legend.position = "none") +
  labs(x = "Habitat", y = "Litter size (n)")


#7  ________________________ LITTER PER YEAR _________________________

# scatter plot
ggplot(data = data, aes(x = Ladult_mass, y = Llitters_year)) + 
  geom_point(aes(color = hab_complex)) +
  theme(legend.title = element_blank()) +
  labs(x = "Log Adult mass (g)", y = "Log Litters per year (n)")

# BOXPLOT 
ggplot(data = data, aes(x = hab_complex, y = Llitters_year, fill=hab_complex)) + 
  geom_boxplot(alpha=0.9) +
  theme(legend.position = "none") +
  labs(x = "Habitat", y = "Log Litters per year (n)")

ggplot(data = data, aes(x = hab_simple, y = litters_per_year_n, fill=hab_simple)) + 
  geom_boxplot(alpha=0.9) +
  theme(legend.position = "none") +
  labs(x = "Habitat", y = "Litters per year (n)")


#8  ________________________ INTERBIRTH INTERVAL _________________________

# scatter plot
ggplot(data = data, aes(x = Ladult_mass, y = Lbirth_interv)) + 
  geom_point(aes(color = hab_simple)) +
  theme(legend.title = element_blank()) +
  labs(x = "Log Adult mass (g)", y = "Log Interbirth interval (d)")

ggplot(data = data, aes(x = Ladult_mass, y = interbirth_interval_d)) + 
  geom_point(aes(color = hab_simple)) +
  theme(legend.title = element_blank()) +
  labs(x = "Log Adult mass (g)", y = "Interbirth interval (d)")

# BOXPLOT -> log
ggplot(data = data, aes(x = hab_simple, y = Lbirth_interv, fill=hab_simple)) + 
  geom_boxplot(alpha=0.9) +
  theme(legend.position = "none") +
  labs(x = "Habitat", y = "Log Interbirth interval (d)")

ggplot(data = data, aes(x = hab_complex, y = Lbirth_interv, fill=hab_complex)) + 
  geom_boxplot(alpha=0.9) +
  theme(legend.position = "none") +
  labs(x = "Habitat", y = "Log Interbirth interval (d)")


#9  ________________________ NEONATE MASS _________________________

# scatter plot
ggplot(data = data, aes(x = Ladult_mass, y = Lneonate_mass)) + 
  geom_point(aes(color = hab_simple)) +
  theme(legend.title = element_blank()) +
  labs(x = "Log Adult mass (g)", y = "Log Neonate mass (g)")

# BOXPLOT 
ggplot(data = data, aes(x = hab_simple, y = log10(neonate_mass_g), fill=hab_simple)) + 
  geom_boxplot(alpha=0.9) +
  theme(legend.position = "none") +
  labs(x = "Habitat", y = "Log Neonate mass (g)")




#10  ________________________ WEANING AGE _________________________

# scatterplot
ggplot(data = data, aes(x = Ladult_mass, y = Lweaning_age)) + 
  geom_point(aes(color = hab_simple)) +
  theme(legend.title = element_blank()) +
  labs(x = "Log Adult mass (g)", y = "Log Weaning age (d)")

# BOXPLOT 
ggplot(data = data, aes(x = hab_simple, y = Lweaning_age, fill=hab_simple)) + 
  geom_boxplot(alpha=0.9) +
  theme(legend.position = "none") +
  labs(x = "Habitat", y = "Log Weaning age (d)")



#11  ________________________ WEANING MASS _________________________

# scatter plot
ggplot(data = data, aes(x = Ladult_mass, y = Lweaning_mass)) + 
  geom_point(aes(color = hab_simple)) +
  theme(legend.title = element_blank()) +
  labs(x = "Log Adult mass (g)", y = "Log Weaning mass (g)")

# BOXPLOT 
ggplot(data = data, aes(x = hab_simple, y = Lweaning_mass, fill=hab_simple)) + 
  geom_boxplot(alpha=0.9) +
  theme(legend.position = "none") +
  labs(x = "Habitat", y = "Log Weaning mass (g)")



#11  ________________________ GENERATION LENGTH _________________________
# 
ggplot(data = data, aes(x = Ladult_mass, y = data$generation_length_d)) + 
  geom_point(aes(color = hab_complex)) +
  theme(legend.title = element_blank()) +
  labs(x = "Log Adult mass (g)", y = "Generation length (d)")

ggplot(data = data, aes(x = Ladult_mass, y = Lgeneration)) + 
  geom_point(aes(color = hab_complex)) +
  theme(legend.title = element_blank()) +
  labs(x = "Log Adult mass (g)", y = "Log Generation length (d)")

# BOXPLOT -> log (hab_complex)
ggplot(data = data, aes(x = hab_complex, y = Lgeneration, fill=hab_complex)) + 
  geom_boxplot(alpha=0.9) +
  theme(legend.position = "none") +
  labs(x = "Habitat", y = "Log Generation length (d)")


################ 

# ggplot adult mass ~ habitat 
ggplot(data = data, aes(x = hab_complex, y = Ladult_mass, fill=hab_complex)) + 
  geom_boxplot(alpha=0.9) +
  theme(legend.position = "none") +
  labs(x = "Habitat", y = "Log Adult mass (g)")

#