
# habitat to a factor
hab_simple <- factor(mammals_data$Class_simple)
hab_complex <- factor(mammals_data$Class_complex)

# transformation of data -> log
df1 <- data.frame(x=c("Abditomys latidens", "Abeomelomys sevia", "Abrawayaomys ruschii", "Abrocoma bennettii", "Abrocoma boliviensis", "Abrocoma budini","Abrocoma cinerea","Abrocoma famatina", "Abrocoma shistacea", "Abrocoma uspallata", "Abrocoma vaccarum", "Abrothrix andinus", "Abrothrix illutea", "Abrothrix jelskii", "Abrothrix lanosus", "Abrothrix longipilis"), y=c(268.09, 57.89, 62.99, 265.5, 158, 221,82, 221,82, 221.82, 164279, 221.82, 34.65, 47.79, 34.5, 25915))
mass_log <- log10(df1$y)

# omit NA
adultmass <- na.omit(mammals_data$adult_mass_g)

data1 <- mammals_data[!is.na(mammals_data$adult_mass_g),]
data1

data2 <- subset(mammals_data, !is.na(adult_mass_g))
data2

df <- data.frame(mammals_data$adult_mass_g, mammals_data$max_longevity_d, hab_simple)
completerecords <- na.omit(df)

data3 <- mammals_data[complete.cases(mammals_data),]
data3

install.packages("tidyr")
library("tidyr")
data4 <- mammals_data %>% drop_na()
data4

####################

# plots - GGPLOT2
library(ggplot2)
ggplot(data = mammals_data, aes(x = adult_mass_g, y = max_longevity_d), na.action = na.exclude) + 
  geom_point(aes(color = hab_simple)) 

ggplot(data = mammals_data, aes(x = adult_mass_g, y = litters_per_year_n), na.action = na.exclude) + 
  geom_point(aes(color = hab_simple)) 

# plot - DENSITY
ggplot(mammals_data, aes(max_longevity_d, fill = hab_simple)) + geom_density(alpha = 0.5) +
  theme_classic() + scale_fill_manual( values = c('seagreen2', 'steelblue3', 'snow3')) +
  theme(legend.title = element_blank()) + 
  labs(x = "Max Longevity", y = "Density")