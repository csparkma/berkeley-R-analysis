mecha_car_mpg <- read.csv(
  file='MechaCar_mpg.csv'
  , check.names=F
  , stringsAsFactors = F
)

suspension_coil <- read.csv(
  file='Suspension_Coil.csv'
  , check.names=F
  , stringsAsFactors = F
)

head(mecha_car_mpg)

#Rename Columns
colnames(mecha_car_mpg)
names(mecha_car_mpg)[names(mecha_car_mpg) == "vehicle length"] <- "vehicle_length"
names(mecha_car_mpg)[names(mecha_car_mpg) == "vehicle weight"] <- "vehicle_weight"
names(mecha_car_mpg)[names(mecha_car_mpg) == "spoiler angle"] <- "spoiler_angle"
names(mecha_car_mpg)[names(mecha_car_mpg) == "ground clearance"] <- "ground_clearance"

# MPG Regression
summary(lm(mpg ~ vehicle_length + ground_clearance,data=mecha_car_mpg))

# Suspension Coil Summary Table
head(suspension_coil)
summarize_psi <- suspension_coil %>% summarize(mean=mean(PSI))
summarize_psi["median"] <- suspension_coil %>% summarize(median=median(PSI))
summarize_psi["variance"] <- suspension_coil %>% summarize(variance=var(PSI))
summarize_psi["stdv"] <- suspension_coil %>% summarize(sd=sd(PSI))

head(summarize_psi)
# Suspension Coil T-Test
t.test(
  suspension_coil$PSI
  ,mu=1500
)
