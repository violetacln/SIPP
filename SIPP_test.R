
# example of code: mortality data smoothing and forecasting ------------


library(demography)
library(smoothAPC)


#-----------------------------------------------------------------------

# STEP 1: mortality smoothing
# using functional models and Hyndman's packages


# use here an example data from the package
m <- log(fr.mort$rate$female[1:30, 150:160])
plot(m)
plot3d(m)

sm <- autoSmoothAPC(m)
plot(sm)
plot(sm, "period")
plot(sm, "cohort")
plot(sm, "surface")
plot(sm, "residuals")
plot(sm, "original", main = "Original data")

plot3d(sm)
plot3d(sm, "surface", color.palette = "special")
plot3d(sm, "cohort")
plot3d(sm, "period")
plot3d(sm, "residuals")
plot3d(sm, "original", color.palette = rainbow)


# same method is then applied for males and for total

#denote the matrices of smoothed rates: --------------
# matrix_sm_rate_total
# matrix_sm_rate_females
# matrix_sm_rate_males


### STEP 2: make a demogdata object for future modeling and forecasting -------------

# make a demogdata object (call it smdemog) from the smoothed components  
# plus the corresponding population matrices (total, males, females) by age, time

# assume we have the dimensions age and time defined as
#Dage <- ...
#Dyear <- ...


# start with some object of the right dimensions
smdemog <- demogdata(data = matrix_pop_total, 
                  pop = matrix_pop_total, 
                  ages = Dage, 
                  years = Dyear, 
                  type = "mortality", 
                  label = "country_name", 
                  name = "total")

# then update the population matrices for females and males
smdemog$pop$female <- matrix_pop_female
dimnames(smdemog$pop$female)<- list(Dage, Dyear)

smdemog$pop$male <- matrix_pop_male
dimnames(smdemog$pop$male) <- list(Dage, Dyear)

# then update the smoothed (in the previous step) mortality rates
smdemog$rate$total <- matrix_sm_rate_total
dimnames(smdemog$rate$total) <- list(Dage, Dyear)

smdemog$rate$female <- matrix_sm_rate_female
dimnames(smdemog$rate$female) <- list(Dage, Dyear)

smdemog$rate$male <- matrix_sm_rate_male
dimnames(smdemog$rate$male) <- list(Dage, Dyear)

# make sure using the right names
names(smdemog[["rate"]]) <- c("total","female", "male")
names(smdemog[["pop"]]) <- c("total","female", "male")


### STEP 3: model fitting  ------------------------------------

# a) if we wish forecasting coherently (for males and females), use:
mfit <- coherentfdm(smdemog)

# b) if we wish forecasting independently for males and females:
mfits <- fdm(smdemog)

plot(residuals(mfit$product))
plot(residuals(mfit$ratio$male))

# can also use the other functions from demography package
# and do lifetables, life expectancy etc


# STEP 4: forecasting ----------------------------------
# apply the usual forecasting functions of the demography-package!

mfor <- forecast(mfit, h=50) # other options, etc

# and the usual graphics and results .... (to add)



