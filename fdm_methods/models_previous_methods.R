
##
# example of older code: mortality data, smoothing and forecasting (same type of method was used for fertility in previous years) 
# list of older methods for migration forecasts (based on ARDL and bsts models)
##

library(demography)
library(smoothAPC)

#---------------------- V1: functional models, see Hyndman's paper and R-packages ----------------------------------------

# example, mortality data
# STEP 1: mortality smoothing

# use for now an example data from the package
# will add: querying directly our web-data and creating the 
# appropriate demogdata object which will be the input for this step


sm_rate_matrices <- list()
i <- 1
for (rate_matrices in list(
                  fr.mort$rate$female[1:30, 150:160],
                  fr.mort$rate$male[1:30, 150:160],
                  fr.mort$rate$total[1:30, 150:160]) 
     )
  {
m <- log(rate_matrices)
#plot(m)
#plot3d(m)
sm <- autoSmoothAPC(m)
#plot(sm)
# plot(sm, "period")
# plot(sm, "cohort")
# plot(sm, "surface")
# plot(sm, "residuals")
# plot(sm, "original", main = "Original data")
# 
# plot3d(sm)
# plot3d(sm, "surface", color.palette = "special")
# plot3d(sm, "cohort")
# plot3d(sm, "period")
# plot3d(sm, "residuals")
# plot3d(sm, "original", color.palette = rainbow)
sm_rate_matrices[[i]] <- sm$result
i <- i+1
  }


# denote the matrices of smoothed rates: --------------
matrix_sm_rate_female <- sm_rate_matrices[[1]]
matrix_sm_rate_male <- sm_rate_matrices[[2]]
matrix_sm_rate_total <- sm_rate_matrices[[3]]


### STEP 2: make a demogdata object for future modeling and forecasting -------------

# make a demogdata object (call it smdemog) from the smoothed components  
# plus the corresponding population matrices (total, males, females) by age, time

# define the dimensions age and time:
Dage <- as.numeric(dimnames(matrix_sm_rate_total)[[1]])
Dyear <- as.integer(dimnames(matrix_sm_rate_total)[[2]])

# define the matrices of population total, female, male
matrix_pop_total <- fr.mort$pop$total[1:30, 150:160]
matrix_pop_female <- fr.mort$pop$female[1:30, 150:160]
matrix_pop_male <- fr.mort$pop$male[1:30, 150:160]

# start with some object of the right dimensions
smdemog <- demogdata(data = matrix_pop_total, 
                  pop = matrix_pop_total, 
                  ages = Dage, 
                  years = Dyear, 
                  type = "mortality", 
                  label = "country_name", 
                  name = "total")

# then update the population matrices for females and males ---
smdemog$pop$female <- matrix_pop_female
dimnames(smdemog$pop$female)<- list(Dage, Dyear)

smdemog$pop$male <- matrix_pop_male
dimnames(smdemog$pop$male) <- list(Dage, Dyear)

#smdemog$pop$total # is already in
dimnames(smdemog$pop$total)  <- list(Dage, Dyear)

# then update the smoothed (in the previous step) mortality rates ------
# --- attention!: need to do "exp" to recover rates , since had log ----
smdemog$rate$total <- exp(matrix_sm_rate_total)
dimnames(smdemog$rate$total) <- list(Dage, Dyear)

smdemog$rate$female <- exp(matrix_sm_rate_female)
dimnames(smdemog$rate$female) <- list(Dage, Dyear)

smdemog$rate$male <- exp(matrix_sm_rate_male)
dimnames(smdemog$rate$male) <- list(Dage, Dyear)

# make sure using the right names
names(smdemog[["rate"]]) <- c("total","female", "male")
names(smdemog[["pop"]]) <- c("total","female", "male")



### STEP 3: fdm model fitting  ------------------------------------


# a) if we wish forecasting coherently (for males and females), use:
mfit <- coherentfdm(smdemog)


# b) if we wish forecasting independently for males and females:
#mfits <- fdm(smdemog)

plot(residuals(mfit$product))
plot(residuals(mfit$ratio$male))

# can also use the other functions from demography package
# and do lifetables, life expectancy etc



# STEP 4: forecasting ----------------------------------
# apply the usual forecasting functions of the demography-package!

mfor <- forecast(mfits, h=50) # other options, etc
plot(mfor)
models(mfor)

# and the usual graphics and results .... (to add)

#-------------------------------------------------------------

#------------------------V2. ARDL models for migration, short term projections -------------------------------------
#..........
#------------------------V3. Bayesian, structural time series models of long term migration projections--------------
#............
#-------------------------------------------------------------

