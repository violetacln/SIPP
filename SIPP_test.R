
# example of code: mortality data smoothing and forecasting ------------

#-----------------------------------------------------------------------

# part 1: mortality, using functional models and Hyndman's packages

library(demography)
library(smoothAPC)


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

### key step **********************
# make a demogdata object (call it smdemog) from the smoothed component sm[[1]] 
# and 
# a similar object for males - data, plus
# whatever else needed, like population by gender matrices
# smdemog <- ...

# then apply the usual forecasting functions of the demography-package!

# a) if wish forecasting coherently (for males and females), use:
mfit <- coherentfdm(smdemog)

# b) if wish forecasting independently for males and females:
mfits <- fdm(smdemog)

plot(residuals(mfit$product))
plot(residuals(mort.fit$ratio$male))

# can also use the other functions from demography package
# and do lifetables, life expectancy etc


