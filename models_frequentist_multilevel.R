### using  frequentist estimates of multilevel models for individual/aggregated events (births, deaths, migration)
### results are useful because fast but also because may be used for initalization, to speed up convergence of Bayesian methods
###

######### M1. Models for counts and rates #######################

## simplest models. One may easily improve by testing models with random slopes, interactions, etc.

## given: data frames dbirths, ddeaths, dmigr

library(lme4)
library(gamm4)
library(splines)

#---------- births
# baseline and testing icc
mbf0 <- glmer(nr ~ offset(log(nrExposed))
              + (1|svf), 
              family=poisson(), data= dbirths)

mbf <- gamm4::gamm4(nr ~ t2(ar) + t2(aldur) + citiz + offset(log(nrExposed)), 
                    family=poisson, random = ~ (1|svf), data= dbirths)

mbf1 <- glmer(nr ~ ns(ar) + ns(aldur) + citiz + offset(log(nrExposed))
              + (1|svf), 
              family=poisson(), data= dbirths)

mbf2 <- glmer(nr ~ ns(ar,aldur) + citiz + offset(log(nrExposed))
              + (1|svf), 
              family=poisson(), data= dbirths)

mbS1 <- gamm4::gamm4(nnr ~ s(ar) + s(aldur) + citiz + offset(log(nnrExposed)), 
                     family=poisson, data= dbirthsSums)


#--------- deaths
mdf <- gamm4::gamm4(nr ~ s(ar, aldur) + kyn + offset(log(nrExposed)),
                 family=poisson, data= ddeaths)


#---------- migration
mmf <- gamm4::gamm4(nr ~ t2(aldur) +t2(ar) + kyn + gerd + citiz  + offset(log(nrExposed)) ,
                 family=poisson
                 #, random = ~ (1|svf)
                 , data= dmigr)

mmf1 <- glmer(nr ~ ns(aldur) + ns(ar)
              + kyn + gerd + citiz 
              + (1|svf) 
              + offset(log(nrExposed)) ,  
              family=poisson() , data= dmigr)


#--------- check model performance ------------------------

m <- mmf1    # if it is glmer type
### or
# m <- mmf$mer  # if it is gamm4

summary(m)

ranef(m)

performance::icc(m)

reEx <- merTools::REsim(m)
#head(reEx)
merTools::plotREsim(reEx, labs=TRUE, oddsRatio=TRUE)
# merTools::plotREsim(reEx, labs=TRUE)

feEx <- merTools::FEsim(m)
#head(feEx)
merTools::plotFEsim(feEx)

library(arm)
sim_fit <- arm::sim( m, 10000)  
bayestestR::hdi(sim_fit)
bayestestR::eti(sim_fit)
xf <- bayestestR::ci(sim_fit, ci = c(.5, .8, .95), effects = "fixed")
library("see")
plot(xf) 


# if appropriate:
# plot(ggeffects::ggpredict(m, terms = c(".....", ....)))


######### M2. Models for microdata #############################


## simplest models also here. One may easily improve by testing models with random slopes, interactions, etc.

## given a data frame called micro: all microdata, with y, yd, y_immExt, y_em_ext, y_m_int:
## binary valued responses concerning
## births, deaths, immigration, emigration, internal migration
## other variables are:
## svf-municipality, Menntun-education, fjs-size of family, svffjolgun-size change of municipality, kyn-gender, 
## aldur-age, aldurF-age as factor, fjolskyldustaerd-size of family, rfang-citizenship, fland-country of birth 

#------------ births

# baseline, testing icc for birth data (y=0,1)
m0_freq <- lme4::glmer(y ~ 1 
                       + (1|svf) 
                       ##+ (1|region)         
                       , family=binomial(logit)
                       , data=micro)

# more complex
m1_freq <- lme4::glmer(y ~ 1 + Menntun + fjs + fjolskyldustaerd + svffjolgun 
                       + I(ar-min(ar)) 
                       + (1|aldurF)
                       , family=binomial()
                       , data=subset(micro, ar<2021))

# random slopes also
m3_freq <- lme4::glmer(y ~ 1 + Menntun + fjs + fjolskyldustaerd + svffjolgun 
                       + I(ar-min(ar)) 
                       + (1+ I(ar-min(ar))|aldurF)
                       , family=binomial()
                       , data=subset(micro, ar<2021))

#------------ deaths

## check if municipality matters --------------
m0d_freq <- lme4::glmer(y_d ~ 1 
                        +(1|svf)
                        , family=binomial()
                        , data=subset(micro, ar<2021))
summary(m0d_freq)
performance::icc(m0d_freq)  ## very small grouping by svf

## more complex
m1d_freq <- lme4::glmer(y_d ~ 1 
                         + kyn
                        + fjolskyldustaerd 
                        ## + Menntun 
                        + fland
                        + I(ar-min(ar)) 
                        + (1|aldurF)
                        , family=binomial()
                        , data=subset(micro, ar<2021))

#------------migration---------------------------

#----- emigration as a separate component

## check if different by municipality
m0eE_freq <- lme4::glmer(y_em_ext ~ 1 
                         +(1|svf)
                         , family=binomial()
                         , data=subset(micro, ar<2021))

## more complex -----------------------
m1eE_freq <- lme4::glmer(y_em_ext ~ 1 
                         + kyn
                         #   + Menntun 
                         + fjs + fjolskyldustaerd 
                         #  + rfang 
                         + fland 
                         #  + svffjolgun
                         ##  + FlutnFyrr   ## to test if matters
                         + I(ar-min(ar)) 
                         + (1|aldurF)
                         , family=binomial()
                         , data=subset(micro, ar<2021))

#----- immigration as a separate component

## check if different by municipality
m0ImmExt_freq <- lme4::glmer(y_imm_ext ~ 1 
                             +(1|svf)
                             , family=binomial()
                             , data=subset(micro, ar<2021))

## more complex 
####  could check whether slopes of various predictors depend on RE
#### could also check whether various interactions matter
m1ImmExt_freq <- lme4::glmer(y_imm_ext ~ 1 + kyn
                             + Menntun + fjs + fjolskyldustaerd 
                             + rfang 
                             + svffjolgun
                             + ns(aldur) + ns(I(arImmExt-min(arImmExt)))     
                             + (1|svf)  
                             , family=binomial()
                             , data=subset(micro, ar<2021))

#------- internal migration

## baseline
m0k_mInt_freq <- lme4::glmer(y_m_int ~ 1 
                             #+ kyn
                             +(1|svf)
                             , family=binomial()
                             , data=subset(micro, ar<2021))

## more complex proposals
m1mInt_freq <- lme4::glmer(y_m_int ~ 1 + kyn
                           + Menntun + fjs + fjolskyldustaerd 
                           + rfang  
                           ##+ fland 
                           + FlutnSidar
                           + svffjolgun
                           + I(ar-min(ar)) 
                           + (1|aldurF)
                           + (1|svf)          
                           #### and could check whether slopes of various depend on it
                           #### could also check whether various interactions matter
                           , family=binomial()
                           , data=subset(micro, ar<2021))

#-------------------------------------------------

############## models' evaluation #######################
## for any of the models above do:

# m <-.... ## the model of interest

summary(m)
# random effects
reEx <- merTools::REsim(m)
merTools::plotREsim(reEx, labs=TRUE, oddsRatio=TRUE)
## or:
## merTools::plotREsim(reEx, labs=TRUE)
# fixed effects
feEx <- merTools::FEsim(m)
merTools::plotFEsim(feEx)
# simulating credible intervals
library(arm)
sim_fit <- arm::sim( m, 10000)  
bayestestR::hdi(sim_fit)
bayestestR::eti(sim_fit)
xf <- bayestestR::ci(sim_fit, ci = c(.5, .8, .95), effects = "fixed")
library("see")
plot(xf) 

##-----------------------------



