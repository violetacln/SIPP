### Bayesian hierarchical models for individual/aggregated events (births, deaths, mogration)

library(brms)

################# M1. Models for counts data ############################

### given: data frames (dbirths, ddeaths, dmigr) of counts of events (nr) and counts of exposed (nrExposed), 
### by municipality (svf), age (aldur), time (ar), gender (kyn) , type of migration (gerd)

## we use poisson models with offset terms---------
## brm(y ~ ... + offset(log(n)), family = poisson())

## simplest models. One may easily test better ones, by allowing more complex error structures, random slopes, interactions, etc.

## births: simplest model
mb <- brms::brm(nr ~ t2(ar,aldur) 
                # t2(ar) + t2(aldur)
                + (1|svf) + citiz + offset(log(nrExposed)), 
                  family=poisson(), data= dbirths,
                  warmup = 500, iter = 2000, chains = 2, seed=123)

# deaths
md <- brms::brm(nr ~ 
                 # t2(ar) + t2(aldur)
                  t2(ar, aldur) 
                + kyn + offset(log(nrExposed)),  
                family=poisson(), data= ddeaths, 
                warmup = 500, iter = 2000, chains = 2, seed=123)

# migration (types are encoded by gerd)
mm <- brms::brm(nr ~ 
                # t2(aldur) + t2(ar) 
                 t2(ar, aldur)
                + kyn + gerd + citiz 
                 + (1|svf) 
                + offset(log(nrExposed)) ,  
                family=poisson(), data= dmigr,
                warmup = 500, iter = 2000, chains = 2, seed=123)


#------------ model performance evaluation -------------------

# chose the model to evaluate
mm <- md ## or any other  
summary(mm)
performance::variance_decomposition(mm)
plot(mm)
brms::ranef(mm)
brms::conditional_effects(mm, points=TRUE)
brms::conditional_effects(mm, surface=TRUE)
# brms::conditional_effects(mm, points=TRUE, re_formula=NULL)  ## much larger bands, when RE considered

brms::pp_check(mm, type = "stat_2d")
#pp_check(mm, type = "error_hist", ndraws = 20)
pp_check(mm, type = "scatter_avg", ndraws = 100)
#pp_check(mm, type = "rootogram")
pp_check(mm, type = "loo_pit")
brms::bayes_R2(mm)
#------------------------------------------------------------------------------------




################# M2. Models for microdata #############################

## simplest models. One may easily test better ones, by allowing more complex error structures, random slopes, interactions, etc.

## given a data frame called: micro; containing all microdata, with y, yd, y_immExt, y_em_ext, y_m_int which are
## binary valued responses concerning
## births, deaths, immigration, emigration, internal migration
## other variables are:
## svf-municipality, Menntun-education, fjs-size of family, svffjolgun-size change of municipality, kyn-gender, 
## aldur-age, aldurF-age as factor, fjolskyldustaerd-size of family, rfang-citizenship, fland-country of birth 

# Bayesian "null" model, for y=binary response corresponding to birth events ------------------------
library(brms)
m_bayes <- brms::brm(formula= y ~ 1 + (1|svf)
                       ,data=micro
                     ,family = binomial()
                     ,warmup = 500, iter = 2000, chains = 2,inits= "0",
                     seed = 123)

## NOTE: may use the future package for more flexible parallelization
# library(future)
# plan(multiprocess)
# fit7 <- update(fit7, future = TRUE)

# evaluate the model --------------------------
summary(m_bayes)
plot(m_bayes)
## head(predict(m_bayes))
# plot(brms::conditional_effects(m_bayes), ask = FALSE)
brms::loo(m_bayes)
brms::pp_check(m_bayes)

# Bayesian model for births, with age and time as RE-------------------------------
library(brms)
m1_bayes <- brms::brm(formula= y|trials(1) ~ 1 +
                      (1|aldurF)
                    +  (1|arF) 
                  # + (1|svf)
                     ,data=micro
                     ,family = binomial()
                     ,warmup = 500, iter = 2000, chains = 2,inits= "0",
                     #cores=2,
                     seed = 123)


# Bayesian model for births, using Gaussian processes by (age, time) -------------------------------------------------------------
m1gp_bayes <- brms::brm(formula= y|trials(1)~ 1 + gp(ar, aldur)  ##, by=Menntun)
                      ##  (1|svf)
                      ,data=micro
                      ,family = binomial()
                      ,warmup = 500, iter = 2000, chains = 1,inits= "0",
                      #cores=2,
                      seed = 123)
# evaluate the model ---------------
summary(m1gp_bayes)
plot(m1gp_bayes)
##head(predict(m_bayes))
# plot(brms::conditional_effects(m_bayes), ask = FALSE)
#brms::loo(m1gp_bayes)
#brms::pp_check(m1gp_bayes)

### same types of models are tested for yd, y_immExt, y_em_ext, y_m_int responses ----------------
### ----------------------------------------------------------------------------------------------



