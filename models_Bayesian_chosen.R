
## Model fitting:

## P1. the chosen Bayesian, hierarchical models, for all demographic components with:

## smooth-dependence/Gaussian process priors along (time/age/time and age) - dimension and fixed/random effects of rest of attributes

## Note: to be improved by using Markov random field terms for municipalities variables

### Notations----------
### nr=counts, nrExposed=counts of exposed, aldur=age, ar=year, citiz=citizenship, kyn=gender, gerd=types of migration (emigration/immigration)
###--------------------

## P1.1. births --------------------

## using mgcv ---
mb_mgcv <- mgcv::bam( nr ~ 1 + s(aldur, bs='gp', by=citiz) 
                   + aldur + ar:aldur + citiz 
                   + offset(log_nrExposed)
                   , family= poisson(), data=traindata_1b)
## using brms ---
mb_brms <- brms:brm(nr ~ 1 + gp(aldur, by=citiz) 
                   + aldur + ar:aldur + citiz 
                   + offset(log_nrExposed)
                   , family= poisson(), data=traindata_1b, chains=4)


## P1.2. deaths ---------------------

## using mgcv ---
md_mgcv <- mgcv::bam( nr ~ 1 + s(aldur, bs='gp', by=kyn) 
                   + aldur + ar:aldur + kyn 
                   + offset(log_nrExposed)
                   , family= poisson(), data=traindata_1d)

## using brms ---
md_brms <- brms:brm(nr ~ 1 + gp(aldur, by=kyn) 
                   + aldur + ar:aldur + kyn 
                   + offset(log_nrExposed)
                   , family= poisson(), data=traindata_1d, chains=4)


## P1.3. migration ------------------

## using mgcv, v1 ---
mm_mgcv <- mgcv::bam( nr ~ 1 + s(aldur, ar) 
                   + kyn*gerd*citiz*ar 
                   + offset(log_nrExposed)
                   , family= poisson(), data=traindata_1m)

## using mgcv, v2 ---
mm_mgcv <- mgcv::bam( nr ~ 1 + s(aldur, bs='gp') + aldur + ar:aldur
                   + kyn*gerd*citiz*ar 
                   + offset(log_nrExposed)
                   , family= poisson(), data=traindata_1m)


## using brms, v1 ---
mm_brms <- brms:brm(nr ~ 1 + t2(aldur, ar) 
                   + kyn*gerd*citiz*ar 
                   + offset(log_nrExposed)
                   , family= poisson(), data=traindata_1m, chains=4)

## using brms, v2 ---
mm_brms <- brms:brm(nr ~ 1 + gp(aldur) + aldur + ar:aldur
                   + kyn*gerd*citiz*ar 
                   + offset(log_nrExposed)
                   , family= poisson(), data=traindata_1m, chains=4)



## P2. stochastic ccmp (stochastically combining paths from the demographic components projected using the models above) ------------

## summary results from P2 ------------------------

## P3. evaluation of components' models (standard procedures) -------------------

## P4. evaluation of resulted population forecast (shorter time series) -------------

## P5. simple local projections (by municipality) as rates, Poisson modelled, 
## with total population as offset, at each time point 
## and using Gaussian process priors for the time dependence functions of local population counts which vary by municipality and time

## using mgcv ---
m_svf_mgcv <- mgcv::gam(nr ~  s(ar, by=svf, bs='gp' ) + s(svf, bs='re')
                              + offset(log_nrtot)
                               #, method="REML"  ## w/wo, depending on goal  
                              , family=poisson(), data=pop_loc)

## using brms ---
m_svf_brms <- brms::brm(nr ~  gp(ar, by=svf) + (1|svf)
                              + offset(log_nrtot) 
                              , family=poisson(), data=pop_loc)


## details to be updated soon! ---------***-----------
