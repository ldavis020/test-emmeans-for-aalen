

## test function for emmeans in aalen ##

library(timereg)
library(emmeans)
library(tidyverse)

# use the PLCO data as test data
plco <- read.csv(file="C:/Users/ldavi/OneDrive/Documents/plco_final.csv")
# make stage and arm into factors 
plco$stage <- factor(plco$stage) # ref = stage 1 
plco$arm <- factor(plco$arm)     # ref = control 

# create aalen model with interaction for arm and stage 
mod <- aalen(Surv(followup, dead==1)~const(arm)*const(stage), plco)
summary(mod)

# function 
# translate aalen model for emmeans
process_aalen_for_emmeans <- function(model_object, source_data){
  coefs <- coef(model_object)[, 1]
  names <- names(coef(model_object)[, 1])
  vcov_named <- vcov(model_object)
  rownames(vcov_named) <- names
  colnames(vcov_named) <- names
  
  emmeans::qdrg(formula = formula(model_object),
                data = source_data,
                coef = coefs,
                vcov = vcov_named)
}

# use function to get contrasts 
process_aalen_for_emmeans(mod, plco) |>
  emmeans(specs = c("arm", "stage"))


# find contrast by hand 
# intervention+stage4 vs control+stage1

