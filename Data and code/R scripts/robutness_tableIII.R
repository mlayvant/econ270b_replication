# Title: Robustness check for Table III
# Description: 
#     (1) Re-run column (1) of Table III using the original Tobit regression model
#     (2) Run column (1) of Table III using a beta regression model

### Call relevant libraries
library(betareg)
library(sandwich)
library(lmtest)
library(margins)


### Tobit model replication
# Create indicator value for insurance enrollment as "treatment"
tab3.data$treatment_indic <- ifelse(tab3.data$treatment == 'Insurance', 1, 0)

# Run Tobit specification
tobit_reg_keep_church <-  AER::tobit(choice_keep_church ~ treatment_indic , left = 0, right = 1, data = tab3.data)

# Calculate standard errors clustered at the session level 
robust.var.tobit <- sandwich::vcovCL(tobit_reg_keep_church,
                               cluster = tab3.data$session) %>%
  diag()
diag(tobit_reg_keep_church$var) <- robust.var.tobit

# Confirm results match Table III column (1), Panel A
summary(tobit_reg_keep_church)     


### Beta regression
# Transform outcome variable using following Smithson and Verkuilen (2006)
#     which notes that if the outcome variable also assumes the extremes 0 and 1, 
#     a useful transformation in practice is (y · (n − 1) + 0.5)/n where n is the sample size. 
tab3.data$keep_church_beta <- (tab3.data$choice_keep_church  * (538 - 1) + .5)/538

# Run beta regression
beta_reg_keep_church <- betareg(keep_church_beta ~ treatment_indic, data = tab3.data)
summary(beta_reg_keep_church)

# Calculate standard errors clustered at the session level 
coeftest(beta_reg_keep_church, vcov = vcovCL, cluster = ~ session)

# Calculate marginal effects and compare to Tobit regression results
margins(beta_reg_keep_church) 