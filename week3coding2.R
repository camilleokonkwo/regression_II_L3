# read in date set
# replace datapath with your data directory
datapath = "~/Documents/P8110Fall2023/datasets/"
datafile = paste0(datapath,"whas100.csv")
data = read.csv(datafile,header=T)
head(data)

########################################
#### compare two survival functions ####
########################################
library(survival)
# log-rank test
print(survdiff(Surv(lenfol, fstat)~gender, data=data),
      digits=5)

# more tests
library(survMisc)
fit = ten(survfit(Surv(lenfol, fstat)~gender, 
                  data=data))
comp(fit)
knitr::kable(attributes(fit)$lrt[,c(1,6:8)],
             "simple",digits=4)

# plot survival curves
library(ggsurvfit)
survfit2(Surv(lenfol, fstat)~gender, data=data) %>% 
  ggsurvfit() +
  add_censor_mark() +
  add_pvalue(location="annotation",
             caption="Log-rank {p.value}")


##########################################
#### compare multiple survival curves ####
##########################################
# create age intervals
data$agecat = cut(data$age, 
                  c(min(data$age),60,70,80,
                    max(data$age)),
                  include.lowest=T,right=F)

# log-rank test
print(survdiff(Surv(lenfol, fstat)~agecat, data=data),
      digits=5)

# plot
survfit2(Surv(lenfol, fstat)~agecat, data=data) %>% 
  ggsurvfit() +
  add_censor_mark() +
  add_pvalue(location="annotation",
             caption="Log-rank {p.value}")

# more tests
fit = ten(survfit(Surv(lenfol, fstat)~agecat, data=data))
comp(fit)
knitr::kable(attributes(fit)$lrt,
             "simple",digits=4)

# Bonferroni correction for multiple testing
# USE WITH CAUTION!
library(survminer)
pairwise_survdiff(Surv(lenfol, fstat)~agecat, data=data,
                  p.adjust.method="bonferroni")

# multiple groups from two or more variables
print(survdiff(Surv(lenfol, fstat)~agecat+gender, 
               data=data), digits=5)