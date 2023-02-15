## Title: ECON 470 HW2
## Author: Ben Yang
## Date Created: 2/13/2023
## Date Edited: 2/13/2023
## Descriptions: This file renders/runs all relevant R code for the assignment

## Preliminaries ---------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, 
               gdata, MatchIt, cobalt, Matching)

## Read data and set workspace for knitr ---------------------------------------

final.hcris.v1996 <- read_rds('data/output/HCRIS_Data_v1996.rds')
final.hcris.v2010 <- read_rds('data/output/HCRIS_Data_v2010.rds')
hcris.data  <- read_rds('data/output/HCRIS_Data.rds')

## Combine data -----------------------------------------------------------------

## create missing variables for columns introduced in v2010 of hcris forms
final.hcris.v1996 = final.hcris.v1996 %>%
  mutate(hvbp_payment=NA, hrrp_payment=NA)

## combine v1996 and v2010 hcris forms, and sort by provider_number/year
final.hcris=rbind(final.hcris.v1996,final.hcris.v2010) %>%
  mutate(fy_end=mdy(fy_end),fy_start=mdy(fy_start),
         date_processed=mdy(date_processed),date_created=mdy(date_created),
         tot_discounts=abs(tot_discounts), hrrp_payment=abs(hrrp_payment)) %>%
  mutate(fyear=year(fy_end)) %>%
  arrange(provider_number,fyear) %>%
  select(-year)

## create count of reports by hospital fiscal year
final.hcris =
  final.hcris %>% 
  add_count(provider_number, fyear, name="total_reports")

## create running total of reports
final.hcris =
  final.hcris %>% 
  group_by(provider_number, fyear) %>%
  mutate(report_number=row_number())

## Create objects for markdown -------------------------------------------------

q1.data <- final.hcris %>% filter(report_number > 1) %>%
  group_by(fyear) %>% summarize(count = length(unique(provider_number)))

q1.plot <- q1.data %>%
  ggplot(aes(x = fyear, y = count)) + geom_point() + geom_line() +
  scale_x_continuous(breaks = c(1997:2017)) +
  labs(x = "Year", y = "Number of Hospitals", Title = "Number of Hospitals with More Than 1 Report in Each Year from 1997 to 2018") +
  theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  theme(
    plot.title = element_text(size = 12, color = "black", hjust = 0.5),
    legend.title = element_text(size = 10, color = "black"),
    legend.position = "top",
    axis.title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 10, color = "black"))

q2.value <- length(unique(hcris.data$provider_number))

q3.plot <- hcris.data %>%
  ggplot(aes(x = factor(year), y = tot_charges)) +
  geom_jitter(alpha = .05) +
  geom_violin(alpha = .9, draw_quantiles = c(0.5)) +
  labs(x = "Year", y = "Total Charges", Title = "Distribution of Total Charges in Each Year from 1997 to 2018") +
  theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  theme(
    plot.title = element_text(size = 12, color = "black", hjust = 0.5),
    legend.title = element_text(size = 10, color = "black"),
    legend.position = "top",
    axis.title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 10, color = "black"))

q4.data <- hcris.data %>% mutate(discount_factor = 1-tot_discounts/tot_charges,
                            price_num = (ip_charges + icu_charges + ancillary_charges)*discount_factor - tot_mcare_payment,
                            price_denom = tot_discharges - mcare_discharges,
                            price = price_num/price_denom)
q4.plot <- q4.data %>%
  ggplot(aes(x = factor(year), y = price)) +
  geom_jitter(alpha = .05) +
  geom_violin(alpha = .9, draw_quantiles = c(0.5)) +
  labs(x = "Year", y = "Estimated Prices", Title = "Distribution of Estimated Prices in Each Year from 1997 to 2018") +
  theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  theme(
    plot.title = element_text(size = 12, color = "black", hjust = 0.5),
    legend.title = element_text(size = 10, color = "black"),
    legend.position = "top",
    axis.title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 10, color = "black"))

## Question 5 Penalty ----------------------------------------------------------

hcris.data <- read_rds('data/output/HCRIS_Data.rds')

hcris.data <- hcris.data %>%
  mutate(discount_factor = 1-tot_discounts/tot_charges,
         price_num = (ip_charges + icu_charges + ancillary_charges)*discount_factor - tot_mcare_payment,
         price_denom = tot_discharges - mcare_discharges,
         price = price_num/price_denom)

final.hcris <- hcris.data %>% ungroup() %>%
  filter(price_denom>100, !is.na(price_denom), 
         price_num>0, !is.na(price_num),
         price<100000, 
         beds>30, year==2012) %>%
  mutate(hvbp_payment = ifelse(is.na(hvbp_payment),0,hvbp_payment),
         hrrp_payment = ifelse(is.na(hrrp_payment),0,abs(hrrp_payment)),
         penalty = (hvbp_payment-hrrp_payment<0))

q5.data <- data.frame(final.hcris %>% group_by(penalty) %>% summarise(mean(price)))

## Question 6 Quarterly --------------------------------------------------------

bed_q1 <- quantile(final.hcris$beds, probs = 0.25, na.rm = TRUE)
bed_q2 <- quantile(final.hcris$beds, probs = 0.5, na.rm = TRUE)
bed_q3 <- quantile(final.hcris$beds, probs = 0.75, na.rm = TRUE)
bed_q4 <- quantile(final.hcris$beds, probs = 1, na.rm = TRUE)

final.hcris <- final.hcris %>% mutate(
  bed_size1 = ifelse(beds < bed_q1, 1, 0),
  bed_size2 = ifelse(beds >= bed_q1 & beds < bed_q2, 1, 0),
  bed_size3 = ifelse(beds >= bed_q2 & beds < bed_q3, 1, 0),
  bed_size4 = ifelse(beds >= bed_q3 & beds <= bed_q4, 1, 0),
  bed_size = ifelse(bed_size1 == 1, 1, 
                    ifelse(bed_size2 == 1, 2, 
                           ifelse(bed_size3 == 1, 3, 
                                  ifelse(bed_size4 == 1, 4, 0)))))

q6.data <- final.hcris %>% group_by(bed_size, penalty) %>% summarise(mean(price))

q6.data$bed_size <- factor(q6.data$bed_size)
q6.data$penalty <- factor(q6.data$penalty)
levels(q6.data$bed_size) <- c("1st Quartile", "2nd Quartile", "3rd Quartile", "4th Quartile")
levels(q6.data$penalty) <- c("Non-Penalized", "Penalized")

q6.data <- q6.data %>% pivot_wider(names_from = bed_size, values_from = `mean(price)`)

## Question 7 Nearest Neighbors ------------------------------------------------

## ALL -------------------------------------------------------------------------

lp.vars <- final.hcris %>%
  dplyr::select(price, penalty, beds, mcaid_discharges, ip_charges, mcare_discharges, tot_mcare_payment) %>%
  na.omit()
lp.covs <- lp.vars %>% dplyr::select(beds, mcaid_discharges, ip_charges, mcare_discharges, tot_mcare_payment)

## nearest neighbor matching with inverse variance distance weights

nn.est1.all <- Matching::Match(Y=lp.vars$price,
                               Tr=lp.vars$penalty,
                               X=lp.covs,
                               M=1,
                               Weight=1,
                               estimand="ATE")
summary(nn.est1.all)
nn.est1.est.all <- -526.95

love.plot(bal.tab(nn.est1.all, covs = lp.covs, treat = lp.vars$penalty), 
          threshold=0.1, 
          grid=FALSE, sample.names=c("Unmatched", "Matched"),
          position="top", shapes=c("circle","triangle"),
          colors=c("black","blue")) + 
  theme_bw()

## nearest neighbor matching with Mahalanobis distance weights

nn.est2.all <- Matching::Match(Y=lp.vars$price,
                               Tr=lp.vars$penalty,
                               X=lp.covs,
                               M=1,
                               Weight=2,
                               estimand="ATE")
summary(nn.est2.all)
nn.est2.est.all <- -492.82

love.plot(bal.tab(nn.est2.all, covs = lp.covs, treat = lp.vars$penalty), 
          threshold=0.1, 
          grid=FALSE, sample.names=c("Unmatched", "Matched"),
          position="top", shapes=c("circle","triangle"),
          colors=c("black","blue")) + 
  theme_bw()

## nearest neighbor matching with propensity score distance weights

logit.model <- glm(penalty ~ beds + mcaid_discharges + ip_charges + mcare_discharges + tot_mcare_payment, family=binomial, data = lp.vars)
ps <- fitted(logit.model)
nn.est3.all <- Matching::Match(Y=lp.vars$price,
                               Tr=lp.vars$penalty,
                               X=ps,
                               M=1,
                               estimand="ATE")
summary(nn.est3.all)
nn.est3.est.all <- -201.03

love.plot(bal.tab(nn.est3.all, covs = lp.covs, treat = lp.vars$penalty), 
          threshold=0.1, 
          grid=FALSE, sample.names=c("Unmatched", "Matched"),
          position="top", shapes=c("circle","triangle"),
          colors=c("black","blue")) + 
  theme_bw()

## inverse propensity weighting (IPW) regression

lp.vars <- lp.vars %>%
  mutate(ipw = case_when(
    penalty==1 ~ 1/ps,
    penalty==0 ~ 1/(1-ps),
    TRUE ~ NA_real_
  ))
mean.t1 <- lp.vars %>% filter(penalty==1) %>%
  dplyr::select(price, ipw) %>% summarize(mean_p=weighted.mean(price,w=ipw))
mean.t0 <- lp.vars %>% filter(penalty==0) %>%
  dplyr::select(price, ipw) %>% summarize(mean_p=weighted.mean(price,w=ipw))
nn.est4.est.all <- mean.t1$mean_p - mean.t0$mean_p

# IPW weighting with regression

ipw.reg <- lm(price ~ penalty, data=lp.vars, weights=ipw)
summary(ipw.reg)

reg1.dat <- lp.vars %>% filter(penalty==1, complete.cases(.))
reg1 <- lm(price ~ beds+ mcaid_discharges + ip_charges + mcare_discharges +
             tot_mcare_payment, data=reg1.dat)
reg0.dat <- lp.vars %>% filter(penalty==0, complete.cases(.))
reg0 <- lm(price ~ beds + mcaid_discharges + ip_charges + mcare_discharges +
             tot_mcare_payment, data=reg0.dat)
pred1 <- predict(reg1,new=lp.vars)
pred0 <- predict(reg0,new=lp.vars)
nn.est5.est1.all <- mean(pred1-pred0)

# IPW weighting with regression in one step

reg.dat <- lp.vars %>% ungroup() %>% filter(complete.cases(.)) %>%
  mutate(beds_diff = penalty*(beds - mean(beds)),
         mcaid_diff = penalty*(mcaid_discharges - mean(mcaid_discharges)),
         ip_diff = penalty*(ip_charges - mean(ip_charges)),
         mcare_diff = penalty*(mcare_discharges - mean(mcare_discharges)),
         mpay_diff = penalty*(tot_mcare_payment - mean(tot_mcare_payment)))
reg <- lm(price ~ penalty + beds + mcaid_discharges + ip_charges + mcare_discharges + tot_mcare_payment + 
            beds_diff + mcaid_diff + ip_diff + mcare_diff + mpay_diff,
          data=reg.dat)
summary(reg)
nn.est5.est2.all <- summary(reg)$coefficients[2,1]

## Quartile --------------------------------------------------------------------

## nearest neighbor matching with inverse variance distance weights

for (i in 1:4) {
  lp.vars <- final.hcris %>% filter(bed_size == i) %>%
    dplyr::select(price, penalty, beds, mcaid_discharges, ip_charges, mcare_discharges, tot_mcare_payment) %>%
    na.omit()
  lp.covs <- lp.vars %>% dplyr::select(beds, mcaid_discharges, ip_charges, mcare_discharges, tot_mcare_payment)
  name <- paste0("nn.est1.bedsQ", i)
  match <- Matching::Match(Y=lp.vars$price,
                           Tr=lp.vars$penalty,
                           X=lp.covs,
                           M=1,
                           Weight=1,
                           estimand="ATE")
  summary(match)
  assign(name, match)
}
nn.est1.est <- c(-6.5365, -742.91, -678.31, -891.79)
nn.est1.est.avg <- mean(nn.est1.est)

## nearest neighbor matching with Mahalanobis distance weights

for (i in 1:4) {
  lp.vars <- final.hcris %>% filter(bed_size == i) %>%
    dplyr::select(price, penalty, beds, mcaid_discharges, ip_charges, mcare_discharges, tot_mcare_payment) %>%
    na.omit()
  lp.covs <- lp.vars %>% dplyr::select(beds, mcaid_discharges, ip_charges, mcare_discharges, tot_mcare_payment)
  name <- paste0("nn.est2.bedsQ", i)
  match <- Matching::Match(Y=lp.vars$price,
                           Tr=lp.vars$penalty,
                           X=lp.covs,
                           M=1,
                           Weight=2,
                           estimand="ATE")
  summary(match)
  assign(name, match)
}
nn.est2.est <- c(-175.83, -803.12, -716.98, -834.59)
nn.est2.est.avg <- mean(nn.est2.est)

## nearest neighbor matching with propensity score distance weights

for (i in 1:4) {
  lp.vars <- final.hcris %>% filter(bed_size == i) %>%
    dplyr::select(price, penalty, beds, mcaid_discharges, ip_charges, mcare_discharges, tot_mcare_payment) %>%
    na.omit()
  
  logit.model <- glm(penalty ~ beds + mcaid_discharges + ip_charges + mcare_discharges + tot_mcare_payment, family=binomial, data = lp.vars)
  ps <- fitted(logit.model)
  
  name <- paste0("nn.est3.bedsQ", i)
  match <- Matching::Match(Y=lp.vars$price,
                           Tr=lp.vars$penalty,
                           X=ps,
                           M=1,
                           estimand="ATE")
  summary(match)
  assign(name, match)
}
nn.est3.est <- c(757.06, -769.75, -495.44, -861.7)
nn.est3.est.avg <- mean(nn.est3.est)

## inverse propensity weighting (IPW) regression

for (i in 1:4) {
  lp.vars <- final.hcris %>% filter(bed_size == i) %>%
    dplyr::select(price, penalty, beds, mcaid_discharges, ip_charges, mcare_discharges, tot_mcare_payment) %>%
    na.omit()

  logit.model <- glm(penalty ~ beds + mcaid_discharges + ip_charges + mcare_discharges + tot_mcare_payment, family=binomial, data = lp.vars)
  ps <- fitted(logit.model)
  
  lp.vars <- lp.vars %>%
    mutate(ipw = case_when(
      penalty==1 ~ 1/ps,
      penalty==0 ~ 1/(1-ps),
      TRUE ~ NA_real_
    ))
  mean.t1 <- lp.vars %>% filter(penalty==1) %>%
    dplyr::select(price, ipw) %>% summarize(mean_p=weighted.mean(price,w=ipw))
  mean.t0 <- lp.vars %>% filter(penalty==0) %>%
    dplyr::select(price, ipw) %>% summarize(mean_p=weighted.mean(price,w=ipw))
  
  name <- paste0("nn.est4.bedsQ", i)
  assign(name, mean.t1$mean_p - mean.t0$mean_p)
}
nn.est4.est <- c(nn.est4.bedsQ1, nn.est4.bedsQ2, nn.est4.bedsQ3, nn.est4.bedsQ4)
nn.est4.est.avg <- mean(nn.est4.est)

# IPW weighting with regression

for (i in 1:4) {
  lp.vars <- final.hcris %>% filter(bed_size == i) %>%
    dplyr::select(price, penalty, beds, mcaid_discharges, ip_charges, mcare_discharges, tot_mcare_payment) %>%
    na.omit()
  
  logit.model <- glm(penalty ~ beds + mcaid_discharges + ip_charges + mcare_discharges + tot_mcare_payment, family=binomial, data = lp.vars)
  ps <- fitted(logit.model)
  
  lp.vars <- lp.vars %>%
    mutate(ipw = case_when(
      penalty==1 ~ 1/ps,
      penalty==0 ~ 1/(1-ps),
      TRUE ~ NA_real_
    ))
  
  ipw.reg <- lm(price ~ penalty, data=lp.vars, weights=ipw)
  reg1.dat <- lp.vars %>% filter(penalty==1, complete.cases(.))
  reg1 <- lm(price ~ beds+ mcaid_discharges + ip_charges + mcare_discharges +
               tot_mcare_payment, data=reg1.dat)
  reg0.dat <- lp.vars %>% filter(penalty==0, complete.cases(.))
  reg0 <- lm(price ~ beds + mcaid_discharges + ip_charges + mcare_discharges +
               tot_mcare_payment, data=reg0.dat)
  pred1 <- predict(reg1,new=lp.vars)
  pred0 <- predict(reg0,new=lp.vars)
  
  name <- paste0("nn.est5.bedsQ", i)
  assign(name,   mean(pred1-pred0))
}
nn.est5.est1 <- c(nn.est5.bedsQ1, nn.est5.bedsQ2, nn.est5.bedsQ3, nn.est5.bedsQ4)
nn.est5.est1.avg <- mean(nn.est5.est1)

# IPW weighting with regression in one step

for (i in 1:4) {
  lp.vars <- final.hcris %>% filter(bed_size == i) %>%
    dplyr::select(price, penalty, beds, mcaid_discharges, ip_charges, mcare_discharges, tot_mcare_payment) %>%
    na.omit()
  
  logit.model <- glm(penalty ~ beds + mcaid_discharges + ip_charges + mcare_discharges + tot_mcare_payment, family=binomial, data = lp.vars)
  ps <- fitted(logit.model)
  
  lp.vars <- lp.vars %>%
    mutate(ipw = case_when(
      penalty==1 ~ 1/ps,
      penalty==0 ~ 1/(1-ps),
      TRUE ~ NA_real_
    ))
  
  reg.dat <- lp.vars %>% ungroup() %>% filter(complete.cases(.)) %>%
    mutate(beds_diff = penalty*(beds - mean(beds)),
           mcaid_diff = penalty*(mcaid_discharges - mean(mcaid_discharges)),
           ip_diff = penalty*(ip_charges - mean(ip_charges)),
           mcare_diff = penalty*(mcare_discharges - mean(mcare_discharges)),
           mpay_diff = penalty*(tot_mcare_payment - mean(tot_mcare_payment)))
  reg <- lm(price ~ penalty + beds + mcaid_discharges + ip_charges + mcare_discharges + tot_mcare_payment + 
              beds_diff + mcaid_diff + ip_diff + mcare_diff + mpay_diff,
            data=reg.dat)
  
  name <- paste0("nn.est5.bedsQ", i)
  assign(name, summary(reg)$coefficients[2,1])
}
nn.est5.est2 <- c(nn.est5.bedsQ1, nn.est5.bedsQ2, nn.est5.bedsQ3, nn.est5.bedsQ4)
nn.est5.est2.avg <- mean(nn.est5.est2)

q7.data <- rbind(nn.est1.est, nn.est2.est, nn.est3.est, nn.est4.est, nn.est5.est1, nn.est5.est2)
q7.data <- cbind(
  c(nn.est1.est.all, nn.est2.est.all, nn.est3.est.all, nn.est4.est.all, nn.est5.est1.all, nn.est5.est2.all),
  q7.data,
  c(nn.est1.est.avg, nn.est2.est.avg, nn.est3.est.avg, nn.est4.est.avg, nn.est5.est1.avg, nn.est5.est2.avg))
colnames(q7.data) <- c("All", "1st Quartile", "2nd Quartile", "3rd Quartile", "4th Quartile", "Average")
rownames(q7.data) <- c("NN Inverse Variance Distance", "NN Mahalanobis Distance", "NN Propensity Score Distance", "Inverse Propensity Weighted Regression", "Simple Linear Regression1", "Simple Linear Regression2")

## Save data for markdown ------------------------------------------------------

rm(list=c("final.hcris.v1996", "final.hcris.v2010", "hcris.data", "final.hcris", "q1.data", "q4.data"))
save.image("Hwk2_workspace.Rdata")
