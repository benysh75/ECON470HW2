## Title: ECON 470 HW2
## Author: Ben Yang
## Date Created: 2/13/2023
## Date Edited: 2/20/2023
## Descriptions: This file renders/runs all relevant R code for the assignment

## Preliminaries ---------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, 
               gdata, MatchIt, cobalt, Matching)

## Read data and set workspace for knitr ---------------------------------------

final.hcris.v1996 <- read_rds('data/output/HCRIS_Data_v1996.rds')
final.hcris.v2010 <- read_rds('data/output/HCRIS_Data_v2010.rds')
hcris.data  <- read_rds('data/output/HCRIS_Data.rds')

## Create objects for markdown -------------------------------------------------

## Question 1 Hospitals with More Than One Report ------------------------------

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
  dplyr::select(-year)

## create count of reports by hospital fiscal year
final.hcris =
  final.hcris %>% 
  add_count(provider_number, fyear, name="total_reports")

q1.data <- final.hcris %>% filter(total_reports > 1) %>%
  group_by(fyear) %>% summarize(count = length(unique(provider_number)))

q1.plot <- q1.data %>%
  ggplot(aes(x = fyear, y = count)) + geom_point() + geom_line() +
  scale_x_continuous(breaks = c(1997:2017)) +
  geom_text(label = q1.data$count, size = 3, nudge_x = 0, nudge_y = 10, check_overlap = TRUE) +
  labs(x = "Year", y = "Number of Hospitals", Title = "Number of Hospitals with More Than 1 Report in Each Year from 1997 to 2018") +
  theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  theme(
    plot.title = element_text(size = 12, color = "black", hjust = 0.5),
    legend.title = element_text(size = 10, color = "black"),
    legend.position = "top",
    axis.title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 10, color = "black"))

## Question 2 Unique Hospital IDs ----------------------------------------------

q2.value <- length(unique(hcris.data$provider_number))

## Question 3 Charge Distribution ----------------------------------------------

q3.sum <- hcris.data %>% group_by(year) %>% 
  summarise(
    Mean = mean(tot_charges, na.rm = TRUE),
    Min = min(tot_charges, na.rm = TRUE),
    Q1 = quantile(tot_charges, 0.25, na.rm = TRUE),
    Median = median(tot_charges, na.rm = TRUE),
    Q3 = quantile(tot_charges, 0.75, na.rm = TRUE),
    Max = max(tot_charges, na.rm = TRUE)
  ) %>%
  mutate(year = factor(year))

q3.plot <- hcris.data %>%
  ggplot(aes(x = factor(year), y = tot_charges)) +
  geom_jitter(alpha = .05) +
  geom_violin(alpha = .9, draw_quantiles = c(0.5)) +
#  scale_y_continuous(trans='log') +
  labs(x = "Year", y = "Total Charges", Title = "Distribution of Total Charges in Each Year from 1997 to 2018") +
  theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  theme(
    plot.title = element_text(size = 12, color = "black", hjust = 0.5),
    legend.title = element_text(size = 10, color = "black"),
    legend.position = "top",
    axis.title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 10, color = "black"))

## Question 4 Price Distribution -----------------------------------------------

q4.data <- hcris.data %>% mutate(discount_factor = 1-tot_discounts/tot_charges,
                            price_num = (ip_charges + icu_charges + ancillary_charges)*discount_factor - tot_mcare_payment,
                            price_denom = tot_discharges - mcare_discharges,
                            price = price_num/price_denom)

q4.sum <- q4.data %>% group_by(year) %>% 
  summarise(
    Mean = mean(price, na.rm = TRUE),
    Min = min(price, na.rm = TRUE),
    Q1 = quantile(price, 0.25, na.rm = TRUE),
    Median = median(price, na.rm = TRUE),
    Q3 = quantile(price, 0.75, na.rm = TRUE),
    Max = max(price, na.rm = TRUE)
  ) %>%
  mutate(year = factor(year))

q4.plot <- q4.data %>%
  ggplot(aes(x = factor(year), y = price)) +
  geom_jitter(alpha = .05) +
  geom_violin(alpha = .9, draw_quantiles = c(0.5)) +
#  scale_y_continuous(trans='log') +
  labs(x = "Year", y = "Estimated Prices", Title = "Distribution of Estimated Prices in Each Year from 1997 to 2018") +
  theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  theme(
    plot.title = element_text(size = 12, color = "black", hjust = 0.5),
    legend.title = element_text(size = 10, color = "black"),
    legend.position = "top",
    axis.title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 10, color = "black"))

## Question 5 Penalty ----------------------------------------------------------

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

final.hcris$bed_size <- ntile(final.hcris$beds, 4)

final.hcris <- final.hcris %>% mutate(
  bed_size1 = ifelse(bed_size == 1, 1, 0),
  bed_size2 = ifelse(bed_size == 2, 1, 0),
  bed_size3 = ifelse(bed_size == 3, 1, 0),
  bed_size4 = ifelse(bed_size == 4, 1, 0))

q6.data <- final.hcris %>% group_by(bed_size, penalty) %>% summarise(mean(price))

q6.data$bed_size <- factor(q6.data$bed_size)
levels(q6.data$bed_size) <- c("1st Quartile", "2nd Quartile", "3rd Quartile", "4th Quartile")

q6.data <- q6.data %>% 
  pivot_wider(names_from = penalty, values_from = `mean(price)`) %>%
  mutate(diff = `TRUE` - `FALSE`)
colnames(q6.data) <- c("Quartile Based on Bed Size", "Non-Penalized (Control)", "Penalized (Treated)", "Mean Difference")

## Question 7 Nearest Neighbors ------------------------------------------------

lp.vars <- final.hcris %>%
  dplyr::select(price, penalty, bed_size1, bed_size2, bed_size3)
lp.covs <- lp.vars %>% dplyr::select(bed_size1, bed_size2, bed_size3)

## nearest neighbor matching with inverse variance distance

nn_ivd <- Matching::Match(Y=lp.vars$price,
                          Tr=lp.vars$penalty,
                          X=lp.covs,
                          M=1,
                          Weight=1,
                          estimand="ATE")
summary(nn_ivd)

# love.plot(bal.tab(nn_ivd, covs = lp.covs, treat = lp.vars$penalty), 
#           threshold=0.1, 
#           grid=FALSE, sample.names=c("Unmatched", "Matched"),
#           position="top", shapes=c("circle","triangle"),
#           colors=c("black","blue")) + 
#   theme_bw()

## nearest neighbor matching with Mahalanobis distance

nn_md <- Matching::Match(Y=lp.vars$price,
                         Tr=lp.vars$penalty,
                         X=lp.covs,
                         M=1,
                         Weight=2,
                         estimand="ATE")
summary(nn_md)

# love.plot(bal.tab(nn_md, covs = lp.covs, treat = lp.vars$penalty), 
#           threshold=0.1, 
#           grid=FALSE, sample.names=c("Unmatched", "Matched"),
#           position="top", shapes=c("circle","triangle"),
#           colors=c("black","blue")) + 
#   theme_bw()

## nearest neighbor matching with propensity score distance

logit.model <- glm(penalty ~ bed_size1 + bed_size2 + bed_size3, data = lp.vars)
ps <- fitted(logit.model)
nn_ps <- Matching::Match(Y=lp.vars$price,
                         Tr=lp.vars$penalty,
                         X=ps,
                         M=1,
                         estimand="ATE")
summary(nn_ps)

# love.plot(bal.tab(nn_ps, covs = lp.covs, treat = lp.vars$penalty), 
#           threshold=0.1, 
#           grid=FALSE, sample.names=c("Unmatched", "Matched"),
#           position="top", shapes=c("circle","triangle"),
#           colors=c("black","blue")) + 
#   theme_bw()

## inverse propensity weighting (IPW) regression

lp.vars <- lp.vars %>%
  mutate(ipw = case_when(
    penalty==1 ~ 1/ps,
    penalty==0 ~ 1/(1-ps),
    TRUE ~ NA_real_
  ))
mean.t1 <- lp.vars %>% filter(penalty==1) %>%
  dplyr::select(price, ipw) %>%
  summarize(mean_p=weighted.mean(price,w=ipw))
mean.t0 <- lp.vars %>% filter(penalty==0) %>%
  dplyr::select(price, ipw) %>%
  summarize(mean_p=weighted.mean(price,w=ipw))
reg_IPW_mean1 <- mean.t1$mean_p - mean.t0$mean_p

reg_IPW <- lm(price ~ penalty, data = lp.vars, weights = ipw)
summary(reg_IPW)

# IPW weighting with Simple Linear Regression
reg_slm_1.data <- lp.vars %>% filter(penalty==1, complete.cases(.))
reg_slm_1 <- lm(price ~ bed_size1 + bed_size2 + bed_size3, data = reg_slm_1.data)
reg_slm_0.data <- lp.vars %>% filter(penalty==0, complete.cases(.))
reg_slm_0 <- lm(price ~ bed_size1 + bed_size2 + bed_size3, data = reg_slm_0.data)
pred1 <- predict(reg1, new = lp.vars)
pred0 <- predict(reg0, new = lp.vars)
reg_IPW_mean2 <- mean(pred1 - pred0)

# IPW weighting with Simple Linear Regression in one step

reg.data <- lp.vars %>% ungroup() %>% filter(complete.cases(.)) %>%
  mutate(bed_size1_diff = penalty*(bed_size1 - mean(bed_size1)),
         bed_size2_diff = penalty*(bed_size2 - mean(bed_size2)),
         bed_size3_diff = penalty*(bed_size3 - mean(bed_size3)))
reg_slm <- lm(price ~ penalty + bed_size1 + bed_size2 + bed_size3 + bed_size1_diff + bed_size2_diff + bed_size3_diff, data = reg.data)
summary(reg_slm)

q7.data <- data.frame(c(nn_ivd$est, nn_md$est, nn_ps$est, as.matrix(reg_IPW$coefficients)[2], as.matrix(reg_slm$coefficients)[2]))
rownames(q7.data) <- c("Nearest Neighbor Matching with Inverse Variance Distance",
                       "Nearest Neighbor Matching with Mahalanobis Distance",
                       "Nearest Neighbor Matching with Propensity Score Distance",
                       "Inverse Propensity Weighted Regression",
                       "Simple Linear Regression")
colnames(q7.data) <- c("Average Treatment Effect")

## Save data for markdown ------------------------------------------------------

rm(list=c("final.hcris.v1996", "final.hcris.v2010", "hcris.data", "final.hcris"))
save.image("Hwk2_workspace.Rdata")
