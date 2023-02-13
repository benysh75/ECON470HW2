## Title: ECON 470 HW2
## Author: Ben Yang
## Date Created: 2/13/2023
## Date Edited: 2/13/2023
## Descriptions: This file renders/runs all relevant R code for the assignment

## Preliminaries ---------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate)

## Read data and set workspace for knitr ---------------------------------------

final.hcris.v1996 <- read_rds('data/output/HCRIS_Data_v1996.rds')
final.hcris.v2010 <- read_rds('data/output/HCRIS_Data_v2010.rds')
final.hcris.data  <- read_rds('data/output/HCRIS_Data.rds')

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

q2.value <- length(unique(
  (final.hcris.data %>% filter(source == "unique reports"))$provider_number
  ))

q3.plot <- final.hcris.data %>%
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

q4.data <- final.hcris.data %>% mutate(discount_factor = 1-tot_discounts/tot_charges,
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

## Save data for markdown ------------------------------------------------------

rm(list=c("final.hcris.v1996", "final.hcris.v2010", "final.hcris.data", "final.hcris", "q1.data", "q4.data"))
save.image("Hwk2_workspace.Rdata")
