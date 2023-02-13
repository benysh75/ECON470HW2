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

## identify hospitals with only one report per fiscal year 
unique.hcris1 =
  final.hcris %>%
  filter(total_reports==1) %>%
  select(-report, -total_reports, -report_number, -npi, -status) %>%
  mutate(source='unique reports')

## Create objects for markdown -------------------------------------------------

q1.data <- final.hcris %>% filter(report_number > 1) %>%
  group_by(fyear) %>% summarize(count = length(unique(provider_number)))

q1.plot <- q1.data %>%
  ggplot(aes(x = fyear, y = count)) + geom_point() + geom_line() +
  labs(x = "Year", y = "Number of Hospitals", Title = "Number of Hospitals with More Than 1 Report in Each Year from 1997 to 2018") +
  theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  theme(
    plot.title = element_text(size = 12, color = "black", hjust = 0.5),
    legend.title = element_text(size = 10, color = "black"),
    legend.position = "top",
    axis.title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 10, color = "black"))

q2.value <- length(unique(unique.hcris1$provider_number))

## Save data for markdown ------------------------------------------------------

rm(list=c("final.hcris", "unique.hcris1", "final.hcris.v1996", "final.hcris.v2010", "final.hcris.data"))
save.image("Hwk2_workspace.Rdata")
