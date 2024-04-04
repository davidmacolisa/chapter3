#======================================================================================================================#
### Packages
#======================================================================================================================#
library(tidyverse)
library(stringr)
library(statar)
#======================================================================================================================#
### Working Directory
#======================================================================================================================#
setwd(dir = "C:/Users/Davidmac.Ekeocha/OneDrive/Documents/ULMS/PhD/")
#======================================================================================================================#
### Loading Data - Security Exchange Commission (SEC) Financial Statements Data---Quarterly and Yearly---Q&A
### https://www.sec.gov/data
### https://www.sec.gov/dera/data/financial-statement-data-sets
#======================================================================================================================#
### 2009Q
#======================================================================================================================#
unzip(zipfile = "./Data_PhD/US/SEC/2009q1.zip", exdir = "./Data_PhD/US/SEC/2009q1")
unzip(zipfile = "./Data_PhD/US/SEC/2009q2.zip", exdir = "./Data_PhD/US/SEC/2009q2")
unzip(zipfile = "./Data_PhD/US/SEC/2009q3.zip", exdir = "./Data_PhD/US/SEC/2009q3")
unzip(zipfile = "./Data_PhD/US/SEC/2009q4.zip", exdir = "./Data_PhD/US/SEC/2009q4")
filepath.sub09q1 <- "./Data_PhD/US/SEC/2009q1/sub.txt" # missing information for 2009q1
filepath.sub09q2 <- "./Data_PhD/US/SEC/2009q2/sub.txt"
filepath.sub09q3 <- "./Data_PhD/US/SEC/2009q3/sub.txt"
filepath.sub09q4 <- "./Data_PhD/US/SEC/2009q4/sub.txt"
# sec.sub09q1 <- read.delim(file = filepath.sub09q1, header = T, sep = "\t")
sec.sub09q2 <- read.delim(file = filepath.sub09q2, header = T, sep = "\t")
sec.sub09q3 <- read.delim(file = filepath.sub09q3, header = T, sep = "\t")
sec.sub09q4 <- read.delim(file = filepath.sub09q4, header = T, sep = "\t")
sec.sub09q <- rbind(sec.sub09q2, sec.sub09q3, sec.sub09q4)

filepath.tag09q1 <- "./Data_PhD/US/SEC/2009q1/tag.txt" # missing information for 2009q1
filepath.tag09q2 <- "./Data_PhD/US/SEC/2009q2/tag.txt"
filepath.tag09q3 <- "./Data_PhD/US/SEC/2009q3/tag.txt"
filepath.tag09q4 <- "./Data_PhD/US/SEC/2009q4/tag.txt"
# sec.tag09q1 <- read.delim(file = filepath.tag09q1, header = T, sep = "\t")
sec.tag09q2 <- read.delim(file = filepath.tag09q2, header = T, sep = "\t")
sec.tag09q3 <- read.delim(file = filepath.tag09q3, header = T, sep = "\t")
sec.tag09q4 <- read.delim(file = filepath.tag09q4, header = T, sep = "\t")
sec.tag09q <- rbind(sec.tag09q2, sec.tag09q3, sec.tag09q4)

filepath.num09q1 <- "./Data_PhD/US/SEC/2009q1/num.txt" # missing information for 2009q1
filepath.num09q2 <- "./Data_PhD/US/SEC/2009q2/num.txt"
filepath.num09q3 <- "./Data_PhD/US/SEC/2009q3/num.txt"
filepath.num09q4 <- "./Data_PhD/US/SEC/2009q4/num.txt"
# sec.num09q1 <- read.delim(file = filepath.num09q1, header = T, sep = "\t")
sec.num09q2 <- read.delim(file = filepath.num09q2, header = T, sep = "\t")
sec.num09q3 <- read.delim(file = filepath.num09q3, header = T, sep = "\t")
sec.num09q4 <- read.delim(file = filepath.num09q4, header = T, sep = "\t")
sec.num09q <- rbind(sec.num09q2, sec.num09q3, sec.num09q4)

filepath.pre09q1 <- "./Data_PhD/US/SEC/2009q1/pre.txt" # missing information for 2009q1
filepath.pre09q2 <- "./Data_PhD/US/SEC/2009q2/pre.txt"
filepath.pre09q3 <- "./Data_PhD/US/SEC/2009q3/pre.txt"
filepath.pre09q4 <- "./Data_PhD/US/SEC/2009q4/pre.txt"
# sec.pre09q1 <- read.delim(file = filepath.pre09q1, header = T, sep = "\t")
sec.pre09q2 <- read.delim(file = filepath.pre09q2, header = T, sep = "\t")
sec.pre09q3 <- read.delim(file = filepath.pre09q3, header = T, sep = "\t")
sec.pre09q4 <- read.delim(file = filepath.pre09q4, header = T, sep = "\t")
sec.pre09q <- rbind(sec.pre09q2, sec.pre09q3, sec.pre09q4)

sec.09q <- merge(x = sec.sub09q, y = sec.num09q, by = "adsh")
sec.09q <- merge(x = sec.09q, y = sec.pre09q)
sec.09q <- merge(x = sec.09q, y = sec.tag09q)
sec.09q <- sec.09q %>%
  filter(countryba == "US") %>%
  filter(fp != "FY") %>%
  select(c(adsh, cik, version, name, stprba, cityba, zipba, sic, form,
           qtrs, iord, uom, value, stmt, plabel, tlabel, datatype, fp, doc)) %>%
  mutate(year = 2009)

sec.09q[] <- lapply(sec.09q[], as.character)
# sec.09q <- sec.09q[sec.09q$stprba %in% c("CA", "OR"),]
sec.09q <- sec.09q[!duplicated(sec.09q),]
sort(unique(sec.09q$tlabel))
sort(unique(sec.09q$stprba))

sec.09q <- sec.09q %>% filter(
  str_detect(tlabel, pattern = "Assets") |
    str_detect(tlabel, pattern = "Cost") |
    str_detect(tlabel, pattern = "Dividends") |
    str_detect(tlabel, pattern = "Earnings") |
    str_detect(tlabel, pattern = "Revenue") |
    str_detect(tlabel, pattern = "Expenditures") |
    str_detect(tlabel, pattern = "Expense") |
    str_detect(tlabel, pattern = "Profit") |
    str_detect(tlabel, pattern = "Income Taxes Paid") |
    str_detect(tlabel, pattern = "Inventories") |
    str_detect(tlabel, pattern = "Inventory") |
    str_detect(tlabel, pattern = "Labor") |
    str_detect(tlabel, pattern = "LIFO") |
    str_detect(tlabel, pattern = "Taxes") |
    str_detect(tlabel, pattern = "Taxes")
)
#======================================================================================================================#
### 2010Q
#======================================================================================================================#
unzip(zipfile = "./Data_PhD/US/SEC/2010q1.zip", exdir = "./Data_PhD/US/SEC/2010q1")
unzip(zipfile = "./Data_PhD/US/SEC/2010q2.zip", exdir = "./Data_PhD/US/SEC/2010q2")
unzip(zipfile = "./Data_PhD/US/SEC/2010q3.zip", exdir = "./Data_PhD/US/SEC/2010q3")
unzip(zipfile = "./Data_PhD/US/SEC/2010q4.zip", exdir = "./Data_PhD/US/SEC/2010q4")
filepath.sub10q1 <- "./Data_PhD/US/SEC/2010q1/sub.txt" # missing information for 2010q1
filepath.sub10q2 <- "./Data_PhD/US/SEC/2010q2/sub.txt"
filepath.sub10q3 <- "./Data_PhD/US/SEC/2010q3/sub.txt"
filepath.sub10q4 <- "./Data_PhD/US/SEC/2010q4/sub.txt"
sec.sub10q1 <- read.delim(file = filepath.sub10q1, header = T, sep = "\t")
sec.sub10q2 <- read.delim(file = filepath.sub10q2, header = T, sep = "\t")
sec.sub10q3 <- read.delim(file = filepath.sub10q3, header = T, sep = "\t")
sec.sub10q4 <- read.delim(file = filepath.sub10q4, header = T, sep = "\t")
sec.sub10q <- rbind(sec.sub10q2, sec.sub10q3, sec.sub10q4)

filepath.tag10q1 <- "./Data_PhD/US/SEC/2010q1/tag.txt" # missing information for 2010q1
filepath.tag10q2 <- "./Data_PhD/US/SEC/2010q2/tag.txt"
filepath.tag10q3 <- "./Data_PhD/US/SEC/2010q3/tag.txt"
filepath.tag10q4 <- "./Data_PhD/US/SEC/2010q4/tag.txt"
sec.tag10q1 <- read.delim(file = filepath.tag10q1, header = T, sep = "\t")
sec.tag10q2 <- read.delim(file = filepath.tag10q2, header = T, sep = "\t")
sec.tag10q3 <- read.delim(file = filepath.tag10q3, header = T, sep = "\t")
sec.tag10q4 <- read.delim(file = filepath.tag10q4, header = T, sep = "\t")
sec.tag10q <- rbind(sec.tag10q2, sec.tag10q3, sec.tag10q4)

filepath.num10q1 <- "./Data_PhD/US/SEC/2010q1/num.txt" # missing information for 2010q1
filepath.num10q2 <- "./Data_PhD/US/SEC/2010q2/num.txt"
filepath.num10q3 <- "./Data_PhD/US/SEC/2010q3/num.txt"
filepath.num10q4 <- "./Data_PhD/US/SEC/2010q4/num.txt"
sec.num10q1 <- read.delim(file = filepath.num10q1, header = T, sep = "\t")
sec.num10q2 <- read.delim(file = filepath.num10q2, header = T, sep = "\t")
sec.num10q3 <- read.delim(file = filepath.num10q3, header = T, sep = "\t")
sec.num10q4 <- read.delim(file = filepath.num10q4, header = T, sep = "\t")
sec.num10q <- rbind(sec.num10q2, sec.num10q3, sec.num10q4)

filepath.pre10q1 <- "./Data_PhD/US/SEC/2010q1/pre.txt" # missing information for 2010q1
filepath.pre10q2 <- "./Data_PhD/US/SEC/2010q2/pre.txt"
filepath.pre10q3 <- "./Data_PhD/US/SEC/2010q3/pre.txt"
filepath.pre10q4 <- "./Data_PhD/US/SEC/2010q4/pre.txt"
sec.pre10q1 <- read.delim(file = filepath.pre10q1, header = T, sep = "\t")
sec.pre10q2 <- read.delim(file = filepath.pre10q2, header = T, sep = "\t")
sec.pre10q3 <- read.delim(file = filepath.pre10q3, header = T, sep = "\t")
sec.pre10q4 <- read.delim(file = filepath.pre10q4, header = T, sep = "\t")
sec.pre10q <- rbind(sec.pre10q2, sec.pre10q3, sec.pre10q4)

sec.10q <- merge(x = sec.sub10q, y = sec.num10q, by = "adsh")
sec.10q <- merge(x = sec.10q, y = sec.pre10q)
sec.10q <- merge(x = sec.10q, y = sec.tag10q)
sec.10q <- sec.10q %>%
  filter(countryba == "US") %>%
  filter(fp != "FY") %>%
  select(c(adsh, cik, version, name, stprba, cityba, zipba, sic, form,
           qtrs, iord, uom, value, stmt, plabel, tlabel, datatype, fp, doc)) %>%
  mutate(year = 2010)

sec.10q[] <- lapply(sec.10q[], as.character)
# sec.10q <- sec.10q[sec.10q$stprba %in% c("CA", "OR"),]
sec.10q <- sec.10q[!duplicated(sec.10q),]
sec.10q <- sec.10q %>% filter(
  str_detect(tlabel, pattern = "Assets") |
    str_detect(tlabel, pattern = "Cost") |
    str_detect(tlabel, pattern = "Dividends") |
    str_detect(tlabel, pattern = "Earnings") |
    str_detect(tlabel, pattern = "Revenue") |
    str_detect(tlabel, pattern = "Expenditures") |
    str_detect(tlabel, pattern = "Expense") |
    str_detect(tlabel, pattern = "Profit") |
    str_detect(tlabel, pattern = "Income Taxes Paid") |
    str_detect(tlabel, pattern = "Inventories") |
    str_detect(tlabel, pattern = "Inventory") |
    str_detect(tlabel, pattern = "Labor") |
    str_detect(tlabel, pattern = "LIFO") |
    str_detect(tlabel, pattern = "Taxes") |
    str_detect(tlabel, pattern = "Taxes")
)
#======================================================================================================================#
### 2011Q
#======================================================================================================================#
unzip(zipfile = "./Data_PhD/US/SEC/2011q1.zip", exdir = "./Data_PhD/US/SEC/2011q1")
unzip(zipfile = "./Data_PhD/US/SEC/2011q2.zip", exdir = "./Data_PhD/US/SEC/2011q2")
unzip(zipfile = "./Data_PhD/US/SEC/2011q3.zip", exdir = "./Data_PhD/US/SEC/2011q3")
unzip(zipfile = "./Data_PhD/US/SEC/2011q4.zip", exdir = "./Data_PhD/US/SEC/2011q4")
filepath.sub11q1 <- "./Data_PhD/US/SEC/2011q1/sub.txt" # missing information for 2011q1
filepath.sub11q2 <- "./Data_PhD/US/SEC/2011q2/sub.txt"
filepath.sub11q3 <- "./Data_PhD/US/SEC/2011q3/sub.txt"
filepath.sub11q4 <- "./Data_PhD/US/SEC/2011q4/sub.txt"
sec.sub11q1 <- read.delim(file = filepath.sub11q1, header = T, sep = "\t")
sec.sub11q2 <- read.delim(file = filepath.sub11q2, header = T, sep = "\t")
sec.sub11q3 <- read.delim(file = filepath.sub11q3, header = T, sep = "\t")
sec.sub11q4 <- read.delim(file = filepath.sub11q4, header = T, sep = "\t")
sec.sub11q <- rbind(sec.sub11q2, sec.sub11q3, sec.sub11q4)

filepath.tag11q1 <- "./Data_PhD/US/SEC/2011q1/tag.txt" # missing information for 2011q1
filepath.tag11q2 <- "./Data_PhD/US/SEC/2011q2/tag.txt"
filepath.tag11q3 <- "./Data_PhD/US/SEC/2011q3/tag.txt"
filepath.tag11q4 <- "./Data_PhD/US/SEC/2011q4/tag.txt"
sec.tag11q1 <- read.delim(file = filepath.tag11q1, header = T, sep = "\t")
sec.tag11q2 <- read.delim(file = filepath.tag11q2, header = T, sep = "\t")
sec.tag11q3 <- read.delim(file = filepath.tag11q3, header = T, sep = "\t")
sec.tag11q4 <- read.delim(file = filepath.tag11q4, header = T, sep = "\t")
sec.tag11q <- rbind(sec.tag11q2, sec.tag11q3, sec.tag11q4)

filepath.num11q1 <- "./Data_PhD/US/SEC/2011q1/num.txt" # missing information for 2011q1
filepath.num11q2 <- "./Data_PhD/US/SEC/2011q2/num.txt"
filepath.num11q3 <- "./Data_PhD/US/SEC/2011q3/num.txt"
filepath.num11q4 <- "./Data_PhD/US/SEC/2011q4/num.txt"
sec.num11q1 <- read.delim(file = filepath.num11q1, header = T, sep = "\t")
sec.num11q2 <- read.delim(file = filepath.num11q2, header = T, sep = "\t")
sec.num11q3 <- read.delim(file = filepath.num11q3, header = T, sep = "\t")
sec.num11q4 <- read.delim(file = filepath.num11q4, header = T, sep = "\t")
sec.num11q <- rbind(sec.num11q2, sec.num11q3, sec.num11q4)

filepath.pre11q1 <- "./Data_PhD/US/SEC/2011q1/pre.txt" # missing information for 2011q1
filepath.pre11q2 <- "./Data_PhD/US/SEC/2011q2/pre.txt"
filepath.pre11q3 <- "./Data_PhD/US/SEC/2011q3/pre.txt"
filepath.pre11q4 <- "./Data_PhD/US/SEC/2011q4/pre.txt"
sec.pre11q1 <- read.delim(file = filepath.pre11q1, header = T, sep = "\t")
sec.pre11q2 <- read.delim(file = filepath.pre11q2, header = T, sep = "\t")
sec.pre11q3 <- read.delim(file = filepath.pre11q3, header = T, sep = "\t")
sec.pre11q4 <- read.delim(file = filepath.pre11q4, header = T, sep = "\t")
sec.pre11q <- rbind(sec.pre11q2, sec.pre11q3, sec.pre11q4)

sec.11q <- merge(x = sec.sub11q, y = sec.num11q, by = "adsh")
sec.11q <- merge(x = sec.11q, y = sec.pre11q)
sec.11q <- merge(x = sec.11q, y = sec.tag11q)
sec.11q <- sec.11q %>%
  filter(countryba == "US") %>%
  filter(fp != "FY") %>%
  select(c(adsh, cik, version, name, stprba, cityba, zipba, sic, form,
           qtrs, iord, uom, value, stmt, plabel, tlabel, datatype, fp, doc)) %>%
  mutate(year = 2011)

sec.11q[] <- lapply(sec.11q[], as.character)
# sec.11q <- sec.11q[sec.11q$stprba %in% c("CA", "OR"),]
sec.11q <- sec.11q[!duplicated(sec.11q),]
sec.11q <- sec.11q %>% filter(
  str_detect(tlabel, pattern = "Assets") |
    str_detect(tlabel, pattern = "Cost") |
    str_detect(tlabel, pattern = "Dividends") |
    str_detect(tlabel, pattern = "Earnings") |
    str_detect(tlabel, pattern = "Revenue") |
    str_detect(tlabel, pattern = "Expenditures") |
    str_detect(tlabel, pattern = "Expense") |
    str_detect(tlabel, pattern = "Profit") |
    str_detect(tlabel, pattern = "Income Taxes Paid") |
    str_detect(tlabel, pattern = "Inventories") |
    str_detect(tlabel, pattern = "Inventory") |
    str_detect(tlabel, pattern = "Labor") |
    str_detect(tlabel, pattern = "LIFO") |
    str_detect(tlabel, pattern = "Taxes") |
    str_detect(tlabel, pattern = "Taxes")
)
#======================================================================================================================#
### 2012Q
#======================================================================================================================#
unzip(zipfile = "./Data_PhD/US/SEC/2012q1.zip", exdir = "./Data_PhD/US/SEC/2012q1")
unzip(zipfile = "./Data_PhD/US/SEC/2012q2.zip", exdir = "./Data_PhD/US/SEC/2012q2")
unzip(zipfile = "./Data_PhD/US/SEC/2012q3.zip", exdir = "./Data_PhD/US/SEC/2012q3")
unzip(zipfile = "./Data_PhD/US/SEC/2012q4.zip", exdir = "./Data_PhD/US/SEC/2012q4")
filepath.sub12q1 <- "./Data_PhD/US/SEC/2012q1/sub.txt" # missing information for 2012q1
filepath.sub12q2 <- "./Data_PhD/US/SEC/2012q2/sub.txt"
filepath.sub12q3 <- "./Data_PhD/US/SEC/2012q3/sub.txt"
filepath.sub12q4 <- "./Data_PhD/US/SEC/2012q4/sub.txt"
sec.sub12q1 <- read.delim(file = filepath.sub12q1, header = T, sep = "\t")
sec.sub12q2 <- read.delim(file = filepath.sub12q2, header = T, sep = "\t")
sec.sub12q3 <- read.delim(file = filepath.sub12q3, header = T, sep = "\t")
sec.sub12q4 <- read.delim(file = filepath.sub12q4, header = T, sep = "\t")
sec.sub12q <- rbind(sec.sub12q2, sec.sub12q3, sec.sub12q4)

filepath.tag12q1 <- "./Data_PhD/US/SEC/2012q1/tag.txt" # missing information for 2012q1
filepath.tag12q2 <- "./Data_PhD/US/SEC/2012q2/tag.txt"
filepath.tag12q3 <- "./Data_PhD/US/SEC/2012q3/tag.txt"
filepath.tag12q4 <- "./Data_PhD/US/SEC/2012q4/tag.txt"
sec.tag12q1 <- read.delim(file = filepath.tag12q1, header = T, sep = "\t")
sec.tag12q2 <- read.delim(file = filepath.tag12q2, header = T, sep = "\t")
sec.tag12q3 <- read.delim(file = filepath.tag12q3, header = T, sep = "\t")
sec.tag12q4 <- read.delim(file = filepath.tag12q4, header = T, sep = "\t")
sec.tag12q <- rbind(sec.tag12q2, sec.tag12q3, sec.tag12q4)

filepath.num12q1 <- "./Data_PhD/US/SEC/2012q1/num.txt" # missing information for 2012q1
filepath.num12q2 <- "./Data_PhD/US/SEC/2012q2/num.txt"
filepath.num12q3 <- "./Data_PhD/US/SEC/2012q3/num.txt"
filepath.num12q4 <- "./Data_PhD/US/SEC/2012q4/num.txt"
sec.num12q1 <- read.delim(file = filepath.num12q1, header = T, sep = "\t")
sec.num12q2 <- read.delim(file = filepath.num12q2, header = T, sep = "\t")
sec.num12q3 <- read.delim(file = filepath.num12q3, header = T, sep = "\t")
sec.num12q4 <- read.delim(file = filepath.num12q4, header = T, sep = "\t")
sec.num12q <- rbind(sec.num12q2, sec.num12q3, sec.num12q4)

filepath.pre12q1 <- "./Data_PhD/US/SEC/2012q1/pre.txt" # missing information for 2012q1
filepath.pre12q2 <- "./Data_PhD/US/SEC/2012q2/pre.txt"
filepath.pre12q3 <- "./Data_PhD/US/SEC/2012q3/pre.txt"
filepath.pre12q4 <- "./Data_PhD/US/SEC/2012q4/pre.txt"
sec.pre12q1 <- read.delim(file = filepath.pre12q1, header = T, sep = "\t")
sec.pre12q2 <- read.delim(file = filepath.pre12q2, header = T, sep = "\t")
sec.pre12q3 <- read.delim(file = filepath.pre12q3, header = T, sep = "\t")
sec.pre12q4 <- read.delim(file = filepath.pre12q4, header = T, sep = "\t")
sec.pre12q <- rbind(sec.pre12q2, sec.pre12q3, sec.pre12q4)

sec.12q <- merge(x = sec.sub12q, y = sec.num12q, by = "adsh")
sec.12q <- merge(x = sec.12q, y = sec.pre12q)
sec.12q <- merge(x = sec.12q, y = sec.tag12q)
sec.12q <- sec.12q %>%
  filter(countryba == "US") %>%
  filter(fp != "FY") %>%
  select(c(adsh, cik, version, name, stprba, cityba, zipba, sic, form,
           qtrs, iord, uom, value, stmt, plabel, tlabel, datatype, fp, doc)) %>%
  mutate(year = 2012)

sec.12q[] <- lapply(sec.12q[], as.character)
# sec.12q <- sec.12q[sec.12q$stprba %in% c("CA", "OR"),]
sec.12q <- sec.12q[!duplicated(sec.12q),]
sec.12q <- sec.12q %>% filter(
  str_detect(tlabel, pattern = "Assets") |
    str_detect(tlabel, pattern = "Cost") |
    str_detect(tlabel, pattern = "Dividends") |
    str_detect(tlabel, pattern = "Earnings") |
    str_detect(tlabel, pattern = "Revenue") |
    str_detect(tlabel, pattern = "Expenditures") |
    str_detect(tlabel, pattern = "Expense") |
    str_detect(tlabel, pattern = "Profit") |
    str_detect(tlabel, pattern = "Income Taxes Paid") |
    str_detect(tlabel, pattern = "Inventories") |
    str_detect(tlabel, pattern = "Inventory") |
    str_detect(tlabel, pattern = "Labor") |
    str_detect(tlabel, pattern = "LIFO") |
    str_detect(tlabel, pattern = "Taxes") |
    str_detect(tlabel, pattern = "Taxes")
)
#======================================================================================================================#
### 2013Q
#======================================================================================================================#
unzip(zipfile = "./Data_PhD/US/SEC/2013q1.zip", exdir = "./Data_PhD/US/SEC/2013q1")
unzip(zipfile = "./Data_PhD/US/SEC/2013q2.zip", exdir = "./Data_PhD/US/SEC/2013q2")
unzip(zipfile = "./Data_PhD/US/SEC/2013q3.zip", exdir = "./Data_PhD/US/SEC/2013q3")
unzip(zipfile = "./Data_PhD/US/SEC/2013q4.zip", exdir = "./Data_PhD/US/SEC/2013q4")
filepath.sub13q1 <- "./Data_PhD/US/SEC/2013q1/sub.txt" # missing information for 2013q1
filepath.sub13q2 <- "./Data_PhD/US/SEC/2013q2/sub.txt"
filepath.sub13q3 <- "./Data_PhD/US/SEC/2013q3/sub.txt"
filepath.sub13q4 <- "./Data_PhD/US/SEC/2013q4/sub.txt"
sec.sub13q1 <- read.delim(file = filepath.sub13q1, header = T, sep = "\t")
sec.sub13q2 <- read.delim(file = filepath.sub13q2, header = T, sep = "\t")
sec.sub13q3 <- read.delim(file = filepath.sub13q3, header = T, sep = "\t")
sec.sub13q4 <- read.delim(file = filepath.sub13q4, header = T, sep = "\t")
sec.sub13q <- rbind(sec.sub13q2, sec.sub13q3, sec.sub13q4)

filepath.tag13q1 <- "./Data_PhD/US/SEC/2013q1/tag.txt" # missing information for 2013q1
filepath.tag13q2 <- "./Data_PhD/US/SEC/2013q2/tag.txt"
filepath.tag13q3 <- "./Data_PhD/US/SEC/2013q3/tag.txt"
filepath.tag13q4 <- "./Data_PhD/US/SEC/2013q4/tag.txt"
sec.tag13q1 <- read.delim(file = filepath.tag13q1, header = T, sep = "\t")
sec.tag13q2 <- read.delim(file = filepath.tag13q2, header = T, sep = "\t")
sec.tag13q3 <- read.delim(file = filepath.tag13q3, header = T, sep = "\t")
sec.tag13q4 <- read.delim(file = filepath.tag13q4, header = T, sep = "\t")
sec.tag13q <- rbind(sec.tag13q2, sec.tag13q3, sec.tag13q4)

filepath.num13q1 <- "./Data_PhD/US/SEC/2013q1/num.txt" # missing information for 2013q1
filepath.num13q2 <- "./Data_PhD/US/SEC/2013q2/num.txt"
filepath.num13q3 <- "./Data_PhD/US/SEC/2013q3/num.txt"
filepath.num13q4 <- "./Data_PhD/US/SEC/2013q4/num.txt"
sec.num13q1 <- read.delim(file = filepath.num13q1, header = T, sep = "\t")
sec.num13q2 <- read.delim(file = filepath.num13q2, header = T, sep = "\t")
sec.num13q3 <- read.delim(file = filepath.num13q3, header = T, sep = "\t")
sec.num13q4 <- read.delim(file = filepath.num13q4, header = T, sep = "\t")
sec.num13q <- rbind(sec.num13q2, sec.num13q3, sec.num13q4)

filepath.pre13q1 <- "./Data_PhD/US/SEC/2013q1/pre.txt" # missing information for 2013q1
filepath.pre13q2 <- "./Data_PhD/US/SEC/2013q2/pre.txt"
filepath.pre13q3 <- "./Data_PhD/US/SEC/2013q3/pre.txt"
filepath.pre13q4 <- "./Data_PhD/US/SEC/2013q4/pre.txt"
sec.pre13q1 <- read.delim(file = filepath.pre13q1, header = T, sep = "\t")
sec.pre13q2 <- read.delim(file = filepath.pre13q2, header = T, sep = "\t")
sec.pre13q3 <- read.delim(file = filepath.pre13q3, header = T, sep = "\t")
sec.pre13q4 <- read.delim(file = filepath.pre13q4, header = T, sep = "\t")
sec.pre13q <- rbind(sec.pre13q2, sec.pre13q3, sec.pre13q4)

sec.13q <- merge(x = sec.sub13q, y = sec.num13q, by = "adsh")
sec.13q <- merge(x = sec.13q, y = sec.pre13q)
sec.13q <- merge(x = sec.13q, y = sec.tag13q)
sec.13q <- sec.13q %>%
  filter(countryba == "US") %>%
  filter(fp != "FY") %>%
  select(c(adsh, cik, version, name, stprba, cityba, zipba, sic, form,
           qtrs, iord, uom, value, stmt, plabel, tlabel, datatype, fp, doc)) %>%
  mutate(year = 2013)

sec.13q[] <- lapply(sec.13q[], as.character)
# sec.13q <- sec.13q[sec.13q$stprba %in% c("CA", "OR"),]
sec.13q <- sec.13q[!duplicated(sec.13q),]
sec.13q <- sec.13q %>% filter(
  str_detect(tlabel, pattern = "Assets") |
    str_detect(tlabel, pattern = "Cost") |
    str_detect(tlabel, pattern = "Dividends") |
    str_detect(tlabel, pattern = "Earnings") |
    str_detect(tlabel, pattern = "Revenue") |
    str_detect(tlabel, pattern = "Expenditures") |
    str_detect(tlabel, pattern = "Expense") |
    str_detect(tlabel, pattern = "Profit") |
    str_detect(tlabel, pattern = "Income Taxes Paid") |
    str_detect(tlabel, pattern = "Inventories") |
    str_detect(tlabel, pattern = "Inventory") |
    str_detect(tlabel, pattern = "Labor") |
    str_detect(tlabel, pattern = "LIFO") |
    str_detect(tlabel, pattern = "Taxes") |
    str_detect(tlabel, pattern = "Taxes")
)
#======================================================================================================================#
### 2014Q
#======================================================================================================================#
unzip(zipfile = "./Data_PhD/US/SEC/2014q1.zip", exdir = "./Data_PhD/US/SEC/2014q1")
unzip(zipfile = "./Data_PhD/US/SEC/2014q2.zip", exdir = "./Data_PhD/US/SEC/2014q2")
unzip(zipfile = "./Data_PhD/US/SEC/2014q3.zip", exdir = "./Data_PhD/US/SEC/2014q3")
unzip(zipfile = "./Data_PhD/US/SEC/2014q4.zip", exdir = "./Data_PhD/US/SEC/2014q4")
filepath.sub14q1 <- "./Data_PhD/US/SEC/2014q1/sub.txt" # missing information for 2014q1
filepath.sub14q2 <- "./Data_PhD/US/SEC/2014q2/sub.txt"
filepath.sub14q3 <- "./Data_PhD/US/SEC/2014q3/sub.txt"
filepath.sub14q4 <- "./Data_PhD/US/SEC/2014q4/sub.txt"
sec.sub14q1 <- read.delim(file = filepath.sub14q1, header = T, sep = "\t")
sec.sub14q2 <- read.delim(file = filepath.sub14q2, header = T, sep = "\t")
sec.sub14q3 <- read.delim(file = filepath.sub14q3, header = T, sep = "\t")
sec.sub14q4 <- read.delim(file = filepath.sub14q4, header = T, sep = "\t")
sec.sub14q <- rbind(sec.sub14q2, sec.sub14q3, sec.sub14q4)

filepath.tag14q1 <- "./Data_PhD/US/SEC/2014q1/tag.txt" # missing information for 2014q1
filepath.tag14q2 <- "./Data_PhD/US/SEC/2014q2/tag.txt"
filepath.tag14q3 <- "./Data_PhD/US/SEC/2014q3/tag.txt"
filepath.tag14q4 <- "./Data_PhD/US/SEC/2014q4/tag.txt"
sec.tag14q1 <- read.delim(file = filepath.tag14q1, header = T, sep = "\t")
sec.tag14q2 <- read.delim(file = filepath.tag14q2, header = T, sep = "\t")
sec.tag14q3 <- read.delim(file = filepath.tag14q3, header = T, sep = "\t")
sec.tag14q4 <- read.delim(file = filepath.tag14q4, header = T, sep = "\t")
sec.tag14q <- rbind(sec.tag14q2, sec.tag14q3, sec.tag14q4)

filepath.num14q1 <- "./Data_PhD/US/SEC/2014q1/num.txt" # missing information for 2014q1
filepath.num14q2 <- "./Data_PhD/US/SEC/2014q2/num.txt"
filepath.num14q3 <- "./Data_PhD/US/SEC/2014q3/num.txt"
filepath.num14q4 <- "./Data_PhD/US/SEC/2014q4/num.txt"
sec.num14q1 <- read.delim(file = filepath.num14q1, header = T, sep = "\t")
sec.num14q2 <- read.delim(file = filepath.num14q2, header = T, sep = "\t")
sec.num14q3 <- read.delim(file = filepath.num14q3, header = T, sep = "\t")
sec.num14q4 <- read.delim(file = filepath.num14q4, header = T, sep = "\t")
sec.num14q <- rbind(sec.num14q2, sec.num14q3, sec.num14q4)

filepath.pre14q1 <- "./Data_PhD/US/SEC/2014q1/pre.txt" # missing information for 2014q1
filepath.pre14q2 <- "./Data_PhD/US/SEC/2014q2/pre.txt"
filepath.pre14q3 <- "./Data_PhD/US/SEC/2014q3/pre.txt"
filepath.pre14q4 <- "./Data_PhD/US/SEC/2014q4/pre.txt"
sec.pre14q1 <- read.delim(file = filepath.pre14q1, header = T, sep = "\t")
sec.pre14q2 <- read.delim(file = filepath.pre14q2, header = T, sep = "\t")
sec.pre14q3 <- read.delim(file = filepath.pre14q3, header = T, sep = "\t")
sec.pre14q4 <- read.delim(file = filepath.pre14q4, header = T, sep = "\t")
sec.pre14q <- rbind(sec.pre14q2, sec.pre14q3, sec.pre14q4)

sec.14q <- merge(x = sec.sub14q, y = sec.num14q, by = "adsh")
sec.14q <- merge(x = sec.14q, y = sec.pre14q)
sec.14q <- merge(x = sec.14q, y = sec.tag14q)
sec.14q <- sec.14q %>%
  filter(countryba == "US") %>%
  filter(fp != "FY") %>%
  select(c(adsh, cik, version, name, stprba, cityba, zipba, sic, form,
           qtrs, iord, uom, value, stmt, plabel, tlabel, datatype, fp, doc)) %>%
  mutate(year = 2014)

sec.14q[] <- lapply(sec.14q[], as.character)
# sec.14q <- sec.14q[sec.14q$stprba %in% c("CA", "OR"),]
sec.14q <- sec.14q[!duplicated(sec.14q),]
sec.14q <- sec.14q %>% filter(
  str_detect(tlabel, pattern = "Assets") |
    str_detect(tlabel, pattern = "Cost") |
    str_detect(tlabel, pattern = "Dividends") |
    str_detect(tlabel, pattern = "Earnings") |
    str_detect(tlabel, pattern = "Revenue") |
    str_detect(tlabel, pattern = "Expenditures") |
    str_detect(tlabel, pattern = "Expense") |
    str_detect(tlabel, pattern = "Profit") |
    str_detect(tlabel, pattern = "Income Taxes Paid") |
    str_detect(tlabel, pattern = "Inventories") |
    str_detect(tlabel, pattern = "Inventory") |
    str_detect(tlabel, pattern = "Labor") |
    str_detect(tlabel, pattern = "LIFO") |
    str_detect(tlabel, pattern = "Taxes") |
    str_detect(tlabel, pattern = "Taxes")
)
#======================================================================================================================#
### 2015Q
#======================================================================================================================#
unzip(zipfile = "./Data_PhD/US/SEC/2015q1.zip", exdir = "./Data_PhD/US/SEC/2015q1")
unzip(zipfile = "./Data_PhD/US/SEC/2015q2.zip", exdir = "./Data_PhD/US/SEC/2015q2")
unzip(zipfile = "./Data_PhD/US/SEC/2015q3.zip", exdir = "./Data_PhD/US/SEC/2015q3")
unzip(zipfile = "./Data_PhD/US/SEC/2015q4.zip", exdir = "./Data_PhD/US/SEC/2015q4")
filepath.sub15q1 <- "./Data_PhD/US/SEC/2015q1/sub.txt" # missing information for 2015q1
filepath.sub15q2 <- "./Data_PhD/US/SEC/2015q2/sub.txt"
filepath.sub15q3 <- "./Data_PhD/US/SEC/2015q3/sub.txt"
filepath.sub15q4 <- "./Data_PhD/US/SEC/2015q4/sub.txt"
sec.sub15q1 <- read.delim(file = filepath.sub15q1, header = T, sep = "\t")
sec.sub15q2 <- read.delim(file = filepath.sub15q2, header = T, sep = "\t")
sec.sub15q3 <- read.delim(file = filepath.sub15q3, header = T, sep = "\t")
sec.sub15q4 <- read.delim(file = filepath.sub15q4, header = T, sep = "\t")
sec.sub15q <- rbind(sec.sub15q2, sec.sub15q3, sec.sub15q4)

filepath.tag15q1 <- "./Data_PhD/US/SEC/2015q1/tag.txt" # missing information for 2015q1
filepath.tag15q2 <- "./Data_PhD/US/SEC/2015q2/tag.txt"
filepath.tag15q3 <- "./Data_PhD/US/SEC/2015q3/tag.txt"
filepath.tag15q4 <- "./Data_PhD/US/SEC/2015q4/tag.txt"
sec.tag15q1 <- read.delim(file = filepath.tag15q1, header = T, sep = "\t")
sec.tag15q2 <- read.delim(file = filepath.tag15q2, header = T, sep = "\t")
sec.tag15q3 <- read.delim(file = filepath.tag15q3, header = T, sep = "\t")
sec.tag15q4 <- read.delim(file = filepath.tag15q4, header = T, sep = "\t")
sec.tag15q <- rbind(sec.tag15q2, sec.tag15q3, sec.tag15q4)

filepath.num15q1 <- "./Data_PhD/US/SEC/2015q1/num.txt" # missing information for 2015q1
filepath.num15q2 <- "./Data_PhD/US/SEC/2015q2/num.txt"
filepath.num15q3 <- "./Data_PhD/US/SEC/2015q3/num.txt"
filepath.num15q4 <- "./Data_PhD/US/SEC/2015q4/num.txt"
sec.num15q1 <- read.delim(file = filepath.num15q1, header = T, sep = "\t")
sec.num15q2 <- read.delim(file = filepath.num15q2, header = T, sep = "\t")
sec.num15q3 <- read.delim(file = filepath.num15q3, header = T, sep = "\t")
sec.num15q4 <- read.delim(file = filepath.num15q4, header = T, sep = "\t")
sec.num15q <- rbind(sec.num15q2, sec.num15q3, sec.num15q4)

filepath.pre15q1 <- "./Data_PhD/US/SEC/2015q1/pre.txt" # missing information for 2015q1
filepath.pre15q2 <- "./Data_PhD/US/SEC/2015q2/pre.txt"
filepath.pre15q3 <- "./Data_PhD/US/SEC/2015q3/pre.txt"
filepath.pre15q4 <- "./Data_PhD/US/SEC/2015q4/pre.txt"
sec.pre15q1 <- read.delim(file = filepath.pre15q1, header = T, sep = "\t")
sec.pre15q2 <- read.delim(file = filepath.pre15q2, header = T, sep = "\t")
sec.pre15q3 <- read.delim(file = filepath.pre15q3, header = T, sep = "\t")
sec.pre15q4 <- read.delim(file = filepath.pre15q4, header = T, sep = "\t")
sec.pre15q <- rbind(sec.pre15q2, sec.pre15q3, sec.pre15q4)

sec.15q <- merge(x = sec.sub15q, y = sec.num15q, by = "adsh")
sec.15q <- merge(x = sec.15q, y = sec.pre15q)
sec.15q <- merge(x = sec.15q, y = sec.tag15q)
sec.15q <- sec.15q %>%
  filter(countryba == "US") %>%
  filter(fp != "FY") %>%
  select(c(adsh, cik, version, name, stprba, cityba, zipba, sic, form,
           qtrs, iord, uom, value, stmt, plabel, tlabel, datatype, fp, doc)) %>%
  mutate(year = 2015)

sec.15q[] <- lapply(sec.15q[], as.character)
# sec.15q <- sec.15q[sec.15q$stprba %in% c("CA", "OR"),]
sec.15q <- sec.15q[!duplicated(sec.15q),]
sec.15q <- sec.15q %>% filter(
  str_detect(tlabel, pattern = "Assets") |
    str_detect(tlabel, pattern = "Cost") |
    str_detect(tlabel, pattern = "Dividends") |
    str_detect(tlabel, pattern = "Earnings") |
    str_detect(tlabel, pattern = "Revenue") |
    str_detect(tlabel, pattern = "Expenditures") |
    str_detect(tlabel, pattern = "Expense") |
    str_detect(tlabel, pattern = "Profit") |
    str_detect(tlabel, pattern = "Income Taxes Paid") |
    str_detect(tlabel, pattern = "Inventories") |
    str_detect(tlabel, pattern = "Inventory") |
    str_detect(tlabel, pattern = "Labor") |
    str_detect(tlabel, pattern = "LIFO") |
    str_detect(tlabel, pattern = "Taxes") |
    str_detect(tlabel, pattern = "Taxes")
)
#======================================================================================================================#
### 2016Q
#======================================================================================================================#
unzip(zipfile = "./Data_PhD/US/SEC/2016q1.zip", exdir = "./Data_PhD/US/SEC/2016q1")
unzip(zipfile = "./Data_PhD/US/SEC/2016q2.zip", exdir = "./Data_PhD/US/SEC/2016q2")
unzip(zipfile = "./Data_PhD/US/SEC/2016q3.zip", exdir = "./Data_PhD/US/SEC/2016q3")
unzip(zipfile = "./Data_PhD/US/SEC/2016q4.zip", exdir = "./Data_PhD/US/SEC/2016q4")
filepath.sub16q1 <- "./Data_PhD/US/SEC/2016q1/sub.txt" # missing information for 2016q1
filepath.sub16q2 <- "./Data_PhD/US/SEC/2016q2/sub.txt"
filepath.sub16q3 <- "./Data_PhD/US/SEC/2016q3/sub.txt"
filepath.sub16q4 <- "./Data_PhD/US/SEC/2016q4/sub.txt"
sec.sub16q1 <- read.delim(file = filepath.sub16q1, header = T, sep = "\t")
sec.sub16q2 <- read.delim(file = filepath.sub16q2, header = T, sep = "\t")
sec.sub16q3 <- read.delim(file = filepath.sub16q3, header = T, sep = "\t")
sec.sub16q4 <- read.delim(file = filepath.sub16q4, header = T, sep = "\t")
sec.sub16q <- rbind(sec.sub16q2, sec.sub16q3, sec.sub16q4)

filepath.tag16q1 <- "./Data_PhD/US/SEC/2016q1/tag.txt" # missing information for 2016q1
filepath.tag16q2 <- "./Data_PhD/US/SEC/2016q2/tag.txt"
filepath.tag16q3 <- "./Data_PhD/US/SEC/2016q3/tag.txt"
filepath.tag16q4 <- "./Data_PhD/US/SEC/2016q4/tag.txt"
sec.tag16q1 <- read.delim(file = filepath.tag16q1, header = T, sep = "\t")
sec.tag16q2 <- read.delim(file = filepath.tag16q2, header = T, sep = "\t")
sec.tag16q3 <- read.delim(file = filepath.tag16q3, header = T, sep = "\t")
sec.tag16q4 <- read.delim(file = filepath.tag16q4, header = T, sep = "\t")
sec.tag16q <- rbind(sec.tag16q2, sec.tag16q3, sec.tag16q4)

filepath.num16q1 <- "./Data_PhD/US/SEC/2016q1/num.txt" # missing information for 2016q1
filepath.num16q2 <- "./Data_PhD/US/SEC/2016q2/num.txt"
filepath.num16q3 <- "./Data_PhD/US/SEC/2016q3/num.txt"
filepath.num16q4 <- "./Data_PhD/US/SEC/2016q4/num.txt"
sec.num16q1 <- read.delim(file = filepath.num16q1, header = T, sep = "\t")
sec.num16q2 <- read.delim(file = filepath.num16q2, header = T, sep = "\t")
sec.num16q3 <- read.delim(file = filepath.num16q3, header = T, sep = "\t")
sec.num16q4 <- read.delim(file = filepath.num16q4, header = T, sep = "\t")
sec.num16q <- rbind(sec.num16q2, sec.num16q3, sec.num16q4)

filepath.pre16q1 <- "./Data_PhD/US/SEC/2016q1/pre.txt" # missing information for 2016q1
filepath.pre16q2 <- "./Data_PhD/US/SEC/2016q2/pre.txt"
filepath.pre16q3 <- "./Data_PhD/US/SEC/2016q3/pre.txt"
filepath.pre16q4 <- "./Data_PhD/US/SEC/2016q4/pre.txt"
sec.pre16q1 <- read.delim(file = filepath.pre16q1, header = T, sep = "\t")
sec.pre16q2 <- read.delim(file = filepath.pre16q2, header = T, sep = "\t")
sec.pre16q3 <- read.delim(file = filepath.pre16q3, header = T, sep = "\t")
sec.pre16q4 <- read.delim(file = filepath.pre16q4, header = T, sep = "\t")
sec.pre16q <- rbind(sec.pre16q2, sec.pre16q3, sec.pre16q4)

sec.16q <- merge(x = sec.sub16q, y = sec.num16q, by = "adsh")
sec.16q <- merge(x = sec.16q, y = sec.pre16q)
sec.16q <- merge(x = sec.16q, y = sec.tag16q)
sec.16q <- sec.16q %>%
  filter(countryba == "US") %>%
  filter(fp != "FY") %>%
  select(c(adsh, cik, version, name, stprba, cityba, zipba, sic, form,
           qtrs, iord, uom, value, stmt, plabel, tlabel, datatype, fp, doc)) %>%
  mutate(year = 2016)

sec.16q[] <- lapply(sec.16q[], as.character)
# sec.16q <- sec.16q[sec.16q$stprba %in% c("CA", "OR"),]
sec.16q <- sec.16q[!duplicated(sec.16q),]
sec.16q <- sec.16q %>% filter(
  str_detect(tlabel, pattern = "Assets") |
    str_detect(tlabel, pattern = "Cost") |
    str_detect(tlabel, pattern = "Dividends") |
    str_detect(tlabel, pattern = "Earnings") |
    str_detect(tlabel, pattern = "Revenue") |
    str_detect(tlabel, pattern = "Expenditures") |
    str_detect(tlabel, pattern = "Expense") |
    str_detect(tlabel, pattern = "Profit") |
    str_detect(tlabel, pattern = "Income Taxes Paid") |
    str_detect(tlabel, pattern = "Inventories") |
    str_detect(tlabel, pattern = "Inventory") |
    str_detect(tlabel, pattern = "Labor") |
    str_detect(tlabel, pattern = "LIFO") |
    str_detect(tlabel, pattern = "Taxes") |
    str_detect(tlabel, pattern = "Taxes")
)
#======================================================================================================================#
### 2017Q
#======================================================================================================================#
unzip(zipfile = "./Data_PhD/US/SEC/2017q1.zip", exdir = "./Data_PhD/US/SEC/2017q1")
unzip(zipfile = "./Data_PhD/US/SEC/2017q2.zip", exdir = "./Data_PhD/US/SEC/2017q2")
unzip(zipfile = "./Data_PhD/US/SEC/2017q3.zip", exdir = "./Data_PhD/US/SEC/2017q3")
unzip(zipfile = "./Data_PhD/US/SEC/2017q4.zip", exdir = "./Data_PhD/US/SEC/2017q4")
filepath.sub17q1 <- "./Data_PhD/US/SEC/2017q1/sub.txt" # missing information for 2017q1
filepath.sub17q2 <- "./Data_PhD/US/SEC/2017q2/sub.txt"
filepath.sub17q3 <- "./Data_PhD/US/SEC/2017q3/sub.txt"
filepath.sub17q4 <- "./Data_PhD/US/SEC/2017q4/sub.txt"
sec.sub17q1 <- read.delim(file = filepath.sub17q1, header = T, sep = "\t")
sec.sub17q2 <- read.delim(file = filepath.sub17q2, header = T, sep = "\t")
sec.sub17q3 <- read.delim(file = filepath.sub17q3, header = T, sep = "\t")
sec.sub17q4 <- read.delim(file = filepath.sub17q4, header = T, sep = "\t")
sec.sub17q <- rbind(sec.sub17q2, sec.sub17q3, sec.sub17q4)

filepath.tag17q1 <- "./Data_PhD/US/SEC/2017q1/tag.txt" # missing information for 2017q1
filepath.tag17q2 <- "./Data_PhD/US/SEC/2017q2/tag.txt"
filepath.tag17q3 <- "./Data_PhD/US/SEC/2017q3/tag.txt"
filepath.tag17q4 <- "./Data_PhD/US/SEC/2017q4/tag.txt"
sec.tag17q1 <- read.delim(file = filepath.tag17q1, header = T, sep = "\t")
sec.tag17q2 <- read.delim(file = filepath.tag17q2, header = T, sep = "\t")
sec.tag17q3 <- read.delim(file = filepath.tag17q3, header = T, sep = "\t")
sec.tag17q4 <- read.delim(file = filepath.tag17q4, header = T, sep = "\t")
sec.tag17q <- rbind(sec.tag17q2, sec.tag17q3, sec.tag17q4)

filepath.num17q1 <- "./Data_PhD/US/SEC/2017q1/num.txt" # missing information for 2017q1
filepath.num17q2 <- "./Data_PhD/US/SEC/2017q2/num.txt"
filepath.num17q3 <- "./Data_PhD/US/SEC/2017q3/num.txt"
filepath.num17q4 <- "./Data_PhD/US/SEC/2017q4/num.txt"
sec.num17q1 <- read.delim(file = filepath.num17q1, header = T, sep = "\t")
sec.num17q2 <- read.delim(file = filepath.num17q2, header = T, sep = "\t")
sec.num17q3 <- read.delim(file = filepath.num17q3, header = T, sep = "\t")
sec.num17q4 <- read.delim(file = filepath.num17q4, header = T, sep = "\t")
sec.num17q <- rbind(sec.num17q2, sec.num17q3, sec.num17q4)

filepath.pre17q1 <- "./Data_PhD/US/SEC/2017q1/pre.txt" # missing information for 2017q1
filepath.pre17q2 <- "./Data_PhD/US/SEC/2017q2/pre.txt"
filepath.pre17q3 <- "./Data_PhD/US/SEC/2017q3/pre.txt"
filepath.pre17q4 <- "./Data_PhD/US/SEC/2017q4/pre.txt"
sec.pre17q1 <- read.delim(file = filepath.pre17q1, header = T, sep = "\t")
sec.pre17q2 <- read.delim(file = filepath.pre17q2, header = T, sep = "\t")
sec.pre17q3 <- read.delim(file = filepath.pre17q3, header = T, sep = "\t")
sec.pre17q4 <- read.delim(file = filepath.pre17q4, header = T, sep = "\t")
sec.pre17q <- rbind(sec.pre17q2, sec.pre17q3, sec.pre17q4)

sec.17q <- merge(x = sec.sub17q, y = sec.num17q, by = "adsh")
sec.17q <- merge(x = sec.17q, y = sec.pre17q)
sec.17q <- merge(x = sec.17q, y = sec.tag17q)
sec.17q <- sec.17q %>%
  filter(countryba == "US") %>%
  filter(fp != "FY") %>%
  select(c(adsh, cik, version, name, stprba, cityba, zipba, sic, form,
           qtrs, iord, uom, value, stmt, plabel, tlabel, datatype, fp, doc)) %>%
  mutate(year = 2017)

sec.17q[] <- lapply(sec.17q[], as.character)
# sec.17q <- sec.17q[sec.17q$stprba %in% c("CA", "OR"),]
sec.17q <- sec.17q[!duplicated(sec.17q),]
sec.17q <- sec.17q %>% filter(
  str_detect(tlabel, pattern = "Assets") |
    str_detect(tlabel, pattern = "Cost") |
    str_detect(tlabel, pattern = "Dividends") |
    str_detect(tlabel, pattern = "Earnings") |
    str_detect(tlabel, pattern = "Revenue") |
    str_detect(tlabel, pattern = "Expenditures") |
    str_detect(tlabel, pattern = "Expense") |
    str_detect(tlabel, pattern = "Profit") |
    str_detect(tlabel, pattern = "Income Taxes Paid") |
    str_detect(tlabel, pattern = "Inventories") |
    str_detect(tlabel, pattern = "Inventory") |
    str_detect(tlabel, pattern = "Labor") |
    str_detect(tlabel, pattern = "LIFO") |
    str_detect(tlabel, pattern = "Taxes") |
    str_detect(tlabel, pattern = "Taxes")
)
#======================================================================================================================#
### 2018Q
#======================================================================================================================#
unzip(zipfile = "./Data_PhD/US/SEC/2018q1.zip", exdir = "./Data_PhD/US/SEC/2018q1")
unzip(zipfile = "./Data_PhD/US/SEC/2018q2.zip", exdir = "./Data_PhD/US/SEC/2018q2")
unzip(zipfile = "./Data_PhD/US/SEC/2018q3.zip", exdir = "./Data_PhD/US/SEC/2018q3")
unzip(zipfile = "./Data_PhD/US/SEC/2018q4.zip", exdir = "./Data_PhD/US/SEC/2018q4")
filepath.sub18q1 <- "./Data_PhD/US/SEC/2018q1/sub.txt" # missing information for 2018q1
filepath.sub18q2 <- "./Data_PhD/US/SEC/2018q2/sub.txt"
filepath.sub18q3 <- "./Data_PhD/US/SEC/2018q3/sub.txt"
filepath.sub18q4 <- "./Data_PhD/US/SEC/2018q4/sub.txt"
sec.sub18q1 <- read.delim(file = filepath.sub18q1, header = T, sep = "\t")
sec.sub18q2 <- read.delim(file = filepath.sub18q2, header = T, sep = "\t")
sec.sub18q3 <- read.delim(file = filepath.sub18q3, header = T, sep = "\t")
sec.sub18q4 <- read.delim(file = filepath.sub18q4, header = T, sep = "\t")
sec.sub18q <- rbind(sec.sub18q2, sec.sub18q3, sec.sub18q4)

filepath.tag18q1 <- "./Data_PhD/US/SEC/2018q1/tag.txt" # missing information for 2018q1
filepath.tag18q2 <- "./Data_PhD/US/SEC/2018q2/tag.txt"
filepath.tag18q3 <- "./Data_PhD/US/SEC/2018q3/tag.txt"
filepath.tag18q4 <- "./Data_PhD/US/SEC/2018q4/tag.txt"
sec.tag18q1 <- read.delim(file = filepath.tag18q1, header = T, sep = "\t")
sec.tag18q2 <- read.delim(file = filepath.tag18q2, header = T, sep = "\t")
sec.tag18q3 <- read.delim(file = filepath.tag18q3, header = T, sep = "\t")
sec.tag18q4 <- read.delim(file = filepath.tag18q4, header = T, sep = "\t")
sec.tag18q <- rbind(sec.tag18q2, sec.tag18q3, sec.tag18q4)

filepath.num18q1 <- "./Data_PhD/US/SEC/2018q1/num.txt" # missing information for 2018q1
filepath.num18q2 <- "./Data_PhD/US/SEC/2018q2/num.txt"
filepath.num18q3 <- "./Data_PhD/US/SEC/2018q3/num.txt"
filepath.num18q4 <- "./Data_PhD/US/SEC/2018q4/num.txt"
sec.num18q1 <- read.delim(file = filepath.num18q1, header = T, sep = "\t")
sec.num18q2 <- read.delim(file = filepath.num18q2, header = T, sep = "\t")
sec.num18q3 <- read.delim(file = filepath.num18q3, header = T, sep = "\t")
sec.num18q4 <- read.delim(file = filepath.num18q4, header = T, sep = "\t")
sec.num18q <- rbind(sec.num18q2, sec.num18q3, sec.num18q4)

filepath.pre18q1 <- "./Data_PhD/US/SEC/2018q1/pre.txt" # missing information for 2018q1
filepath.pre18q2 <- "./Data_PhD/US/SEC/2018q2/pre.txt"
filepath.pre18q3 <- "./Data_PhD/US/SEC/2018q3/pre.txt"
filepath.pre18q4 <- "./Data_PhD/US/SEC/2018q4/pre.txt"
sec.pre18q1 <- read.delim(file = filepath.pre18q1, header = T, sep = "\t")
sec.pre18q2 <- read.delim(file = filepath.pre18q2, header = T, sep = "\t")
sec.pre18q3 <- read.delim(file = filepath.pre18q3, header = T, sep = "\t")
sec.pre18q4 <- read.delim(file = filepath.pre18q4, header = T, sep = "\t")
sec.pre18q <- rbind(sec.pre18q2, sec.pre18q3, sec.pre18q4)

sec.18q <- merge(x = sec.sub18q, y = sec.num18q, by = "adsh")
sec.18q <- merge(x = sec.18q, y = sec.pre18q)
sec.18q <- merge(x = sec.18q, y = sec.tag18q)
sec.18q <- sec.18q %>%
  filter(countryba == "US") %>%
  filter(fp != "FY") %>%
  select(c(adsh, cik, version, name, stprba, cityba, zipba, sic, form,
           qtrs, iord, uom, value, stmt, plabel, tlabel, datatype, fp, doc)) %>%
  mutate(year = 2018)

sec.18q[] <- lapply(sec.18q[], as.character)
# sec.18q <- sec.18q[sec.18q$stprba %in% c("CA", "OR"),]
sec.18q <- sec.18q[!duplicated(sec.18q),]
sec.18q <- sec.18q %>% filter(
  str_detect(tlabel, pattern = "Assets") |
    str_detect(tlabel, pattern = "Cost") |
    str_detect(tlabel, pattern = "Dividends") |
    str_detect(tlabel, pattern = "Earnings") |
    str_detect(tlabel, pattern = "Revenue") |
    str_detect(tlabel, pattern = "Expenditures") |
    str_detect(tlabel, pattern = "Expense") |
    str_detect(tlabel, pattern = "Profit") |
    str_detect(tlabel, pattern = "Income Taxes Paid") |
    str_detect(tlabel, pattern = "Inventories") |
    str_detect(tlabel, pattern = "Inventory") |
    str_detect(tlabel, pattern = "Labor") |
    str_detect(tlabel, pattern = "LIFO") |
    str_detect(tlabel, pattern = "Taxes") |
    str_detect(tlabel, pattern = "Taxes")
)
#======================================================================================================================#
### 2019Q
#======================================================================================================================#
unzip(zipfile = "./Data_PhD/US/SEC/2019q1.zip", exdir = "./Data_PhD/US/SEC/2019q1")
unzip(zipfile = "./Data_PhD/US/SEC/2019q2.zip", exdir = "./Data_PhD/US/SEC/2019q2")
unzip(zipfile = "./Data_PhD/US/SEC/2019q3.zip", exdir = "./Data_PhD/US/SEC/2019q3")
unzip(zipfile = "./Data_PhD/US/SEC/2019q4.zip", exdir = "./Data_PhD/US/SEC/2019q4")
filepath.sub19q1 <- "./Data_PhD/US/SEC/2019q1/sub.txt" # missing information for 2019q1
filepath.sub19q2 <- "./Data_PhD/US/SEC/2019q2/sub.txt"
filepath.sub19q3 <- "./Data_PhD/US/SEC/2019q3/sub.txt"
filepath.sub19q4 <- "./Data_PhD/US/SEC/2019q4/sub.txt"
sec.sub19q1 <- read.delim(file = filepath.sub19q1, header = T, sep = "\t")
sec.sub19q2 <- read.delim(file = filepath.sub19q2, header = T, sep = "\t")
sec.sub19q3 <- read.delim(file = filepath.sub19q3, header = T, sep = "\t")
sec.sub19q4 <- read.delim(file = filepath.sub19q4, header = T, sep = "\t")
sec.sub19q <- rbind(sec.sub19q2, sec.sub19q3, sec.sub19q4)

filepath.tag19q1 <- "./Data_PhD/US/SEC/2019q1/tag.txt" # missing information for 2019q1
filepath.tag19q2 <- "./Data_PhD/US/SEC/2019q2/tag.txt"
filepath.tag19q3 <- "./Data_PhD/US/SEC/2019q3/tag.txt"
filepath.tag19q4 <- "./Data_PhD/US/SEC/2019q4/tag.txt"
sec.tag19q1 <- read.delim(file = filepath.tag19q1, header = T, sep = "\t")
sec.tag19q2 <- read.delim(file = filepath.tag19q2, header = T, sep = "\t")
sec.tag19q3 <- read.delim(file = filepath.tag19q3, header = T, sep = "\t")
sec.tag19q4 <- read.delim(file = filepath.tag19q4, header = T, sep = "\t")
sec.tag19q <- rbind(sec.tag19q2, sec.tag19q3, sec.tag19q4)

filepath.num19q1 <- "./Data_PhD/US/SEC/2019q1/num.txt" # missing information for 2019q1
filepath.num19q2 <- "./Data_PhD/US/SEC/2019q2/num.txt"
filepath.num19q3 <- "./Data_PhD/US/SEC/2019q3/num.txt"
filepath.num19q4 <- "./Data_PhD/US/SEC/2019q4/num.txt"
sec.num19q1 <- read.delim(file = filepath.num19q1, header = T, sep = "\t")
sec.num19q2 <- read.delim(file = filepath.num19q2, header = T, sep = "\t")
sec.num19q3 <- read.delim(file = filepath.num19q3, header = T, sep = "\t")
sec.num19q4 <- read.delim(file = filepath.num19q4, header = T, sep = "\t")
sec.num19q <- rbind(sec.num19q2, sec.num19q3, sec.num19q4)

filepath.pre19q1 <- "./Data_PhD/US/SEC/2019q1/pre.txt" # missing information for 2019q1
filepath.pre19q2 <- "./Data_PhD/US/SEC/2019q2/pre.txt"
filepath.pre19q3 <- "./Data_PhD/US/SEC/2019q3/pre.txt"
filepath.pre19q4 <- "./Data_PhD/US/SEC/2019q4/pre.txt"
sec.pre19q1 <- read.delim(file = filepath.pre19q1, header = T, sep = "\t")
sec.pre19q2 <- read.delim(file = filepath.pre19q2, header = T, sep = "\t")
sec.pre19q3 <- read.delim(file = filepath.pre19q3, header = T, sep = "\t")
sec.pre19q4 <- read.delim(file = filepath.pre19q4, header = T, sep = "\t")
sec.pre19q <- rbind(sec.pre19q2, sec.pre19q3, sec.pre19q4)

sec.19q <- merge(x = sec.sub19q, y = sec.num19q, by = "adsh")
sec.19q <- merge(x = sec.19q, y = sec.pre19q)
sec.19q <- merge(x = sec.19q, y = sec.tag19q)
sec.19q <- sec.19q %>%
  filter(countryba == "US") %>%
  filter(fp != "FY") %>%
  select(c(adsh, cik, version, name, stprba, cityba, zipba, sic, form,
           qtrs, iord, uom, value, stmt, plabel, tlabel, datatype, fp, doc)) %>%
  mutate(year = 2019)

sec.19q[] <- lapply(sec.19q[], as.character)
# sec.19q <- sec.19q[sec.19q$stprba %in% c("CA", "OR"),]
sec.19q <- sec.19q[!duplicated(sec.19q),]
sec.19q <- sec.19q %>% filter(
  str_detect(tlabel, pattern = "Assets") |
    str_detect(tlabel, pattern = "Cost") |
    str_detect(tlabel, pattern = "Dividends") |
    str_detect(tlabel, pattern = "Earnings") |
    str_detect(tlabel, pattern = "Revenue") |
    str_detect(tlabel, pattern = "Expenditures") |
    str_detect(tlabel, pattern = "Expense") |
    str_detect(tlabel, pattern = "Profit") |
    str_detect(tlabel, pattern = "Income Taxes Paid") |
    str_detect(tlabel, pattern = "Inventories") |
    str_detect(tlabel, pattern = "Inventory") |
    str_detect(tlabel, pattern = "Labor") |
    str_detect(tlabel, pattern = "LIFO") |
    str_detect(tlabel, pattern = "Taxes") |
    str_detect(tlabel, pattern = "Taxes")
)
#======================================================================================================================#
### 2020Q
#======================================================================================================================#
unzip(zipfile = "./Data_PhD/US/SEC/2020q1.zip", exdir = "./Data_PhD/US/SEC/2020q1")
unzip(zipfile = "./Data_PhD/US/SEC/2020q2.zip", exdir = "./Data_PhD/US/SEC/2020q2")
unzip(zipfile = "./Data_PhD/US/SEC/2020q3.zip", exdir = "./Data_PhD/US/SEC/2020q3")
unzip(zipfile = "./Data_PhD/US/SEC/2020q4.zip", exdir = "./Data_PhD/US/SEC/2020q4")
filepath.sub20q1 <- "./Data_PhD/US/SEC/2020q1/sub.txt" # missing information for 2020q1
filepath.sub20q2 <- "./Data_PhD/US/SEC/2020q2/sub.txt"
filepath.sub20q3 <- "./Data_PhD/US/SEC/2020q3/sub.txt"
filepath.sub20q4 <- "./Data_PhD/US/SEC/2020q4/sub.txt"
sec.sub20q1 <- read.delim(file = filepath.sub20q1, header = T, sep = "\t")
sec.sub20q2 <- read.delim(file = filepath.sub20q2, header = T, sep = "\t")
sec.sub20q3 <- read.delim(file = filepath.sub20q3, header = T, sep = "\t")
sec.sub20q4 <- read.delim(file = filepath.sub20q4, header = T, sep = "\t")
sec.sub20q <- rbind(sec.sub20q2, sec.sub20q3, sec.sub20q4)

filepath.tag20q1 <- "./Data_PhD/US/SEC/2020q1/tag.txt" # missing information for 2020q1
filepath.tag20q2 <- "./Data_PhD/US/SEC/2020q2/tag.txt"
filepath.tag20q3 <- "./Data_PhD/US/SEC/2020q3/tag.txt"
filepath.tag20q4 <- "./Data_PhD/US/SEC/2020q4/tag.txt"
sec.tag20q1 <- read.delim(file = filepath.tag20q1, header = T, sep = "\t")
sec.tag20q2 <- read.delim(file = filepath.tag20q2, header = T, sep = "\t")
sec.tag20q3 <- read.delim(file = filepath.tag20q3, header = T, sep = "\t")
sec.tag20q4 <- read.delim(file = filepath.tag20q4, header = T, sep = "\t")
sec.tag20q <- rbind(sec.tag20q2, sec.tag20q3, sec.tag20q4)

filepath.num20q1 <- "./Data_PhD/US/SEC/2020q1/num.txt" # missing information for 2020q1
filepath.num20q2 <- "./Data_PhD/US/SEC/2020q2/num.txt"
filepath.num20q3 <- "./Data_PhD/US/SEC/2020q3/num.txt"
filepath.num20q4 <- "./Data_PhD/US/SEC/2020q4/num.txt"
sec.num20q1 <- read.delim(file = filepath.num20q1, header = T, sep = "\t")
sec.num20q2 <- read.delim(file = filepath.num20q2, header = T, sep = "\t")
sec.num20q3 <- read.delim(file = filepath.num20q3, header = T, sep = "\t")
sec.num20q4 <- read.delim(file = filepath.num20q4, header = T, sep = "\t")
sec.num20q <- rbind(sec.num20q2, sec.num20q3, sec.num20q4)

filepath.pre20q1 <- "./Data_PhD/US/SEC/2020q1/pre.txt" # missing information for 2020q1
filepath.pre20q2 <- "./Data_PhD/US/SEC/2020q2/pre.txt"
filepath.pre20q3 <- "./Data_PhD/US/SEC/2020q3/pre.txt"
filepath.pre20q4 <- "./Data_PhD/US/SEC/2020q4/pre.txt"
sec.pre20q1 <- read.delim(file = filepath.pre20q1, header = T, sep = "\t")
sec.pre20q2 <- read.delim(file = filepath.pre20q2, header = T, sep = "\t")
sec.pre20q3 <- read.delim(file = filepath.pre20q3, header = T, sep = "\t")
sec.pre20q4 <- read.delim(file = filepath.pre20q4, header = T, sep = "\t")
sec.pre20q <- rbind(sec.pre20q2, sec.pre20q3, sec.pre20q4)

sec.20q <- merge(x = sec.sub20q, y = sec.num20q, by = "adsh")
sec.20q <- merge(x = sec.20q, y = sec.pre20q)
sec.20q <- merge(x = sec.20q, y = sec.tag20q)
sec.20q <- sec.20q %>%
  filter(countryba == "US") %>%
  filter(fp != "FY") %>%
  select(c(adsh, cik, version, name, stprba, cityba, zipba, sic, form,
           qtrs, iord, uom, value, stmt, plabel, tlabel, datatype, fp, doc)) %>%
  mutate(year = 2020)

sec.20q[] <- lapply(sec.20q[], as.character)
# sec.20q <- sec.20q[sec.20q$stprba %in% c("CA", "OR"),]
sec.20q <- sec.20q[!duplicated(sec.20q),]
sec.20q <- sec.20q %>% filter(
  str_detect(tlabel, pattern = "Assets") |
    str_detect(tlabel, pattern = "Cost") |
    str_detect(tlabel, pattern = "Dividends") |
    str_detect(tlabel, pattern = "Earnings") |
    str_detect(tlabel, pattern = "Revenue") |
    str_detect(tlabel, pattern = "Expenditures") |
    str_detect(tlabel, pattern = "Expense") |
    str_detect(tlabel, pattern = "Profit") |
    str_detect(tlabel, pattern = "Income Taxes Paid") |
    str_detect(tlabel, pattern = "Inventories") |
    str_detect(tlabel, pattern = "Inventory") |
    str_detect(tlabel, pattern = "Labor") |
    str_detect(tlabel, pattern = "LIFO") |
    str_detect(tlabel, pattern = "Taxes") |
    str_detect(tlabel, pattern = "Taxes")
)
#======================================================================================================================#
### 2021Q
#======================================================================================================================#
unzip(zipfile = "./Data_PhD/US/SEC/2021q1.zip", exdir = "./Data_PhD/US/SEC/2021q1")
unzip(zipfile = "./Data_PhD/US/SEC/2021q2.zip", exdir = "./Data_PhD/US/SEC/2021q2")
unzip(zipfile = "./Data_PhD/US/SEC/2021q3.zip", exdir = "./Data_PhD/US/SEC/2021q3")
unzip(zipfile = "./Data_PhD/US/SEC/2021q4.zip", exdir = "./Data_PhD/US/SEC/2021q4")
filepath.sub21q1 <- "./Data_PhD/US/SEC/2021q1/sub.txt" # missing information for 2021q1
filepath.sub21q2 <- "./Data_PhD/US/SEC/2021q2/sub.txt"
filepath.sub21q3 <- "./Data_PhD/US/SEC/2021q3/sub.txt"
filepath.sub21q4 <- "./Data_PhD/US/SEC/2021q4/sub.txt"
sec.sub21q1 <- read.delim(file = filepath.sub21q1, header = T, sep = "\t")
sec.sub21q2 <- read.delim(file = filepath.sub21q2, header = T, sep = "\t")
sec.sub21q3 <- read.delim(file = filepath.sub21q3, header = T, sep = "\t")
sec.sub21q4 <- read.delim(file = filepath.sub21q4, header = T, sep = "\t")
sec.sub21q <- rbind(sec.sub21q2, sec.sub21q3, sec.sub21q4)

filepath.tag21q1 <- "./Data_PhD/US/SEC/2021q1/tag.txt" # missing information for 2021q1
filepath.tag21q2 <- "./Data_PhD/US/SEC/2021q2/tag.txt"
filepath.tag21q3 <- "./Data_PhD/US/SEC/2021q3/tag.txt"
filepath.tag21q4 <- "./Data_PhD/US/SEC/2021q4/tag.txt"
sec.tag21q1 <- read.delim(file = filepath.tag21q1, header = T, sep = "\t")
sec.tag21q2 <- read.delim(file = filepath.tag21q2, header = T, sep = "\t")
sec.tag21q3 <- read.delim(file = filepath.tag21q3, header = T, sep = "\t")
sec.tag21q4 <- read.delim(file = filepath.tag21q4, header = T, sep = "\t")
sec.tag21q <- rbind(sec.tag21q2, sec.tag21q3, sec.tag21q4)

filepath.num21q1 <- "./Data_PhD/US/SEC/2021q1/num.txt" # missing information for 2021q1
filepath.num21q2 <- "./Data_PhD/US/SEC/2021q2/num.txt"
filepath.num21q3 <- "./Data_PhD/US/SEC/2021q3/num.txt"
filepath.num21q4 <- "./Data_PhD/US/SEC/2021q4/num.txt"
sec.num21q1 <- read.delim(file = filepath.num21q1, header = T, sep = "\t")
sec.num21q2 <- read.delim(file = filepath.num21q2, header = T, sep = "\t")
sec.num21q3 <- read.delim(file = filepath.num21q3, header = T, sep = "\t")
sec.num21q4 <- read.delim(file = filepath.num21q4, header = T, sep = "\t")
sec.num21q <- rbind(sec.num21q2, sec.num21q3, sec.num21q4)

filepath.pre21q1 <- "./Data_PhD/US/SEC/2021q1/pre.txt" # missing information for 2021q1
filepath.pre21q2 <- "./Data_PhD/US/SEC/2021q2/pre.txt"
filepath.pre21q3 <- "./Data_PhD/US/SEC/2021q3/pre.txt"
filepath.pre21q4 <- "./Data_PhD/US/SEC/2021q4/pre.txt"
sec.pre21q1 <- read.delim(file = filepath.pre21q1, header = T, sep = "\t")
sec.pre21q2 <- read.delim(file = filepath.pre21q2, header = T, sep = "\t")
sec.pre21q3 <- read.delim(file = filepath.pre21q3, header = T, sep = "\t")
sec.pre21q4 <- read.delim(file = filepath.pre21q4, header = T, sep = "\t")
sec.pre21q <- rbind(sec.pre21q2, sec.pre21q3, sec.pre21q4)

sec.21q <- merge(x = sec.sub21q, y = sec.num21q, by = "adsh")
sec.21q <- merge(x = sec.21q, y = sec.pre21q)
sec.21q <- merge(x = sec.21q, y = sec.tag21q)
sec.21q <- sec.21q %>%
  filter(countryba == "US") %>%
  filter(fp != "FY") %>%
  select(c(adsh, cik, version, name, stprba, cityba, zipba, sic, form,
           qtrs, iord, uom, value, stmt, plabel, tlabel, datatype, fp, doc)) %>%
  mutate(year = 2021)

sec.21q[] <- lapply(sec.21q[], as.character)
# sec.21q <- sec.21q[sec.21q$stprba %in% c("CA", "OR"),]
sec.21q <- sec.21q[!duplicated(sec.21q),]
sec.21q <- sec.21q %>% filter(
  str_detect(tlabel, pattern = "Assets") |
    str_detect(tlabel, pattern = "Cost") |
    str_detect(tlabel, pattern = "Dividends") |
    str_detect(tlabel, pattern = "Earnings") |
    str_detect(tlabel, pattern = "Revenue") |
    str_detect(tlabel, pattern = "Expenditures") |
    str_detect(tlabel, pattern = "Expense") |
    str_detect(tlabel, pattern = "Profit") |
    str_detect(tlabel, pattern = "Income Taxes Paid") |
    str_detect(tlabel, pattern = "Inventories") |
    str_detect(tlabel, pattern = "Inventory") |
    str_detect(tlabel, pattern = "Labor") |
    str_detect(tlabel, pattern = "LIFO") |
    str_detect(tlabel, pattern = "Taxes") |
    str_detect(tlabel, pattern = "Taxes")
)
#======================================================================================================================#
### 2022Q
#======================================================================================================================#
unzip(zipfile = "./Data_PhD/US/SEC/2022q1.zip", exdir = "./Data_PhD/US/SEC/2022q1")
unzip(zipfile = "./Data_PhD/US/SEC/2022q2.zip", exdir = "./Data_PhD/US/SEC/2022q2")
unzip(zipfile = "./Data_PhD/US/SEC/2022q3.zip", exdir = "./Data_PhD/US/SEC/2022q3")
unzip(zipfile = "./Data_PhD/US/SEC/2022q4.zip", exdir = "./Data_PhD/US/SEC/2022q4")
filepath.sub22q1 <- "./Data_PhD/US/SEC/2022q1/sub.txt" # missing information for 2022q1
filepath.sub22q2 <- "./Data_PhD/US/SEC/2022q2/sub.txt"
filepath.sub22q3 <- "./Data_PhD/US/SEC/2022q3/sub.txt"
filepath.sub22q4 <- "./Data_PhD/US/SEC/2022q4/sub.txt"
sec.sub22q1 <- read.delim(file = filepath.sub22q1, header = T, sep = "\t")
sec.sub22q2 <- read.delim(file = filepath.sub22q2, header = T, sep = "\t")
sec.sub22q3 <- read.delim(file = filepath.sub22q3, header = T, sep = "\t")
sec.sub22q4 <- read.delim(file = filepath.sub22q4, header = T, sep = "\t")
sec.sub22q <- rbind(sec.sub22q2, sec.sub22q3, sec.sub22q4)

filepath.tag22q1 <- "./Data_PhD/US/SEC/2022q1/tag.txt" # missing information for 2022q1
filepath.tag22q2 <- "./Data_PhD/US/SEC/2022q2/tag.txt"
filepath.tag22q3 <- "./Data_PhD/US/SEC/2022q3/tag.txt"
filepath.tag22q4 <- "./Data_PhD/US/SEC/2022q4/tag.txt"
sec.tag22q1 <- read.delim(file = filepath.tag22q1, header = T, sep = "\t")
sec.tag22q2 <- read.delim(file = filepath.tag22q2, header = T, sep = "\t")
sec.tag22q3 <- read.delim(file = filepath.tag22q3, header = T, sep = "\t")
sec.tag22q4 <- read.delim(file = filepath.tag22q4, header = T, sep = "\t")
sec.tag22q <- rbind(sec.tag22q2, sec.tag22q3, sec.tag22q4)

filepath.num22q1 <- "./Data_PhD/US/SEC/2022q1/num.txt" # missing information for 2022q1
filepath.num22q2 <- "./Data_PhD/US/SEC/2022q2/num.txt"
filepath.num22q3 <- "./Data_PhD/US/SEC/2022q3/num.txt"
filepath.num22q4 <- "./Data_PhD/US/SEC/2022q4/num.txt"
sec.num22q1 <- read.delim(file = filepath.num22q1, header = T, sep = "\t")
sec.num22q2 <- read.delim(file = filepath.num22q2, header = T, sep = "\t")
sec.num22q3 <- read.delim(file = filepath.num22q3, header = T, sep = "\t")
sec.num22q4 <- read.delim(file = filepath.num22q4, header = T, sep = "\t")
sec.num22q <- rbind(sec.num22q2, sec.num22q3, sec.num22q4)

filepath.pre22q1 <- "./Data_PhD/US/SEC/2022q1/pre.txt" # missing information for 2022q1
filepath.pre22q2 <- "./Data_PhD/US/SEC/2022q2/pre.txt"
filepath.pre22q3 <- "./Data_PhD/US/SEC/2022q3/pre.txt"
filepath.pre22q4 <- "./Data_PhD/US/SEC/2022q4/pre.txt"
sec.pre22q1 <- read.delim(file = filepath.pre22q1, header = T, sep = "\t")
sec.pre22q2 <- read.delim(file = filepath.pre22q2, header = T, sep = "\t")
sec.pre22q3 <- read.delim(file = filepath.pre22q3, header = T, sep = "\t")
sec.pre22q4 <- read.delim(file = filepath.pre22q4, header = T, sep = "\t")
sec.pre22q <- rbind(sec.pre22q2, sec.pre22q3, sec.pre22q4)

sec.22q <- merge(x = sec.sub22q, y = sec.num22q, by = "adsh")
sec.22q <- merge(x = sec.22q, y = sec.pre22q)
sec.22q <- merge(x = sec.22q, y = sec.tag22q)
sec.22q <- sec.22q %>%
  filter(countryba == "US") %>%
  filter(fp != "FY") %>%
  select(c(adsh, cik, version, name, stprba, cityba, zipba, sic, form,
           qtrs, iord, uom, value, stmt, plabel, tlabel, datatype, fp, doc)) %>%
  mutate(year = 2022)

sec.22q[] <- lapply(sec.22q[], as.character)
# sec.22q <- sec.22q[sec.22q$stprba %in% c("CA", "OR"),]
sec.22q <- sec.22q[!duplicated(sec.22q),]
sec.22q <- sec.22q %>% filter(
  str_detect(tlabel, pattern = "Assets") |
    str_detect(tlabel, pattern = "Cost") |
    str_detect(tlabel, pattern = "Dividends") |
    str_detect(tlabel, pattern = "Earnings") |
    str_detect(tlabel, pattern = "Revenue") |
    str_detect(tlabel, pattern = "Expenditures") |
    str_detect(tlabel, pattern = "Expense") |
    str_detect(tlabel, pattern = "Profit") |
    str_detect(tlabel, pattern = "Income Taxes Paid") |
    str_detect(tlabel, pattern = "Inventories") |
    str_detect(tlabel, pattern = "Inventory") |
    str_detect(tlabel, pattern = "Labor") |
    str_detect(tlabel, pattern = "LIFO") |
    str_detect(tlabel, pattern = "Taxes") |
    str_detect(tlabel, pattern = "Taxes")
)

sec <- bind_rows(sec.09q, sec.10q, sec.11q, sec.12q, sec.13q, sec.14q, sec.15q,
                 sec.16q, sec.17q, sec.18q, sec.19q, sec.20q, sec.21q, sec.22q)
write_rds(x = sec, file = "./Data_PhD/US/SEC/sec.rds", compress = "xz")
