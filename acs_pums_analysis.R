# acs_pums_analysis.R
# Statistical analysis of household types and incomes.
#
# Copyright (C) 2015  Nick Hepler
#
#  Version 0.9.0
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

require(dplyr)

# Download raw data and read into global envrionment.
temp <- tempfile()
download.file("http://www2.census.gov/acs2012_1yr/pums/csv_hny.zip",temp)
raw <- read.csv(unz(temp, "ss12hny.csv"))


#---------------------------------------------------------#
# Data Management                                         #
# Converts the raw data into a clean data set suitable    #
# for statistrical analysis                               #
#---------------------------------------------------------#

# Create a transitional data set, rename variables to lower case, and select
# only the fincp & npp variables for analysis
transition <- raw
names(transition) <- tolower(names(transition))
transition <- select(transition, fincp, npp)

# Convert npp variable to a factor, rename the variables.
transition$household <- NA
transition$household[transition$npp == 0] <- 
  "Not a grandparent headed household with no parent present"
transition$household[transition$npp == 1] <- 
  "Grandparent headed household with no parent present"
transition <- select(transition, -(npp))
transition$household <- factor(transition$household, levels=c(
  "Not a grandparent headed household with no parent present",
  "Grandparent headed household with no parent present"))
names(transition)[1] <- "income"
transition$income <- as.numeric(transition$income)

# Subset the samples
nghh <- filter(transition, 
  household == "Not a grandparent headed household with no parent present")
ghh <- filter(transition, 
  household == "Grandparent headed household with no parent present")

# Identify missing values
missing.household <- sum(is.na(transition$household))
missing.income <- sum(is.na(transition$income))
missing.income.nghh <- sum(is.na(nghh$income))
missing.income.ghh <- sum(is.na(ghh$income))

# Calculate number of observations with valid values.
valid.income <- length(transition$income) - missing.income
valid.household <- length(transition$household) - missing.household
valid.income.nghh <- length(nghh$income) - missing.income.nghh
valid.income.ghh <- length(ghh$income) - missing.income.ghh
valid.income.total <- valid.income.nghh + valid.income.ghh

# Group data using dplyr by household type.
by_household <- group_by(transition, household)

#---------------------------------------------------------#
# Statistics                                              #
# Converts the raw data into a clean data set suitable    #
# for statistrical analysis                               #
#---------------------------------------------------------#

# Mean/Median for each sample.
summarise(by_household, mean(income, na.rm=TRUE))
summarise(by_household, median(income, na.rm=TRUE))

# Min/Max for each sample.
summarise(by_household, min(income, na.rm=TRUE))
summarise(by_household, max(income, na.rm=TRUE))

# Calculate range for each sample.
diff(range(nghh$income, na.rm=TRUE))
diff(range(ghh$income, na.rm=TRUE))

# Calculate interquartile range for each sample.
quantile.nghh <- quantile(nghh$income, na.rm=TRUE)
quantile.ghh <- quantile(ghh$income, na.rm=TRUE)
quantile.nghh[4] - quantile.nghh[2]
quantile.ghh[4] - quantile.ghh[2]


# Calculate standard deviation for each sample.
summarise(by_household, sd(income, na.rm=TRUE))

# Create histogram for each sample.
par(cex.lab=.90)
options(scipen=999)

pdf("./report/hist_nghh.pdf")
hist(nghh$income,
  breaks="FD",
  col="#660000",
  xlab="Annual Income (in USD)", col.lab="#636363",
  main="Household Income Histogram", col.main="#4B245E",
  sub="Grandparent Not Head of Household", col.sub="#E0AD12")
dev.off()

pdf("./report/hist_ghh.pdf")
hist(ghh$income,
  breaks="FD",
  col="#660000",
  xlab="Annual Income (in USD)", col.lab="#636363",
  main="Household Income Histogram", col.main="#4B245E",
  sub="Grandparent Head of Household", col.sub="#E0AD12")
dev.off()

pdf("./report/box_nghh.pdf")
boxplot(nghh$income,
  sub="Grandparent Not Head of Household", col.sub="#E0AD12",
  ylab="Annual Income (in USD)",
  main="Household Income Box Plot", col.main="#4B245E")
dev.off()

pdf("./report/box_ghh.pdf")
boxplot(ghh$income,
  sub="Grandparent Head of Household", col.sub="#E0AD12",
  ylab="Annual Income (in USD)",
  main="Household Income Box Plot", col.main="#4B245E")
dev.off()

par(cex.lab=1)
options(scipen=0)

# Clean up global envrionment.
rm(list=ls())

# Remove temporary file
unlink(temp)
