library(acs)
library(tigris)
library(dplyr)


# ------------------------------------------------------------- #
# Using the American Community Survey 5-year estimates and the
# census tract of the maternal residence, we will assemble a 
# database of aggregate measures on poverty status, median 
# income, racial composition and place of birth (census variables 
# B13010, B17010, B17026, S1702, B15001, B19013 and B05006)
# ------------------------------------------------------------- #


# Grab the spatial data for 5 counties
counties<-c(5, 47, 61, 81, 85)
tracts<-tracts(state = 'NY', county = counties, cb = TRUE)


geo<-geo.make(state = c("NY"),
	county = counties, tract = "*")





# ------------------------------------------------------------- #
# List of variables -----
# Grab the data. Below are the variables of interest with the
# Census definition and below that, the final calculation we need.
# ------------------------------------------------------------- #
# B13010: WOMEN 15 TO 50 YEARS WHO HAD A BIRTH IN THE PAST 12 MONTHS 
# BY MARITAL STATUS AND POVERTY STATUS IN THE PAST 12 MONTHS
# Women with birth < 200% of poverty as proportion of all women with birth
# Women with birth < 200% of poverty who are unmarried as proportion of all 
# women with birth

# B17010: POVERTY STATUS IN THE PAST 12 MONTHS OF FAMILIES BY FAMILY 
# TYPE BY PRESENCE OF RELATED CHILDREN UNDER 18 YEARS BY AGE OF 
# RELATED CHILDREN 
# Families (any type) with children < 18 in poverty as a proportion 
# of all families with children <18

# B17026: RATIO OF INCOME TO POVERTY LEVEL OF FAMILIES IN THE 
# PAST 12 MONTHS
# Proportion of familes < 2.0 of poverty as proportion of all families

# B15001: SEX BY AGE BY EDUCATIONAL ATTAINMENT FOR THE POPULATION 
# 18 YEARS AND OVER
# Proportion of people 18 years and over (all) with no high school degree

# B05006: PLACE OF BIRTH FOR THE FOREIGN-BORN POPULATION IN THE UNITED STATES
# Proportion foreign born, I think you need to use total population 
# and then place of birth total

# S1702: POVERTY STATUS IN THE PAST 12 MONTHS OF FAMILIES

# B19013: MEDIAN HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 
# 2014 INFLATION-ADJUSTED DOLLARS)
# ------------------------------------------------------------- #


# acsnames<-c("B13010", "B17010", "B17026", "B15001", "B05006")
# # notfound<-c("S1702", "B19013") # These 2 we'll need to download manually


getACSdata<-function(tableid, tablecols){
	dat<-acs.fetch(endyear = 2014, span = 5, geography = geo,
		table.number = tableid, col.names = "pretty")
	
	dat2<-data.frame(GEOID = paste0(str_pad(dat@geography$state, 2, "left", pad="0"), 
		str_pad(dat@geography$county, 3, "left", pad="0"), 
		str_pad(dat@geography$tract, 6, "left", pad="0")),
		dat@estimate[,c(tablecols)])
	row.names(dat2)<-NULL
	return (dat2)
}



# ------------------------------------------------------------- #
# B13010 -----
# WOMEN 15 TO 50 YEARS WHO HAD A BIRTH IN THE PAST 12 MONTHS 
# BY MARITAL STATUS AND POVERTY STATUS IN THE PAST 12 MONTHS

# Women with birth < 200% of poverty as proportion of all 
# women with birth
# Women with birth < 200% of poverty who are unmarried as 
# proportion of all women with birth
# ------------------------------------------------------------- #

keep<-c("WOMEN 15 TO 50 YEARS WHO HAD A BIRTH IN THE PAST 12 MONTHS BY MARITAL STATUS AND POVERTY STATUS IN THE PAST 12 MONTHS: Women who had a birth in the past 12 months:",
	"WOMEN 15 TO 50 YEARS WHO HAD A BIRTH IN THE PAST 12 MONTHS BY MARITAL STATUS AND POVERTY STATUS IN THE PAST 12 MONTHS: Women who had a birth in the past 12 months: Now married (including separated and spouse absent): Below 100 percent of poverty level in the past 12 months",
	"WOMEN 15 TO 50 YEARS WHO HAD A BIRTH IN THE PAST 12 MONTHS BY MARITAL STATUS AND POVERTY STATUS IN THE PAST 12 MONTHS: Women who had a birth in the past 12 months: Now married (including separated and spouse absent): 100 to 199 percent of poverty level in the past 12 months",
	"WOMEN 15 TO 50 YEARS WHO HAD A BIRTH IN THE PAST 12 MONTHS BY MARITAL STATUS AND POVERTY STATUS IN THE PAST 12 MONTHS: Women who had a birth in the past 12 months: Unmarried (never married, widowed and divorced): Below 100 percent of poverty level in the past 12 months",
	"WOMEN 15 TO 50 YEARS WHO HAD A BIRTH IN THE PAST 12 MONTHS BY MARITAL STATUS AND POVERTY STATUS IN THE PAST 12 MONTHS: Women who had a birth in the past 12 months: Unmarried (never married, widowed and divorced): 100 to 199 percent of poverty level in the past 12 months")

dat<-getACSdata("B13010", keep)

# attr(dat, "acs.colnames")
# summary(dat)



dat1<-rename(dat, geoid = GEOID, 
	totbirth = `WOMEN.15.TO.50.YEARS.WHO.HAD.A.BIRTH.IN.THE.PAST.12.MONTHS.BY.MARITAL.STATUS.AND.POVERTY.STATUS.IN.THE.PAST.12.MONTHS..Women.who.had.a.birth.in.the.past.12.months.`) %>%
	mutate(totm200 = rowwise(.) %>% select(contains("Women.who.had.a.birth.in.the.past.12.months..Now.married")) %>% rowSums(),
		totum200 = rowwise(.) %>% select(contains("Women.who.had.a.birth.in.the.past.12.months..Unmarried..never.married")) %>% rowSums()) %>%
	select(-contains("WOMEN.15.TO.50.YEARS.WHO.HAD.A.BIRTH.IN.THE.PAST.12.MONTHS.BY.MARITAL.STATUS"))


dat1<-mutate(dat1, birthpov = (totm200 + totum200)/totbirth, 
	birthpovum = totum200/totbirth, 
	birthpov = ifelse(is.na(birthpov), 0, birthpov),
	birthpovum = ifelse(is.na(birthpovum), 0, birthpovum)) %>%
	select(geoid, birthpov, birthpovum)
		
	

rm(dat, keep)







# ------------------------------------------------------------- #
# B17010 -----
# POVERTY STATUS IN THE PAST 12 MONTHS OF FAMILIES BY FAMILY 
# TYPE BY PRESENCE OF RELATED CHILDREN UNDER 18 YEARS BY AGE OF 
# RELATED CHILDREN 

# Families (any type) with children < 18 in poverty as a 
# proportion of all families with children <18
# ------------------------------------------------------------- #

keep<-c("Poverty Status of Families by Family Type by Presence of Related Children Under 18 Years by Age: Total:",
	"Poverty Status of Families by Family Type by Presence of Related Children Under 18 Years by Age: Income in the past 12 months below poverty level:")


dat<-getACSdata("B17010", keep)

# attr(dat, "acs.colnames")
# summary(dat)

names(dat)<-c("geoid", "tot", "povtot")

dat2<-mutate(dat, povfamchld = povtot/tot) %>%
	select(geoid, povfamchld) %>%
	mutate(povfamchld = ifelse(is.na(povfamchld), 0, povfamchld))


rm(dat, keep)







# ------------------------------------------------------------- #
# B17026 -----
# RATIO OF INCOME TO POVERTY LEVEL OF FAMILIES IN THE PAST 12 MONTHS

# Proportion of familes < 2.0 of poverty as proportion of all families
# ------------------------------------------------------------- #


keep<-c("Ratio of Income to Poverty Level of Families in the Past 12 Months: Total:",
	"Ratio of Income to Poverty Level of Families in the Past 12 Months: Under .50",
	"Ratio of Income to Poverty Level of Families in the Past 12 Months: .50 to .74",
	"Ratio of Income to Poverty Level of Families in the Past 12 Months: .75 to .99",
	"Ratio of Income to Poverty Level of Families in the Past 12 Months: 1.00 to 1.24",
	"Ratio of Income to Poverty Level of Families in the Past 12 Months: 1.25 to 1.49",
	"Ratio of Income to Poverty Level of Families in the Past 12 Months: 1.50 to 1.74",
	"Ratio of Income to Poverty Level of Families in the Past 12 Months: 1.75 to 1.84",
	"Ratio of Income to Poverty Level of Families in the Past 12 Months: 1.85 to 1.99")


dat<-getACSdata("B17026", keep)

# attr(dat, "acs.colnames")
# summary(dat)



dat3<-rename(dat, geoid = GEOID,
	tot = `Ratio.of.Income.to.Poverty.Level.of.Families.in.the.Past.12.Months..Total.`) %>%
	mutate(temp = rowwise(.) %>% select(contains("Ratio.of.Income.to.Poverty.Level.of.Families.in.the.Past.12.Months")) %>% rowSums(),
		povfaminc = temp/tot) %>%
	select(geoid, povfaminc) %>%
	mutate(povfaminc = ifelse(is.na(povfaminc), 0, povfaminc))


rm(dat, keep)







# ------------------------------------------------------------- #
# B15001 -----
# SEX BY AGE BY EDUCATIONAL ATTAINMENT FOR THE POPULATION 
# 18 YEARS AND OVER

# Proportion of people 18 years and over (all) with no 
# high school degree
# ------------------------------------------------------------- #

keep<-c("SEX BY AGE BY EDUCATIONAL ATTAINMENT FOR THE POPULATION 18 YEARS AND OVER: Total:",
	"SEX BY AGE BY EDUCATIONAL ATTAINMENT FOR THE POPULATION 18 YEARS AND OVER: Male: 18 to 24 years: Less than 9th grade",
	"SEX BY AGE BY EDUCATIONAL ATTAINMENT FOR THE POPULATION 18 YEARS AND OVER: Male: 18 to 24 years: 9th to 12th grade, no diploma",
	"SEX BY AGE BY EDUCATIONAL ATTAINMENT FOR THE POPULATION 18 YEARS AND OVER: Male: 25 to 34 years: Less than 9th grade",
	"SEX BY AGE BY EDUCATIONAL ATTAINMENT FOR THE POPULATION 18 YEARS AND OVER: Male: 25 to 34 years: 9th to 12th grade, no diploma",
	"SEX BY AGE BY EDUCATIONAL ATTAINMENT FOR THE POPULATION 18 YEARS AND OVER: Male: 35 to 44 years: Less than 9th grade",
	"SEX BY AGE BY EDUCATIONAL ATTAINMENT FOR THE POPULATION 18 YEARS AND OVER: Male: 35 to 44 years: 9th to 12th grade, no diploma",
	"SEX BY AGE BY EDUCATIONAL ATTAINMENT FOR THE POPULATION 18 YEARS AND OVER: Male: 45 to 64 years: Less than 9th grade",
	"SEX BY AGE BY EDUCATIONAL ATTAINMENT FOR THE POPULATION 18 YEARS AND OVER: Male: 45 to 64 years: 9th to 12th grade, no diploma",
	"SEX BY AGE BY EDUCATIONAL ATTAINMENT FOR THE POPULATION 18 YEARS AND OVER: Male: 65 years and over: Less than 9th grade",
	"SEX BY AGE BY EDUCATIONAL ATTAINMENT FOR THE POPULATION 18 YEARS AND OVER: Male: 65 years and over: 9th to 12th grade, no diploma",
	"SEX BY AGE BY EDUCATIONAL ATTAINMENT FOR THE POPULATION 18 YEARS AND OVER: Female: 18 to 24 years: Less than 9th grade",
	"SEX BY AGE BY EDUCATIONAL ATTAINMENT FOR THE POPULATION 18 YEARS AND OVER: Female: 18 to 24 years: 9th to 12th grade, no diploma",
	"SEX BY AGE BY EDUCATIONAL ATTAINMENT FOR THE POPULATION 18 YEARS AND OVER: Female: 25 to 34 years: Less than 9th grade",
	"SEX BY AGE BY EDUCATIONAL ATTAINMENT FOR THE POPULATION 18 YEARS AND OVER: Female: 25 to 34 years: 9th to 12th grade, no diploma",
	"SEX BY AGE BY EDUCATIONAL ATTAINMENT FOR THE POPULATION 18 YEARS AND OVER: Female: 35 to 44 years: Less than 9th grade",
	"SEX BY AGE BY EDUCATIONAL ATTAINMENT FOR THE POPULATION 18 YEARS AND OVER: Female: 35 to 44 years: 9th to 12th grade, no diploma",
	"SEX BY AGE BY EDUCATIONAL ATTAINMENT FOR THE POPULATION 18 YEARS AND OVER: Female: 45 to 64 years: Less than 9th grade",
	"SEX BY AGE BY EDUCATIONAL ATTAINMENT FOR THE POPULATION 18 YEARS AND OVER: Female: 45 to 64 years: 9th to 12th grade, no diploma",
	"SEX BY AGE BY EDUCATIONAL ATTAINMENT FOR THE POPULATION 18 YEARS AND OVER: Female: 65 years and over: Less than 9th grade",
	"SEX BY AGE BY EDUCATIONAL ATTAINMENT FOR THE POPULATION 18 YEARS AND OVER: Female: 65 years and over: 9th to 12th grade, no diploma")


dat<-getACSdata("B15001", keep)

# attr(dat, "acs.colnames")
# summary(dat)



dat4<-rename(dat, geoid = GEOID,
	tot = `SEX.BY.AGE.BY.EDUCATIONAL.ATTAINMENT.FOR.THE.POPULATION.18.YEARS.AND.OVER..Total.`) %>%
	mutate(temp = rowwise(.) %>% select(contains("SEX.BY.AGE.BY.EDUCATIONAL.ATTAINMENT.FOR.THE.POPULATION.18.YEARS.AND.OVER")) %>% rowSums(),
		ppnohsdeg = temp/tot,
		ppnohsdeg = ifelse(is.na(ppnohsdeg), 0, ppnohsdeg)) %>%
	select(geoid, ppnohsdeg)

rm(keep, dat)






# ------------------------------------------------------------- #
# B05006 -----
# PLACE OF BIRTH FOR THE FOREIGN-BORN POPULATION IN THE UNITED STATES

# Proportion foreign born, I think you need to use total population 
# and then place of birth total
# ------------------------------------------------------------- #


keep<-c("B05006. Place of Birth for the Foreign-Born Population in the United States: Total:")


dat<-getACSdata("B05006", keep)
pop<-getACSdata("B01003", "Total Population: Total")

# summary(dat)
# summary(pop)


dat5<-full_join(pop, dat, by = "GEOID") %>%
	rename(geoid = GEOID, totpop = `dat.estimate...c.tablecols...x`) %>%
	mutate(ppforborn = `dat.estimate...c.tablecols...y`/totpop) %>%
	select(geoid, totpop, ppforborn) %>%
	mutate(ppforborn = ifelse(is.na(ppforborn), 0, ppforborn))


rm(dat, keep, pop)







# ------------------------------------------------------------- #
# S1702 -----
# POVERTY STATUS IN THE PAST 12 MONTHS OF FAMILIES
# This one had to be manually downloaded.
# ------------------------------------------------------------- #


options(scipen = 999999)

dat<-read.csv("X:/projects/abruption/data/data_collected/20170324_census_acs_2015/ACS_14_5YR_S1702/ACS_14_5YR_S1702.csv", stringsAsFactors = FALSE, skip = 1)

dat6<-select(dat, Id2, `All.families....Percent.below.poverty.level..Estimate..Families`) %>%
	rename(geoid = Id2, povfamblw = `All.families....Percent.below.poverty.level..Estimate..Families`) %>%
	mutate(povfamblw = povfamblw/100,
		povfamblw = ifelse(is.na(povfamblw), -99999, povfamblw),
		geoid = as.factor(geoid))



rm(dat)







# ------------------------------------------------------------- #
# B19013 -----
# MEDIAN HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 
# 2014 INFLATION-ADJUSTED DOLLARS)
# ------------------------------------------------------------- #


keep<-c("B19013. Median Household Income in the Past 12 Months (in 2014 Inflation-Adjusted Dollars): Median household income in the past 12 months (in 2014 Inflation-adjusted dollars)")

dat<-getACSdata("B19013", keep)

# summary(dat)


dat7<-rename(dat, geoid = GEOID, medhhinc = dat.estimate...c.tablecols..) %>%
	mutate(medhhinc = ifelse(is.na(medhhinc), -99999, medhhinc))

rm(dat, keep)







# ------------------------------------------------------------- #
# Combine into final table -----
# ------------------------------------------------------------- #


findat<-full_join(dat1, dat2, by = "geoid") %>%
	full_join(dat3, by = "geoid") %>%
	full_join(dat4, by = "geoid") %>%
	full_join(dat5, by = "geoid") %>%
	full_join(dat6, by = "geoid") %>%
	full_join(dat7, by = "geoid")


# Check records where zero population.
x<-filter(findat, totpop == 0)



# Where there's no population medhhinc and
# povfambelow should be zero, otherwise -99999
findat<-mutate(findat, medhhinc = ifelse(totpop == 0, 0, medhhinc),
	povfamblw = ifelse(totpop == 0, 0, povfamblw))



findat<-select(findat, -totpop)



rm(dat1, dat2, dat3, dat4, dat5, dat6, dat7, x)




# ------------------------------------------------------------- #
# Merge the data with the shapefile -----
# ------------------------------------------------------------- #

library(sf)

trfin<-geo_join(tracts[,c("GEOID")], findat, "GEOID", "geoid")
trfin<-trfin[,!(names(trfin) %in% "GEOID")]
names(trfin)[names(trfin) == "geoid"]<-"trfips"

# prj<-"+proj=lcc +lat_1=40.66666666666666 +lat_2=41.03333333333333 +lat_0=40.16666666666666 +lon_0=-74 +x_0=300000 +y_0=0 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs"
# trfin<-sp::spTransform(trfin, prj)


trfin<-st_as_sf(trfin)



st_write(trfin,
	dsn = "X:/projects/abruption/data/data_processed/lur_variables_lattice",
	layer = "variables_census", driver = "ESRI Shapefile")



cat("
	March 27, 2017
	ZevRoss Spatial Analysis

	Census variables calculated for abruption project using
	R script 23_census_related_tract_vars.R.

	All variables are ACS 5-year estimates using 2010-2014 data at Census tract level. All variables except S1702 (povfamblw) were accessed using R library(acs), otherwise we manually downloaded.

	birthpov: Women with birth < 200% of poverty as proportion of all women with birth
	birthpovum: Women with birth < 200% of poverty who are unmarried as proportion of all women with birth
	povfamchld: Families (any type) with children < 18 in poverty as a proportion of all families with children <18
	povfaminc: Proportion of familes < 2.0 of poverty as proportion of all families
	ppnohsdeg: Proportion of familes < 2.0 of poverty as proportion of all families
	ppforborn: Proportion foreign born, I think you need to use total population and then place of birth total
	povfamblw: Proportion of families below poverty level
	medhhinc: median household income
	",
	file="X:/projects/abruption/data/data_processed/lur_variables_lattice/readme_zrsa_census_vars.txt")



##


