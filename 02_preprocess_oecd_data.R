
library(countrycode)
library(dplyr)


setwd("F:\\Desktop\\科研项目\\1.负责科研项目\\Climate Policy DID Science-厦大_北师大\\Science")

#load CAPMF data from the OECD

oecd_data = read.csv('CAPMF_Policy.csv')

## add policy data for the US (not available in CAPMF) which were publicly available from the IPAC dashboard until March 2023 (https://www.oecd.org/climate-action/ipac/dashboard)

policy_data_usa = read.csv("IPAC_policy_usa.csv")
  
## bind
oecd_data = rbind(oecd_data, policy_data_usa)

#rename the "valueCAP_Comp" column as "Value" for simplicity

colnames(oecd_data)[colnames(oecd_data) == "valueCAP_Comp"] ="Value"

#also rename the "PriorityArea" column as "Module" 
colnames(oecd_data)[colnames(oecd_data) == "PriorityArea"] ="Module"

##exclude all years we do not consider 

#keep 1998 to 2022 to allow for symmetric confidence windows
#for now keep 1996 and 1997 so we have no trouble with NAs on the 1998 line
#for the US we only have data from 2000 to 2020

oecd_data = oecd_data[oecd_data$year > 1995,]

#fix naming issue

oecd_data$Policy[oecd_data$Policy=='ETS_Buildings'] = "ETS Buildings"

##remove duplicates after renaming 
oecd_data = subset(oecd_data, select=-c(X))
oecd_data = oecd_data[!duplicated(oecd_data),]

##get 2-year stringency diff for 1 and 2 year lag values that are NA will produce an NA in the difference. -> will be further used in step 2
oecd_grouped <- oecd_data %>%
  group_by(ISO, Module, Policy) %>%
  arrange(year) %>%
  mutate(diff = Value - lag(Value), diff_2 = Value - lag(Value, n=2)) %>%
  ungroup()


#1. Get introductions

oecd_introductions = oecd_grouped
#we set NA values to 0 for now which means we will catch the first non-zero value whether or not there was an NA before. 
#introductions that came from NA are then filtered out later
oecd_introductions$Value[is.na(oecd_introductions$Value)] = 0
oecd_introductions = oecd_introductions %>% group_by(ISO,Module,Policy) %>% filter(year == year[min(which(Value>0))])

#now filter out all 1996 & 1997 introductions (we can't distinguish whether this is an introduction or a preexisting policy, take conservative approach here)

oecd_introductions = oecd_introductions[!oecd_introductions$year == 1996,]
oecd_introductions = oecd_introductions[!oecd_introductions$year == 1997,]


#add a marker for introductions
oecd_introductions$introduction = 1
#
# #subset and merge
oecd_introductions_sub = as.data.frame(oecd_introductions[c('ISO','Module','Policy','year','introduction')])
#
oecd_grouped = merge(oecd_grouped,oecd_introductions_sub,by=c('ISO','Module','Policy','year'),all.x=TRUE)
oecd_grouped$introduction[is.na(oecd_grouped$introduction)] = 0

#now filter out all the introductions that came from NA changes 
oecd_grouped <- oecd_grouped %>%
  group_by(ISO, Module, Policy) %>%
  arrange(year) %>%
  mutate(introduction = ifelse(is.na(lag(Value)), 0,introduction)) %>%
  ungroup()

#2. Get policy intensifications

## Set all NAs to 0 such that NA jumps are not counted for jumps for sure. 
oecd_grouped$Value[is.na(oecd_grouped$Value)] = 0
oecd_grouped$diff[is.na(oecd_grouped$diff)] = 0
oecd_grouped$diff_2[is.na(oecd_grouped$diff_2)] = 0

#make dummies if we get an introduction from first or second lag
oecd_grouped = oecd_grouped %>% mutate(diff_adj = ifelse(diff>=2,1,0), diff_2_adj = ifelse(diff_2>=2,1,0))

##if there is a jump in lag(t-1), don't also count a jump in t-2
oecd_grouped <- oecd_grouped %>%
  group_by(ISO, Module, Policy) %>%
  arrange(year) %>%
  mutate(diff_2_adj = ifelse(lag(diff_adj) == 1, 0,diff_2_adj)) %>%
  ungroup()

## if there is an introduction in t-1, don't also count a jump in t-2 if it increases by 1
oecd_grouped <- oecd_grouped %>%
  group_by(ISO, Module, Policy) %>%
  arrange(year) %>%
  mutate(diff_2_adj = ifelse(lag(introduction) == 1, 0,diff_2_adj)) %>%
  ungroup()

oecd_grouped$diff_2_adj[is.na(oecd_grouped$diff_2_adj)] = 0


#keep all jumps and introductions
oecd_grouped = oecd_grouped[oecd_grouped$diff_adj==1 | oecd_grouped$diff_2_adj==1 | oecd_grouped$introduction==1,]

oecd_grouped = as.data.frame(oecd_grouped)
rownames(oecd_grouped) <- NULL

## filter by modules and merge RDD policies into sectors 
oecd_grouped = oecd_grouped[oecd_grouped$Module %in% c('Buildings','Industry','Transport','Electricity'),]


## adoption subsidies for (i) low-carbon vehicles in transport (e.g. purchase subsidy, tax credit, scrappage scheme) and 
## (ii) major renovations in buildings are currently not covered by the CAPMF. CO2-based vehicle taxes in transport 
## (e.g. registration tax, ownership tax, bonus-malus scheme) are also not yet covered by the CAPMF. 
## We use IEA’s Policies and Measures Database (https://www.iea.org/policies/about) and NewClimate Institute's Climate Policy Database (https://zenodo.org/records/7774473)
## to add available data for these policy instruments.

missing = read.csv('add_on_data.csv')
missing = subset(missing, select=-c(X))

#distinguish sources

#the source OECD indicates both the CAPMF and the IPAC data
oecd_grouped$source ='OECD'

names(missing)[names(missing) == "Year"] <- "year"

missing$source = 'add-on'

oecd_grouped  <- dplyr::bind_rows(oecd_grouped,missing)

## change the names of the policies (for consistency CAPMF names are used for all policies) 

oecd_names = read.csv('CAPMF_policies_names.csv',sep=';')
oecd_names = oecd_names[c('Module',"Policy_new",'Policy_name_fig_1','Policy_name_fig_2_3','Policy_name_fig_4','Market_non_market','Cluster_categories')]
names(oecd_names)[names(oecd_names) == "Policy.Name..our.short.framing."] <- "Policy_name"
names(oecd_names)[names(oecd_names) == "Broad.Category"] <- "Broad_category"
names(oecd_names)[names(oecd_names) == "Policy_new"] <- "Policy"

oecd_grouped <- merge(oecd_grouped, oecd_names, by=c('Module','Policy'),all.x=TRUE)

## add info on country group (high income refers to developed economies, low income to developing economies)
all_countries = as.data.frame(unique(oecd_grouped$ISO))
colnames(all_countries) = c('ISO')
all_countries$high_income = 0  

developed_countries = read.csv('country_groupings.csv')
developed_countries$ISO = countrycode(developed_countries$Developed,origin='country.name',destination='iso3c')
all_countries[all_countries$ISO %in% developed_countries$ISO,'high_income'] = 1


##reference for EU countries  
EU<- data.frame('country'=c("Austria", "Croatia", "Belgium", "Bulgaria", "Cyprus", 
                            "Czech Republic", "Germany", "Denmark", "Spain", "Estonia", "Finland",
                            "France", "United Kingdom", "Greece", "Hungary", "Ireland", "Italy",
                            "Lithuania", "Luxembourg", "Latvia", "Malta", "Netherlands", "Poland",
                            "Portugal", "Romania", "Slovak Republic", "Slovenia", "Sweden"))
EU$ISO = countrycode(EU$country,origin='country.name',destination='iso3c')

EU_15 <- data.frame('country' = c("Austria", "Belgium", "Germany", "Denmark", "Spain", "Finland",
                                  "France", "United Kingdom", "Greece", "Ireland", "Italy",
                                  "Luxembourg", "Netherlands",
                                  "Portugal", "Sweden"))

EU_2004 <- data.frame('country' = c("Czech Republic","Lithuania","Slovenia","Slovak Republic","Hungary","Estonia","Cyprus","Malta","Latvia","Poland"))

EU_2007 <- data.frame('country' = c("Bulgaria","Romania"))

##filter the ones out that were dropped from break detection analysis because overall emissions are too low
final_countries = read.csv('final_countries_in_analysis.csv')
final_countries$ISO = countrycode(final_countries$unique.df_excl.country.,origin='country.name',destination='iso3c')

all_countries = all_countries[all_countries$ISO %in% final_countries$ISO,]
EU_countries = EU[EU$ISO %in% final_countries$ISO,]

oecd_grouped = oecd_grouped[oecd_grouped$ISO %in% final_countries$ISO,]

#1. remove all EU policies we controlled for from EU countries
#because the oecd policise have slightly different names in the "Policy" column this does not filter out any add-on policies

#we retain the 2014+2015 UK breaks bc they are a national and not a European ETS policy 
oecd_grouped$Policy[oecd_grouped$ISO == 'GBR' & oecd_grouped$year %in% c(2014,2015) & oecd_grouped$Policy == "ETS Electricity"] = 'Carbon Price Floor'

EU_policy_controls = c('Labels Appliances','MEPS of appliances','ETS Electricity','ETS Industry','MEPS electric motors','MEPS Transport')
oecd_grouped <- oecd_grouped %>% filter(!(ISO %in% c(EU_countries$ISO) & Policy %in% EU_policy_controls))
oecd_grouped <- oecd_grouped %>% filter(!(ISO %in% c('NOR','ICE') & Policy %in% c('ETS Electricity','ETS Industry')))


#make label df
labels_labels <-expand.grid(country = EU_15$country,Module = c('Buildings'), Policy_name = 'EU-Labels', year = c(2001),label=1)
labels_labels_1 <-expand.grid(country = EU_2004$country,Module = c('Buildings'), Policy_name = 'EU-Labels', year = c(2004),label=1)
labels_labels_2 <-expand.grid(country = EU_2007$country,Module = c('Buildings'), Policy_name = 'EU-Labels', year = c(2007),label=1)
labels_labels_3 <-expand.grid(country = EU$country,Module = c('Buildings'), Policy_name = 'EU-Labels', year = c(2013),label=1)

ets_labels <- expand.grid(country = c(EU_15$country),Module = c('Industry','Electricity'), Policy_name = 'EU-ETS', year = c(2005),label=1)
ets_labels_1 <- expand.grid(country = c(EU_2004$country),Module = c('Industry','Electricity'), Policy_name = 'EU-ETS', year = c(2004),label=1)
ets_labels_2 <- expand.grid(country = c(EU_2007$country),Module = c('Industry','Electricity'), Policy_name = 'EU-ETS', year = c(2007),label=1)
ets_labels_3 <- expand.grid(country = c("Norway","Iceland"),Module = c('Industry','Electricity'), Policy_name = 'EU-ETS', year = c(2008),label=1)
ets_labels_4 <- expand.grid(country = c(EU$country,'Norway','Iceland'),Module = c('Industry','Electricity'), Policy_name = 'EU-ETS', year = c(2018),label=1)

meps_labels <- expand.grid(country = EU$country,Module = c('Buildings'), Policy_name = 'EU-MEPS', year = c(2009,2013),label=1)
meps_labels_2 <- expand.grid(country = EU$country,Module = c('Industry'), Policy_name = 'EU-MEPS', year = c(2011,2015),label=1)
meps_labels_3 <- expand.grid(country = EU$country,Module = c('Transport'), Policy_name = 'EU-MEPS', year = c(2009),label=1)


label_df <- rbind(labels_labels, labels_labels_1, labels_labels_2,labels_labels_3,ets_labels,ets_labels_1,ets_labels_2,ets_labels_3,ets_labels_4,meps_labels,meps_labels_2,meps_labels_3)

label_df$country = gsub(" ","",label_df$country,fixed=TRUE)

## with everything filtered, tag policies that are still slow increases in the data 

#order the data by year in each group 
oecd_grouped <- oecd_grouped %>%
  group_by(ISO, Policy_name_fig_1, Module) %>%
  arrange(year)

oecd_grouped$label= 'jump'

oecd_grouped <- oecd_grouped %>%
  group_by(ISO, Policy_name_fig_1, Module) %>%
  dplyr::mutate(label = ifelse(n_distinct(year) > 1 & diff_2_adj == 1 & lag(diff_2_adj) == 1 & lag(year)==year-1, "slow_increase", label))

#we clean up the remaining NAs in the label column. They come from add-on policies and the first ocurrence of policies that exist multiple times
oecd_grouped$label[is.na(oecd_grouped$label)] = 'jump'

### SAVE the label df (for plotting)

write.csv(label_df,'EU_policies_label_df.csv')

##finally add country group info

developed_countries = read.csv('country_groupings.csv')
developed_countries$ISO = countrycode(developed_countries$Developed,origin='country.name',destination='iso3c')
oecd_grouped$High_income = 0
oecd_grouped[oecd_grouped$ISO %in% developed_countries$ISO,'High_income'] = 1

##count number of developed economies and developing economies 
length(unique(oecd_grouped$ISO[oecd_grouped$High_income==1]))
length(unique(oecd_grouped$ISO[oecd_grouped$High_income==0]))

#27 countries in the developed economies group, 14 countries in the developing economies group group 


write.csv(oecd_grouped,'OECD_data_preprocessed.csv')



