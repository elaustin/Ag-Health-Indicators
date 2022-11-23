library(data.table)
library(bit64)
library(ggplot2)
library(ggthemes)

setwd("C:\\Users\\Elena\\OneDrive\\Documents\\UW Postdoc\\Dairy Project - SHARP\\Employment Data")

#download data Washington data by Firm Size & Race Ethnicity

nqwibase_wa <- "http://lehd.ces.census.gov/pub/wa/"
release <- "R2017Q1"
# select DVD-sa_fs for sex/age and DVD-se_fs for sex/education change to fa for firm age
#type <- "DVD-rh_fs"
type <- "DVD-sa_fs"

#get file naming schema
#http://lehd.ces.census.gov/data/schema/latest/lehd_csv_naming.html
#qwi_wa_rh_fs <- "qwi_wa_rh_fs_gs_n4_op_u.csv.gz" #4 digit NAICS state level

qwi_wa_sa_fs <- "qwi_wa_sa_fs_gs_n4_op_u.csv.gz" #4 digit NAICS state level

conr <- gzcon(url(
  paste(nqwibase_wa,release,type,qwi_wa_sa_fs,
        sep="/")))
txt <- readLines(conr)
qwir <- read.csv(textConnection(txt))
qwir<-data.table(qwir)

data_rf_fs <- fread("qwi_wa_rh_fs_gs_n4_op_u.csv")

data_sa_fs <- fread("qwi_wa_sa_fs_gs_n4_op_u.csv")

#generate summary tables

# get data schema
# https://lehd.ces.census.gov/data/schema/V4.1.3/lehd_csv_naming.html 
#get column names

data_rf_fs[year%in%2008:2014&
             industry=="1121"&
             firmsize==0&
             firmage==0&
             agegrp=="A00"&
             race=="A0"&
             ind_level=="4"&
             ownercode=="A05"&
             education=="E0", mean(Emp), 
           by=c("ethnicity", "year")]

data_rf_fs[industry=="1121", median(Emp), 
             by=c("firmsize", "year","quarter","ethnicity","education")]

data_sa_fs[industry=="1121", unique(agegrp), 
           by=c("year")]

data_rf_fs[, year_quarter := paste0(year,".",100*(as.numeric(quarter)/4))]

#Emp most similar to QCEW data
ggplot(data_rf_fs[industry=="1121", mean(Emp,na.rm=T), 
                  by=c("year_quarter","ethnicity","education")], 
       aes(as.numeric(year_quarter), V1, color=ethnicity))+
  geom_point()+stat_smooth() + ylab("Beginning-of-Quarter Employment: Counts")+
  xlab("Year") + scale_color_discrete(labels=c("All", "Not Hispanic","Hispanic"))

firm_size_names <- c(
  `0` = "All Firm Sizes",
  `1` = "<20 Employees",
  `2` = "20-49 Employees",
  `3` = "50-249 Employees",
  `4` = "250-499 Employees",
  `5` = "500+ Employees"
)

#facet plot ethnicity by firm size
ggplot(data_rf_fs[industry=="1121", mean(Emp,na.rm=T), 
                  by=c("year_quarter","ethnicity","firmsize")], 
       aes(as.numeric(year_quarter), V1, color=ethnicity))+
  geom_point()+stat_smooth() + ylab("Beginning-of-Quarter Employment: Counts")+
  xlab("Year") + scale_color_discrete(labels=c("All", "Not Hispanic","Hispanic"))+
  facet_wrap(~firmsize, nrow=2,scales="free_y", labeller = as_labeller(firm_size_names))

#facet plot earnings and ethnicity by firm size
ggplot(data_rf_fs[year%in%2008:2014&industry=="1121", mean(Emp,na.rm=T), 
                  by=c("year","ethnicity","firmsize")], 
       aes(as.numeric(year), V1, color=ethnicity))+
  geom_point()+stat_smooth() + 
  ylab("Beginning-of-Quarter Employment: Average Monthly Earnings")+
  xlab("Year") + scale_color_discrete(labels=c("All", "Not Hispanic","Hispanic"))+
  facet_wrap(~firmsize, nrow=2,scales="free_y", labeller = as_labeller(firm_size_names))


#Hiring rate by quarter

ggplot(data_rf_fs[industry=="1121", mean(HirAEndR,na.rm=T), 
                  by=c("quarter","year","ethnicity","firmsize")], 
       aes(as.numeric(quarter), V1, color=ethnicity))+
  geom_point()+stat_smooth() + 
  ylab("End-of-Quarter Hiring Rate")+
  xlab("Year") + scale_color_discrete(labels=c("All", "Not Hispanic","Hispanic"))+
  facet_wrap(~year, nrow=4,scales="free_y")




#national aggregated data
nqwibase <- "http://lehd.ces.census.gov/data/qwi/us"
release <- "R2016Q1"
type <- "DVD-sa_f"
qwi_us_sa <- "qwi_us_sa_f_gn_ns_op_u.csv.gz"
qwir_us_sa <- "qwir_us_sa_f_gn_ns_op_u.csv.gz"
qwirv_us_sa <- "qwirv_us_sa_f_gn_ns_op_u.csv.gz"

schemabase <- "http://lehd.ces.census.gov/data/schema"
schemaver <- "V4.1d-draft"

#Reading in the Gzipped data
conr <- gzcon(url(
  paste(nqwibase,release,type,qwir_us_sa,
        sep="/")))
txt <- readLines(conr)
qwir <- read.csv(textConnection(txt))
qwir<-data.table(qwir)

#read in variability measures
conrv <- gzcon(url(
  paste(nqwibase,release,type,qwirv_us_sa,
        sep="/")))
txtv <- readLines(conrv)
qwirv <- read.csv(textConnection(txtv))

ids <- read.csv(url
                (paste(schemabase,schemaver,
                       "lehd_identifiers_qwi.csv",
                       sep="/")
                ))

myvars <- merge(
  qwir[which(qwir$industry=="31-33"
             & qwir$agegrp %in% c("A04","A05")
             & qwir$sex   !="0"  ),
       c(as.vector(ids$Variable),
         "HirAEndr")],
  qwirv[,
        c(as.vector(ids$Variable),
          "st_HirAEndr","df_HirAEndr")]
)

# Create bounds
myvars$HirAEndR_lo <-
  myvars$HirAEndR - 1.645*myvars$st_HirAEndR
myvars$HirAEndR_hi <-
  myvars$HirAEndR + 1.645*myvars$st_HirAEndR


