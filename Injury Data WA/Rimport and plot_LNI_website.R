#Logging Data analysis
#August 16 2018
#Updated November 2022
#Elena Austin


#import data

######
#Injury Rates
######
library(data.table)
library(readxl)
library(ggplot2)

yearly_path = "Injury Data WA/inetempstatsbySIC.xlsx"

rate = lapply(2:10, FUN = function(x) {
  
  temp = data.table(read_excel(yearly_path, sheet = x, skip=6, col_names = c("SIC Code",
                                                                             "SIC Description",
                                                                             "Firm Size",
                                                                             "Average Experience Factor",
                                                                             "Experience Factor .0001-.4999**",	
                                                                             "Experience Factor .5000-.9999",	
                                                                             "Experience Factor 1.0000",	
                                                                             "Experience Factor 1.0001-1.4999",	
                                                                             "Experience Factor 1.5000-plus",
                                                                             "Total Number of Firms", 
                                                                             "Number of Claim-Free Firms",
                                                                             "Claims per 200000 hr"),  na = "",  col_types = "text"))
  temp = temp[!is.na(`Claims per 200000 hr`)   ,]
  temp$year = 2016 - x
  
  temp
})

rate = rbindlist(rate)

yearly_path = "Injury Data WA/inetempstatsbyNAICS_2018.xlsx"

rate2 = lapply(1, FUN = function(x) {
  
  temp2 = data.table(read_excel(yearly_path, sheet = x, skip=6, col_names = c("NAICS Code",
                                                                              "NAICS Description",
                                                                              "Firm Size",
                                                                              "Average Experience Factor",
                                                                              "Experience Factor .0001-.4999**",	
                                                                              "Experience Factor .5000-.9999",	
                                                                              "Experience Factor 1.0000",	
                                                                              "Experience Factor 1.0001-1.4999",	
                                                                              "Experience Factor 1.5000-plus",
                                                                              "Total Number of Firms", 
                                                                              "Number of Claim-Free Firms",
                                                                              "Claims per 200000 hr"),  na = "",  col_types = "text"))
  temp2 = temp2[!is.na(`Claims per 200000 hr`)   ,]
  temp2$year = 2016 - x
  
  temp2
})

rate2 = rbindlist(rate2)

rate[`Firm Size` == 1,`Firm Size` := "1-5 Employees"  ]
rate[`Firm Size` == 6,`Firm Size` := "6-50 Employees"  ]
rate[`Firm Size` == 50,`Firm Size` := "50+ Employees"  ]
rate[`Firm Size` == 0,`Firm Size` := "Zero Hours"  ]
rate[`Firm Size` == "50+",`Firm Size` := "50+ Employees"  ]
rate[`Firm Size` == "6 to 50",`Firm Size` := "6-50 Employees"  ]
rate[`Firm Size` == "1 to 5",`Firm Size` := "1-5 Employees"  ]
rate$`Firm Size` = factor(rate$`Firm Size`, levels =  
                               c("Zero Hours", "1-5 Employees", "6-50 Employees", 
                                 "50+ Employees")) 

rate2[`Firm Size` == 1,`Firm Size` := "1-5 Employees"  ]
rate2[`Firm Size` == 6,`Firm Size` := "6-50 Employees"  ]
rate2[`Firm Size` == 50,`Firm Size` := "50+ Employees"  ]
rate2[`Firm Size` == 0,`Firm Size` := "Zero Hours"  ]
rate2[`Firm Size` == "50+",`Firm Size` := "50+ Employees"  ]
rate2[`Firm Size` == "6 to 50",`Firm Size` := "6-50 Employees"  ]
rate2[`Firm Size` == "1 to 5",`Firm Size` := "1-5 Employees"  ]
rate2$`Firm Size` = factor(rate2$`Firm Size`, levels =  
                            c("Zero Hours", "1-5 Employees", "6-50 Employees", 
                              "50+ Employees")) 


sum(rate$`SIC Description` %in%
rate2$`NAICS Description`)

combined  = rbindlist(list(rate[`SIC Description` %in% sic.vals], 
                           rate2[`NAICS Description` %in% naics.vals]))

#obtained from LNI BLS reports
#https://www.lni.wa.gov/claims/for-employers/workers-compensation-injury-data/labor-statistics-blsi
waprivate = data.table(Year = c(2006:2021), 
                       Injury = c(6.6, 6.1, 5.6, 5.1, 4.8, 4.9, 4.8, 4.75, 4.6, 4.5, 4.3, 4.0, 4.0, 3.8, 3.5, 3.5))

ggplot(combined, aes(as.numeric(year), as.numeric(`Claims per 200000 hr`), 
                     color = `Firm Size`)) + 
  geom_point() + stat_smooth(se=F, span = 0.8) + 
  theme_light(14)+ xlab("") + ggtitle("Claims per 100 Full Time Dairy Employees") +
  ylab("Claims Rate (#/100 Employees)") +
  scale_x_continuous(breaks = 2006:2015, minor_breaks = 2006:2015) + ylim(0,18) + 
  geom_point(data  = waprivate, aes(Year, Injury, color="WA All Private Sector")) + 
  stat_smooth(data  = waprivate, aes(Year, Injury, color="WA All Private Sector"), se=F) + theme(legend.position="bottom") +
  labs(color = "Dairy Size")


######
#NATURE
######
nature_path = "Injury Data WA/inetsfclaimssoc2kinjurynatureFY2007-21.xls"
end_year = 2021

nature = lapply(1:15, FUN = function(x) {
  
  temp = data.table(read_excel(nature_path, sheet = x, skip=6, col_names = c("SOC2K Description",
                                                                             "Injury Nature",
                                                                             "Number of Claims",
                                                                             "Incurred Costs Total",
                                                                             "Incurred Costs Average"), 
                               col_types = "text"))
  temp = temp[!is.na("Injury Nature")   ,]
  temp$year = end_year - x +1
  
  temp
})

nature = rbindlist(nature)

nature[, Year := as.POSIXct(as.character(year), "%Y", tz="America/Los_Angeles")]
nature[, Year := paste0("\'", format(Year, "%y"))]
nature[, Year := as.factor(reorder(Year, as.numeric(year)))]


######
#TYPE
######

type_path = "Injury Data WA/inetsfclaimssoc2kaccidenttypeFY2007-21.xls"
end_year = 2021

type = lapply(1:15, FUN = function(j) {
  temp = data.table(read_excel(type_path,  sheet = j, skip=6, col_names = c("SOC2K Description",
                                                                            "Accident Type",
                                                                            "Number of Claims",
                                                                            "Incurred Costs Total",
                                                                            "Incurred Costs Average") , 
                               col_types = "text"))
  temp = temp[!is.na(`Accident Type`)   ,]
  temp$year = end_year - j + 1
  temp
})

type = rbindlist(type)

type[, Year := as.POSIXct(as.character(year), "%Y", tz="America/Los_Angeles")]
type[, Year := paste0("\'", format(Year, "%y"))]
type[, Year := as.factor(reorder(Year, as.numeric(year)))]

######
#SOURCE
######

source_path = "Injury Data WA/inetsfclaimssoc2ksourceFY2007-21.xls"
end_year = 2021

source = lapply(1:11, FUN = function(j) {
  temp = data.table(read_excel(source_path,  sheet = j, skip=6, col_names = c("SOC2K Description",
                                                                              "Accident source",
                                                                              "Number of Claims",
                                                                              "Incurred Costs Total",
                                                                              "Incurred Costs Average") , 
                               col_types = "text"))
  temp = temp[!is.na(`Accident source`)   ,]
  temp$year = end_year - j +1
  temp
})

source = rbindlist(source)

source[, Year := as.POSIXct(as.character(year), "%Y", tz="America/Los_Angeles")]
source[, Year := paste0("\'", format(Year, "%y"))]
source[, Year := reorder(Year, as.numeric(year))]
 
#create datasets

saveRDS(nature, "Injury Data WA/SOC2KNature.RDS")
saveRDS(rate, "Injury Data WA/SICRate.RDS")
saveRDS(type, "Injury Data WA/SOC2KType.RDS")
saveRDS(source, "Injury Data WA/SOC2KSource.RDS")
saveRDS(waprivate, "Injury Data WA/WAPrivate_rate.RDS")
