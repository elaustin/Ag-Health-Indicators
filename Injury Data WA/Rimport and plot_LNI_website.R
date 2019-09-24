#Logging Data analysis
#August 16 2018
#Elena Austin


#import data

setwd("C:\\Users\\elaustin\\OneDrive\\Documents\\UW Postdoc\\PNASH\\DataCOD\\Logging\\LNI Injury data (website)")

library(pacman)
p_load("readxl","data.table","ggplot2", "scales")

pdf("summary_dairy.pdf", paper = "letter")

######
#Injury Rates
######
yearly_path = "inetempstatsbySIC.xlsx"

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

yearly_path = "inetempstatsbyNAICS.xlsx"

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



combined  = rbindlist(list(rate[`SIC Description` %in% sic.vals], 
                           rate2[`NAICS Description` %in% naics.vals]))

waprivate = data.table(Year = c(2006:2015), Injury = c(6.6, 6.1, 5.6, 5.1, 4.8, 4.9, 4.8, 4.75, 4.6, 4.5))

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
nature_path = "inetsfclaimssoc2kinjurynatureFY2007-14FY2007-17.xls"

nature = lapply(1:11, FUN = function(x) {
  
  temp = data.table(read_excel(nature_path, sheet = x, skip=6, col_names = c("SOC2K Description",
                                                                             "Injury Nature",
                                                                             "Number of Claims",
                                                                             "Incurred Costs Total",
                                                                             "Incurred Costs Average"), 
                               col_types = "text"))
  temp = temp[!is.na("Injury Nature")   ,]
  temp$year = 2018 - x
  
  temp
})

nature = rbindlist(nature)

nature[, Year := as.POSIXct(as.character(year), "%Y", tz="America/Los_Angeles")]
nature[, Year := paste0("\'", format(Year, "%y"))]
nature[, Year := as.factor(reorder(Year, as.numeric(year)))]

loggingsock2 = unique(grep("Farmworkers, Farm and Ranc", nature$`SOC2K Description`, value = T, ignore.case = T))

nature_logging = nature[`SOC2K Description`%in% loggingsock2]

ggplot(nature_logging[, sum(as.numeric(`Number of Claims`), na.rm=T), by=c("year", "Year")], 
       aes(year, V1)) + 
  geom_point() + theme_light(16) + stat_smooth() + 
  ylab("Number of Claims") + xlab("") + 
  scale_x_continuous(breaks = 2007:2017,  labels = levels(nature_logging$Year))  +
  ggtitle("Total Number of Claims per Year (Logging)")

ggplot(nature_logging[, sum(as.numeric(`Incurred Costs Total`), na.rm=T), by=c("year", "Year")], 
       aes(year, V1/1000)) + 
  geom_point(outlier.colour = NA) + theme_light(16) + stat_smooth() + 
  ylab("Total Incurred Cost (Thousands $)") + xlab("") + 
  scale_x_continuous(breaks = 2007:2017,  labels = levels(nature_logging$Year))  +
  ggtitle("Total Cost of Claims per Year (Logging)")


ggplot(nature_logging[, sum(as.numeric(`Incurred Costs Average`), na.rm=T), by=c("year", "Year")], 
       aes(year, V1/1000)) + 
  geom_point() + theme_light(16) +
  ylab("Average Incurred Costs (Thousands $)") + xlab("")+  stat_smooth() +
  scale_x_continuous(breaks = 2007:2017,  labels = levels(nature_logging$Year))  +
  ggtitle("Average Cost of Claims per Year (Logging)")


nature_logging[, cat := reorder(`Injury Nature`,  -as.numeric(`Number of Claims`), sum, na.rm=T)]
nature_logging[, totalcases := sum(as.numeric(`Number of Claims`), na.rm=T), 
               by = cat]
ggplot(nature_logging[totalcases>15], 
       aes(year, as.numeric(`Number of Claims`), 
           color = `SOC2K Description`)) + 
  geom_point() + theme_light(12) + stat_smooth() +
  ylab("Number of Claims") + xlab("") + 
  facet_wrap(~cat, ncol = 3, 
             labeller = labeller(cat = 
                                   label_wrap_gen(12, width = 20)))+
  scale_x_continuous(breaks = 2007:2017,  
                     labels = levels(nature_logging$Year)) +
  theme(legend.position="bottom") +ggtitle("Number of Claims per Year by Nature of Injury (>15 claims)")

nature_logging[, cat := reorder(`Injury Nature`,  as.numeric(`Incurred Costs Total`), sum, na.rm=T)]
ggplot(nature_logging[as.numeric(`Incurred Costs Total`)/1000>400,], aes(cat,
                                                                         as.numeric(`Incurred Costs Total`)/1000)) + 
  geom_bar(stat = "identity") + theme_light(12) + ylab("Total Incurred Cost (Thousands $)") +
  xlab("") + coord_flip()

nature_logging[, cat := reorder(`Injury Nature`,  as.numeric(`Number of Claims`), sum, na.rm=T)]
nature_logging[, totalcases := sum(as.numeric(`Number of Claims`), na.rm=T), by = cat]
ggplot(nature_logging[totalcases>=200], aes(cat,
                                           as.numeric(`Number of Claims`))) + 
  geom_bar(stat = "identity", width = 0.5) + theme_light(12) + ylab("Total Cases") +
  xlab("") + coord_flip() + labs(fill = "") + ggtitle("Injury Description (Animal Farmworkers)")

######
#TYPE
######

type_path = "inetsfclaimssoc2kaccidenttypeFY2007-17.xlsx"

type = lapply(1:11, FUN = function(j) {
  temp = data.table(read_excel(type_path,  sheet = j, skip=6, col_names = c("SOC2K Description",
                                                                            "Accident Type",
                                                                            "Number of Claims",
                                                                            "Incurred Costs Total",
                                                                            "Incurred Costs Average") , 
                               col_types = "text"))
  temp = temp[!is.na(`Accident Type`)   ,]
  temp$year = 2018 - j
  temp
})

type = rbindlist(type)

type[, Year := as.POSIXct(as.character(year), "%Y", tz="America/Los_Angeles")]
type[, Year := paste0("\'", format(Year, "%y"))]
type[, Year := as.factor(reorder(Year, as.numeric(year)))]

#loggingsock2 = unique(grep("logging", type$`SOC2K Description`, value = T, ignore.case = T))

type_logging = type[`SOC2K Description`%in% loggingsock2]


type_logging[, cat := reorder(`Accident Type`,  -as.numeric(`Number of Claims`), sum, na.rm=T)]
type_logging[, totalcases := sum(as.numeric(`Number of Claims`), na.rm=T), by = cat]

ggplot(type_logging[totalcases >=40*10], aes(year, as.numeric(`Number of Claims`), color = cat))+   
  geom_point() + theme_light(16) + ylab("Number of Claims") +
  xlab("") +   
  #facet_wrap(~cat, ncol = 3, labeller = labeller(cat = label_wrap_gen(10, width = 20))) +
  theme(legend.position="bottom") + stat_smooth(se = F, span = 0.9) + 
  scale_x_continuous(breaks = 2007:2017,  labels = levels(type_logging$Year))+
  labs(color  = "")

ggplot(type_logging[totalcases >=40*10], aes(year, as.numeric(`Incurred Costs Total`), color = cat))+   
  geom_point() + theme_light(16) + ylab("Total Cost") +
  xlab("") +   
  #facet_wrap(~cat, ncol = 3, labeller = labeller(cat = label_wrap_gen(10, width = 20))) +
  theme(legend.position="bottom") + stat_smooth(se = F, span = 0.9) + 
  scale_x_continuous(breaks = 2007:2017,  labels = levels(type_logging$Year))+
  labs(color  = "") + 
  scale_y_continuous(labels = dollar)


type_logging[, cat := reorder(`Incurred Costs Average`,  
                              -as.numeric(`Number of Claims`), sum, na.rm=T)]
type_logging[, totalcost := sum(as.numeric(`Incurred Costs Total`), na.rm=T), by = cat]

# ggplot(type_logging[as.numeric(averagecost)>50000], 
#        aes(year,as.numeric(`Incurred Costs Average`)/1000, color = `SOC2K Description`)) + 
#   geom_point() + theme_light(12) +  stat_smooth() + 
#   ylab("Average Incurred Cost (Thousands $)") +
#   xlab("") + ylim(0,250) + 
#   scale_x_continuous(breaks = 2007:2017,  labels = levels(type_logging$Year)) + 
#   facet_wrap(~`Accident Type`, ncol = 3, labeller = labeller(`Accident Type` = label_wrap_gen(12, width = 20))) +  theme(legend.position="bottom")


type_logging[, cat := reorder(`Accident Type`,  as.numeric(`Incurred Costs Total`), sum, na.rm=T)]
ggplot(type_logging[totalcost>1000000,], aes(cat,
                                             as.numeric(`Incurred Costs Total`)/1000)) + 
  geom_bar(stat = "identity") + theme_light(12) + ylab("Total Incurred Cost (Thousands $)") +
  xlab("") + coord_flip()

type_logging[, cat := reorder(`Accident Type`,  as.numeric(`Number of Claims`), sum, na.rm=T)]
ggplot(type_logging[totalcases>=11*5], aes(cat,
                                           as.numeric(`Number of Claims`),
                                           fill =`SOC2K Description`)) + 
  geom_bar(stat = "identity") + theme_light(12) + ylab("Total Cases") +
  xlab("") + coord_flip()  + labs(fill = "")



######
#SOURCE
######

source_path = "inetsfclaimssoc2ksourceFY2007-17.xlsx"

source = lapply(1:11, FUN = function(j) {
  temp = data.table(read_excel(source_path,  sheet = j, skip=6, col_names = c("SOC2K Description",
                                                                              "Accident source",
                                                                              "Number of Claims",
                                                                              "Incurred Costs Total",
                                                                              "Incurred Costs Average") , 
                               col_types = "text"))
  temp = temp[!is.na(`Accident source`)   ,]
  temp$year = 2018 - j
  temp
})

source = rbindlist(source)

source[, Year := as.POSIXct(as.character(year), "%Y", tz="America/Los_Angeles")]
source[, Year := paste0("\'", format(Year, "%y"))]
source[, Year := reorder(Year, as.numeric(year))]

source_logging = source[`SOC2K Description`%in% loggingsock2]

source_logging[, cat := reorder(`Accident source`,  -as.numeric(`Number of Claims`), sum, na.rm=T)]
source_logging[, totalcases := sum(as.numeric(`Number of Claims`), na.rm=T), by = cat]

ggplot(source_logging[totalcases >=5*11], aes(year, as.numeric(`Number of Claims`), color=`SOC2K Description`))+   geom_point() + theme_light(12) + ylab("Number of Claims") +
  xlab("") +   facet_wrap(~cat, ncol = 3, labeller = labeller(cat = label_wrap_gen(10, width = 20))) +
  theme(legend.position="bottom") + stat_smooth() + 
  scale_x_continuous(breaks = 2007:2017,  labels = levels(source_logging$Year))  

source_logging[, cat := reorder(`Accident source`,  -as.numeric(`Number of Claims`), sum, na.rm=T)]
source_logging[, totalcost := sum(as.numeric(`Incurred Costs Total`), na.rm=T), by = cat]
# 
# ggplot(source_logging[totalcost>1000000,], 
#        aes(Year,as.numeric(`Incurred Costs Total`)/1000, color = `SOC2K Description`)) + 
#   geom_point() + theme_light(12) + ylab("Total Incurred Cost (Thousands $)") +
#   xlab("") + 
#   facet_wrap(~`Accident source`, ncol = 3, labeller = labeller(`Accident source` = label_wrap_gen(12, width = 30))) +
#   theme(legend.position="bottom")

source_logging[, cat := reorder(`Incurred Costs Average`,  -as.numeric(`Number of Claims`), sum, na.rm=T)]
source_logging[, averagecost := sum(as.numeric(`Incurred Costs Average`), na.rm=T), by = cat]

ggplot(source_logging[as.numeric(totalcost)>1000000], 
       aes(year,as.numeric(`Incurred Costs Average`)/1000, color = `SOC2K Description`)) + 
  geom_point() + theme_light(12) +  stat_smooth() + 
  ylab("Average Incurred Cost (Thousands $)") +
  xlab("") + ylim(0,250) + 
  scale_x_continuous(breaks = 2007:2017,  labels = levels(source_logging$Year)) + 
  facet_wrap(~`Accident source`, ncol = 3, labeller = labeller(`Accident source` = label_wrap_gen(12, width = 20))) +  theme(legend.position="bottom")


source_logging[, cat := reorder(`Accident source`,  as.numeric(`Incurred Costs Total`), sum, na.rm=T)]
ggplot(source_logging[totalcost>1000000,], aes(cat,
                                               as.numeric(`Incurred Costs Total`)/1000, fill= Year)) + 
  geom_bar(stat = "identity") + theme_light(12) + ylab("Total Incurred Cost (Thousands $)") +
  xlab("") + coord_flip()

source_logging[, cat := reorder(`Accident source`,  as.numeric(`Number of Claims`), sum, na.rm=T)]
ggplot(source_logging[totalcases>=100], aes(cat,
                                            as.numeric(`Number of Claims`), fill= `SOC2K Description`)) + 
  geom_bar(stat = "identity") + theme_light(12) + ylab("Total Cases") +
  xlab("") + coord_flip()

dev.off()

