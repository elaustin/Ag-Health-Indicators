#Get Ag census data
#https://steemit.com/education/@somethingburger/importing-usda-census-of-agriculture-data-into-r-1511564625-3931718

library(pacman)
p_load(data.table)


#set up
dir.create("raw")
dir.create("rImported")

#all files from USDA FTP quickstats site ftp://ftp.nass.usda.gov/quickstats/
#best way for of downloadable tabular census data I know of
#download to censusData
setwd("raw")
download.file("ftp://ftp.nass.usda.gov/quickstats/qs.census2002.txt.gz", "2002.txt.gz")
download.file("ftp://ftp.nass.usda.gov/quickstats/qs.census2007.txt.gz", "2007.txt.gz")
download.file("ftp://ftp.nass.usda.gov/quickstats/qs.census2012.txt.gz", "2012.txt.gz")

#convert to r
cag2002 <- read.table(gzfile("2002.txt.gz"), sep="\t", header=TRUE)
cag2007 <- read.table(gzfile("2007.txt.gz"), sep="\t", header=TRUE)
cag2012 <- read.table(gzfile("2012.txt.gz"), sep="\t", header=TRUE)

cag2002 = data.table(cag2002)
cag2007 = data.table(cag2007)
cag2012 = data.table(cag2012)

#save
setwd("../rImported")
save(cag2002,file="cag2002.Rda")
save(cag2007,file="cag2007.Rda")
save(cag2012,file="C:/Users/elaustin/OneDrive/Documents/UW Postdoc/PNASH/DataCOD/DataCOD/visualizer/rImported/cag2012.Rda")

