#
# Markham Life Fix
#
# Libraries and cleanup
# rm(list=ls())
library(stringr)
library(dplyr)
library(tidyr)

# load csv with NAs - CSV generated on Tabula
ps<-read.csv("preschool.csv",stringsAsFactors = FALSE,na.strings = "")
colnames(ps) <- c("Program.Name","Age","Location","Day","Time","Start.Date","Classes","Course.Code","Fee")
ch<-read.csv("children.csv",stringsAsFactors = FALSE,na.strings = "")
colnames(ch) <- c("Program.Name","Age","Location","Day","Time","Start.Date","Classes","Course.Code","Fee")
pt<-read.csv("preteen.csv",stringsAsFactors = FALSE,na.strings = "")
colnames(pt) <- c("Program.Name","Age","Location","Day","Time","Start.Date","Classes","Course.Code","Fee")

df<-rbind(ps,ch,pt)
commcentres <- read.csv("commcenters.csv",stringsAsFactors = FALSE)


# repeat.before function from http://stackoverflow.com/questions/7735647/replacing-nas-with-latest-non-na-value
repeat.before <- function(x) {   # repeats the last non NA value. Keeps leading NA
  ind = which(!is.na(x))      # get positions of nonmissing values
  if(is.na(x[1]))             # if it begins with a missing, add the
    ind = c(1,ind)        # first position to the indices
  rep(x[ind], times = diff(   # repeat the values at these indices
    c(ind, length(x) + 1) )) # diffing the indices + length yields how often
}                               # they need to be repeated

# Fix/Expand Program Names
df$Program.Name<-repeat.before(df$Program.Name)

# Expand Age & Replace with Months
df$Age<-repeat.before(df$Age)
df$F_Age <- df$Age  # Save original Age description for later



df$F_Age <- gsub(" yrs\\+"," - 99 yrs",df$F_Age)

age_convert <- function(x) { # Simple conversion of "3 yrs" to 3*12 months or "1 mth"
  if(grepl("mth",x[2])) {
    return(as.numeric(x[1]))
  }
  if(grepl("yr",x[2])) {
    return(as.numeric(x[1])*12)
  }
}

# Take "x [unit] - y unit" and convert to months
age_calc <- function (x) {
  d <- unlist(str_split(x," - "))
  agestart <- str_trim(unlist(str_split(d[1]," ")))
  ageend <- str_trim(unlist(str_split(d[2]," ")))
  if (is.na(agestart[2])) {
    agestart[2] <- ageend[2]
  }
  ageend[1]<-as.numeric(ageend[1])+1 # "5 yr" means up to "5 yrs, 364 days"

  converted <- paste(age_convert(agestart),age_convert(ageend),sep=" - ")
  return(converted)
}

df$F_Age <- sapply(df$F_Age,age_calc)

# Adjust location and covert to factor
df$Location<-str_trim(df$Location)
df$Location<-as.factor(df$Location)

# Adjust day of the week
df$Day<-str_trim(df$Day)
df$F_Day <- gsub("[\\. ]","",df$Day)
df$F_Day<-as.factor(tolower(df$F_Day))

# Adjust start time
df$F_Time <- gsub("\\.","",df$Time)
df$F_Time <- toupper(df$F_Time)

# convert start date to actual date
base_year <- "2016"
df$F_Start.Date <- paste(gsub("\\.","",df$Start.Date),base_year,sep = ' ')
df$F_Start.Date <- as.Date(df$F_Start.Date,format = "%b %d %Y")

#birthday <- as.Date("2010-04-25")

t1 <- df %>%
  separate(col=F_Age,into=c("F_AgeStart","F_AgeEnd"),sep = " - ") %>%
  separate(col=F_Time,into=c("F_TimeStart","F_TimeEnd"),sep = " - ")
t1$F_AgeStart <- as.numeric(t1$F_AgeStart)
t1$F_AgeEnd <- as.numeric(t1$F_AgeEnd)
t1$F_TimeStart <- as.POSIXct(t1$F_TimeStart,format="%I:%M %p")
t1$F_TimeEnd <- as.POSIXct(t1$F_TimeEnd,format="%I:%M %p")

