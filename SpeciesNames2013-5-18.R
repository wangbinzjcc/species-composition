setwd('F:\\DataW\\lg-data')
dir()
rc00 <- read.csv('弄岗总物种名录.csv')
rc01 <- read.csv("物种名录wangbin2013-5-17.csv")
head(rc00)
head(rc01)
m00 <- match(rc00$种名,rc01$中文名)
m01 <- na.omit(m00)
m02 <- attr(m01,"na.action")
 m01 m02
 
rc00$生活型[-m02] <- rc01$生活型.乔灌草.[m01]
rc00$常绿落叶[-m02] <- rc01$常绿.落叶[m01]
write.csv(rc00,'弄岗总物种名录00.csv')