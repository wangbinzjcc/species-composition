#########################################
setwd("F:/lg-data/composition")
dir()
h00 <- read.csv("hysIV2013.csv")
head(h00)
sum(h00$Important.value>=1)
sum(h00$Important.value[1:26])


################################################################################
hys00 <- read.csv('distru.ke.shu.hys2013-2-19 170601.csv')
lgd00 <- read.csv('lgdat2013.csv')
lgd00 <- subset(lgd00,sp!='00枯立木' & is.na(bra))
head(lgd00)

s00 <- sort(table(lgd00$sp), decreasing=T)
10/223
sum(s00[1:11])/sum(s00)
57/223
sum(s00[1:57])/sum(s00)



#
jzct <- subset(lgd00,sp=='劲直刺桐' & is.na(bra))
head(jzct)
sum(jzct$dbh<=40 & jzct$dbh>=10)/sum(jzct$dbh<200)
#
tab00 <- table(lgd00$sp)
sum(tab00)
sum(tab00<=150 & tab00 >15)
tab00[tab00<=15]
sum(tab00[tab00<=150& tab00>15])
sum(tab00[tab00>15*10])
#
head(lgd00)
sum(pi/40000*lgd00$dbh^2)/15


lgd01 <- lgd00[is.na(lgd00$bra),]
mean(lgd01$dbh)
sum(lgd01$dbh<=3)/sum(lgd01$dbh<200)

lgd02 <- lgd00[!is.na(lgd00$bra),]
mean(lgd02$dbh)

which.max(lgd00$dbh)
lgd00$sp[87512]
#######################################################################
####
zhong <- table(hys00$科名)
ke.la <- hys00$科拉丁名[match(names(table(hys00$科名)),hys00$科名)]
shu <- tapply(hys00$属名,hys00$科名,function(X)length(unique(X)))
#
abu <- table(hys00$科名[match(lgd00$sp,hys00$种名)])
dbh <- tapply(lgd00$dbh, hys00$科名[match(lgd00$sp,hys00$种名)], function(X)sum(pi/40000*X^2))
IV <- (zhong/sum(zhong) + abu/sum(abu) + dbh/sum(dbh, na.rm=T))*100/3
iv.dat <- round(cbind(shu,zhong,abu,dbh,IV),2) 
nam <- sapply(1:length(ke.la),function(X)paste(names(zhong)[X],ke.la[X]))
iv.dat1 <-  cbind(nam, iv.dat)[order(IV, decreasing=T),]
#
write.csv(iv.dat1, 'ke.iv.data.csv')


########
dir()
hys01 <- read.csv('spe_cod_Hys-2012-10-26 110946.csv')
area00 <- read.csv('科属区系.csv')
head(hys01)
head(area00)
famType00 <- area00$科分布区类型[match(hys01$科名,area00$科名)]
genType00 <- area00$属分布区类型[match(hys01$属名,area00$属名)]
hys02 <- cbind(hys01,famtype=as.factor(famType00),gentype=as.factor(genType00))
write.csv(hys02,'distru.ke.shu.hys1.csv')

######
dist00 <- read.csv('distru.ke.shu.hys2013-2-19 170601.csv')
head(dist00)
t00 <- table(dist00$属名)
sort(t00, decreasing=T)
sum(t00==1)



sub00 <- substr(dist00$科区系,1,1)
tap00 <- tapply(dist00$科名,sub00,function(X)length(unique(X)))
sum(tap00)
length(unique(dist00$科名))
#
head(dist00)
sub01 <- substr(dist00$属区系,1,2)
dist01 <- dist00[-grep('^([[:digit:]][[:digit:]])',sub01),]
sub02 <- substr(dist01$属区系,1,1)

tap00 <- tapply(dist01$属名,sub02,function(X)length(unique(X)))
sum(tap00)
length(unique(dist01$属名))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
head(dist00)
sub01 <- substr(dist00$属区系,1,2)
dist02 <- dist00[grep('^([[:digit:]][[:digit:]])',sub01),]
sub02 <- substr(dist02$属区系,1,2)
tap02 <- tapply(dist02$属名,sub02,function(X)length(unique(X)))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
head(dist00)
sub01 <- substr(dist00$属区系,1,2)
dist03 <- dist00[is.na(sub01),]
sub02 <- substr(dist02$属区系,1,2)
tap02 <- tapply(dist02$属名,sub02,function(X)length(unique(X)))














