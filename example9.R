library(mosaicData)
data(SaratogaHouses)
names(SaratogaHouses)
sh.price<-as.numeric(SaratogaHouses$price)
length(sh.price)

l.sh.price <- log(sh.price)

mean(l.sh.price)
sd(l.sh.price)
summary(l.sh.price)

xx = seq(from=8.4,to=13.6,length.out = 26)

hist(l.sh.price,xx)

hist(l.sh.price,breaks=xx)


xx1 = c(8.4,seq(11,13.6,0.2))
hist(l.sh.price,xx1)

hist(l.sh.price,breaks=xx1)

x = rnorm(1728,12,0.4)
chisq.test(x)

chisq.test(l.sh.price)




