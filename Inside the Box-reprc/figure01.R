## figure01.R
## This file creates Figures 1, 2, 5, and Table 2 in "Inside the Box: Safety,
## Health, and Isolation in Prison"
## Bruce Western, 20/25/2021

setwd(".")
library(gdata)
library(readstata13)
library(dplyr)

## ----------------------------------------------------------------------
## Figure 1. Mass incareration in three figures.
## ----------------------------------------------------------------------

# Imprisonment and incarceration times series
x0 <- read.xls("mass incarceration.xlsx", sheet=1)

# Comparative incarceration data
x1 <- read.xls("mass incarceration.xlsx", sheet=2)
n <- dim(x1)[1]
o <- order(x1$rate18)
lab0 <- x1$country

# Cumulative risk of incarceration
x2 <- read.xls("mass incarceration.xlsx", sheet=3)

# Creating the figure
pdf("ts01.pdf", height=10, width=6)
par(mfrow=c(3,1))
par(fig=c(.05,.95, 2/3-.02, 1))
plot(x0$Year, x0$Prison.Rate.per.100k, type="l", col="blue",
     xlim=c(1920, 2020), ylim=c(0, 800),
     xlab="", ylab="Incarceration Rate (per 100,000)", lty=1, lwd=2)
title("(a) US Incarceration, 1925-2018")
lines(x0$Year, x0$TotalRate, lty=2, col="red", lwd=2)
legend(1925, 770, c("Prison and jail","Prison only"), lwd=2,
       lty=c(2,1), col=c("red","blue"), bty="n")
abline(h=seq(0, 800, 200), v=seq(1920,2020,20), lty=3, col="gray")

par(new=T)
par(fig=c(.15,.95, 1/3-.01, 2/3+.01))
plot(x1$rate18[o], 1:n, axes=F, xlab="",
     ylab="", pch="")
abline(h=1:n, lty=3, col="gray")
points(x1$rate18[o], 1:n, pch=16, col="blue", cex=1.5)
title("(b) Incarceration Rates in 15 Countries")
mtext("Incarceration Rate (per 100,000)", side=1, line=1.8, cex=.75)
mtext(lab0[o], 2, at=1:n, las=1, line=1, cex=.7)
axis(1)

par(new=T)
par(fig=c(.05,.95,0, 1/3+.02))
    z <- barplot(x2$risk, col=c("red","blue"), ylim=c(0, 70),
             space=c(0, c(0,.25, 0, .25, 0), 1.5, c(0, .25, 0, .25, .0)),
             ylab="Cumulative Risk of Prison")
title("(c) Men's Cumulative Imprisonment Risk, by Cohort")
mtext(rep(c("All","< College","<HS"), 1), side=1,
      at=c(sum(z[1:2])/2, sum(z[3:4])/2, sum(z[5:6])/2,
           sum(z[7:8])/2, sum(z[9:10])/2, sum(z[11:12])/2),
      line=.5, cex=.7)
mtext(c("White","Black"), side=1,
      at=c(sum(z[3:4])/2, sum(z[9:10])/2), line=1.75, cex=.8)
legend(1.5, 70, c("Born 1945-1949","Born 1975-1979"), fill=c("blue","red"),
       bty="n")
dev.off()

## ----------------------------------------------------------------------
## Figure 2. Enrolment in drug, education, job training programs, and work
## assignment, state prisoners, by region, Survey of Inmates of State Prison
## Inmates.
## ----------------------------------------------------------------------

x <- read.dta13("spi86to16.dta")
x <- x[x$admit==1,]

drugev  <- data.matrix(x %>% group_by(region, year) %>% summarize(weighted.mean(drugev, wt, na.rm=T)))
drugev  <- drugev[!is.nan(drugev[,3]),]
vocev   <- data.matrix(x %>% group_by(region, year) %>% summarize(weighted.mean(vocev, wt, na.rm=T)))
vocev   <- data.matrix(vocev[!is.nan(vocev[,3]),])
work    <- data.matrix(x %>% group_by(region, year) %>% summarize(weighted.mean(work, wt, na.rm=T)))
work    <- data.matrix(work[!is.nan(work[,3]),])
edev    <- data.matrix(x %>% group_by(region, year) %>% summarize(weighted.mean(edev, wt, na.rm=T)))
edev    <- data.matrix(edev[!is.nan(edev[,3]),])

dimnames(drugev) <- dimnames(vocev) <- dimnames(work) <- dimnames(edev) <-
    list(NULL, c("region","year","xbar"))


# Creating the figure
pdf("programs.pdf",height=6, width=8)
par(mfrow=c(1,4))
par(fig=c(0,.29, 0,1))
plot(drugev[drugev[,"region"]==1,"year"],
     drugev[drugev[,"region"]==1,"xbar"]*100,
     type="b", pch=16, main="Drug Program",
     xlim=c(1986, 2016), ylim=c(0,80), col="skyblue2",
     axes=F, ylab="Percent", xlab="")
axis(1)
axis(2)
lines(drugev[drugev[,"region"]==2,"year"], drugev[drugev[,"region"]==2,"xbar"]*100,
      type="b", pch=17, col="dodgerblue")
lines(drugev[drugev[,"region"]==3,"year"], drugev[drugev[,"region"]==3,"xbar"]*100,
      type="b", pch=18, col="red")
lines(drugev[drugev[,"region"]==4,"year"], drugev[drugev[,"region"]==4,"xbar"]*100,
      type="b", pch=19, col="blue")
legend(1986, 80, c("Northeast","Midwest","South","West"),
       pch=16:19, col=c("skyblue2", "dodgerblue", "red","blue"),
       lty=1, bty="n", cex=1.1)
abline(v=seq(1985, 2015, 10), h=seq(0,80, 20), col="gray",lty=3)
par(new=T)
par(fig=c(.237,.527, 0,1))
plot(edev[edev[,"region"]==1,"year"],
     edev[edev[,"region"]==1,"xbar"]*100, type="b", pch=16, main="Education",
     xlim=c(1986, 2016), ylim=c(0,80), col="skyblue2",
     axes=F, ylab="", xlab="")
axis(1)
lines(edev[edev[,"region"]==2,"year"], edev[edev[,"region"]==2,"xbar"]*100, type="b", pch=17, col="dodgerblue")
lines(edev[edev[,"region"]==3,"year"], edev[edev[,"region"]==3,"xbar"]*100, type="b", pch=18, col="red")
lines(edev[edev[,"region"]==4,"year"], edev[edev[,"region"]==4,"xbar"]*100, type="b", pch=19, col="blue")
abline(v=seq(1985, 2015, 10), h=seq(0,80, 20), col="gray",lty=3)
par(new=T)
par(fig=c(.473,.763, 0,1))
plot(vocev[vocev[,"region"]==1,"year"],
     vocev[vocev[,"region"]==1,"xbar"]*100, type="b", pch=16, main="Job Training",
     xlim=c(1986, 2016), ylim=c(0,80), col="skyblue2",
     axes=F, ylab="", xlab="")
axis(1)
lines(vocev[vocev[,"region"]==2,"year"], vocev[vocev[,"region"]==2,"xbar"]*100, type="b", pch=17, col="dodgerblue")
lines(vocev[vocev[,"region"]==3,"year"], vocev[vocev[,"region"]==3,"xbar"]*100, type="b", pch=18, col="red")
lines(vocev[vocev[,"region"]==4,"year"], vocev[vocev[,"region"]==4,"xbar"]*100, type="b", pch=19, col="blue")
abline(v=seq(1985, 2015, 10), h=seq(0,80, 20), col="gray",lty=3)
par(new=T)
par(fig=c(.71,1.0, 0,1))
plot(work[work[,"region"]==1,"year"],
     work[work[,"region"]==1,"xbar"]*100, type="b", pch=16, main="Work",
     xlim=c(1986, 2016), ylim=c(0,80), col="skyblue2",
     axes=F, ylab="", xlab="")
axis(1)
lines(work[work[,"region"]==2,"year"], work[work[,"region"]==2,"xbar"]*100, type="b", pch=17, col="dodgerblue")
lines(work[work[,"region"]==3,"year"], work[work[,"region"]==3,"xbar"]*100, type="b", pch=18, col="red")
lines(work[work[,"region"]==4,"year"], work[work[,"region"]==4,"xbar"]*100, type="b", pch=19, col="blue")
abline(v=seq(1985, 2015, 10), h=seq(0,80, 20), col="gray",lty=3)
dev.off()


## ---------------------------------------------------------------------------
## Figure 5. Density plots for solitary confinement
## NB Data are confidential and no data file is supplied
## ---------------------------------------------------------------------------
library(foreign)
x <- read.dta("analysis_data_finalv11.dta")

## function to return estimated density from kernel density plot
f <- function(dens, quant) {
    near <- (dens$x-quant)^2
    onear <- order(near)[1:2]
    dnear <- diff(dens$x[onear])
    frac <- (quant - dens$x[onear[1]])/dnear
    y <- diff(dens$y[onear])*frac + dens$y[onear[1]]
    y
}

## MEN
x1 <- aggregate(x0$acdcdays, by=list(x0$id, x0$numstint, x0$mentalhealth, x0$male), sum)
x1ma <- x1$x[x1$Group.4=="male" & x1$Group.3=="A"]
x1ma <- x1ma[!is.na(x1ma)]
ma <- density(x1ma)
x1mb <- x1$x[x1$Group.4=="male" & x1$Group.3=="B"]
x1mb <- x1mb[!is.na(x1mb)]
mb <- density(x1mb)
x1mc <- x1$x[x1$Group.4=="male" & x1$Group.3=="C"]
x1mc <- x1mc[!is.na(x1mc)]
mc <- density(x1mc)
x1md <- x1$x[x1$Group.4=="male" & x1$Group.3=="D"]
x1md <- x1md[!is.na(x1md)]
md <- density(x1md)

scale <- .25
mymax <- max(ma$y, mb$y, mc$y, md$y)
ma$y <- 0   + scale*ma$y/mymax
mb$y <- .25 + scale*mb$y/mymax
mc$y <- .50 + scale*mc$y/mymax
md$y <- .75 + scale*md$y/mymax

medm <- round(
        c(quantile(x1ma, .5),
          quantile(x1mb, .5),
          quantile(x1mc, .5),
          quantile(x1md, .5)))
labmedm <- paste(medm, " days", sep="")

p90m <- round(
        c(quantile(x1ma, .9),
          quantile(x1mb, .9),
          quantile(x1mc, .9),
          quantile(x1md, .9)))
labp90m <- paste(p90m, " days", sep="")

## WOMEN
x1fa <- x1$x[x1$Group.4=="female" & x1$Group.3=="A"]
x1fa <- x1fa[!is.na(x1fa)]
fa <- density(x1fa)
x1fb <- x1$x[x1$Group.4=="female" & x1$Group.3=="B"]
x1fb <- x1fb[!is.na(x1fb)]
fb <- density(x1fb)
x1fc <- x1$x[x1$Group.4=="female" & x1$Group.3=="C"]
x1fc <- x1fc[!is.na(x1fc)]
fc <- density(x1fc)
x1fd <- x1$x[x1$Group.4=="female" & x1$Group.3=="D"]
x1fd <- x1fd[!is.na(x1fd)]
fd <- density(x1fd)

scale <- .25
fymax <- max(fa$y, fb$y, fc$y, fd$y)
fa$y <- 0   + scale*fa$y/fymax
fb$y <- .25 + scale*fb$y/fymax
fc$y <- .50 + scale*fc$y/fymax
fd$y <- .75 + scale*fd$y/fymax

medf <- round(
        c(quantile(x1fa, .5),
          quantile(x1fb, .5),
          quantile(x1fc, .5),
          quantile(x1fd, .5)))
labmedf <- paste(medf, " days", sep="")

p90f <- round(
        c(quantile(x1fa, .9),
          quantile(x1fb, .9),
          quantile(x1fc, .9),
          quantile(x1fd, .9)))
labp90f <- paste(p90f, " days", sep="")

# Creating the figure
pdf("density01r.pdf",height=6, width=8)
par(mfrow=c(1,2))
par(fig=c(.1, .6, 0, 1))
plot(c(0,500), c(0,1), pch="", axes=F, xlab="Days in Solitary Confinement", ylab="")
axis(1)
title("Men")
polygon(c(ma$x,rev(ma$x)), c(ma$y, rep(0, length(ma$x))),
                             col="cornflowerblue", border=NA)
segments(quantile(x1ma, .5), 0, quantile(x1ma, .5), f(ma, quantile(x1ma, .5)))
segments(quantile(x1ma, .9), 0, quantile(x1ma, .9), f(ma, quantile(x1ma, .9)))

polygon(c(mb$x,rev(mb$x)), c(mb$y, rep(.25, length(mb$x))),
                             col="cornflowerblue", border=NA)
segments(quantile(x1mb, .5), .25, quantile(x1mb, .5), f(mb, quantile(x1mb, .5)))
segments(quantile(x1mb, .9), .25, quantile(x1mb, .9), f(mb, quantile(x1mb, .9)))

polygon(c(mc$x,rev(mc$x)), c(mc$y, rep(.5, length(mc$x))),
                             col="cornflowerblue", border=NA)
segments(quantile(x1mc, .5), .5, quantile(x1mc, .5), f(mc, quantile(x1mc, .5)))
segments(quantile(x1mc, .9), .5, quantile(x1mc, .9), f(mc, quantile(x1mc, .9)))

polygon(c(md$x,rev(md$x)), c(md$y, rep(.75, length(md$x))),
                             col="cornflowerblue", border=NA)
segments(quantile(x1md, .5), .75, quantile(x1md, .5), f(md, quantile(x1md, .5)))
segments(quantile(x1md, .9), .75, quantile(x1md, .9), f(md, quantile(x1md, .9)))

text(medm+10, seq(0,.75,.25)+.1, labmedm, adj=0)
text(p90m-c(40, 40, 100, 140), seq(0,.75,.25)+.025, labp90m, adj=0)

mtext(c("No Mental\nIllness","Prior\nDiagnosis","Current\nDiagnosis",
        "Serious\nMental Illness"), side=2, at=seq(0,.75,.25)+.1,
      line=0.5, las=1, cex=1.07)

par(new=T)
par(fig=c(.5, 1, 0, 1))
plot(c(0,450), c(0,1), pch="", axes=F, xlab="Days in Solitary Confinement", ylab="")
axis(1)
title("Women")
polygon(c(fa$x,rev(fa$x)), c(fa$y, rep(0, length(fa$x))),
                             col="cornflowerblue", border=NA)
segments(quantile(x1fa, .5), 0, quantile(x1fa, .5), f(fa, quantile(x1fa, .5)))
segments(quantile(x1fa, .9), 0, quantile(x1fa, .9), f(fa, quantile(x1fa, .9)))

polygon(c(fb$x,rev(fb$x)), c(fb$y, rep(.25, length(fb$x))),
                             col="cornflowerblue", border=NA)
segments(quantile(x1fb, .5), .25, quantile(x1fb, .5), f(fb, quantile(x1fb, .5)))
segments(quantile(x1fb, .9), .25, quantile(x1fb, .9), f(fb, quantile(x1fb, .9)))

polygon(c(fc$x,rev(fc$x)), c(fc$y, rep(.5, length(fc$x))),
                             col="cornflowerblue", border=NA)
segments(quantile(x1fc, .5), .5, quantile(x1fc, .5), f(fc, quantile(x1fc, .5)))
segments(quantile(x1fc, .9), .5, quantile(x1fc, .9), f(fc, quantile(x1fc, .9)))

polygon(c(fd$x,rev(fd$x)), c(fd$y, rep(.75, length(fd$x))),
                             col="cornflowerblue", border=NA)
segments(quantile(x1fd, .5), .75, quantile(x1fd, .5), f(fd, quantile(x1fd, .5)))
segments(quantile(x1fd, .9), .75, quantile(x1fd, .9), f(fd, quantile(x1fd, .9)))

text(medf+20, seq(0,.75,.25)+.15, labmedf, adj=0)
text(p90f-c(40, 40, 40, 80), seq(0,.75,.25)+.025, labp90f, adj=0)

dev.off()

## ---------------------------------------------------------------------------
## Table 2. Imprisonment and Solitary Confinement Statistics
## ---------------------------------------------------------------------------
n <- aggregate(x$stintdays, by=list(x$id, x$numstint, x$mentalhealth, x$male), mean)
n <- table(n[,3], n[,4])

## Median length of stay
los <- aggregate(x$stintdays, by=list(x$id, x$numstint, x$mentalhealth, x$male), mean)
losa <- aggregate(los$x, by=list(los$Group.4), median)
los <- aggregate(los$x, by=list(los$Group.3, los$Group.4), median)

## Proportion doing ACDC in stint
x$anysc <- x$s_dc + x$s_ac
x$anysc[is.na(x$anysc)] <- 0
anycs <- aggregate(x$anysc, by=list(x$id, x$numstint, x$mentalhealth, x$male),
                   function(i) ifelse(sum(i)>0, 1, 0))
anycsa <- aggregate(anycs$x, by=list(anycs$Group.4), mean)
anycs <- aggregate(anycs$x, by=list(anycs$Group.3, anycs$Group.4), mean)

## Average number of ACDC spells in a stint, given acdcdays>0
x0 <- x[x$acdcdays>0,]
numcs <- aggregate(x0$anysc, by=list(x0$id, x0$numstint, x0$mentalhealth, x0$male), sum)
numcsa <- aggregate(numcs$x, by=list(numcs$Group.4), mean)
numcs <- aggregate(numcs$x, by=list(numcs$Group.3, numcs$Group.4), mean)

## Median number of total ACDC days in a stint, given acdcdays>0
dayscs <- aggregate(x0$acdcdays, by=list(x0$id, x0$numstint, x0$mentalhealth, x0$male), sum)
dayscsa <- aggregate(dayscs$x, by=list(dayscs$Group.4), median)
dayscs <- aggregate(dayscs$x, by=list(dayscs$Group.3, dayscs$Group.4), median)

## Median number of days in an ACDC spell, given acdcdays>0
days1cs <- aggregate(x0$acdcdays, by=list(x0$mentalhealth, x0$male), median)
days1csa <- aggregate(x0$acdcdays, by=list(x0$male), median)


## Constructing Table 2
all <- c(losa$x[2],anycsa$x[2], numcsa$x[2], days1cs$x[2], dayscsa$x[2],
         losa$x[1],anycsa$x[1], numcsa$x[1], days1cs$x[1], dayscsa$x[1])

table2 <- rbind(los[5:8,3], anycs[5:8,3], numcs[5:8,3], days1cs[5:8,3], dayscs[5:8,3],
              los[1:4,3], anycs[1:4,3], numcs[1:4,3], days1cs[1:4,3], dayscs[1:4,3])
table2 <- cbind(all, table2)
table2[c(1,6),] <- table2[c(1,6),]/365
dimnames(table2) <- list(c("M LOS","M SC","M Num SC","M Days","M Tot Days",
                         "F LOS","F SC","F Num SC","F Days","F Tot Days"),
                       c("All", "A","B","C","D"))

## Adding Sample Size
table2 <- rbind(table2[1:5,],  c(sum(n[,2]), n[,2]),
              table2[6:10,], c(sum(n[,1]), n[,1]))

table2 <- round(table2, 2)
