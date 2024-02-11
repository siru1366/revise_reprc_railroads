## covid01.R
## This file creates a time series plot of COVID-19 case rates for people in prison,
## prison staff (Figure 3), and the general population, and a dotplot of state prison
## COVID-19 case rates iiin the general population (Figure 4).

## working directory
setwd(".")

## ---------------------------------------------------------------------------
## Figure 3. Time series plot
## ---------------------------------------------------------------------------
library("zoo")
pop <- c(1217352, 412783, 328239523)                                # population denominators
x <- read.csv("covidts01.csv", header=T)                            # reading covid data
x$date <- as.Date(x$date, "%m/%d/%y")                               # date format

## Incidence TS
pp <- 1e5*rollmean((x$pp_cases[-1]-x$pp_cases[-132]), k=7)/pop[1]   # prison rate
sp <- 1e5*rollmean((x$sp_cases[-1]-x$sp_cases[-132]), k=7)/pop[2]   # staff rate
gp <- 1e5*rollmean((x$gp_cases[-1]-x$gp_cases[-132]), k=7)/pop[3]   # population rate

## Creating the figure
pdf("covidts.pdf")
plot(x$date[-(1:7)], pp, type="l", lty=1, col="red", ylim=c(0,150),
     ylab="New Cases per 100,000", xlab="",lwd=2)
lines(x$date[-(1:7)], sp, type="l", lty=2, col="seagreen", lwd=2)
lines(x$date[-(1:7)], gp, type="l", lty=3, col="blue", lwd=2)
abline(v=as.Date(c("2020-04-01","2020-05-01","2020-06-01","2020-07-01",
           "2020-08-01","2020-09-01")), h=seq(0, 150, 25),
       col="gray", lty=3)
legend("topleft", c("Incarcerated","Prison Staff","General Population"),
       lty=1:3, col=c("red","seagreen","blue"), lwd=2, bty="n")
dev.off()

## ---------------------------------------------------------------------------
## Figure 4. Case rates by state
## ---------------------------------------------------------------------------

x <- read.csv("all-states-history.csv", header=T)                    # reading state covid data

x122 <- x[x$date=="2021-01-22",]                                     # data from 1/22/21
pop <- read.csv("statepop.csv", header=F)                            # state population data
ind <- match(x122$state, pop[,2])                                    # matching states
x122$pop <- pop[ind,3]                                               # state population
x122$ccr <- x122$positive/(x122$pop*1e6)                             # state case rate

x1 <- read.csv("COVID-19 Cases in State and Federal Prison Systems.csv",
               header=T)                                             # reading prison covid data

x1 <- lapply(x1, function(i) { sub(",", "", i) })                    # data cleaning
x1 <- lapply(x1, function(i) { sub("NR", NA, i) })
x1[names(x1)[c(2:7,9)]]  <- lapply(x1[names(x1)[c(2:7,9)]], as.numeric)
x1$incpop <- x1$Incarcerated.Positive/x1$Incarcerated.Case.rate.per.1.000

ind <- match(x122$state, x1$Prison.System)                           # matching states
ccr <- data.frame(pop=1000*x122$ccr,
                  prisn=x1$Incarcerated.Case.rate.per.1.000[ind],
                  state=x1$state[ind])                               # case rate dataframe
ccr <- ccr[complete.cases(ccr),]

o1 <- order(ccr[,2])
n1 <- length(o1)


## Creating the figure
pdf("covidcr.pdf",height=8,width=6)
par(fig=c(.1,1,0,1))
plot(ccr[o1,2], 1:n1, pch="", xlim=c(0,800),
     xlab="Cumulative Case Rate (per 1000)", axes=F, ylab="")
abline(h=1:n1, lty=5, col="gray")
axis(1)
points(ccr[o1,1], 1:n1, pch=17, col="blue")
points(ccr[o1,2], 1:n1, pch=16, col="red")
mtext(ccr$state[o1], side=2, at=1:n1, las=1, line=.5, cex=.75)
legend(450, 7, c("Prison","General population"), pch=c(16, 17),
       col=c("red","blue"), bty="n", cex=1.1)
dev.off()

