## violence01.R
## reading in and analyzing vital statistics mortality data on homicide
## and firearm deaths: https://wonder.cdc.gov to construct Table 1

setwd(".")


## -------------------------------------------------------------------------------------
## Age-specific homicide rates in prison (NCJ251920, BJS 2020)
## E. Ann Carson, Mary P. Cowhig, Mortality In State And Federal Prisons, 2001-2016
## Downloaded from https://www.bjs.gov/index.cfm?ty=pbdetail&iid=6766, 12/28/20
## -------------------------------------------------------------------------------------

## State and federal prisoner deaths, 2012-2016 (BJS data tables)
dx0 <- read.csv("msfp0116stt02data.csv",
                row.names=1, header=T)
dx0 <- dx0[,c("X2012","X2013","X2014","X2015","X2016")]
hom1216 <- sum(dx0["Homicide",])

## 2001-2016 total state prison deaths by demographics (inc. age)
demodx <- read.csv("msfp0116stt09data.csv",
                   row.names=1, header=T)
pdage <- demodx[8:13,11]/demodx[1,11]        # prop'n of deaths by age (state prison)

## State and federal prison population, 2012-2016
popx <- read.csv("msfp0116stat01data.csv",
                 row.names=1, header=T)
popx <- popx[,c("X2012","X2013","X2014","X2015","X2016")]
pop1216 <- apply(popx[c("17 or younger","18-24","25-34","35-44","45-54","55 or older"),],
                 1, sum)
totpop <- popx[1,]

## death and population by sex
pdm <- demodx[2,11]/demodx[1,11]          # prop'n of deaths by sex (state prison)
ppm <- sum(popx[2,])/sum(popx[2:3,])      # prop'n of inmates by sex (state)

## state prison population age
ppage <- pop1216/sum(pop1216)

## total prison homicides, men, by age 2012-2016 (state prison age-sex distribution)
hommage <- hom1216 * pdage * pdm

## total prison population, men, by age 2012-2016
popmage <- sum(totpop) * ppm * ppage

## men's age-specific homicide rate (total, <=17, 18-24, 25-34, 35-44, 45-54, >=55)
hommrate <- 1e5 * hommage/popmage             # Age-specific men's homicide rate, 2012-2016
totalpris <- sum(hommrate * ppage)    # Age-adjusted total homicide rate

## Demographic weights for age-race adjustment of CDC homicide rates
## Race distribution at age category
wt <- apply(popx[-1,],1,sum)/sum(popx[2:3,])
racewt <- c(wt["black"],wt["white"]+wt["other"],wt["Hispanic"])

## -------------------------------------------------------------------------------------
## Age-specific homicide rates in the population, firearm and nonfirearm
## -------------------------------------------------------------------------------------
## Homicide files are from CDC Wonder extraction from vital statistics mortality files.
## Files downloaded from: https://www.cdc.gov/nchs/fastats/homicide.htm (12/28/20)
## Files are constructed by year, gender, race, Hispanic origin, ten year age groups
## Selection criteria: males, Hispanic or non-Hispanic origin, aged 15-85+,
## injury intent is homicide, and injury mechanism is All Causes of Death

x0 <- read.table("Underlying Cause of Death, 1999-2019 homicide.txt",
                 sep="\t",nrows=1383,header=T)
x0 <- x0[x0$Ten.Year.Age.Groups.Code != "", ]
black <- x0$Race=="Black or African American" & x0$Hispanic.Origin=="Not Hispanic or Latino"
white <- (x0$Race=="White" | x0$Race=="American Indian or Alaska Native" |
          x0$Race=="Asian or Pacific Islander") &  x0$Hispanic.Origin=="Not Hispanic or Latino"
hispanic <- x0$Hispanic.Origin == "Hispanic or Latino"
x0$race <- ifelse(black, 0, NA)
x0$race[white] <- 1
x0$race[hispanic] <- 2
x0$Deaths <- as.numeric(x0$Deaths)
x0$Population <- as.numeric(x0$Population)

## age-specific homicide rate, by race
## Men
xm0 <- x0[x0$Year.Code==2016 & x0$Gender=="Male",]
xm0 <- xm0[!is.na(xm0$Deaths + xm0$Population),]

xm0 <- aggregate(xm0[c("Deaths","Population")],
                  by=list(xm0$race, xm0$Ten.Year.Age.Groups.Code), sum)
xm0[xm0$Group.2=="15-24", c("Deaths","Population")] <-
    xm0[xm0$Group.2=="15-24", c("Deaths","Population")] * .7
temp <- xm0[xm0$Group.2=="55-64"|xm0$Group.2=="65-74"|xm0$Group.2==" 75-84"|xm0$Group.2=="85+",]
temp <- aggregate(temp[,c("Deaths","Population")], by=list(temp$Group.1), sum)
xm0 <- xm0[xm0$Group.2!="65-74" & xm0$Group.2!="75-84" & xm0$Group.2!="85+", ]
xm0[xm0$Group.1=="55-64",] <- temp
xm0$Group.2[xm0$Group.2=="55-64"] <- "55+"
xm0$Group.2[xm0$Group.2=="15-24"] <- "18-24"

xm0$wt <- rep(racewt,5)
deathrt <- 1e5*xm0$Deaths/xm0$Population
wdeathrt <- deathrt * xm0$wt

#race-adjusted homicide rate
cdchomrt <- aggregate(wdeathrt, by=list(xm0$Group.2), sum)
cdchomrttot <- sum(cdchomrt$x * wt[c("18-24","25-34","35-44","45-54","55 or older")])

## Firearm homicide mortality
## Homicide files are from CDC Wonder extraction from vital statistics mortality files.
## Files are constructed by year, gender, race, Hispanic origin, ten year age groups
## Selection criteria: males, Hispanic or non-Hispanic origin, aged 15-85+,
## injury intent is homicide, and injury mechanism is Firearm
x1 <- read.table("Underlying Cause of Death, 1999-2019 homicide gun.txt",
                 sep="\t",nrows=1291,header=T)
x1 <- x1[x1$Ten.Year.Age.Groups.Code != "", ]
black <- x1$Race=="Black or African American" & x1$Hispanic.Origin=="Not Hispanic or Latino"
white <- (x1$Race=="White" | x1$Race=="American Indian or Alaska Native" |
          x1$Race=="Asian or Pacific Islander") &  x1$Hispanic.Origin=="Not Hispanic or Latino"
hispanic <- x1$Hispanic.Origin == "Hispanic or Latino"
x1$race <- ifelse(black, 0, NA)
x1$race[white] <- 1
x1$race[hispanic] <- 2
x1$Deaths <- as.numeric(x1$Deaths)
x1$Population <- as.numeric(x1$Population)

## age-specific homicide rate, by race
## Men
xm1 <- x1[x1$Year.Code==2016 & x1$Gender=="Male",]
xm1 <- xm1[!is.na(xm1$Deaths + xm1$Population),]

xm1 <- aggregate(xm1[c("Deaths","Population")],
                  by=list(xm1$race, xm1$Ten.Year.Age.Groups.Code), sum)
xm1[xm1$Group.2=="15-24", c("Deaths","Population")] <-
    xm1[xm1$Group.2=="15-24", c("Deaths","Population")] * .7
temp <- xm1[xm1$Group.2=="55-64"|xm1$Group.2=="65-74"|xm1$Group.2==" 75-84"|xm1$Group.2=="85+",]
temp <- aggregate(temp[,c("Deaths","Population")], by=list(temp$Group.1), sum)
xm1 <- xm1[xm1$Group.2!="65-74" & xm1$Group.2!="75-84" & xm1$Group.2!="85+", ]
xm1[xm1$Group.1=="55-64",] <- temp
xm1$Group.2[xm1$Group.2=="55-64"] <- "55+"
xm1$Group.2[xm1$Group.2=="15-24"] <- "18-24"


## Non-firearm homicides
xm0$nfdeaths <- xm0$Deaths - xm1$Deaths
nfdeathrt <- 1e5*xm0$nfdeaths/xm0$Population
wnfdeathrt <- nfdeathrt * xm0$wt

#race-adjusted homicide rate
cdcnfhomrt <- aggregate(wnfdeathrt, by=list(xm0$Group.2), sum)
cdcnfhomrttot <- sum(cdcnfhomrt$x * wt[c("18-24","25-34","35-44","45-54","55 or older")])

tab0 <- cbind(c(totalpris, hommrate[-1]),
              c(cdchomrttot, cdchomrt$x),
              c(cdcnfhomrttot, cdcnfhomrt$x))
dimnames(tab0)[[1]][1] <- "All"
dimnames(tab0)[[2]] <- c("Prison","Pop.","Non-Gun Pop.")


## ------------------------------------------------------------------------------------
## Survey of Prison Inmates (2016), UCR arrest (2016) for Part I violence and assault
## ------------------------------------------------------------------------------------
## SPI analyzed in violence01.do (this folder)

## race distribution: NH white/other, NH black, Hispanic
## tab agecat race [aweight=wt] if sex==1 & RV0003B<9, r nof
## Age by race distribution for men

race <- rbind(
         c(    32.86 ,     40.29 ,     26.85 ),
         c(    38.82 ,     35.67 ,     25.51 ),
         c(    40.95 ,     34.73 ,     24.32 ),
         c(    45.81 ,     34.41 ,     19.78 ),
         c(    55.43 ,     28.10 ,     16.46 ))

## . tab agecat sex [aweight=wt], col nof
## Age distribution

spi0 <- rbind(
c(         1 ,      9.72  ,     8.21 ),
c(         2 ,     31.28  ,    36.25 ),
c(         3 ,     27.20  ,    28.98 ),
c(         4 ,     18.88  ,    18.75 ),
c(         5 ,     12.92  ,     7.82 ))

agem <- spi0[,2]
agef <- spi0[,2]


## Respondents written up for assault by age and sex in last 12 months
##. tab agecat sex [aweight=wt], sum(aslt) mean (NB cells are proportions)

spi1 <- rbind(
             c(1 , .12857177,  .11781309 , .12792572),
             c(2 , .07490518,  .05967671 , .07367882),
             c(3 , .04524377,  .02526314 , .04375495),
             c(4 , .02945342,   .0138484 , .02836433),
             c(5 , .02035991,  .02270344 , .02046251))

dimnames(spi1) <- list(c("18-24","25-34","35-44","45-54","55 and over"),
                      c("Age Group","Men","Women","Total"))
total <- c(sum(spi1[,2]*agem/100), sum(spi1[,3]*agef/100))
spiar <- 1000*rbind(total, spi1[,2:3])


## NCVS 2016 victimization rates (from ncvs01.do)
ncvs <- rbind(
         c( .01517572,  .01091703,  .01688312 ),
         c( .01219512,  .01082251,  .00772672 ),
         c(  .0094284,  .00814664,  .00753669 ),
         c( .00943076,  .00887762,  .00657586 ),
         c( .00584322,  .00664568,  .00665883 ))

## Race adjusted victimization rates
ncvsr  <-  1000*apply(ncvs * race/100, 1, sum)

## age-adjusted victimization rates
ncvsaa <- sum(ncvsr*agem/100)

## ---------------------------------------------------------------------
## Table 1
## ---------------------------------------------------------------------
table1 <- round(cbind(tab0, spiar[,1], c(ncvsaa, ncvsr)), 1)
dimnames(table1) <- list(dimnames(table1)[[1]],
                      c("Hom (Pris)","Hom (Pop)", "NF Hom (Pop)",
                        "Asslt (Pris)","Asslt (Pop)"))
