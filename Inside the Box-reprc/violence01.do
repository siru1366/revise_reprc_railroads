* violence01.do
* Analysis of 2016 Survey of Prison Inmates and 2016 NCVS data on violence
* 12/19/2020

clear

cd  "~/Dropbox/jep/data report/data and code"

capture log close
log using violence01.log, replace
set more off


* Survey of Prison Inmates, 2016
use "spi_violence.dta", clear

gen coaslt=1 if V1404>=1 & V1404<=60
replace coaslt=0 if V1404==. | V1404==0
gen inaslt=1 if V1406>=1 & V1406<=240
replace inaslt=0 if V1406==0 | V1406==.

gen aslt=1 if coaslt==1 | inaslt==1
replace aslt=0 if coaslt==0& inaslt==0

gen agecat=RV0002
replace agecat=5 if RV0002==6

* race code: white/other, NH black, Hispanic
gen race=1 if RV0003B==1 | RV0003B==4 | RV0003B==5
replace race=2 if RV0003B==2
replace race=3 if RV0003B==3
replace race=. if RV0003B==9

gen sex=RV0005

gen wt=V1585

tab agecat sex [aweight=wt], col nof
tab agecat sex [aweight=wt], sum(aslt) mean

tab agecat race [aweight=wt] if sex==1 & RV0003B<9, r nof

* NCVS, 2016
use "ncvs2016.dta", clear

gen age=V3014

gen agecat=1 if age>=18 & age<=24
replace agecat=2 if age>=25 & age<=34
replace agecat=3 if age>=35 & age<=44
replace agecat=4 if age>=45 & age<=54
replace agecat=5 if age>55 & age<=90

gen sex=V3018

gen wt1 = PERREPWGT1

* race code: white/other, NH black, Hispanic
gen race=1 if V3023A==1 | (V3023A>=3 & V3023A<=5) | (V3023A>=7 & V3023A<=9) | ///
             (V3023A>=13 & V3023A<=14) | (V3023A>=17 & V3023A<=20)
replace race=2 if V3023A==2 | V3023A==6 | (V3023A>=10 & V3023A<=12) | (V3023A>=15 & V3023A<=16)
replace race=3 if V3024A==1

* attacked
gen attack=1 if V3040==1
replace attack=0 if V3040==2

tab agecat race if sex==1, sum(attack) mean
tab agecat race if sex==1 [aw=wt1], sum(attack) mean


log close

exit
