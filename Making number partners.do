
********************************************************************************
************************           No. Partners         ************************
********************************************************************************

* Biobank website "Field 2149 was collected from all participants except those who indicated they never had had sexual intercourse, as defined by their answers to Field 2139

********************************************************************************
***************************           WOMEN         ****************************
********************************************************************************

use "**.dta", clear

keep n_eid n_31_0_0 n_2149_0_0 n_2139_0_0

save "**.dta", replace

count //  273,442

* Never had sex
tab n_2139
gen no_sex = 0 if n_2139==-2

* Number of sexual partners
* Drop refused to answer/did not know
* Those that did not report having had sex - biobank did <1
* [Those answering a number greater than 99 were asked to confirm] - biobank did
tab n_2149_0_0
gen num_partners = n_2149_0_0
replace num_partners=. if n_2149_0_0 <0
tab num_partners
replace num_partners=. if num_partners>30 // dropped at nearest to 99th percentile
tab num_partners // 219106

save "**.dta", replace

********************************************************************************
****************************           MEN         *****************************
********************************************************************************

use "**.dta", clear

keep n_eid n_31_0_0 n_2149_0_0 n_2139_0_0

save "**.dta", replace

count // 229,159

* Never had sex
tab n_2139
gen no_sex = 0 if n_2139==-2

* Number of sexual partners
* Drop refused to answer/did not know
* Those that did not report having had sex - biobank did <1
* [Those answering a number greater than 99 were asked to confirm] - biobank did
tab n_2149_0_0
gen num_partners = n_2149_0_0
replace num_partners=. if n_2149_0_0 <0
tab num_partners
replace num_partners=. if num_partners>100 // dropped at nearest to 99th percentile 
tab num_partners // 182751


save "**.dta", replace

********************************************************************************
***************************           Both         *****************************
********************************************************************************

merge 1:1 n_eid using "**.dta"
drop _merge
count 

summ num_partners, detail // 401857

gen ten_nsp = 0
replace ten_nsp =. if num_partners==.
replace ten_nsp = 1 if num_partners>=12 & num_partners!=. //90th percentile
tab ten_nsp // 401857

gen id = n_eid

save "**.dta", replace
cd "**"
outsheet using "**.csv", comma replace
