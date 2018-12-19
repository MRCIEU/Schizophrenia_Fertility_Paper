set more off

********************************************************************************
*******************         MAKING DATASET WOMEN       *************************
********************************************************************************
use "**.dta", clear

keep n_eid n_31_0_0 n_34_0_0 n_52_0_0 n_54_0_0 n_2734_0_0 n_2754_0_0 n_2764_0_0 ///
n_2774_0_0 n_3140_0_0 n_3659_0_0 n_3829_0_0 n_3839_0_0 n_3872_0_0 n_21003_0_0 n_21000_0_0 n_1239_0_0 n_1249_0_0 n_20116_0_0 n_2754_0_0

count //273,467
save "**.dta", replace

* Merge in withdrawals
merge 1:1 n_eid using "**.dta"
drop if _merge ==2 
drop _merge

* Merge in schizophrenia ICD var
merge 1:1 n_eid using "**.dta"
drop if _merge==2
drop _merge

* Saving women dataset
gen sex = n_31_0_0 // check just women (coded as 0)
tab sex

count
save "**.dta", replace

********************************************************************************
*********************         MAKING DATASET MEN       *************************
********************************************************************************
use "**.dta", clear

keep n_3659_0_0 n_21000_0_0 n_21003_0_0 n_eid n_31_0_0 n_34_0_0 n_52_0_0 n_54_0_0 ///
n_2405_0_0 n_2734_0_0 n_2754_0_0 n_2764_0_0 n_2774_0_0 n_2784_0_0 n_2794_0_0 ///
n_2804_0_0 n_2814_0_0 n_2824_0_0 n_3140_0_0 n_21003_0_0 n_21000_0_0 n_1239_0_0 n_1249_0_0 n_20116_0_0 n_2754_0_0

count // 229,182
save "**.dta", replace

* Merge in withdrawals
merge 1:1 n_eid using "**.dta"
drop if _merge ==2 | _merge==3
drop _merge

* merge in schizophrenia ICD var
merge 1:1 n_eid using "**.dta"
drop if _merge==2
drop _merge

* Saving men dataset
gen sex = n_31_0_0 // check just men (coded as 1)

save "**.dta", replace

********************************************************************************
********************           MERGING DATASETS         ************************
********************************************************************************

use "**.dta", clear

merge 1:1 n_eid using "**.dta"
drop if _merge==3
drop _merge
count 

save "**.dta", replace

********************************************************************************
************************           SPLIT VAR         ***************************
********************************************************************************

* merge in interim genetic release data to generate a split var incase need to split in any analysis checks
merge m:1 n_eid using "**.dta", keepus(n_22003_0_0 n_22001_0_0)
drop if _merge==1 
drop if _merge==2
drop _merge
gen split = .
replace split = 1 if n_22001_0_0!=.
replace split = 2 if split!=1

save "**.dta", replace










