use "**.dta", clear
cd "**" 
set more off

********************************************************************************
*************************           VARIABLES         **************************
********************************************************************************
* recode sex for plink covar if needed
* recode sex(1=2) (0=1), gen(sexplink)

* age
gen age_centre = n_21003_0_0

* Females...How many children have you given birth to? (Please include live births only)
gen num_kids = n_2734_0_0
replace num_kids=. if num_kids <0
* Childlessness variable
gen childlessF = n_2734_0_0
replace childlessF=. if n_2734_0_0 <0
replace childlessF=100 if n_2734_0_0==0
replace childlessF=0 if childlessF!=100 & childlessF!=.
replace childlessF=1 if childlessF==100
*recode childlessF (0 = 1) (1 = 2), gen(childlessplinkF)

* Males... How many children have you fathered?
gen kids_fathered = n_2405_0_0
replace kids_fathered=. if kids_fathered <0
* Childlesness variable
gen childlessM = n_2405_0_0
replace childlessM=. if n_2405_0_0 <0
replace childlessM=100 if n_2405_0_0==0
replace childlessM=0 if childlessM!=100 & childlessM!=.
replace childlessM=1 if childlessM==100
recode childlessM (0 = 1) (1 = 2), gen(childlessplinkM)

* Creating pooled sex NEB
gen NEB = .
replace NEB=kids_fathered if kids_fathered!=.
replace NEB=num_kids if num_kids!=.
gen childless =.
replace childless = childlessM if childlessM!=.
replace childless = childlessF if childlessF!=.
*recode childless(1=2) (0=1), gen(childlessplink)

* Z score first before you combine pooled NEB - analysis check
zscore num_kids
zscore kids_fathered
gen z_NEB = .
replace z_NEB=z_kids_fathered if z_kids_fathered!=.
replace z_NEB=z_num_kids if z_num_kids!=.

* Ever smoked - analysis check for selection bias
tab n_1239_0_0 // current smoking
replace n_1239_0_0=. if n_1239_0_0 <0
tab n_1249_0_0 // past smoking
replace n_1249_0_0=. if n_1249_0_0 <0
gen ever_smok = .
replace ever_smok =1 if n_1239_0_0==1 | n_1239_0_0==2 | n_1249_0_0==1 | n_1249_0_0==2 | n_1249_0_0==3
replace ever_smok=0 if n_1239_0_0==0 & n_1249_0_0==4 // if said never on both questions
tab ever_smok, missing
* recode ever_smok (0 = 1) (1 = 2), gen(ever_smokplink)
gen smok_status = n_20116_0_0
replace smok_status=. if n_20116_0_0 <0

* Age at first birth - "how old were you when you had your first child?"
gen age_fbirth_multi = n_2754_0_0 // if indicated that they had had more than 1 child
replace age_fbirth_multi=. if n_2754_0_0 <0

********************************************************************************
*******************                  SAVE            ***************************
********************************************************************************

save "**.dta", replace 
outsheet using "**.csv", comma replace

*################################## END ########################################
