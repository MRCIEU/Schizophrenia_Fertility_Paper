use "**.dta", clear

********************************************************************************
*************************           VARIABLES         **************************
********************************************************************************

* recode sex for plink covar
*recode sex(1=2) (0=1), gen(sexplink)

* age
gen age_centre = n_21003_0_0

****** EXPOSURE EDUCATION: 

* Qualification
tab n_6138_0_0
replace n_6138_0_0=. if n_6138_0_0==-3 // prefer not to say
replace n_6138_0_0=0 if n_6138_0_0==-7 // none of the above
replace n_6138_0_1=. if n_6138_0_1==-3
replace n_6138_0_1=0 if n_6138_0_1==-7
replace n_6138_0_2=. if n_6138_0_2==-3
replace n_6138_0_2=0 if n_6138_0_2==-7
replace n_6138_0_3=. if n_6138_0_3==-3
replace n_6138_0_3=0 if n_6138_0_3==-7
replace n_6138_0_4=. if n_6138_0_4==-3
replace n_6138_0_4=0 if n_6138_0_4==-7
replace n_6138_0_5=. if n_6138_0_5==-3
replace n_6138_0_5=0 if n_6138_0_5==-7
* Subjects who selected several response categories were assigned the higher qualification.
egen qualification=rowmax(n_6138_0_0 n_6138_0_1 n_6138_0_2 n_6138_0_3 n_6138_0_4 n_6138_0_5)

* iscd conversion to years based on Okbay et al. 2016
gen edu_years = .
replace edu_years=20 if qualification==1
replace edu_years=13 if qualification==2
replace edu_years=10 if qualification==3
replace edu_years=10 if qualification==4
replace edu_years=19 if qualification==5
replace edu_years=15 if qualification==6
replace edu_years=7 if qualification==0

* iscd conversion to college binary based on Okbay et al. 2016
gen college=.
replace college=1 if qualification==1
replace college=0 if qualification==2
replace college=0 if qualification==3
replace college=0 if qualification==4
replace college=1 if qualification==5
replace college=0 if qualification==6
replace college=0 if qualification==0
*recode college(1=2) (0=1), gen(collegeplink)

****** OUTCOME NEB:

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
* Childlessness variable
gen childlessM = n_2405_0_0
replace childlessM=. if n_2405_0_0 <0
replace childlessM=100 if n_2405_0_0==0
replace childlessM=0 if childlessM!=100 & childlessM!=.
replace childlessM=1 if childlessM==100
*recode childlessM (0 = 1) (1 = 2), gen(childlessplinkM)

* Creating pooled sex variables
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

* Age at first birth - which one is "how old were you when you had your first child?"
gen age_fbirth_multi = n_2754_0_0 // if indicated that they had had more than 1 child
replace age_fbirth_multi=. if n_2754_0_0 <0


********************************************************************************
*******************                  SAVE            ***************************
********************************************************************************

save "**.dta", replace
cd "**"
outsheet using "**.csv", comma replace

*################################## END ########################################
