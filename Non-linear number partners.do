clear
cd "**"
import delimited nonlinear_data_schiz, numericcols(_all)

rename id n_eid
merge 1:1 n_eid using "**.dta"
drop if _merge!=3
drop _merge

summ num_partners
prop ten_nsp

********************************************************************************
********************************** PLOT ****************************************
********************************************************************************

* derive categorical num_partners

* with reproductive success on the X
mean schizscoremean if ten_nsp==0
mean schizscoremean if ten_nsp==1

* with genetic score on the X
xtile schizscoremean_cat = schizscoremean, nq(5)
mean num_partners if schizscoremean_cat==1
mean num_partners if schizscoremean_cat==2
mean num_partners if schizscoremean_cat==3
mean num_partners if schizscoremean_cat==4
mean num_partners if schizscoremean_cat==5

* deciles - added for R1
xtile schizscoremean_decile = schizscoremean, nq(10)
mean num_partners if schizscoremean_decile==1
mean num_partners if schizscoremean_decile==2
mean num_partners if schizscoremean_decile==3
mean num_partners if schizscoremean_decile==4
mean num_partners if schizscoremean_decile==5
mean num_partners if schizscoremean_decile==6
mean num_partners if schizscoremean_decile==7
mean num_partners if schizscoremean_decile==8
mean num_partners if schizscoremean_decile==9
mean num_partners if schizscoremean_decile==10

********************************************************************************
********************************** QUADRATIC ***********************************
********************************************************************************

* quadratic regression
generate schizscoremean_square = schizscoremean*schizscoremean
regress num_partners schizscoremean sex age_centre pc1-pc10 schizscoremean_square
regress num_partners schizscoremean age_centre pc1-pc10 schizscoremean_square if sex==0 
regress num_partners schizscoremean age_centre pc1-pc10 schizscoremean_square if sex==1

* quadratic plot
twoway qfit num_partners schizscoremean
twoway qfit num_partners schizscoremean if sex==0 
twoway qfit num_partners schizscoremean if sex==1

regress num_partners schizscoremean sex age_centre pc1-pc10 
estimates table, p(%28.26f)
regress num_partners schizscoremean age_centre pc1-pc10  if sex==0 
regress num_partners schizscoremean age_centre pc1-pc10  if sex==1
estimates table, p(%28.26f)

********************************************************************************
***************************** CROPPING SCORE ***********************************
********************************************************************************

* find out deciles cut offs for below
summ schizscoremean if schizscoremean_decile==1
summ schizscoremean if schizscoremean_decile==2
summ schizscoremean if schizscoremean_decile==3
summ schizscoremean if schizscoremean_decile==4
summ schizscoremean if schizscoremean_decile==5
summ schizscoremean if schizscoremean_decile==6
summ schizscoremean if schizscoremean_decile==7
summ schizscoremean if schizscoremean_decile==8
summ schizscoremean if schizscoremean_decile==9
summ schizscoremean if schizscoremean_decile==10

/*Deciles:
minimum = 64
10% = 88
20% = 91
30% = 93
40% = 94.73
50% = 96.07
60% = 98
70% = 99.71
80% = 101.86
90% = 104.48
max = 128
*/

* dropping top 10 percent
generate schizscoremean90 = schizscoremean
replace schizscoremean90=. if schizscoremean >104.487
regress num_partners schizscoremean90 pc1-pc10 
estimates table, p(%28.26f)

* dropping top 20 percent
generate schizscoremean80 = schizscoremean
replace schizscoremean80=. if schizscoremean >101.86
regress num_partners schizscoremean80 pc1-pc10 
estimates table, p(%28.26f)

* dropping top 30 percent
generate schizscoremean70 = schizscoremean
replace schizscoremean70=. if schizscoremean >99.71
regress num_partners schizscoremean70 pc1-pc10 
estimates table, p(%28.26f)

* dropping top 40 percent
generate schizscoremean60 = schizscoremean
replace schizscoremean60=. if schizscoremean >98
regress num_partners schizscoremean60 pc1-pc10 
estimates table, p(%28.26f)

* dropping top 50 percent
generate schizscoremean50 = schizscoremean
replace schizscoremean50=. if schizscoremean >96.07
regress num_partners schizscoremean50 pc1-pc10 
estimates table, p(%28.26f)



* WITHOUT CASES!!!!!!!!!!!!!!!!!!!!!!!!!
drop if schizophrenia==1 // !!!!!!!!!!! DROPPING PEOPLE
xtile schizscoremean_decile = schizscoremean, nq(10)
summ schizscoremean if schizscoremean_decile==5
summ schizscoremean if schizscoremean_decile==6
summ schizscoremean if schizscoremean_decile==7
summ schizscoremean if schizscoremean_decile==8
summ schizscoremean if schizscoremean_decile==9
summ schizscoremean if schizscoremean_decile==10

/*Deciles:
40% = 94.72
50% = 96.07
60% = 98
70% = 99.70
80% = 101.85
90% = 104.47
max = 128
*/

* dropping top 10 percent
generate schizscoremean90 = schizscoremean
replace schizscoremean90=. if schizscoremean >104.47
regress num_partners schizscoremean90 pc1-pc10 
estimates table, p(%28.26f)

* dropping top 20 percent
generate schizscoremean80 = schizscoremean
replace schizscoremean80=. if schizscoremean >101.85
regress num_partners schizscoremean80 pc1-pc10 
estimates table, p(%28.26f)

* dropping top 30 percent
generate schizscoremean70 = schizscoremean
replace schizscoremean70=. if schizscoremean >99.70
regress num_partners schizscoremean70 pc1-pc10 
estimates table, p(%28.26f)


* dropping top 40 percent
generate schizscoremean60 = schizscoremean
replace schizscoremean60=. if schizscoremean >98
regress num_partners schizscoremean60 pc1-pc10 
estimates table, p(%28.26f)


* dropping top 50 percent
generate schizscoremean50 = schizscoremean
replace schizscoremean50=. if schizscoremean >96.07
regress num_partners schizscoremean50 pc1-pc10 
estimates table, p(%28.26f)







