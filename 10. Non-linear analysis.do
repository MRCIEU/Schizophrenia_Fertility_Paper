cd "**"
import delimited **, numericcols(_all)

********************************************************************************
******************************     CHECKS    ***********************************
********************************************************************************

*hist schizscore
hist schizscoremean
logistic schizophrenia schizscoremean
xtile schizscoremean_cat = schizscoremean, nq(5)

********************************************************************************
********************************** PLOT ****************************************
********************************************************************************

* genetic score on the X
mean neb if schizscoremean_cat==1
mean neb if schizscoremean_cat==2
mean neb if schizscoremean_cat==3
mean neb if schizscoremean_cat==4
mean neb if schizscoremean_cat==5

mean age_fbirth_multi if schizscoremean_cat==1
mean age_fbirth_multi if schizscoremean_cat==2
mean age_fbirth_multi if schizscoremean_cat==3
mean age_fbirth_multi if schizscoremean_cat==4
mean age_fbirth_multi if schizscoremean_cat==5

* with reproductive success on the X - R1
mean schizscoremean if neb_cat==0
mean schizscoremean if neb_cat==1
mean schizscoremean if neb_cat==2
mean schizscoremean if neb_cat==3
mean schizscoremean if neb_cat==4

mean schizscoremean if afb_cat==0
mean schizscoremean if afb_cat==1
mean schizscoremean if afb_cat==2
mean schizscoremean if afb_cat==3

* deciles - R1
xtile schizscoremean_decile = schizscoremean, nq(10)
mean neb if schizscoremean_decile==1
mean neb if schizscoremean_decile==2
mean neb if schizscoremean_decile==3
mean neb if schizscoremean_decile==4
mean neb if schizscoremean_decile==5
mean neb if schizscoremean_decile==6
mean neb if schizscoremean_decile==7
mean neb if schizscoremean_decile==8
mean neb if schizscoremean_decile==9
mean neb if schizscoremean_decile==10

mean age_fbirth_multi if schizscoremean_decile==1
mean age_fbirth_multi if schizscoremean_decile==2
mean age_fbirth_multi if schizscoremean_decile==3
mean age_fbirth_multi if schizscoremean_decile==4
mean age_fbirth_multi if schizscoremean_decile==5
mean age_fbirth_multi if schizscoremean_decile==6
mean age_fbirth_multi if schizscoremean_decile==7
mean age_fbirth_multi if schizscoremean_decile==8
mean age_fbirth_multi if schizscoremean_decile==9
mean age_fbirth_multi if schizscoremean_decile==10

* plot in excel with 95%CI

********************************************************************************
********************************** QUADRATIC ***********************************
********************************************************************************
generate schizscoremean_square = schizscoremean*schizscoremean

* quadratic regression
regress neb schizscoremean sex age_centre pc1-pc10
regress neb schizscoremean sex age_centre pc1-pc10 schizscoremean_square
regress neb schizscoremean pc1-pc10 age_centre if sex ==0
regress neb schizscoremean age_centre pc1-pc10 schizscoremean_square if sex==0 
regress neb schizscoremean pc1-pc10 age_centre if sex ==1
regress neb schizscoremean age_centre pc1-pc10 schizscoremean_square if sex==1
regress age_fbirth_multi schizscoremean pc1-pc10 
regress age_fbirth_multi schizscoremean pc1-pc10 schizscoremean_square 

* quadratic plot
twoway qfit neb schizscoremean
twoway qfit neb schizscoremean if sex==0 
twoway qfit neb schizscoremean if sex==1
twoway qfit age_fbirth_multi schizscoremean


********************************************************************************
***************************** CROPPING SCORE ***********************************
********************************************************************************

* find out deciles cut offs for below
xtile schizscoremean_decile = schizscoremean, nq(10)
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

* dropping top 10 percent
generate schizscoremean90 = schizscoremean
replace schizscoremean90=. if schizscoremean >104.487
regress neb schizscoremean90 pc1-pc10 

* dropping top 20 percent
generate schizscoremean80 = schizscoremean
replace schizscoremean80=. if schizscoremean >101.86
regress neb schizscoremean80 pc1-pc10 

* dropping top 30 percent
generate schizscoremean70 = schizscoremean
replace schizscoremean70=. if schizscoremean >99.71
regress neb schizscoremean70 pc1-pc10 

* dropping top 40 percent
generate schizscoremean60 = schizscoremean
replace schizscoremean60=. if schizscoremean >98
regress neb schizscoremean60 pc1-pc10 

* dropping top 50 percent
generate schizscoremean50 = schizscoremean
replace schizscoremean50=. if schizscoremean >96.07
regress neb schizscoremean50 pc1-pc10 

**** FOR AFB - R1
* dropping top 10 percent
regress age_fbirth_multi schizscoremean90 pc1-pc10 

* dropping top 20 percent
regress age_fbirth_multi schizscoremean80 pc1-pc10 

* dropping top 30 percent
regress age_fbirth_multi schizscoremean70 pc1-pc10 

* dropping top 40 percent
regress age_fbirth_multi schizscoremean60 pc1-pc10 

* dropping top 50 percent
regress age_fbirth_multi schizscoremean50 pc1-pc10 

********** Without Schizophrenia cases
drop if schizophrenia==1 
xtile schizscoremean_decile = schizscoremean, nq(10)
summ schizscoremean if schizscoremean_decile==5
summ schizscoremean if schizscoremean_decile==6
summ schizscoremean if schizscoremean_decile==7
summ schizscoremean if schizscoremean_decile==8
summ schizscoremean if schizscoremean_decile==9
summ schizscoremean if schizscoremean_decile==10

* dropping top 10 percent
generate schizscoremean90 = schizscoremean
replace schizscoremean90=. if schizscoremean >104.47
regress neb schizscoremean90 pc1-pc10 

* dropping top 20 percent
generate schizscoremean80 = schizscoremean
replace schizscoremean80=. if schizscoremean >101.85
regress neb schizscoremean80 pc1-pc10 

* dropping top 30 percent
generate schizscoremean70 = schizscoremean
replace schizscoremean70=. if schizscoremean >99.70
regress neb schizscoremean70 pc1-pc10 

* dropping top 40 percent
generate schizscoremean60 = schizscoremean
replace schizscoremean60=. if schizscoremean >98
regress neb schizscoremean60 pc1-pc10 

* dropping top 50 percent
generate schizscoremean50 = schizscoremean
replace schizscoremean50=. if schizscoremean >96.07
regress neb schizscoremean50 pc1-pc10 

* FOR AFB - R1

* dropping top 10 percent
regress age_fbirth_multi schizscoremean90 pc1-pc10 

* dropping top 20 percent
regress age_fbirth_multi schizscoremean80 pc1-pc10 

* dropping top 30 percent
regress age_fbirth_multi schizscoremean70 pc1-pc10 

* dropping top 40 percent
regress age_fbirth_multi schizscoremean60 pc1-pc10 

* dropping top 50 percent
regress age_fbirth_multi schizscoremean50 pc1-pc10 






