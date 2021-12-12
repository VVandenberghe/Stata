
*************************************************************************************************************
*																											*
* Deannuitization vs retirement age differentiation          
*************************************************************************************************************	
* Computation of impact of retirement age differentiation of the propensity to violate acturial fairness ie;
* equalization of rate of return (Bismark) & lifetime benefits (Beveridge)
* indroduction of survival up to retirement S(ra) dummy
* use averages (overall, by SES cat) to differentiate

*Last update : Nov 2021

*actual  retirement age= [alpha] *considered age of death



clear all
set more off

set maxvar 32767  

cd "C:\Users\adminv\OneDrive - UCL\Deannuitization\Stata_work"

//////////////////////////////////////////////////////////////////////////////////////////////////////////
/***Part  1- Simulated mortality rate																	*/
////////////////////////////////////////////////////////////////////////////////////////////////////////////

local l M1 
local h F100

import excel "C:\Users\adminv\OneDrive - UCL\Deannuitization\The-Limited-Power-of-Socioeconomic-Status-to-Predict-Lifespan-main\Data simulation\By income percentile and sex\Simulation - Chetty technique.xlsx", ///
 sheet("Simulation to Stata")  firstrow clear
missings dropvars, force
describe
 keep `l' `h'

 replace `l'=`l'/100000
replace `h'=`h'/100000


gen age = 39+[_n]
gen `l'_w = age*`l'
gen `h'_w = age*`h'
egen age`l'_m=total(`l'_w)
egen age`h'_m=total(`h'_w)
keep if age<=110

local `l'_m =age`h'_m[1]
local `h'_m =age`l'_m[1]

twoway (lowess  `l' age if age<110, bwidth(.15) lc(blue*.3) ) ///
         (lowess `h' age  if  age<110, bwidth(.15) lc(red)), ///
		 xlabel(40[10]110) ///
		 xline(``l'_m',  lwidth(vthin) lc(blue*.3) lp(dash) ) ///
		 	 xline(``h'_m',  lwidth(vthin) lc(red) lp(dash) ) ///
		 legend (label(1 "`l'{superscript:th}perc") label(2 "`h'{superscript:th}perc") region(color(none)))
graph export "C:\Users\adminv\OneDrive - UCL\Deannuitization\fig1.eps", replace


/////////////////////////////////////////////////////////////////////////////////////////////////////////////
/**Part 2 - Identify differentiated retirement ages                                                        */
/////////////////////////////////////////////////////////////////////////////////////////////////////////////

/*load Chetty data Male, Female death by age 40-120 [rows] and income percentile [columns]**/

import excel "C:\Users\adminv\OneDrive - UCL\Deannuitization\The-Limited-Power-of-Socioeconomic-Status-to-Predict-Lifespan-main\Data simulation\By income percentile and sex\Distribution - Chetty technique.xlsx", ///
 sheet("Sheet1") cellrange(A1:OJ81) firstrow clear

** by age of death/longevity (row=1 to 80 ie. l=40 ...120), compute for different gender and percentile, the probability of dying*/

foreach j in F M {
forvalues i = 1(1)100 {
egen T`j'`i' = total(`j'`i') 			//by gender & income percentile : total death 
gen `j'`i'b = `j'`i'/T`j'`i' 			//by gender & income percentile: % of death by age
drop `j'`i'
rename `j'`i'b pr`j'`i'  //probabilty of death/longevity for category J=j*i  (row 1: age 40, row 80= age 120) ==> sum pr=1
}  
}



drop T*

gen age = 39+[_n]


///withdraw * to the one you want 

save data_, replace



/*******Examination zone******************/

use data_, clear
gen n_1= 0
gen n_2= 0
tempvar n
foreach j in F M {  		//loop over gender
forvalues i = 1(1)1 {   	//loog over income percentile
        gen `n'= C`j'`i' * pr`j'`i'
		qui: replace n_1 = n_1 + `n' //(cumulative) number of individuals who die in each cat. 
		qui: replace n_2 = n_2 + (C`j'`i' * pr`j'`i') //(cumulative) number of individuals who die in each cat. 
		drop `n'
		}
}

list age n_1 n_2


  /*********************************************************************************************************/
  /***Data are structured  with one record per (potential) age of death/longevity [age], with [C] containing cat.
	                 population and [pr] the prob to die at that age  ***/
					 /****************************************************/
					
clear all
use data_, clear

****average longevity (and computation of alpha for ra=65)***********

egen N=rowtotal(C*) //total population (1 line is enough as C is identical for each [age])
dis %24.0f N
gen n_= 0
tempvar n
foreach j in F M {  		//loop over gender
forvalues i = 1(1)100 {   	//loog over income percentile
        gen `n'= C`j'`i' * pr`j'`i'
		qui: replace n_ = n_ + `n' //(cumulative) number of individuals who die in each cat. 
		drop `n'
		}
}

*list age n_

gen l_=age* n_/N   //age * share of individuals who dies at [age] in total population
egen l_av=total(l_)

****average longevity by 200 cat. ******
**************************************

foreach j in F M {  		//loop over gender
forvalues i = 1(1)100 {   	//loop over income percentile
        gen l_`j'`i'= age* pr`j'`i'
}
}

list l_F1


foreach j in F M {  		//loop over gender
forvalues i = 1(1)100 {   	//loog over income percentile
        egen l_av`j'`i'=total(l_`j'`i')
}
}
list l_av l_avF1 l_avF100 l_avM1 l_avM100 if _n==1


****average longevity (2 quantiles) ******
**************************************
gen N_Q1=0
foreach j in F M {  		//loop over gender
forvalues i = 1(1)50 {   	//loop over income percentile
        replace N_Q1= N_Q1 + C`j'`i'
}
}
dis N_Q1


gen l_Q1=0
foreach j in F M {  		//loop over gender
forvalues i = 1(1)50 {   	//loog over income percentile
        replace l_Q1=l_Q1+ l_av`j'`i'* (C`j'`i'/N_Q1)
}
}

gen N_Q2=0
foreach j in F M {  		//loop over gender
forvalues i = 51(1)100 {   	//loop over income percentile
        replace N_Q2= N_Q2 + C`j'`i'
}
}
dis N_Q2

gen l_Q2=0
foreach j in F M {  		//loop over gender
forvalues i = 51(1)100 {   	//loog over income percentile
        replace l_Q2= l_Q2+ l_av`j'`i'* (C`j'`i'/N_Q2)
}
}
dis l_Q2


**** Simulated income level for 200 cat ***
/////////////////////////////////

 scalar list  _all 
//assuming male 50th income percentile=1
scalar r=4	// ratio highest/lowest percentile (US 2019) [within gender]
scalar gwg=0 		// gender wage gap (US 2018) ref=male
scalar b=(scalar(r)-1 )/(50+49*scalar(r))
scalar a=1 -50*scalar(b)
dis scalar(a)
dis scalar(b)
dis scalar(a)+ scalar(b)*1
dis scalar(a)+ scalar(b)*50  //should be one
dis scalar(a)+ scalar(b)*100

foreach j in F {  		//loop over gender
forvalues i = 1(1)100 {   	//loog over income percentile
        gen w`j'`i'=(1-scalar(gwg))*[scalar(a) + scalar(b)*`i' ]
}
}

foreach j in  M {  		//loop over gender
forvalues i = 1(1)100 {   	//loog over income percentile
		 gen w`j'`i'=scalar(a) + scalar(b)*`i'  
}
}
save data1_, replace
br



/////////////////////////////////////////////////////////////////////////////////////////////////////////
/**Part 3 -Simulations                                                                                 */
/////////////////////////////////////////////////////////////////////////////////////////////////////////

/*3.1 Impact of diff on inequality*/
////////////////////////////////////

////   Bismark //////////////////

use data1_, clear
scalar drop _all

scalar alpha= 65/l_av
dis scalar(alpha)
scalar alph=(1-scalar(alpha))/scalar(alpha)
dis scalar(alph)

scalar obj = "BIS"

capture frame drop res
capture frame create res Denominator ///
                      Numerator_2 ///
					  index_r ///
					  index_r_adj ///
					  alpha ///
					  str5 obj ///
					  str5 cat_sex_per ///
					  rage_sex_perc ///
					  

capture frame drop temp1
frame copy default temp1

frame temp1 {
	
	*** Denominator (uniform retirement age) ***
***********************************************
tempvar ra
tempvar n
tempvar S
gen long x_ra=0
foreach j in F  M {  //gender
forvalues i = 1(1)100 { 
	    gen `ra'= scalar(alpha)* l_av //retirement age as alpha*average longevity ==> we set it at 65
	     gen `S'= (age>`ra')  		//dummy S
	     gen `n' =C`j'`i' * pr`j'`i'  	//number of individuals in cat.  whose longevity is at least [age]
		 quiet: replace x_ra=x_ra +   abs(scalar(alph) - `S'*[age -`ra']/`ra')* `n'  
		 drop  `ra' `n' `S'
				}  
		  }  //At this stage one has summed gap for all cat. by retirement age [ra] and age of death [age/row]
 list age x_ra

egen float gap_ra = total(x_ra) // Sum over all possible ages of death 

 list age x_ra gap_ra if _n==1

drop  x_* 

scalar Denominator = gap_ra[1] //retain only first line

drop gap*

dis %20.0fc  scalar(Denominator) 


***Numerator (retirement differentiated by GENDER x INCOME PERCENTILE)*****
*******************************************************************************

	
//compute budget_adjusted b_a/b = 
            // [wyir(ra_j)/wyir(ra)]  *  with [wyir] =>wage weighted years spent in retirement (benefit ratio)
           //[wyie(ra_j)/wyie(ra)]  *  with [wyir] =>wage weighted years spent in employment  (contribution ratio)
 *OPTIONAL !!!!            
tempvar ra
tempvar ra_j 	
tempvar n
tempvar S
tempvar S_j
gen float num_B=0
gen float den_B=0
gen float num_C=0
gen float den_C=0
foreach j in F M {
forvalues i = 1(1)100 {
	   gen `ra'= scalar(alpha)* l_av  //retirement age as alpha*average longevity (ie 65)
	  gen `ra_j'= scalar(alpha)* l_av`j'`i' //retirement age as alpha*average longevity of cat.
	  gen `S'=(age> `ra')   		//dummy S
	   gen `S_j'=(age> `ra_j')   		//dummy S_j
      gen `n' =(C`j'`i' * pr`j'`i' ) *  w`j'`i'	 //# of ind. in cat.  whose longevity is at least [age] *WAGE
        replace num_B= num_B + `S'*[age-`ra']* `n' 
		  replace den_B= den_B + `S_j'*[age-`ra_j']* `n' 
		    replace num_C= num_C + (1-`S_j') *age * `n' + `S_j'*`ra_j'* `n' 
		    replace den_C= den_C + (1-`S') *age * `n'  + `S'*`ra'* `n' 
		  
	drop `ra'   `ra_j' `S' `S_j' `n' 
}
}


*list age num_B den_B
 egen float tnum_B= total(num_B)   //sum over all [age]  	wyir(ra)
  egen float tden_B= total(den_B)   //sum over all [age]   wyir(ra_j)
   egen float tnum_C= total(num_C)   //sum over all [age]  	wyie(ra_j)
  egen float tden_C= total(den_C)   //sum over all [age]   wyie(ra)
* list age tnum_B tden_B
drop num_* den_*
scalar b_ =tnum_B[1]/tden_B[1]
scalar c_=tnum_C[1]/tden_C[1]
scalar b_adj=scalar(b_)* scalar(c_)
dis %9.3f scalar(b_) _col(5)  %9.3f scalar(c_)   _col(10) %9.3f scalar(b_adj)


//numerator per se

tempvar ra_j
tempvar n
tempvar S_j
foreach j in F M {
forvalues i = 1(1)100 {
	 *dis l_av`j'`i'
	   gen `ra_j'= scalar(alpha)* l_av`j'`i' //retirement age as alpha*average longevity of cat.
	   gen `S_j'=(age> `ra_j')   		//dummy S
	    gen `n' =C`j'`i' * pr`j'`i'  	//number of individuals in cat.  whose longevity is at least [age]
        gen long x_`j'`i'= abs(scalar(alph)- `S_j'*[age -`ra_j']/`ra_j')* `n' 
		drop  `ra_j' `n' `S_j'
}
}

*list  age x_M1

drop C* pr* 

foreach j in F M {
forvalues i = 1(1)100 {
   egen long gap_`j'`i' = total(x_`j'`i')   //sum over all [age]
}
}

*list  age x_M1 gap_M1

drop  x_*
 *des gap_*
egen Numerator_= rowtotal(gap_*) //sum over all cat.
drop gap_*

//Index per se
scalar Numerator_2 = Numerator_[1]
dis %20.0fc scalar(Numerator_2)
dis %20.0fc scalar(Denominator)
scalar Index_r =  scalar(Numerator_2)/scalar(Denominator)  //without
scalar Index_r_adj =  scalar(b_adj)*scalar(Numerator_2)/scalar(Denominator)  //with budget adjustment

dis %9.3f `=Index_r' // !!!! Equity reduction ==> theta_prime/theta  in de-annuitization
dis %9.3f `=Index_r_adj' // !!!! Equity reduction ==> theta_prime/theta  in de-annuitization
}


*** To know the retirement age by sex and by percentile **

foreach j in F  M {
forvalues i = 1(1)100 {
 scalar cat_`j'_`ì'="`j'_`i'"	
gen rage`j'`i'= scalar(alpha)* l_av + (l_av`j'`i' -l_av) 

//Export results
frame post res (Denominator) (Numerator_2) (Index_r)  (Index_r_adj) (alpha) (obj) (cat_`j'_`ì') (rage`j'`i')

}
}


frame res: save results_bis.dta, replace
use results_bis, clear
br

//////////////////////////////////
////   Beveridge //////////////////
//////////////////////////////

use data1_, clear

scalar drop _all
scalar alpha= 65/l_av
dis scalar(alpha)
scalar alph=(1-scalar(alpha))/scalar(alpha)
dis scalar(alph)

scalar obj = "BEV"
scalar omega= (1-`=alpha')*l_av
dis scalar(omega)

capture frame drop res
capture frame create res Denominator ///
                      Numerator_2 ///
					  index_r ///
					  index_r_adj ///
					  omega ///
					  str5 obj ///
					  str5 cat_sex_per ///
					  rage_sex_perc ///
					  

capture frame drop temp1
frame copy default temp1

frame temp1 {
	
********Denominator (unif. retirement)*******************
tempvar ra
tempvar n
tempvar S

gen long x_ra=0

foreach j in F  M {  //gender
forvalues i = 1(1)100 { 
	    gen `ra'= scalar(alpha)* l_av //retirement age as alpha*average longevity ==> we set it at 65
	     gen `S'= (age>`ra')  		//dummy S
	     gen `n' =C`j'`i' * pr`j'`i'  	//number of individuals in cat.  whose longevity is at least [age]
		 quiet: replace x_ra=x_ra +   abs(scalar(omega) - `S'*[age -`ra'])* `n'  
		 drop  `ra' `n' `S'
				}  
		  }  //At this stage one has summed gap for all cat. by retirement age [ra] and age of death [age/row]
 nois list age x_ra

 des x_ra
egen  float  gap_ra = total(x_ra) // Sum over all possible ages of death 

 list age x_ra gap_ra if _n==1

drop  x_* 

scalar Denominator = gap_ra[1] //retain only first line

dis %20.0fc scalar(Denominator) 

drop gap*


***Numerator (retirement diff. by GENDER x INCOME PERCENTILE)*****
********************************************************************************

//compute budget_adjusted b_a/b =(YIR(ra) /YIR(ra_j)) with YIR=total years spent in retirement
*OPTIONAL !!!!
tempvar ra
tempvar ra_j 	
tempvar n
tempvar S
tempvar S_j
gen float num_=0
gen float den_=0
foreach j in F M {
forvalues i = 1(1)100 {
	   gen `ra'= scalar(alpha)* l_av  //retirement age as alpha*average longevity (ie 65)
	  gen `ra_j'=  l_av`j'`i' - scalar(omega) //retirement age as alpha*average longevity of cat.
	   *gen `ra_j'= `ra' + (l_av`j'`i' -l_av) //retirement age 65 - longevity advantage
	  gen `S'=(age> `ra')   		//dummy S
	   gen `S_j'=(age> `ra_j')   		//dummy S_j
	    gen `n' =C`j'`i' * pr`j'`i'  	//number of individuals in cat.  whose longevity is at least [age]
        replace num_= num_ + `S'*[age-`ra']* `n' 
		  replace den_= den_ + `S_j'*[age-`ra_j']* `n' 
	drop `ra'   `ra_j' `S' `S_j' `n' 
}
}

list age num_ den_
 egen float yir_ra= total(num_)   //sum over all [age]  	yir(ra)
  egen float yir_ra_j= total(den_)   //sum over all [age]   yir(ra_j)
 
drop num_ den_
scalar b_adj= (yir_ra[1]/yir_ra_j[1])
dis %9.3f scalar(b_adj)


//Numerator per se

	dis `=alpha'
	dis `=alph'
	dis `=Denominator '
tempvar ra_j 	
tempvar n
tempvar S_j
foreach j in F M {
forvalues i = 1(1)100 {
	 *dis l_av`j'`i'
	   gen `ra_j'= scalar(alpha)* l_av`j'`i' //retirement age as alpha*average longevity of cat.
	   gen `S_j'=(age> `ra_j')   		//dummy S
	    gen `n' =C`j'`i' * pr`j'`i'  	//number of individuals in cat.  whose longevity is at least [age]
        gen long x_`j'`i'= abs(scalar(omega) - `S_j'*[age-`ra_j'])* `n' 
		drop  `ra_j' `n' `S_j'
}
}

*nois list  age x_M1

drop C* pr* 

foreach j in F M {
forvalues i = 1(1)100 {
   egen gap_`j'`i' = total(x_`j'`i')   //sum over all [age]
}
}

nois list  age x_M1 gap_M1
drop  x_*
 nois des gap_*
egen Numerator_= rowtotal(gap_*) //sum over all cat.
drop gap_*

//Indice per se 
scalar Numerator_2 = Numerator_[1]
dis  %20.0fc  scalar(Numerator_2)
dis %20.0fc  scalar(Denominator)
scalar index_r=  (scalar(Numerator_2)/scalar(Denominator)) //no adjusted
scalar index_r_adj= scalar(b_adj)* (scalar(Numerator_2)/scalar(Denominator)) //adjusted
dis %9.3f scalar(index_r) // !!!! Equity reduction ==> b_prime/b  in de-annuitization
dis %9.3f scalar(index_r_adj) // !!!! Equity reduction ==> b_prime/b  in de-annuitization

*** To know the retirement age by sex and by percentile **

foreach j in F  M {
forvalues i = 1(1)100 {
	
 scalar cat_`j'_`ì'="`j'_`i'"	
gen rage`j'`i'= scalar(alpha)* l_av`j'`i'

//Export results
frame post res (Denominator) (Numerator_2) (index_r) (index_r_adj)  (omega) (obj) (cat_`j'_`ì') (rage`j'`i')

}
}

}

frame res: save results_bev.dta, replace
use results_bev, clear
br

***********   ASSEMBLE RESULTS  for diff ret. *******************************
use results_bis, clear
append using results_bev
rename rage_sex_perc rage_
gen gender=substr(cat,1,1)
gen perc=substr(cat,3,.)
destring perc, replace
save results, replace
des

//Export latex table //	
keep if inlist(perc, 1,10,20,30, 40, 50,60,70,80, 90, 100)	 
keep index_r index_r_adj rage_ obj gender perc
gen obj_gen= obj+ge
tab obj_gen
save x, replace
br

use x, clear
keep rage_ perc obj_gen
reshape wide  rage_ , i(perc) j(obj_gen) string
rename (rage_*) (*)
save x1, replace

use x, clear
keep index_r index_r_adj perc obj_gen
reshape wide  index_r index_r_adj  , i(perc) j(obj_gen) string
rename (index_r*) (*)
keep if _n==1
recode perc (1=-9999) 
save x2, replace

use x, clear
gen ref_rage=65
keep ref_rage perc obj_gen
reshape wide  ref_rage  , i(perc) j(obj_gen) string
rename (ref_rage*) (*)
keep if _n==1
recode perc (1=9999) 
save x3, replace


use x2, clear
append using x1
append using x3
des 
br

/*without adjustment*/
estpost  tabstat B*, by(perc) stat(mean) notot
esttab . using "C:\Users\adminv\OneDrive - UCL\Deannuitization\tab1.tex", ///
  cells("BEVF(fmt(%9.3fc)) BEVM BISF BISM") replace ///
 noobs nomtitle nonumber compress nogaps     ///
 addnote("Source: Chelly, our calculations")
 
 /*with adjustment*/
 estpost  tabstat BEVF _adjBEVF  BEVM  _adjBEVM BISF _adjBISF BISM _adjBISM, by(perc) stat(mean) notot
esttab . using "C:\Users\adminv\OneDrive - UCL\Deannuitization\tab1_app.tex", ///
  cells("BEVF(fmt(%9.3fc))_adjBEVF  BEVM  _adjBEVM BISF _adjBISF BISM _adjBISM") replace ///
 noobs nomtitle nonumber compress nogaps     ///
 addnote("Source: Chelly, our calculations")


graph twoway (scatter rage_ perc if gender=="M" & obj=="BIS", msymbol(x) msize(small) mcol(blue*.5) ) ///
           (scatter rage_ perc if gender=="F" & obj=="BIS", msymbol(x) msize(small) mcol(red)) , ///
		 legend (label(1 "Male") label(2 "Female") region(color(none))) ///
		 ytitle("Retirement Age", size(small)) ///
     xtitle("Income Percentile", size(small)) ///
	 yline(65,  lwidth(vthin) lcol(green) ) ///
	 text(64.5 40 "Uniform ret. age", size(vsmall) lwidth(thin) col(green)) 
graph export "C:\Users\adminv\OneDrive - UCL\Deannuitization\fig2_BIS.eps", replace


graph twoway (scatter rage_ perc if gender=="M" & obj=="BEV", msymbol(x) msize(small) mcol(blue*.5) ) ///
           (scatter rage_ perc if gender=="F" & obj=="BEV", msymbol(x) msize(small) mcol(red)) , ///
		 legend (label(1 "Male") label(2 "Female") region(color(none))) ///
		 ytitle("Retirement Age", size(small)) ///
     xtitle("Income Percentile", size(small)) ///
	 yline(65,  lwidth(vthin) lcol(green) ) ///
	 text(64.5 40 "Uniform ret. age", size(vsmall) lwidth(thin) col(green)) 
graph export "C:\Users\adminv\OneDrive - UCL\Deannuitization\fig2_BEV.eps", replace


/////////////////////////////////////
/*3.2 Impact of VOTE / PREFERENCES */
////////////////////////////////////
* De-annuitization is intrinsically better at dealing with (realized) longevidy differences
* What about the chance that is gets supported


////////////////////////////
* Beveridge
//////////////////////////////////

****De-annuitization
*********************
scalar drop _all

use results_bev, clear
scalar index_r=index_r[1]    //recuperate index reduction from ret. diff. simulations
dis `=index_r'                   // i.e. b'/b= index_r
 
use data1_, clear
scalar alpha= 65/l_av
dis scalar(alpha)
scalar alph=(1-scalar(alpha))/scalar(alpha)
dis scalar(alph)

capture frame drop temp5
frame copy default temp5

frame temp5 {

tempvar ra 	
tempvar n
tempvar S
gen float x_=0
foreach j in F M {
forvalues i = 1(1)100 {
	 *dis l_av`j'`i'
	  gen `ra'= `=alpha'* l_av //retirement age as alpha*average longevity
	   gen `S'=(age> `ra')   		//dummy S
	    gen `n' =C`j'`i' * pr`j'`i'  	//number of individuals in cat.  whose longevity is at least [age]
        replace x_= x_ + `S'*[age-`ra']* `n' 
		drop `ra' `n' `S'
}
}
list age x_ 
 egen float yir= total(x_)   //sum over all [age] ==> years in retirement
drop x_
//caracterise LS = (b-b') yir(ra)/N ==> LS/b = (1-index_red) yir(ra)/N

 scalar yir= yir[1]
dis %20.0fc `=yir'
dis %20.0fc N[1]
scalar  ayir = `=yir'/N[1]
dis %20.3fc `=ayir'

scalar LS_b= (1-`=index_r')*`=ayir'
dis  %9.3f `=LS_b'   //LS as share of annuity

// Identify winners under de-annuitization
tempvar ra
tempvar n
tempvar S
tempvar I
gen x_=0
foreach j in F M {
forvalues i = 1(1)100 {
	  gen `ra'= `=alpha'* l_av //retirement age as alpha*average longevity
	   gen `S'=(age> `ra')   		//dummy S
	    gen `n' =C`j'`i' * pr`j'`i'  	//number of individuals in cat.  whose longevity is at least [age]
		*We would write gen `I'=(LS +`S'*b'[age-`ra'] >= `S'*b[age-`ra']) //people who win under diff.
		*How to avoid having to specify b?
		    *Extract or divide all terms by b
			*Note that LS/b=(1 - index_r)* yir(ra)/N
			* and b'/b= index_r
			* and b/b=1
			*thus ...
        gen `I'=(scalar(LS_b)  +`S'*`=index_r'*[age-`ra'] >= `S'*1*[age-`ra']) 
		replace x_= x_ + `I'* `n' 
		drop `ra'  `S'  `n' `I'
}
}
list age x_ 
 egen float N_I= total(x_)   //sum over all [age]
 drop x_

 dis %20.0fc N_I[1]
dis %20.0fc N[1]
dis %9.3f N_I[1]/N[1]  //share of people who win (ref. retirement age 65)
}

****De-annuitization - sensitivity of support to retirement age (ra)
*********************************************************************
*Loop over ra 

capture frame drop res
capture frame create res rage ///
                         share
					  

scala drop _all
use data1_, clear


 scalar min = 50
scalar max =  int(l_av)  //loop from 60 to 82

capture frame drop temp7
frame copy default temp7

frame temp7 {

quiet forvalues ra =`=min'(1) `=max' { //loop over retirement ages

//Compute index reduction based on retirement age [ra] ==> `=index_r'
include  "C:\Users\adminv\OneDrive - UCL\Deannuitization\Index_Bev.do"


//caracterise LS = (b-b') yir(ra)/N ==> LS/b = (1-index_red) yir(ra)/N

			//computer yir(ra)
tempvar n
tempvar S
gen float x_`ra'=0
foreach j in F M {
forvalues i = 1(1)100 {
	   gen `S'=(age> `ra')   		//dummy S
	    gen `n' =C`j'`i' * pr`j'`i'  	//number of individuals in cat.  whose longevity is at least [age]
        replace x_`ra'= x_`ra' + `S'*[age-`ra']* `n' 
		drop  `n' `S'
}
}
*list age x_`ra' 
 egen float yir_`ra'= total(x_`ra')   //sum over all [age] ==> years in retirement
drop x_`ra'

nois dis `ra' _col(10) %20.0fc yir_`ra'


  			//computer yir(ra) /N
 scalar yir`ra'= yir_`ra'[1]
nois dis %20.0fc `=yir`ra''
nois dis %20.0fc N[1]
 
scalar  ayir`ra' = `=yir`ra''/N[1]
nois dis `ra' _col(5) %20.3fc `=ayir`ra''


         		//and finally LS/b
scalar LS_b`ra'= (1-`=index_r')*`=ayir`ra''
nois dis  %9.0f `ra'  %9.3f `=LS_b`ra''   //LS as share of annuity


// Compute indifference age

scalar age_I_`ra'= `=ayir`ra''+ `ra'
nois dis  %9.0f `ra' _col(5) %9.3f  `=age_I_`ra''  //share of people who win (ref. retirement age 65)

// Identify winners under de-annuitization
tempvar n
tempvar S
tempvar I
gen x_=0
foreach j in F M {
forvalues i = 1(1)100 {
	   gen `S'=(age> `ra')   		//dummy S
	    gen `n' =C`j'`i' * pr`j'`i'  	//number of individuals in cat.  whose longevity is at least [age]
		*We would write gen `I'=(LS +`S'*b'[age-`ra'] >= `S'*b[age-`ra']) //people who win under diff.
		*How do avoid having to specify b?
		    *Extract or divide all terms by b
			*Note that LS/b=(1 - index_r)* yir(ra)/N= LS_b
			*and b'/b= index_r
			* and b/b=1
			*thus ...
        gen `I'=(`=LS_b`ra''  +`S'*scalar(index_r)*[age-`ra'] >= `S'*1*[age-`ra']) 
		replace x_= x_ + `I'* `n' 
		drop  `S'  `n' `I'
}
}
*list age x_`ra' 
 egen float N_I= total(x_)   //sum over all [age]
 drop x_

scalar  share_= N_I[1]/N[1]
drop N_I
nois dis  %9.0f `ra' _col(5) %9.3f   scalar(share_)  //share of people who win (ref. retirement age 65)

//export results
frame post res (`ra') (share_) 

}
}


frame res: save results_bev_ra.dta, replace
use results_bev_ra, clear
br

////////////////////////////
* Bismark
//////////////////////////////////

****De-annuitization ra=65
*********************

scalar drop _all
use results_bis, clear
scalar index_r=index_r[1]    //recuperate index reduction from ret. diff. simulations (rho'/rho)
dis `=index_r'                   

 
use data1_, clear

scalar alpha= 65/l_av
dis scalar(alpha)
scalar alph=(1-scalar(alpha))/scalar(alpha)
dis scalar(alph)

capture frame drop temp5
frame copy default temp5


frame temp5 {

* Num_ & Den_       which are wage weighted      
tempvar ra
tempvar n
tempvar S
tempvar S_j
gen float num_=0
gen float den_=0

foreach j in F M {
forvalues i = 1(1)100 {
	   gen `ra'= scalar(alpha)* l_av  //retirement age as alpha*average longevity (ie 65)
	  gen `S'=(age> `ra')   		//dummy S
      gen `n' =(C`j'`i' * pr`j'`i' ) *  w`j'`i'	 //# of ind. in cat.  whose longevity is at least [age] *WAGE
        replace num_= num_+ `S'*[age-`ra']* `n' 
		  replace den_= den_ + `ra'* `n' 	  
	drop `ra'   `S'  `n' 
}
}

list age num_ den_
 egen float num= total(num_)   //sum over all [age] 
  egen float den= total(den_)   //sum over all [age] ==> years in retirement
drop num_  den_

* mu = (rho-rho')* num/den  

scalar mu= (1-`=index_r')* num[1]/den[1]
dis  %9.3f scalar(mu)   //mu as garanteed rr

// Identify winners under de-annuitization
tempvar ra
tempvar n
tempvar S
tempvar I
gen x_=0
foreach j in F M {
forvalues i = 1(1)100 {
	  gen `ra'= `=alpha'* l_av //retirement age as alpha*average longevity
	   gen `S'=(age> `ra')   		//dummy S
	    gen `n' =C`j'`i' * pr`j'`i'  	//number of individuals in cat.  whose longevity is at least [age]
		*We would write gen `I'=(mu + rho' * `S'[age-`ra']/ra >= rho* `S'*b[age-`ra']/ra) .
		*equivalently as rho-rho'= (1-index_r)
        gen `I'=(scalar(mu) > (1-scalar(index_r))*`S'*[age-`ra']/`ra') 
		replace x_= x_ + `I'* `n' 
		drop `ra'  `S'  `n' `I'
}
}
list age x_ 
 egen float N_I= total(x_)   //sum over all [age]
 drop x_

 dis %20.0fc N_I[1]
dis %20.0fc N[1]
dis %9.3f N_I[1]/N[1]  //share of people who win (ref. retirement age 65)
}

****De-annuitization - sensitivity of support to retirement age (ra)
*********************************************************************
*Loop over ra 
			  
capture frame drop res
capture frame create res rage ///
                         share
					  
use data1_, clear

scalar drop _all



 scalar min = 50
scalar max = int(l_av)  //loop from 60 to 82

capture frame drop temp7
frame copy default temp7

frame temp7 {

quiet forvalues ra =`=min'(1) `=max' { //loop over retirement ages
nois dis "`ra'"

//Compute index reduction achieved via RAD based on retirement age [ra] ==> index_r
include  "C:\Users\adminv\OneDrive - UCL\Deannuitization\Index_Bis.do"

* Num_ & Den_       which are wage weighted      

tempvar n
tempvar S
gen float num_=0
gen float den_=0

foreach j in F M {
forvalues i = 1(1)100 {
	  gen `S'=(age> `ra')   		//dummy S
      gen `n' =(C`j'`i' * pr`j'`i' ) *  w`j'`i'	 //# of ind. in cat.  whose longevity is at least [age] *WAGE
        replace num_= num_+ `S'*[age-`ra']* `n' 
		  replace den_= den_ + `ra'* `n' 	  
	drop   `S'  `n' 
}
}


list age num_ den_
 egen float tnum_= total(num_)   //sum over all [age] 
  egen float tden_= total(den_)   //sum over all [age] ==> years in retirement
  
* mu = (rho-rho')* tnum/tden   NB rho - rho' is 1-index_r

scalar mu= (1-`=index_r')* tnum_[1]/tden_[1]
drop *num* *den*
dis  %9.3f scalar(mu)   //mu as garanteed rr

// Identify winners under de-annuitization

tempvar n
tempvar S
tempvar I
gen x_=0
foreach j in F M {
forvalues i = 1(1)100 {
	   gen `S'=(age> `ra')   		//dummy S
	    gen `n' =C`j'`i' * pr`j'`i'  	//number of individuals in cat.  whose longevity is at least [age]
		*We would write gen `I'=(mu + rho' * `S'[age-`ra']/ra >= rho* `S'*b[age-`ra']/ra) .
		*equivalently as rho-rho'= (1-index_r)
        gen `I'=(scalar(mu) > (1-scalar(index_r))*`S'*[age-`ra']/`ra') 
		replace x_= x_ + `I'* `n' 
		drop  `S'  `n' `I'
}
}


list age x_ 
 egen float N_I= total(x_)   //sum over all [age]
 drop x_

scalar share_=N_I[1]/N[1]
drop N_I
dis %9.3f scalar(share_) //share of people who win (ref. retirement age 65)

//export results
frame post res (`ra') (share_) 
}
}


frame res: save results_bis_ra.dta, replace
use results_bis_ra, clear

use results_bis_ra, clear
rename share share_bis
des
merge 1:1 rage using results_bev_ra
rename share share_bev


graph twoway (line share_bis rage , msymbol(x) msize(small) mcol(blue) ) ///
           (line share_bev rage , msymbol(x) lpatter(dash) msize(small) mcol(red)) , ///
		   	 legend (label(1 "Bismark") label(2 "Beveridge") region(color(none))) ///
		 ytitle("Share of winners", size(small)) ///
		 xlabel(50(5)85) ///
     xtitle("Ref. retirement age", size(small)) ///
	 xline(65,  lwidth(vthin) lcol(green) ) 
graph export "C:\Users\adminv\OneDrive - UCL\Deannuitization\fig3_.eps", replace

/////////////////////////////////////////////////////////////////////
///Last extension - assuming only BETWEEN SES group differences matter
////////////////////////////////////////////
 

////   Bismark //////////////////
scalar drop _all

use data1_, clear
scalar alpha= 65/l_av
dis scalar(alpha)

scalar alph=(1-scalar(alpha))/scalar(alpha)
dis scalar(alph)

scalar obj = "BIS"


tempvar per
foreach j in F  M {  //gender
forvalues i = 1(1)100 { 
	gen `per'= `i'
	gen ra`j'`i'=.
	replace ra`j'`i'=scalar(alpha)*l_Q1 if `per'<=50
	replace  ra`j'`i'= scalar(alpha)*l_Q2 if `per'>50
	drop `per'
	
				}  
		  } 
		  
dis scalar(alpha)*l_av
dis raF1 
dis raM1
dis raF49
dis raM49
dis raF99
dis raM99
		  

keep if  _n ==1 //we only need the first line 
drop pr* age //we no longer need the probability by [age] within SES cat;

list  l_av*
capture frame drop res
capture frame create res Denominator ///
                      Numerator_2 ///
					  index_r ///
					  index_r_adj ///
					  alpha ///
					  str5 obj ///
					  str5 cat_sex_per ///
					  rage_sex_perc ///
					  
capture frame drop temp1
frame copy default temp1

frame temp1 {
	
	*** Denominator (uniform retirement age) ***
***********************************************
tempvar ra
tempvar n
tempvar S
gen long gap_ra=0
foreach j in F  M {  //gender
forvalues i = 1(1)100 { 
	    gen `ra'= scalar(alpha)* l_av //retirement age as alpha*average longevity ==> we set it at 65
	     gen `S'= (l_av`j'`i'>`ra')  		//dummy S
	     gen `n' =C`j'`i'     	//# ind. in cat.  whose expected long. is [l_av`j'`i']
		 quiet: replace gap_ra=gap_ra +   abs(scalar(alph) - `S'*[l_av`j'`i' -`ra']/`ra')* `n'  
		 drop  `ra' `n' `S'
				}  
		  }  //At this stage one has summed gap for all cat. by retirement age [ra] and age of death [age/row]

 list gap_ra 

scalar Denominator = gap_ra[1] //retain only first line

drop gap*

dis %20.0fc  scalar(Denominator) 

***Numerator (retirement differentiated by GENDER x INCOME PERCENTILE)*****
*******************************************************************************

	
//compute budget_adjusted b_a/b = 
            // [wyir(ra_j)/wyir(ra)]  *  with [wyir] =>wage weighted years spent in retirement (benefit ratio)
           //[wyie(ra_j)/wyie(ra)]  *  with [wyir] =>wage weighted years spent in employment  (contribution ratio)
 *OPTIONAL !!!!            
tempvar ra	
tempvar n
tempvar S
tempvar S_j
gen float num_B=0
gen float den_B=0
gen float num_C=0
gen float den_C=0
foreach j in F M {
forvalues i = 1(1)100 {
	   gen `ra'= scalar(alpha)* l_av  //retirement age as alpha*average longevity (ie 65)
	  gen `S'=(l_av`j'`i'> `ra')   		//dummy S
	   gen `S_j'=(l_av`j'`i'> ra`j'`i')   		//dummy S_j
      gen `n' =(C`j'`i' /** pr`j'`i' */) *  w`j'`i'	 //# of ind. in cat.  whose long. is >=[l_av`j'`i']
        replace num_B= num_B + `S'*[l_av`j'`i'-`ra']* `n' 
		  replace den_B= den_B + `S_j'*[l_av`j'`i'-ra`j'`i']* `n' 
		    replace num_C= num_C + (1-`S_j') *l_av`j'`i'* `n' + `S_j'*ra`j'`i'* `n' 
		    replace den_C= den_C + (1-`S') *l_av`j'`i' * `n'  + `S'*`ra'* `n' 
		  
	drop `ra'   `S' `S_j' `n' 
}
}


*list age num_B den_B
 egen float tnum_B= total(num_B)   //sum over all [age]  	wyir(ra)
  egen float tden_B= total(den_B)   //sum over all [age]   wyir(ra_j)
   egen float tnum_C= total(num_C)   //sum over all [age]  	wyie(ra_j)
  egen float tden_C= total(den_C)   //sum over all [age]   wyie(ra)
* list age tnum_B tden_B
drop num_* den_*
scalar b_ =tnum_B[1]/tden_B[1]
scalar c_=tnum_C[1]/tden_C[1]
scalar b_adj=scalar(b_)* scalar(c_)
dis %9.3f scalar(b_) _col(5)  %9.3f scalar(c_)   _col(10) %9.3f scalar(b_adj)


//numerator per se
tempvar n
tempvar S_j
foreach j in F M {
forvalues i = 1(1)100 {
	 *dis l_av`j'`i'
	   gen `S_j'=(l_av`j'`i'> ra`j'`i')   		//dummy S_j
	    gen `n' =C`j'`i'	//number of individuals in cat.  whose longevity is at least [l_av`j'`i']
        gen long gap_`j'`i'= abs(scalar(alph)- `S_j'*[l_av`j'`i' -ra`j'`i']/ra`j'`i')* `n' 
		drop   `n' `S_j'
}
}

*list  age x_M1

drop C* /*pr* */


*list  age x_M1 gap_M1
 *des gap_*
egen Numerator_= rowtotal(gap_*) //sum over all cat.
drop gap_*

//Index per se
scalar Numerator_2 = Numerator_[1]
dis %20.0fc scalar(Numerator_2)
dis %20.0fc scalar(Denominator)
scalar Index_r =  scalar(Numerator_2)/scalar(Denominator)  //without
scalar Index_r_adj =  scalar(b_adj)*scalar(Numerator_2)/scalar(Denominator)  //with budget adjustment

dis %9.3f `=Index_r' // !!!! Equity reduction ==> theta_prime/theta  in de-annuitization
dis %9.3f `=Index_r_adj' // !!!! Equity reduction ==> theta_prime/theta  in de-annuitization

*** Export results and parameters **

foreach j in F  M {
forvalues i = 1(1)100 {
 scalar cat_`j'_`ì'="`j'_`i'"	

//Export results
frame post res (Denominator) (Numerator_2) (Index_r)  (Index_r_adj) (alpha) (obj) (cat_`j'_`ì') (ra`j'`i')

}
}
}

frame res: save results_bis_ses.dta, replace
use results_bis_ses, clear
br

//////////////////////////////////
////   Beveridge //////////////////
//////////////////////////////

use data1_, clear

scalar drop _all
scalar alpha= 65/l_av
dis scalar(alpha)
scalar alph=(1-scalar(alpha))/scalar(alpha)
dis scalar(alph)

scalar obj = "BEV"
scalar omega= (1-`=alpha')*l_av
dis scalar(omega)

tempvar per
foreach j in F  M {  //gender
forvalues i = 1(1)100 { 
	gen `per'= `i'
	gen ra`j'`i'=.
	replace ra`j'`i'=l_Q1 - scalar(omega) if `per'<=50
	replace  ra`j'`i'= l_Q2 -scalar(omega) if `per'>50
	drop `per'
	
				}  
		  } 
		  
dis scalar(alpha)*l_av
dis raF1 
dis raM99


keep if  _n ==1 //we only need the first line 
drop pr* age //we no longer need the probability by [age] within SES cat;


capture frame drop res
capture frame create res Denominator ///
                      Numerator_2 ///
					  index_r ///
					  index_r_adj ///
					  omega ///
					  str5 obj ///
					  str5 cat_sex_per ///
					  rage_sex_perc ///
					 		
					  
capture frame drop temp1
frame copy default temp1

frame temp1 {
	
********Denominator (unif. retirement)*******************
tempvar ra
tempvar n
tempvar S

gen long gap_ra=0

foreach j in F  M {  //gender
forvalues i = 1(1)100 { 
	    gen `ra'= scalar(alpha)* l_av //retirement age as alpha*average longevity ==> we set it at 65
	     gen `S'= (l_av`j'`i' >`ra')  		//dummy S
	     gen `n' =C`j'`i'  	//number of individuals in cat.  whose longevity is at least [age]
		 quiet: replace gap_ra=gap_ra +   abs(scalar(omega) - `S'*[ l_av`j'`i'  -`ra'])* `n'  
		 drop  `ra' `n' `S'
				}  
		  }  //At this stage one has summed gap for all cat. by retirement age [ra] and age of death [age/row]

 list gap_ra  

scalar Denominator = gap_ra[1] //retain only first line

dis %20.0fc scalar(Denominator) 

drop gap*


***Numerator (retirement diff. by GENDER x INCOME PERCENTILE)*****
********************************************************************************

//compute budget_adjusted b_a/b =(YIR(ra) /YIR(ra_j)) with YIR=total years spent in retirement
*OPTIONAL !!!!
tempvar ra	
tempvar n
tempvar S
tempvar S_j
gen float yir_ra=0
gen float yir_ra_j=0
foreach j in F M {
forvalues i = 1(1)100 {
	   gen `ra'= scalar(alpha)* l_av  //retirement age as alpha*average longevity (ie 65)
	  gen `S'=( l_av`j'`i'> `ra')   		//dummy S
	   gen `S_j'=(l_av`j'`i'> ra`j'`i' )   		//dummy S_j
	    gen `n' =C`j'`i' 	//number of individuals in cat.  whose longevity is at least [age]
        replace yir_ra= yir_ra + `S'*[l_av`j'`i'-`ra']* `n' 
		  replace yir_ra_j= yir_ra_j + `S_j'*[l_av`j'`i' - ra`j'`i']* `n' 
	drop `ra'   `S' `S_j' `n' 
}
}
scalar b_adj= (yir_ra[1]/yir_ra_j[1])
drop yir*
dis %9.3f scalar(b_adj)


//Numerator per se

	dis `=alpha'
	dis `=alph'
	dis `=Denominator '

tempvar n
tempvar S_j
foreach j in F M {
forvalues i = 1(1)100 {
	   gen `S_j'=(l_av`j'`i'> ra`j'`i')   		//dummy S
	    gen `n' =C`j'`i'  	//number of individuals in cat.  whose longevity is at least [age]
        gen long gap_`j'`i'= abs(scalar(omega) - `S_j'*[l_av`j'`i'-ra`j'`i'])* `n' 
		drop  `n' `S_j'
}
}

*nois list  age x_M1

drop C* 

 nois des gap_*
egen Numerator_= rowtotal(gap_*) //sum over all cat.
drop gap_*

//Indice per se 
scalar Numerator_2 = Numerator_[1]
dis  %20.0fc  scalar(Numerator_2)
dis %20.0fc  scalar(Denominator)
scalar index_r=  (scalar(Numerator_2)/scalar(Denominator)) //no adjusted
scalar index_r_adj= scalar(b_adj)* (scalar(Numerator_2)/scalar(Denominator)) //adjusted
dis %9.3f scalar(index_r) // !!!! Equity reduction ==> b_prime/b  in de-annuitization
dis %9.3f scalar(index_r_adj) // !!!! Equity reduction ==> b_prime/b  in de-annuitization

*** To know the retirement age by sex and by percentile **

foreach j in F  M {
forvalues i = 1(1)100 {
	
 scalar cat_`j'_`ì'="`j'_`i'"	
gen rage`j'`i'= ra`j'`i'

//Export results
frame post res (Denominator) (Numerator_2) (index_r) (index_r_adj)  (omega) (obj) (cat_`j'_`ì') (rage`j'`i')

}
}

}

frame res: save results_bev_ses.dta, replace
use results_bev_ses, clear
br

use results_bis_ses, clear

keep cat_sex_per index_r
keep if _n==1
rename index_r index_r_bis
save x1, replace
br

use results_bev_ses, clear
keep cat_sex_per index_r
keep if _n==1
rename index_r index_r_bev
save x2, replace
br

use x1, clear
merge 1:1 cat_sex_per using x2

estpost  tabstat index_r*, by(cat_sex_per) stat(mean) notot
esttab . using "C:\Users\adminv\OneDrive - UCL\Deannuitization\tab2.tex",  ///
  cells("index_r_bis(fmt(%9.3fc)) index_r_bev") replace  ///
 noobs nomtitle nonumber compress nogaps     ///
 addnote("Source: Chelly, our calculations")

//////////////////////////////////////////
/////// Other things (to be considered)
/////////////////////////////////////////////////
* From a normative point of view, so far we have considered that all realized longevity differences matter (ex-post stand point à-la-Ponthière- Fleurbaey)
* We will consider here that only BETWEEN SES (gender X Income percential) gaps matter (the within is risk or behavioral)  ==> the gaps to consider are now 
		// Bismark with no diff. :  gap = rho*[alph - (lj - ra)/ra]
		// Beveridge with no diff. :  gap = b*[omega - (lj - ra)]
		// where lj is the average longevity for NARROW SES category j=1...K 
		
*Ideally, the planner would want to go for full differentiation eg. raj=alpha*lj  where j=1.......K
*Imagine, however that she can (in practice) only go for rad=alpha*ld, d=1..K_) with K_r< K  e.g. 2 retirement ages instead of 200 ==>
       // Index reduction will look like  
	   // Biskmark : Index_r= f([alph - (ld - ra)/ra])/ f([alph - (lj - ra)/ra])
	    // Beverigde  : Index_r= f([omega - (ld - ra)])/ f([omega - (lj - ra)])
	   	// where ld is the average longevity for BROAD SES category d=1...K_<K
		
*The will invariably lead to limited gains in terms of our inequality index ==> index_r<1
*And again, we should have that the same reduction can be achieved via (index_r=rho'/rho )
*With less administrative costs, legal issues and risk of moral hazard... but probably larger degree of deannuitization that what we have estimated above  (==> lower capacity to ensure the longevity risk)
