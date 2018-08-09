clear
cd /Users/ywang19/Desktop/RS
*********************************************
*** Step1: load and save CRSP
*********************************************
clear
insheet using CRSP.csv // 
save CRSP, replace
drop if naics == .
/* explore difference in siccd and naics
sort siccd naics
quietly by siccd naics: gen dup1 = cond(_N==1,0,_n)
sort dup1 naics 
by naics siccd, sort: gen nvals11 = _n == 1 
count if nvals11
*/
tostring naics, replace 
save CRSP, replace

*********************************************
*** Step2: merge with industry code data from Nunn
*********************************************
clear
use contract_intensity_IO_1997 //381obs
rename industry_code naics
merge 1:m naics using CRSP
save merged, replace
keep if _merge == 3 //_merge=1 not traded; _merge=2 not manufacture industry
drop _merge
drop shrcd
* convert date
tostring date, replace
gen newdate = date(date,"YMD")
format newdate %td
gen newdate2 = mofd(newdate)
format newdate2 %tm
drop date newdate 
rename newdate2 date
drop if ret == "" | ret =="B" | ret=="C"
destring ret, gen(Ret)
drop ret
save merged, replace


*********************************************
*** Step3: load FF3 factors t
*********************************************
clear
local first = 5 //indicate first data line to be read
local last = 1104 - `first' +1 //# obs to be read
set more off
infix `first' firstline ym 1-6 mktrf 7-14 smb 15-22 hml 23-30 rf 31-38 in 1 / `last' using F-F_Research_Data_Factors.txt, clear
replace ym = ym(trunc(.01*ym), ym-100*trunc(.01*ym)) // change format
format %tm ym
keep if ym >=ym(2004,1)
rename ym date
save FFtemp, replace //save FF-factors to a temp stat file

merge 1:m date using merged
keep if _merge == 3
drop _merge
gen re = Ret*100 - rf
save Firm, replace

*********************************************
*** Step3.1: sort into monthly naics portfolio returns
*********************************************
clear
use Firm
by date, sort: egen quintile = xtile(frac_lib_not_h~g), nq(5)
* EW
by date quintile, sort: egen re_EW = mean(re) // 1 is low, 5 is high
* VW
gen mktcap = prc*shrout
by date quintile, sort: egen den = total(mktcap)
gen re_weighted = re * mktcap / den
by date quintile, sort: egen re_VW = total(re_weighted) // 1 is low, 5 is high

keep date quintile re_EW re_VW mktrf smb hml rf
duplicates drop
* reshape from long to wide
reshape wide re_EW re_VW, i(date) j(quintile)
gen lmh_EW = re_EW1 - re_EW5
gen lmh_VW = re_VW1 - re_VW5

save FF3_quintile, replace


clear
use Firm
by date, sort: egen quintile = xtile(frac_lib_not_h~g), nq(3)
* EW
by date quintile, sort: egen re_EW = mean(re) // 1 is low, 3 is high
* VW
gen mktcap = prc*shrout
by date quintile, sort: egen den = total(mktcap)
gen re_weighted = re * mktcap / den
by date quintile, sort: egen re_VW = total(re_weighted) // 1 is low, 3 is high

keep date quintile re_EW re_VW mktrf smb hml rf
duplicates drop
* reshape from long to wide
reshape wide re_EW re_VW, i(date) j(quintile)
gen lmh_EW = re_EW1 - re_EW3
gen lmh_VW = re_VW1 - re_VW3

save FF3_tercile, replace

*********************************************
*** Step3.2: * Table1: SortedReturn (quintile sort)
*********************************************
clear
use FF3_quintile
*EW
matrix tb1_EW = J(6,4,0) //collect results t a matrix
matrix colnames tb1_EW = Mean Std SR t-stat
matrix rownames tb1_EW = "Low=1" 2 3 4 "High=5" "Low-High(EW)" 
forvalues i = 1(1)5 {
	quietly summ re_EW`i' 
	matrix tb1_EW[`i',1] = round(r(mean), .001) //round to 3 decimal points
	matrix tb1_EW[`i',2] = round(r(sd), .001) 
	matrix tb1_EW[`i',3] = round(r(mean)/r(sd), .001) 
	matrix tb1_EW[`i',4] = round(r(mean)/r(sd)*sqrt(_N), .001) 
	}
quietly summ lmh_EW 
matrix tb1_EW[6,1] = round(r(mean), .001) //round to 3 decimal points
matrix tb1_EW[6,2] = round(r(sd), .001) 
matrix tb1_EW[6,3] = round(r(mean)/r(sd), .001)
matrix tb1_EW[6,4] = round(r(mean)/r(sd)*sqrt(_N), .001) 
matrix list tb1_EW // print
*VW
matrix tb1_VW = J(6,4,0) //collect results t a matrix
matrix colnames tb1_VW  = Mean Std SR t-stat
matrix rownames tb1_VW  = "Low=1" 2 3 4 "High=5" "Low-High(EW)" 
forvalues i = 1(1)5 {
	quietly summ re_VW`i' 
	matrix tb1_VW [`i',1] = round(r(mean), .001) //round to 3 decimal points
	matrix tb1_VW [`i',2] = round(r(sd), .001) 
	matrix tb1_VW [`i',3] = round(r(mean)/r(sd), .001) 
	matrix tb1_VW [`i',4] = round(r(mean)/r(sd)*sqrt(_N), .001) 
	}
quietly summ lmh_VW 
matrix tb1_VW [6,1] = round(r(mean), .001) //round to 3 decimal points
matrix tb1_VW [6,2] = round(r(sd), .001) 
matrix tb1_VW [6,3] = round(r(mean)/r(sd), .001)
matrix tb1_VW [6,4] = round(r(mean)/r(sd)*sqrt(_N), .001) 
matrix list tb1_VW  // print

*save as LaTex
outtable using tb1_SortedRet, mat(tb1_EW) replace
outtable using tb1_SortedRet, mat(tb1_VW) append
*save as txt
mat2txt, matrix(tb1_EW) saving(tb1_SortedRet) replace
mat2txt, matrix(tb1_VW) saving(tb1_SortedRet) append

*********************************************
*** Step3.3: * Table2: alphas and loadings (Tercile sort)
*********************************************
clear
use FF3_tercile
*EW
matrix tb2_EW = J(5,5,0) //collect results t a matrix
matrix colnames tb2_EW = Return Alphas MKT SMB HML
matrix rownames tb2_EW = Low Medium High "Low-High(EW)" t_stat //t-stat of LMH(EW)
forvalues i = 1/3 {
	quietly summ re_EW`i' 
	matrix tb2_EW[`i',1] = round(r(mean), .001) //round to 3 decimal points
	quietly reg re_EW`i' mktrf smb hml 
	matrix tb2_EW[`i',2] = round(_b[_cons], .001)
	matrix tb2_EW[`i',3] = round(_b[mktrf], .001)
	matrix tb2_EW[`i',4] = round(_b[smb], .001)
	matrix tb2_EW[`i',5] = round(_b[hml], .001)
	}
quietly summ lmh_EW 
matrix tb2_EW[4,1] = round(r(mean), .001) //round to 3 decimal points
matrix tb2_EW[5,1] = round(r(mean)/r(sd)*sqrt(_N), .001) 
quietly reg lmh_EW`i' mktrf smb hml 
matrix tb2_EW[4,2] = round(_b[_cons], .001)
matrix tb2_EW[4,3] = round(_b[mktrf], .001)
matrix tb2_EW[4,4] = round(_b[smb], .001)
matrix tb2_EW[4,5] = round(_b[hml], .001)
matrix tb2_EW[5,2] = round(_b[_cons]/_se[_cons], .001)
matrix tb2_EW[5,3] = round(_b[mktrf]/_se[mktrf], .001)
matrix tb2_EW[5,4] = round(_b[smb]/_se[smb], .001)
matrix tb2_EW[5,5] = round(_b[hml]/_se[hml], .001)
matrix list tb2_EW // print
*VW
matrix tb2_VW = J(5,5,0) //collect results t a matrix
matrix colnames tb2_VW = Return Alphas MKT SMB HML
matrix rownames tb2_VW = Low Medium High "Low-High(VW)" t_stat //t-stat of LMH(EW)
forvalues i = 1/3 {
	quietly summ re_VW`i' 
	matrix tb2_VW[`i',1] = round(r(mean), .001) //round to 3 decimal points
	quietly reg re_VW`i' mktrf smb hml 
	matrix tb2_VW[`i',2] = round(_b[_cons], .001)
	matrix tb2_VW[`i',3] = round(_b[mktrf], .001)
	matrix tb2_VW[`i',4] = round(_b[smb], .001)
	matrix tb2_VW[`i',5] = round(_b[hml], .001)
	}
quietly summ lmh_VW 
matrix tb2_VW[4,1] = round(r(mean), .001) //round to 3 decimal points
matrix tb2_VW[5,1] = round(r(mean)/r(sd)*sqrt(_N), .001) 
quietly reg lmh_VW`i' mktrf smb hml 
matrix tb2_VW[4,2] = round(_b[_cons], .001)
matrix tb2_VW[4,3] = round(_b[mktrf], .001)
matrix tb2_VW[4,4] = round(_b[smb], .001)
matrix tb2_VW[4,5] = round(_b[hml], .001)
matrix tb2_VW[5,2] = round(_b[_cons]/_se[_cons], .001)
matrix tb2_VW[5,3] = round(_b[mktrf]/_se[mktrf], .001)
matrix tb2_VW[5,4] = round(_b[smb]/_se[smb], .001)
matrix tb2_VW[5,5] = round(_b[hml]/_se[hml], .001)
matrix list tb2_VW // print

*save as LaTex
outtable using tb2_FF3, mat(tb2_EW) replace
outtable using tb2_FF3, mat(tb2_VW) append
*save as txt
mat2txt, matrix(tb2_EW) saving(tb2_FF3) replace
mat2txt, matrix(tb2_VW) saving(tb2_FF3) append









*********************************************
*** Step4: panel regression
*********************************************
clear 
use merged
xtset permno date
set more off
xtreg Ret frac_lib_d~f i.date, fe vce(cluster permno)

set more off
xi: regress Ret frac_lib_d~f i.date


set more off
xtreg Ret frac_lib_not_h~g i.date, fe vce(cluster permno)
