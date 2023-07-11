*09mar2021
*AERA/ICPS PEERS SEDA Webinar
*Authors: Ben Shear, Erin Fahle, sean reardon, Andrew Ho
*https://www.icpsr.umich.edu/web/pages/peersdatahub/
*Sample code to accompany webinar
*Code written in Stata Version 16.1


* !!!!! *
* Before proceeding, please complete the data use agreement here:
* https://edopportunity.org/get-the-data/
* !!!!! *


clear all


* ---------------------------------------------------------------------------- *		
* Pooled data file example

* Load achievement data (district, CS, pooled)

use "https://stacks.stanford.edu/file/druid:db586ns4974/seda_geodist_pool_cs_4.0.dta" , clear
tempfile ach
save `ach'

* Load covariate data (district, pooled)

use "https://stacks.stanford.edu/file/druid:db586ns4974/seda_cov_geodist_pool_4.0.dta" , clear

* Merge files together. Keep matched records.

merge 1:m sedalea fips using "`ach'" 

keep if _merge == 3
drop _merge

keep if subgroup == "all"
drop if cs_mn_avg_eb==.
drop if cs_mn_grd_eb==.
drop if sesavgall==.

di _N

* Summary statistics and correlations

summarize cs_mn_avg_eb cs_mn_grd_eb sesavgall

cor cs_mn_avg_eb cs_mn_grd_eb sesavgall


* How much variation is there in average achievement?
histogram cs_mn_avg_eb , freq name(hist_avg, replace)


* How much variation is there in learning rates?
histogram cs_mn_grd_eb , freq name(hist_grd, replace)


* How much variation is there in SES?
histogram sesavgall , freq name(hist_ses, replace)


* What is the association between SES and average achievement?
twoway 	(scatter cs_mn_avg_eb sesavgall [aw=totenrl], ///
			mlc(gs13) mfc(gs10%20) mlwidth(vvthin)) ///
		(lowess cs_mn_avg_eb sesavgall) , ///
		legend(off) ///
		ylab(,angle(0)) ///
		xtitle("Average District Socioeconomic Status") ///
		ytitle("Average Test Scores (Grade 5.5, CS Scale)") ///
		name(ses_v_avg, replace)

		
* What is the association between SES and learning rates?
twoway 	(scatter cs_mn_grd_eb sesavgall [aw=totenrl], ///
			mlc(gs13) mfc(gs10%20) mlwidth(vvthin)) ///
		(lowess cs_mn_grd_eb sesavgall) , ///
		legend(off) ///
		ylab(,angle(0)) ///
		xtitle("Average District Socioeconomic Status") ///
		ytitle("Average Test Scores (Grade 5.5, CS Scale)") ///
		name(ses_v_grd, replace)

		
* Compare gradient in CA and KY
global yvar cs_mn_avg_eb
glob state1 = "CA"	
glob state2 = "KY"
glob statename1 = "California"
glob statename2 = "Kentucky"
glob msize "large"

* These extra variables ensure the relative point sizes remain constant
g ses1 = sesavgall
replace ses1=. if !inlist(stateabb,"$state1")
g ses2 = sesavgall
replace ses2=. if !inlist(stateabb,"$state2")

twoway	(scatter $yvar sesavgall [aw=totenrl] , mlc(gs13) mfc(gs10%20) ms(o) mlw(vthin) msize(${msize}))  ///
		(scatter $yvar ses1 [aw=totenrl], mfc(eltblue%50) ms(o) mlc(gs3) mlw(vthin) msize(${msize})) ///
		(lfit $yvar sesavgall if stateabb == "$state1", lc(gs5) lp(dash) lw(medium)) ///
		(scatter $yvar ses2 [aw=totenrl] , mfc(maroon%50) ms(o) mlc(gs8) mlw(vthin) msize(${msize})) ///
		(lfit $yvar sesavgall if stateabb == "$state2", lc(gs5) lp(longdash_dot) lw(medium)), ///
		title("District Socioeconomic Status and District Test Scores", size(4) pos(12)) ///
		xtitle("District Socioeconomic Status", size(4)) ///
		ytitle("Average Test Scores (Grade 5.5, CS Scale)", size(4)) ///
		legend(ring(0) pos(4) ///
			order(4 "${statename2}" 2 "${statename1}" ///
			5 "Fitted Line, $state2" 3 "Fitted Line, $state1") symxsize(10)) ///
		ylin(0, lp(dash) lw(thin) lc(gs8)) ///
		xsc(ran(-4 3)) ///
		ysc(ran(-1.5 1.25)) ///
		xlab(-4(1)3, labsize(small)) ///
		ylab(-1.5(.5)1, labsize(small) format(%2.1f)) ///
		xsize(13.33) ysize(7.5) ///
		name(mn_${state1}_${state2}a, replace)


		
* ---------------------------------------------------------------------------- *		
* Long form data file example

* Warning: the long files are much larger and take longer to load.
use "https://stacks.stanford.edu/file/druid:db586ns4974/seda_geodist_long_cs_4.0.dta" , clear

preserve
keep if sedalea==622710 // LAUSD
table grade year subject , c(mean cs_mn_all) f(%9.2f)
restore



