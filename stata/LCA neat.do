
// Some more useful refs
// https://www.stata.com/features/overview/latent-class-analysis/
// https://www.ucl.ac.uk/population-health-sciences/sites/population_health_sciences/files/lca.pdf


set seed 141088

cd "/Users/clairecalderwood/Library/CloudStorage/OneDrive-LondonSchoolofHygieneandTropicalMedicine/Projects/FracturesE3/5_Data analysis/"

putexcel set "fe3-multimorbidity/stata/LCA.xlsx", replace
putexcel set "fe3-multimorbidity/stata/LCA_assignment.xlsx", replace


// Import and create additional variables used
use "fe3-multimorbidity/stata/df_forlca.dta", replace
gen que_dep_szp = que_dep
replace que_dep_szp = 1 if que_szp == 1
tab que_dep_szp site
gen que_bow_grd = que_bow
replace que_bow_grd = 1 if que_grd == 1
gen que_heart_cho = que_heart
replace que_heart_cho = 1 if que_cho == 1

// Variables included in definition (all)

global variables = "que_epi que_kid que_tb hiv_pos_comb que_asth que_heart que_cho que_thy que_can que_stk que_dem que_rhe que_ost que_bow que_dep que_szp que_grd que_anm que_nrp que_park que_gout ce_diab_dx ce_hbp_dx que_all"

global site = "1 2 3 4 5"

// Variables selected for each site in previous explorative script
// Based on number in each cell (see descriptive table)
// Tetrachoric correlation for each site using all vars can be found in R scripts

global variables_zim10 = "que_epi que_tb hiv_pos_comb que_asth que_heart_cho que_can que_stk que_rhe que_ost que_dep_szp ce_diab_dx ce_hbp_dx que_bow_grd"

global variables_gamr10 = "que_dep_szp ce_diab_dx ce_hbp_dx"

global variables_gamu10 = "que_tb que_asth que_heart que_cho que_stk que_dem que_rhe que_ost que_bow_grd que_dep_szp ce_diab_dx ce_hbp_dx"

global variables_sau10 = "que_epi que_kid que_tb hiv_pos_comb que_asth que_heart que_cho que_can que_stk  que_rhe que_ost que_bow_grd que_dep_szp ce_diab_dx ce_hbp_dx que_dem que_gout"

global variables_sar10 = "hiv_pos_comb que_heart que_cho que_stk que_rhe que_ost que_dep_szp que_bow_grd ce_diab_dx ce_hbp_dx que_dem que_tb que_asth que_epi"

// I always constrain one class to have high probability of HTN
constraint 1 _b[ce_hbp_dx:1.C] = 15

// All done excluding people with 0 conditions - add them back in later
keep if multim_n >0

// Clear any old estimates to avoid errors
estimates clear

// Set starting row for relative putexcel
loc row_s = 1
loc row_e = 1



//**** Gambia rural ****//

// Model stats sheet

preserve
keep if site == 3
gsem ($variables_gamr10 <-, logit) , lclass(C 1)  
est sto m1
gsem ($variables_gamr10 <-, logit) , lclass(C 2) startvalue(randomid, draws(50) seed(123321)) em(iter(5)) iterate(50) constraint(1)
est sto m2

putexcel set "fe3-multimorbidity/stata/LCA.xlsx", sheet("Model fit statistics") modify

estimates stats m1 m2

return list
matrix A = r(S)
matrix list A

putexcel A`row_s' = "Gambia rural"

putexcel B`row_s' = matrix(A), names nformat(number_d2)
putexcel I`row_s' = "SSBIC"
loc row_s = `row_s' + 1

est restore m1

scalar SSBIC_1 = -2 * e(ll) + e(rank) * ln((e(N)+2) / 24)
di SSBIC_1
putexcel I`row_s' = SSBIC_1
loc row_s = `row_s' + 1

estimates restore m2

scalar SSBIC_2 = -2 * e(ll) + e(rank) * ln((e(N)+2) / 24)
di SSBIC_2
putexcel I`row_s' = SSBIC_2
loc row_s = `row_s' + 2

// Marginal probabilities and means sheet

estimates restore m2

putexcel set "fe3-multimorbidity/stata/LCA.xlsx", sheet("LCMP") modify

estimates restore m2
estat lcprob
matrix A = r(table)
matrix At = A'
putexcel A`row_e' = "Gambia rural"
putexcel B`row_e' = matrix(At), names nformat(number_d2)
loc row_e = `row e' + 4

putexcel set "fe3-multimorbidity/stata/LCA.xlsx", sheet("Gambia rural LCMM") modify

estat lcmean
matrix A = r(table)'
putexcel A1 = "Gambia rural"
putexcel B1 = matrix(A), names nformat(number_d2)

// Calculate class assignment probabilities and save to merge back into R

	predict class4allpost*, classposteriorpr
	egen class4allmax = rmax(class4allpost*)

export excel using "fe3-multimorbidity/stata/LCA_assignment.xlsx", sheet("Gambia rural",replace) firstrow(variables) 

restore
estimates clear



//**** Gambia urban ****//

// Model stats sheet

preserve
keep if site == 2
gsem ($variables_gamu10 <-, logit) , lclass(C 1)  
est sto m1
gsem ($variables_gamu10 <-, logit) , lclass(C 2) startvalue(randomid, draws(50) seed(123321)) em(iter(5)) iterate(50) constraint(1)
est sto m2

putexcel set "fe3-multimorbidity/stata/LCA.xlsx", sheet("Model fit statistics") modify

estimates stats m1 m2

return list
matrix A = r(S)
matrix list A

putexcel A`row_s' = "Gambia urban"

putexcel B`row_s' = matrix(A), names nformat(number_d2)
putexcel I`row_s' = "SSBIC"
loc row_s = `row_s' + 1

est restore m1

scalar SSBIC_1 = -2 * e(ll) + e(rank) * ln((e(N)+2) / 24)
di SSBIC_1
putexcel I`row_s' = SSBIC_1
loc row_s = `row_s' + 1

estimates restore m2

scalar SSBIC_2 = -2 * e(ll) + e(rank) * ln((e(N)+2) / 24)
di SSBIC_2
putexcel I`row_s' = SSBIC_2
loc row_s = `row_s' + 2

// Marginal probabilities and means sheet

putexcel set "fe3-multimorbidity/stata/LCA.xlsx", sheet("LCMP") modify

estimates restore m2
estat lcprob
matrix A = r(table)
matrix At = A'
putexcel A`row_e' = "Gambia urban"
putexcel B`row_e' = matrix(At), names nformat(number_d2)
loc row_e = `row e' + 8

putexcel set "fe3-multimorbidity/stata/LCA.xlsx", sheet("Gambia urban LCMM") modify

estat lcmean
matrix A = r(table)'
putexcel A1 = "Gambia urban"
putexcel B1 = matrix(A), names nformat(number_d2)

// Calculate class assignment probabilities and save to merge back into R

	predict class4allpost*, classposteriorpr
	egen class4allmax = rmax(class4allpost*)

export excel using "fe3-multimorbidity/stata/LCA_assignment.xlsx", sheet("Gambia urban",replace) firstrow(variables) 

restore
estimates clear



//**** Zimbabwe ****//

// Model stats sheet

preserve
keep if site == 1
gsem ($variables_zim10 <-, logit) , lclass(C 1)  
est sto m1
gsem ($variables_zim10 <-, logit) , lclass(C 2) startvalue(randomid, draws(50) seed(123321)) em(iter(5)) iterate(50) constraint(1)
est sto m2

putexcel set "fe3-multimorbidity/stata/LCA.xlsx", sheet("Model fit statistics") modify

estimates stats m1 m2

return list
matrix A = r(S)
matrix list A

putexcel A`row_s' = "Zimbabwe urban"

putexcel B`row_s' = matrix(A), names nformat(number_d2)
putexcel I`row_s' = "SSBIC"
loc row_s = `row_s' + 1

est restore m1

scalar SSBIC_1 = -2 * e(ll) + e(rank) * ln((e(N)+2) / 24)
di SSBIC_1
putexcel I`row_s' = SSBIC_1
loc row_s = `row_s' + 1

estimates restore m2

scalar SSBIC_2 = -2 * e(ll) + e(rank) * ln((e(N)+2) / 24)
di SSBIC_2
putexcel I`row_s' = SSBIC_2
loc row_s = `row_s' + 2

// Marginal probabilities and means sheet

estimates restore m2

putexcel set "fe3-multimorbidity/stata/LCA.xlsx", sheet("LCMP") modify

estimates restore m2
estat lcprob
matrix A = r(table)
matrix At = A'
putexcel A`row_e' = "Zimbabwe urban"
putexcel B`row_e' = matrix(At), names nformat(number_d2)
loc row_e = `row e' + 12

putexcel set "fe3-multimorbidity/stata/LCA.xlsx", sheet("Zimbabwe urban LCMM") modify

estat lcmean
matrix A = r(table)'
putexcel A1 = "Zimbabwe urban"
putexcel B1 = matrix(A), names nformat(number_d2)

// Calculate class assignment probabilities and save to merge back into R

	predict class4allpost*, classposteriorpr
	egen class4allmax = rmax(class4allpost*)

export excel using "fe3-multimorbidity/stata/LCA_assignment.xlsx", sheet("Zimbabwe urban", replace) firstrow(variables) 

restore
estimates clear



//**** South Africa rural ****//

// Model stats sheet

preserve
keep if site == 5
gsem ($variables_sar10 <-, logit) , lclass(C 1)  
est sto m1
gsem ($variables_sar10 <-, logit) , lclass(C 2) startvalue(randomid, draws(50) seed(123321)) em(iter(5)) iterate(50) constraint(1)
est sto m2

putexcel set "fe3-multimorbidity/stata/LCA.xlsx", sheet("Model fit statistics") modify

estimates stats m1 m2

return list
matrix A = r(S)
matrix list A

putexcel A`row_s' = "South Africa rural"

putexcel B`row_s' = matrix(A), names nformat(number_d2)
putexcel I`row_s' = "SSBIC"
loc row_s = `row_s' + 1

est restore m1

scalar SSBIC_1 = -2 * e(ll) + e(rank) * ln((e(N)+2) / 24)
di SSBIC_1
putexcel I`row_s' = SSBIC_1
loc row_s = `row_s' + 1

estimates restore m2

scalar SSBIC_2 = -2 * e(ll) + e(rank) * ln((e(N)+2) / 24)
di SSBIC_2
putexcel I`row_s' = SSBIC_2
loc row_s = `row_s' + 2

// Marginal probabilities and means sheet

estimates restore m2

putexcel set "fe3-multimorbidity/stata/LCA.xlsx", sheet("LCMP") modify

estimates restore m2
estat lcprob
matrix A = r(table)
matrix At = A'
putexcel A`row_e' = "South Africa rural"
putexcel B`row_e' = matrix(At), names nformat(number_d2)
loc row_e = `row e' + 16

putexcel set "fe3-multimorbidity/stata/LCA.xlsx", sheet("South Africa rural LCMM") modify

estat lcmean
matrix A = r(table)'
putexcel A1 = "South Africa rural"
putexcel B1 = matrix(A), names nformat(number_d2)

// Calculate class assignment probabilities and save to merge back into R

	predict class4allpost*, classposteriorpr
	egen class4allmax = rmax(class4allpost*)

export excel using "fe3-multimorbidity/stata/LCA_assignment.xlsx", sheet("South Africa rural", replace) firstrow(variables)

restore
estimates clear


//**** South Africa urban ****//

// Model stats sheet

preserve
keep if site == 4
gsem ($variables_sau10 <-, logit) , lclass(C 1)  
est sto m1
gsem ($variables_sau10 <-, logit) , lclass(C 2) startvalue(randomid, draws(50) seed(123321)) em(iter(5)) iterate(50) constraint(1)
est sto m2

putexcel set "fe3-multimorbidity/stata/LCA.xlsx", sheet("Model fit statistics") modify

estimates stats m1 m2

return list
matrix A = r(S)
matrix list A

putexcel A`row_s' = "South Africa urban"

putexcel B`row_s' = matrix(A), names nformat(number_d2)
putexcel I`row_s' = "SSBIC"
loc row_s = `row_s' + 1

est restore m1

scalar SSBIC_1 = -2 * e(ll) + e(rank) * ln((e(N)+2) / 24)
di SSBIC_1
putexcel I`row_s' = SSBIC_1
loc row_s = `row_s' + 1

estimates restore m2

scalar SSBIC_2 = -2 * e(ll) + e(rank) * ln((e(N)+2) / 24)
di SSBIC_2
putexcel I`row_s' = SSBIC_2
loc row_s = `row_s' + 2

// Marginal probabilities and means sheet

estimates restore m2

putexcel set "fe3-multimorbidity/stata/LCA.xlsx", sheet("LCMP") modify

estimates restore m2
estat lcprob
matrix A = r(table)
matrix At = A'
putexcel A`row_e' = "South Africa urban"
putexcel B`row_e' = matrix(At), names nformat(number_d2)
loc row_e = `row e' +  20

putexcel set "fe3-multimorbidity/stata/LCA.xlsx", sheet("South Africa urban LCMM") modify

estat lcmean
matrix A = r(table)'
putexcel A1 = "South Africa urban"
putexcel B1 = matrix(A), names nformat(number_d2)

// Calculate class assignment probabilities and save to merge back into R

	predict class4allpost*, classposteriorpr
	egen class4allmax = rmax(class4allpost*)

export excel using "fe3-multimorbidity/stata/LCA_assignment.xlsx", sheet("South Africa urban", replace) firstrow(variables)

restore
estimates clear

