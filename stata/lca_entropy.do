
//https://www.tandfonline.com/doi/abs/10.1080/10705511.2013.824781
//https://pmc.ncbi.nlm.nih.gov/articles/PMC7746621/

set seed 141088

cd "/Users/clairecalderwood/Library/CloudStorage/OneDrive-LondonSchoolofHygieneandTropicalMedicine/Projects/FracturesE3/5_Data analysis/"

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

*** install lca_entropy
net install lca_entropy, from("https://tdmize.github.io/data") replace

//**** Gambia rural ****//

// Model stats sheet

preserve
keep if site == 3
gsem ($variables_gamr10 <-, logit) , lclass(C 1)  
est sto m1
gsem ($variables_gamr10 <-, logit) , lclass(C 2) startvalue(randomid, draws(50) seed(123321)) em(iter(5)) iterate(50) constraint(1)
est sto m2

*** calculate LCA entropy program
lca_entropy

restore


//**** Gambia urban ****//

// Model stats sheet

preserve
keep if site == 2
gsem ($variables_gamu10 <-, logit) , lclass(C 1)  
est sto m1
gsem ($variables_gamu10 <-, logit) , lclass(C 2) startvalue(randomid, draws(50) seed(123321)) em(iter(5)) iterate(50) constraint(1)
est sto m2

*** calculate LCA entropy program
lca_entropy

restore

//**** Zimbabwe ****//

// Model stats sheet

preserve
keep if site == 1
gsem ($variables_zim10 <-, logit) , lclass(C 1)  
est sto m1
gsem ($variables_zim10 <-, logit) , lclass(C 2) startvalue(randomid, draws(50) seed(123321)) em(iter(5)) iterate(50) constraint(1)
est sto m2

*** calculate LCA entropy program
lca_entropy

restore


//**** South Africa rural ****//

// Model stats sheet

preserve
keep if site == 5
gsem ($variables_sar10 <-, logit) , lclass(C 1)  
est sto m1
gsem ($variables_sar10 <-, logit) , lclass(C 2) startvalue(randomid, draws(50) seed(123321)) em(iter(5)) iterate(50) constraint(1)
est sto m2

*** calculate LCA entropy program
lca_entropy

restore

//**** South Africa urban ****//

// Model stats sheet

preserve
keep if site == 4
gsem ($variables_sau10 <-, logit) , lclass(C 1)  
est sto m1
gsem ($variables_sau10 <-, logit) , lclass(C 2) startvalue(randomid, draws(50) seed(123321)) em(iter(5)) iterate(50) constraint(1)
est sto m2

*** calculate LCA entropy program
lca_entropy

restore
