# Multimorbidity analysis
# Fractures E3
# Claire Calderwood

# Footnote strings for the compiled documents --------
# Requires: objects created by the analysis scripts, for the counts quoted below
# Creates:  the fn_* strings the two compile scripts consume
# Feeds:    footnotes throughout both documents
#
# Footnotes that count missingness live in the script that builds the table they
# annotate, so that the count and the table cannot drift apart. The strings here
# are the ones that carry no computed value, or that describe a method rather
# than a count.

## Values that are produced outside R --------
# Entropy is computed by stata/lca_entropy.do, which prints to the Stata results
# window and writes nothing to disk, so the values are transcribed here. If the
# latent class models are refitted these must be updated by hand.
lca_entropy = c(`Gambia rural` = 1.00, `Gambia urban` = 0.56,
                `Zimbabwe urban` = 0.70, `South Africa rural` = 0.62,
                `South Africa urban` = 0.74)

# Population pyramids --------
fn_poppyramid = paste0(
  "Prevalence of 0 to 5 or more conditions directly standardised to published ",
  "national population estimates in 5-year age bands. For South Africa and The Gambia, ",
  "the proportion of the national population living in rural areas (",
  ruralperc_sa * 100, "% and ", ruralperc_gam * 100, "% respectively) was used to weight ",
  "the site-specific data, with the urban weight taken as the complement. For Zimbabwe, ",
  "where only an urban site was sampled, the urban data were applied directly to the ",
  "national population estimates.")

# Standardised prevalence --------
fn_std_morbidities = paste0(
  "Prevalence directly standardised to the national population aged 40 years and over, ",
  "by sex and age stratum (", paste(age_strata, collapse = ", "), " years). Each site is ",
  "standardised to its own country's age and sex structure. The overall column combines ",
  "the rural and urban sub-sites within each country by national rural and urban ",
  "population proportions, then takes an unweighted mean of the three countries, so the ",
  "three countries contribute equally regardless of population or sample size. ",
  "Presented as percentage (95% confidence interval). Confidence intervals are Wald ",
  "intervals and are unreliable for low-prevalence conditions.")

fn_std_morbidities_who = paste0(
  "Prevalence directly standardised to the WHO World Standard Population, by sex and ",
  "age stratum. Otherwise as supplementary table 5.")

# Proportion previously diagnosed --------
fn_propdiagnosed = paste0(
  "Denominator for a known condition is all people categorised as having that condition ",
  "in the study. Presented as number (percentage), with header numbers showing the total ",
  "number of participants by category. HIV testing was not undertaken at the two Gambian ",
  "sites, so previously diagnosed HIV is not reported for those sites.")

# EQ-5D --------
fn_eq5dindex = paste0(
  "EQ-5D value index calculated using the Zimbabwe value set for all sites, with a scale ",
  "of 0 to ", eq5d_upper, ", where ", eq5d_upper, " represents the maximum value, that is ",
  "no difficulty reported in any domain. Percentage below ", eq5d_upper, " is the ",
  "percentage of the study population with an index value less than the maximum. ",
  "p values are from Mann-Whitney tests for a difference in medians.")

fn_eq5dvas = paste0(
  "p values are from Mann-Whitney tests for a difference in medians. ",
  "Abbreviations: n/N = number with multimorbidity over total number of participants ",
  "by site; IQR = interquartile range.")

fn_eq5d_all = paste0(
  "Comparison of three modelling approaches for the association between multimorbidity ",
  "and the EQ-5D value index. Tobit models treat the index as right-censored at ",
  eq5d_upper, ". Generalised additive models include all terms parametrically. ",
  "The logistic models dichotomise the EQ-5D value index at the top of the scale, ",
  "so the outcome is an index below ", eq5d_upper, ", that is reporting any ",
  "difficulty rather than none, and the estimates are odds ratios. ",
  "Models adjusting for socioeconomic position included wealth ",
  "index tertile, maximum educational attainment and food insecurity. p values are from ",
  "Wald tests. Abbreviations: GAM = generalised additive model; 95% CI = 95% confidence ",
  "interval; n/N = number with multimorbidity over total number of participants by site.")

fn_eq5d_combo = paste0(
  "EQ-5D value index calculated using the Zimbabwe value set for all sites, with a scale ",
  "of 0 to ", eq5d_upper, ". Coefficients from Tobit regression presented, with Wald ",
  "p values. The adjusted model included age as a linear term, and wealth index tertile, ",
  "maximum educational attainment and food insecurity as indicators of socioeconomic ",
  "position. A separate model for multimorbidity excluding HIV is not shown for The ",
  "Gambia, as no participants there had HIV. Abbreviations: n/N = number of people with ",
  "multimorbidity over total number of participants by site; 95% CI = 95% confidence ",
  "interval; ref = reference category.")

fn_eq5d_class = paste0(
  "Coefficients from Tobit regression of the EQ-5D value index on latent class ",
  "assignment, with Wald p values. The Gambian sites are not shown because the latent ",
  "class structure there was not comparable. Otherwise as table 2.")

# Latent classes --------
fn_lca_fit = paste0(
  "N is the number of people included in the dataset for the latent class models, that ",
  "is the number with at least one condition. Models were fitted in Stata; see the ",
  "repository README for the software handoff. Entropy for the two-class model was ",
  paste0(names(lca_entropy), " ", format(lca_entropy, nsmall = 2), collapse = ", "), ". ",
  "Abbreviations: df = degrees of freedom; AIC = Akaike information criterion; ",
  "BIC = Bayesian information criterion; SSBIC = sample-size adjusted BIC.")

fn_lca_postprob = paste0(
  "Presented as the number observed as having the condition by latent class assignment, ",
  "the proportion with that condition within the class (%), and the observed-to-expected ",
  "ratio comparing people assigned to that class with the study population overall at ",
  "that site. Only conditions with more than 10 individuals at that site are shown.")

fn_lca_oe = paste0(
  "The observed-to-expected ratio is the proportion of people with a condition among ",
  "those assigned to a class, divided by the proportion with that condition in the study ",
  "population at that site. The size of each circle is the number of people with that ",
  "condition in that class. Only conditions with more than 10 individuals at that site ",
  "are shown. The Gambian sites are not shown because the latent class structure there ",
  "was not comparable.")
