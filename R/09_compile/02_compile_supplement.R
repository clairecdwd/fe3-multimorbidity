# Multimorbidity analysis
# Fractures E3
# Claire Calderwood

# Compile the supplementary document --------
# Requires: every table and figure object built by the preceding scripts
# Creates:  output/FE3_MM_supplement.docx
#
# Numbering follows the submitted supplement, which also contains items that are
# not produced by code and are therefore not in this repository: supplementary
# tables 1 to 4 (country indices, condition definitions, latent class model
# specifications, STROBE checklist) and supplementary figure 1 (recruitment flow
# diagram). Because of those gaps the numbers here are written literally rather
# than as Word SEQ fields, so that every item carries the same number it has in
# the manuscript.
#
# Supplementary tables 18 to 21 give the numbers behind the disease combinations
# quoted in the results text and behind supplementary figures 7 and 8.

supp_caption <- function(doc, text) {
  body_add_par(doc, text, style = "Caption")
}

doc_supp <- read_docx(template_docx) |>

  body_add_par("Fractures E3 multimorbidity analysis: supplementary results",
               style = "heading 1") |>

  ## Supplementary table 5 --------
  supp_caption(paste("Supplementary table 5: Prevalence of chronic conditions,",
                     "standardised to national population structure",
                     "(weighted prevalence)")) |>
  body_add_flextable(tbl_std_morbidities) |>
  add_footnote(fn_std_morbidities) |>
  body_add_break() |>

  ## Supplementary table 6 --------
  supp_caption(paste("Supplementary table 6: Prevalence of chronic conditions,",
                     "standardised to WHO standard population")) |>
  body_add_flextable(tbl_std_morbidities_who) |>
  add_footnote(fn_std_morbidities_who) |>
  body_end_block_section(value = block_section(property = ps_portrait)) |>

  ## Supplementary figure 2 --------
  supp_caption(paste("Supplementary figure 2: Intersection plots illustrating common",
                     "disease combinations as observed in the study population,",
                     "by country")) |>
  body_add_img(src = path_upset_crude, width = 10, height = 6) |>
  add_footnote(fn_upset) |>
  body_end_block_section(value = block_section(property = ps_landscape)) |>

  ## Supplementary table 7 --------
  supp_caption(paste("Supplementary table 7: Observed proportion of diabetes, HIV and",
                     "hypertension among study participants that was previously",
                     "diagnosed")) |>
  body_add_flextable(table_propdiagnosed) |>
  add_footnote(fn_propdiagnosed) |>
  body_end_block_section(value = block_section(property = ps_portrait)) |>

  ## Supplementary figure 3 --------
  supp_caption(paste("Supplementary figure 3: Intersection plots illustrating common",
                     "disease combinations, by site")) |>
  body_add_img(src = path_upset_site_std, width = 10, height = 6) |>
  add_footnote(fn_upset_std) |>
  body_end_block_section(value = block_section(property = ps_landscape)) |>

  ## Supplementary figure 4 --------
  supp_caption(paste("Supplementary figure 4: Percentage of people with chronic",
                     "conditions in the study population in 5-year age bands, by site",
                     "and including or excluding HIV from the multimorbidity",
                     "definition")) |>
  body_add_gg(
    ggarrange(obspyramid_sites + plot_standardformat +
                theme(plot.margin = margin(12, 0, 0, 0)),
              obspyramid_sites_exchiv + plot_standardformat +
                theme(plot.margin = margin(12, 0, 0, 0)),
              ncol = 1, labels = c("Including HIV", "Excluding HIV"),
              font.label = list(size = 9, color = "black", face = "bold"),
              legend = "bottom", common.legend = TRUE),
    width = fig_width, height = 10) |>
  add_footnote(paste("Multimorbidity defined as two or more chronic conditions,",
                     "with or without including HIV as a condition.")) |>
  body_add_break() |>

  ## Supplementary figure 5 --------
  supp_caption(paste("Supplementary figure 5: Number of people with chronic conditions,",
                     "standardised to national population in 5-year age bands,",
                     "excluding HIV as a condition")) |>
  body_add_gg(poppyramid_exchiv + plot_standardformat, width = fig_width, height = 4) |>
  add_footnote(fn_poppyramid) |>
  body_add_break() |>

  ## Supplementary figure 6 --------
  supp_caption(paste("Supplementary figure 6: Prevalence of HIV, diabetes and",
                     "hypertension in 5-year age bands, stratified by site and sex")) |>
  body_add_gg(plot_conditionsoverage_sites + plot_standardformat +
                theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)),
              width = fig_width, height = 6) |>
  add_footnote(paste("Solid lines and circles represent prevalence among men and",
                     "dashed lines and squares represent prevalence among women.")) |>
  body_add_break() |>

  ## Supplementary figure 7 --------
  supp_caption(paste("Supplementary figure 7: Univariable and age and sex-adjusted risk",
                     "factors for multimorbidity, by site and including or excluding HIV",
                     "from the multimorbidity definition")) |>
  body_add_gg(plot_rf_univagesex + plot_standardformat +
                guides(colour = guide_legend(nrow = 3, byrow = TRUE, title.position = "top"),
                       linetype = guide_legend(nrow = 2, byrow = TRUE, title.position = "top"),
                       shape = guide_legend(nrow = 2, byrow = TRUE, title.position = "top"),
                       alpha = guide_legend(nrow = 2, byrow = TRUE, title.position = "top")) +
                theme(axis.title.y = element_blank(),
                      legend.key.spacing.y = unit(-0.2, "cm")),
              width = fig_width, height = 10) |>
  add_footnote(fn_rf_fig) |>
  body_add_break() |>

  ## Supplementary figure 8 --------
  supp_caption(paste("Supplementary figure 8: Risk factors for multimorbidity,",
                     "adjusted for age, sex and socioeconomic position")) |>
  body_add_gg(plot_rf_agesexsep + plot_standardformat +
                theme(axis.title.y = element_blank()),
              width = fig_width, height = 6) |>
  add_footnote(fn_rf_fig) |>
  body_end_block_section(value = block_section(property = ps_portrait)) |>

  ## Supplementary table 8 --------
  supp_caption("Supplementary table 8: Association between multimorbidity and EQ-5D value index") |>
  body_add_flextable(table_eq5dindex) |>
  add_footnote(fn_eq5dindex) |>
  body_add_break() |>

  ## Supplementary table 9 --------
  supp_caption(paste("Supplementary table 9: Association between multimorbidity and EQ-5D",
                     "value: sensitivity analysis comparing modelling approaches")) |>
  body_add_flextable(table_eq5d_all_all) |>
  add_footnote(fn_eq5d_all) |>
  body_end_block_section(value = block_section(property = ps_landscape)) |>

  ## Supplementary table 10 --------
  supp_caption(paste("Supplementary table 10: Median EQ-5D visual analogue scale by",
                     "multimorbidity status")) |>
  body_add_flextable(table_eq5dvas) |>
  add_footnote(fn_eq5dvas) |>
  body_add_break() |>

  ## Supplementary table 11 --------
  supp_caption("Supplementary table 11: Model fit parameters for latent class models") |>
  body_add_flextable(table_lca_fit) |>
  add_footnote(fn_lca_fit) |>
  body_end_block_section(value = block_section(property = ps_portrait)) |>

  ## Supplementary table 12 --------
  supp_caption(paste("Supplementary table 12: Posterior probability of each chronic",
                     "condition by latent class assignment")) |>
  body_add_flextable(table_lca_postprob) |>
  add_footnote(fn_lca_postprob) |>
  body_end_block_section(value = block_section(property = ps_landscape)) |>

  ## Supplementary table 13 --------
  supp_caption(paste("Supplementary table 13: Observed prevalence of chronic conditions",
                     "by latent class assignment")) |>
  body_add_flextable(tbl_cond_class) |>
  add_footnote(fn_cond_class) |>
  body_add_break() |>

  ## Supplementary tables 14 to 16 --------
  supp_caption(paste("Supplementary table 14: Risk factors associated with latent class",
                     "assignment in urban Zimbabwe")) |>
  body_add_flextable(table_rf_class_zim) |>
  add_footnote(fn_rfclass) |>
  body_add_break() |>

  supp_caption(paste("Supplementary table 15: Risk factors associated with latent class",
                     "assignment in rural South Africa")) |>
  body_add_flextable(table_rf_class_sar) |>
  add_footnote(fn_rfclass) |>
  body_add_break() |>

  supp_caption(paste("Supplementary table 16: Risk factors associated with latent class",
                     "assignment in urban South Africa")) |>
  body_add_flextable(table_rf_class_sau) |>
  add_footnote(fn_rfclass) |>
  body_add_break() |>

  ## Supplementary table 17 --------
  supp_caption(paste("Supplementary table 17: Association between latent class assignment",
                     "and EQ-5D value index, using Tobit regression")) |>
  body_add_flextable(table_eq5d_class_tobit) |>
  add_footnote(fn_eq5d_class) |>
  body_add_break() |>

  ## Supplementary tables 18 and 19 - new --------
  supp_caption(paste("Supplementary table 18: Common disease dyads by site, presented as",
                     "rank (number of participants, age- and sex-standardised",
                     "prevalence)")) |>
  body_add_flextable(table_commondyads) |>
  add_footnote(fn_commondyads) |>
  body_add_break() |>

  supp_caption(paste("Supplementary table 19: Common disease triads by site, presented as",
                     "rank (number of participants, age- and sex-standardised",
                     "prevalence)")) |>
  body_add_flextable(table_commontriads) |>
  add_footnote(fn_commontriads) |>
  body_end_block_section(value = block_section(property = ps_portrait)) |>

  ## Supplementary tables 20 and 21 - new --------
  supp_caption(paste("Supplementary table 20: Univariable and age and sex-adjusted odds",
                     "ratios for multimorbidity, by site")) |>
  body_add_flextable(table_rf_univ_agesex) |>
  add_footnote(fn_rf_univ_agesex) |>
  body_add_break() |>

  supp_caption(paste("Supplementary table 21: Odds ratios for multimorbidity adjusted for",
                     "age, sex and socioeconomic position, by site")) |>
  body_add_flextable(table_rf_agesexsep) |>
  add_footnote(fn_rf_agesexsep) |>

  body_end_block_section(value = block_section(property = ps_landscape))

print(doc_supp, target = file.path(output_dir, "FE3_MM_supplement.docx"))

cat("Wrote", file.path(output_dir, "FE3_MM_supplement.docx"), "\n")
