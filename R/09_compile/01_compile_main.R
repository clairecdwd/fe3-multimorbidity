# Multimorbidity analysis
# Fractures E3
# Claire Calderwood

# Compile the main document --------
# Requires: every table and figure object built by the preceding scripts
# Creates:  output/FE3_MM_main.docx
# Feeds:    tables 1 and 2, figures 1 to 3 of the manuscript
#
# Numbers are live Word SEQ fields, so on first opening the document select all
# and update fields (F9) to populate them and any list of tables or figures.
# The main document has no gaps in its numbering, so the fields resolve to the
# numbers used in the manuscript. Figure 4 of the manuscript is the visual
# abstract, which is not produced by code and is not included here.

doc_main <- read_docx(template_docx) |>

  body_add_par("Fractures E3 multimorbidity analysis", style = "heading 1") |>

  ## Table 1 --------
  body_add_caption(block_caption(
    "Characteristics and observed prevalence of chronic conditions in the study population",
    style = "Caption", autonum = autonum)) |>
  body_add_flextable(tbl_desc_morbidities_site) |>
  add_footnote(fn_desctable) |>
  body_add_break() |>

  ## Figure 1 --------
  body_add_caption(block_caption(
    "Intersection plots illustrating common disease combinations, by country",
    style = "Caption", autonum = autonum_fig)) |>
  body_add_img(src = path_upset_std, width = fig_width, height = 5) |>
  add_footnote(fn_upset_std) |>
  body_add_break() |>

  ## Figure 2 --------
  body_add_caption(block_caption(
    paste("A: Number of people with chronic conditions, standardised to national",
          "population in 5-year age bands. B: Percentage of people with chronic",
          "conditions in the study population by 5-year age bands.",
          "C: Prevalence of HIV, diabetes and hypertension by 5-year age band"),
    style = "Caption", autonum = autonum_fig)) |>
  body_add_gg(
    ggarrange(
      ggarrange(poppyramid + plot_standardformat + theme(plot.margin = margin(12, 0, 0, 0)),
                obspyramid + plot_standardformat + theme(plot.margin = margin(12, 0, 0, 0)),
                ncol = 1, labels = c("A", "B"),
                font.label = list(size = 9, color = "black", face = "bold"),
                legend = "bottom", common.legend = TRUE),
      plot_conditionsoverage_country + plot_standardformat +
        theme(legend.position = "bottom",
              axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)),
      ncol = 1, labels = c("", "C"),
      font.label = list(size = 9, color = "black", face = "bold"),
      heights = c(2, 1.11),
      # Legends are deliberately not collected across the two blocks: the
      # pyramids and the coverage panel use different fills
      common.legend = FALSE),
    width = fig_width, height = 9) |>
  add_footnote(fn_poppyramid) |>
  body_add_break() |>

  ## Table 2 --------
  body_add_caption(block_caption(
    paste("Association between multimorbidity, and multimorbidity clusters,",
          "and EQ-5D value index with Tobit regression"),
    style = "Caption", autonum = autonum)) |>
  body_add_flextable(table_eq5d_tobit_combo) |>
  add_footnote(fn_eq5d_combo) |>
  body_add_break() |>

  ## Figure 3 --------
  body_add_caption(block_caption(
    paste("Observed-to-expected ratio for chronic conditions by latent class",
          "in Zimbabwe and South Africa"),
    style = "Caption", autonum = autonum_fig)) |>
  body_add_gg(plot_lca_oe_nogam + plot_standardformat, width = fig_width, height = 5) |>
  add_footnote(fn_lca_oe) |>

  body_set_default_section(ps_portrait)

print(doc_main, target = file.path(output_dir, "FE3_MM_main.docx"))

cat("Wrote", file.path(output_dir, "FE3_MM_main.docx"), "\n")
