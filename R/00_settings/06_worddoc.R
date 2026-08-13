# Multimorbidity analysis
# Fractures E3
# Claire Calderwood

# Word document sections and styles --------
# The template in assets/ is read-only; nothing here overwrites it at run time.

ps_landscape <- prop_section(
  page_size = page_size(orient = "landscape"),
  type = "continuous",
  page_margins = page_mar(bottom = 0.5, top = 0.5, right = 0.5, left = 0.5)
)

ps_portrait <- prop_section(
  page_size = page_size(orient = "portrait"),
  type = "continuous",
  page_margins = page_mar(bottom = 0.5, top = 0.5, right = 0.5, left = 0.5)
)

template_docx = paste0(projdir, "fe3-multimorbidity/assets/fe3_mm_template.docx")

# Footnote blocks are written with a bold "Footnotes:" label and the text in
# normal weight, so this helper is used everywhere rather than a plain par.
add_footnote <- function(doc, text) {
  officer::body_add_fpar(
    doc,
    officer::fpar(
      officer::ftext("Footnotes: ", officer::fp_text_lite(bold = TRUE,
                                                          font.family = "Arial",
                                                          font.size = 9)),
      officer::ftext(text, officer::fp_text_lite(bold = FALSE,
                                                 font.family = "Arial",
                                                 font.size = 9))
    )
  )
}
