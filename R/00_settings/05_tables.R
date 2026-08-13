# Multimorbidity analysis
# Fractures E3
# Claire Calderwood

# Table formatting --------

flextable::set_flextable_defaults(font.family = "Arial", font.size = 8)

fontsize = 8
internalmargins = 3

inner_border  = fp_border(color = "black", style = "dotted", width = 1)
header_border = fp_border(color = "black", style = "solid", width = 1)

# Live Word SEQ fields, so that reordering in Word renumbers automatically and a
# list of tables or figures can be generated. The prop pins the number to the
# same Arial black as the caption; without it Word reverts it to the theme font.
caption_prop = fp_text_lite(bold = TRUE, color = "black",
                            font.family = "Arial", font.size = 10)

autonum     = run_autonum(seq_id = "tab", pre_label = "Table ",
                          post_label = ". ", bkm = "tab", prop = caption_prop)

autonum_fig = run_autonum(seq_id = "fig", pre_label = "Figure ",
                          post_label = ". ", bkm = "fig", prop = caption_prop)
