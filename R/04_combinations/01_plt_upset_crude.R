# Multimorbidity analysis
# Fractures E3
# Claire Calderwood

# Crude intersection plot, by country --------
# Requires: df, defn_mm, defn_mm_labels, set_order, n_intersections, sex_colors
# Creates:  plot_upset, fn_upset
# Feeds:    Supplementary figure 2
#
# Unweighted intersections in the study population as observed. The
# population-standardised versions, which the main figure uses, are built in
# 02_plt_upset_standardised.R.
#
# Country panels only; the site-level figure is the standardised version in
# 02_plt_upset_standardised.R.

# Membership matrix --------
# %in% "Yes" so that missing condition status is treated as the condition being
# absent, rather than yielding a logical NA and leaving ComplexUpset to decide
# what an NA membership means.
df_upset <- df |>
  select(all_of(defn_mm), con_sex, con_age, site, site2) |>
  filter(if_any(all_of(defn_mm), ~ .x %in% "Yes")) |>
  mutate(across(all_of(defn_mm), ~ .x %in% "Yes")) |>
  rename(all_of(defn_mm_labels)) |>
  select(all_of(set_order), con_sex, con_age, site, site2)

# Sets are drawn top to bottom, so the display order is reversed
upset_sets = rev(set_order)

# Panels --------
# Bars are counts, so the y limit is taken from the data rather than fixed: a
# hardcoded limit clips without saying so if an intersection is larger.
upset_ymax = df_upset |>
  count(site2, across(all_of(set_order))) |>
  pull(n) |>
  max()

upset_panel <- function(data, title, show_y = FALSE) {
  (upset(data,
         intersect = upset_sets,
         n_intersections = n_intersections,
         keep_empty_groups = TRUE,
         set_sizes = FALSE,
         sort_sets = FALSE,
         base_annotations = list(
           `Intersection size` = intersection_size(
             text = aes(size = 1.6, color = "black"),
             mapping = aes(fill = con_sex),
             bar_number_threshold = 1)
           + coord_cartesian(ylim = c(0, upset_ymax))
           + scale_fill_manual(values = sex_colors)
           + if (show_y) theme() else theme(axis.title.y = element_blank(),
                                            axis.text.y = element_blank())))
   + theme(axis.title.x = element_blank(),
           axis.text.y = if (show_y) element_text() else element_blank())
   + ggtitle(title))
}

u_gam <- upset_panel(df_upset |> filter(site2 == "Gambia"), "Gambia", show_y = TRUE)
u_zim <- upset_panel(df_upset |> filter(site2 == "Zimbabwe"), "Zimbabwe")
u_sa  <- upset_panel(df_upset |> filter(site2 == "South Africa"), "South Africa")

plot_upset <- (u_gam | u_zim | u_sa) + plot_layout(guides = "collect")

# Written to disk because the compile step inserts it as an image rather than a
# ggplot: ComplexUpset returns a patchwork, which body_add_gg cannot size.
ggsave(filename = file.path(plot_dir, "plot_upset_crude.png"),
       plot = plot_upset, width = 15, height = 9, dpi = 300)

path_upset_crude = file.path(plot_dir, "plot_upset_crude.png")

fn_upset = paste0(
  "Colours indicate sex of participant: purple = female and orange = male. ",
  "Largest ", n_intersections, " exclusive intersections shown per panel, that is the number ",
  "of people with that unique combination of conditions. The 'no conditions' ",
  "intersection is excluded from display. Conditions which did not contribute to any ",
  "of the ", n_intersections, " largest groups in any site are not listed; participants may ",
  "additionally have conditions that are not shown. Intersections shown here are ",
  "crude, that is not standardised to the national population.")
