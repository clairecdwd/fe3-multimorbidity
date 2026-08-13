# Multimorbidity analysis
# Fractures E3
# Claire Calderwood

# Plot formatting --------

plot_fsiz_title  = 8
plot_fsiz_labels = 8

alp1 = 0.15
lin1 = 0.3
siz1 = 1

# Every figure is written at the same width so that text renders at the same
# visible size across the document; only height varies.
fig_width = 7

plot_standardformat <-
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = plot_fsiz_title, face = "bold"),
    legend.text = element_text(size = plot_fsiz_labels),
    legend.key.width = unit(0.8, "cm"),
    legend.spacing.x = unit(0, "cm"),
    legend.key.spacing.y = unit(-0.2, "cm"),
    axis.title.x = element_text(size = plot_fsiz_labels, margin = margin(t = 8), face = "bold"),
    axis.title.y = element_text(size = plot_fsiz_labels, margin = margin(r = 8), face = "bold"),
    axis.text = element_text(size = plot_fsiz_labels),
    legendry.legend.subtitle.position = "top",
    legendry.legend.subtitle = element_text(size = 8),
    strip.text.x = element_text(size = plot_fsiz_labels, face = "bold")
  )

# Save a figure at the standard width. Only height varies between figures.
save_fig <- function(plot, path, height) {
  ggsave(filename = path, plot = plot,
         width = fig_width, height = height, units = "in", dpi = 300)
}
