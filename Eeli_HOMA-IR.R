library(ggplot2)
library(dplyr)
library(extrafont)
loadfonts(device = "pdf")
  # this should be the only change needed to make the code print
kansio <- "C:/Users/eelih/OneDrive/diabetes/Taxa-kuvaaja"

results <- read.table(file.path(kansio, "results_daa_260526.csv"), sep = ",", header = TRUE)

# ── 1. FILTER HOMA-IR SIGNIFICANT SPECIES ────────────────────────────────────
homa <- results %>%
  filter(predictor == "HOMA-IR") %>%
  filter(q < 0.05) %>%
  arrange(est)

# ── 2. ORDER SPECIES BY ESTIMATE ─────────────────────────────────────────────
homa$species <- factor(homa$species, levels = homa$species)

# ── 3. FDR LABEL ──────────────────────────────────────────────────────────────
homa$fdr_label <- paste0("q=", round(homa$q, 3))
homa$ci_label <- paste0(round(homa$est, 3), " (", round(homa$lwr_95, 3), ", ", round(homa$upr_95, 3), ")")


# ── 3b. PHYLUM COLORS ──── extra colours to use as a base for other traits
library(paletteer)
phylum_colors <- c(
  "Actinobacteriota" = "#4E79A7", # not visible
  "Bacteroidota"     = "#F28E2B", # not visible
  "Firmicutes_A"     = "#1B7837",   
  "Firmicutes_C"     = "#E15759", # not visible
  "Firmicutes_D"     = "#525252",  
  "Proteobacteria"   = "#8B5E3C",   
  "Verrucomicrobiota"= "#9467BD"  # not visible
)

# ── 4. PLOT ───────────────────────────────────────────────────────────────────
p <- ggplot(homa, aes(x = est, y = species, color = phylum)) +
  geom_vline(xintercept = 0, color = "grey75", linewidth = 0.8) +
  geom_errorbarh(aes(xmin = lwr_95, xmax = upr_95),
                 height = 0.3, linewidth = 0.6) +
  geom_point(size = 3) +
  geom_text(aes(x = 0.22, label = ci_label),
            hjust = 0.5, size = 3.5, family = "ArialMT", show.legend = FALSE) +
  scale_x_continuous(limits = c(NA, 0.25), breaks = c(-0.1, 0.0, 0.1)) +
  scale_color_manual(values = phylum_colors, name = "Phylum") +
  labs(
    title = "HOMA-IR — significant bacterial species (FDR < 0.05)",
    x     = "Estimate (β) with 95% CI",
    y     = NULL
  ) +
  theme_classic(base_family = "ArialMT") +
  theme(
    legend.position       = c(1.95, 0.5),
    legend.justification  = c(1, 0.5),
    legend.text           = element_text(size = 12, family = "ArialMT"),
    legend.title          = element_text(size = 14, face = "bold", family = "ArialMT"),
    axis.text.y           = element_text(size = 11, face = "italic"),
    axis.text.x           = element_text(size = 11),
    plot.title            = element_text(size = 14, face = "bold")
  ) +
  coord_cartesian(clip = "off") +
  theme(plot.margin = margin(10, 225, 10, 10))

# ── 5. SAVE ───────────────────────────────────────────────────────────────────
pdf(file.path(kansio, "forestplot_homa_ir.pdf"), width = 10, height = 7)
print(p)
dev.off()