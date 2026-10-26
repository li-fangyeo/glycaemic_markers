library(ggtree)
library(ggtreeExtra)
library(ggnewscale)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(extrafont)
library(grid)
library(mia)
loadfonts(device = "pdf")
      # this should be the only change needed to make the code print
kansio <- "C:/Users/lifyeo/diabetes"

# ── 0. LOAD DATA ──────────────────────────────────────────────────────────────
results <- read.table(file.path(kansio, "results_daa_260526.csv"), sep = ",", header = TRUE)

# ── 1. FILTER AND RESHAPE ─────────────────────────────────────────────────────
sugar <- results %>%
  filter(predictor %in% c("Glucose (0 min)", "Glucose (120 min)")) %>%
  filter(q < 0.05) %>%
  select(!c(p,lwr_95,upr_95)) %>%
  pivot_wider(
    names_from  = predictor,
    values_from = c(est, q),
    names_sep   = "_"
  ) %>%
  replace(is.na(.), 0) %>%
  arrange(Species)

# ── 2. CREATE FAMILY2: families with only one species → "Others" ──────────────
e <- sugar%>% replace(is.na(.), 0) %>% arrange(Species)

f <- e %>% group_by(Family) %>%
  mutate(count = n()) %>%
  ungroup() %>%
  mutate(Family2 = ifelse(count < 2, "Others", Family)) %>%
  select(-count)%>% 
  arrange(Family2, Phylum) %>%
  mutate(Phylum = factor(Phylum, levels = unique(Phylum)))


# Order by phylum, then Family2, then species
f <- f %>%
  arrange(Phylum, Family2, Species)

# ── 3. DATA CHECK ─────────────────────────────────────────────────────────────
glimpse(f)
cat("Total species:", nrow(f), "\n")
cat("Phyla:", paste(unique(f$phylum), collapse = ", "), "\n")
cat("Family2 levels:", paste(sort(unique(f$Family2)), collapse = ", "), "\n")

# ── 4. COLORS ─────────────────────────────────────────────────────────────────
library(paletteer)
cols <- as.character(paletteer::paletteer_d("ggthemes::Tableau_20"))

levels_family <- sort(unique(f$Family2))
family_colors <- setNames(cols[1:length(levels_family)], levels_family)
family_colors["Others"] <- "#AAAAAA"

phylum_levels <- c("Actinobacteriota", "Bacteroidota", "Firmicutes_A",
                   "Firmicutes_C", "Firmicutes_D", "Proteobacteria",
                   "Verrucomicrobiota")

phylum_colors <- setNames(cols[c(1, 3, 5, 9, 13, 19, 20)], phylum_levels)

# ── 5. BUILD COMB TREE (GENUS-LEVEL) ───────────────────────────────────────────

# subset tse to significant genera
selected_rows <- rowData(tse)$Species %in% f$Species & 
  !is.na(rowData(tse)$Species)

tse2 <- tse[selected_rows, ]

# agglomerate and update tree at Genus level
tse2 <- tse2 %>% 
  mia::agglomerateByRank(rank = "Species", update.tree = TRUE) %>%
  mia::transformAssay(method = "clr", pseudocount = 1)

tse2 <- subsetByLeaf(tse2, rowLeaf = rownames(tse2))
tse2 <- mia::addHierarchyTree(tse2)

tree <- rowTree(tse2)
metadata <- rowData(tse2)

#need this to map the tree
f$Species <- paste0("Species:", f$Species)


# ── 6. BASE TREE PLOT ─────────────────────────────────────────────────────────
p <- ggtree(tree, layout = "fan", open.angle = 20)

# ── 7. PHYLUM RING ────────────────────────────────────────────────────────────
q <- p +
  geom_fruit(
    data    = f,
    geom    = geom_tile,
    mapping = aes(y = Species, x = 1, fill = Phylum),
    width   = 0.05,
    offset  = 0.01
  ) +
  scale_fill_manual(
    values = phylum_colors,
    name   = "Phylum",
    guide  = guide_legend(order = 1)
  )

# ── 8. FAMILY BARS AND ZERO LINES ────────────────────────────────────────────
q <- q +
  new_scale_fill() +
  geom_fruit(
    data        = f,
    geom        = geom_bar,
    mapping     = aes(y = Species, x = `est_Glucose (0 min)`, fill = Family2),
    orientation = "y",
    stat        = "identity",
    width       = 0.6,
    offset      = 0.3
  ) +
  geom_fruit(
    data        = f,
    geom        = geom_bar,
    mapping     = aes(y = Species, x = `est_Glucose (120 min)`, fill = Family2),
    orientation = "y",
    stat        = "identity",
    width       = 0.6,
    offset      = 0.3
  ) +
  scale_fill_manual(values = family_colors, name = "Family") +
  theme(
      legend.position = "right",
      legend.text     = element_text(size = 14),
      legend.title    = element_text(size = 16, face = "bold") 
      ) +
  geom_vline(xintercept = 0.91, color = "grey", linetype = "solid", size = 0.4) +  # WHR
  geom_vline(xintercept = 1.21, color = "grey", linetype = "solid", size = 0.4)
  
q
# ── 9. SAVE WITH RING LABELS ──────────────────────────────────────────────────
pdf(file.path(kansio, "beautifulcircle_taxa.pdf"), width = 12, height = 12)
grid.draw(ggplotGrob(q))
grid.text("FG",   x = 0.6025, y = 0.4625, rot = 270, gp = gpar(fontsize = 20, fontface = "bold"))
grid.text("PPG", x = 0.665,  y = 0.4575, rot = 270, gp = gpar(fontsize = 20, fontface = "bold"))
dev.off()

#save dataframe for supp. data
write.csv(f, "sigTaxa.csv")
