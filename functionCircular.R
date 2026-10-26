##Circular heatmap
library(ggplot2)
library(dplyr)
library(tibble)

sigfunc <- read.table("SigPathway.csv", sep = ",", header = TRUE)

e <-sigfunc %>%
  group_by(LevelTwo) %>%
  mutate(count = n()) %>%
  ungroup() %>%
  mutate(pathway2 = ifelse(count < 2, "Others", LevelTwo)) %>%
  select(-count)%>% 
  arrange(pathway2, Pathway) %>%
  mutate(Pathway = factor(Pathway, levels = unique(Pathway)))


# Create the circular bar plot
A<- ggplot(e, aes(x = factor(Pathway), y = estimate, fill = pathway2)) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar(start = 0) +
  theme_classic() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major.y = element_line(color = "lightgrey", linewidth = 0.2),
    axis.title = element_blank(),
    axis.line = element_blank(),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  ) +
  scale_fill_brewer(palette = "Spectral", name = "Pathway class")

A
##Top 20 functional pathway dotplot
et<- e %>% 
  arrange(qval_fdr) %>%
  slice_head(n=20) %>%
  select(Pathway, pathway2, estimate, estimate, qval_fdr)

funcolor<- c("Biosynthesis" = "#D53E4F", "Carrier biosynthesis" = "#F46D43", 
             "Cell wall biosynthesis" = "#FDAE61", "Nucleotide biosynthesis" = "#E6F598",
             "Others" = "#ABDDA4", "Proteinogenic amino acid biosynthesis" = "#66C2A5", 
             "Sugar biosynthesis" = "#3288BD", "Terpenoid biosynthesis" = "#5E4FA2")
# Create the dot plot
B<- ggplot(et, aes(x = estimate, y = Pathway, color = pathway2, size = qval_fdr)) +
  geom_point() +
  scale_size_continuous(range = c(15, 5),
                        breaks = c(0.005, 0.01, 0.0235),
                        labels = c("<0.005", "<0.01", "<0.05")
  ) +
  labs(
    x = "Estimate",
    y = "Pathway",
    color = "Class",
    size = "FDR"
  ) +
  theme_classic() +
  scale_color_manual(values = funcolor) +
  theme(
    text = element_text(color = "black"),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16))

library(patchwork)
nested <- (A / B) +
  plot_annotation(tag_levels = 'A')
nested
ggplot2::ggsave(filename = "nested2.pdf", 
                plot = nested,
                #dpi = 300,
                width = 18,
                height = 15,
                units = "in" )

