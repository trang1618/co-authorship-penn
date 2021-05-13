
library(igraph)
library(tidyverse)
library(ggraph)

coauthor_df <- readxl::read_excel("Co-author table.xlsx")
coauthor_graph <- coauthor_df %>%
  pivot_longer(-Name,
    names_to = "author2",
    values_to = "n"
  ) %>%
  filter(n != 0) %>%
  rename(author1 = Name) %>%
  graph_from_data_frame()

coauthor_graph %>%
  ggraph(layout = "auto") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name),
    repel = TRUE,
    point.padding = unit(0.2, "lines")
  ) +
  theme_void()

igraph_layouts <- c("dh", "fr", "kk", "stress")

p <- list()
for (ilayout in igraph_layouts) {
  p[[ilayout]] <- coauthor_graph %>%
    ggraph(layout = ilayout) +
    geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
    geom_node_point(size = 5) +
    geom_node_text(aes(label = name),
      repel = TRUE,
      point.padding = unit(0.2, "lines")
    ) +
    ggtitle(paste0("Layout: ", ilayout)) +
    theme_void() +
    theme(legend.position = "None")
}
cowplot::plot_grid(plotlist = p)
# p[[7]]
