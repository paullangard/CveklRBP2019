read_tsv("../data/derived_data/RBP_functionalannotations_processed.tsv") %>%
  dplyr::filter(!(na_count>1|(group4=="Translation" & is.na(group5)))) %>%
  mutate(group4 = case_when(group4 == "Translation" ~ group5,
                            TRUE ~ group4)) %>%
  dplyr::select(gene, group1=group3, group2=group4) %>%
  left_join(t_proteomicZhao2018_RBP, by="gene") %>%
  drop_na()%>%
  ungroup() %>%
  group_by(group1, group2, tissue) %>%
  summarise(tissuecount=n(), genes = paste(gene, collapse=", ")) %>% ungroup() %>%
  group_by(group1, group2) %>%
  mutate(n=sum(tissuecount), genes = paste(tissue, genes, collapse="//", sep="; ")) %>%
  ungroup() %>%
  arrange(group2, n) %>%
  mutate(group1 = group1 %>% str_wrap(15) %>% {factor(case_when(.=="ncRNA binding" ~ "ncRNA\nbinding", TRUE ~.), levels = c("pre-mRNA\nbinding", "mRNA binding", "ncRNA\nbinding"))},
         group2 = group2 %>% str_wrap(24) %>% {factor(., levels = unique(.))%>% fct_reorder(dplyr::desc(n))}) %>%
  ggplot(aes(x=group2, y=tissuecount, fill=tissue)) +
  geom_bar(stat="identity", width = .4, position = position_stack(reverse=T)) +
  scale_fill_manual(values=c("gray", "blue", "red"), labels=c("Both", "Lens epithelium", "Lens fibers")) +
  guides(fill=guide_legend(reverse = T)) +
  #scale_color_manual(values=c("gray", "blue", "red"), labels=c("Both", "Lens epithelium", "Lens fibers")) +
  scale_x_discrete(expand = c(0, 1.5)) +
  scale_y_continuous(expand = c(0, 10)) +
  facet_grid(.~group1, scales = "free", space="free") +
  geom_text(aes(x=group2, y=n+8, label = n), size=2.5) +
  #ylim(c(0,85)) +
  theme_pubclean() +
  labs(fill=paste("proteomic \nrank <", proteomicRanking.cutoff))+
  ylab("number of RBP") +
  theme(axis.text.x = element_text(angle=45, hjust = 1, color="black", face = "bold"),
        text = element_text(size=7),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=7),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.text = element_text(size=7),
        legend.box.margin = margin(), legend.key.width = unit(2, "mm"), legend.key.height = unit(2, "mm"),
        panel.background = element_rect(fill="transparent"),
        legend.position = "right",  legend.margin = margin(),
        strip.text = element_text(face="bold", size = 7),
        legend.background = element_blank(),
        strip.background = element_rect(fill="transparent", color="black", linetype = 1),
        legend.direction = "vertical")
