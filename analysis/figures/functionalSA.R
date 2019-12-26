levelsFunction <- c("splicing", "pre-mRNA Transport and processing",
                    "mRNA processing", "translational regulation",
                    "mRNA transport", "mRNA stability",
                    "mRNA sequestration", "Stress response",
                    "mRNA degradation", "translational initiation", "ribosome", "ncRNA binding")

t_annotationsRBP_figure <- t_annotationsRBP_annotated %>%
  group_by(gene) %>%
  summarise(groups=paste(unique(group), collapse = ", ")) %>%
  ungroup() %>%
  arrange(gene) %>%
  dplyr::filter(!gene=="") %>%
  mutate(idgene=row_number()) %>%
  separate_rows(groups, sep=", ") %>%
  mutate(fat = case_when(idgene < 54 ~ 1,
                         54 <= idgene & idgene < 108 ~ 2,
                         108 <= idgene & idgene < 162 ~ 3,
                         idgene >= 162  ~ 4))  %>%
  left_join(t_proteomicZhao2018_RBP, by="gene") %>%
  dplyr::filter(groups %in% levelsFunction)

levelsFunction_wrapped <- c("splicing", "pre-mRNA Transport \nand processing",
                            "mRNA processing", "translational regulation",
                            "mRNA transport", "mRNA stability",
                            "mRNA sequestration", "Stress response",
                            "mRNA degradation", "translational initiation", "ribosome", "ncRNA binding")

t_annotationsRBP_figure %>%
  mutate(groups = case_when(groups=="pre-mRNA Transport and processing" ~ "pre-mRNA Transport \nand processing", T ~  groups)) %>% #filter(grepl("pre", groups))
  #mutate(groups = str_wrap(groups, 20)) %>%
  ggplot(aes(x=factor(groups, levels=levelsFunction_wrapped),
             y=reorder(gene, dplyr::desc(gene)),
             fill=factor(groups, levels=levelsFunction_wrapped))) +
  geom_tile(color="black") +
  geom_point(aes(color=tissue, shape=tissue,
                 x=-.35,
                 y=reorder(gene, dplyr::desc(gene))),
             size=2,
             inherit.aes = F) +
  facet_wrap(~fat, scales="free_y", nrow = 1) +
  geom_text(aes(label=gene, x=-.85, fontface=case_when(PWMavailable == T ~ "bold", T~"plain")), hjust="right", size=2.8) +
  #use Set3 palette, replace grey #D9D9D9 by tan3 so as to avoid visual confusion with tissue colors
  scale_fill_manual(values = RColorBrewer::brewer.pal(12, name = "Set3") %>% {case_when(. == "#D9D9D9" ~ "tan3", T ~ .)})+
  scale_color_manual(values=c("gray", "blue", "red"),
                     labels=c("Both", "Lens epithelium", "Lens fibers")) +
  scale_shape_manual(values=c(1, 2, 0),
                     labels=c("Both", "Lens epithelium", "Lens fibers")) +
  guides(fill=guide_legend(nrow=3,byrow=TRUE, title = ""),
         color=guide_legend(title = "proteomic rank < 1800"),
         shape=guide_legend(title = "proteomic rank < 1800")) +
  theme(aspect.ratio = 5,
        axis.text.x = element_blank(),
        # axis.text.x = element_text(angle=45, hjust=1, colour = "black", size=6),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "top",
        axis.text.y = element_text(color="transparent",
                                   family = "Helvetica",
                                   size=7,
                                   margin = margin(r=3, unit="mm")),
        panel.grid.major.x = element_line(color="white", size = .2),
        panel.grid.major.y = element_line(color="white", size = .2),
        panel.border = element_rect(color="black", fill = NA),
        legend.title = element_text(size=8),
        panel.background = element_rect(fill="#eeeeee",size = .2),
        plot.margin = margin(0,0,0,0, unit = "cm"),
        strip.background = element_blank(),
        #axis.ticks.y = element_line(color="black"),
        legend.key.size = unit(2, "mm"), legend.box = "vertical",
        legend.text = element_text(size=8),
        strip.text.x = element_blank()) +
  coord_cartesian(xlim = c(1, 12), # This focuses the x-axis on the range of interest
                  clip = 'off')
