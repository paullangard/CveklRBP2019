## code to prepare `transcriptomicZhao2018` dataset goes here

t_transcriptomicZhao2018 <- read_tsv("data-raw/rnaseq_zhao2018_mouse_rawcount.tsv") %>%
  gather(-gene, key="key", value="FPKM") %>%
  separate(col="key", into=c("stage", "tissue"), sep="_") %>%
  arrange(gene, stage, tissue) %>%
  mutate(replicate = str_remove(tissue, "epi|fiber"),
         tissue = str_remove(tissue, "[1-3]"))

usethis::use_data(t_transcriptomicZhao2018, overwrite = T)
