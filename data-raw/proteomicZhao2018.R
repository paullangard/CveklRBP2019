## code to prepare `proteomicZhao2018` dataset goes here
#explore available data
filepath_ProteomeZhao2018 <- "data-raw/DAVI-429_proteinGroups.xlsx"
#data are located in the excel sheet "proteinGroups"

#not all columns are of interest, define which one to keep at data import
coltypes_ProteomeZhao2018 <- c(
  rep("skip",7),
  rep("text",2),#col Protein names and gene names
  rep("skip",29),
  rep("numeric",2), #col intensity epith and intensity fibers
  rep("skip",3),
  rep("numeric",2), #col ibaq epithelium + rank
  "skip",
  rep("numeric",2), #col ibaq fiber + rank
  rep("skip",4),
  rep("numeric",2), #lfq fibers and epith
  rep("skip",5),
  rep("numeric",2), # ms/ms fiber and epith
  rep("skip",20),
  "text", #uniprot id,
  rep("skip",5),
  "text", #mgi gene name,
  rep("skip",2),
  rep("text",14), #annotation
  "skip")


#get R friendly colnames for few annotation columns
annotationsFeatures <- readxl::read_xlsx(filepath_ProteomeZhao2018,
                                         n_max = 10,
                                         skip = 4,
                                         sheet = "proteinGroups") %>%
  names() %>% .[91:104] %>%
  str_remove(.,"KW: |GO: ") %>%
  str_remove(.," ")

#defining colnames
colnames_ProteomeZhao2018 <- c(
  "ProteinNames",
  "GeneNames",
  paste0("Intensity",c("Epith","Fibers")),
  paste0(c("iBAQ","rank"),"Epith"),
  paste0(c("iBAQ","rank"),"Fibers"),
  paste0("LFQ",c("Epith","Fibers")),
  paste0("MsMs",c("Epith","Fibers")),
  "UniprotID",
  "MGIGeneNames",
  annotationsFeatures
)

#import dataset proteomic zhao 2018
t_proteomicZhao2018 <- readxl::read_xlsx(filepath_ProteomeZhao2018,
                                         skip = 5,
                                         sheet = "proteinGroups",
                                         col_types = coltypes_ProteomeZhao2018,
                                         col_names = colnames_ProteomeZhao2018) %>%
  separate_rows(sep=";",GeneNames) %>% #split lines with mentionning multiple genes
  dplyr::filter(!is.na(GeneNames))

usethis::use_data(t_proteomicZhao2018, overwrite = T)
