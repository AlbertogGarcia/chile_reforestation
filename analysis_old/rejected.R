file_dir <- "C:/Users/garci/Dropbox/chile_reforestation/"

file_list <- lapply(list.files(paste0(file_dir, "external_data/concurso_conaf/unawarded"), pattern = "*xlsx", full.names = TRUE)
                    , read_xlsx)


rejected_df <- bind_rows(file_list)%>%
  mutate(`Contest type` = ifelse(`Tipo Concurso` == "Otros Interesados", "Other interested", "Smallholder"),
         `Contest year` = `Año Concurso`)

library(rio)
export(rejected_df, "paper/results/rejected_df.rds")

