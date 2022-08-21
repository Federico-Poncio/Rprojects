dataset <- read.csv("dataset_regla1_conVOs_yNAs_yLikert.csv", 
                    row.names = 1)
cols.df <- colnames(dataset)[6:24]

for (i in 6:20) {
  dataset[,i] <- factor(dataset[,i], levels = c(1,2,3,4,5,9),
                        labels = c("Muy en desacuerdo",
                                   "En desacuerdo",
                                   "Ni tr. Ni est.",
                                   "De acuerdo",
                                   "Muy de acuerdo",
                                   "NsNc"))
}

dataset$EDAD_AGRUP <- factor(dataset$EDAD_AGRUP,
                             levels = c(2,3,4,5,6),
                             labels= c("18-24","25-40", 
                                       "41-55", "56-64", "65+"))
dataset$SEXO <- factor(dataset$SEXO, levels = c(1,2), 
                       labels = c("Hombre", "Mujer"))
dataset$NED <- factor(dataset$NED, levels = c(1,2,3,4,5), 
                       labels = c("Primario", "Sec. inc.", 
                                  "Secundario", "Terc. inc.",
                                  "Terciario o Univ"))

dataset$LUGAR <- factor(dataset$LUGAR, levels = c(1,2), 
                      labels = c("CABA", "Conurbano"))

dataset$P1 <- factor(dataset$P1, levels = c(1,2,3,4,5),
                     labels = c("Estudiante", "Sector Priv", 
                                "Sector Pub", "Jubilado o Ama de casa",
                                "No tiene trabajo"))
write.csv(dataset, "dataset_regla1_conVOs_yNAs_yLikert_factors.csv")

results <-
  survey::svydesign(~ 1, data = dataset, weights = ~ pesos_finales) %>%
  tbl_svysummary(
    by = P22.factor,
    include = cols.df,
    statistic = list(all_categorical() ~ "{n_unweighted} ({p}%)")
  )%>%
  modify_header(label = "**Voto 2019**",
                all_stat_cols(FALSE) ~  "**{level}**<br>N = {n_unweighted}")
results
results %>%
  as_flex_table() %>%
  flextable::save_as_docx(path = "FrecuenciasVoto2019.docx")

results <-
  survey::svydesign(~ 1, data = dataset, weights = ~ pesos_finales) %>%
  tbl_svysummary(
    by = P21.factor,
    include = cols.df,
    statistic = list(all_categorical() ~ "{n_unweighted} ({p}%)")
  )%>%
  modify_header(label = "**Voto futuro**",
                all_stat_cols(FALSE) ~  "**{level}**<br>N = {n_unweighted}")
results
results %>%
  as_flex_table() %>%
  flextable::save_as_docx(path = "FrecuenciasVotoFuturo.docx")

results <-
  survey::svydesign(~ 1, data = dataset, weights = ~ pesos_finales) %>%
  tbl_svysummary(include = cols.df,
    statistic = list(all_categorical() ~ "{n_unweighted} ({p}%)")
  )%>%
  modify_header(label = "**Preguntas**",
                all_stat_cols(FALSE) ~  "**{level}**")
results

install.packages("flextable")
results %>%
  as_flex_table() %>%
  flextable::save_as_docx(path = "FrecuenciasBase.docx")


tbl_cross(dataset, row=P22.factor, 
          label = list(P22.factor ~ "Elecciones 2019", P21.factor ~ "Próximo mes"),
          col=P21.factor,
          margin="column",percent="row", statistic = "{p}%\n({n})")

results <-
  survey::svydesign(~ 1, data = dataset, weights = ~ pesos_finales) %>%
  tbl_svysummary(
    by = P21.factor,
    include = P22.factor,
    statistic = list(all_categorical() ~ "{p}% ({n_unweighted})")
  )%>%
  modify_header(label = "**Voto 2019 y futuro**",
                all_stat_cols(FALSE) ~  "**{level}**<br>N = {n_unweighted}") %>%
  remove_row_type(P22.factor, type = "header")
results
results %>%
  as_flex_table() %>%
  flextable::save_as_docx(path = "Voto2019yFuturo.docx")

colnames(dataset)[1:5] <- c("GrupoEtario", "Lugar", "NivelEd", 
                            "Sexo", "Ocupación")

results <-
  survey::svydesign(~ 1, data = dataset, weights = ~ pesos_finales) %>%
  tbl_svysummary(
    by = P21.factor,
    include = c(GrupoEtario, Lugar, NivelEd, Ocupación, Sexo),
    statistic = list(all_categorical() ~ "{p}% ({n_unweighted})")
  )%>%
  modify_header(label = "**Voto futuro**",
                all_stat_cols(FALSE) ~  "**{level}**<br>N = {n_unweighted}") %>%
  remove_row_type(P22.factor, type = "header")
results
results %>%
  as_flex_table() %>%
  flextable::save_as_docx(path = "VotoFuturo_Cruces.docx")


results <-
  survey::svydesign(~ 1, data = dataset, weights = ~ pesos_finales) %>%
  tbl_svysummary(
    by = P22.factor,
    include = c(GrupoEtario, Lugar, NivelEd, Ocupación, Sexo),
    statistic = list(all_categorical() ~ "{p}% ({n_unweighted})")
  )%>%
  modify_header(label = "**Voto 2019**",
                all_stat_cols(FALSE) ~  "**{level}**<br>N = {n_unweighted}") %>%
  remove_row_type(P22.factor, type = "header")
results
results %>%
  as_flex_table() %>%
  flextable::save_as_docx(path = "Voto2019_Cruces.docx")



results <-
  survey::svydesign(~ 1, data = dataset, weights = ~ pesos_finales) %>%
  tbl_svysummary(
    by = P22.factor,
    include = c(km.cluster.lik),
    statistic = list(all_categorical() ~ "{p}% ({n_unweighted})")
  )%>%
  modify_header(label = "**Voto 2019**",
                all_stat_cols(FALSE) ~  "**{level}**<br>N = {n_unweighted}") %>%
  remove_row_type(P22.factor, type = "header")
results
