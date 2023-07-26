library(DescTools)
library(ggtext)
library(tidyverse)
library(lubridate)
library(scales)
library(readxl)

source("theme.R")

# Functions ----

convert_to_date_chr <- function(chr) {
  chr %>%
    as.numeric() %>%
    as.Date(origin = ymd("1899-12-30")) %>%
    as.character()
}

convert_date_column <- function(chr) {
  ifelse(chr %in% c(NA, "N/D", "N/A", "N/C"), chr, convert_to_date_chr(chr))
}

calc_day_interval <- function(start, end) {
  ifelse(
    (start %in% c("N/D", "N/A", "N/C")) | (start %in% c("N/D", "N/A", "N/C")),
    NA, as.numeric(ymd(end) - ymd(start))
  )
}

calc_date_stat <- function(date_vec, fun) {
  ifelse(
    date_vec %in% c("N/D", "N/A", "N/C"),
    NA, ymd(date_vec)
  ) %>%
    fun(na.rm = TRUE) %>%
    as_date()
}

calc_first_response <- function(date1, date2) {
  if (date1 %in% c(NA, "N/D", "N/A", "N/C")) {
    if (date2 %in% c(NA, "N/D", "N/A", "N/C")) {
      return(NA)
    } else {
      return(date2)
    }
  } else {
    if (date2 %in% c(NA, "N/D", "N/A", "N/C")) {
      return(date1)
    } else {
      return(
        c(ymd(date1), ymd(date2)) %>%
          min() %>%
          as_date() %>%
          as.character()
      )
    }
  }
}

save_plot <- function(filename, path = "./plots", height = 6, width = 10, dpi = 300, ...) {
  ggsave(filename=filename, height=height, width=width, path=path, dpi=dpi, ...)
}

formater_percent <- label_percent(accuracy = 0.1, suffix = "%", big.mark = ".", decimal.mark = ",")

# Data ----

data <- read_csv("dados.csv") %>%
  janitor::clean_names() %>%
  filter(agencia %in% c("ANTT", "ANTAQ", "ANAC")) %>%
  filter(str_detect(nome_mecanismo_agencia, "Pública"))

data <- data %>%
  mutate(ano_date = paste0(ano, "-01-01") %>% ymd(),
         data_contribuicao_inicio = convert_date_column(data_contribuicao_inicio),
         data_contribuicao_fim = convert_date_column(data_contribuicao_fim),
         data_convocacao = convert_date_column(data_convocacao),
         rac_data = convert_date_column(rac_data),
         relatorio_sintese_data = convert_date_column(relatorio_sintese_data),
         produto_final_data = convert_date_column(produto_final_data),
         atualizacao_data = convert_date_column(atualizacao_data),
         tempo_preparacao = calc_day_interval(data_contribuicao_inicio, data_contribuicao_fim),
         primeira_reposta = map2_chr(rac_data, relatorio_sintese_data, calc_first_response),
         tempo_de_resposta = calc_day_interval(data_contribuicao_fim, primeira_reposta)
      )

mean_tempo_preparacao <- mean(data$tempo_preparacao, na.rm = TRUE)
median_tempo_preparacao <- median(data$tempo_preparacao, na.rm = TRUE)

tempo_preparacao_agencias_count <- data %>%
  filter(!is.na(tempo_preparacao) & tempo_preparacao > 0) %>%
  group_by(agencia) %>%
  summarize(n = n(), position = quantile(tempo_preparacao, probs = 0.75)) %>%
  mutate(text = paste0("N = ", n))

tempo_preparacao_agencias_ano_count <- data %>%
  mutate(year_range = case_when(
    ano < 2021 ~ "2002-2020",
    TRUE ~ "2021-2022"
  )) %>%
  filter(!is.na(tempo_preparacao)) %>%
  group_by(agencia, year_range) %>%
  summarize(n = n(), position = quantile(tempo_preparacao, probs = 0.75)) %>%
  mutate(text = paste0("N = ", n))

tempo_resposta_agencias_objetivo_count <- data %>%
  filter(objetivo_participacao %in% c("Normas e regulamentos", "Atos e contratos")) %>%
  filter(!is.na(tempo_de_resposta) & tempo_de_resposta > 0) %>%
  group_by(objetivo_participacao, agencia) %>%
  summarize(n = n(), position = quantile(tempo_de_resposta, probs = 0.75)) %>%
  mutate(text = paste0("N = ", n))

tempo_resposta_agencias_count <- data %>%
  filter(!is.na(tempo_de_resposta) & tempo_de_resposta > 0) %>%
  group_by(agencia) %>%
  summarize(n = n(), position = quantile(tempo_de_resposta, probs = 0.75)) %>%
  mutate(text = paste0("N = ", n))

ag_counts_by_year <- data %>%
  group_by(agencia, ano) %>%
  summarize(n = n())

ag_counts_by_type <- data %>%
  filter(!instrumento_participacao %in% c(NA, "N/D", "N/A", "N/C")) %>%
  group_by(agencia, instrumento_participacao) %>%
  summarize(n = n()) %>%
  mutate(perc = n / sum(n),
         text = ifelse(perc < 0.05, NA, paste0(n, " (", formater_percent(perc), ")")),
         type = case_when(
           instrumento_participacao == "PNP" ~ "Documental",
           instrumento_participacao == "PP" ~ "Presencial",
           TRUE ~ "Ambos"
  ) %>% factor(ordered = TRUE, levels = c("Presencial", "Documental", "Ambos")))

ag_counts_by_type_objective <- data %>%
  filter(objetivo_participacao %in% c("Normas e regulamentos", "Atos e contratos")) %>%
  filter(!instrumento_participacao %in% c(NA, "N/D", "N/A", "N/C")) %>%
  group_by(objetivo_participacao, agencia, instrumento_participacao) %>%
  summarize(n = n()) %>%
  mutate(perc = n / sum(n),
         text = ifelse(perc < 0.05, NA, paste0(n, " (", formater_percent(perc), ")")),
         type = case_when(
           instrumento_participacao == "PNP" ~ "Documental",
           instrumento_participacao == "PP" ~ "Presencial",
           TRUE ~ "Ambos"
         ) %>% factor(ordered = TRUE, levels = c("Presencial", "Documental", "Ambos")))

ag_counts_by_objective <- data %>%
  filter(!objetivo_participacao %in% c(NA, "N/D", "N/A", "N/C")) %>%
  mutate(objetivo_participacao = ifelse(objetivo_participacao %in% c("Atos e contratos", "Normas e regulamentos"), objetivo_participacao, "Outros")) %>%
  group_by(agencia, objetivo_participacao) %>%
  summarize(n = n()) %>%
  mutate(perc = n / sum(n),
         text = ifelse(perc < 0.05, NA, paste0(n, " (", formater_percent(perc), ")")),
         objective = fct_reorder(objetivo_participacao, perc))

# Graphs ----

subtitle <- "Consultas e Audiências Públicas das Agências Reguladoras Federais de Transportes (2002-2022)"
caption <- "**Fonte**: Pesquisa sobre Mecanismos de Participação Social do &nbsp;&nbsp;&nbsp;<em style=\"color: white;\">.</em> <br/> Projeto Regulação em Números (https:&#47;&#47;bit.ly/3Q5WzbM\\)"

theme_set({{theme_bw() + theme(plot.caption = element_markdown(), legend.position = "bottom")}})

## Total ----

data %>%
  count(agencia) %>%
  ggplot(aes(x = agencia, y = n, label = n)) +
  geom_col(fill = fgv_palette$secondary) +
  geom_text(nudge_y = 8, color = fgv_palette$secondary) +
  labs(title = "Volume total de consultas e audiências públicas por agência",
       subtitle = subtitle, caption = caption,
       x = "\nAgência", y = "Número de consultas e audiências públicas\n")


save_plot("total.png")

## Evolution ----

ag_counts_by_year %>%
  ggplot(aes(x = as.character(ano), y = n, group = agencia, color = agencia)) +
  geom_point(alpha = 0.8) +
  geom_line(alpha = 0.8) +
  scale_color_manual(values = c(fgv_palette$blues1, color_ramp_fgv(6)[c(1, 4)])) +
  scale_y_continuous(breaks = seq(0, 40, 10), limits = c(0, 41)) +
  labs(title = "Uso de consultas e audiências públicas ao longo do tempo",
       subtitle = subtitle, caption = caption,
       x = "\nAno", y = "Número de consultas e audiências públicas\n", color = "Agência: ")

save_plot("evolution.png")

## Type ----

ag_counts_by_type %>%
  ggplot(aes(x = agencia, y = n, fill = type, label = text)) +
  geom_col(position = "fill") +
  geom_text(position = position_fill(vjust = 0.5), color = "white", size = 3.5) +
  scale_fill_manual(values = c(color_ramp_fgv(6)[c(1, 4)], fgv_palette$blues1)) +
  scale_y_continuous(labels = label_percent()) +
  labs(title = "Modo de partcipação social por agência",
       subtitle = subtitle, caption = caption,
       x = "\nAno", y = "Percentual das consultas e audiências públicas\n", fill = "Modo de Participação: ") +
  guides(fill = guide_legend(reverse = TRUE))

save_plot("type.png")

ag_counts_by_type_objective %>%
  ggplot(aes(x = agencia, y = n, fill = type, label = text)) +
  geom_col(position = "fill") +
  geom_text(position = position_fill(vjust = 0.5), color = "white", size = 3.5) +
  scale_fill_manual(values = c(color_ramp_fgv(6)[c(1, 4)], fgv_palette$blues1)) +
  scale_y_continuous(labels = label_percent()) +
  labs(title = "Modo de partcipação social por agência",
       subtitle = subtitle, caption = caption,
       x = "\nAno", y = "Percentual das consultas e audiências públicas\n", fill = "Modo de Participação: ") +
  guides(fill = guide_legend(reverse = TRUE)) +
  facet_wrap(~objetivo_participacao)

save_plot("type_objective.png")

## Objective ----

ag_counts_by_objective %>%
  ggplot(aes(x = agencia, y = n, fill = objective, label = text)) +
  geom_col(position = "fill") +
  geom_text(position = position_fill(vjust = 0.5), color = "white", size = 3.5) +
  scale_fill_manual(values = c(color_ramp_fgv(6)[c(1, 4)], fgv_palette$blues1)) +
  scale_y_continuous(labels = label_percent()) +
  labs(title = "Objetivo da partcipação social, por agência",
       subtitle = subtitle, caption = caption,
       x = "\nAno", y = "Percentual das consultas e audiências públicas\n", fill = "Objetivo: ") +
  guides(fill = guide_legend(reverse = TRUE))

save_plot("objective.png")


## Prep Time ----

data %>%
  filter(!is.na(tempo_preparacao) & tempo_preparacao > 0) %>%
  ggplot(aes(x = agencia, y = Winsorize(tempo_preparacao))) +
  geom_boxplot(color = fgv_palette$background, fill = "grey95") +
  stat_summary(fun=mean, geom="point", shape=20, size=6, color= fgv_palette$secondary, fill = fgv_palette$secondary) +
  geom_text(data = tempo_preparacao_agencias_count, aes(x = agencia, y = position + 1.3, label = text, hjust = -0.1), color = fgv_palette$background, size = 3) +
  labs(title = "Tempo de preparação por agência",
       subtitle = subtitle, caption = caption,
       x = "\nAgência", y = "Tempo de preparação (em dias)\n")

save_plot("prep_time.png")

## Response Time ----

data %>%
  filter(!is.na(tempo_de_resposta) & tempo_de_resposta > 0) %>%
  ggplot(aes(x = agencia, y = Winsorize(tempo_de_resposta))) +
  geom_boxplot(color = fgv_palette$background, fill = "grey95") +
  stat_summary(fun=mean, geom="point", shape=20, size=6, color= fgv_palette$secondary, fill = fgv_palette$secondary) +
  geom_text(data = tempo_resposta_agencias_count, aes(x = agencia, y = position + 8, label = text, hjust = -0.1), color = fgv_palette$background, size = 3) +
  labs(title = "Tempo de resposta por agência",
       subtitle = subtitle, caption = caption,
       x = "\nAgência", y = "Tempo de resposta (em dias)\n")

save_plot("resp_time.png")

data %>%
  filter(objetivo_participacao %in% c("Normas e regulamentos", "Atos e contratos")) %>%
  filter(!is.na(tempo_de_resposta) & tempo_de_resposta > 0) %>%
  ggplot(aes(x = agencia, y = Winsorize(tempo_de_resposta))) +
  geom_boxplot(color = fgv_palette$background, fill = "grey95") +
  stat_summary(fun=mean, geom="point", shape=20, size=6, color= fgv_palette$secondary, fill = fgv_palette$secondary) +
  geom_text(data = filter(tempo_resposta_agencias_objetivo_count, objetivo_participacao %in% c("Normas e regulamentos", "Atos e contratos")), 
            aes(x = agencia, y = position + 10, label = text, hjust = -0.1), color = fgv_palette$background, size = 3) +
  labs(title = "Tempo de resposta por agência, por objetivo",
       subtitle = subtitle, caption = caption,
       x = "\nAgência", y = "Tempo de resposta (em dias)\n") +
  facet_wrap(~objetivo_participacao)

save_plot("resp_time_objective.png")

# Counts ----

data %>%
  filter(!is.na(tempo_preparacao)) %>%
  group_by(agencia) %>%
  summarize(
    mean_t = mean(tempo_preparacao),
    sdt = sd(tempo_preparacao),
    q0 = min(tempo_preparacao),
    q25 = quantile(tempo_preparacao, probs = 0.25),
    q50 = median(tempo_preparacao),
    q75 = quantile(tempo_preparacao, probs = 0.75),
    q100 = max(tempo_preparacao)
  )


data %>%
  filter(!is.na(tempo_de_resposta) & tempo_de_resposta > 0) %>%
  group_by(agencia) %>%
  summarize(
    mean_t = mean(tempo_de_resposta),
    sdt = sd(tempo_de_resposta),
    q0 = min(tempo_de_resposta),
    q25 = quantile(tempo_de_resposta, probs = 0.25),
    q50 = median(tempo_de_resposta),
    q75 = quantile(tempo_de_resposta, probs = 0.75),
    q100 = max(tempo_de_resposta)
  )
