# Libraries ---------------------------------------------------------------
library(shinydashboard)
library(magrittr)
library(ggplot2)
# library(sf)
# Import -----------------------------------------------------------------------

# devtools::isntall_github("abjur/rjn")
# devtools::isntall_github("abjur/abjData")
# u <- stringr::str_glue("http://www.cnj.jus.br/files/conteudo/arquivo/2017/09/",
#                        "fa42794ccca590dd4d53fbb3d9498d2d.zip")
# 
# path <- "tmp_cnj"
# fs::dir_create(path)
# httr::GET(u, httr::write_disk("tmp_cnj/z.zip", overwrite = TRUE))
# jn <- rjn:::jn_read("tmp_cnj") %>%
#   dplyr::select(-nome) %>%
#   tidyr::unnest() %>%
#   dplyr::filter(justica == "Estadual", sigla != "TJ") %>%
#   tidyr::separate(dsc_tribunal, c("tj", "ufn"), sep = " - ") %>%
#   dplyr::select(ano_cnj = ano, uf_abrangida, justica, ufn, uf_sede,
#                 dplyr::starts_with("cn")) %>%
#   dplyr::select_if(~any(.x != "nd")) %>%
#   dplyr::distinct(ano_cnj, uf_sede, .keep_all = TRUE) %>%
#   dplyr::mutate_at(dplyr::vars(dplyr::starts_with("cn")),
#                    dplyr::funs(dplyr::if_else(.=="nd", NA_character_, .))) %>%
#   dplyr::mutate_at(dplyr::vars(dplyr::starts_with("cn")), as.integer)
# readr::write_rds(jn, "data/jn.rds")
# jn <- readr::read_rds("data/jn.rds")

# Tidy -------------------------------------------------------------------------
# d_plt <- abjData::pnud_uf %>% 
#   dplyr::filter(ano == 2010) %>% 
#   dplyr::select(ano, ufn, dplyr::starts_with("idhm"), popt) %>% 
#   dplyr::inner_join(jn, "ufn")
# readr::write_rds(d_plt, "data/d_plt.rds")
d_plt <- readr::read_rds("data/d_plt.rds")

# df_uf <- ufshape::df_uf
# Visualize --------------------------------------------------------------------

# all_cn <- d_plt %>%
#   dplyr::select(dplyr::starts_with("cn")) %>%
#   names()
# cnj_choices <- "~/Downloads/dc09b5c270dc3227814c8ffe5c59db4c (1).xlsx" %>%
#   readxl::read_excel() %>%
#   dplyr::select(nm = X__5, txt = X__6) %>%
#   dplyr::slice(3:24) %>%
#   dplyr::mutate(
#     nm = tolower(nm),
#     txt = stringr::str_remove(txt, "^[^–]+– "),
#     txt = stringr::str_remove(txt, ":.+$")
#   ) %>%
#   dplyr::filter(nm %in% all_cn) %>%
#   with(purrr::set_names(nm, txt))
# readr::write_rds(cnj_choices, "data/cnj_choices.rds")
cnj_choices <- readr::read_rds("data/cnj_choices.rds") %>% 
  c("Casos Novos" = "cn")
pnud_choices <- c("IDH" = "idhm",
                  "IDH-Educação" = "idhm_e",
                  "IDH-Renda" = "idhm_r",
                  "IDH-Longevidade" = "idhm_l")

# UI ----------------------------------------------------------------------
ui <- dashboardPage(
  header = dashboardHeader(title = tags$a(tags$img(src = "abj.png"),
                                          href = "https://abj.org.br",
                                          target = "_blank")),
  skin = "black",
  title = "Litigiosidade e Desenvolvimento",
  
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem(
        text = "Justiça em Números",
        tabName = "visao_geral",
        icon = shiny::icon("globe")
      ),
      menuItem(
        text = "Sobre",
        tabName = "sobre",
        icon = shiny::icon("info")
      ),
      tags$hr(),
      selectInput(
        inputId = "x",
        label = "Desenvolvimento (eixo X)",
        choice = pnud_choices
      ),
      selectInput(
        inputId = "y",
        label = "Litigiosidade (eixo Y)",
        choice = cnj_choices,
        selected = "cn"
      ),
      selectInput(
        inputId = "ano",
        label = "Ano",
        choice = 2009:2016, 
        selected = 2016
      )    
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "visao_geral",
        fluidRow(
          box(
            title = "Gráfico Litigiosidade e Desenvolvimento",
            width = 6,
            height = 700,
            plotOutput(outputId = "plot", height = 600)
          )
        )
      ),
      tabItem(
        tabName = "sobre",
        fluidRow(
          box(
            title = "Sobre",
            width = 8,
            height = 150,
            tags$p("Fonte: Justiça em Números e PNUD."),
            tags$p("Elaborado pela Associação Brasileira de Jurimetria (ABJ)")
          )
        )
      )
    )
  )
)

# Server ------------------------------------------------------------------
server <- function(input, output, session) {
  
  df_disp <- reactive({
     dplyr::filter(d_plt, ano_cnj == input$ano)
  })
  output$plot <- renderPlot({
    
    corr <- df_disp() %>% 
      dplyr::summarise(corr = cor(!!sym(input$x), !!sym(input$y)),
                       corr = scales::percent(corr))

    p <- df_disp() %>% 
      ggplot(aes(x = !!sym(input$x), 
                 y = !!sym(input$y) / popt * 1e5, 
                 label = uf_sede)) +
      geom_smooth(method = "lm", se = FALSE, size = .4, linetype = 2) +
      geom_text(aes(colour = uf_abrangida), fontface = "bold", size = 5) +
      theme_light(14) +
      scale_colour_brewer(palette = "Dark2") +
      labs(x = "IDH Estadual",
           y = "Casos novos / 100.000 habitantes",
           title = glue::glue("Correlação: ", corr$corr),
           colour = "",
           subtitle = glue::glue("{names(cnj_choices[cnj_choices==input$y])}",
                                 " / 100.000 habitantes ({input$ano}) vs \n",
                                 "{names(pnud_choices[pnud_choices==input$x])}",
                                 " das UFs (2010)")) +
      theme(legend.position = "bottom",
            legend.background = element_blank(),
            aspect.ratio = 3/4)
    
    p
  })
}

shinyApp(ui = ui, server = server)

