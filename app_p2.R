library(tidyr)
library(dplyr)
library(tibble)

#-------------- 資料讀取分類 start -------------------------------

clean_fh1 <- read_csv("clean_fh1.csv")
table <- as_tibble(clean_fh1)


#分類

#酸，材料有包含檸檬/酸橙/酸相關的sour欄為1
sour <- table %>%
  filter(grepl("((.+)?\\sLemon\\s(.+)?)|((.+)?\\sSour\\s(.+)?)|((.+)?\\sLime\\s(.+)?)",table$cocktails_name)
         |grepl("((.+)?Lemon(.+)?)|((.+)?Lime(.+)?)",table$ingredient_1)
         |grepl("((.+)?Lemon(.+)?)|((.+)?Lime(.+)?)",table$ingredient_2)
         |grepl("((.+)?Lemon(.+)?)|((.+)?Lime(.+)?)",table$ingredient_3)
         |grepl("((.+)?Lemon(.+)?)|((.+)?Lime(.+)?)",table$ingredient_4)
         |grepl("((.+)?Lemon(.+)?)|((.+)?Lime(.+)?)",table$ingredient_5)
         |grepl("((.+)?Lemon(.+)?)|((.+)?Lime(.+)?)",table$ingredient_6)
         |grepl("((.+)?Lemon(.+)?)|((.+)?Lime(.+)?)",table$ingredient_7)
         |grepl("((.+)?Lemon(.+)?)|((.+)?Lime(.+)?)",table$ingredient_8)
         |grepl("((.+)?Lemon(.+)?)|((.+)?Lime(.+)?)",table$ingredient_9)) %>%
  mutate(sour=1) %>%
  select(cocktails_name,sour)
#合併
table <-merge(table, sour, by = "cocktails_name", all = T)

#甜，材料有包含糖漿/糖/蜂蜜/巧克力/甜相關的sweet欄為1
sweet <- table %>%
  filter(grepl("((.+)?\\sSyrup(.+)?)|((.+)?\\sSweet(.+)?)|((.+)?\\sHoney(.+)?)|((.+)?\\sSugar(.+)?)|((.+)?\\sChocolate(.+)?)"
               ,table$cocktails_name)
         |grepl("((.+)?Syrup(.+)?)|((.+)?Honey(.+)?)|((.+)?Sugar(.+)?)|((.+)?Chocolate(.+)?)",table$ingredient_1)
         |grepl("((.+)?Syrup(.+)?)|((.+)?Honey(.+)?)|((.+)?Sugar(.+)?)|((.+)?Chocolate(.+)?)",table$ingredient_2)
         |grepl("((.+)?Syrup(.+)?)|((.+)?Honey(.+)?)|((.+)?Sugar(.+)?)|((.+)?Chocolate(.+)?)",table$ingredient_3)
         |grepl("((.+)?Syrup(.+)?)|((.+)?Honey(.+)?)|((.+)?Sugar(.+)?)|((.+)?Chocolate(.+)?)",table$ingredient_4)
         |grepl("((.+)?Syrup(.+)?)|((.+)?Honey(.+)?)|((.+)?Sugar(.+)?)|((.+)?Chocolate(.+)?)",table$ingredient_5)
         |grepl("((.+)?Syrup(.+)?)|((.+)?Honey(.+)?)|((.+)?Sugar(.+)?)|((.+)?Chocolate(.+)?)",table$ingredient_6)
         |grepl("((.+)?Syrup(.+)?)|((.+)?Honey(.+)?)|((.+)?Sugar(.+)?)|((.+)?Chocolate(.+)?)",table$ingredient_7)
         |grepl("((.+)?Syrup(.+)?)|((.+)?Honey(.+)?)|((.+)?Sugar(.+)?)|((.+)?Chocolate(.+)?)",table$ingredient_8)
         |grepl("((.+)?Syrup(.+)?)|((.+)?Honey(.+)?)|((.+)?Sugar(.+)?)|((.+)?Chocolate(.+)?)",table$ingredient_9)) %>%
  mutate(sweet=1) %>%
  select(cocktails_name,sweet)
#合併
table <-merge(table, sweet, by = "cocktails_name", all = T)

#苦，材料有包含苦精相關的bitter欄為1
bitter <- table %>%
  filter(grepl("(.+)?\\sBitter(.+)?",table$cocktails_name)|grepl("(.+)?Bitter(.+)?",table$ingredient_1)|grepl("(.+)?Bitter(.+)?",table$ingredient_2)|grepl("(.+)?Bitter(.+)?",table$ingredient_3)|grepl("(.+)?Bitter(.+)?",table$ingredient_4)|grepl("(.+)?Bitter(.+)?",table$ingredient_5)|grepl("(.+)?Bitter(.+)?",table$ingredient_6)|grepl("(.+)?Bitter(.+)?",table$ingredient_7)|grepl("(.+)?Bitter(.+)?",table$ingredient_8)|grepl("(.+)?Bitter(.+)?",table$ingredient_9)) %>%
  mutate(bitter=1) %>%
  select(cocktails_name,bitter)
#合併
table <-merge(table, bitter, by = "cocktails_name", all = T)

#薄荷，材料有包含薄荷的mint欄為1
mint <- table %>%
  filter(grepl("(.+)?\\sMint(.+)?",table$cocktails_name)|grepl("(.+)?\\sMint(.+)?",table$ingredient_1)|grepl("(.+)?\\sMint(.+)?",table$ingredient_2)|grepl("(.+)?\\sMint(.+)?",table$ingredient_3)|grepl("(.+)?\\sMint(.+)?",table$ingredient_4)|grepl("(.+)?\\sMint(.+)?",table$ingredient_5)|grepl("(.+)?\\sMint(.+)?",table$ingredient_6)|grepl("(.+)?\\sMint(.+)?",table$ingredient_7)|grepl("(.+)?\\sMint(.+)?",table$ingredient_8)|grepl("(.+)?\\sMint(.+)?",table$ingredient_9)) %>%
  mutate(mint=1) %>%
  select(cocktails_name,mint)
#合併
table <-merge(table, mint, by = "cocktails_name", all = T)

#把table中的NA值取代為0
table[is.na(table)] <- 0





#--------------------資料讀取分類 end------------------------------




library(shiny)
library(readr)
library(ggplot2)
library(DT)
library(shinythemes)

#-------------------shiny ui start---------------------------------

cocktails_flavor <- table %>% select(cocktails_name,sour,sweet,bitter,mint)

ui <- fluidPage(theme = shinytheme("simplex"),
  # tags$head(
  tags$style("body{background-color:#EDBC7A}"),
  tags$style("td{background-color:#F2F4EF}"),
  tags$style("form.well{background-color:#F2F4EF}"),
  tags$style("#DataTables_Table_0_length{color: white}"),
  tags$style("select{color: black}"),
  # font-family: BentonSans Book;background-color: black;
  # ),
  #ui標題
  
  titlePanel(strong("Recommend the Cocktails to You")),
  br(),
  
  #將版面分成兩塊(左)
  sidebarLayout(
    sidebarPanel(
      width = 3,
      #複選input
      checkboxGroupInput("flavor", label = strong("Choose the flavor you like"), choices = names(cocktails_flavor) ,selected = names(cocktails_flavor) ),
      #下拉選項
    
        
        #下拉選項
       
               # selectInput("abv",
               #             "能飲一杯無?",
               #             c("All",
               #               unique(as.character(table$classified_by_abv)))),
               # 
               # 
               # selectInput("drink",
               #             "千杯不醉?",
               #             c("All",
               #               unique(as.character(table$long_and_short_drink)))),
               # 
        # verbatimTextOutput("summary"),
        
        #將版面分成兩塊(右)
        
     
      
      # Input: Select a dataset ----
     
      #心情拉桿input
    #   sliderInput("table", "Your mood : 1(very bad)~10(very good)", value = 5, min = 0, max = 10),
    ),
    # 
    # verbatimTextOutput("summary"),
    
    #將版面分成兩塊(右)
    mainPanel(
  
      DT::dataTableOutput("cocktails")
    )
  )
)

#-------------------shiny ui end---------------------------------



#-------------------shiny server start---------------------------

cocktails_name <- cocktails_flavor %>% select(cocktails_name)

server <- function(input, output, session) {
  # dataset <- reactive({
  #   get(input$dataset, cocktails_flavor)
  # })
  
  # output$summary <- renderPrint({
  #   summary(dataset())
  # })
  
  #顯示datatable
  output$cocktails <- DT::renderDataTable({
    DT::datatable(
     
     
      #  if (input$abv != "All") {
      #    data1 <- data1[ data1$classified_by_abv == input$abv,]
      #  }
      # data1
      
     table[, input$flavor],filter = 'top',options = list(orderClasses = TRUE))
    #  data1 <-table %>% select(cocktails_name,classified_by_abv,abv)
    # if (input$abv != "All") {
    #   data1 <- data1[  data1$classified_by_abv == input$abv,]
    #   
    # }
    # if (input$drink != "All") {
    #   data1 <-  data1[  data1$long_and_short_drink == input$drink,]
    #   
    # }
    
      #依勾選可將該欄刪除不比較 
      #filter="top"可做為flavor為 1 或 0 的條件選擇
      #orderClasses == True 使點選該行時，會有灰底框住
      
      #table[, input$flavor],filter = 'top',options = list(orderClasses = TRUE))
    
     
    
  })
  
}

#-------------------shiny server end-----------------------------

#啟動shiny
shinyApp(ui=ui,server = server)

