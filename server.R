library(shiny)
library(readxl)
library(ggplot2)
library(ggpubr)
library(shinythemes)
library(plotly)
library(ggthemes)
library(DT)
library(reshape)
library(reshape2)
library(qdapTools)
library(tidyverse)
library(ggmosaic)

Tempo_marks_groupings<- read_excel("Tempo marks_groupings.xlsx")
Arias_change_fake  <- read_excel("Arias_change_fake.xlsx")
Arias_proportions_fake <- read_excel("Arias_proportions_fake.xlsx")
Arias_scoring_fake <- read_excel("Arias_scoring_fake.xlsx")
shinyServer(function(input, output) {
  
  
  # Download simulated data
  wd=getwd()
  filesToSave <- c("Arias_change_fake.xlsx", "Arias_proportions_fake.xlsx", "Arias_scoring_fake.xlsx") #List to hold paths to your files in shiny
  
  filesToSave_full_path=path.expand(paste0(wd,"\\www\\",filesToSave))
  
  
  output$fakedata <- downloadHandler(
    
    filename = 'data.zip',
    content = function(fname) {
      tmpdir <- tempdir()
      lapply(filesToSave_full_path,function(i) file.copy(i,tmpdir))
      #setwd(tmpdir)
      zip(zipfile=fname, files = filesToSave)
      
      setwd(wd)
      unlink(tmpdir)
      
    },
    contentType = "application/zip"
  
  )
  
    
  ## Data loading and transformations
  
  mydata <- reactive({
    infile_ch <-req(input$file_ch)
    data_ch <- read_excel(infile_ch$datapath, skip = 2)
    infile_prop <-req(input$file_prop)
    data_prop <- read_excel(infile_prop$datapath, skip = 1)
    infile_sco<-req(input$file_sco)
    data_sco <- read_excel(infile_sco$datapath, skip = 1)
    
    data <-merge(data_ch, data_prop)
    rm(data_ch)
    rm(data_prop)
    data_sco <- select(data_sco, ID:Form, fl, ob, eh, hn, tp)
    data <-merge(data,data_sco)
    rm(data_sco)
    data <-data[!is.na(data$ID),]
    data$tp= as.numeric(data$tp)
    data$ob= as.numeric(data$ob)
    data$fl= as.numeric(data$fl)
    data$hn= as.numeric(data$hn)
    data$eh= as.numeric(data$eh)
    data <- data%>%
      mutate(Rit1 = if_else(is.na(Rit1), 0, Rit1),
             a1 = if_else(is.na(a1), 0, a1),
             Rit2 = if_else(is.na(Rit2), 0, Rit2),
             a2 = if_else(is.na(a2), 0, a2),
             Rit3 = if_else(is.na(Rit3), 0, Rit3),
             `Rit1'`=if_else(is.na(`Rit1'`), 0, `Rit1'`), 
             `a1'`=if_else(is.na(`a1'`), 0, `a1'`),
             `Rit2'`=if_else(is.na(`Rit2'`), 0, `Rit2'`),
             `a2'`=if_else(is.na(`a2'`), 0, `a2'`),
             `Rit3'`=if_else(is.na(`Rit3'`), 0, `Rit3'`),
             Rit4=if_else(is.na(Rit4), 0, Rit4),
             tp = if_else(is.na(tp),0,tp),
             ob = if_else(is.na(ob),0,ob),
             fl = if_else(is.na(fl),0,fl),
             eh = if_else(is.na(eh),0,eh),
             hn = if_else(is.na(hn),0,hn)
             )
    
     
    data <-select(data, -Type...12, -c(A...17:`Type of change...37`), -A...40,-B...41,-c(`Type of change...43`:`Within B...45`),-c(Towards...49:Type...56))
    
    names(data)[names(data)=="A...14"] <- "Key_A"
    names(data)[names(data)=="B...15"] <- "Key_B"
    names(data)[names(data)=="A...38"] <- "TSig_A"
    names(data)[names(data)=="B...39"] <- "TSig_B"
    names(data)[names(data)=="Y/N...42"] <- "Time_Change"
    names(data)[names(data)=="A...46"] <- "Temp_A"
    names(data)[names(data)=="B...47"] <- "Temp_B"
    names(data)[names(data)=="Y/N...48"] <- "Temp_Change"
    names(data)[names(data)=="Total cc"] <- "Total_mm"
    names(data)[names(data)=="tp"] <- "tpt"
    names(data)[names(data)=="A1"] <- "A"
    names(data)[names(data)=="Type"] <- "ChType"
    floor_decade    = function(value){ return(value - value %% 10) }
    
    data = data %>%
      mutate(Temp_B = ifelse(Temp_Change == "N", 
                             Temp_A, Temp_B),
             Key_B = ifelse(Change == "N",
                            Key_A,Key_B),
             TSig_B = ifelse(Time_Change == "N",
                             TSig_A,TSig_B),
             pulsos_A = case_when(
               endsWith(TSig_A, "C") ~ 4,
               endsWith(TSig_A, "C|") ~ 2,
               endsWith(TSig_A, "2/4") ~ 2,
               endsWith(TSig_A, "3/4") ~ 3,
               endsWith(TSig_A, "3/8") ~ 1,
               endsWith(TSig_A, "6/8") ~ 2
             ),
             pulsos_B = case_when(
               endsWith(TSig_B, "C") ~ 4,
               endsWith(TSig_B, "C|") ~ 2,
               endsWith(TSig_B, "2/4") ~ 2,
               endsWith(TSig_B, "3/4") ~ 3,
               endsWith(TSig_B, "3/8") ~ 1,
               endsWith(TSig_B, "6/8") ~ 2
             ),
             
             Decade = floor_decade(as.numeric(Year))
      )
    
    data$Simp_Temp_A = lookup(data$Temp_A, Tempo_marks_groupings$`Tempo mark`, Tempo_marks_groupings$`Grouping 1`)
    data$Simp_Temp_B = lookup(data$Temp_B, Tempo_marks_groupings$`Tempo mark`, Tempo_marks_groupings$`Grouping 1`)
    data$index_A = lookup(data$Temp_A, Tempo_marks_groupings$`Tempo mark`, Tempo_marks_groupings$index)
    data$index_B = lookup(data$Temp_B, Tempo_marks_groupings$`Tempo mark`, Tempo_marks_groupings$index)
    
    data = data %>% mutate(
      Rit1_min = ((Rit1*pulsos_A)/90)/index_A,
      a1_min = ((a1*pulsos_A)/90)/index_A,
      Rit2_min = ((Rit2*pulsos_A)/90)/index_A,
      a2_min = ((a2*pulsos_A)/90)/index_A,
      Rit3_min = ((Rit3*pulsos_A)/90)/index_A,
      A_min = ((A*pulsos_A)/90)/index_A,
      B_min = ((B*pulsos_B)/90)/index_B,
      Ap_min= ((`A'`*pulsos_A)/90)/index_A,
      Total_min = A_min + B_min + Ap_min,
      A_per = A/Total_mm,
      B_per = B/Total_mm,
      Ap_per =`A'`/Total_mm,
      A_min_per = A_min/Total_min,
      B_min_per = B_min/Total_min,
      Ap_min_per = Ap_min/Total_min
    )
    
    data = data %>% mutate_if(is.numeric, round, digits=2)
    
    data$TSig_A = factor(data$TSig_A, levels = c("C", "4/4", "2/4", "2/2", "C|", "3/4", "3/8", "6/4", "6/8", "12/8", "9/8"))
    data$TSig_B = factor(data$TSig_B, levels = c("C", "4/4", "2/4", "2/2", "C|", "3/4", "3/8", "6/4", "6/8", "12/8", "9/8"))
    data$Key_A = factor(data$Key_A, levels = c("Db", "bb", "Ab", "f", "Eb", "c", "Bb", "g", "F", "d", "C", "a", "G", "e", "D", "b", "A", "f#", "E", "c#", "B", "G#"))
    data$Key_B = factor(data$Key_B, levels = c("Db", "bb", "Ab", "f", "Eb", "c", "Bb", "g", "F", "d", "C", "a", "G", "e", "D", "b", "A", "f#", "E", "c#", "B", "G#"))
    data$ChType = factor(data$ChType, levels = c("Senior ruler", "Female lover 1", "Male lover 1", "Female lover 2", "Male lover 2", "Female lover 3", "Male lover 3", "Antagonist", "Confidant", "Female lover 1,Male lover 1", "Female lover 1,Male lover 1,Antagonist", "Female lover 1,Male lover 2,Antagonist", "MAle lover 1,Female lover 1", "Female lover 1,Male lover 1,Male lover 2"))
    
    
    data$tpt= as.factor(data$tpt)
    data$ob= as.factor(data$ob)
    data$fl= as.factor(data$fl)
    data$hn= as.factor(data$hn)
    data$eh= as.factor(data$eh)
    data$Composer = as.factor(data$Composer)
    data$City = as.factor(data$City)
    data$Country = as.factor(data$Country)
    data$Character = as.factor(data$Character)
    data$ChType = as.factor(data$ChType)
    data$Form = as.factor(data$Form)
    data$TSig_A = as.factor(data$TSig_A)
    data$TSig_B = as.factor(data$TSig_B)
    data$Key_A = as.factor(data$Key_A)
    data$Key_B = as.factor(data$Key_B)
    data$Simp_Temp_A = as.factor(data$Simp_Temp_A)
    data$Simp_Temp_B = as.factor(data$Simp_Temp_B)
    data$Year = as.numeric(data$Year)
    
    data = data %>% mutate(
      tp = ifelse(tpt == "1","Yes","No"),
      ob = ifelse(ob == "1","Yes","No"),
      fl = ifelse(fl == "1","Yes","No"),
      eh = ifelse(eh == "1","Yes","No"),
      hn = ifelse(hn == "1","Yes","No")
    )
    
    data$tpt= as.factor(data$tpt)
    data$ob= as.factor(data$ob)
    data$fl= as.factor(data$fl)
    data$hn= as.factor(data$hn)
    data$eh= as.factor(data$eh)
    
    data <-filter(data, Form == "ABA" | Form == "Al segno" | Form == "Da capo" | Form == "ABC")

  })
  
  
  ## Plots for the variables 
  data1 <- reactive({
        input$allvariables
    })

  output$singleplot = renderPlot({
  data <- mydata()
  data$fl= as.factor(data$fl)
  data$ob = as.factor(data$ob)
  data$eh = as.factor(data$eh)
  data$hn = as.factor(data$hn)
  data$tpt = as.factor(data$tpt)
  data$TSig_A = factor(data$TSig_A, levels = c("C", "4/4", "2/4", "2/2", "C|", "3/4", "3/8", "6/4", "6/8", "12/8", "9/8"))
  data$TSig_B = factor(data$TSig_B, levels = c("C", "4/4", "2/4", "2/2", "C|", "3/4", "3/8", "6/4", "6/8", "12/8", "9/8"))
  data$Key_A = factor(data$Key_A, levels = c("Db", "bb", "Ab", "f", "Eb", "c", "Bb", "g", "F", "d", "C", "a", "G", "e", "D", "b", "A", "f#", "E", "c#", "B", "G#"))
  data$Key_B = factor(data$Key_B, levels = c("Db", "bb", "Ab", "f", "Eb", "c", "Bb", "g", "F", "d", "C", "a", "G", "e", "D", "b", "A", "f#", "E", "c#", "B", "G#"))
  data$ChType = factor(data$ChType, levels = c("Senior ruler", "Female lover 1", "Male lover 1", "Female lover 2", "Male lover 2", "Female lover 3", "Male lover 3", "Antagonist", "Confidant", "Female lover 1,Male lover 1", "Female lover 1,Male lover 1,Antagonist", "Female lover 1,Male lover 2,Antagonist", "MAle lover 1,Female lover 1", "Female lover 1,Male lover 1,Male lover 2"))
  
  cont_vars = select(data, Year,Total_mm:`A'`, Rit1_min:Total_min, A_per:Ap_min_per)
  cat_vars = select(data, Composer, City:ChType, Key_A, Key_B,TSig_A, TSig_B,fl:tpt, Simp_Temp_A, Simp_Temp_B)
  
    if(input$allvariables %in% names(cont_vars)){
      ggplot(data = data, aes_string(x = input$allvariables)) +
        geom_histogram(bins = 50, fill = "mediumturquoise", color="grey97") +
        theme_bw() +
        xlab(toString(input$allvariables)) +
        ylab("Total count")
    }
    
    else{
      data = drop_na(data, input$allvariables)
      ggplot(data, aes_string(input$allvariables)) +
        geom_bar(fill = "#E69F00") +
        theme_pubclean()+
        theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
    }
  })

  output$tablecount = renderPrint({
    data <- mydata()
    dat <- select(data, toString(input$allvariables))
    summary(dat)
})
  
  combined <- reactive({
      input$var_1
      input$var_2
      input$vari_1
      input$vari_2
      input$vari_3
    })
    
    
  output$CombinedPlot = renderPlot({
      data <- mydata()
      cont_vars = select(data, Year,Total_mm:`A'`, Rit1_min:Total_min, A_per:Ap_min_per)
      cat_vars = select(data, Composer, City:Form, TSig_A:Key_B,fl:tpt)
      
      if(input$var_1 %in% names(cont_vars) & input$var_2 %in% names(cont_vars)){
        ggplot(data=data, aes_string(input$var_1, input$var_2)) + 
          geom_point()+
          theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
      }
      else if (input$var_1 %in% names(cat_vars) & input$var_2 %in% names(cat_vars)){
        data = drop_na(data, input$var_1, input$var_2)
        ggplot(data) +
          geom_mosaic(aes(x = product(!!sym(input$var_1)), fill=!!sym(input$var_2)))+
          theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
        
      }
      else{
        data = drop_na(data, input$var_1, input$var_2)
        ggplot(data=data, aes_string(input$var_1, input$var_2)) + 
          geom_boxplot(aes_string(fill = input$var_2), color = "coral1", fill = "orange", alpha = 0.2)+  
          theme(legend.position="none")+ 
          theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
        
      }
    })
    
  output$Fillingplot = renderPlot({
      data <- mydata()
     
      cont_vars = select(data, Year,Total_mm:`A'`, Rit1_min:Total_min, A_per:Ap_min_per)
      cat_vars = select(data, Composer, City:Form, TSig_A:Key_B,fl:tpt)
      if(input$vari_1 %in% names(cont_vars) & input$vari_2 %in% names(cont_vars)){
        ggplot(data=data, aes_string(input$vari_1, input$vari_2, color = input$vari_3)) + 
          geom_point()+
          theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
      }
      else if (input$vari_1 %in% names(cat_vars) & input$vari_2 %in% names(cat_vars)){
        data = drop_na(data, input$vari_1, input$vari_2, input$vari_3)
        ggplot(data) +
          geom_mosaic(aes(x = product(!!sym(input$vari_1), !!sym(input$vari_2)), fill=!!sym(input$vari_2)))+
          theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
        
      }
      else{
        data = drop_na(data, input$vari_1, input$vari_2, input$vari_3)
        ggplot(data=data, aes_string(input$vari_1, input$vari_2, fill = input$vari_3)) + 
          geom_boxplot()+  
          facet_wrap(~get(input$vari_3))+
          theme(legend.position="none")+ 
          theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
        
      }
    })
    
    
   ## Regressions
    output$RegSum <- renderPrint({
      data<-mydata()
      if (input$IndVar2 == "No factor"){
        lm1 <- lm(reformulate(input$IndVar, input$DepVar), data = data)
        summary(lm1)
      }
      else {
        lm1 <- lm(reformulate(c(input$IndVar, input$IndVar2), input$DepVar), data = data)
        summary(lm1)
      }
      })
    output$Regression <-renderPlot({
      data <-mydata()
      if (input$IndVar2 == "No factor"){
        ggplot(data, aes_string(x = input$IndVar, y = input$DepVar)) +
          geom_point() +
          stat_smooth(method = "lm", se = FALSE) +
          
          
          theme_minimal()
      }
      else  {ggplot(data, aes_string(x = input$IndVar, y = input$DepVar, colour = input$IndVar2, fill = input$IndVar2)) +
          geom_point() +
          stat_smooth(method = "lm", se = FALSE) +
          
          
          theme_minimal() }
      
    })
    
    output$RegSum2 <- renderPrint({
      data<-mydata()
      lm2 <- lm(reformulate(c(input$IndVar_, input$IndVar2_), input$DepVar_), data = data)
      summary(lm2)})
    
    output$Regression2<- renderPlot({
      data<-mydata()
      mod <- lm(reformulate(c(input$IndVar_, input$IndVar2_), input$DepVar_), data = data)
      ggplot(mod, aes(.fitted, .resid)) +
        geom_point() +
        geom_hline(yintercept = 0) +
        geom_smooth(se = FALSE)+
        xlab("Fitted values")+
        ylab("Residuals")
    })
    

    ## my table
    output$mytable1 <- DT::renderDataTable({
      data<-mydata()
      data = select(data, input$show_vars,input$show_vars_1,input$show_vars_2, input$show_vars_3) 
        DT::datatable(data)
    })
    
   
    ## Plots for temporal evolution
    
    data2 <-reactive({input$numvar
      input$Facvar
    })
    output$Graph = renderPlot({
      data <-mydata()
      
      if(input$Facvar == 'Country'){
        data <- data %>% filter(Country == input$country)
      }
      else if (input$Facvar == 'Composer'){
        data <- data %>% filter(Composer == input$comp)
      }
      else if (input$Facvar == 'City'){
        data <- data %>% filter(City == input$city)
      }
      else if (input$Facvar == 'Character'){
        data <- data %>% filter(Character == input$charac)
      }
      else if (input$Facvar == 'ChType'){
        data <- data %>% filter(ChType == input$type)
      }
      else if (input$Facvar == 'Form'){
        data <- data %>% filter(Form == input$form)
      }
      else if (input$Facvar == 'TSig_A'){
        data <- data %>% filter(Tsig_A == input$timeA)
      }
      else if (input$Facvar == 'TSig_B'){
        data <- data %>% filter(Tsig_B == input$timeB)
      }
      else if (input$Facvar == 'Key_A'){
        data <- data %>% filter(Key_A == input$key_A)
      }
      else if (input$Facvar == 'Key_B'){
        data <- data %>% filter(Key_B == input$key_B)
      }
      else if (input$Facvar == 'fl'){
        data <- data %>% filter(fl == input$flute)
      }
      else if (input$Facvar == 'ob'){
        data <- data %>% filter(ob == input$oboe)
      }
      else if (input$Facvar == 'eh'){
        data <- data %>% filter(eh == input$enhorn)
      }
      else if (input$Facvar == 'hn'){
        data <- data %>% filter(hn == input$horn)
      }
      else if (input$Facvar == "tpt"){
        data <- data %>% filter(tpt == input$trumpet)
      }
      else{
        data <-data
      }
      data <-data[!is.na(data$index_A),]
      data <-data[!is.na(data$index_B),]
      data <-data[!is.na(data$pulsos_A),]
      data <-data[!is.na(data$pulsos_B),]
      data_C <- data %>% 
        select(Year,Total_mm:`A'`, Rit1_min:Total_min, A_per:Ap_min_per)%>%
        dplyr::group_by(Year)%>%
        summarize(
          Year,
          Rit1 = mean(Rit1),
          a1 = mean(a1),
          Rit2 = mean(Rit2), 
          a2 = mean(a2),
          Rit3 = mean(Rit3),
          A = mean(A),
          B= mean(B),
          Rit4 = mean(Rit4),
          `Rit1'` =mean(`Rit1'`),
          `a1'` = mean(`a1'`),
          `Rit2'` = mean(`Rit2'`),
          `a2'` = mean(`a2'`),
          `Rit3'` = mean(`Rit3'`),
          `A'` = mean(`A'`),
          A_per = mean(A/Total_mm),
          B_per = mean(B/Total_mm),
          Ap_per = mean(`A'`/Total_mm),
          Total_mm = mean(Total_mm),
          Rit1_min = mean(Rit1_min),
          a1_min = mean(a1_min),
          Rit2_min = mean(Rit2_min),
          a2_min = mean(a2_min),
          Rit3_min = mean(Rit3_min),
          A_min = mean(A_min),
          B_min = mean(B_min),
          Ap_min = mean(Ap_min),
          Total_min = mean(Total_min),
          A_min_per = mean(A_min/Total_min),
          B_min_per = mean(B_min/Total_min),
          Ap_min_per = mean(Ap_min/Total_min)
        )%>%
        distinct()
      df <- data_C
      ggplot(data = df, aes(x =as.integer(Year), y = get(input$numvar)))+
        geom_bar(stat = "identity", fill="steelblue")+
        theme(axis.text.x = element_text(angle = 90, size = 7))+
        scale_x_continuous(name = "Year",
                           breaks = seq(1720, 1810, by = 1),
                           labels = paste0(seq(1720, 1810, by = 1)),
                           expand = expansion(c(0.05, 0.05))) +
        xlab("Year") +
        ylab(toString(input$numvar))
        
    })
    
    output$Graph_2 = renderPlot({
      data <-mydata()
      
      if(input$Facvar == 'Country'){
        data <- data %>% filter(Country == input$country)
      }
      else if (input$Facvar == 'Composer'){
        data <- data %>% filter(Composer == input$comp)
      }
      else if (input$Facvar == 'City'){
        data <- data %>% filter(City == input$city)
      }
      else if (input$Facvar == 'Character'){
        data <- data %>% filter(Character == input$charac)
      }
      else if (input$Facvar == 'ChType'){
        data <- data %>% filter(ChType == input$type)
      }
      else if (input$Facvar == 'Form'){
        data <- data %>% filter(Form == input$form)
      }
      else if (input$Facvar == 'TSig_A'){
        data <- data %>% filter(Tsig_A == input$timeA)
      }
      else if (input$Facvar == 'TSig_B'){
        data <- data %>% filter(Tsig_B == input$timeB)
      }
      else if (input$Facvar == 'Key_A'){
        data <- data %>% filter(Key_A == input$key_A)
      }
      else if (input$Facvar == 'Key_B'){
        data <- data %>% filter(Key_B == input$key_B)
      }
      else if (input$Facvar == 'fl'){
        data <- data %>% filter(fl == input$flute)
      }
      else if (input$Facvar == 'ob'){
        data <- data %>% filter(ob == input$oboe)
      }
      else if (input$Facvar == 'eh'){
        data <- data %>% filter(eh == input$enhorn)
      }
      else if (input$Facvar == 'hn'){
        data <- data %>% filter(hn == input$horn)
      }
      else if (input$Facvar == "tpt"){
        data <- data %>% filter(tpt == input$trumpet)
      }
      else{
        data <-data
      }
      data <-data[!is.na(data$index_A),]
      data <-data[!is.na(data$index_B),]
      data <-data[!is.na(data$pulsos_A),]
      data <-data[!is.na(data$pulsos_B),]
      data_C <- data %>% 
        select(Decade,Total_mm:`A'`, Rit1_min:Total_min, A_per:Ap_min_per)%>%
        dplyr::group_by(Decade)%>%
        summarize(
          Decade,
          Rit1 = mean(Rit1),
          a1 = mean(a1),
          Rit2 = mean(Rit2), 
          a2 = mean(a2),
          Rit3 = mean(Rit3),
          A = mean(A),
          B= mean(B),
          Rit4 = mean(Rit4),
          `Rit1'` =mean(`Rit1'`),
          `a1'` = mean(`a1'`),
          `Rit2'` = mean(`Rit2'`),
          `a2'` = mean(`a2'`),
          `Rit3'` = mean(`Rit3'`),
          `A'` = mean(`A'`),
          A_per = mean(A/Total_mm),
          B_per = mean(B/Total_mm),
          Ap_per = mean(`A'`/Total_mm),
          Total_mm = mean(Total_mm),
          Rit1_min = mean(Rit1_min),
          a1_min = mean(a1_min),
          Rit2_min = mean(Rit2_min),
          a2_min = mean(a2_min),
          Rit3_min = mean(Rit3_min),
          A_min = mean(A_min),
          B_min = mean(B_min),
          Ap_min = mean(Ap_min),
          Total_min = mean(Total_min),
          A_min_per = mean(A_min/Total_min),
          B_min_per = mean(B_min/Total_min),
          Ap_min_per = mean(Ap_min/Total_min)
        )%>%
        distinct()
      df <- data_C
      ggplot(data = df, aes(x =as.integer(Decade), y = get(input$numvar)))+
        geom_bar(stat = "identity", fill="steelblue")+
        theme(axis.text.x = element_text(angle = 90, size = 7))+
        scale_x_continuous(name = "Year",
                           breaks = seq(1720, 1810, by = 10),
                           labels = paste0(seq(1720, 1810, by = 10)),
                           expand = expansion(c(0.05, 0.05))) +
        xlab("Decade") +
        ylab(toString(input$numvar))
      
    })
    
    
   ## Plots for proportions
    
    
    output$generalg= renderPlot({
      data <-mydata()
      if(input$condition == 'Country'){
        data <- data %>% filter(Country == input$country_1)
      }
      else if (input$condition == 'Composer'){
        data <- data %>% filter(Composer == input$comp_1)
      }
      else if (input$condition == 'City'){
        data <- data %>% filter(City == input$city_1)
      }
      else if (input$condition == 'Character'){
        data <- data %>% filter(Character == input$charac_1)
      }
      else if (input$condition == 'ChType'){
        data <- data %>% filter(ChType == input$type_1)
      }
      else if (input$condition == 'Form'){
        data <- data %>% filter(Form == input$form_1)
      }
      else if (input$condition == 'TSig_A'){
        data <- data %>% filter(Tsig_A == input$timeA_1)
      }
      else if (input$condition == 'TSig_B'){
        data <- data %>% filter(Tsig_B == input$timeB_1)
      }
      else if (input$condition == 'Key_A'){
        data <- data %>% filter(Key_A == input$key_A_1)
      }
      else if (input$condition == 'Key_B'){
        data <- data %>% filter(Key_B == input$key_B_1)
      }
      else if (input$condition == 'fl'){
        data <- data %>% filter(fl == input$flute_1)
      }
      else if (input$condition == 'ob'){
        data <- data %>% filter(ob == input$oboe_1)
      }
      else if (input$condition == 'eh'){
        data <- data %>% filter(eh == input$enhorn_1)
      }
      else if (input$condition == 'hn'){
        data <- data %>% filter(hn == input$horn_1)
      }
      else if(input$condition == 'tpt'){
        data <- data %>% filter(tpt == input$trumpet_1)
      }
      else{
        data <- data
      }
      data <-data[!is.na(data$index_A),]
      data <-data[!is.na(data$index_B),]
      data <-data[!is.na(data$pulsos_A),]
      data <-data[!is.na(data$pulsos_B),]
      
      df_ABA = select(data, Year, Total_mm, A,B,`A'`)
      df_ABA= df_ABA %>%
        group_by(Year) %>%
        summarize(
          `A'` = mean(`A'`)/mean(Total_mm),
          B= mean(B)/mean(Total_mm),
          A = mean(A)/mean(Total_mm)
        )
      df_ABA_m<- melt(df_ABA, id.vars = "Year")
      
      df_ABAm = select(data, Year, A_min, B_min,Ap_min, Total_min)
      df_ABAm= df_ABAm %>%
        group_by(Year) %>%
        summarize(
          `A'` = mean(Ap_min)/mean(Total_min),
          B= mean(B_min)/mean(Total_min),
          A = mean(A_min)/mean(Total_min)
        )
      df_ABAm_m<- melt(df_ABAm, id.vars = "Year")
      
      df_Ac = select(data, Year, Rit1:A)
      df_Ac= df_Ac %>%
        group_by(Year) %>%
        summarize(
          `Ritornello 3` = mean(Rit3)/mean(A),
          a2 =mean(a2)/mean(A),
          `Ritornello 2`= mean(Rit2)/mean(A),
          a1= mean(a1)/mean(A),
          `Ritornello 1` = mean(Rit1)/mean(A))
      df_Ac_m<- melt(df_Ac, id.vars = "Year")
      
      
      df_Am = select(data, Year, Rit1_min:A_min)
      df_Am= df_Am %>%
        group_by(Year) %>%
        summarize(
          `Ritornello 3` = mean(Rit3_min)/mean(A_min),
          a2 =mean(a2_min)/mean(A_min),
          `Ritornello 2`= mean(Rit2_min)/mean(A_min),
          a1= mean(a1_min)/mean(A_min),
          `Ritornello 1` = mean(Rit1_min)/mean(A_min))
      df_Am_m<- melt(df_Am, id.vars = "Year")
      
      if(input$pr == "ABA"){
        ggplot(data = df_ABA_m, aes(x = as.integer(Year) , y = value, fill = variable)) + 
          geom_bar(stat = 'identity') + 
          ggtitle("A, B & A' in measures") + 
          xlab('Year') + 
          ylab('%') + 
          scale_x_continuous(name = "Year",
                             breaks = seq(1720, 1810, by = 1),
                             labels = paste0(seq(1720, 1810, by = 1)),
                             expand = expansion(c(0.05, 0.05))) +
          coord_flip() + 
          guides(fill = guide_legend(reverse=TRUE))+
          theme_minimal()}
      else if(input$pr == "ABAmin"){
        ggplot(data = df_ABAm_m, aes(x = as.integer(Year) , y = value, fill = variable)) + 
          geom_bar(stat = 'identity') + 
          ggtitle("A, B & A' in minutes") + 
          xlab('Year') + 
          ylab('%') + 
          scale_x_continuous(name = "Year",
                             breaks = seq(1720, 1810, by = 1),
                             labels = paste0(seq(1720, 1810, by = 1)),
                             expand = expansion(c(0.05, 0.05))) +
          coord_flip() + 
          labs(fill = "Sections")+
          guides(fill = guide_legend(reverse=TRUE))+
          theme_minimal()}
      else if(input$pr == "Ac") {
        ggplot(data = df_Ac_m, aes(x = as.integer(Year) , y = value, fill = variable)) + 
          geom_bar(stat = 'identity') + 
          ggtitle("Subsections of A in measures") + 
          xlab('Year') + 
          ylab('%') + 
          scale_x_continuous(name = "Year",
                             breaks = seq(1720, 1810, by = 1),
                             labels = paste0(seq(1720, 1810, by = 1)),
                             expand = expansion(c(0.05, 0.05))) +
          coord_flip() + 
          labs(fill = "Sections")+
          guides(fill = guide_legend(reverse=TRUE))+
          theme_minimal()
      }
      else {
        ggplot(data = df_Am_m, aes(x = as.integer(Year) , y = value, fill = variable)) + 
          geom_bar(stat = 'identity') + 
          ggtitle("Subsections of A in minutes") + 
          xlab('Year') + 
          ylab('%') + 
          scale_x_continuous(name = "Year",
                             breaks = seq(1720, 1810, by = 1),
                             labels = paste0(seq(1720, 1810, by = 1)),
                             expand = expansion(c(0.05, 0.05))) +
          coord_flip() + 
          labs(fill = "Sections")+
          guides(fill = guide_legend(reverse=TRUE))+
          theme_minimal()
      }
      
    })
    
    output$generaldec= renderPlot({
      data <-mydata()
      if(input$condition == 'Country'){
        data <- data %>% filter(Country == input$country_1)
      }
      else if (input$condition == 'Composer'){
        data <- data %>% filter(Composer == input$comp_1)
      }
      else if (input$condition == 'City'){
        data <- data %>% filter(City == input$city_1)
      }
      else if (input$condition == 'Character'){
        data <- data %>% filter(Character == input$charac_1)
      }
      else if (input$condition == 'ChType'){
        data <- data %>% filter(ChType == input$type_1)
      }
      else if (input$condition == 'Form'){
        data <- data %>% filter(Form == input$form_1)
      }
      else if (input$condition == 'TSig_A'){
        data <- data %>% filter(Tsig_A == input$timeA_1)
      }
      else if (input$condition == 'TSig_B'){
        data <- data %>% filter(Tsig_B == input$timeB_1)
      }
      else if (input$condition == 'Key_A'){
        data <- data %>% filter(Key_A == input$key_A_1)
      }
      else if (input$condition == 'Key_B'){
        data <- data %>% filter(Key_B == input$key_B_1)
      }
      else if (input$condition == 'fl'){
        data <- data %>% filter(fl == input$flute_1)
      }
      else if (input$condition == 'ob'){
        data <- data %>% filter(ob == input$oboe_1)
      }
      else if (input$condition == 'eh'){
        data <- data %>% filter(eh == input$enhorn_1)
      }
      else if (input$condition == 'hn'){
        data <- data %>% filter(hn == input$horn_1)
      }
      else if(input$condition == 'tpt'){
        data <- data %>% filter(tpt == input$trumpet_1)
      }
      else{
        data <- data
      }
      data <-data[!is.na(data$index_A),]
      data <-data[!is.na(data$index_B),]
      data <-data[!is.na(data$pulsos_A),]
      data <-data[!is.na(data$pulsos_B),]
      df_ABA = select(data, Decade, Total_mm, A,B,`A'`)
      df_ABA= df_ABA %>%
        group_by(Decade) %>%
        summarize(
          `A'` = mean(`A'`)/mean(Total_mm),
          B= mean(B)/mean(Total_mm),
          A = mean(A)/mean(Total_mm)
        )
      df_ABA_m<- melt(df_ABA, id.vars = "Decade")
      
      df_ABAm = select(data, Decade, A_min, B_min,Ap_min, Total_min)
      df_ABAm= df_ABAm %>%
        group_by(Decade) %>%
        summarize(
          `A'` = mean(Ap_min)/mean(Total_min),
          B= mean(B_min)/mean(Total_min),
          A = mean(A_min)/mean(Total_min)
        )
      df_ABAm_m<- melt(df_ABAm, id.vars = "Decade")
      
      df_Ac = select(data, Decade, Rit1:A)
      df_Ac= df_Ac %>%
        group_by(Decade) %>%
        summarize(
          `Ritornello 3` = mean(Rit3)/mean(A),
          a2 =mean(a2)/mean(A),
          `Ritornello 2`= mean(Rit2)/mean(A),
          a1= mean(a1)/mean(A),
          `Ritornello 1` = mean(Rit1)/mean(A))
      df_Ac_m<- melt(df_Ac, id.vars = "Decade")
      
      
      df_Am = select(data, Decade, Rit1_min:A_min)
      df_Am= df_Am %>%
        group_by(Decade) %>%
        summarize(
          `Ritornello 3` = mean(Rit3_min)/mean(A_min),
          a2 =mean(a2_min)/mean(A_min),
          `Ritornello 2`= mean(Rit2_min)/mean(A_min),
          a1= mean(a1_min)/mean(A_min),
          `Ritornello 1` = mean(Rit1_min)/mean(A_min))
      df_Am_m<- melt(df_Am, id.vars = "Decade")
      
      if(input$pr == "ABA"){
        ggplot(data = df_ABA_m, aes(x = Decade, y = value, fill = variable)) + 
          geom_bar(stat = 'identity') + 
          ggtitle("A, B & A' in measures") + 
          xlab('Decade') + 
          ylab('%') + 
          scale_x_continuous(name = "Decade",
                             breaks = seq(1720, 1810, by = 10),
                             labels = paste0(seq(1720, 1810, by = 10)),
                             expand = expansion(c(0.05, 0.05))) +
          coord_flip() + 
          labs(fill = "Sections")+
          guides(fill = guide_legend(reverse=TRUE))+
          
          theme_minimal()}
      else if(input$pr == "ABAmin"){
        ggplot(data = df_ABAm_m, aes(x = Decade , y = value, fill = variable)) + 
          geom_bar(stat = 'identity') + 
          ggtitle("A, B & A' in minutes") + 
          xlab('Decade') + 
          ylab('%') + 
          scale_x_continuous(name = "Decade",
                             breaks = seq(1720, 1810, by = 10),
                             labels = paste0(seq(1720, 1810, by = 10)),
                             expand = expansion(c(0.05, 0.05))) +
          coord_flip() +            labs(fill = "Sections")+           guides(fill = guide_legend(reverse=TRUE))+ 
          theme_minimal()}
      else if(input$pr == "Ac") {
        ggplot(data = df_Ac_m, aes(x =Decade , y = value, fill = variable)) + 
          geom_bar(stat = 'identity') + 
          ggtitle("Subsections of A in measures") + 
          xlab('Decade') + 
          ylab('%') + 
          scale_x_continuous(name = "Decade",
                             breaks = seq(1720, 1810, by = 10),
                             labels = paste0(seq(1720, 1810, by = 10)),
                             expand = expansion(c(0.05, 0.05))) +
          coord_flip() +            labs(fill = "Sections")+           guides(fill = guide_legend(reverse=TRUE))+ 
          theme_minimal()
      }
      else {
        ggplot(data = df_Am_m, aes(x = Decade , y = value, fill = variable)) + 
          geom_bar(stat = 'identity') + 
          ggtitle("Subsections of A in minutes") + 
          xlab('Decade') + 
          ylab('%') + 
          scale_x_continuous(name = "Decade",
                             breaks = seq(1720, 1810, by = 10),
                             labels = paste0(seq(1720, 1810, by = 10)),
                             expand = expansion(c(0.05, 0.05))) +
          coord_flip() +            labs(fill = "Sections")+           guides(fill = guide_legend(reverse=TRUE))+ 
          theme_minimal()
      }
      
    })
    
   
    
    ##Plots for lines
    
    observe({
      data <-mydata()
      df_lines<- data %>% select( Year, Total_mm:B,`A'`)
      df_ABA= df_lines %>%
        group_by(Year) %>%
        summarize(
          Total_mm = mean(Total_mm),
          A = mean(A),
          Rit1 = mean(Rit1),
          a1 = mean(a1),
          Rit2 = mean(Rit2), 
          a2 = mean(a2),
          Rit3 =mean(Rit3),
          B= mean(B),
          `A'` = mean(`A'`))
      
      df_ABA_m<- melt(df_ABA, id.vars = "Year")
      
      dcast_off <- dcast(df_ABA_m, Year~variable)
      trick_off <- df_ABA_m[df_ABA_m$variable %in% input$section_m,]
      output$linesmeasures <- renderPlot({
        O <-ggplot(data = trick_off, aes(x=Year,y=value, color=variable, group = variable)) +
          
          geom_line(size = 2, alpha = 0.75) +
          geom_point(size =3, alpha = 0.75) +
          
      
          labs(x="Year",y="Average number of measures")+
          
          theme_classic()+
          theme(axis.text.x = element_text(angle = 45))+
          labs(color= "Selected section(s)")
        O
      })
      
    })
    
    
    observe({
      data <-mydata()
      data <-data[!is.na(data$index_A),]
      data <-data[!is.na(data$index_B),]
      data <-data[!is.na(data$pulsos_A),]
      data <-data[!is.na(data$pulsos_B),]
      df_lines = select(data, Year, Rit1_min:Total_min)
      df_ABA= df_lines %>%
        group_by(Year) %>%
        summarize(
          Total_min = mean(Total_min),
          A_min = mean(A_min),
          Rit1_min = mean(Rit1_min),
          a1_min = mean(a1_min),
          Rit2_min = mean(Rit2_min), 
          a2_min = mean(a2_min),
          Rit3_min =mean(Rit3_min),
          B_min= mean(B_min),
          Ap_min = mean(Ap_min))
      
      df_ABA_m<- melt(df_ABA, id.vars = "Year")
      
      dcast_off <- dcast(df_ABA_m, Year~variable)
      trick_off <- df_ABA_m[df_ABA_m$variable %in% input$section_min,]
      output$linesminutes <- renderPlot({
        O <-ggplot(data = trick_off, aes(x=Year,y=value, color=variable, group = variable)) +
          
          geom_line(size = 2, alpha = 0.75) +
          geom_point(size =3, alpha = 0.75) +
          
          labs(x="Year",y="Average number of minutes")+
          
          theme_classic()+
          theme(axis.text.x = element_text(angle = 45))+
          labs(color = "Selected section(s)")
        
        O
      })
      
    })
    
    
      
      
    
})