# You can run the application by clicking
# the 'Run App' in RStudio.
# MDCOR 

library(shiny)
library(shinythemes)
library(shinyBS)
library("shinycssloaders")
library(DT)

options(spinner.color="#CD1076", spinner.type = 6, shiny.maxRequestSize=100*1024^2)

ui <- fluidPage(theme = shinytheme("cyborg"),

pageWithSidebar(
headerPanel(
"Machine Driven Classification of Open-ended Responses (MDCOR)"
),
sidebarPanel(
tags$head(tags$style(HTML("a {color: #CD1076; font-weight: bold}"))),
tags$style(".btn-file {background-color:#CD1076; border-color: #2e6da4; } .btn:hover {
color: #ffffff;
background-color: #CD1076;
border-color: #2e6da4;
}
.progress-bar {background-color: #CD1076; }",
"
                    .dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter, .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_processing, .dataTables_wrapper .dataTables_paginate {
                    color: #ffffff;
                    }

                    thead {
                    color: #ffffff;
                    }

                     tbody {
                    color: #000000;
                    }

                   "),

    tags$b("Program Description"),
    tags$p(HTML("Machine Driven Classification of Open-ended Responses
    <b>(MDCOR)</b> analyses via Latent Dirichlet Allocation & Gibbs Sampling.<br>
    <b>MDCOR</b> identifies the optimal number of code allocations based on four metrics.<br>
    <b>F</b>inal outputs show the most representative open-ended responses per code
    and an interactive platform to evaluate code/word relevance.<br>
    <b>T</b>he code-classified database can be downloaded for further analyses")),

    tags$p(a("About MDCOR",target="_blank",href="DDS_about_PHUDCFILY.pdf" )),
    #	     img(src = "DDS_phudcfily.png", style = "float: left; width: 200px; margin-right: 5px; margin-top: 1px"),

    tags$hr(),

    tags$b("Steps to execute the MDCOR processes:"),
    div(style = "margin-top: -2.5px"),
    tags$ol(list(
    tags$li("Select database from local folder"),
    #      shinyDirButton("directory", "Select Folder", "Please select a folder", icon= icon("folder")),
    div(style = "margin-top: -20px"),
    fileInput('directory', '', placeholder = 'Or drag your file here', multiple = TRUE, accept = c("csv", ".csv"), buttonLabel=list(icon("folder"),"Browse")),
    div(style = "margin-top: -20px"),
    tags$li("Execute Text Mining/Cleaning procedures"),
    # div(style = "margin-top: -20px"),
    # tags$li("Execute Text Mining/Cleaning procedures"),
    actionButton("goButtontm", "Execute Text Mining", icon("filter")),
    #tags$li("Select numbers of most common words to trim (0 removes no words)"),
    tags$li("Select words to trim by their position. I.e., 1, 5 trims 1st and 5th words"),
    #div(style = "margin-top: -20px"),
    #numericInput("trim", "", 0),
    textInput('trimvec', 'Enter numbers separated by commas: 1, 3, 7'),
    actionButton("goButton", "Trim Common Words", icon("strikethrough")),
tags$li("Select numbers of: burn-in & Gibbs resamplings and Alpha & Beta (recommended values pre-selected)"),#If the output of the folder selected, shown in the right panel, is correct, s
    numericInput("burnin", "Burn-in Samples", 500),
    numericInput("iterations", "MCMC/Gibbs resamplings", 5000),
    numericInput("alpuser", "Alpha prior (Advanced, see appendix)", 50),
    numericInput("deluser", "Beta prior (Advanced, see appendix)", 0.1),

    tags$li("Execute Metrics' to find the optimal number of codes"),
    actionButton("goButtonT", "Execute Metrics", icon("server")),


    tags$li("Based on the metrics plot, select optimal number of codes"),
    sliderInput("obs", "Default value is 2, update below", min = 2, max = 60, value = 2),
    #tags$li("Change the number of top articles per topic"),
    div(style = "margin-top: -20px"),
    tags$li("Execute the MDCOR engine (can readjust burn-in & resampling settings)"),
    actionButton("go", "Execute MDCOR", icon("rocket")),
    tags$li("Test for Code Associations by Group"), #new PHUDCFILY 10.4.22
	selectInput('grouping', 'Group column', ""),         #new PHUDCFILY 10.4.22
     actionButton("goButtongroup", "Execute Group Comparison", icon("compress")),
	 uiOutput("tab"))
    )
    ),
    mainPanel(
    tags$h4("1. File upload information"),

    withSpinner(textOutput("corpusphu"), ),
    selectInput('ego', 'ID column', ""),
    selectInput('alter', 'Text column', ""),
     tags$hr(),
     tags$h4("2. Initial text mining output"),
     downloadButton('downloaddroppedcases', 'Download all dropped responses'),
    withSpinner(verbatimTextOutput("nText")),
    tags$hr(),
    tags$h4("3. Trimming Common words output (if unsatisfied, change number in step 3 and re-execute steps 2 & 3)"),
    withSpinner(verbatimTextOutput("nTextlda")),
    tags$hr(),
    tags$h4("4-5. Metrics output (you can download the plot as *.PNG)"),
    withSpinner(verbatimTextOutput("nTopiclda")),
    #    tags$p("You can save the metrics plot"),
    downloadButton("save", "Download Metrics Plot"),
    plotOutput("plot"),

    tags$hr(),
    tags$h4("6-7. Code Modeling, Vizualization, and Most Representative Responses per Code"),


    tags$h5("After MDCOR completion you can download the resulting datasets"),
    #textOutput("text"),
    #    radioButtons("filetype", "File type, comma separated (csv) or tab separated (tsv):",
    #                choices = c("csv", "tsv"), inline=TRUE),
    downloadButton('downloadData', 'Download Full MDCOR data (all text chunks)'),
    downloadButton('downloadDatas', 'Download Most Representative cases (shown below)'),
    withSpinner(verbatimTextOutput("TopicModeling")),
    sliderInput("topphu", "Select the number of most representative responses per code", min = 1, max = 20, value = 5),
    dataTableOutput("table"),
  	withSpinner(verbatimTextOutput("GroupTestPHUDCFILY")),
    tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #CD1076}")),
    tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: #CD1076}"))
    )
    ))

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  observe({invalidateLater(10000, session)
           cat("Phudcfily")
         })
        library(LDAvis)
        library(servr)
        library(topicmodels)
        library("ldatuning")
        library("tsne")
        library(textstem)
        library(dplyr)
        library(stringi)
        library(stringr)
        library(fs)
        #library(pdftools)
        library(tidyverse)
        library(parallel)
        library(tm)
        library("syuzhet")
        library(quanteda)
        library(gistr)

        values <- reactiveValues(x1 = NULL)
        lst1 <- eventReactive(input$directory, {
             values$x1 <- read.csv(input$directory$datapath, header = TRUE, sep = ",", fill = TRUE, comment.char="", stringsAsFactors = FALSE)
               print(paste("You uploaded a database with", dim(values$x1)[1], "rows and", dim(values$x1)[2], "columns. Please select ID (i.e., identifier for merging) and Text (i.e., open-ended responses) columns below to proceed with text mining procedures", sep=" "))
values$groups <-values$x1
            })

            outVar0 = reactive({
                  mydata = values$x1
                  names(mydata)#c("select an option below (e.g., 'actor_ID' or 'sender' depending on your data & method)",
                })
                observe({
                  updateSelectInput(session, "ego",
                  choices = outVar0()
                )})

            outVar1 = reactive({
                  mydata = values$x1
                  c(names(mydata)[2],names(mydata))#c("select an option below (e.g., 'code_ID' or 'receiver' depending on your data & method)",
                })
                observe({
                  updateSelectInput(session, "alter",
                  choices = outVar1()
                )})
         outVarG = reactive({
                                  mydata = values$groups
                                  c(names(mydata)[3],names(mydata))#c("select an option below (e.g., 'actor_ID' or 'sender' depending on your data & method)",
                                })
                observe({
                                  updateSelectInput(session, "grouping",
                                  choices = outVarG()
                                )})
        output$corpusphu <- renderPrint ({
		            lst1()
        })

        #  a<-parseDirPath(volumes, input$directory)
        ntext <- eventReactive(input$goButtontm, {

          x1 <- values$x1
          # phu=input$woman
          colnames(values$groups)[colnames(values$groups) == input$ego] <- "doc_id"
          colnames(x1)[colnames(x1) == input$ego] <- "doc_id"
          colnames(x1)[colnames(x1) == input$alter] <- "text"
          x1 <- x1[x1$text!=""&!is.na(x1$text),c("doc_id","text")]
# x1$text<-as.character(x1$text)
          x1$text <- gsub("\x89۪", "", x1$text)
          x1$text <- gsub("\xdb۪", "", x1$text)
          x1$text <- gsub("\xf7۪", "", x1$text)
          x1$text <- gsub("‰ûï", "",  x1$text)
          x1$text <- gsub("‰Û", "",   x1$text)
          x1$text <- gsub("\n", "",   x1$text)
          x1$text <- gsub("Ô", "",   x1$text)
          x1$text <- gsub("ø", "",   x1$text)
          x1$text <- gsub("Ω", "",   x1$text)
          x1$text <- gsub("Ô", "",   x1$text)
          x1$text <- gsub("€", "",   x1$text)
          x1$text <- gsub("™", "",   x1$text)
          # wc<-str_count(x1$text, '\\w+')
        #  x1<-x1[wc>3,]
          #print(str(x1))
          phu<-x1
          colnames(phu)[1]<-"files" #instead of doc_id PHUDCFILY
          phu$id<-rownames(phu) #just counting row numbers
phu$id_nm <- paste(phu$id, phu$files)
          values$phu <- phu
x1$doc_id <- phu$id_nm
          values$x1<-x1

          print(paste("The initial valid number of open-ended responses is ",dim(x1)[1], ". Text mining results are shown below.", sep=""))
          corp <- VCorpus(DataframeSource(x1),readerControl = list(language = "en"))

            ##Clean text based on thousands of text analyses 
            for (j in seq(corp)) {
                corp[[j]] <- gsub("\\t", " ", corp[[j]])
                corp[[j]] <- gsub("<U+00AE>", " ", corp[[j]])
                corp[[j]] <- gsub("U+06EA", " ", corp[[j]])
                corp[[j]] <- gsub("<U+00B4>", " ", corp[[j]])
                corp[[j]] <- gsub("<U+0097>", " ", corp[[j]])
                corp[[j]] <- gsub("<U+0096>", " ", corp[[j]])
                corp[[j]] <- gsub("<U+0094>", " ", corp[[j]])
                corp[[j]] <- gsub("<U+0091>", " ", corp[[j]])
                corp[[j]] <- gsub("<U+0092>", " ", corp[[j]])
                corp[[j]] <- gsub("<U+0093>", " ", corp[[j]])
                corp[[j]] <- gsub("<u+2afd>", " ", corp[[j]])
                corp[[j]] <- gsub("<u+4872>", " ", corp[[j]])
                corp[[j]] <- gsub("<U+00AD>", " ", corp[[j]])
                corp[[j]] <- gsub("<u+fb01>", " ", corp[[j]])
                corp[[j]] <- gsub("<u+fb02>", " ", corp[[j]])
                corp[[j]] <- gsub("\\001", " ", corp[[j]])
                corp[[j]] <- gsub("\\023", " ", corp[[j]])
                corp[[j]] <- gsub("<u+03c7>", " ", corp[[j]])
                corp[[j]] <- gsub("<e9>", " ", corp[[j]])
                corp[[j]] <- gsub("won‰ûªt", " ", corp[[j]])
                corp[[j]] <- gsub("/", " ", corp[[j]])
                corp[[j]] <- gsub("#", " ", corp[[j]])
                corp[[j]] <- gsub("@", " ", corp[[j]])
                corp[[j]] <- gsub("\\|", " ", corp[[j]])
                corp[[j]] <- gsub("\u2028", " ", corp[[j]])
                corp[[j]] <- gsub("i‰ûªd", " ", corp[[j]])
                corp[[j]] <- gsub("doesn‰ûªt", " ", corp[[j]])
                corp[[j]] <- gsub("haven‰ûªt", " ", corp[[j]])
                corp[[j]] <- gsub("hasn‰ûªt", " ", corp[[j]])
                corp[[j]] <- gsub("'", " ", corp[[j]])
                corp[[j]] <- gsub("i‰ûªve", " ", corp[[j]])
                corp[[j]] <- gsub("i‰ûªll", " ", corp[[j]])
                corp[[j]] <- gsub("ﬂ", " ", corp[[j]])
                corp[[j]] <- gsub("''", " ", corp[[j]])
                corp[[j]] <- gsub("''", " ", corp[[j]])
                corp[[j]] <- gsub("©", " ", corp[[j]])
                corp[[j]] <- gsub("-", " ", corp[[j]])
                corp[[j]] <- gsub("http", " ", corp[[j]])
                corp[[j]] <- gsub("www", " ", corp[[j]])
                corp[[j]] <- gsub("https", " ", corp[[j]])
                corp[[j]] <- gsub("‚", " ", corp[[j]])
                corp[[j]] <- gsub("~", " ", corp[[j]])
                corp[[j]] <- gsub("#", " ", corp[[j]])
                corp[[j]] <- gsub("Y", " ", corp[[j]])
                corp[[j]] <- gsub("Æ", " ", corp[[j]])
                corp[[j]] <- gsub("it‰ûªs", " ", corp[[j]])
                corp[[j]] <- gsub("‰Ûª", " ", corp[[j]])
                corp[[j]] <- gsub("i‰ûªm", " ", corp[[j]])
                corp[[j]] <- gsub("&", " ", corp[[j]])
                corp[[j]] <- gsub("%", " ", corp[[j]])
                corp[[j]] <- gsub("-", " ", corp[[j]])
                corp[[j]] <- gsub("‰Ûª", " ", corp[[j]])
                corp[[j]] <- gsub("‰ûï", " ", corp[[j]])
                corp[[j]] <- gsub("‰Û", " ", corp[[j]])
                corp[[j]] <- gsub("¶¶", " ", corp[[j]])
                     corp[[j]] <- gsub("Ô", " ", corp[[j]])
                    corp[[j]] <- gsub("ø", " ", corp[[j]])
                   corp[[j]] <- gsub("Ω", " ", corp[[j]])
                  corp[[j]] <- gsub("€", " ", corp[[j]])
                 corp[[j]] <- gsub("™", " ", corp[[j]])
                corp[[j]] <- gsub(" *\\b[[:alpha:]]{1,3}\\b *", " ", corp[[j]]) # Remove 1-3 letter words
            }
            aphu<-as.data.frame(unlist(corp))
            #x1<-values$x1
            x1$text<-aphu[,1]
            x1$text<-as.character(x1$text)
            corp <- VCorpus(DataframeSource(x1),readerControl = list(language = "en"))

            corp<-tm_map(corp, content_transformer(stri_trans_tolower))

            b<-data.frame(doc_id=x1$doc_id, text=t(as.data.frame(sapply(corp, `[`, cbind("content"), simplify = FALSE, USE.NAMES = FALSE))))
            b$text <- gsub("content", "", b$text)
            b$text <- gsub("content.", "", b$text)
            corp <- VCorpus(DataframeSource(b),readerControl = list(language = "en"))
            #remove punctuation
            corp <- tm_map(corp, removePunctuation)
            #Strip digits
            corp <- tm_map(corp, removeNumbers)
            #remove whitespace
            corp <- tm_map(corp, stripWhitespace)
            removeURL <- function(x) gsub("http[[:alnum:]]*", "",x)
            corp <- tm_map(corp, content_transformer(removeURL))
            removeURL <- function(x) gsub(" *\\b[[:alpha:]]{1,2}\\b *", " ", x)
            corp <- tm_map(corp, content_transformer(removeURL))
#lemmatization via textstem PHUDCFILY
            corp <- tm_map(corp, lemmatize_strings)
#required transformations
            aphu<-as.data.frame(unlist(corp))
            toDelete <-seq(1, nrow(aphu), 2)
            aphu<-as.data.frame(aphu[ toDelete ,])
            # aphu<-as.data.frame(aphu[aphu!="",])
            aphu[,1]<-as.character(aphu[,1])
            x1$text<-aphu[,1]
            corp <- VCorpus(DataframeSource(x1),readerControl = list(language = "en"))
#remove stopwords
            exceptions <- grep(pattern = "hav|has|do|not|n't", x = stopwords(), value = TRUE)
            my_stopwords <- setdiff(stopwords("en"), exceptions)
            corp<- tm_map(corp, removeWords, my_stopwords)
          # corp <- tm_map(corp, removeWords, c(stopwords("english"),c("phu", "dcf","ily")))

            for (j in seq(corp)) {
                corp[[j]] <- gsub(" *\\b[[:alpha:]]{1,3}\\b *", " ", corp[[j]]) # Remove 1-3 letter words
            }
            aphu<-as.data.frame(unlist(corp))
            # aphu<-as.data.frame(aphu[aphu!="",])
            aphu[,1]<-as.character(aphu[,1])
            x1$text<-aphu[,1]
            x1$text <- gsub("������", "",   x1$text)

            corp <- VCorpus(DataframeSource(x1),readerControl = list(language = "en"))
            #    print(names(corp))
            values$nphu<-names(corp)
            #Convert to document matrix
            dtm <- DocumentTermMatrix(corp)

            #remove sparse words
            dtm <- removeSparseTerms(dtm,0.998)

            ui = unique(dtm$i)
            dtm = dtm[ui,]
values$tnames <- dtm$dimnames$Docs
            print(dtm)

            top_10_words <-data.frame(words=names(head((sort(rowSums(as.matrix(t(dtm))),decreasing=TRUE)),20)),freq=head((sort(rowSums(as.matrix(t(dtm))),decreasing=TRUE)),20), row.names=NULL)

            #    phu<-names(head((sort(rowSums(as.matrix(t(dtm))),decreasing=TRUE)),20))
            values$dtm<-dtm

            dtm2list <- apply(values$dtm, 1, function(x) {
                paste(rep(names(x), x), collapse=" ")
            })
            tphu<-as.data.frame(dtm2list)
      			rownames(tphu)<-dtm$dimnames$Docs
            values$tphu<-tphu
            tphu$dropped<-0
            values$x1phu <- values$x1
            values$x1phu$dropped <- tphu$dropped[match(values$x1phu$doc_id, rownames(tphu))]

            print(paste("Based on the DocumentTermMatrix information, a total of ", dim(values$x1phu[is.na(values$x1phu$dropped),])[1], " responses were dropped due to sparsity (irrelevance) issues", sep=""))
			print(paste("If applicable, to assess dropped cases and potentially use them for non-response analyses, select 'Download all dropped responses' above", sep=""))
			print(paste("If applicable, MDCOR shows the first 10 responses dropped and the top 10 most common words", sep=""))
                        print(paste("Response(s) dropped, ID: ",head(values$x1phu$doc_id[is.na(values$x1phu$dropped)],10), " Text: ", head(values$x1phu$text[is.na(values$x1phu$dropped)],10), sep= ""))

            print(top_10_words)
values$top_10_words<-top_10_words
            ## convert to a Corpus
            corp <- VCorpus(VectorSource(dtm2list))
            names(corp) <- values$tnames
            #    print(names(corp))
            values$corp<-corp
        })

        ntextlda <- eventReactive(input$goButton, {

            ## Convert tdm to a list of text
            #dtm2list <- apply(values$dtm, 1, function(x) {
            #    paste(rep(names(x), x), collapse=" ")
            #})
            ## convert to a Corpus
            #corp <- VCorpus(VectorSource(dtm2list))
            #names(corp) <- rownames(values$tphu)
            #print(names(corp))

            #    corp <- tm_map(corp, removeWords, values$phu[input$caption])

corp<-values$corp
           txphu <- as.numeric(unlist(strsplit(input$trimvec,",")))
           corp <- tm_map(corp, removeWords, values$top_10_words[txphu,1])
#            corp <- tm_map(corp, removeWords, names(head((sort(rowSums(as.matrix(t(values$dtm))),decreasing=TRUE)),input$trim)))
            #print(names(corp))
            dtm <- DocumentTermMatrix(corp)
            ui = unique(dtm$i)
            dtm = dtm[ui,]
values$tnames <- dtm$dimnames$Docs
            ## Convert tdm to a list of text
            dtm2list <- apply(dtm, 1, function(x) {
                paste(rep(names(x), x), collapse=" ")
            })
            ## convert to a Corpus
            corp <- VCorpus(VectorSource(dtm2list))
            #tphu<-as.data.frame(dtm2list)
            names(corp) <- values$tnames

            trp_10_words<-data.frame(words=names(head((sort(rowSums(as.matrix(t(dtm))),decreasing=TRUE)),20)),freq=head((sort(rowSums(as.matrix(t(dtm))),decreasing=TRUE)),20), row.names=NULL)
            print(dtm)
            print(trp_10_words)
            #print(names(corp))
            values$dtm<-dtm
            values$corp<-corp
        })

        ntopiclda <- eventReactive(input$goButtonT, {
            #cluster <- makeCluster(detectCores(logical = TRUE) - 1)

            FindTopicsNumberphu <- function(dtm, topics = seq(10, 40, by = 10),
                                         metrics = "Griffiths2004",
                                         method = "Gibbs", control = list(),
                                         mc.cores = NA, return_models = FALSE,
                                         verbose = FALSE, libpath = NULL) {
              # check parameters
              if (length(topics[topics < 2]) != 0) {
                if (verbose) cat("warning: topics count can't to be less than 2, incorrect values was removed.\n")
                topics <- topics[topics >= 2]
              }
              topics <- sort(topics, decreasing = TRUE)

              if ("Griffiths2004" %in% metrics) {
                if (method == "VEM") {
                  # memory allocation error
                  if (verbose) cat("'Griffiths2004' is incompatible with 'VEM' method, excluded.\n")
                  metrics <- setdiff(metrics, "Griffiths2004")
                } else {
                  # save log-likelihood when generating model
                  if (!"keep" %in% names(control)) control <- c(control, keep = input$alpuser)
                }
              }

              # fit models
              if (verbose) cat("fit models...")


              models <- lapply(X = topics, FUN = function(x) {
              # if (is.null(libpath) == FALSE) { .libPaths(libpath) }
                topicmodels::LDA(dtm, k = x, method = method, control = control)
              })
              # if (! any(class(mc.cores) == "cluster")) {
                # parallel::stopCluster(cl)
              # }
              if (verbose) cat(" done.\n")

              # calculate metrics
              if (verbose) cat("calculate metrics:\n")

              if (return_models &
                  requireNamespace("tibble", quietly = TRUE)
              ) {
                result <- cbind(topics, tibble::enframe(models, value = "LDA_model"))
                result$name <- NULL
              } else {
                if (return_models) {
                  message("The tibble package is required for returning models. Returning results only.")
                }
                result <- data.frame(topics)
              }
              for(m in metrics) {
                if (verbose) cat(sprintf("  %s...", m))
                if (! m %in% c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014")) {
                  cat(" unknown!\n")
                } else {
                  result[m] <- switch(m,
                    "Griffiths2004" = Griffiths2004(models, control),
                    "CaoJuan2009"   = CaoJuan2009(models),
                    "Arun2010"      = Arun2010(models, dtm),
                    "Deveaud2014"   = Deveaud2014(models),
                    NaN
                  )
                  if (verbose) cat(" done.\n")
                }
              }

              return(result)
            }

            #' Griffiths2004
            #'
            #' Implement scoring algorithm. In order to use this algorithm, the LDA model MUST
            #' be generated using the keep control parameter >0 (defaults to 50) so that the
            #' logLiks vector is retained.
            #' @param models An object of class "\link[topicmodels]{LDA}
            #' @param control A named list of the control parameters for estimation or an
            #'   object of class "\linkS4class{LDAcontrol}".
            #' @return A scalar LDA model score
            #'
            #' @export
            #'
            Griffiths2004 <- function(models, control) {
              # log-likelihoods (remove first burnin stage)
              burnin  <- ifelse("burnin" %in% names(control), control$burnin, 0)

              logLiks <- lapply(models, function(model) {
                # Check to make sure logLiks were kept; if not, value is NaN
                if (length(model@logLiks) == 0) {
                  message("No logLiks were kept, which is required to use this scoring algorithm. Please regenerate the model using the keep control parameter set to a reasonable value (default = 50).")
                  NaN
                } else {
                  utils::tail(model@logLiks, n = length(model@logLiks) - burnin/control$keep)
                  # model@logLiks[-(1 : (control$burnin/control$keep))]
                }
              })

              # harmonic means for every model
              metrics <- sapply(logLiks, function(x) {
                # code is a little tricky, see explanation in [Ponweiser2012 p. 36]
                # ToDo: add variant without "Rmpfr"
                llMed <- stats::median(x)
                metric <- as.double(
                  llMed - log( Rmpfr::mean( exp( -Rmpfr::mpfr(x, prec=2000L) + llMed )))
                )
                return(metric)
              })
              return(metrics)
            }

            #' CaoJuan2009
            #'
            #' Implement scoring algorithm
            #' @param models An object of class "\link[topicmodels]{LDA}
            #' @return A scalar LDA model score
            #'
            #' @export
            #'
            CaoJuan2009 <- function(models) {
              metrics <- sapply(models, function(model) {
                # topic-word matrix
                m1 <- exp(model@beta)
                # pair-wise cosine distance
                pairs <- utils::combn(nrow(m1), 2)
                cos.dist <- apply(pairs, 2, function(pair) {
                  x <- m1[pair[1], ]
                  y <- m1[pair[2], ]
                  # dist <- lsa::cosine(x, y)
                  dist <- crossprod(x, y) / sqrt(crossprod(x) * crossprod(y))
                  return(dist)
                })
                # metric
                metric <- sum(cos.dist) / (model@k*(model@k-1)/2)
                return(metric)
              })
              return(metrics)
            }

            #' Arun2010
            #'
            #' Implement scoring algorithm
            #' @param models An object of class "\link[topicmodels]{LDA}
            #' @param dtm An object of class "\link[tm]{DocumentTermMatrix}" with
            #'   term-frequency weighting or an object coercible to a
            #'   "\link[slam]{simple_triplet_matrix}" with integer entries.
            #' @return A scalar LDA model score
            #'
            #' @export
            #'
            Arun2010 <- function(models, dtm) {
              # length of documents (count of words)
              len <- slam::row_sums(dtm)
              # evaluate metrics
              metrics <- sapply(models, FUN = function(model) {
                # matrix M1 topic-word
                m1 <- exp(model@beta) # rowSums(m1) == 1
                m1.svd <- svd(m1)
                cm1 <- as.matrix(m1.svd$d)
                # matrix M2 document-topic
                m2   <- model@gamma   # rowSums(m2) == 1
                cm2  <- len %*% m2    # crossprod(len, m2)
                norm <- norm(as.matrix(len), type="m")
                cm2  <- as.vector(cm2 / norm)
                # symmetric Kullback-Leibler divergence
                divergence <- sum(cm1*log(cm1/cm2)) + sum(cm2*log(cm2/cm1))
                return ( divergence )
              })
              return(metrics)
            }

            #' Deveaud2014
            #'
            #' Implement scoring algorithm
            #' @param models An object of class "\link[topicmodels]{LDA}
            #' @return A scalar LDA model score
            #'
            #' @export
            #'
            Deveaud2014 <- function(models) {
              metrics <- sapply(models, function(model) {
                ### original version
                # topic-word matrix
                m1 <- exp(model@beta)
                # prevent NaN
                if (any(m1 == 0)) { m1 <- m1 + .Machine$double.xmin }
                # pair-wise Jensen-Shannon divergence
                pairs  <- utils::combn(nrow(m1), 2)
                jsd <- apply(pairs, 2, function(pair) {
                  x <- m1[pair[1], ]
                  y <- m1[pair[2], ]
                  ### standard Jensen-Shannon divergence
                  # m <- (x + y) / 2
                  # jsd <- 0.5 * sum(x*log(x/m)) + 0.5 * sum(y*log(y/m))
                  ### divergence by Deveaud2014
                  jsd <- 0.5 * sum(x*log(x/y)) + 0.5 * sum(y*log(y/x))
                  return(jsd)
                })

            #     ### optimized version
            #     m1   <- model@beta
            #     m1.e <- exp(model@beta)
            #     pairs  <- utils::combn(nrow(m1), 2)
            #     jsd <- apply(pairs, 2, function(pair) {
            #       x   <- m1[pair[1], ]
            #       y   <- m1[pair[2], ]
            #       x.e <- m1.e[pair[1], ]
            #       y.e <- m1.e[pair[2], ]
            #       jsd <- ( sum(x.e*(x-y)) + sum(y.e*(y-x)) ) / 2
            #       return(jsd)
            #     })

                # metric
                metric <- sum(jsd) / (model@k*(model@k-1))
                return(metric)
              })
              return(metrics)
            }

            result <- FindTopicsNumberphu(
            values$dtm,
            topics = seq(from = 2, to = 60, by = 1),
            metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
            method = "Gibbs",
            control = list(seed = 47, burnin = input$burnin, iter = input$iterations, delta = input$deluser),
            #mc.cores = NA,
            verbose = TRUE
            )
            values$results<-result
            output$plot <- renderPlot({
                input$newplot
                # Add a little noise to the cars data
                FindTopicsNumber_plot(result)
            })
            #str(documents)
        })

        #p2 <- reactive(FindTopicsNumber_plot(values$results))
        output$save <- downloadHandler(
        file = "Metrics_plot.pdf" , # variable with filename
        content = function(file) {
            #ggsave(p(), filename = file)
            pdf(file = file, width = 20, height = 10, onefile=FALSE)
            # p2()
            FindTopicsNumber_plot(values$results)
            dev.off()
        })

        output$nText <- renderPrint ({
            ntext()
        })

        output$nTextlda <- renderPrint ({
            ntextlda()
        })

        output$nTopiclda <- renderPrint ({
            ntopiclda()
        })

        topicmodeling <- eventReactive(input$go, {


            ap_lda <- LDA(values$dtm, k = input$obs, method = "Gibbs", control = list(seed = 47, burnin = input$burnin, iter = input$iterations, delta = input$deluser, alpha=(input$alpuser/input$obs)))
            ###new matching Phudcfily
            fitted<-ap_lda
            corpus<-values$corp
            doc_term<-values$dtm

            phi <- posterior(fitted)$terms %>% as.matrix
            theta <- posterior(fitted)$topics %>% as.matrix
            vocab <- colnames(phi)
            doc_length <- vector()
            for (i in 1:length(corpus)) {
                temp <- paste(corpus[[i]]$content, collapse = ' ')
                doc_length <- c(doc_length, stri_count(temp, regex = '\\S+'))
            }
            temp_frequency <- as.matrix(doc_term)#inspect(doc_term)
            freq_matrix <- data.frame(ST = colnames(temp_frequency),
            Freq = colSums(temp_frequency))
            rm(temp_frequency)

            # Convert to json
            svd_tsne <- function(x) tsne(svd(x)$u)
            json_lda <- createJSON(phi = phi, theta = theta,
            vocab = vocab,
            doc.length = doc_length,
            term.frequency = freq_matrix$Freq,
            mds.method = svd_tsne)

            serVis2 <- function (json, out.dir = tempfile(),
                as.gist = FALSE, ...)
            {
                dir.create(out.dir)
                src.dir <- system.file("htmljs", package = "LDAvis")
                to.copy <- Sys.glob(file.path(src.dir, "*"))
                file.copy(to.copy, out.dir, overwrite = TRUE, recursive = TRUE)
                cat(json, file = file.path(out.dir, "lda.json"))
            d<<-(out.dir)
            m<<-httd(d, daemon = TRUE)$url
            }

            serVis2(json_lda)
            # print(d) #just to point the tmp local dir
            # print(m) #this should be active based on @yihui help!!! Thank you again!
            utils::browseURL(m)
            new.order <- RJSONIO::fromJSON(json_lda)$topic.order
            ###

            topicProbabilities <- as.data.frame(ap_lda@gamma); tnlda <- names(topicProbabilities)
            topicProbabilities <- topicProbabilities[, new.order]; names(topicProbabilities) <- tnlda
            topicProbabilities <- round(topicProbabilities,3)
            #head(topicProbabilities)
            #dim(topicProbabilities)
            topicProbabilities$topic<-colnames(topicProbabilities)[max.col(topicProbabilities,ties.method="first")]
            topicProbabilities$text<- ap_lda@documents
            topicProbabilities$quote <- values$x1$text[match(topicProbabilities$text, values$x1$doc_id)]
            topicProbabilities$text#<-NULL

            topicProbabilities$relative_probability <- apply(topicProbabilities[,1:input$obs], 1, max)
            #Select the five best examples PHUDCFILY

            g<-as.data.frame(aggregate(relative_probability~topic, topicProbabilities, max))
            gavg<-as.data.frame(aggregate(relative_probability~topic, topicProbabilities, mean))#PHUDCFILY
            topicProbabilities$max_group <- g$relative_probability[match(topicProbabilities$topic, g$topic)]
            topicProbabilities$max_group_avg <- gavg$relative_probability[match(topicProbabilities$topic, gavg$topic)]
            topicProbabilities$post_estimate<-topicProbabilities$relative_probability#/topicProbabilities$max_group

            topicProbabilities$relative_probability<-topicProbabilities$relative_probability/topicProbabilities$max_group
            topicProbabilities$relative_group_fit<-topicProbabilities$max_group_avg/(max(topicProbabilities$max_group_avg))

            topicProbabilities$relative_text_contribution <- round(topicProbabilities$relative_probability, 3)
            topicProbabilities$relative_group_fit <- round(topicProbabilities$relative_group_fit, 3)
            topicProbabilities$code <- topicProbabilities$topic
            topicProbabilities<-topicProbabilities[,c("code","text","relative_text_contribution", "relative_group_fit", "quote")] #"max_group", "max_group_avg", "post_estimate", PHUDCFILY
            topicProbabilities$ID <- gsub(".* ", "", topicProbabilities$text)
            topicProbabilities$row_num <- gsub(" .*$", "", topicProbabilities$text)
            topicProbabilities$text<-NULL
            values$findings<-topicProbabilities



            output$table <- renderDataTable({
                ex1 <-  as.data.frame(topicProbabilities %>% group_by(code) %>% top_n(input$topphu, relative_text_contribution))
                values$ex1<-ex1
                datatable(ex1, options = list(
                pageLength = input$topphu*input$obs))
            })

            observeEvent(input$topphu, {
                updateSliderInput(session,inputId = "table" ,value = input$topphu,
                min = 1, max = 20, step = 1)
            })

        })

        output$TopicModeling <- renderPrint ({
            topicmodeling()
        })

        # downloadHandler() takes two arguments, both functions.
        # The content function is passed a filename as an argument, and
        #   it should write out data to that filename.
        output$downloadData <- downloadHandler(

        # This function returns a string which tells the client
        # browser what name to use when saving the file.
        filename = function() {
            paste("Full_Machine_Learned_Classes", "csv", sep = ".")
        },

        # This function should write data to a file given to it by
        # the argument 'file'.
        content = function(file) {
            #  sep <- switch(input$filetype, "csv" = ",", "tsv" = "\t")

            # Write to a file specified by the 'file' argument
            write.table(values$findings, file, sep = ",",
            row.names = FALSE)
        }
        )

###PHUDCFILY
output$downloaddroppedcases <- downloadHandler(
filename = function() {
    paste("Dropped_cases_for_analysis", "csv", sep = ".")
},
content = function(file) {
    write.table(values$x1phu[is.na(values$x1phu$dropped),], file, sep = ",",
    row.names = FALSE)
}
)
###PHUDCFILY

        output$downloadDatas <- downloadHandler(

        # This function returns a string which tells the client
        # browser what name to use when saving the file.
        filename = function() {
            paste("Most_Representative_Docs", "csv", sep = ".")
        },

        # This function should write data to a file given to it by
        # the argument 'file'.
        content = function(file) {
            #sep <- switch(input$filetype, "csv" = ",", "tsv" = "\t")

            # Write to a file specified by the 'file' argument
            write.table(values$ex1, file, sep = ",",
            row.names = FALSE)
        }
        )

        grouptestPHUDCFILY <- eventReactive(input$goButtongroup, {
            groups <- values$groups
            colnames(groups)[colnames(groups) == input$ego] <- "doc_id"
            colnames(groups)[colnames(groups) == input$grouping] <- "group"
            # values$groups <- groups
            groups$code <- values$findings$code[match(groups$doc_id, values$findings$ID)]
        library(igraph)
        gphu<-graph.data.frame(groups[!is.na(groups$group),c("group","code")])
        V(gphu)$type <- V(gphu)$name %in% groups[,"group"]
        E(gphu)$weight<-1
        gphu <- simplify(gphu)
        E(gphu)$weight
        #hypothesis testing PHUDCFILY
        datatable<-t(get.incidence(gphu, attr="weight"))
        rownames(datatable)<-paste(rownames(datatable), ", n=", rowSums(datatable), sep="")
        colnames(datatable)<-paste(colnames(datatable), ", n=", colSums(datatable), sep="")
        chisq.test(datatable,simulate.p.value = TRUE, B=5000)
        dt<-as.table(datatable)
        library("graphics")
        pdf("www/independence_test.pdf", 8.5, 9)
        mosaicplot(dt, shade = TRUE, las=2, main=paste("Test of independence, X-squared = ", round(chisq.test(datatable,simulate.p.value = TRUE, B=5000)[1][[1]],2), ", p.val =< ", round(chisq.test(datatable,simulate.p.value = TRUE, B=5000)[3][[1]],3), sep=""))
        dev.off()
              url <- a("Click here to Open the Chi-squared test of independence",target="_blank",href="independence_test.pdf")#PHUDCFILY, download = 'independence_test.pdf')
            output$tab <- renderUI({
              tagList("Hypothesis test:", url)
            })
gphu <- delete_vertices(gphu, "NA")
        g <- graph.adjacency(as.matrix((get.incidence(gphu, attr="weight"))) %*% (as.matrix(t(get.incidence(gphu, attr="weight")))>1), diag=F, mode="plus", weighted = TRUE)
        #print(head(as.data.frame(cbind(get.edgelist(g),as.numeric(E(g)$weight)))))
        #print(get.adjacency(g, attr="weight"))
        g<-as.data.frame(cbind(get.edgelist(g),as.numeric(E(g)$weight)))
        #g$V3 <- as.numeric(g$V3)
        # print((gphu))
        # print((datatable))
        dg <- data.frame(id = colnames(t(get.incidence(gphu, attr="weight"))), dg = colSums(t(get.incidence(gphu, attr="weight"))))
        #print(dg)
        detach(package:igraph)
        library(ndtv)
        library(networkDynamic) # load the dynamic extensions
        # library(dplyr)
        library(viridis)
        net3 <- network(g[g$V1!="NA",c("V1", "V2")], matrix.type="edgelist",
                        loops=T, multiple=F, ignore.eval = F)
        net3 %e% "weigth"<- as.numeric(as.character(g$V3))
        #print(as.numeric(as.character(g$V3)))
        #print(g)
        #print(g$V3)
        #print(net3 %e% "weigth")
        #delete.vertices(net3, "NA")
        #print(net3 %v% "vertex.names")
        net3 <- networkDynamic(base.net=net3)
        net3 <- network.collapse(net3, onset=1, terminus=nrow(g)+1)

        net3 %v% "labels" <- as.character(dg$id[match(net3 %v% "vertex.names", dg$id)])
        net3 %v% "cex" <- (dg$dg[match(net3 %v% "vertex.names", dg$id)])
        #print(net3 %v% "vertex.names")

         render.d3movie(net3,
         usearrows = F, label.cex=.5,
         vertex.tooltip = paste("<b>Code name:</b>",(net3 %v% "labels"),"<br>", "<b>No. of unique connections:</b>",(net3 %v% "cex")),
         vertex.col = alpha("magenta", 0.8),
         label.col=rgb(250, 250, 250,max=255, 255/2),
         bg=rgb(11, 11, 11, max=255, 255/1), #222, 222, 222
         vertex.border="NA",
         vertex.cex = (net3 %v% "cex"/max(net3 %v% "cex")+.5),
         edge.lwd = log(net3 %e% "weigth"),
         edge.col = ifelse(net3 %e% "weigth"==max(net3 %e% "weigth"), "red", "snow"), #viridis( length(net3 %e% "weigth"), option="plasma",  direction = -1),
        edge.tooltip = paste("<b>No. of shared connections:</b>", (net3 %e% "weigth")),
         launchBrowser=T, filename="collapsed_analyses.html",
         main="")
        utils::browseURL(paste(getwd(),"/collapsed_analyses.html",sep=""))

        })
           output$GroupTestPHUDCFILY <- renderPrint ({
               grouptestPHUDCFILY()
           })
   # output$distPlot <- renderPlot({
      # # generate bins based on input$bins from ui.R
      # x    <- faithful[, 2]
      # bins <- seq(min(x), max(x), length.out = input$bins + 1)

      # # draw the histogram with the specified number of bins
      # hist(x, breaks = bins, col = 'darkgray', border = 'white')
   # })



}

# Run the application
shinyApp(ui = ui, server = server)
