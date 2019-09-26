server <- function(input, output, session) {
  # toggle state for the prepare data button (become active only when either a dataset or the toy dataset are uploaded)
  observe({
    shinyjs::toggleState("load", 
                         !is.null(input$datafile) | input$checkbox == T)
  })
  # toggle states SC-IAT
  observe({
    shinyjs::toggleState("sc_load", 
                         !is.null(input$datafile_sc) | input$example_sciat == T)
  })
  
  observe({
    shinyjs::toggleState("downloadSciat", input$sc_update > 0)
  })
  
  observe({
    shinyjs::toggleState("sc_update", input$sc_load >0)
  })
  
  
  # toggle state for the update button (become active only when a D-score is selected)
  observe({
    shinyjs::toggleState("update", input$sel_d != 0)
  })
  # toggle state for the Download button (become active only when the update buttton has been clicked at least once)
  observe({
    shinyjs::toggleState("downloadData", input$update > 0)
  })
  # create reactive object where data, results, and options can be stored
  values <- reactiveValues()
  # check whether the toy dataset has been selected 
  dataentry <- observe({
    if (input$checkbox == T){
      datasetInput <- reactive({
        # define the wd for the example dataset AND HAS TO BE CHANGED ACCORDINGLY
        dataset <- read.csv("~/GitHub/DscoreApp/raceAPP.csv")
      })
    } else {
      # if the toy dataset has not been found --> import and store users' dataset
      # Import data
      datasetInput <- reactive({
        infile <- input$datafile
        if (is.null(infile)) {
          # User has not uploaded a file yet
          return(NULL)
        }
        isolate({
          input$load
          dataset <- read.csv(infile$datapath)
        })
        dataset
      })
    }
    # store the data in the reactive object
    observe({
      values$dataset <- data.frame(datasetInput())
    })
    # store the blcok labels that are in the dataframe
    observe({
      values$dataset$block_label <- isolate(values$dataset$block)
    })
  })
  # import data SC-IAT ----
  # check whether you want to use the example sciat_dataset
  dataentry_sciat <- observe({
    if (input$example_sciat == T){
      sciat_datasetInput <- reactive({
        # sciat_dataset <- read.csv("/srv/shiny-server/DscoreApp/raceAPP.csv")
        sciat_dataset <- read.csv("~/GitHub/DscoreApp/singleSCIAT.csv")
      })
    } else {
      # Import data
      sciat_datasetInput <- reactive({
        infile_sc <- input$datafile_sc
        if (is.null(infile_sc)) {
          # User has not uploaded a file yet
          return(NULL)
        }
        isolate({
          input$sc_load
          sciat_dataset <- read.csv(infile_sc$datapath)
        })
        sciat_dataset
      })
    }
    observe({
      values$sciat_dataset <- data.frame(sciat_datasetInput())
    })
    observe({
      values$sciat_dataset$block_label <- isolate(values$sciat_dataset$block)
    })
  })
  # IAT labels ----
  # Select variable MappingA practice block
  output$label_mapA_practice <- renderUI({
    # save the unique blcoks labels as they are in the dataframe
    labels.options <- unique(values$dataset$block)
    selectInput("mapA_practice", h5("e.g. practiceWhiteGood"), 
                choices = labels.options,
                # display the first label as default
                labels.options[1])
  })  
  # select variable MappingA test block
  output$label_mapA_test <- renderUI({
    # save the unique blcoks labels as they are in the dataframe
    labels.options <- unique(values$dataset$block)
    selectInput("mapA_test", h5("e.g. testWhiteGood"), 
                choices = labels.options,
                # display the second label as default
                labels.options[2] )
  })
  # select variable MappingB practice block
  output$label_mapB_practice <- renderUI({
    # save the unique blcoks labels as they are in the dataframe
    labels.options <- unique(values$dataset$block)
    selectInput("mapB_practice", h5("e.g. practiceWhiteBad"), 
                choices = labels.options,
                # display the third label as default
                labels.options[3] )
  })  
  # select variable MappingA test block
  output$label_mapB_test <- renderUI({
    # save the unique blcoks labels as they are in the dataframe
    labels.options <- unique(values$dataset$block)
    selectInput("mapB_test", h5("e.g. testWhiteBad"), 
                choices = labels.options,
                # display the fourth label as default
                labels.options[4])
  })
  
  # SC-IAT labels ----
  # select variable MappingA practice block
  output$label_mapA_sciat1 <- renderUI({
    labels.options_sc <- unique(values$sciat_dataset$block)
    selectInput("mapA_sciat1", h5("e.g. CokeGood"), 
                choices = labels.options_sc,
                labels.options_sc[1])
  })  
  # select variable MappingA test block
  output$label_mapB_sciat1 <- renderUI({
    labels.options_sc <- unique(values$sciat_dataset$block)
    selectInput("mapB_sciat1", h5("e.g. CokeBad"), 
                choices = labels.options_sc,
                labels.options_sc[2] )
  })
  ### labels check IAT ----
  labels_check<- observeEvent(
    input$load, 
    {
      # check whether there are more blocks labels than expected 
      if (length(unique(values$dataset$block_label)) > 4){
        alert <- "There are more blocks than expected. Remove the extra blocks 
        and restart the app." 
        values$alert <- "restart" # create and save an alert
      }
      # if the number of blocks labels is correct, check whether users tried 
      # to select the same label for two blocks
      # if they did, an alert is created and saved
      else if(input$mapA_practice == input$mapA_test){
        alert <- "check your labels and restart the app!"
        values$alert <- "restart"
      }
      else if(input$mapA_practice == input$mapB_practice){
        alert <- "check your labels and restart the app!"
        values$alert <- "restart"
      }
      else if(input$mapA_practice == input$mapB_test){
        alert <- "check your labels and restart the app!"
        values$alert <- "restart"
      }
      else if(input$mapB_practice == input$mapB_test){
        alert <- "check your labels and restart the app!"
        values$alert <- "restart"
      }
      else if(input$mapB_practice == input$mapA_test){
        alert <- "check your labels and restart the app!"
        values$alert <- "restart"
      }
      else if(input$mapB_test == input$mapA_test){
        alert <- "check your labels and restart the app!"
        values$alert <- "restart"
      }
      else{
        return()
      }
      # if at least one of the previous conditions is true, an alert is displayed 
      shinyjs::alert(alert)
    }
  )
  
  # Labels check SC-IAT
  # ### prepare data #### don't know why but it doesn't work. 
  labels_check_sc <- observeEvent(
    input$sc_load,
    {
      labels <- unique(values$sciat_dataset$block)
      if (length(labels) > 2){
        sc_alert <- "There are too many blocks/labels!"
        values$sc_alert <- "restart"
      } else if(input$mapA_sciat1 == input$mapB_sciat1){
        sc_alert <- "Check your labels and restart the app!"
        values$sc_alert <- "restart"
      } else {
        return()
      }
      shinyjs::alert(sc_alert)
    })
  
  # Prepare the dataframe for the D-score computation IAT ----
  newentry <- observeEvent(
    input$load,
    {
      # rename blocks labels in MappingA and MappingB to create the IAT 
      # conditions variable
      values$dataset$Condition <- as.character(values$dataset$block)
      values$dataset$Condition <- with(values$dataset,
                                       ifelse(block == input$mapA_practice |
                                                block == input$mapA_test, 
                                              "MappingA",
                                              ifelse(
                                                block == input$mapB_practice |
                                                  block == input$mapB_test, 
                                                "MappingB",
                                                "error")))
      # Create order of presentation variable and save it in a dataframe
      values$condition_order <- with(values$dataset,
                                     aggregate(Condition, 
                                               by = list(participant), 
                                               FUN = unique))
      colnames(values$condition_order) <- c("participant", "order")
      # create the variable of the order of blocks presentation
      values$condition_order$cond_ord <- paste(values$condition_order$order[,1],
                                               values$condition_order$order[,2],
                                               sep = "_") 
      # select just the participant and condition order variables
      values$condition_order <- values$condition_order[, c("participant", 
                                                           "cond_ord")]
      # rename the order so that is consistent with the "MappingA" and "MappingB"
      values$condition_order$cond_ord <- with(values$condition_order,
                                              ifelse(
                                                cond_ord == "MappingA_MappingB",
                                                "MappingA_First",
                                                "MappingB_First"))
      # create the legend for MappingA
      values$condition_order$legendMappingA <- paste(input$mapA_practice,
                                                     "and",input$mapA_test, 
                                                     sep = "_")
      # create the legend for MappingB
      values$condition_order$legendMappingB <- paste(input$mapB_practice,
                                                     "and",input$mapB_test, 
                                                     sep = "_")
      
      # rename the block level in practice and test block
      # save the original starting blocks labels in a new variable
      values$dataset$blockR <- as.character(values$dataset$block)
      # crate the block_pool variable (just practice vs test)
      # needed for the computation of the pooled sd
      values$dataset$block_pool <- with(values$dataset,
                                        ifelse(block == input$mapA_practice |
                                              block == input$mapB_practice, 
                                                        "practice",
                                               ifelse(
                                               block == input$mapA_test |
                                               block == input$mapB_test, 
                                                 "test",
                                                 "error"
                                               )))
      # create the block variable (practice_MappingA, practice_MappingB,
      # test_MappingA, test_MappingB)
      values$dataset$blockR <- with(values$dataset,
                                    paste(block_pool, Condition, 
                                          sep = "_"))
      
      # both data with and without built in have the same column 'latency' containing the RTs
      # in the built-in correction case, the latency has to be corrected with the inflation for the responses beforehand
      # create a variable identifying slow trials ( > 10,000 ms)
      values$dataset$slow <- ifelse(values$dataset$latency > 10000, 
                                    "no", "yes")
      # create a variable identifying fast responses to be eliminated according to the D-score selected (< 400 ms)       
      values$dataset$fast400 <- with(values$dataset,
                                     ifelse(latency < 400, 
                                            "no", "yes"))
      # create a variable identifying fast response (< 300 ms) for the elimination of the fast participants
      values$dataset$fast300 <- ifelse(values$dataset$latency < 300, 
                                       "no", "yes")
      
      # number of slow responses for each participant
      values$num_slow <- data.frame(with(values$dataset, 
                                         table(slow, participant)))
      values$num_slow <- values$num_slow[values$num_slow$slow %in% "yes", ]
      # number of trials actually performed before any deletion 
      values$num_trial <- data.frame(with(values$dataset, 
                                          table(participant)))
      colnames(values$num_trial) <- c("participant", "n_trial")
      # merge the number of slow trials with the number of trails
      values$num_slow <- merge(values$num_slow, values$num_trial, 
                               by = "participant")
      # compute the difference between the number of trials actually peformed and the number of slow trials
      values$num_slow$slow10000 <- with(values$num_slow, 
                                        n_trial - Freq)
      values$num_slow <- values$num_slow[, c("participant", "n_trial",
                                             "slow10000")]
      # number of fast reasponses (< 300) for each participant
      values$num_fast300 <- data.frame(with(values$dataset,
                                            table(fast300, participant)))
      values$num_fast300 <- values$num_fast300[values$num_fast300$fast300 %in% 
                                                 "no", c("participant", "Freq")]
      colnames(values$num_fast300) <- c("participant", "num.300")
      
      # number of fast responses (< 400) for each participant
      values$num_fast400 <- data.frame(with(values$dataset,
                                            table(fast400, participant)))
      values$num_fast400 <- values$num_fast400[values$num_fast400$fast400 %in% 
                                                 "no", c("participant", "Freq")]
      colnames(values$num_fast400) <- c("participant", "num.400")
      
      # compute the percentage of fast responses
      # needed for deciding whether to eliminate participants or not
      values$dataset$participant <- as.character(values$dataset$participant)
      # pnumber of trials < 300 ms for each participant
      values$sbj_300 <- data.frame(with(values$dataset, 
                                        table(latency < 300, participant)))
      # select only the lines that evaluated in TRUE
      values$sbj_300 <- values$sbj_300[values$sbj_300$Var1 %in% "TRUE", c(2,3)]
      values$sbj_300$participant <- as.character(values$sbj_300$participant)
      # create the decision variable for the fast responses participants deletion (if it evaluates in TRUE --> "out) 
      for(i in 1:length(unique(values$dataset$participant))){
        values$sbj_300$out_fast <- ifelse(values$sbj_300$Freq > 
                                            (table(values$dataset$participant)[i])*0.10, 
                                          "out", "keep")
        
      }
      colnames(values$sbj_300)[2] <- "n_trial300"
      values$sbj_300$participant <- as.character(values$sbj_300$participant)
      # merge dataset to values$sbj_300 to create the filter variable
      values$dataset <- merge(values$dataset, values$sbj_300, 
                              by = "participant")
      # create variable for the output of total number of slow responses
      values$slow <- values$dataset[values$dataset$slow %in% "no", ]
      if (nrow(values$slow) !=0) {
        values$slow_perc <- round((nrow(values$slow) / nrow(values$dataset)) * 100, 
                                  2)
      }
      
      # create a varible for telling whether data are ready
      values$ready <- values$dataset[values$dataset$slow %in% "yes", ]
      # compute proportion of correct responses for each participant in each 
      # condition
      values$correct_response <- with(values$dataset,
                                      aggregate(correct, 
                                                by = list(Condition, participant),
                                                FUN = mean))
      colnames(values$correct_response) <- c("Condition", "participant", 
                                             "prop_correct_cond")
      # reahspe in wide format 
      values$correct_response_wide <- reshape(values$correct_response, 
                                              idvar = "participant",
                                              timevar = "Condition", 
                                              direction = "wide")
      # calculate the proportion of error responses (error_cond)
      values$correct_response$error_cond <- with(values$correct_response,
                                                 1 - prop_correct_cond)
      # merge original dataframe with the proportion of error responses to 
      # create the filter variable
      values$dataset <- merge(values$dataset, values$correct_response,
                              by = c("participant", "Condition"))
      # compute proportion of correct responses for each participant in each block
      values$accuracy_block <- with(values$dataset,
                                    aggregate(correct, 
                                              by = list(participant, blockR), 
                                              FUN = mean) )
      colnames(values$accuracy_block) <- c("participant", "block", 
                                           "p_correct_block")
      # reshape in wide format
      values$accuracy_block_wide <- reshape(values$accuracy_block,
                                            idvar = "participant", 
                                            timevar = "block",
                                            direction = "wide")
      # compute proportion of correct responses for each participant in each 
      # block_pool (practice vs test)
      values$accuracy_block_pool <- with(values$dataset,
                                         aggregate(correct, 
                                                   by = list(participant, 
                                                             block_pool), 
                                                   FUN = mean))
      colnames(values$accuracy_block_pool) <- c("participant", "block_pool", 
                                                "p_correct_bpool")
      # reshape the dataframe
      values$accuracy_block_pool_wide <- reshape(values$accuracy_block_pool, 
                                                 idvar = "participant",
                                                 timevar = "block_pool", 
                                                 direction = "wide")
      # compute overall proportion of correct responses for each participant
      values$accuracy_tot <- with(values$dataset,
                                  aggregate(correct, by = list(participant), 
                                            FUN = mean))
      colnames(values$accuracy_tot) <- c("participant", "p_correct_tot")
      # merge accuracy_block and accuracy_block_pool
      values$accuracy <- merge(values$accuracy_block_wide,
                               values$accuracy_block_pool_wide, 
                               by = "participant")
      # merge overall accuracy with correct_response_wide 
      # (proportion of correct responses in each condition)
      values$accuracy <- merge(values$accuracy, 
                               values$correct_response_wide, 
                               by = "participant")
      # merge accuracy with accuracy accuracy_tot
      values$accuracy <- merge(values$accuracy, 
                               values$accuracy_tot, 
                               by = "participant")
      ## descriptive information on participants' time performance
      # merge number of fast trials (both < 300 ms and < 400 ms)
      values$fast_sbj <- merge(values$num_fast300, values$num_fast400,
                               by = "participant")
      # merge the number of slow responses with the number of fast responses
      values$time <- merge(values$num_slow, values$fast_sbj,
                           by = "participant")
      # overall RTs average for each participant 
      values$subject_mean <- with(values$dataset,
                                  aggregate(latency,
                                            by = list(participant), 
                                            FUN = mean))
      
      colnames(values$subject_mean) <- c("participant", "mean.tot")
      # merge the time dataset (containing the information on fast and slow 
      # responses) with the overall average response time
      values$time <- merge(values$time, values$subject_mean,
                           by = "participant")
      values$time <- merge(values$time, values$accuracy, 
                           by = "participant")
      # take out slow responses 
      values$dataset <- values$dataset[values$dataset$slow %in% "yes", ]
    })
  
  # tell the user whether the data are ready to use IAT ----
  output$data_ready <- renderUI({
    
    loading <- ifelse(is.null(values$ready), ("Waiting for data"), 
                      "Data are ready!") 
    helpText(h3(loading))
  })
  
  
  # prepare the dataset for the D SC-IAT computation ----
  newentry_sciat <- observeEvent(
    input$sc_load,
    {
      values$sc_final_data <- data.frame(participant = 
                                        unique(values$sciat_dataset$participant))
      # check for the alert trials 
      if (input$window_check == TRUE & 
          any(colnames(values$sciat_dataset) == "trial") == FALSE) {
        values$sc_alert <- "restart" 
        shinyjs::alert("Can't find the column with the trials labels!")
      } else if (input$window_check == TRUE & input$label_window == ""){
        values$sc_alert <- "restart"
        shinyjs::alert("You haven't specified the label for the trials 
                       exceeding the response time window!")
        
      } else if (input$window_check == TRUE & 
                 any(values$sciat_dataset$trial == input$label_window) == FALSE) {
        values$sc_alert <- "restart"
        shinyjs::alert("Are you sure you spelled the label correctly?")
      } else if (input$window_check == T) {
        # count the number of discarded trials BEFORE taking them out
        values$tot_window <- round((
          sum(values$sciat_dataset$trial == input$label_window) / 
            nrow(values$sciat_dataset)) * 100, 2)
        values$tot_window <- c("number" = 
                                 sum(values$sciat_dataset$trial == input$label_window),
                               "percentage" = values$tot_window)
        values$sbj_window <- data.frame(with(values$sciat_dataset, 
                                             table(participant, 
                                                   trial == input$label_window)))
        values$sbj_window <- reshape(values$sbj_window, 
                                     idvar = "participant", 
                                     timevar = "Var2", 
                                     direction = "wide")
        values$sbj_window$perc_window <- with(values$sbj_window,
                                              round((Freq.TRUE / (Freq.FALSE + 
                                                                    Freq.TRUE)) 
                                                    * 100, 2))
        values$sbj_window <- values$sbj_window[, c(1,4)]
        values$sc_final_data <- merge(values$sc_final_data, 
                                      values$sbj_window, 
                                      by = "participant")
        values$sciat_dataset <- values$sciat_dataset[!values$sciat_dataset$trial 
                                                     %in% input$label_window, ]
      } else if (input$window_check == F) {
        values$sciat_dataset <- values$sciat_dataset
      }
      # rename levels of condition in MappingA and MappingB
      values$sciat_dataset$Condition <- as.character(values$sciat_dataset$block)
      
      values$sciat_dataset$Condition <- with(values$sciat_dataset,
                                            ifelse(block == input$mapA_sciat1,
                                                   "MappingA",
                                                   ifelse(
                                                     block == input$mapB_sciat1,
                                                     "MappingB",
                                                     "error")))
      # order of presentation
      
      values$condition_order_sc <- with(values$sciat_dataset,
                                        aggregate(Condition, 
                                                  by = list(participant), 
                                                  FUN = unique))
      colnames(values$condition_order_sc) <- c("participant", "order")
      values$condition_order_sc$cond_ord <- paste(
        values$condition_order_sc$order[,1],
        values$condition_order_sc$order[,2],
        sep = "_") 
      values$condition_order_sc <- values$condition_order_sc[, c("participant", 
                                                               "cond_ord")]
      values$condition_order_sc$cond_ord <- with(values$condition_order_sc,
                                                 ifelse(
                                                   cond_ord == "MappingA_MappingB",
                                                   "MappingA_First",
                                                   "MappingB_First"))
      values$condition_order_sc$legendMappingA <- input$mapA_sciat1
      
      values$condition_order_sc$legendMappingB <- input$mapB_sciat1
      
      # compute the percentage of fast responses for each participant
      values$fast350_sc <- data.frame(with(values$sciat_dataset, 
                                           table(participant, latency < 350)))
      values$fast350_sc <- reshape(values$fast350_sc, 
                                   idvar = "participant", 
                                   timevar = "Var2", 
                                   direction = "wide")
      
      values$fast350_sc$perc_fast <- with(values$fast350_sc, 
                                          round((Freq.TRUE/(Freq.FALSE + Freq.TRUE))*100, 2))
      values$perc_tot_fast <- with(values$fast350_sc, 
                                   round(sum(Freq.TRUE)/(sum(Freq.FALSE) + 
                                                           sum(Freq.TRUE)) 
                                         * 100, 2))
      values$perc_tot_fast <- c("number" = sum(values$fast350_sc$Freq.TRUE), 
                                "percentage" = values$perc_tot_fast)
      values$fast350_sc <- values$fast350_sc[, c("participant", "perc_fast")]
      
      # take out fast responses 
      values$sciat_dataset$outfast <- with(values$sciat_dataset, 
                                          ifelse(latency < 350, 
                                                 "out", "keep"))
      values$sciat_dataset <- values$sciat_dataset[values$sciat_dataset$outfast %in% "keep", ]
      
      values$sc_ready <- "ready"
      
    })
  
  # data are ready message sc-iat -----
  # tell the user whether the data are ready to use
  output$sc_select1 <- renderUI({
    
    sc_loading <- ifelse(is.null(values$sc_ready), "Waiting for data", 
                         "Data are ready!") 
    helpText(h3(sc_loading))
  })
  
  # prevent buttons ####
  # toggle state for the update button (become active only when a D-score is selected and there's something in the ready object)
  observe({
    shinyjs::toggleState("update", input$sel_d != 0 && !(is.null(values$ready)))
  })
  # toggle state for the Select D drop-down menu (become active only when there's something in the ready object)
  observe({
    shinyjs::toggleState("sel_d", !(is.null(values$ready)))
  })
  # toggle state for the Accuracy deletion option (become active only when a
  # D-score is selected)
  observe({
    shinyjs::toggleState("accuracy_del", input$sel_d != 0 )
  })
  # toggle state for the Fast participants deletion option (become active only 
  # when a D-score is selected
  observe({
    shinyjs::toggleState("sbjFast_del", input$sel_d != 0 )
  })
  
  ### calculate the  D-score ####
  cleandata <- observeEvent(
    input$update, 
    {
      # Compute the D-score according to the specific algorithm selected by the 
      # users
      if (input$sel_d == 1){
        # d1: built in, no lower tail treatment
        values$out_400 <- "Not expected for this D"
        values$d1 <- values$dataset
        # create the variable latency_cor for the computatiopn of the D-score
        values$d1$latency_cor <- values$d1$latency
        values$data <- values$d1
        values$d_select <- 1
      } else if (input$sel_d == 2){
        # d2: built in, lower tail treatment 400ms
        values$out_400 <- sum(values$dataset$fast400 == "no")
        values$out_400 <- c("number" = values$out_400, 
                            "percentage" = 
                              round(values$out_400 / nrow(values$dataset) * 100, 2))
        values$d2 <- values$dataset[values$dataset$fast400 %in% "yes", ]
        # create the variable latency_cor for the computatiopn of the D-score
        values$d2$latency_cor <- values$d2$latency
        values$data <- values$d2
        values$d_select <- 2
      } else if (input$sel_d == 3){
        values$out_400 <- "Not expected for this D"
        # d3: no built in, no lower tail treatment, error = mean + 2*sd
        values$d3 <- values$dataset
        # Compute the mean on the correct responses for the error correction
        values$correct_time_d3 <- values$d3[which(values$d3$correct == 1), ]
        values$mean_correct_d3 <- with(values$correct_time_d3,
                                       aggregate(latency, 
                                                 by = list(blockR, participant),
                                                 FUN = mean))
        colnames(values$mean_correct_d3) <- c("blockR", "participant", "mean")
        # merge original data with mean on correct responses
        values$d3 <- merge(values$d3, values$mean_correct_d3,
                           by = c("participant", "blockR"))
        
        # Compute the sd on the correct responses for the error correction
        values$sd_correct_d3 <- with(values$correct_time_d3,
                                     aggregate(latency, 
                                               by = list(blockR, participant),
                                               FUN = sd))
        colnames(values$sd_correct_d3) <- c("blockR", "participant", "sd_block")
        # merge original data with correct sd
        values$d3 <- merge(values$d3, values$sd_correct_d3,
                           by = c("participant", "blockR"))
        # compute the penalty mean + 2*sd
        values$d3$sd_penalty <- with(values$d3,
                                     mean +  (2 * sd_block))
        # if the respone is incorrect --> penalty, otherwise latency
        values$d3$latency_cor <- with(values$d3,
                                      ifelse(correct == 0, 
                                             sd_penalty, latency))
        values$data <- values$d3
        values$d_select <- 3
      } else if (input$sel_d == 4) {
        values$out_400 <- "Not expected for this D"
        # d4: no built in, no lower tail treatment, error = mean + 600
        values$d4 <- values$dataset 
        # Compute the mean on the correct responses for the error correction
        values$correct_time_d4 <- values$d4[which(values$d4$correct == 1), ]
        values$mean_correct_d4 <- with(values$correct_time_d4,
                                       aggregate(latency, 
                                                 by = list(blockR, participant),
                                                 FUN = mean))
        colnames(values$mean_correct_d4) <- c("blockR", "participant", "mean")
        # merge original data with correct mean
        values$d4 <- merge(values$d4, values$mean_correct_d4,
                           by = c("participant", "blockR"))
        # compute the peanlty mean + 600
        values$d4$penalty <- with(values$d4,
                                  mean + 600)
        # if teh response is incorrect --> penalty, otherwise latency
        values$d4$latency_cor <- with(values$d4, 
                                      ifelse(correct == 0, penalty, latency))
        values$data <- values$d4
        values$d_select <- 4
        
      } else if(input$sel_d == 5){
        #d5: no built in, lower tail treatment, error = mean + 2*sd
        values$out_400 <- sum(values$dataset$fast400 == "no")
        values$out_400 <- c("number" = values$out_400, 
                            "percentage" = 
                              round(values$out_400 / nrow(values$dataset) * 100, 2))
        values$d5 <- values$dataset[values$dataset$fast400 %in% "yes", ]
        
        # Compute the mean on the correct responses for the error correction
        values$correct_time_d5 <- values$d5[which(values$d5$correct == 1), ]
        values$mean_correct_d5 <- with(values$correct_time_d5,
                                       aggregate(latency, by = list(blockR, participant),
                                                 FUN = mean))
        colnames(values$mean_correct_d5) <- c("blockR", "participant", "mean")
        # merge original data to correct mean
        values$d5 <- merge(values$d5, values$mean_correct_d5,
                           by = c("participant", "blockR"))
        # Compute the sd on the correct responses for the error correction
        values$sd_correct_d5 <- with(values$correct_time_d5,
                                     aggregate(latency, by = list(blockR, participant),
                                               FUN = sd))
        colnames(values$sd_correct_d5) <- c("blockR", "participant", "sd_block")
        # merge origianl data to sd correct
        values$d5 <- merge(values$d5, values$sd_correct_d5,
                           by = c("participant", "blockR"))
        # compute penalty mean + 2*sd
        values$d5$sd_penalty <- with(values$d5,
                                     mean + (2 * sd_block))
        values$d5$latency_cor <- with(values$d5,
                                      ifelse(correct == 0, sd_penalty, latency))
        values$data <- values$d5
        values$d_select <- 5
      } else if (input$sel_d == 6){
        # d6: no builtin, lower tail treatment, error = mean + 600
        values$out_400 <- sum(values$dataset$fast400 == "no")
        values$out_400 <- c("number" = values$out_400, 
                            "percentage" = 
                              round(values$out_400 / nrow(values$dataset) * 100, 2))
        values$d6 <- values$dataset[values$dataset$fast400 %in% "yes", ]
        
        # Compute the mean on the correct responses for the error correction
        values$correct_time_d6 <- values$d6[which(values$d6$correct == 1), ]
        values$mean_correct_d6 <- with(values$correct_time_d6,
                                       aggregate(latency, by = list(blockR, participant),
                                                 FUN = mean))
        colnames(values$mean_correct_d6) <- c("blockR", "participant", "mean")
        # merge original data with correct mean
        values$d6 <- merge(values$d6, values$mean_correct_d6,
                           by = c("participant", "blockR"))
        # compute penalty mean + 600
        values$d6$penalty <- with(values$d6,
                                  mean + 600)
        values$d6$latency_cor <- with(values$d6, 
                                      ifelse(correct == 0, penalty, latency))
        values$data <- values$d6
        values$d_select <- 6
      }
      
      # Actually compute the Dscore
      # compute the vraiance on the blocik pool (practice vs test)
      values$variance <- with(values$data,
                              aggregate(latency_cor, 
                                        by = list(participant, block_pool),
                                        FUN = var))
      colnames(values$variance) <- c("participant", "block_pool", "variance")
      # compute the mean for each subject in each block
      values$sbj_mean <- with(values$data,
                              aggregate(latency_cor, 
                                        by = list(participant, blockR),
                                        FUN = mean))
      colnames(values$sbj_mean) <- c("participant", "blockR", "mean")
      # create a variable indicating just whether the block was a practice or a test block, so that this dataframe can be merged with the datfarme containing the variance
      values$sbj_mean$block_pool <- values$sbj_mean$block
      values$sbj_mean$block_pool  <- gsub(".MappingA", '', 
                                          values$sbj_mean$block_pool)
      values$sbj_mean$block_pool  <- gsub(".MappingB", '', values$sbj_mean$block_pool )
      values$sbj_data <- merge(values$variance, 
                               values$sbj_mean, 
                               by = c("participant","block_pool"))
      # reshape in wide format
      values$sbj_data_wide <- reshape(values$sbj_data, 
                                      idvar = "participant", 
                                      timevar = "blockR",
                                      direction = "wide")
      # select only useful variables 
      values$sbj_data_wide <- values$sbj_data_wide[, 
                                                   c("participant", 
                                                     "block_pool.practice_MappingA", 
                                                     "variance.practice_MappingA",   
                                                     "mean.practice_MappingA",
                                                     "mean.test_MappingA", 
                                                     "block_pool.test_MappingB",
                                                     "variance.test_MappingB",
                                                     "mean.test_MappingB",
                                                     "mean.practice_MappingB")]
      # rename the columns
      colnames(values$sbj_data_wide) <- c("participant", 
                                          "block_pool_practice_MappingA", 
                                          "variance_practice",   
                                          "mean_practice_MappingA", 
                                          "mean_test_MappingA", 
                                          "block_pool_test_MappingB",
                                          "variance_test", 
                                          "mean_test_MappingB", 
                                          "mean_practice_MappingB")
      # compute the difference in the average response time for the practice 
      # blocks of the two mappings
      values$sbj_data_wide$diff_practice <- with(values$sbj_data_wide,
                                                 mean_practice_MappingB - 
                                                   mean_practice_MappingA)
      # compute the difference in the average response time for the tests bloks 
      # of the two mappings
      values$sbj_data_wide$diff_test <- with(values$sbj_data_wide,
                                             mean_test_MappingB - 
                                             mean_test_MappingA)
      # compute the D-score for the practice blocks 
      values$sbj_data_wide$d_practice <- with(values$sbj_data_wide,
                                              diff_practice / 
                                                sqrt(variance_practice))
      # compute the D-score for the test blocks 
      values$sbj_data_wide$d_test <- with(values$sbj_data_wide,
                                          diff_test / sqrt(variance_test))
      # compute the D-score as the mean between the practice and test D-score
      values$sbj_data_wide$dscore <- with(values$sbj_data_wide,
                                          (rowSums(
                                            values$sbj_data_wide[, 
                                                                 c("d_practice", 
                                                                    "d_test")])) 
                                          / 2)
      # select only useful columns
      values$dframe <- values$sbj_data_wide[, c("participant", "d_practice", 
                                                "d_test", "dscore")]
      # merge the dataset containing the D-score with the dataset containing 
      # the details on participants performance
      values$descript_data <- merge(values$time, 
                                    values$dframe,
                                    by = "participant")
      # merge the descript_data dataset with the dataset containing the order 
      # of presentation of the blocks
      values$descript_data <- merge(values$descript_data, 
                                    values$condition_order)
      # specificy which D-score was compute by pasting the number of the D-score 
      # to the d_practice, d_test and d_score variables
      colnames(values$descript_data)[16:18] <- paste(colnames(
        values$descript_data)[16:18], input$sel_d, sep = "_")
      # compute the accuracy based on the percentage enetered by the users
      values$dataset$test_acc <- with(values$dataset,
                                      ifelse(values$dataset$error_cond > 
                                               input$perc_error / 100,
                                             "out", "keep"))
      # create a dataframe containing the IDs of the partciipants to eliminate 
      # based on the accuracy deletion
      values$sbj_accuracy <- values$dataset[values$dataset$test_acc %in% "out", ]
      # create a dataframe containing the IDs of the participants to 
      # eliminate based on fast responses
      values$sbj_time <- values$dataset[values$dataset$out_fast %in% "out", ]
      # merge together partcipants filter variables for both accuracy and 
      # time deletion
      values$out_participants <- c(values$sbj_accuracy$participant, 
                                   values$sbj_time$participant)
      # create teh condition for displaying the participants according to 
      # users' display configurations
      if (input$accuracy_del == 1 & input$sbjFast_del == 1){ 
        # Display all participants
        # save the dataset with the results in a temporary dataframe 
        # (values$display)
        values$display <- values$dframe
        # compute the reliability only on the selected participants
        values$test_practice_rel <- cor(values$display[, c("d_practice", 
                                                           "d_test")])
        # display the descriptive statistics only for the selected participants
        values$desc_stats <- values$data[values$data$participant %in% 
                                           values$display$participant, ]
        
      } else if (input$accuracy_del == 2 & input$sbjFast_del == 1){ 
        # Accuracy deletion only
        values$display <- values$dframe[!(values$dframe$participant) %in%
                                          values$sbj_accuracy$participant, ]
        
        values$out_400d <- values$dataset[!(values$dataset$participant) %in%
                                            values$sbj_accuracy$participant, ]
        
        values$out_400 <- sum(values$out_400d$fast400 == "no")
        values$out_400 <- c("number" = values$out_400, 
                            "percentage" = 
                              round(values$out_400 / nrow(values$dataset) * 100, 2))
        
        values$test_practice_rel <- cor(values$display[, c("d_practice", 
                                                           "d_test")])
        values$desc_stats <- values$data[values$data$participant %in% 
                                           values$display$participant, ]
      } else if (input$accuracy_del == 1 & input$sbjFast_del == 2) { 
        # Fast participants deletion only
        values$display <- values$dframe[!(values$dframe$participant) %in%
                                          values$sbj_time$participant, ]
        
        values$out_400d <-  values$dataset[!(values$dataset$participant) %in%
                                             values$sbj_time$participant, ]
        
        values$out_400 <- sum(values$out_400d$fast400 == "no")
        values$out_400 <- c("number" = values$out_400, 
                            "percentage" = 
                              round(values$out_400 / nrow(values$dataset) * 100, 2))
        
        values$test_practice_rel <- cor(values$display[, c("d_practice", 
                                                           "d_test")])
        values$desc_stats <- values$data[values$data$participant %in% 
                                           values$display$participant, ]
      } else if (input$accuracy_del == 2 & input$sbjFast_del == 2){ 
        # Both accuracy and fast participants deletion
        values$display <- values$dframe[!(values$dframe$participant) %in%
                                          values$out_participants, ]
        
        values$out_400d <- values$dataset[!(values$dataset$participant) %in%
                                            values$out_participants, ]
        
        values$out_400 <- sum(values$out_400d$fast400 == "no")
        values$out_400 <- c("number" = values$out_400, 
                            "percentage" = 
                              round(values$out_400 / nrow(values$dataset) * 100, 2))
        
        values$test_practice_rel <- cor(values$display[, c("d_practice", 
                                                           "d_test")])
        
        values$desc_stats <- values$data[values$data$participant %in% 
                                           values$display$participant, ]
        
      }
      # create a label stating whether the fast trails cleaning was applied or not 
      if(values$d_select == 1 || values$d_select == 3 || values$d_select == 4){
        values$out_400 <- "Not expected for this D"
      } else {
        values$out_400 <- values$out_400
      }
      
    })
  # compute the D-score for the SC-IAT ----
  computeD <- observeEvent(
    input$sc_update, 
    {
      # eliminate fast responses, if selected
      if (input$slow_del_sc == TRUE) {
        values$sciat_dataset$filter_time <- ifelse(values$sciat_dataset$latency 
                                                   > input$time_slow, 
                                                  "out", "keep") 
        values$out_filter_sc <- round((sum(values$sciat_dataset$filter_time == 
                                             "out") / nrow(values$sciat_dataset)) 
                                      * 100, 2)
        values$out_filter_sc <- c("number" = 
                                   round(sum(values$sciat_dataset$filter_time == "out")),
         "percentage" = values$out_filter_sc)
        values$sbj_filter_sc <- data.frame(with(values$sciat_dataset, 
                                                table(participant, 
                                                      filter_time == "out")))
        values$sbj_filter_sc <- reshape(values$sbj_filter_sc, 
                                        idvar = "participant", 
                                        timevar = "Var2", 
                                        direction = "wide")
        values$sbj_filter_sc$perc_filter <- with(values$sbj_filter_sc, 
                                                 round((Freq.TRUE / (Freq.FALSE 
                                                      + Freq.TRUE)) * 100, 2))
        values$sbj_filter_sc <- values$sbj_filter_sc[, c("participant", 
                                                         "perc_filter")]
        # values$sc_final_data <- merge(values$sc_final_data, 
        #                            values$sbj_filter_sc, 
        #                            by = "participant")
        values$sciat <- values$sciat_dataset[values$sciat_dataset$filter_time 
                                             %in% "keep", ]
      } else{
        values$sciat <- values$sciat_dataset
      }
      
      # compute the percentage of error for each participant 
      values$accuracy_tot_sc <- aggregate(correct ~ participant, 
                                          data = values$sciat, mean)
      values$accuracy_tot_sc$correct <- round(values$accuracy_tot_sc$correct * 100, 2)
      colnames(values$accuracy_tot_sc)[2] <- "tot_accuracy"
      
      # compute the percenatge of error for each participant in each condition
      values$accuracy_cond_sc <- aggregate(correct ~ participant + Condition, 
                                           data = values$sciat, mean)
      values$accuracy_cond_sc$correct <- round(values$accuracy_cond_sc$correct*100, 2)
      values$accuracy_cond_sc <- reshape(values$accuracy_cond_sc, 
                                         idvar = "participant", 
                                         timevar = "Condition", 
                                         direction = "wide")
      colnames(values$accuracy_cond_sc)[2:3] <- paste(gsub("correct.", "", 
                                                           colnames(values$accuracy_cond_sc)[2:3]), 
                                                      "accuracy", sep = "_")
      # create a unqiue dataframe fir the accuracy percentage
      values$accuracy_sc <- merge(values$accuracy_tot_sc, 
                                  values$accuracy_cond_sc, 
                                  by = "participant")
      # compute the mean RT for each participant
      
      values$mean_tot_sc <- aggregate(latency ~ participant, 
                                      data = values$sciat, mean)
      colnames(values$mean_tot_sc)[2] <- "tot_meanRT"
      
      # compute the mean RT for each participant in each condition
      values$mean_cond_sc <- aggregate(latency ~ participant + Condition, 
                                       data = values$sciat, mean)
      
      values$mean_condW_sc <- reshape(values$mean_cond_sc, 
                                      idvar = "participant", 
                                      timevar = "Condition", 
                                      direction = "wide")
      colnames(values$mean_condW_sc)[2:3] <- paste(gsub("latency.", "", 
                                                        colnames(values$mean_condW_sc)[2:3]), 
                                                   "meanRT", sep = "_")
      
      values$mean_time_sc <- merge(values$mean_tot_sc, 
                                   values$mean_condW_sc, by = "participant")
      # prepare the final data 
      values$sc_final_data <- merge(values$sc_final_data, 
                                    values$fast350_sc, by = "participant")
      values$sc_final_data <- merge(values$sc_final_data, 
                                    values$mean_time_sc, 
                                    by = "participant")
      values$sc_final_data <- merge(values$sc_final_data, 
                                    values$accuracy_sc, 
                                    by = "participant")
      values$sc_final_data <- merge(values$sc_final_data, 
                                    values$condition_order_sc, 
                                    by = "participant")
      # compute D-score sciat ----
      # take out correct responses
      values$correct_sc <- values$sciat[values$sciat$correct == 1, ]
      
      # calcolo la deviazione standard solo per le risposte corrette across 
      # conditions
      values$sd_correct_sc <- aggregate(latency ~ participant, 
                                        data = values$correct_sc, sd)
      colnames(values$sd_correct_sc)[2] <- "sd_correct"
      
      # merge mean response time for each participant in each condition to the 
      # original dataframe 
      colnames(values$mean_cond_sc)[3] <- "mean_lat"
      values$sciat <- merge(values$sciat, values$mean_cond_sc, 
                            by = c("participant", "Condition"))
      
      # merge the original dataframe with the sd computed on the correct 
      # latencies
      values$sciat <- merge(values$sciat,
                            values$sd_correct_sc,
                            by = "participant")
      
      # replace error responses with the mean plus 400 ms penalty
      values$sciat$latency_cor <- with(values$sciat,
                                       ifelse(correct == 0,
                                              mean_lat + 400,
                                              latency))
      # compute the mean response time in each condition on the corrected 
      # latencies 
      values$mean_correct_sc <- aggregate(latency_cor ~ participant + Condition,
                                          data = values$sciat,
                                          mean)
      
      values$mean_correct_sc <- reshape(values$mean_correct_sc,
                                        idvar = "participant",
                                        timevar = "Condition", direction = "wide")
      
      
      # merge mean_correct with sd_correct
      
      values$calc_d <- merge(values$mean_correct_sc,
                             values$sd_correct_sc, by = "participant")
      colnames(values$calc_d) <- gsub("latency_cor.", 'lat_',
                                      colnames(values$calc_d))
      values$calc_d$diff <- with(values$calc_d,
                                 lat_MappingA - lat_MappingB)
      
      values$calc_d$dsciat <- with(values$calc_d,
                                   diff/sd_correct)
      
    })
  
  # define the action for activating all the pop-up menu specified in the UI
  # All the pop-up menus are based on the click
  shinyjs::onclick("imp_text",
                   shinyjs::toggle(id = "details_imptext", anim = TRUE))
  
  shinyjs::onclick("imp_text_sc",
                   shinyjs::toggle(id = "details_imptext_sc", anim = TRUE))
  
  shinyjs::onclick("imp_intro_sc",
                   shinyjs::toggle(id = "details_intro_sc", anim = TRUE))
  
  shinyjs::onclick("imp_intro",
                   shinyjs::toggle(id = "details_intro", anim = TRUE))
  
  shinyjs::onclick("det_works",
                   shinyjs::toggle(id = "details_works", anim = TRUE))
  
  shinyjs::onclick("det_works_sc",
                   shinyjs::toggle(id = "details_works_sc", anim = TRUE))
  
  shinyjs::onclick("det_descriptive",
                   shinyjs::toggle(id = "details_descriptive", anim = TRUE))
  
  shinyjs::onclick("det_descriptive_sc",
                   shinyjs::toggle(id = "details_descriptive_sc", anim = TRUE))
  
  shinyjs::onclick("example_det",
                   shinyjs::toggle(id = "details_example", anim = TRUE))
  
  shinyjs::onclick("det_dpanel",
                   shinyjs::toggle(id = "details_dpanel", anim = TRUE))
  
  shinyjs::onclick("det_dpanel_sc",
                   shinyjs::toggle(id = "details_dpanel_sc", anim = TRUE))
  
  shinyjs::onclick("det_getting",
                   shinyjs::toggle(id = "details_getting", anim = TRUE))
  
  shinyjs::onclick("det_getting_sc",
                   shinyjs::toggle(id = "details_getting_sc", anim = TRUE))
  
  shinyjs::onclick("det_references",
                   shinyjs::toggle(id = "details_references", anim = TRUE))
  
  shinyjs::onclick("det_contacts",
                   shinyjs::toggle(id = "details_contacts", anim = TRUE))
  
  shinyjs::onclick("det_license",
                   shinyjs::toggle(id = "details_license", anim = TRUE))
  
  shinyjs::onclick("imp_det",
                   shinyjs::toggle(id = "details_import", anim = TRUE))
  
  shinyjs::onclick("sbjFast_det",
                   shinyjs::toggle(id = "details_sbjFast", anim = TRUE))
  
  shinyjs::onclick("take400_det",
                   shinyjs::toggle(id = "details_take400", anim = TRUE))
  
  shinyjs::onclick("builtin_det",
                   shinyjs::toggle(id = "details_builtin", anim = TRUE))
  
  shinyjs::onclick("graph_det",
                   shinyjs::toggle(id = "details_graph", anim = TRUE))
  
  shinyjs::onclick("select_D",
                   shinyjs::toggle(id = "details_D", anim = TRUE))
  
  shinyjs::onclick("info_prepare",
                   shinyjs::toggle(id = "details_prepare", anim = TRUE))
  
  shinyjs::onclick("practice_det_mapA",
                   shinyjs::toggle(id = "details_practice_mapA", anim = TRUE))
  
  shinyjs::onclick("test_det_mapA",
                   shinyjs::toggle(id = "details_test_mapA", anim = TRUE))
  
  shinyjs::onclick("practice_det_mapB",
                   shinyjs::toggle(id = "details_practice_mapB", anim = TRUE))
  
  shinyjs::onclick("test_det_mapB",
                   shinyjs::toggle(id = "details_test_mapB", anim = TRUE))
  
  shinyjs::onclick("mapA_det",
                   shinyjs::toggle(id = "details_mapA", anim = TRUE))
  
  shinyjs::onclick("mapB_det",
                   shinyjs::toggle(id = "details_mapB", anim = TRUE))
  
  shinyjs::onclick("built_det",
                   shinyjs::toggle(id = "details_built", anim = TRUE))
  
  shinyjs::onclick("point_det",
                   shinyjs::toggle(id = "details_point", anim = TRUE))
  
  shinyjs::onclick("hist_det",
                   shinyjs::toggle(id = "details_histogram", anim = TRUE))
  
  shinyjs::onclick("accuracy_det",
                   shinyjs::toggle(id = "details_accuracy", anim = TRUE))
  
  shinyjs::onclick("percentage_det",
                   shinyjs::toggle(id = "details_perc", anim = TRUE))
  
  shinyjs::onclick("percentage_det1",
                   shinyjs::toggle(id = "details_perc1", anim = TRUE))
  
  # pop up menus SC-IAT ----
  # menu
  
  shinyjs::onclick("sc_exampledata",
                   shinyjs::toggle(id = "details_scdata", anim = TRUE))
  
  shinyjs::onclick("info_window",
                   shinyjs::toggle(id = "details_window", anim = TRUE))
  
  shinyjs::onclick("info_windowlabel",
                   shinyjs::toggle(id = "details_windowlabel", anim = TRUE))
  
  shinyjs::onclick("info_mapAsciat1",
                   shinyjs::toggle(id = "details_mapAsciat1", anim = TRUE))
  
  shinyjs::onclick("info_mapBsciat1",
                   shinyjs::toggle(id = "details_mapBsciat1", anim = TRUE))
  
  shinyjs::onclick("info_comprel",
                   shinyjs::toggle(id = "details_comprel", anim = TRUE))
  
  shinyjs::onclick("sc_graph_det",
                   shinyjs::toggle(id = "sc_details_graph", anim = TRUE))
  
  shinyjs::onclick("impsc_det", 
                   shinyjs::toggle(id = "details_scimport", anim = TRUE))
  
  shinyjs::onclick("info_scprepare", 
                   shinyjs::toggle(id = "details_scprepare", anim = TRUE))
  
  shinyjs::onclick("hist_sc", 
                   shinyjs::toggle(id = "sc_histogram", anim = TRUE))
  
  shinyjs::onclick("info_timeslow_sc", 
                   shinyjs::toggle(id = "details_timeslow_sc", anim = TRUE))
  
  
  # DSCORE output ----
  output$summary <- renderPrint({
    # Display a message while waiting for data
    if(input$reset == 0){
      validate(
        need(input$update > 0 , "Waiting for data")
      ) 
    } else if (input$reset > 0){
      validate(
        need(input$update > 0  && 
               (!is.null(values$display)), "Waiting for data")
      ) 
    }
    # show the summary statistics
    dframe <- values$display
    summary(dframe[, c("d_practice", "d_test", "dscore")])
  })
  
  # Number of slow trials ####
  output$slow <- renderPrint({
    # Display a message while waiting for data
    if(input$reset == 0){
      validate(
        need(input$update > 0 , "Waiting for data")
      ) 
    } else if (input$reset > 0){
      validate(
        need(input$update > 0  && 
               (!is.null(values$display)), "Waiting for data")
      ) 
    }
    dataset <- values$slow
    sum_slow <- nrow(dataset)
    # If there aren't slow trials, the "None" label is displayed
    if (sum_slow !=0) {
      return(c("number" = sum_slow, "percentage" = values$slow_perc))
    } else {
      return("None")
    }
  })
  
  # Number of fast trials ####
  output$fast <- renderPrint({
    # Display a message while waiting for data
    if(input$reset == 0){
      validate(
        need(input$update > 0 , "Waiting for data")
      ) 
    } else if (input$reset > 0){
      validate(
        need(input$update > 0  && 
               (!is.null(values$display)), "Waiting for data")
      ) 
    }
    
    out_400 <- values$out_400
    # If there aren't fast trials, the "None" label is displayed
    if (is.character(out_400) == TRUE){
      return(out_400)
    } else if (out_400[1] == 0){
      return("None")
    } else {
      return(out_400)
    }
  })
  
  # Number of participants deleted for the accuracy deletion ####
  output$mistakes <- renderPrint({
    # Display a message while waiting for data
    if(input$reset == 0){
      validate(
        need(input$update > 0 , "Waiting for data")
      ) 
    } else if (input$reset > 0){
      validate(
        need(input$update > 0  && 
               (!is.null(values$display)), "Waiting for data")
      ) 
    }
    
    dataset <- values$sbj_accuracy
    num <- c("number" = length(unique(dataset$participant)), 
             "percentage" = round((length(unique(dataset$participant)) / 
               length(unique(values$dataset$participant))) * 100, 2))
    # If there aren't inaccurate participants, the "None" label is displayed
    if (nrow(dataset) == 0){
      return("None")
    } else {
      return(num)
    }
  })
  
  ## fast participants ##########
  output$sbjFast <- renderPrint({
    # Display a message while waiting for data
    if(input$reset == 0){
      validate(
        need(input$update > 0 , "Waiting for data")
      ) 
    } else if (input$reset > 0){
      validate(
        need(input$update > 0  && 
               (!is.null(values$display)), "Waiting for data")
      ) 
    }
    
    sbj_fast <- values$sbj_time
    # If there aren't too fast participants, the "None" label is displayed
    num_fast <- c("number" = length(unique(sbj_fast$participant)), 
                  "percentage" = 
                   round((length(unique(sbj_fast$participant)) / 
                      length(unique(values$dataset$participant))) * 100 , 2))
    if (nrow(sbj_fast) == 0){
      return("None")
    } else {
      return(num_fast)
    }
  })
  
  # Graphic displays IAT ####
  output$distribution <- renderPlot({
    # Display a message while waiting for data
    if(input$reset == 0){
      validate(
        need(input$update > 0 , "Waiting for data")
      ) 
    } else if (input$reset > 0){
      validate(
        need(input$update > 0  && 
               (!is.null(values$display)), "Waiting for data")
      ) 
    }
    dframe <- values$display
    
    dframe <- dframe[, c("participant", "dscore")]
    
    library(ggplot2)
    # points #####
    if(input$graph == 1){
      if(input$point_opts == 1){
        # create the order for displaying participants
        values$type_graph <- "PointDefault"
        start_time <- Sys.time() # it's needed for computing the waiting time for the shiny notification
        dframe <- dframe[order(dframe$participant), ]
        dframe$dscore_cres <- dframe$participant
        dframe$dscore_cres <- as.factor(dframe$dscore_cres)
        # create a variable for teh position of the labels for the effect size according to the number of participants
        coordinates_labels <- ifelse(length(unique(dframe$participant)) < 150, 
                                     nrow(dframe)-1, 
                                     nrow(dframe)-10)
        # prepare the graph (points)
        g_graph <- ggplot(dframe,
                          aes(y = dscore, x = dscore_cres)) +
          geom_point(col = "springgreen4",  size = 2)
        g_graph <- g_graph + scale_x_discrete(name = "Participant",
                                              labels = dframe$participant)
        g_graph <- g_graph + annotate("text", x= coordinates_labels,
                                      y = 0.21, label= "slight",
                                      col = "royalblue" )
        g_graph <- g_graph + annotate("text", x= coordinates_labels,
                                      y = -0.21, label= "slight",
                                      col = "royalblue" )
        g_graph <- g_graph + annotate("text", x= coordinates_labels,
                                      y = 0.40, label= "moderate",
                                      col = "orchid3" )
        g_graph <- g_graph + annotate("text", x= coordinates_labels,
                                      y = -0.41, label= "moderate",
                                      col = "orchid3" )
        g_graph <- g_graph + annotate("text", x= coordinates_labels,
                                      y = 0.70, label= "strong",
                                      col = "slateblue4" )
        g_graph <- g_graph + annotate("text", x= coordinates_labels,
                                      y = -0.70, label= "strong",
                                      col = "slateblue4" )
        stop_time <- Sys.time() # it's needed for computing the waiting time for the shiny notification
        
      } else if(input$point_opts == 2){
        # create the order for displaying participants
        values$type_graph <- "PointCrescent"
        start_time <- Sys.time() # it's needed for computing the waiting time for the shiny notification
        dframe <- dframe[order(dframe$dscore), ]
        dframe$dscore_cres <- 1:nrow(dframe)
        dframe$dscore_cres <- as.factor(dframe$dscore_cres)
        coordinates_labels <- ifelse(length(unique(dframe$participant)) < 150, 
                                     nrow(dframe)-1, nrow(dframe)-10 )
        # prepare the graph (points)
        g_graph <- ggplot(dframe,
                          aes(y = dscore, x = dscore_cres)) +
          geom_point(col = "springgreen4",  size = 2)
        g_graph <- g_graph + scale_x_discrete(name = "Participant",
                                              labels = dframe$participant)
        g_graph <- g_graph + annotate("text", x= coordinates_labels,
                                      y = 0.21, label= "slight",
                                      col = "royalblue" )
        g_graph <- g_graph + annotate("text", x= coordinates_labels,
                                      y = -0.21, label= "slight",
                                      col = "royalblue" )
        g_graph <- g_graph + annotate("text", x= coordinates_labels,
                                      y = 0.40, label= "moderate",
                                      col = "orchid3" )
        g_graph <- g_graph + annotate("text", x= coordinates_labels,
                                      y = -0.41, label= "moderate",
                                      col = "orchid3" )
        g_graph <- g_graph + annotate("text", x= coordinates_labels,
                                      y = 0.70, label= "strong",
                                      col = "slateblue4" )
        g_graph <- g_graph + annotate("text", x= coordinates_labels
                                      ,
                                      y = -0.70, label= "strong",
                                      col = "slateblue4" )
        stop_time <- Sys.time() # it's needed for computing the waiting time for the shiny notification
        
      } else if(input$point_opts == 3){
        # create the order for displaying participants
        values$type_graph <- "PointDecrescent"
        start_time <- Sys.time() # it's needed for computing the waiting time for the shiny notification
        dframe <- dframe[order(dframe$dscore, decreasing = T), ]
        dframe$dscore_cres <- 1:nrow(dframe)
        dframe$dscore_cres <- as.factor(dframe$dscore_cres)
        coordinates_labels <- ifelse(length(unique(dframe$participant)) < 150, 
                                     (nrow(dframe)-(nrow(dframe)-2)), 
                                     (nrow(dframe)-(nrow(dframe)-15)) )
        # prepare the graph (points)
        g_graph <- ggplot(dframe,
                          aes(y = dscore, x = dscore_cres)) +
          geom_point(col = "springgreen4",  size = 2)
        g_graph <- g_graph + scale_x_discrete(name = "Participant",
                                              labels = dframe$participant)
        g_graph <- g_graph + annotate("text", x= coordinates_labels,
                                      y = 0.21, label= "slight",
                                      col = "royalblue" )
        g_graph <- g_graph + annotate("text", x= coordinates_labels,
                                      y = -0.21, label= "slight",
                                      col = "royalblue" )
        g_graph <- g_graph + annotate("text", x= coordinates_labels,
                                      y = 0.40, label= "moderate",
                                      col = "orchid3" )
        g_graph <- g_graph + annotate("text", x= coordinates_labels,
                                      y = -0.41, label= "moderate",
                                      col = "orchid3" )
        g_graph <- g_graph + annotate("text", x= coordinates_labels,
                                      y = 0.70, label= "strong",
                                      col = "slateblue4" )
        g_graph <- g_graph + annotate("text", x= coordinates_labels,
                                      y = -0.70, label= "strong",
                                      col = "slateblue4" )
        stop_time <- Sys.time() # it's needed for computing the waiting time for the shiny notification
        
      }
      
      # graph specifications common to all the point graphs
      g_graph <- g_graph  + theme_classic()
      if (nrow(dframe) > 70){
        g_graph <- g_graph + theme(
          axis.text.x = element_blank())
      } else {
        g_graph <- g_graph + theme(axis.text.x = element_text(size = 5))
      }
      
      g_graph <- g_graph + geom_abline(slope = 0, intercept = 0.15,
                                       col = "royalblue",lty = 2, size = .50)
      g_graph <- g_graph + geom_abline(slope = 0, intercept = -0.15,
                                       col = "royalblue",lty = 2, size = .50)
      g_graph <- g_graph + geom_abline(slope = 0, intercept = 0.35,
                                       col = "orchid3",lty = 2, size = .50)
      g_graph <- g_graph + geom_abline(slope = 0, intercept = -0.35,
                                       col = "orchid3",lty = 2, size = .50)
      g_graph <- g_graph + geom_abline(slope = 0, intercept = 0.65,
                                       col = "slateblue4",lty = 2, size = .50)
      g_graph <- g_graph + geom_abline(slope = 0, intercept = -0.65,
                                       col = "slateblue4",lty = 2, size = .50)
      g_graph <- g_graph + ylab("D-score")
    }
    ## histogram ####
    else if(input$graph == 2){
      # Histogram graph
      values$type_graph <- "Histogram"
      start_time <- Sys.time() # it's needed for computing the waiting time for the shiny notification
      g_graph <- ggplot(dframe,
                        aes(x = dscore)) +
        geom_histogram(bins = input$num.bin, col = "royalblue",  # number of bins depends on users' configuration
                       fill = "royalblue",
                       alpha = .50)
      g_graph <- g_graph  + theme_classic()
      g_graph <- g_graph + geom_vline( xintercept = 0.15,
                                       col = "royalblue",lty = 2, size = .50)
      g_graph <- g_graph + geom_vline( xintercept = -0.15,
                                       col = "royalblue",lty = 2, size = .50)
      g_graph <- g_graph + geom_vline( xintercept = 0.35,
                                       col = "orchid3",lty = 2, size = .50)
      g_graph <- g_graph + geom_vline( xintercept = -0.35,
                                       col = "orchid3",lty = 2, size = .50)
      g_graph <- g_graph + geom_vline( xintercept = 0.65,
                                       col = "slateblue4",lty = 2, size = .50)
      g_graph <- g_graph + geom_vline( xintercept = -0.65,
                                       col = "slateblue4",lty = 2, size = .50)
      g_graph <- g_graph + annotate("text", x= 0.20,
                                    y = -0.20, label= "slight", 
                                    col = "royalblue" )
      g_graph <- g_graph + annotate("text", x= -0.10,
                                    y = -0.20, label= "slight", 
                                    col = "royalblue" )
      g_graph <- g_graph + annotate("text", x= 0.43,
                                    y = -0.20, label= "moderate",
                                    col = "orchid3" )
      g_graph <- g_graph + annotate("text", x= -0.25,
                                    y = -0.20, label= "moderate",
                                    col = "orchid3" )
      g_graph <- g_graph + annotate("text", x= -0.58,
                                    y = -0.20, label= "strong",
                                    col = "slateblue4" )
      g_graph <- g_graph + annotate("text", x= 0.71,
                                    y = -0.20, label= "strong",
                                    col = "slateblue4" )
      g_graph <- g_graph + xlab("D-score") + theme(axis.title.y = element_blank())
      stop_time <- Sys.time() # it's needed for computing the waiting time for the shiny notification
      
    } else if(input$graph == 3){ # density ####
      # Density graph
      values$type_graph <- "Density"
      start_time <- Sys.time() # it's needed for computing the waiting time for the shiny notification
      g_graph <- ggplot(dframe,
                        aes(x = dscore)) +
        geom_density(alpha = 0.70, fill = "seagreen" , col = "seagreen")
      g_graph <- g_graph  + theme_classic()
      g_graph <- g_graph + geom_vline( xintercept = 0.15,
                                       col = "royalblue",lty = 2, size = .50)
      g_graph <- g_graph + geom_vline( xintercept = -0.15,
                                       col = "royalblue",lty = 2, size = .50)
      g_graph <- g_graph + geom_vline( xintercept = 0.35,
                                       col = "orchid3",lty = 2, size = .50)
      g_graph <- g_graph + geom_vline( xintercept = -0.35,
                                       col = "orchid3",lty = 2, size = .50)
      g_graph <- g_graph + geom_vline( xintercept = 0.65,
                                       col = "slateblue4",lty = 2, size = .50)
      g_graph <- g_graph + geom_vline( xintercept = -0.65,
                                       col = "slateblue4",lty = 2, size = .50)
      g_graph <- g_graph + annotate("text", x= 0.20,
                                    y = -0.20, label= "slight", 
                                    col = "royalblue" )
      g_graph <- g_graph + annotate("text", x= -0.10,
                                    y = -0.20, label= "slight", 
                                    col = "royalblue" )
      g_graph <- g_graph + annotate("text", x= 0.43,
                                    y = -0.20, label= "moderate",
                                    col = "orchid3" )
      g_graph <- g_graph + annotate("text", x= -0.25,
                                    y = -0.20, label= "moderate",
                                    col = "orchid3" )
      g_graph <- g_graph + annotate("text", x= -0.58,
                                    y = -0.20, label= "strong",
                                    col = "slateblue4" )
      g_graph <- g_graph + annotate("text", x= 0.71,
                                    y = -0.20, label= "strong",
                                    col = "slateblue4" )
      g_graph <- g_graph + xlab("D-score") + theme(axis.title.y = element_blank())
      stop_time <- Sys.time() # it's needed for computing the waiting time for the shiny notification
    } else if (input$graph == 4){  # density + histogram #####
      # density + histogram graph
      values$type_graph <- "HistDens"
      start_time <- Sys.time() # it's needed for computing the waiting time for the shiny notification
      g_graph <- ggplot(dframe,
                        aes(x = dscore)) +
        geom_histogram(aes(y=..density..), bins = input$num.bin, # number of bins depends on users' configuration
                       col = "royalblue",
                       fill = "royalblue", alpha = .50)
      g_graph <- g_graph  + theme_classic()
      g_graph <- g_graph + geom_density(alpha = .70, col = "seagreen", 
                                        fill = "seagreen", trim =F)
      g_graph <- g_graph + geom_vline( xintercept = 0.15,
                                       col = "royalblue", lty = 2, size = .70)
      g_graph <- g_graph + geom_vline( xintercept = -0.15,
                                       col = "royalblue", lty = 2, size = .70)
      g_graph <- g_graph + geom_vline( xintercept = 0.35,
                                       col = "orchid3", lty = 2, size = .70)
      g_graph <- g_graph + geom_vline( xintercept = -0.35,
                                       col = "orchid3", lty = 2, size = .70)
      g_graph <- g_graph + geom_vline( xintercept = 0.65,
                                       col = "slateblue4", lty = 2, size = .70)
      g_graph <- g_graph + geom_vline( xintercept = -0.65,
                                       col = "slateblue4", lty = 2, size = .70)
      
      g_graph <- g_graph + annotate("text", x= 0.20,
                                    y = -0.20, label= "slight", 
                                    col = "royalblue" )
      g_graph <- g_graph + annotate("text", x= -0.10,
                                    y = -0.20, label= "slight", 
                                    col = "royalblue" )
      g_graph <- g_graph + annotate("text", x= 0.43,
                                    y = -0.20, label= "moderate",
                                    col = "orchid3" )
      g_graph <- g_graph + annotate("text", x= -0.25,
                                    y = -0.20, label= "moderate",
                                    col = "orchid3" )
      g_graph <- g_graph + annotate("text", x= -0.58,
                                    y = -0.20, label= "strong",
                                    col = "slateblue4" )
      g_graph <- g_graph + annotate("text", x= 0.71,
                                    y = -0.20, label= "strong",
                                    col = "slateblue4" )
      g_graph <- g_graph + xlab("D-score") + theme(axis.title.y = element_blank())
      stop_time <- Sys.time() # it's needed for computing the waiting time for the shiny notification
    }
    values$g_graph <- g_graph # safe the graph in the reactive object
    sec_time <- as.numeric(stop_time - start_time) + 0.02 # compute the time needed for making each of the graph
    # prepare the shiny notification according to the time for doing the grpah
    withProgress(message = ifelse(input$update == 1,
                                  "Computing...", "Updating..."), 
                 value = input$update, {
                   for (i in 1:15) {
                     incProgress(1/15)
                     Sys.sleep(sec_time)
                   }
                 })
    
    return(values$g_graph)
    
  })
  
  # SC-IAT graphic output ----
  output$sc_distribution <- renderPlot({
    if(input$sc_reset == 0){
      validate(
        need(input$sc_update > 0 , "Waiting for data")
      ) 
    } else if (input$sc_reset > 0){
      validate(
        need(input$sc_update > 0  && 
               (!is.null(values$calc_d)), "Waiting for data")
      ) 
    }
    d <- values$calc_d
    
    d <- d[, c("participant", "dsciat")]
    
    library(ggplot2)
    # points #####
    if(input$graph_sciat == 1){
      if(input$point_opts_sc == 1){
        values$type_graph_sc <- "PointDefault"
        start_time_sc <- Sys.time()
        d <- d[order(d$participant), ]
        d$dsciat_cres <- d$participant
        d$dsciat_cres <- as.factor(d$dsciat_cres)
        coordinates_labels <- ifelse(length(unique(d$participant)) < 150, 
                                     nrow(d) - 1, 
                                     nrow(d) - 10)
        sc_graph <- ggplot(d,
                           aes(y = dsciat, x = dsciat_cres)) +
          geom_point(col = "springgreen4",  size = 2)
        sc_graph <- sc_graph + scale_x_discrete(name = "Participant",
                                                labels = d$participant)
        # sc_graph <- sc_graph + annotate("text", x= coordinates_labels,
        #                                 y = 0.21, label= "slight",
        #                                 col = "royalblue" )
        # sc_graph <- sc_graph + annotate("text", x= coordinates_labels,
        #                                 y = -0.21, label= "slight",
        #                                 col = "royalblue" )
        # sc_graph <- sc_graph + annotate("text", x= coordinates_labels,
        #                                 y = 0.40, label= "moderate",
        #                                 col = "orchid3" )
        # sc_graph <- sc_graph + annotate("text", x= coordinates_labels,
        #                                 y = -0.41, label= "moderate",
        #                                 col = "orchid3" )
        # sc_graph <- sc_graph + annotate("text", x= coordinates_labels,
        #                                 y = 0.70, label= "strong",
        #                                 col = "slateblue4" )
        # sc_graph <- sc_graph + annotate("text", x= coordinates_labels,
        #                                 y = -0.70, label= "strong",
        #                                 col = "slateblue4" )
        stop_time_sc <- Sys.time()
        
      } else if(input$point_opts_sc == 2){
        values$type_graph_sc <- "PointCrescent"
        start_time_sc <- Sys.time()
        d <- d[order(d$dsciat), ]
        d$dsciat_cres <- 1:nrow(d)
        d$dsciat_cres <- as.factor(d$dsciat_cres)
        coordinates_labels <- ifelse(length(unique(d$participant)) < 150, 
                                     nrow(d) - 1, nrow(d)-10 )
        sc_graph <- ggplot(d,
                           aes(y = dsciat, x = dsciat_cres)) +
          geom_point(col = "springgreen4",  size = 2)
        sc_graph <- sc_graph + scale_x_discrete(name = "Participant",
                                                labels = d$participant)
        # sc_graph <- sc_graph + annotate("text", x= coordinates_labels,
        #                                 y = 0.21, label= "slight",
        #                                 col = "royalblue" )
        # sc_graph <- sc_graph + annotate("text", x= coordinates_labels,
        #                                 y = -0.21, label= "slight",
        #                                 col = "royalblue" )
        # sc_graph <- sc_graph + annotate("text", x= coordinates_labels,
        #                                 y = 0.40, label= "moderate",
        #                                 col = "orchid3" )
        # sc_graph <- sc_graph + annotate("text", x= coordinates_labels,
        #                                 y = -0.41, label= "moderate",
        #                                 col = "orchid3" )
        # sc_graph <- sc_graph + annotate("text", x= coordinates_labels,
        #                                 y = 0.70, label= "strong",
        #                                 col = "slateblue4" )
        # sc_graph <- sc_graph + annotate("text", x= coordinates_labels
        #                                 ,
        #                                 y = -0.70, label= "strong",
        #                                 col = "slateblue4" )
        stop_time_sc <- Sys.time()
        
      } else if (input$point_opts_sc == 3){
        values$type_graph_sc <- "PointDecrescent"
        start_time_sc <- Sys.time()
        d <- d[order(d$dsciat, decreasing = TRUE), ]
        d$dsciat_cres <- 1:nrow(d)
        d$dsciat_cres <- as.factor(d$dsciat_cres)
        coordinates_labels <- ifelse(length(unique(d$participant)) < 150, 
                                     (nrow(d) - (nrow(d) - 2)), 
                                     (nrow(d)- (nrow(d) - 15)) )
        sc_graph <- ggplot(d,
                           aes(y = dsciat, x = dsciat_cres)) +
          geom_point(col = "springgreen4",  size = 2)
        sc_graph <- sc_graph + scale_x_discrete(name = "Participant",
                                                labels = d$participant)
        # sc_graph <- sc_graph + annotate("text", x= coordinates_labels,
        #                                 y = 0.21, label= "slight",
        #                                 col = "royalblue" )
        # sc_graph <- sc_graph + annotate("text", x= coordinates_labels,
        #                                 y = -0.21, label= "slight",
        #                                 col = "royalblue" )
        # sc_graph <- sc_graph + annotate("text", x= coordinates_labels,
        #                                 y = 0.40, label= "moderate",
        #                                 col = "orchid3" )
        # sc_graph <- sc_graph + annotate("text", x= coordinates_labels,
        #                                 y = -0.41, label= "moderate",
        #                                 col = "orchid3" )
        # sc_graph <- sc_graph + annotate("text", x= coordinates_labels,
        #                                 y = 0.70, label= "strong",
        #                                 col = "slateblue4" )
        # sc_graph <- sc_graph + annotate("text", x= coordinates_labels,
        #                                 y = -0.70, label= "strong",
        #                                 col = "slateblue4" )
        stop_time_sc <- Sys.time()
        
      }
      
      sc_graph <- sc_graph  + theme_classic()
      if (nrow(d) > 70){
        sc_graph <- sc_graph + theme(
          axis.text.x = element_blank())
        } else {
          sc_graph <- sc_graph + theme(axis.text.x = element_text(size = 5))
        }
      

      # sc_graph <- sc_graph + geom_abline(slope = 0, intercept = 0.15,
      #                                    col = "royalblue",lty = 2, size = .50)
      # sc_graph <- sc_graph + geom_abline(slope = 0, intercept = -0.15,
      #                                    col = "royalblue",lty = 2, size = .50)
      # sc_graph <- sc_graph + geom_abline(slope = 0, intercept = 0.35,
      #                                    col = "orchid3",lty = 2, size = .50)
      # sc_graph <- sc_graph + geom_abline(slope = 0, intercept = -0.35,
      #                                    col = "orchid3",lty = 2, size = .50)
      # sc_graph <- sc_graph + geom_abline(slope = 0, intercept = 0.65,
      #                                    col = "slateblue4",lty = 2, size = .50)
      # sc_graph <- sc_graph + geom_abline(slope = 0, intercept = -0.65,
      #                                    col = "slateblue4",lty = 2, size = .50)
      sc_graph <- sc_graph + ylab("D-score")
    } else if (input$graph_sciat == 2){ ## histogram ####
      values$type_graph_sc <- "Histogram"
      start_time_sc <- Sys.time()
      sc_graph <- ggplot(d,
                         aes(x = dsciat)) +
        geom_histogram(bins = input$num.bin_sc, col = "royalblue", 
                       fill = "royalblue",
                       alpha = .50)
      sc_graph <- sc_graph  + theme_classic()
      # sc_graph <- sc_graph + geom_vline( xintercept = 0.15,
      #                                    col = "royalblue",lty = 2, size = .50)
      # sc_graph <- sc_graph + geom_vline( xintercept = -0.15,
      #                                    col = "royalblue",lty = 2, size = .50)
      # sc_graph <- sc_graph + geom_vline( xintercept = 0.35,
      #                                    col = "orchid3",lty = 2, size = .50)
      # sc_graph <- sc_graph + geom_vline( xintercept = -0.35,
      #                                    col = "orchid3",lty = 2, size = .50)
      # sc_graph <- sc_graph + geom_vline( xintercept = 0.65,
      #                                    col = "slateblue4",lty = 2, size = .50)
      # sc_graph <- sc_graph + geom_vline( xintercept = -0.65,
      #                                    col = "slateblue4",lty = 2, size = .50)
      # sc_graph <- sc_graph + annotate("text", x= 0.20,
      #                                 y = -0.20, label= "slight", 
      #                                 col = "royalblue" )
      # sc_graph <- sc_graph + annotate("text", x= -0.10,
      #                                 y = -0.20, label= "slight", 
      #                                 col = "royalblue" )
      # sc_graph <- sc_graph + annotate("text", x= 0.43,
      #                                 y = -0.20, label= "moderate",
      #                                 col = "orchid3" )
      # sc_graph <- sc_graph + annotate("text", x= -0.25,
      #                                 y = -0.20, label= "moderate",
      #                                 col = "orchid3" )
      # sc_graph <- sc_graph + annotate("text", x= -0.58,
      #                                 y = -0.20, label= "strong",
      #                                 col = "slateblue4" )
      # sc_graph <- sc_graph + annotate("text", x= 0.71,
      #                                 y = -0.20, label= "strong",
      #                                 col = "slateblue4" )
      sc_graph <- sc_graph + xlab("D-score") + theme(axis.title.y = element_blank())
      stop_time_sc <- Sys.time()
      
    }
    # density ####
    else if (input$graph_sciat == 3){
      values$type_graph_sc <- "Density"
      start_time_sc <- Sys.time()
      sc_graph <- ggplot(d,
                         aes(x = dsciat)) +
        geom_density(alpha = 0.70, fill = "seagreen" , col = "seagreen")
      sc_graph <- sc_graph  + theme_classic()
      # sc_graph <- sc_graph + geom_vline( xintercept = 0.15,
      #                                    col = "royalblue",lty = 2, size = .50)
      # sc_graph <- sc_graph + geom_vline( xintercept = -0.15,
      #                                    col = "royalblue",lty = 2, size = .50)
      # sc_graph <- sc_graph + geom_vline( xintercept = 0.35,
      #                                    col = "orchid3",lty = 2, size = .50)
      # sc_graph <- sc_graph + geom_vline( xintercept = -0.35,
      #                                    col = "orchid3",lty = 2, size = .50)
      # sc_graph <- sc_graph + geom_vline( xintercept = 0.65,
      #                                    col = "slateblue4",lty = 2, size = .50)
      # sc_graph <- sc_graph + geom_vline( xintercept = -0.65,
      #                                    col = "slateblue4",lty = 2, size = .50)
      # sc_graph <- sc_graph + annotate("text", x= 0.20,
      #                                 y = -0.20, label= "slight", 
      #                                 col = "royalblue" )
      # sc_graph <- sc_graph + annotate("text", x= -0.10,
      #                                 y = -0.20, label= "slight", 
      #                                 col = "royalblue" )
      # sc_graph <- sc_graph + annotate("text", x= 0.43,
      #                                 y = -0.20, label= "moderate",
      #                                 col = "orchid3" )
      # sc_graph <- sc_graph + annotate("text", x= -0.25,
      #                                 y = -0.20, label= "moderate",
      #                                 col = "orchid3" )
      # sc_graph <- sc_graph + annotate("text", x= -0.58,
      #                                 y = -0.20, label= "strong",
      #                                 col = "slateblue4" )
      # sc_graph <- sc_graph + annotate("text", x= 0.71,
      #                                 y = -0.20, label= "strong",
      #                                 col = "slateblue4" )
      # sc_graph <- sc_graph + xlab("D-score") + theme(axis.title.y = element_blank())
      stop_time_sc <- Sys.time()
    }
    # density + histogram #####
    else if(input$graph_sciat == 4){
      values$type_graph_sc <- "HistDens"
      start_time_sc <- Sys.time()
      sc_graph <- ggplot(d,
                         aes(x = dsciat)) +
        geom_histogram(aes(y=..density..), bins = input$num.bin_sc, 
                       col = "royalblue",
                       fill = "royalblue", alpha = .50)
      sc_graph <- sc_graph  + theme_classic()
      sc_graph <- sc_graph + geom_density(alpha = .70, col = "seagreen", 
                                          fill = "seagreen", trim =F)
      # sc_graph <- sc_graph + geom_vline( xintercept = 0.15,
      #                                    col = "royalblue", lty = 2, size = .70)
      # sc_graph <- sc_graph + geom_vline( xintercept = -0.15,
      #                                    col = "royalblue", lty = 2, size = .70)
      # sc_graph <- sc_graph + geom_vline( xintercept = 0.35,
      #                                    col = "orchid3", lty = 2, size = .70)
      # sc_graph <- sc_graph + geom_vline( xintercept = -0.35,
      #                                    col = "orchid3", lty = 2, size = .70)
      # sc_graph <- sc_graph + geom_vline( xintercept = 0.65,
      #                                    col = "slateblue4", lty = 2, size = .70)
      # sc_graph <- sc_graph + geom_vline( xintercept = -0.65,
      #                                    col = "slateblue4", lty = 2, size = .70)
      # 
      # sc_graph <- sc_graph + annotate("text", x= 0.20,
      #                                 y = -0.20, label= "slight", 
      #                                 col = "royalblue" )
      # sc_graph <- sc_graph + annotate("text", x= -0.10,
      #                                 y = -0.20, label= "slight", 
      #                                 col = "royalblue" )
      # sc_graph <- sc_graph + annotate("text", x= 0.43,
      #                                 y = -0.20, label= "moderate",
      #                                 col = "orchid3" )
      # sc_graph <- sc_graph + annotate("text", x= -0.25,
      #                                 y = -0.20, label= "moderate",
      #                                 col = "orchid3" )
      # sc_graph <- sc_graph + annotate("text", x= -0.58,
      #                                 y = -0.20, label= "strong",
      #                                 col = "slateblue4" )
      # sc_graph <- sc_graph + annotate("text", x= 0.71,
      #                                 y = -0.20, label= "strong",
      #                                 col = "slateblue4" )
      sc_graph <- sc_graph + xlab("D-score") + theme(axis.title.y = element_blank())
      stop_time_sc <- Sys.time()
    }
    values$sc_graph <- sc_graph
    sec_time <- as.numeric(stop_time_sc - start_time_sc) + 0.02
    
    withProgress(message = ifelse(input$sc_update == 1,
                                  "Computing...", "Updating..."), 
                 value = input$sc_update, {
                   for (i in 1:15) {
                     incProgress(1/15)
                     Sys.sleep(sec_time)
                   }
                 })
    return(values$sc_graph)
  })
  
  # single graph indicator SC-IAT -----
  output$click_info_sc <- renderPrint({
    if(input$sc_reset == 0){
      validate(
        need(input$sc_plot1_click , "Click on a point")
      ) 
    } else if (input$sc_reset > 0){
      validate(
        need(input$sc_plot1_click, "Click on a point")
      ) 
    }
    
    
    d <- values$calc_d
    
    d <- d[, c("participant", "dsciat")]
    
    if(input$point_opts_sc == 1){
      d <- d[order(d$participant), ]
      d$dsciat_cres <- d$participant
      d$dsciat_cres <- as.factor(d$dsciat_cres)
      
    } else if(input$point_opts_sc == 2){
      d <- d[order(d$dsciat), ]
      d$dsciat_cres <- 1:nrow(d)
      d$dsciat_cres <- as.factor(d$dsciat_cres)
      
    } else if(input$point_opts_sc == 3){
      d <- d[order(d$dsciat, decreasing = T), ]
      d$dsciat_cres <- 1:nrow(d)
      d$dsciat_cres <- as.factor(d$dsciat_cres)
      
    }
    single_id <- nearPoints(d, input$sc_plot1_click, addDist = F)
    rownames(single_id) <- NULL
    single_id <- single_id[,1:2]
    validate(
      need(nrow(single_id) != 0, "Click on a point")
    )
    single_id
  })
  
  # area indicator SC-IAT ----
  output$brush_info_sc <- renderPrint({
    if(input$sc_reset == 0){
      validate(
        need(input$sc_plot1_brush , "Highlight graph area")
      ) 
    } else if (input$sc_reset > 0){
      validate(
        need(input$sc_plot1_brush, "Highlight graph area")
      ) 
    }
    
    d <- values$calc_d
    
    d <- d[, c("participant", "dsciat")]
    
    if(input$point_opts_sc == 1){
      d <- d[order(d$participant), ]
      d$dsciat_cres <- d$participant
      d$dsciat_cres <- as.factor(d$dsciat_cres)
      
    } else if(input$point_opts_sc == 2){
      d <- d[order(d$dsciat), ]
      d$dsciat_cres <- 1:nrow(d)
      d$dsciat_cres <- as.factor(d$dsciat_cres)
      
    } else if(input$point_opts_sc == 3){
      d <- d[order(d$dsciat, decreasing = T), ]
      d$dsciat_cres <- 1:nrow(d)
      d$dsciat_cres <- as.factor(d$dsciat_cres)
      
    }
    brush_id <- brushedPoints(d, input$sc_plot1_brush)
    validate(
      need(nrow(brush_id) != 0, "Highlight graph area")
    )
    rownames(brush_id) <- NULL
    brush_id[,1:2]
  })
  
  # histogram indicator SC-IAT ----
  output$info_hist_sc <- renderPrint({
    
    if(input$sc_reset == 0){
      validate(
        need(input$sc_plot1_brush , "Highlight graph area")
      )
    } else if (input$sc_reset > 0){
      validate(
        need(input$sc_plot1_brush, "Highlight graph area")
      )
    }
    
    d <- values$calc_d
    
    d <- d[, c("participant", "dsciat")]
    d$new <- (d$dsciat > input$sc_plot1_brush$xmin &
                d$dsciat < input$sc_plot1_brush$xmax)
    d_plot <- d[which(d$new == T), ]
    d_plot <- d_plot[order(d_plot$dsciat), ]
    rownames(d_plot) <- NULL
    d_plot <- d_plot[, 1:2]
    d_plot
    
  })
  
  # toggle state for the download graph button IAT ----
  # (become active only when the values$g_graph object has something inside) 
  observe({
    shinyjs::toggleState("down_plot", !(is.null(values$g_graph)))
  })
  
  # single graph indicator IAT ####
  output$click_info <- renderPrint({
    # Display a message while waiting for data
    if(input$reset == 0){
      validate(
        need(input$plot1_click , "Click on a point")
      ) 
    } else if (input$reset > 0){
      validate(
        need(input$plot1_click, "Click on a point")
      ) 
    }
    
    dframe <- values$display
    
    dframe <- dframe[, c("participant", "dscore")]
    # specificy the order of participants for getting the correct point in the 
    # dataframe
    if(input$point_opts == 1){
      dframe <- dframe[order(dframe$participant), ]
      dframe$dscore_cres <- dframe$participant
      dframe$dscore_cres <- as.factor(dframe$dscore_cres)
      
    } else if(input$point_opts == 2){
      dframe <- dframe[order(dframe$dscore), ]
      dframe$dscore_cres <- 1:nrow(dframe)
      dframe$dscore_cres <- as.factor(dframe$dscore_cres)
      
    } else if(input$point_opts == 3){
      dframe <- dframe[order(dframe$dscore, decreasing = T), ]
      dframe$dscore_cres <- 1:nrow(dframe)
      dframe$dscore_cres <- as.factor(dframe$dscore_cres)
      
    }
    # select the participant corresponding to the point
    single_id <- nearPoints(dframe, input$plot1_click, addDist = F)
    rownames(single_id) <- NULL
    single_id <- single_id[,1:2]
    validate(
      need(nrow(single_id) != 0, "Click on a point")
    )
    single_id
  })
  
  
  # multiple indicator in graph IAT ----
  output$brush_info <- renderPrint({
    # Display a message while waiting for data
    if(input$reset == 0){
      validate(
        need(input$plot1_brush , "Highlight graph area")
      ) 
    } else if (input$reset > 0){
      validate(
        need(input$plot1_brush, "Highlight graph area")
      ) 
    }
    
    dframe <- values$display
    
    dframe <- dframe[, c("participant", "dscore")]
    # specificy the order of participants for getting the correct point iun the dataframe
    if (input$point_opts == 1){
      dframe <- dframe[order(dframe$participant), ]
      dframe$dscore_cres <- dframe$participant
      dframe$dscore_cres <- as.factor(dframe$dscore_cres)
      
    } else if (input$point_opts == 2) {
      dframe <- dframe[order(dframe$dscore), ]
      dframe$dscore_cres <- 1:nrow(dframe)
      dframe$dscore_cres <- as.factor(dframe$dscore_cres)
      
    } else if (input$point_opts == 3) {
      dframe <- dframe[order(dframe$dscore, decreasing = T), ]
      dframe$dscore_cres <- 1:nrow(dframe)
      dframe$dscore_cres <- as.factor(dframe$dscore_cres)
      
    }
    # select the participants in the selected area
    brush_id <- brushedPoints(dframe, input$plot1_brush)
    validate(
      need(nrow(brush_id) != 0, "Highlight graph area")
    )
    rownames(brush_id) <- NULL
    brush_id[,1:2]
  })
  
  # ## histogram indicator IAT #####
  output$info_hist <- renderPrint({
    # Display a message while waiting for data
    if(input$reset == 0){
      validate(
        need(input$plot1_brush , "Highlight graph area")
      )
    } else if (input$reset > 0){
      validate(
        need(input$plot1_brush, "Highlight graph area")
      )
    }
    
    dframe <- values$display
    
    dframe <- dframe[, c("participant", "dscore")]
    # select the minimum and the maximu of the selected area and theri corresponding points in teh dataframe
    dframe$new <- (dframe$dscore > input$plot1_brush$xmin &
                     dframe$dscore < input$plot1_brush$xmax)
    d_plot <- dframe[which(dframe$new == T), ]
    d_plot <- d_plot[order(d_plot$dscore), ]
    rownames(d_plot) <- NULL
    d_plot <- d_plot[, 1:2]
    d_plot
    
  })
  ## test pratice reliability ----
  output$pt_reliability <- renderPrint({
    # Display a message while waiting for data
    if(input$reset == 0){
      validate(
        need(input$update > 0 , "Waiting for data")
      ) 
    } else if (input$reset > 0){
      validate(
        need(input$update > 0  && 
               (!is.null(values$display)), "Waiting for data")
      ) 
    }
    test_practice_rel <- values$test_practice_rel
    round(test_practice_rel[2,1],2)
  })
  
  # Descriptive statistics IAT ----
  # mean block ----
  output$mean.block <- renderPrint({
    # Display a message while waiting for data
    if(input$reset == 0){
      validate(
        need(input$update > 0 , "Waiting for data")
      ) 
    } else if (input$reset > 0){
      validate(
        need(input$update > 0  && 
               (!is.null(values$display)), "Waiting for data")
      ) 
    }
    # Compute the average response time in each condition
    dataset <- values$desc_stats
    latency_descript <- with(dataset, 
                             aggregate(latency_cor, 
                                       by = list(Condition), 
                                       FUN = summary))
    # Compute the average response time in the pratice and test blocks
    latency_descript <- rbind(latency_descript, 
                              with(dataset, 
                                   aggregate(latency_cor, 
                                             by = list(block_pool), 
                                             FUN = summary)))
    # Compute the average response time in each block
    latency_descript <- rbind(latency_descript,
                              with(dataset, 
                                   aggregate(latency_cor, 
                                             by = list(blockR), 
                                             FUN = summary)))
    
    colnames(latency_descript) <- gsub('x', '', 
                                       colnames(latency_descript))
    colnames(latency_descript)[1] <- ""
    for(i in 2:ncol(latency_descript)){
      latency_descript[,i] <- round(latency_descript[,i], 2)
    }
    latency_descript
  })
  
  
  # accuracy block IAT ----
  output$accuracy_block <- renderPrint({
    # Display a message while waiting for data
    if(input$reset == 0){
      validate(
        need(input$update > 0 , "Waiting for data")
      ) 
    } else if (input$reset > 0){
      validate(
        need(input$update > 0  && 
               (!is.null(values$display)), "Waiting for data")
      ) 
    }
    dataset <- values$desc_stats
    # Compute the proportion of correct responses in each condition
    accuracy_descript <- with(dataset, 
                              aggregate(correct, 
                                        by = list(Condition), 
                                        FUN = mean))
    # Compute the proportion of correct responses in pratice and test blocks
    accuracy_descript <- rbind(accuracy_descript, 
                               with(dataset, 
                                    aggregate(correct, 
                                              by = list(block_pool), 
                                              FUN = mean)))
    # Compute the proportion of correct responses in each block
    accuracy_descript <- rbind(accuracy_descript, 
                               with(dataset, 
                                    aggregate(correct, 
                                              by = list(blockR), 
                                              FUN = mean)))
    
    colnames(accuracy_descript) <- c("", "Proportion_correct")
    accuracy_descript$Proportion_correct <- round(
      accuracy_descript$Proportion_correct, 2)
    accuracy_descript
  })
  
  #  DOWNLOAD Results IAT ----
  output$downloadData <- downloadHandler(
    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
      paste("ShinyAPPDscore",input$sel_d, ".csv", sep = "")
    },
    content = function(file) {
      sbj_data_wide <- values$descript_data
      write.table(sbj_data_wide, file, sep = ",",
                  row.names = FALSE)
    }
  )
  #
  
  # reset app IAT ----
  observeEvent(input$reset | !is.null(values$alert), 
               {
                 shinyjs::reset("Dapp")
                 values$ready <-NULL
                 values$dataset <-NULL
                 values$descript <-NULL
                 values$data <- NULL
                 values$display <- NULL
                 values$out_400 <- NULL
                 values$sbj_accuracy <- NULL
                 values$sbj_time <- NULL
                 values$test_practice_rel <- NULL
                 values$g_graph <- NULL
                 
               })  
  
  # Download graphs IAT ----
  output$down_plot <- downloadHandler(
    filename = function(){
      paste(values$type_graph, "Dscore",input$sel_d, ".pdf", sep = "")
    }, 
    content = function(file){
      graph <- values$g_graph + theme(plot.margin=unit(c(1.2,1.2,1.2,1.2),"cm"))
      ggsave(file, plot = graph, width = 15, height = 7)
      
    }
  )
  
  # Download results SC-IAT
  output$downloadSciat <- downloadHandler(
    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
      paste("Dscore_SCIAT", ".csv", sep = "")
    },
    content = function(file) {
      calc_d <- values$calc_d[, c("participant", "dsciat")]
      sc_final_data <- values$sc_final_data
      sc_final_data <- merge(sc_final_data, calc_d, 
                             by ="participant") 
      write.table(sc_final_data, file, sep = ",",
                  row.names = FALSE)
      rm(sc_final_data)
    }
  )
  

  # Download graphs SC-IAT -----
  output$down_plot_sc <- downloadHandler(
    filename = function(){
      paste(values$type_graph_sc, "SCIAT", ".pdf", sep = "")
    }, 
    content = function(file){
      graph <- values$sc_graph + theme(
        plot.margin = unit(c(1.2,1.2,1.2,1.2), "cm"))
      ggsave(file, plot = graph, width = 15, height = 7)
      
    }
  )
  
  
  # DOWNLOAD template IAT ----
  
  output$downloadTemplate <- downloadHandler(
    filename = function() {
      paste("templateDscore_IAT", ".csv", sep = "")
    },
    content = function(file) {
      
      data <- data.frame(matrix(nrow=1, ncol = 4))
      data[is.na(data)] <- ""
      colnames(data) <- c("participant", "block", "correct",
                          "latency")
      write.table(data, file, sep = ",", row.names = FALSE)
    }
  )
  
  # DOWNLOAD template SC-IAT ----
  
  output$templateSciat <- downloadHandler(
    filename = function() {
      paste("templateDscore_SCIAT", ".csv", sep = "")
    },
    content = function(file) {
      
      data <- data.frame(matrix(nrow=1, ncol = 5))
      data[is.na(data)] <- ""
      colnames(data) <- c("participant", "block", "correct",
                          "latency", "alert")
      write.table(data, file, sep = ",", row.names = FALSE)
    }
  )
  
  # SC-IAT output ----
  # summary results 
  output$fast_trials <- renderPrint({
    if(input$sc_reset == 0){
      validate(
        need(input$sc_update > 0 , "Waiting for data")
      ) 
    } else if (input$sc_reset > 0){
      validate(
        need(input$sc_update > 0  && 
               (!is.null(values$calc_d)), "Waiting for data")
      ) 
    }
    values$perc_tot_fast
  })
  # output beyond response time trials 
  output$perc_out_window <- renderPrint({
    if(input$sc_reset == 0){
      validate(
        need(input$sc_update > 0 , "Waiting for data")
      ) 
    } else if (input$sc_reset > 0){
      validate(
        need(input$sc_update > 0  && 
               (!is.null(values$calc_d)), "Waiting for data")
      ) 
    }
    values$tot_window
  })  
  # # output reliability ####
  # output$rel_output <- renderPrint({
  #   if(input$sc_reset == 0){
  #     validate(
  #       need(input$sc_update > 0 && 
  #              (!is.null(values$sciat_rel)), "Waiting for data")
  #     ) 
  #   } else if (input$sc_reset > 0){
  #     validate(
  #       need(input$sc_update > 0  && 
  #              (!is.null(values$sciat_rel)), "Waiting for data")
  #     ) 
  #   }
  #   
  #   values$sciat_rel
  # })
  
  # percentage of responses beyond the response time set by the users
  output$perc_rt_delete <- renderPrint({
    if(input$sc_reset == 0){
      validate(
        need(input$sc_update > 0 && 
               (!is.null(values$out_filter_sc)) , "Waiting for data")
      ) 
    } else if (input$sc_reset > 0){
      validate(
        need(input$sc_update > 0  && 
               (!is.null(values$out_filter_sc)), "Waiting for data")
      ) 
    }
    values$out_filter_sc
    
  })
  
  # results summary SC-IAT -----
  output$sciat_res <- renderPrint({
    if(input$sc_reset == 0){
      validate(
        need(input$sc_update > 0 , "Waiting for data")
      ) 
    } else if (input$sc_reset > 0){
      validate(
        need(input$sc_update > 0  && 
               (!is.null(values$calc_d)), "Waiting for data")
      ) 
    }
    
    summary(values$calc_d[, c("lat_MappingA", "lat_MappingB", "dsciat")])
    
  })
  
  # Reset App SC-IAT ----
  observeEvent(input$sc_reset | !is.null(values$sc_alert), 
               {
                 shinyjs::reset("sciatApp")
                 values$sc_ready <- NULL
                 values$out_filter_sc <- NULL
                 values$calc_d <- NULL
                 values$sciat_dataset <- NULL
                 values$tot_window <- NULL
                 values$perce_tot_fast <- NULL
                 values$sc_final_data <- NULL
                 values$sc_graph <- NULL
                 values$sc_alert <- NULL
                 values$mean_correct_sc <- NULL
                 labels.options_sc <- NULL
               })
}




