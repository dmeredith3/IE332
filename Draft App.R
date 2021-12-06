library(shiny)
library(rvest)
library(stringi)
library(dplyr)
library(plyr)
library(stringr)
library(matrixStats)
library(keras)
library(tidyverse)
library(rsample)
library(tfdatasets)
library(RMySQL)

# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Uploading Files"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      # Input: Select a file ----
      fileInput("file1", "Choose Draft File",
                multiple = TRUE,
                accept = c("text",
                           "text/comma-separated-values,text/plain",
                           ".txt")
      ),
      
      # Horizontal line ----
      tags$hr(),
      
      dateInput('date','Select the date for this analysis',
                format = "yyyy-mm-dd",
      ),
      
      # Input: Select separator ----
      checkboxGroupInput("KPIs", "Select the stats you would like the draft to be evaluated on:",
                         choices = c("Goals" = "G",
                                     "Assists" = "A",
                                     "Power Play Goals" = "PPG",
                                     "Power Play Assists" = "PPA",
                                     "Short Handed Goals" = "SHG",
                                     "Short Handed Assists" = "SHA",
                                     "PIM" = "PIM",
                                     "Shots" = "S",
                                     "Hits" = "HIT",
                                     "Blocks" = "BLK",
                                     "Games Started (Goalies)" = "GS",
                                     "Wins (Goalies)" = "W",
                                     "Saves (Goalies)" = "S",
                                     "Save Percentage (Goalies)" = "SVP",
                                     "Shutouts (Goalies)" = "SO")
      ),
      
      textInput("gmIds", 'Insert GM IDs seperated by commas with no spaces:', 
                value = "Joe1,Joe2,Joe3,Joe4,Joe5,Joe6,Joe7,Joe8,Joe9,Joe10,Joe11,Joe12"  
      ),
      actionButton("goButton", "Get Report", class = "btn-success")),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Data file ----
      tableOutput("contents")
      
    )
    
  )
)

# Define server logic to read selected file ----
server <- function(input, output) {
  observeEvent(input$goButton, {
    output$contents <- renderTable({
      input$goButton
      
      get_draft <- function(file, league_id_input) {
        
        #Reads in txt file and formats character array
        raw_draft <- readLines(file)
        raw_draft <- raw_draft[-which(raw_draft == "")]
        num_rounds <- length(grep("Round", raw_draft, ignore.case = TRUE)) - 0
        num_teams <- grep("Round 2", raw_draft, ignore.case = TRUE) -2
        raw_draft <- raw_draft[-grep("Round", raw_draft, ignore.case = TRUE)]
        
        #Initializing Variables
        Draft_pos <- 1:length(raw_draft)
        Team <- paste("", 1:length(raw_draft))
        Player <- paste("", 1:length(raw_draft))
        First <- paste("", 1:length(raw_draft))
        Last <- paste("", 1:length(raw_draft))
        Pos <- paste("", 1:length(raw_draft))
        
        #Iterates over raw_draft pulling required data
        for (i in 1:length(raw_draft)) { 
          Draft_pos[i] <- i
          #Round[i] <- floor(i/num_rounds) + 1
          Team[i] <- trimws(substring(raw_draft[i], 5, regexpr(" - ",raw_draft[i]) - 1), which = c("both", "left", "right") )
          name <- strsplit(strsplit(raw_draft[i], split = "\\(")[[1]][2], split=" - ")[[1]][2] %>% trimws(which = c("both", "left", "right"))
          Player[i] <- name
          First[i] <- strsplit(name, split = " ")[[1]][1]
          
          if (length(strsplit(Player[i], split = " ")[[1]]) > 1) {
            Last[i] <- substring(Player[i], regexpr(" ",Player[i]) + 1)  #strsplit(Player[i], split = " ")[[1]][2]
          } else {
            Last[i] <- ""
          }
          
          Pos[i] <- strsplit(strsplit(raw_draft[i], split = "-")[[1]][length(strsplit(raw_draft[i], split = "-")[[1]])], split = ")") %>% trimws(which = c("both", "left", "right"))
          if (grepl(",",Pos[i])) {
            Pos[i] <- substring(Pos[i],1,regexpr(",",Pos[i]) - 1)
          }
        }
        
        team_id <- paste(league_id_input, "_", 1:length(Team), sep = "")
        output <- data.frame("Draft Position" = Draft_pos, "Team" = Team, "Team_ID" = team_id, "Player" = Player, "First" = First, "Last" = Last, "Position" = Pos)
        
        #Change Positions
        FDG <- c()
        pos <- c()
        for (row in 1:nrow(output)){
          if (output[row,'Position'] == 'RW' || output[row,'Position'] == 'LW' || output[row,'Position'] == 'C'){
            FDG[row] <- 'F'
            pos[row] <- output[row,'Position']
          }
          else{
            FDG[row] <- output[row,'Position']
            pos[row] <- output[row,'Position']
          }
        }
        output$pos <- FDG
        return(output)
      }
      
      
      db <- dbConnect(MySQL(), user = "g1117498", password = "332group17", dbname = "g1117498", host = "mydb.itap.purdue.edu")
      #league_inputs_raw <- dbReadTable(db, 'draft') 
      #draft <- get_draft(league_inputs_raw[1]) #Replace with get_draft(file location) where file location gets the .txt file
      #current_date <- league_inputs_raw[2] #Ensure columns match up
      #gm_ids_raw <- league_inputs_raw[3]
      #league_KPIs_raw <- league_inputs_raw[4]
      
      league_id <- length(dbReadTable(db, "league")[,1]) + 1 #KNOWN WARNING, SKIP
      draft <- get_draft(input$file1$datapath, 1)
      current_date <- as.Date(input$date, origin = "1969-12-02") #REMOVE AFTER TESTING
      gm_ids_raw <- c(input$gmIds) #REMOVE AFTER TESTING
      league_KPIs_raw <- c('G','A','GS','SV') #REMOVE AFTER TESTING
      
      KPIs <- input$KPIs
      official_skater_KPIs <- c("G", "A", "PPG", "PPA", "SHG","SHA", "PIM", "S", "HIT", "BLK")
      
      #Format Inputs
      team_names <- unique(draft$Team)
      num_teams <- length(team_names)-0
      num_rounds <- length(draft$Draft.Position)/num_teams
      gm_ids <- unlist(strsplit(gm_ids_raw,","))                         
      league_KPIs <- KPIs
      #db_gm_ids <- dbReadTable(db, gms)$gm_id #MUST INCLUDE a query for all gm ids
      
      #Valid Input Check
      valid = FALSE
      reason = ""
      if (current_date > Sys.Date()) {
        reason = "Invalid Date"
      } else if (length(gm_ids) != num_teams) {
        reason = "Incompatible draft txt file and gm list"
      } else if (!all(league_KPIs %in% KPIs)) {
        reason = "Inputted KPIs Do Not Exist"
        #} else if (!all(gm_ids %in% db_gm_ids)) { #Uncomment when the gm id table works
        #reason = "One or more GM IDs were not found"
        #} 
      } else {
        valid = TRUE
      }
      
      #Stopping
      if (!valid) {
        print("Error pls stop:") #Fix these messages and pass back to website
        print(reason)
      } else {
        print("valid input") #Fix these messages
      }
      
      #Check Analysis Types and Query Database Inputs (UNTESTED)
      if (current_date > as.Date("2021-10-12")) { 
        anal_type <- "Retrospective Analysis"
        db_stats <- dbReadTable(db, 'stats')  
        db_players <- dbReadTable(db, 'players')
        db_stats <- merge(db_stats,db_players,by = "Id", all.y = TRUE)
        db_stats[is.na(db_stats)] = 0
      } else if (current_date > as.Date("2021-7-7")){
        anal_type <- "Predictive Analysis"
        db_stats <- dbReadTable(db, 'predictions')
      } else { 
        anal_type <- "Past Season Analysis"
        db_stats <- get_stats(as.numeric(format(current_date, "%Y")))
      }
      
      print(anal_type) #REMOVE AFTER TESTING
      print(db_stats[1:5,]) #REMOVE AFTER TESTING
      
      #Prepare Score Scraping
      goalie_KPIs <- c()
      skater_KPIs <- c()
      for (i in 1:length(league_KPIs)) {
        if (all(league_KPIs[i] %in% official_skater_KPIs)) {
          skater_KPIs <- c(skater_KPIs, toString((league_KPIs[i])))
        } else {
          goalie_KPIs <- c(goalie_KPIs, as.character(league_KPIs[i]))
        }
      }
      
      #goalie_KPIs <- c('GS', 'SV') #REMOVE AFTER TESTING
      goalie_score_input <- db_stats[(db_stats$pos == "G"),c(1:7)]
      goalie_league_scores <- get_goalie_score(goalie_score_input, goalie_KPIs) 
      
      skater_score_input <- db_stats[(db_stats$pos != "G"),c(1,20,8:17)]
      skater_league_scores <- get_skater_score(skater_score_input, skater_KPIs) 
      player_league_scores <- rbind.fill(skater_league_scores, goalie_league_scores) 
      
      league_stats <- merge(db_stats, player_league_scores[,c('scores', 'Id')], by ='Id', sort = FALSE)
      league_stats <- league_stats[order(-league_stats$scores),]
      league_stats_raw <- league_stats
      
      #Changing positions to F/D/G
      FDG <- c()
      pos <- c()
      for (row in 1:nrow(league_stats)){
        if (league_stats[row,'pos'] == 'RW' ||league_stats[row,'pos'] == 'LW' ||league_stats[row,'pos'] == 'C'){
          FDG[row] <- 'F'
          pos[row] <- league_stats[row,'pos']
        }
        else{
          FDG[row] <- league_stats[row,'pos']
          pos[row] <- league_stats[row,'pos']
        }
      }
      league_stats$pos <- FDG
      
      #Make a new ids to merge on with first/last/position
      FLP <- paste("", 1:length(draft$Team))
      for (i in 1:length(draft$Team)) {
        FLP[i] <- toupper(paste(draft$First[i], draft$Last[i], draft$pos[i]))
      }
      draft <- cbind(draft,FLP)
      
      FLP <- paste("", 1:length(league_stats$Id))
      for (i in 1:length(league_stats$Id)) {
        FLP[i] <- toupper(paste(league_stats$first[i], league_stats$last[i], league_stats$pos[i]))
      }
      FLP <- stringi::stri_trans_general(FLP, "Latin-ASCII")
      league_stats <- cbind(league_stats,FLP)
      
      merge_stats_raw <- merge(draft, league_stats, by = 'FLP', all.x = TRUE, sort = FALSE)
      
      league_stats[1:5,]
      #==============================================================================
      #Draft Quality
      #==============================================================================
      #Pick Significance Calculation
      pick_sig <- 100/num_rounds 
      coeff_var <- sd(league_stats$scores[1:(length(draft))])/mean(league_stats$scores[1:(length(draft))]) #Andres Points?
      full_pick_sig <- c(1)
      for (i in 1:num_rounds) {
        pick_sig[i] = (coeff_var )* (num_rounds/2 - i) + 100/num_rounds #/ (num_teams / 2 )
        full_pick_sig <- c(full_pick_sig, rep(pick_sig[i],num_teams))
      }
      full_pick_sig <- full_pick_sig[-1]
      
      pick_rating <- (1:length(draft$Draft.Position)) - 0 #Penalizing for bad point contribution
      optimal_pick <-rep("",length(draft$Draft.Position))
      gm_report <- data.frame("Name" = team_names, "Grade" = rep(0,num_teams),"playersMissing" = rep(0,num_teams)) #Remove Players Missing after testing
      
      #merge_stats <- merge(draft, league_stats, by ='Id', all.x = TRUE, sort = FALSE) #Merge by First/Last/Team?
      merge_stats_clean <- data.frame("Id" = merge_stats_raw$Id, "Player" = merge_stats_raw$Player, "FLP" = merge_stats_raw$FLP, "Team" = merge_stats_raw$Team, "Team_id"= merge_stats_raw$Team_ID, "Draft Position" = merge_stats_raw$Draft.Position, "scores" = merge_stats_raw$scores, "pos" = merge_stats_raw$pos.y) #CHANGE TO TEAM ID ONCE FIXED
      available_player_stats <- league_stats
      
      #Calculate Draft Quality (Player performance, gm grade, gm report, and team specific report)
      #Iterate over draft, calculate pick rating, store in gm_report
      for (i in 1:length(merge_stats_clean$FLP)) {
        if(grepl(merge_stats_clean$FLP[i], filter(available_player_stats, pos == merge_stats_clean$pos[i])$FLP) %>% sum() > 0) { #If the player is found in both the draft and current season
          #optimal pick and pick_rating calculation
          bestPts <- filter(available_player_stats, pos == merge_stats_clean$pos[i])$scores[1]
          optimal_pick[i] <- filter(available_player_stats, pos == merge_stats_clean$pos[i])$FLP[1]
          pick_rating[i] <- 1 - (bestPts - merge_stats_clean$scores[i])/ bestPts
          
          #Remove from available players (current season)
          available_player_stats <- available_player_stats[-grep(merge_stats_clean$FLP[i], available_player_stats$FLP),]
          
          #Store in gm_report with adjusted significance
          team_location <- grep(merge_stats_clean$Team[i], gm_report$Name)[1]
          gm_report$Grade[team_location] <- gm_report$Grade[team_location] + (pick_rating[i] * full_pick_sig[i])
          # pick_ratings <- c(pick_ratings, gm_report$Grade[team_location] + (pick_rating[i] * full_pick_sig[i]))
        } else {
          #For Players Missing Data
          pick_rating[i] <- 0
          team_location <- grep(merge_stats_clean$Team[i], gm_report$Name)
          gm_report$playersMissing[team_location] <- gm_report$playersMissing[team_location] + 1
        }
      }
      #gm_report_text <- get_gm_report() from Jose
      #===========================SAFE TO RUN=======================================================
      # #GM REPORT
      # draft_report <- data.frame("Draft.Position" = merge_stats_clean$Draft.Position, "Team" = merge_stats_clean$Team, "Player" = merge_stats_clean$Player, "Pick Rating" = pick_rating)
      # groupteams <- group_by(draft_report, draft_report$Team)
      # 
      # draft_report_metrics <- data.frame(isGoalie = grepl('G', merge_stats_raw$pos.x), Team = merge_stats_raw$Team, Player = merge_stats_raw$Player, merge_stats_raw[,11:26])
      # 
      # df_1 <- groupteams %>% 
      #   filter(Pick.Rating %in% range(Pick.Rating)) %>% 
      #   mutate(Best = ifelse(Pick.Rating==max(Pick.Rating),1,0)) %>% 
      #   left_join(draft_report_metrics) %>% 
      #   arrange(desc(Pick.Rating))
      # 
      # x <- filter(groupteams, Pick.Rating %in% range(Pick.Rating))
      # y <- mutate(x,Best = ifelse(Pick.Rating==max(Pick.Rating),1,0))
      # #Loops and Messages for Skaters/Goalies
      # #Reference for stats
      # #Skaters stats: G(goals), A(Assists), PPG(Power play goals), PPA(power play assists), SHG(short handed goals), SHA(short handed assists), PIM(penalty infraction minutes), S(shots), HIT(hits), BLK(blocks)
      # #Goalie stats: GS(Games started), SV(saves), SVP(save percentage), SO(shutouts), W(wins)
      # 
      # df_2 <- df_1 %>% 
      #   mutate(Message = case_when(isGoalie & Best ~ paste(Player,'was the best pick for',Team,'they averaged the following stats:',GS,'games started,',SV,'saves,',SVP,'save percentage,',SO,'shutouts and',W,'wins.'),
      #                              isGoalie & !Best ~ paste(Player,'was the worst pick for',Team,'they averaged the following stats:',GS,'games started,',SV,'saves,',SVP,'save percentage,',SO,'shutouts and',W,'wins.'),
      #                              !isGoalie& Best ~ paste(Player,'was the best pick for',Team,'they averaged the following stats',G,'goals,',A,'assists,',PPG,'power play goals,',PPA,'power play assists,',SHG,'short handed goals,',SHA,'short handed assists,',PIM,'penalty infraction minutes,',S,'shots,',HIT,'hits and',BLK,'blocks.'),
      #                              !isGoalie & !Best ~ paste(Player,'was the worst pick for',Team,'they averaged the following stats',G,'goals,',A,'assists,',PPG,'power play goals,',PPA,'power play assists,',SHG,'short handed goals,',SHA,'short handed assists,',PIM,'penalty infraction minutes,',S,'shots,',HIT,'hits and',BLK,'blocks.')
      #   ))  #%>% 
      # #df_2$Message #To print messages in console
      # 
      # #Making 2 separate tables for best and worst then joining them
      # bestpickfilter <- filter(df_2, Best == TRUE)
      # bestpicktable <- data.frame(bestpickfilter$Team, bestpickfilter$Message)
      # 
      # worstpickfilter <- filter(df_2, !Best == TRUE)
      # worstpicktable <- data.frame(worstpickfilter$Team, worstpickfilter$Message)
      # message <- cbind(bestpicktable, worstpicktable)
      # 
      # df_3 <- worstpickfilter %>% left_join(bestpickfilter, by = 'Team')
      # df_4 <- df_3 %>% select(Team, Message.x,Message.y)
      #GM REPORT
      
      #Save Values Locally
      draft <- cbind(draft, 'Pick Rating' = pick_rating, 'Optimal Pick' = optimal_pick)
      league_ids <- rep(league_id, num_teams)
      team_ids <- paste(league_ids, gm_ids,sep = "") #Will need to update
      teams <- data.frame("Team ID" = team_ids, 
                          "GM ID" = gm_ids, 
                          "League ID" = league_ids, 
                          "Team Name" = gm_report$Name, 
                          "Grade" = gm_report$Grade#, 
                          #"Report" = gm_report_text
      )
      
      return(teams)
      
    })
  })
}
# Run the app ----
shinyApp(ui, server)