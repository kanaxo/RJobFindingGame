

# source("usePackages.R")
# pkgnames <- c("DT","tidyverse","shiny","DBI","jsonlite","shinydashboard")
# loadPkgs(pkgnames)
library(shiny)
library(DT)
library(tidyverse)
library(DBI)
library(jsonlite)
library(shinydashboard)
library(rsconnect)

#### Career Fair Helper ####
# Constant values
TICKERS <- 8
GRIDSIZE <- 4
TOTAL <- GRIDSIZE*GRIDSIZE
####Threshold Score ####

thresholdscore <- 30

#### Apply Helper ####

"Detail : 4x4 whack a mole game 
N: number of trials in total
n: number of flashes each time
t: time interval unit 0.1second"
N <- 5
t <- 20
n <- 2

# Help functions
# 1- Random position generator 
"A postition(x,y) 4x4"
random <- function(){
  mole <- matrix(rep(c(0,0),n*n),nrow=n,ncol=n,byrow=TRUE)
  for(i in 1:n){
    x <- sample(1:4,1)
    y <- sample(1:4,1)
    mole[i,1] <- x
    mole[i,2] <- y}
  return(mole)
}

#### Skill Workshop Helper ####
source("setAWSPassword.R")

getAWSConnection <- function(){
  conn <- dbConnect(
    drv = RMySQL::MySQL(),
    dbname = "student033",
    host = "esddbinstance-1.ceo4ehzjeeg0.ap-southeast-1.rds.amazonaws.com",
    username = "student033",
    password = getOption("AWSPassword"))
  conn
}

getRandomSentence <- function(conn){
  #Given a connection, call the View 'RandomSentence' and return the resulting name
  result <- dbGetQuery(conn,"SELECT * FROM RandomSentence")
  # result should be a dataframe with a single row and a column named 'randomname'
  randomname <- result$randomname[1]
  # To test what happens when there is a duplicate entry, we can override the random result
  #randomname <- "SophisticatedImaginaryZoo" # This matches an existing player in my database
  randomname
}

# Help Functions 
# Generate random Image 
randomI <- function(){
  a <- seq(1,8)
  randomTickers <- c(sample(a),sample(a))
  Images <- matrix(rep(0,GRIDSIZE*GRIDSIZE),nrow=GRIDSIZE,ncol=GRIDSIZE,byrow=TRUE)
  count <- 1
  for(i in 1:GRIDSIZE){
    for(j in 1:GRIDSIZE){
      Images[i,j]=randomTickers[count]
      count <- count+1
    }
  }
  return(Images)
}


#### Modals ####

nameFailedModal <- function(){
  modalDialog(
    title = "Please input a valid name"
  )
}

changeAvatarModal <- function(){
  modalDialog(
    title = "Change avatar",
    #p("Proceed to ",tags$b("Welcome tab!")),
    footer = tagList(
      actionButton("welcomeok","Ok!")
    )
  )
}

avatarUpdateModal <- function(changed = 0,impressionscore){#whether avatar was selected previously before
    modalDialog(
      if (changed==1)
        p("Avatar changed."),
      if (changed==0)
      p("Avatar Selected"),
      p("New Impression Score", impressionscore),
      #p("Please click on Home Page tab to continue."),
      
      footer = tagList(
        actionButton("homeok","Ok!")
      )
    )
}

# When exiting game, show improved stats and prompt player
exitGameModal <- function(game,
                          impression=0,
                          skills=0,
                          confidence=0,
                          social=0){
  modalDialog(
    title = paste("Exiting",game),
    h3("Score improvements"),
    p("Impression: +",impression),
    p("Skills: +",skills),
    p("Confidence: +",confidence),
    p("Social: +",social),
    #h4("Click on Home Page tab to continue."),
    footer = tagList(
      actionButton("homeok","Ok!")
    )
  )
}

# entering game, prompt player
enterGameModal <- function(gamestring,energy_required=0){
  modalDialog(
    title = paste("Entering", gamestring),
    p("Energy used: ", energy_required),
    #p("Click on Mini Game tab to continue."),
    footer = tagList(
      #modalButton("OK")
      actionButton("enterok","Ok!")
    )
  )
}

enterGameModalFailed <- function(){
  modalDialog(
    title = "OOPS!",
    p("Not enough energy! Go to sleep or choose some other activity."),
    footer = tagList(
      modalButton("OK")
    )
  )
}

sleepModal <- function(){
  modalDialog(
    title = "You are rejuvenated!",
    p("Energy back to 100"),
    p("Day increased by 1")
  )
}

youWinModal <- function(days){
  modalDialog(
    title = "You got a job! :)",
    p("You got a job in ",tags$b(days)," days!"),
    p("Please return to Welcome tab"),
    footer=tagList(
      actionButton("winok","OK!")
    )
  )
}

youLoseModal <- function(){
  modalDialog(
    title = "You didn't manage to get a job! :(",
    p("Try to get your scores up first before you apply for the job."),
    footer = tagList(
      actionButton("loseok","OK")
    )
  )
}



#### dashboard MAIN UI ####

ui <- dashboardPage(skin="purple",
  dashboardHeader(title = "Job Finding Game"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      #https://fontawesome.com/icons?d=gallery
      menuItem("Welcome", tabName = "welcome", icon = icon("door-open")),
      menuItem("Home Page", tabName = "home", icon = icon("home")),
      menuItem("Mini Game", tabName = "game", icon = icon("chess-board"))
      #htmlOutput("gamestate")
    )
  ),
  
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "welcome",
              tags$head(
                # Note the wrapping of the string in HTML()
                tags$style(HTML("
                  #backgroundimage {
                    position:absolute;
                    z-order:0;
                    top:0;
                    left:100;
                    right:0;
                    bottom:0;
                    margin:auto;
                    height:100%;
                    width:100%;
                    object-fit: cover;
                   /* blur image */
                    opacity:0.5;
                    filter: blur(4px);
                    -webkit-filter: blur(4px);
                  }"))
              ),
              tags$div(img(src='WelcomeBackground.jpg',id="backgroundimage"),
              uiOutput("welcometab",style="position:relative;z-order:1"))
              
      ),
      
      # Second tab content
      tabItem(tabName = "home",
              tags$div(img(src='WelcomeBackground.jpg',id="backgroundimage")),
              tags$div(style="position:relative;z-order:1",
                tabBox( width = "100%",
                  tabPanel("Landing Page", uiOutput("landing")),
                  tabPanel("Instructions", uiOutput("landing_instructions")),
                  p(tags$b("Goal"),paste(": Get a job ASAP by increasing scores (>",thresholdscore,")!"),align='center')
                )
              )
      )
      ,
      
      # Third tab content
      tabItem(tabName = "game",
              tags$div(img(src='WelcomeBackground.jpg',id="backgroundimage")),
              tags$div(style="position:relative;z-order:1",
                h2("Mini Game Tab"), 
                uiOutput("minigame",width="100%")
              )
        )
      ), 
    )
  ) #dashboard body

#### DASHBOARD SERVER ####

server <- function(input, output, session) {
  #Set game parameters
  
  # reactiveValues objects for storing in-game items
  vals <- reactiveValues(playerid=NULL,playername=NULL,gamestate="welcome",
                         energy=90,days=0,
                         skills=0,impression=0,social=0,confidence=0,
                         sentence=NULL,status=TRUE)
  avatarc <- c(rep(0,8))
  avatarvals <- reactiveValues(avatar = avatarc)
  
  #### SIDEBAR OUTPUT ####
  output$gamestate <- renderUI({
    paste("game state:",vals$gamestate)
  })
  
  #### WELCOME PAGE OUTPUT ####
  
  output$welcometab <- renderUI(
    {if (vals$gamestate == "welcome"){
      if (is.null(vals$playername)){
        fluidPage(
          h2("Welcome!"),
          textInput("playernameinput","Please enter your name for your avatar: "),
          actionButton("submitnameok","Submit",class="btn btn-success",style='color:white'),
          br(),
          hr(),
          # actionButton("referencebutton","Show References"),
          # uiOutput("references")
          downloadButton("referenceData", " Download References")
        )
      } else{
        fluidPage(align="center",
          #tags$h4("Logged in as:"),
          #htmlOutput("loggedInAs"),
          #tags$br(),
          tags$h3("Choose your avatar!",align='center'),
          tags$p("Make sure to pick one that is presentable to interviewers."),
          htmlOutput("avatarselected"),
          img(src="avatarbackground.png",width="800px",height="415px",style="position:absolute;z-order:0"),
          imageOutput("cell1",height="200px",width="200px",click="click1",inline=TRUE), # height and width are for the containing div, not the image itself
          imageOutput("cell2",height="200px",width="200px",click="click2",inline=TRUE),  # height and width are for the containing div, not the image itself
          imageOutput("cell3",height="200px",width="200px",click="click3",inline=TRUE), # height and width are for the containing div, not the image itself
          imageOutput("cell4",height="200px",width="200px",click="click4",inline=TRUE),  # height and width are for the containing div, not the image itself
          tags$br(),
          imageOutput("cell5",height="200px",width="200px",click="click5",inline=TRUE), # height and width are for the containing div, not the image itself
          imageOutput("cell6",height="200px",width="200px",click="click6",inline=TRUE),  # height and width are for the containing div, not the image itself
          imageOutput("cell7",height="200px",width="200px",click="click7",inline=TRUE), # height and width are for the containing div, not the image itself
          imageOutput("cell8",height="200px",width="200px",click="click8",inline=TRUE),  # height and width are for the containing div, not the image itself
          tags$br()
        )
      }
    } else {
      if (vals$gamestate != "home"&vals$gamestate != "welcome")
        fluidPage(
          h2("Please go to Mini Game Tab")
        )
      else fluidPage(
        h2("Please go to ",vals$gamestate, " Tab")
      )
    }
    })
  
  # React to successful login
  output$loggedInAs <- renderUI({
    if (is.null(vals$playername))
      "Not logged in yet."
    else
      vals$playername
  })
  
  #### Welcome Page SERVER ####
  # referencevalue <- reactive(input$referencebutton%%2)
  # 
  # output$references <- renderUI({
  #   if (referencevalue == 1){
  #     
  #   }
  # })
  
  output$referenceData <- downloadHandler(
    filename = "References.pdf",
    content = function(file) {
      file.copy("www/References.pdf", file)
    }
  )
  
  # player clicks submit name, game saves playername and sets playerid
  observeEvent(input$submitnameok, {
    if (input$playernameinput!="")
      vals$playername <- input$playernameinput
    else
      showModal(nameFailedModal())
    vals$playerid <- 1
  })
  
  observeEvent(input$enterok,{
    removeModal()
    updateTabItems(session,"tabs","game")
  })
  
  observeEvent(input$welcomeok,{
    removeModal()
    updateTabItems(session,"tabs","welcome")
  })
  
  observeEvent(input$homeok,{
    removeModal()
    updateTabItems(session,"tabs","home")
  })
  
  #### AVATAR OUTPUT ####
  output$avatarselected <- renderUI({
    #p(paste("this is ", toString(avatarvals$avatar)))
    if (sum(avatarvals$avatar == 1))
      {p("Avatar selected: ", which.max(avatarvals$avatar))} 
    else p("Avatar is not selected yet.")
      
  })
  
  # output$avatarbackground <- renderImage(
  #   lisst(src='www/avatarbackground.PNG',style="position:absolute;z-order:0"),deleteFile=FALSE
  # )
  # 
  renderCell <- function(avatarnum){
    renderImage({
      #select the icon appropriate for this cell
      imageid <- avatarnum
      imgsrc=switch(imageid,"www/avatar1.png",
                    "www/avatar2.png",
                    "www/avatar3.png",
                    "www/avatar4.png",
                    "www/avatar5.png",
                    "www/avatar6.png",
                    "www/avatar7.png",
                    "www/avatar8.png")
      if (avatarvals$avatar[avatarnum]==0)
      list(src=imgsrc,style="position:relative;z-order:999;") 
      else
        list(src=imgsrc,style="border:2px solid green;position:relative;z-order:999") 
    },deleteFile=FALSE)
  }

  output$cell1 <- renderCell(1)  
  output$cell2 <- renderCell(2) 
  output$cell3 <- renderCell(3)  
  output$cell4 <- renderCell(4)  
  output$cell5 <- renderCell(5) 
  output$cell6 <- renderCell(6) 
  output$cell7 <- renderCell(7)  
  output$cell8 <- renderCell(8)  
  
  #### AVATAR EVENTS ####
  
  processClickEvent <- function(avatarnum){
    # If it is not this player's turn or if the cell is occupied, then ignore the click
    req(vals$playername)
    #print(avatarnum)
    #get initial status, whether it's new game or change avatar
    initial_status = max(avatarvals$avatar)
    # update avatar number
    avatarvals$avatar <- c(rep(0,8))
    avatarvals$avatar[avatarnum] <- 1
    #print(avatarvals$avatar)
    # update gamestate
    changeGameState("home")
    # update scores
    vals$impression <- case_when(
      avatarnum == 1 | avatarnum == 5 ~ 10,
      avatarnum == 2 | avatarnum == 6 ~ 7,
      avatarnum == 3 | avatarnum == 7 ~ 3,
      avatarnum == 4 | avatarnum == 8 ~ 0
    )
    showModal(avatarUpdateModal(changed=initial_status,vals$impression))
  }
  
  observeEvent(input$click1,{processClickEvent(1)})
  observeEvent(input$click2,{processClickEvent(2)})
  observeEvent(input$click3,{processClickEvent(3)})
  observeEvent(input$click4,{processClickEvent(4)})
  observeEvent(input$click5,{processClickEvent(5)})
  observeEvent(input$click6,{processClickEvent(6)})
  observeEvent(input$click7,{processClickEvent(7)})
  observeEvent(input$click8,{processClickEvent(8)})
  observeEvent(input$click9,{print("clicked")})
  
  #### OVERALL SERVER ####
  
  #Functions to update game reactive vals
  
  addGameScore <- function(impression=0,skills=0,confidence=0,social=0){
    # add scores
    vals$impression = vals$impression + impression
    vals$skills = vals$skills + skills
    vals$confidence = vals$confidence + confidence
    vals$social = vals$social + social
  }
  
  changeGameState <- function(newgamestate){
    vals$gamestate <- newgamestate
  }
  
  checkPlayerStats <- function(game){ #game has to be in string
    ##### ENERGY REQUIREMENTS HERE #####
    #add in r list of energy required for each game here
    energy_list <- list(
                        careerfair = 60,
                        skillsworkshop=40,
                        applyforjob=50)
    energy_required = energy_list[[game]]
    #if more than energy required,
    if (vals$energy >= energy_required){
      #change game state
      changeGameState(game)
      #minus player stats
      vals$energy = vals$energy - energy_required
      #show enter game modal
      #create list of game titles first
      gametitles = list(industryseminar = "Industry Seminar",
                        careerfair = "Career Fair",
                        skillsworkshop = "Skills Workshop",
                        applyforjob = "Interview")
      #obtain string
      gamestring = gametitles[[game]]
      showModal(enterGameModal(gamestring,energy_required))
    } else {
      #show failed
      showModal(enterGameModalFailed())
    }
  }
  
  #mini game ends
  gameHasEnded <- function(game,impression,skills,confidence,social){ #make sure game is in string format
    #add in list of score improvements here
    # show modal and add scores and change game state
    showModal(exitGameModal(game,impression,skills,confidence,social))
    addGameScore(impression,skills,confidence,social)
    changeGameState("home")
  }
  
  #sleep
  sleep <- function(){
    vals$days <- vals$days + 1
    vals$energy = 100
    showModal(sleepModal())
  }
  
  #### HOME/LANDING PAGE OUTPUTS ####
  output$landing <- renderUI({
    req(vals$playername) # if vals$playerid is NULL controls will not be available. 
    fluidPage(
      column(width=4,
             fluidRow(
               box(height="275px",width = NULL,status= "primary",
                   p("Today is day ", tags$b(vals$days),", what will ",tags$b(vals$playername)," do today?"),
                   uiOutput("avatarchoices")
                   
               )),
             fluidRow(
               uiOutput("energylanding"),
             )
      )
      ,
      column(width=4,
             fluidRow(box(width = NULL,height="405px",status="info",
                          p("Player Avatar"),
                          uiOutput("avatarlanding")
                          #p(tags$b("Goal"),": Get a job ASAP by increasing scores!",align='center')
             )
             )
      ),
      column(width = 4,
             fluidRow(
               box(width=NULL, status = "warning",
                   p("Scores"),
                   (plotOutput("scoresPlot",width="100%",height="230px")),
                   uiOutput("dynamic")
               )
             ),
             fluidRow(uiOutput("dayslanding"))
      )
      
    )
  })
  
  output$avatarchoices <- renderUI({
    if (vals$gamestate == "home")
      fluidPage(
        column(12,align='center',
               # actionButton("sleep","Sleep (+100)",style="width:100%;background-color:green;color: white;"),
               # br(),
               # #actionButton("industryseminar","Industry Seminar",style = "width:100%;"),
               # #br(),
               # actionButton("careerfair","Career Fair (-60)",style = "width:100%;"),
               # br(),
               # actionButton("skillsworkshop","Skills Workshop (-40)",style = "width:100%;"),
               # br(),
               # actionButton("changeavatar","Change Avatar",style = "width:100%;background-color:#FCF3CF;"),
               # br(),
               # actionButton("applyforjob","Apply for Job (-50)",style = "width:100%;background-color:#98FB98;"),
               # br(),
               div(class = "btn-group-vertical",style="width:100%;",
                   actionButton("sleep","Sleep (+100)", class = "btn btn-primary", style = "color:white;"),
                   actionButton("changeavatar","Change Avatar", class = "btn btn-primary", style = "color:white;"),
               ),
               br(),
               hr(),
               div(class = "btn-group-vertical",style="width:100%;",
                   actionButton("careerfair","Career Fair (-60)",style = "width:100%;color:white;",class="btn btn-info"),
                   actionButton("skillsworkshop","Skills Workshop (-40)",style = "width:100%;color:white;",class="btn btn-info"),
                   actionButton("applyforjob","Apply for Job (-50)",style = "width:100%;color:white;",class="btn btn-success")
               ),
               br(),
               p("Note: (Energy Consumed/Added)",align='center')
        )
      ) else{
        if (vals$gamestate != "welcome")
          fluidPage(
            h2("Please go to Mini Game Tab")
          )
        else fluidPage(
          h2("Please go to ",vals$gamestate, " Tab")
        )
      }
  })
  
  output$energyPlot <- renderPlot({
    ggplot() +
      geom_col(aes("", 100)) +
      geom_col(aes("", vals$energy), fill = "forestgreen") +
      coord_flip() +
      theme_minimal() +
      theme(
        #axis.title = element_blank(),
        #axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
        #panel.background = element_blank()
      )+
      labs(y=paste("Energy:",vals$energy),x=NULL)
    
  })
  
  output$energylanding <- renderUI({
    box(width = NULL,title="Energy",background="blue",
        plotOutput("energyPlot",height="50px")
    )
  })
  
  
  output$dayslanding <- renderUI({
    box(width = NULL,title = "Days passed:",
        background = "light-blue",
        p(tags$b(vals$days,style = "font-size: 30px;"), " days have passed!",style="padding:0px;margin:0px;")
    )
  })
  
  output$avatarlanding <- renderUI({
    if (sum(avatarvals$avatar) != 0)
      imageOutput("avatarlandingimage")
    else
      p("Avatar not selected yet.")
  })
  
  output$avatarlandingimage <- renderImage({
    imageid <- which.max(avatarvals$avatar)
    imgsrc=switch(imageid,"www/avatar1.png",
                  "www/avatar2.png",
                  "www/avatar3.png",
                  "www/avatar4.png",
                  "www/avatar5.png",
                  "www/avatar6.png",
                  "www/avatar7.png",
                  "www/avatar8.png")
    list(src=imgsrc,
         contentType = 'image/png',height = 200,style="display: block; margin-left: auto; margin-right: auto;")
  },deleteFile=FALSE)
  
  output$scoresPlot <- renderPlot({
    scoresdata <- data.frame(scorescat = c("Skills","Confidence","Impression","Social"),
                             scoresval = c(vals$skills, vals$confidence, vals$impression, vals$social))
    ggplot(scoresdata,aes(x=scorescat,y=scoresval))+geom_col(aes(fill=scorescat))+geom_text(aes(label=scoresval),size=5,vjust=1)+
      labs(x="",y="")+theme_minimal()+theme(legend.position="none")
  })
  
  #### landing instructions output ####
  
  output$landing_instructions <- renderUI({
    fluidPage(
      h3("Instructions"),
      h4("Goal: To apply and get a job in the least possible number of days."),
      p("You have an energy level of 100 each day, and each choice you pick will spend some energy."),
      p("Once energy is spent, you'll have to ",tags$b("sleep")," to regain your energy. The day counter will increase by one."),
      p("Each choice will require you to play a minigame, and give you some score improvements in ", tags$b("impression, skills, confidence, and social")," scores."),
      # game info image
      imageOutput("instructionsimage",height="100%"),
      p("Once you reach a certain score, you can then ", tags$b("Apply for Job"), " to try winning the game!")
    )
  })
  
  output$instructionsimage <- renderImage({
    imgsrc = 'www/instructions3.png'
    list(src=imgsrc,style="max-width:700px;width:100%;") 
  },deleteFile = FALSE)
  
  #### LANDING PAGE OBSERVE EVENTS ####
  #sleep
  observeEvent(input$sleep,{
    sleep()
  })
  
  observeEvent(input$industryseminar,{
    checkPlayerStats("industryseminar") #updates game state and minus player stats
    #put tabset mini game panel to instructions page
    updateTabsetPanel(session, "minigametabset",selected = "panel1")
    
  })
  
  observeEvent(input$careerfair,{
    checkPlayerStats("careerfair")
    updateTabsetPanel(session, "minigametabset",selected = "panel1")
  })
  
  observeEvent(input$skillsworkshop,{
    checkPlayerStats("skillsworkshop")
    updateTabsetPanel(session, "minigametabset",selected = "panel1")
  })
  
  observeEvent(input$applyforjob,{
    checkPlayerStats("applyforjob")
    updateTabsetPanel(session, "minigametabset",selected = "panel1")
  })
  
  observeEvent(input$changeavatar,{
    showModal(changeAvatarModal())
    changeGameState("welcome")
  })
  
  #### GAME PAGE OUTPUTS ####
    #### Main game UI (Do not edit here) ####
  output$minigame <- renderUI({
    #put tabs for game and instructions
    fluidPage(
      mainPanel(width="100%",
        tabsetPanel(id="minigametabset",
                    tabPanel(title = "Instructions",value = "panel1",uiOutput("minigameinstructions")),
                    tabPanel(title = "Mini Game", value = "panel2",uiOutput("minigameoutput"))
                    )
      )
    )
  })
  
  # Mini game instructions tab
  
  output$minigameinstructions <- renderUI({
    if (vals$gamestate == "industryseminar"){
      uiOutput("industryseminarinstructions")
    } else if (vals$gamestate == "careerfair"){
      uiOutput("careerfairinstructions")
    } else if (vals$gamestate == "skillsworkshop"){
      uiOutput("skillsworkshopinstructions")
    } else if (vals$gamestate == "applyforjob"){
      uiOutput("applyforjobinstructions")}
    else {
      p("Mini game not started")
      if (vals$gamestate != "home"&vals$gamestate != "welcome")
        fluidPage(
          p("Please go to Mini Game Tab")
        )
      else fluidPage(
        p("Please go to ",vals$gamestate, " Tab")
      )
    }
  })
  
  # Mini Game output Tab
  output$minigameoutput <- renderUI({
    if (vals$gamestate == "industryseminar"){
      uiOutput("industryseminaroutput")
    } else if (vals$gamestate == "careerfair"){
      uiOutput("careerfairoutput")
    } else if (vals$gamestate == "skillsworkshop"){
      uiOutput("skillsworkshopoutput")
    } else if (vals$gamestate == "applyforjob"){
      uiOutput("applyforjoboutput")}
    else {
      p("Mini game not started")
      if (vals$gamestate != "home"&vals$gamestate != "welcome")
        fluidPage(
          p("Please go to Mini Game Tab")
        )
      else fluidPage(
        p("Please go to ",vals$gamestate, " Tab")
      )
    }
  })
  

  # !!!Add Game Server functions here!!!
  #### GAME PAGE SERVER ####
  
  #### careerfair output####
  
  output$careerfairinstructions<- renderUI({
    fluidPage(align="center",
      h3("You are now in career fair."),
      h4("Click on the tiles and match the hidden images."),
      actionButton("careerfairstart", "Go to Game",width="100%"),
      imageOutput("careerfairinstructionsimage",height="100%"),
      tags$br()
    )
  })
  
  output$careerfairinstructionsimage <- renderImage({
    imgsrc = 'www/careerfairinstructions.png'
    list(src=imgsrc,style="max-width:700px;width:100%;") 
  },deleteFile = FALSE)
  
  output$careerfairoutput<- renderUI({
    fluidPage(align="center",
      fluidRow(
        box(width="100%",
            h2("Welcome to Career Fair game"),
            # start button to initialze timer
            uiOutput("careerfairstartbutton",align='center'),
            uiOutput("careerfairendbutton",align='center'),
            # timer
            h2(textOutput("Time"), align = 'center'),
            # background imag width height need to change
            img(src="Background.jpg",style="position:absolute;z-order:0;opacity: 0.2;",width="415px",height="415px"),
            
            # holes 4x4 
            imageOutput("cell111",height="100px",width="100px",click="click111",inline=TRUE), # height and width are for the containing div, not the image itself
            imageOutput("cell121",height="100px",width="100px",click="click121",inline=TRUE),  # height and width are for the containing div, not the image itself
            imageOutput("cell131",height="100px",width="100px",click="click131",inline=TRUE), # height and width are for the containing div, not the image itself
            imageOutput("cell141",height="100px",width="100px",click="click141",inline=TRUE),  # height and width are for the containing div, not the image itself
            tags$br(),
            imageOutput("cell211",height="100px",width="100px",click="click211",inline=TRUE), # height and width are for the containing div, not the image itself
            imageOutput("cell221",height="100px",width="100px",click="click221",inline=TRUE),  # height and width are for the containing div, not the image itself
            imageOutput("cell231",height="100px",width="100px",click="click231",inline=TRUE), # height and width are for the containing div, not the image itself
            imageOutput("cell241",height="100px",width="100px",click="click241",inline=TRUE),  # height and width are for the containing div, not the image itself
            tags$br(),
            imageOutput("cell311",height="100px",width="100px",click="click311",inline=TRUE), # height and width are for the containing div, not the image itself
            imageOutput("cell321",height="100px",width="100px",click="click321",inline=TRUE),  # height and width are for the containing div, not the image itself
            imageOutput("cell331",height="100px",width="100px",click="click331",inline=TRUE), # height and width are for the containing div, not the image itself
            imageOutput("cell341",height="100px",width="100px",click="click341",inline=TRUE),  # height and width are for the containing div, not the image itself
            tags$br(),
            imageOutput("cell411",height="100px",width="100px",click="click411",inline=TRUE), # height and width are for the containing div, not the image itself
            imageOutput("cell421",height="100px",width="100px",click="click421",inline=TRUE),  # height and width are for the containing div, not the image itself
            imageOutput("cell431",height="100px",width="100px",click="click431",inline=TRUE), # height and width are for the containing div, not the image itself
            imageOutput("cell441",height="100px",width="100px",click="click441",inline=TRUE), # height and width are for the containing div, not the image itself
            tags$br(),
            tags$br()
            
        )
      ),
      
    )
  })
  
  #### careerfair server ####
  
  observeEvent(input$careerfairstart,{
    updateTabsetPanel(session, "minigametabset",selected = "panel2")
  })
  
  observeEvent(input$careerfairend,{
    # Use end game function here
    gameHasEnded("career fair",0,0,max(floor(120/time_r()),2),max(2,floor(120/time_r()))) #Add scores here
    gamevals$Status <- matrix(rep(0,GRIDSIZE*GRIDSIZE),nrow=GRIDSIZE,ncol=GRIDSIZE,byrow=TRUE)
    gamevals$Count <- 0
    time_r(0)
    finished(FALSE)
    gamevals$Wrong <- FALSE
    gamevals$Identiy <- NULL
    gamevals$Images <- randomI()
    gamevals$Previous <- c(-1,-1)
    gamevals$PPrevious<- c(-1,-1)
    gamevals$Frozen <- matrix(rep(FALSE,GRIDSIZE*GRIDSIZE),nrow=GRIDSIZE,ncol=GRIDSIZE,byrow=TRUE)
    
  }) 
  
  # Game reactive values:
  # Picture status : 0: blank 1:show the corresponding images 
  Status <- matrix(rep(0,GRIDSIZE*GRIDSIZE),nrow=GRIDSIZE,ncol=GRIDSIZE,byrow=TRUE)
  # Picture location : Fixed -> Random
  Images <- randomI()
  # Uique identifier
  Identiy <- NULL
  # Track the last click 
  Previous <- c(-1,-1)
  PPrevious<- c(-1,-1)
  # Wrong 
  Wrong <- FALSE
  # End track
  Count <- 0
  # Frozen
  Frozen <- matrix(rep(FALSE,GRIDSIZE*GRIDSIZE),nrow=GRIDSIZE,ncol=GRIDSIZE,byrow=TRUE)
  once <- matrix(rep(FALSE,GRIDSIZE*GRIDSIZE),nrow=GRIDSIZE,ncol=GRIDSIZE,byrow=TRUE)
  # ReactiveValues
  " Status reactive matrix : 1: Flash 0:normal"
  Status2 <- matrix(rep(0,GRIDSIZE*GRIDSIZE),nrow=GRIDSIZE,ncol=GRIDSIZE,byrow=TRUE)
  mole <- random()
  score <- 0
  # All Reaction values
  gamevals <- reactiveValues(Status=Status,Identiy=Identiy,Previous=Previous,PPrevious=PPrevious,Wrong=Wrong,Count=Count,Frozen=Frozen,
                             Images=Images)
  gamevals2 <- reactiveValues(Status2=Status2,mole=mole,score=score, once = once)
  # Timer function:
  # timer render by click start button
  # careerfairscore <- reactiveVal(value=0)
  time_r <- reactiveVal(value = 0)
  started <- reactiveVal(value = FALSE)
  finished <- reactiveVal(value=FALSE)
  observeEvent(input$Start, {
    time_r(0)
    finished(FALSE)
    started(TRUE)
    gamevals$Status=Status
  }, ignoreInit = TRUE)
  observe({
    if (started()) {
      
      invalidateLater(100, session)
      isolate({
        newTime <- time_r() + 0.1
        time_r(newTime)
      })
    }
  })
  output$Time <- renderText({
    if(gamevals$Count!=8){
      paste("Time:",as.character(round(time_r(),digits = 2)))}
    else{
      started(FALSE)
      finished(TRUE)
      paste("Game End:",as.character(round(time_r(),digits = 2)))
    }
  })
  
  # Show the initil layout
  # display the hole and moles
  renderCell <- function(gridrow,gridcol){
    renderImage({
      # original back side of pictures & image
      Blank="www/blank.png"
      Flipped= paste0('www/',gamevals$Images[gridrow,gridcol],'.png')
      # Check click events 
      imgsrc=switch(gamevals$Status[gridrow,gridcol]+1,Blank,Flipped)
      # display the pictures 
      list(src=imgsrc,style="position:relative;z-order:999") 
    },deleteFile=FALSE)
  }
  # show function
  
  output$cell111 <- renderCell(1,1)  
  output$cell121 <- renderCell(1,2) 
  output$cell131 <- renderCell(1,3)  
  output$cell141 <- renderCell(1,4)  
  output$cell211 <- renderCell(2,1) 
  output$cell221 <- renderCell(2,2) 
  output$cell231 <- renderCell(2,3)  
  output$cell241 <- renderCell(2,4)  
  output$cell311 <- renderCell(3,1) 
  output$cell321 <- renderCell(3,2) 
  output$cell331 <- renderCell(3,3)  
  output$cell341 <- renderCell(3,4)
  output$cell411 <- renderCell(4,1) 
  output$cell421 <- renderCell(4,2) 
  output$cell431 <- renderCell(4,3)  
  output$cell441 <- renderCell(4,4)
  
  
  # click 
  processClickEvent1 <- function(gridrow,gridcol,Frozen){
    if (started()){
      # if it alr frozen 
      if(Frozen[gridrow,gridcol]){
        # show the flipped one 
        gamevals$Status[gridrow,gridcol] <- 1
      }
      else{
        # Show image 
        # Check if click the first image 
        if(gamevals$Previous[1]==-1 && gamevals$Previous[1]==-1){
          gamevals$Status[gridrow,gridcol] <- (gamevals$Status[gridrow,gridcol]+1)%%2
          gamevals$Identiy <-  gamevals$Images[gridrow,gridcol]
          gamevals$Wrong <- FALSE
          gamevals$Previous <- c(gridrow,gridcol)
        }
        # Check if click the same icon twice:
        if(!(gamevals$Previous[1] == gridrow && gamevals$Previous[2]==gridcol)){
          gamevals$Status[gridrow,gridcol] <- (gamevals$Status[gridrow,gridcol]+1)%%2
          # Get and Check the value 
          if(is.null(gamevals$Identiy)){
            gamevals$Identiy <-  gamevals$Images[gridrow,gridcol]
            if(gamevals$Wrong){
              gamevals$Status[gamevals$Previous[1],gamevals$Previous[2]] <- (gamevals$Status[gamevals$Previous[1],gamevals$Previous[2]]+1)%%2
              gamevals$Status[gamevals$PPrevious[1],gamevals$PPrevious[2]] <- (gamevals$Status[gamevals$PPrevious[1],gamevals$PPrevious[2]]+1)%%2
              gamevals$Wrong <- FALSE
            }
          }
          else{
            if( gamevals$Images[gridrow,gridcol]==gamevals$Identiy){
              # Frozen the images
              gamevals$Identiy <-NULL 
              Frozen[gamevals$Previous[1],gamevals$Previous[2]] <- TRUE
              Frozen[gridrow,gridcol] <- TRUE
              gamevals$Count <-gamevals$Count +1
              gamevals$Wrong <- FALSE
            }
            else{
              # wrong click   
              gamevals$Wrong <- TRUE
              gamevals$Identiy <- NULL
              #gamevals$Status[gridrow,gridcol] <- (gamevals$Status[gridrow,gridcol]+1)%%2
            }
            
          }
          gamevals$PPrevious <- gamevals$Previous
          gamevals$Previous <- c(gridrow,gridcol)
        }}}
  }
  # Click event 
  
  
  observeEvent(input$click111,{processClickEvent1(1,1,gamevals$Frozen)})#, (once =  (gamevals$Frozen[1,1])))
  observeEvent(input$click121,{processClickEvent1(1,2,gamevals$Frozen)})#, (once =  (gamevals$Frozen[1,2])))
  observeEvent(input$click131,{processClickEvent1(1,3,gamevals$Frozen)})#, (once =  (gamevals$Frozen[1,3])))
  observeEvent(input$click141,{processClickEvent1(1,4,gamevals$Frozen)})#, (once =  (gamevals$Frozen[1,4])))
  observeEvent(input$click211,{processClickEvent1(2,1,gamevals$Frozen)})#, (once =  (gamevals$Frozen[2,1])))
  observeEvent(input$click221,{processClickEvent1(2,2,gamevals$Frozen)})#, (once =  (gamevals$Frozen[2,2])))
  observeEvent(input$click231,{processClickEvent1(2,3,gamevals$Frozen)})#, (once =  (gamevals$Frozen[2,3])))
  observeEvent(input$click241,{processClickEvent1(2,4,gamevals$Frozen)})#, (once =  (gamevals$Frozen[2,4])))
  observeEvent(input$click311,{processClickEvent1(3,1,gamevals$Frozen)})#, (once =  (gamevals$Frozen[3,1])))
  observeEvent(input$click321,{processClickEvent1(3,2,gamevals$Frozen)})#, (once =  (gamevals$Frozen[3,2])))
  observeEvent(input$click331,{processClickEvent1(3,3,gamevals$Frozen)})#, (once =  (gamevals$Frozen[3,3])))
  observeEvent(input$click341,{processClickEvent1(3,4,gamevals$Frozen)})#, (once =  (gamevals$Frozen[3,4])))
  observeEvent(input$click411,{processClickEvent1(4,1,gamevals$Frozen)})#, (once =  (gamevals$Frozen[4,1])))
  observeEvent(input$click421,{processClickEvent1(4,2,gamevals$Frozen)})#, (once =  (gamevals$Frozen[4,2])))
  observeEvent(input$click431,{processClickEvent1(4,3,gamevals$Frozen)})#, (once =  (gamevals$Frozen[4,3])))
  observeEvent(input$click441,{processClickEvent1(4,4,gamevals$Frozen)})#, (once =  (gamevals$Frozen[4,4])))
  
  
  output$careerfairendbutton <- renderUI({
    req(finished())
    actionButton("careerfairend","End Game",class = "btn btn-info btn-lg btn-block")
  })
  
  output$careerfairstartbutton <- renderUI({
    if(finished()==FALSE){
      if(started()==FALSE)
      actionButton("Start","Start!",class = "btn btn-success btn-lg btn-block")
      else tags$br()
    }
  })
  
  #### skillsworkshop output ####
  
  output$skillsworkshopinstructions<- renderUI({
    fluidPage(align="center",
      h3("You are now in skills workshop."),
      h4("Click start to commence game and type out the words as fast you can! The less time you take, the higher the score!
         Take note that the input is caps-sensitive. Press enter after you're done!"),
      actionButton("skillsworkshopstart", "Go to Game",width="100%"),
      imageOutput("skillsworkshopinstructionsimage",height="100%"),
      tags$br()
    )
  })
  
  output$skillsworkshopinstructionsimage <- renderImage({
    imgsrc = 'www/skillsworkshopinstructions.png'
    list(src=imgsrc,style="max-width:700px;width:100%;") 
  },deleteFile = FALSE)
  
  output$skillsworkshopoutput<- renderUI({
    fluidPage(align="center",
      fluidRow(
        box(width="100%",
          uiOutput("skillsworkshopstartbutton",align='center'),
          uiOutput("skillsworkshopsendbutton",align='center'),
          # timer 
          h2(textOutput("Time3"),align='center'),
          # Display teh sentence
          h3(textOutput("Sentence"),style="width:100%;align=center;"),
          #textfield for player input
          textInput("text", h3("Text input"), 
                    value = ""),
          p(tags$b("Caps-sensitive.")," Press enter when done.")
          ),
      #actionButton("skillsworkshopend","End Game")
      tags$br(),
      
      tags$br()
    ))
  })
    
    output$skillsworkshopsendbutton <- renderUI({
      req(finished())
      actionButton("skillsworkshopsend","End Game",class = "btn btn-info btn-lg btn-block")
    })
    
    output$skillsworkshopstartbutton <- renderUI({
      if(finished()==FALSE)
        actionButton("Start3","Start",class = "btn btn-success btn-lg btn-block")
    })
  
    #### skillsworkshop server ####
    
  observeEvent(input$skillsworkshopstart,{
    updateTabsetPanel(session, "minigametabset",selected = "panel2")
  })
  
  observeEvent(input$skillsworkshopsend,{
    # Use end game function here
    gameHasEnded("skills workshop",0,floor(200/time_r()),0,0) #Add scores here
    time_r(0)
    finished(FALSE)
    vals$status <- TRUE
    vals$sentence <- NULL
  })  
  
  #Insert below#
  output$Sentence <- renderText({
    # Game reactive values:
    vals$sentence 
  })
  # timer render by click start button
  observeEvent(input$Start3, {
    time_r(0)
    finished(FALSE)
    started(TRUE)
    conn <- getAWSConnection()
    vals$sentence <- getRandomSentence(conn)
    
  }, ignoreInit = TRUE)
  observe({
    if (started()) {
      invalidateLater(100, session)
      isolate({
        newTime <- time_r() + 0.1
        time_r(newTime)
      })
    }
  })
  
  output$Time3 <- renderText({
    if(vals$status){
      paste("Time:",as.character(round(time_r(),digits = 2)))}
    else{
      started(FALSE)
      finished(TRUE)
      paste("Game End:",as.character(round(time_r(),digits = 2)))
    }
  })
  
  observe({
    if (started()) {
      req(input$text)
      if(!is.null(vals$sentence)){
        if(input$text==vals$sentence){
          # Stop the game
          vals$status <- FALSE
        }}}
  })
  

  
  
  #### apply for job output ####
  
  output$applyforjobinstructions<- renderUI({
    fluidPage(align="center",
      #fluidRow(
        h3("You are now in interview"),
        h4("Click start to commence the game and click on the icons of 2 people conversing with each other to earn points before the time runs out.
           The higher the points you have, the higher the chances of winning and getting a job."),
        actionButton("applyforjobstart", "Go to Game", width = "100%"),
        tags$br(),
        imageOutput("applyforjobinstructionsimage",height="100%")
      
    )
  })
  
  output$applyforjobinstructionsimage <- renderImage({
    imgsrc = 'www/applyforjobinstructions.png'
    list(src=imgsrc,style="max-width:700px;width:100%;") 
  },deleteFile = FALSE)
  
  
  output$applyforjoboutput<- renderUI({
    fluidPage(align="center",
      fluidRow(
        box(width='100px',status = "info",
          h2("Welcome to Apply For Job game", align = 'center'),
          # start button to initialze timer
          uiOutput("applyforjobstartbutton",align = 'center'),
          uiOutput("applyforjobendbutton", align = 'center'),
          # timer
          h2(textOutput("Time2"), align = 'center'),
          h2(textOutput("Score"), align = 'center'),
          # background imag width height need to change
          img(src="Background.jpg",style="position:absolute;z-order:0;opacity: 0.2;",width="415px",height="415px"),
          
          # holes 4x4 
          imageOutput("acell11",height="100px",width="100px",click="click11",inline=TRUE), # height and width are for the containing div, not the image itself
          imageOutput("acell12",height="100px",width="100px",click="click12",inline=TRUE),  # height and width are for the containing div, not the image itself
          imageOutput("acell13",height="100px",width="100px",click="click13",inline=TRUE), # height and width are for the containing div, not the image itself
          imageOutput("acell14",height="100px",width="100px",click="click14",inline=TRUE),  # height and width are for the containing div, not the image itself
          tags$br(),
          imageOutput("acell21",height="100px",width="100px",click="click21",inline=TRUE), # height and width are for the containing div, not the image itself
          imageOutput("acell22",height="100px",width="100px",click="click22",inline=TRUE),  # height and width are for the containing div, not the image itself
          imageOutput("acell23",height="100px",width="100px",click="click23",inline=TRUE), # height and width are for the containing div, not the image itself
          imageOutput("acell24",height="100px",width="100px",click="click24",inline=TRUE),  # height and width are for the containing div, not the image itself
          tags$br(),
          imageOutput("acell31",height="100px",width="100px",click="click31",inline=TRUE), # height and width are for the containing div, not the image itself
          imageOutput("acell32",height="100px",width="100px",click="click32",inline=TRUE),  # height and width are for the containing div, not the image itself
          imageOutput("acell33",height="100px",width="100px",click="click33",inline=TRUE), # height and width are for the containing div, not the image itself
          imageOutput("acell34",height="100px",width="100px",click="click34",inline=TRUE),  # height and width are for the containing div, not the image itself
          tags$br(),
          imageOutput("acell41",height="100px",width="100px",click="click41",inline=TRUE), # height and width are for the containing div, not the image itself
          imageOutput("acell42",height="100px",width="100px",click="click42",inline=TRUE),  # height and width are for the containing div, not the image itself
          imageOutput("acell43",height="100px",width="100px",click="click43",inline=TRUE), # height and width are for the containing div, not the image itself
          imageOutput("acell44",height="100px",width="100px",click="click44",inline=TRUE), # height and width are for the containing div, not the image itself
          #sliderInput("finalscore","Final Score:",1,10,1,value = 8),
          tags$br(),
          tags$br()
        ))
    )
  })
  
  output$applyforjobendbutton <- renderUI({
    req(finished())
    actionButton("applyforjobend","End Game",class = "btn btn-info btn-lg btn-block")
  })
  
  output$applyforjobstartbutton <- renderUI({
    if(finished()==FALSE){
      if (started()==FALSE)
      actionButton("Start2","Start!",class = "btn btn-success btn-lg btn-block")
      else tags$br()
    }
  })
  
  #### apply for job server ####
  observeEvent(input$applyforjobstart,{
    updateTabsetPanel(session, "minigametabset",selected = "panel2")
  })
  #Insert below #
  

  #gamevals <- reactiveValues()
  # timer render by click start button
  observeEvent(input$Start2, {
    time_r(0)
    finished(FALSE)
    started(TRUE)
    gamevals2$score=0
  }, ignoreInit = TRUE)
  
  output$Time2 <- renderText({
    if(N*t*0.1-time_r()<0){
      started(FALSE)
      finished(TRUE)
      "Time Runout"
    }
    else{if(as.integer(as.double(time_r())*10) %/% t<=N){
      paste("Time:",as.character(round(N*t*0.1-time_r(),digits=2)))}}
  })
  
  
  # random position update
  observe({if((as.integer(as.double(time_r())*10)%%t==0) && (as.integer((as.double(time_r()))*10) %/% t<N) && (started()) ){ 
    gamevals2$mole <- random()
    gamevals2$Status2 <- Status2
    gamevals2$once <- once
    show2()
  }})
  
  # display the hole and moles
  renderCell2 <- function(gridrow,gridcol){
    renderImage({
      imgsrc=("www/cross.png")
      for(i in 1:n){
        if (gridrow==gamevals2$mole[i,1] && gridcol==gamevals2$mole[i,2]){
          imgsrc=("www/mole.png")
          gamevals2$Status2[gamevals2$mole[i,1],gamevals2$mole[i,2]] <- 1
          break
        }}
      # display the holes 
      list(src=imgsrc,style="position:relative;z-order:999") 
    },deleteFile=FALSE)
  }
  
  # show function
  show2 <- function(){
    output$acell11 <- renderCell2(1,1)  
    output$acell12 <- renderCell2(1,2) 
    output$acell13 <- renderCell2(1,3)  
    output$acell14 <- renderCell2(1,4)  
    output$acell21 <- renderCell2(2,1) 
    output$acell22 <- renderCell2(2,2) 
    output$acell23 <- renderCell2(2,3)  
    output$acell24 <- renderCell2(2,4)  
    output$acell31 <- renderCell2(3,1) 
    output$acell32 <- renderCell2(3,2) 
    output$acell33 <- renderCell2(3,3)  
    output$acell34 <- renderCell2(3,4)
    output$acell41 <- renderCell2(4,1) 
    output$acell42 <- renderCell2(4,2) 
    output$acell43 <- renderCell2(4,3)  
    output$acell44 <- renderCell2(4,4)
  }
  show2()
  
  # click 
  processClickEvent2 <- function(gridrow,gridcol,Oncematrix){
    if ((gamevals2$Status2[gridrow,gridcol]==1)&&(Oncematrix[gridrow,gridcol]==FALSE)&&(started())){
      gamevals2$score <- gamevals2$score+1
      #print(gamevals2$Status2)
      #print(paste("clicked",gridrow,gridcol))
      gamevals2$Status2[gridrow,gridcol]<-0
      #print(gamevals2$Status2)
      Oncematrix[gridrow,gridcol]<- TRUE
      #print(Oncematrix)
    } else {
      #print(paste("click detected",gridrow,gridcol))
      #print(gamevals2$Status2)
      #print(Oncematrix)
      }
  }
  
  # update score
  output$Score <- renderText(paste("Score",as.character(sum(gamevals2$score))))
  

  observeEvent(input$click11,{processClickEvent2(1,1,gamevals2$once)})#,once = TRUE)
  observeEvent(input$click12,{processClickEvent2(1,2,gamevals2$once)})#)},once = TRUE)
  observeEvent(input$click13,{processClickEvent2(1,3,gamevals2$once)})#)},once = TRUE)
  observeEvent(input$click14,{processClickEvent2(1,4,gamevals2$once)})#)},once = TRUE)
  observeEvent(input$click21,{processClickEvent2(2,1,gamevals2$once)})#)},once = TRUE)
  observeEvent(input$click22,{processClickEvent2(2,2,gamevals2$once)})#)},once = TRUE)
  observeEvent(input$click23,{processClickEvent2(2,3,gamevals2$once)})#)},once = TRUE)
  observeEvent(input$click24,{processClickEvent2(2,4,gamevals2$once)})#)},once = TRUE)
  observeEvent(input$click31,{processClickEvent2(3,1,gamevals2$once)})#)},once = TRUE)
  observeEvent(input$click32,{processClickEvent2(3,2,gamevals2$once)})#)},once = TRUE)
  observeEvent(input$click33,{processClickEvent2(3,3,gamevals2$once)})#)},once = TRUE)
  observeEvent(input$click34,{processClickEvent2(3,4,gamevals2$once)})#)},once = TRUE)
  observeEvent(input$click41,{processClickEvent2(4,1,gamevals2$once)})#)},once = TRUE)
  observeEvent(input$click42,{processClickEvent2(4,2,gamevals2$once)})#)},once = TRUE)
  observeEvent(input$click43,{processClickEvent2(4,3,gamevals2$once)})#)},once = TRUE)
  observeEvent(input$click44,{processClickEvent2(4,4,gamevals2$once)})#)},once = TRUE)
  
  # End-of-Job-Interview Code #
  observeEvent(input$applyforjobend,{
    #check if final interview score above threshold
    time_r(0)
    finished(FALSE)
    gamevals2$Status2 <- Status2
    gamevals2$once <- once
    
    winorlose <- checkFinalScore(gamevals2$score)
    if (winorlose == "win"){
      showModal(youWinModal(vals$days))
      #remove player name once game has been won
      vals$playername <- NULL
      vals$playerid <- NULL
      vals$days <- 0
      vals$energy <- 90
      avatarvals$avatar <- c(rep(0,8))
      vals$gamestate <- "welcome"
      vals$skills=0
      vals$impression=0
      vals$social=0
      vals$confidence=0
      gamevals2$score <- 0
    }else{
      showModal(makeModal("You Lose!:("))
    }
  })
  
  makeModal <- function(text){
    modalDialog(
                p(text),
                p("Get your scores up and Try again next time."),
                footer=actionButton("loseok","OK")
                )
  }
  
  observeEvent(input$winok,{
    removeModal()
    updateTabItems(session,"tabs","welcome")
  })
  
  observeEvent(input$loseok,{
    removeModal()
    gameHasEnded("Interview",0,0,gamevals2$score,0)
    gamevals2$score<-0
  })
  
  checkFinalScore <- function(score){
    # set threshold score here! #
    sumscore <- vals$social + vals$confidence + vals$impression + vals$skills
    #max(45 - sumscore,2)
    if (sumscore >= thresholdscore && score>2){
      return("win")
    } else {
      return("lose")
    }
  }
  # End-of-Job-Interview Code End #


}
  
  
  
  
  
shinyApp(ui, server)

