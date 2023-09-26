# Countdown timer

# prepare environment
quickcode::clean()
quickcode::libraryAll(shiny, nextGenShinyApps, shinyjs, shinyStorePlus, clear = TRUE)

# app.version
app.version <- 0.7


# application UI object

ui <- fluidPage(
  # Theme: Select color style from 1-13
  style = "8",

  # add custom background
  custom.bg.color = "#ffd54f",

  # add scripts
  tags$head(
    tags$script(src = "script.js"),
    tags$link(rel = "stylesheet", href = "style.css")
  ),

  # Header: Insert header content using titlePanel ------------
  header = titlePanel(left = paste0("Countdown Timer v",app.version), right = "@rpkg.net"),
  useShinyjs(), #use shiny js
  initStore(), #use shinyStorePlus
  row(
    altPanel(
      card(
        title = "Timer settings",
        numericInput("timeentry", "Enter countdown time (minutes)", 1),
        actionButton("starttimer", "▶ Start timer", size = "l", bg.type = "success"),
        actionButton("stopmusic", "♬ Stop music", size = "l", bg.type = "danger"),
        actionButton("stoptimer", "Stop timer", class = "d-none"),
        actionButton("pausetimer", "Pause timer", class = "d-none"),
        actionButton("add1min", "+ 1 minute", class = "d-none"),
        actionButton("minus1min", "- 1 minute", class = "d-none"),
        hr(),
        div("Note: At the end of the timer, a sound will be played continuously")
      )
    ),
    mainPanel(
      div(
        id = "countdownresponse",
        div(
          shiny::tags$span(id = "countdownoutput", HTML("0&emsp;00"))
        ),
        shiny::tags$h1(HTML("Minutes &emsp;&emsp; Seconds"))
      ),
      shiny::tags$span(id="countdownresponseaudio")
    )
  )
)





# application Server function

server <- function(input, output, session) {
  # create variable to store current time
  timeholder <- reactiveVal(0)
  # create timer attributes holder
  timerattr <- reactiveValues()
  typeof(timerattr)
  timerattr$countdown <- FALSE
  # trigger start of time when start button is clicked
  # this uses the time entered by user
  observeEvent(input$starttimer, {
    timeholder(as.numeric(input$timeentry) * 60)
    timerattr$countdown <- TRUE
  })



  # response to the trigger of start timer

  # countdown every second

  observe({
    setnewtime <- 0
    if (timerattr$countdown) {
      setnewtime <- timeholder() - 1

      formattime <-
        sprintf("%s&emsp;%02d", floor(setnewtime / 60), setnewtime %% 60)

      # display current timer left for the user to see
      shinyjs::runjs(paste0("$('#countdownresponseaudio').html('')"))
      shinyjs::runjs(paste0("$('#countdownoutput').html('", formattime, "')"))



      # set new time
      timeholder(setnewtime)
      Sys.sleep(1)


    # stop timer when the time reaches zero

    if (!timeholder()) {
      insertUI(
        "#countdownresponseaudio",
        ui = shiny::tags$audio(autoplay = "autoplay", loop = "true", src = "//cdn.shinyappstore.com/media/sound/drumbeat.mp3")
      )
      timerattr$countdown <- FALSE
    }
    }
    setnewtime
  })


  # stop music

  observeEvent(input$stopmusic,
    {
      shinyjs::runjs(paste0("$('#countdownresponseaudio').html('')"))
    }
  )

  #insert at the bottom  !!!IMPORTANT
  appid = "simplecountdownapp"
  setupStorage(appId = appid,inputs = TRUE)


  # subtract 1 minute to the current countdown time

  # observeEvent(input$minus1min, {
  #   setnewtime <- timeholder() - (1 * 60)
  #
  #
  #
  #   if (setnewtime < 0) {
  #     setnewtime <- 0
  #   }
  #
  #
  #
  #   # set new time
  #
  #   timeholder(setnewtime)
  # })
}



shinyApp(ui, server)
