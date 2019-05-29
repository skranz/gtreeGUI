examples.chatbox = function() {
  app = eventsApp()
  app$experiment.match = em
  ai = list(name = "mychat", player = 1:2, timer=200, identifiers = "", identifierMode = "fixed", type = "chatbox")


  app$ui = fluidPage(
    uiOutput("myui")
  )
  appInitHandler(function(...) {
    setUI("myui", chatbox(id="mychatbox",players=1:2,ai=ai, em=em))
  })
  viewApp(app)

  chatbox()
}

# will be called when a default page for a stage is created
gtree.addin.chatbox.page.txt = function(ai, rg, ...) {
  txt = paste0('{{chatbox(id="', ai$name,'", label="Chat",input.label="Enter a message",btn.label="Send")}}',"\n",
  "<!-- You may customize your chatbox further by providing corresponding arguments of the chatbox or chatbox.ui functions. -->")
  txt
}

# will be called when the submit button of a stage page is pressed
gtree.addin.chatbox.submit.page = function(ai, stage, stage.ind, ..., em=get.em()) {
  # we don't need to do or check anything here
  return(ok=TRUE, msg="")
}

# will be called, when a match is finished (i.e. the last stage is shown to all players)
gtree.addin.chatbox.finish.match = function(ai, rg, ..., em=get.em()) {
  # let us destroy all observers
  # to save memory and performance
  for (cb in em$chatbox.li) {
    ob = cb$observers[[player.ind]]
    if (!is.null(ob))
      ob$destroy()
  }
  cb = em$chatbox.li[[id]]

}



# we need to get a function to get the current addin by its id
chatbox = function(id=ai$name,players=ai$players, identifiers=ai$identifiers,colors=c("#000000","#000088","#880000","#008800","#440044"), player=em$player, timer=ai$timer, timeout.label="Time to chat is over", timer.label = "Remaining time: {{seconds_to_period(sec)}}", ..., em=get.em(), app=getApp(), ai = get.em.ai(id, em=em) ) {
  restore.point("chatbox")

  if (is.empty(players)) {
    players = em$players
  }
  colors = rep(colors,length.out=length(players))
  if (is.empty(identifiers)) {
    identifiers = players
  }

  if (!is.empty(timer)) {
    timer = as.integer(timer)
  } else {
    timer = NULL
  }


  cat("\nInit chatbox for player ", player)
  player.ind = match(player, players)

  if (is.null(em[["chatbox.li"]])) {
    em$chatbox.li = list()
  }

  cb = em$chatbox.li[[id]]
  if (is.null(cb)) {
    cb= list(
      ai = ai,
      players = ai$players,
      colors = colors,
      identifiers = identifiers,
      text = "",
      html = "",
      react = reactiveValues(counter=0),
      observers = vector("list", length(players)),
      timer = timer,
      timer.label = timer.label,
      timeout.label = timeout.label,
      timer.observers = vector("list", length(players)),
      start.time = as.integer(Sys.time())
    )
  }

  em$chatbox.li[[id]] = cb

  # destroy existing observer to avoid double triggering
  ob = cb$observers[[player.ind]]
  if (!is.null(ob)) ob$destroy()
  ob = cb$timer.observers[[player.ind]]
  if (!is.null(ob)) ob$destroy()

  # add new observer
  em$chatbox.li[[id]]$observers[[player.ind]] = observe({
    cb$react$counter
    cat("\nupdate chatbox for player ", player)
    update.chatbox.discussion(id=ai$name, player=player)
  })

  # add timer observer
  if (!is.null(cb$timer)) {
    em$chatbox.li[[id]]$timer.observers[[player.ind]] = observe({
      cat("\nupdate chatbox timer for player ", player)
      update.chatbox.timer(id=ai$name, player=player)
    })

  }


  ui = chatbox.ui(ai=ai, player=player, em=em, vg=em$vg,...)
  ns = NS(paste0(ai$name,"-", player, "-chat"))
  buttonHandler(ns("chatSendBtn"),function(...) chatbox.send.btn.click(...,player=player, ai=ai))
  ui
}

# will be called whenever some player posted a new entry
update.chatbox.discussion = function(id, player, em=get.em(), app=getApp()) {
  restore.point("update.chatbox.view")
  ns = NS(paste0(id,"-", player, "-chat"))
  cb = em$chatbox.li[[id]]
  setInnerHTML(id=ns("chatView"), html=cb$html)

}

update.chatbox.timer = function(id, player, em=get.em(), app=getApp()) {
  restore.point("update.chatbox.timer")
  ns = NS(paste0(id,"-", player, "-chat"))
  cb = em$chatbox.li[[id]]
  sec.left = cb$timer+1 - (as.integer(Sys.time())- cb$start.time)

  # timer is out
  if (sec.left<0) {
    shinyEvents::setHtmlHide(id = ns("chatInputDiv"))
    setInnerHTML(id=ns("timer"), html=cb$timeout.label)

    return()
  }

  # round time
  if (sec.left > 300) {
    sec = floor(sec.left / 60)*60
  } else if (sec.left > 60) {
    sec = floor(sec.left / 10)*10
  } else {
    sec = sec.left
  }

  html = paste.whiskers(cb$timer.label, list(sec=sec))
  setInnerHTML(id=ns("timer"), html=html)
  # update every minute
  if (sec.left > 300) {
    invalidateLater(1000*60)
  } else if (sec.left > 60) {
    invalidateLater(1000*10)
  } else {
    invalidateLater(1000)
  }

}

chatbox.send.btn.click = function(formValues, ..., ai, player, em=get.em(), app=getApp()) {
  restore.point("chatbox.send.btn.click")

  cat("\nchatbox sendBtn by player ", player)

  id = ai$name
  ns = NS(paste0(id,"-", player, "-chat"))
  add = str.trim(formValues[[ns("chatInput")]])
  cb = em$chatbox.li[[id]]

  player.ind = match(player,players)
  ident = cb$identifiers[player.ind]
  color = cb$colors[player.ind]

  cb$text = paste0(cb$text,ident,": ", add,"\n")
  add.html = gsub("\n","<br>   ",add, fixed=TRUE)
  cb$html = paste0(cb$html,"<font color='",color,"'>",ident,": ", add.html,"</font>","<br>\n")
  em$chatbox.li[[id]] = cb

  # trigger updates of all chatboxes
  cb$react$counter = cb$react$counter+1

  # update current chatbox directly
  #setInnerHTML(id=ns("chatView"), html=cb$html)

  # clear player's input box
  evalJS(paste0('$("#', ns("chatInput"),'").val("");'))
  #updateTextAreaInput(session = app$session,ns("chatInput"),value = "")

}

chatbox.ui = function(ai, player=em$player, em=get.em(), vg=em$vg, label="Chat",input.label="Enter a message",btn.label="Send", timer.label="Remaining Seconds", timer=ai$timer, chat.view.rows=10, chat.view.font="Consolas",
  chat.view.style = list.to.style(list("overflow-y"="auto", border="solid #aaaaaa 1px", "font-family"=chat.view.font, "height"=paste0(chat.view.rows,"em")))) {
  restore.point("chatbox.ui")
  ns = NS(paste0(ai$name,"-", player, "-chat"))
  ui = tagList(
    tags$table(tags$tr(tags$td(label),tags$td(style="padding-left: 5em;",span(id=ns("timer"))))),
    div(id = ns("chatView"),style=chat.view.style),
    div(id=ns("chatInputDiv"),
      shiny::textAreaInput(ns("chatInput"),label=input.label,value=""),
      smallButton(ns("chatSendBtn"),btn.label,form.ids = c(ns("chatInput"),ns("chatView")))
    )
  )

}
