# Running experiments
#
# Structure:
# LoginApp -> Opens ExpApp with webkey
# ExpApp: Same app for admin and participants

examples.exp.app = function() {
	restore.point.options(display.restore.point = TRUE)
  setwd("D:/libraries/gtree/myproject")
  app = expApp()
  browseURL("http://localhost:7733?key=s1&mode=subject")
  browseURL("http://localhost:7733?key=s2&mode=subject")
  browseURL("http://localhost:7733?key=admin&mode=admin")
  viewApp(app, port=7733, launch.browser = FALSE)
	runApp
}

expAdminLoginApp = function(project.dir=get.project.dir()) {

}

expSubjectLoginApp = function(project.dir=get.project.dir()) {

}


expApp = function(project.dir=get.project.dir(), url="http://localhost:7733", theme="default") {
  restore.point("xsApp")

  library(shinyEventsUI)
  addXEconRessourcePath()

  app = eventsApp()
  glob = app$glob

  glob$project.dir = project.dir
  glob$exp.dir = file.path(project.dir,"experiments")

  glob$subjects = list()
  glob$subApps = list()

  glob$reactiveProgress = reactiveValues(count=0L)

  setwd(project.dir)
	app$ui = fluidPage(
		#theme = "xecon/css/freelancer.css",
	  tags$link(rel="stylesheet", type="text/css", href="xecon/exp_admin.css"),
		aceEditorHeader(),
		uiOutput("mainUI")
	)
  appInitHandler(function(app,xs=app$xs,session = session,...) {
		#query <- parseQueryString(session$clientData$url_search)
		initialQueryDispatch(function(query,app=getApp(),...) {
			restore.point("initDispatchExpApp")
			# TO DO: check query key
			mode = query$mode
			session = app$session

			app$mode = mode
			if (isTRUE(mode=="admin")) {
				restore.point("initDispatchExpApp.admin")
        exp.start.admin.app(app)
			} else if (isTRUE(mode=="subject")) {
				restore.point("initDispatchExpApp.subject")
				exp.subject.connects(query=query)
			}
		})
  })
  app
}

getExpAdminApp = function() {
	getApp()$glob$adminApp
}

exp.start.admin.app = function(app) {
  restore.point("exp.start.admin.app")

  app$ns = NS("ExpAdmin")
	app$glob$adminApp = app

  setUI("mainUI", exp.admin.ui())
  exp.show.num.subjects()

  # observe progress of the experiment
  app$observeProgress = observe({
    count = app$glob$reactiveProgress$count
    if (count >0) {
      em.admin.progress()
    }
  })

}

exp.subject.connects = function(query, app=getApp()) {
	restore.point("exp.subject.connects")

	app$subId = subId = query$key
	sub = app$glob$subjects[[subId]]

	# new subject arrived
	if (is.null(sub)) {
		app$glob$subApps[[subId]] = app
		sub = list(subId = subId, subNum = length(app$glob$subjects)+1, connected=TRUE)
		sub$nick = paste0("subject", sub$subNum)
		app$glob$subjects[[subId]] = sub

		subject.arrived(subId=subId)

	# subject reconnected
	} else {
		# the subjected is connected twice!
		if (isTRUE(sub$connected)) {
			ui = HTML(colored.html(paste0("Sorry, there is already another client (another browser window) connected to the experiment with your subject id. There cannot be multiple connections for a subject.")))
			setUI("mainUI",ui)
			return()
		}

		# subject is reconnected from a disrupted
		# connection
		app$glob$subApps[[subId]] = app
		subject.reconnected(subId=subId)
	}

	app$session$onSessionEnded(function() {
		subject.disconnected(subId=subId)
		cat("\nSubject", sub$nick, "has disconnected from the experiment...")
	})

	setUI("mainUI", exp.subject.ui())

}

subject.arrived = function(subId) {
	aapp=getExpAdminApp(); glob = aapp$glob
	restore.point("subject.arrived")
	cat("\nNumber of subjects: ", length(glob$subjects))
	exp.show.num.subjects()

}

subject.reconnected = function(subId) {
	app$glob$subjects[[subId]]$connected=TRUE
	exp.show.num.subjects()
}

subject.disconnected = function(subId, app=getExpAdminApp()) {
	app$glob$subjects[[subId]]$connected=FALSE
	exp.show.num.subjects()
}


exp.admin.ui = function(app=getApp()) {
	restore.point("exp.admin.ui")
	glob=app$glob
	exps = list.files(app$glob$exp.dir)
	exps = tools::file_path_sans_ext(exps)
	ns=app$ns
	ui = tagList(
		selectInput(ns("exps"),"Pick an experiment", choices = c("---",exps)),
		div(id=ns("expDiv"),style="display: none",
			tabsetPanel(id=ns("expTabset"),
				tabPanel("Run",
				  uiOutput(ns("expInfo")),
					uiOutput(ns("numSubjects")),
				  #tags$table(tags$tr(
					simpleButton(ns("assignBtn"),"Assign Subjects"),
					simpleButton(ns("startBtn"),"Start Experiment"),
				  uiOutput(ns("expProgress"))
				  #))
				),
				tabPanel("Specification",
					smallButton(ns("saveExpBtn"),"Save",form.ids=c(ns("expAce"))),
					HTML(aceEditorHtml(ns("expAce"),mode="yaml",value=""))
				)
			)
		)
	)
	selectChangeHandler(ns("exps"), function(value,...,app=getApp()) {
		restore.point("expChange")
		glob = app$glob
		if (value=="---") {
			setHtmlHide(ns("expDiv"))
			return()
		}
		setHtmlShow(ns("expDiv"))
		exp = admin.init.exp(expId = value)
		show.admin.exp.info(exp)

	})

	buttonHandler(ns("startBtn"),start.experiment)
	buttonHandler(ns("assignBtn"),assign.subjects.to.sequences.click)
  es.info.handlers()

	ui
}

admin.init.exp = function(expId, app=getApp()) {
	restore.point("admin.init.exp")

	glob = app$glob
	ns = app$ns
	exp = as.environment(list(expId = expId))
	glob$exp = exp

	# load yaml
	file = file.path(glob$exp.dir,paste0(expId,".yaml"))
	yaml = merge.lines(readLines(file))
	updateAceEditor(app$session,ns("expAce"),value = yaml)

	exp$st = read.yaml(text=yaml, keep.quotes = FALSE)

	# sequences are optional in the yaml specification
	if (is.null(exp$st[["sequences"]])) {
		fields = setdiff(names(exp$st),"expId")
		exp$st$sequences = list(main=exp$st[fields])
	}

	exp$es.li =  lapply(seq_along(exp$st$sequences), function(i) {
		name = names(exp$st$sequences)[i]
		es = exp$st$sequences[[i]]
		init.exp.sequence(name=name, es=es, exp=exp)
	})
	names(exp$es.li) = names(exp$st$sequences)
  exp$all.es.li = exp$es.li

	exp = as.environment(exp)
	exp
}

init.exp.sequence = function(name, es, exp) {
	restore.point("init.exp.sequence")

	es$name = name

	part = es$parts[[1]]
	es$parts = lapply(es$parts, function(part) {
		# currently we only have games
		# but there may be more part types
		# like questionaires
		part$name = first.non.null(part$gameId)
		if (!is.null(part$gameId)) {
		  part$type = "game"
		} else {
		  part$type == "unknown"
		}

		if (part$type=="game") {
			rg = get.rg(gameId=part$gameId)
			if (is.null(part[["variant"]])) {
				part$variant = rg$variants[[1]]
			}
			part$vg.id = part$id = vg.id = paste0(part$gameId,"_",part$variant)

			if (vg.id %in% names(exp$vg.li)) {
				part$vg = exp$vg.li[[vg.id]]
			} else {
				vg = get.vg(variant=part$variant, rg=rg)
				part$vg = exp$vg.li[[vg.id]] = vg
			}
			part$numPlayers = part$vg$params$numPlayers
		}
		if (is.null(part[["numRounds"]])) {
			part$numRounds = 1L
		}

		part = as.environment(part)
		part

	})

	# minimum number of subjects for the sequence
	if (is.null(es[["minSub"]])) {
		numPlayers = unlist(lapply(es$parts, function(part) {
			part$numPlayer
		}))
		es$minSub = first.non.null(max(numPlayers),1)
	}

	if (is.null(es[["matchingMethod"]])) {
		es$matchingMethod = first.non.null(exp$st$matchingMethod,"stranger")
	}
	es$enabled = TRUE
	es$numSub = 0
	es = as.environment(es)
	es
}

assign.subjects.to.sequences.click = function(exp = app$glob$exp, app=getApp(),...) {
	restore.point("assign.subjects.to.sequences.click")

	assign.subjects.to.sequences(exp)
	show.admin.exp.info(exp)
}

start.experiment = function(exp = app$glob$exp, app=getApp(),...) {
	restore.point("start.experiment")

	assign.subjects.to.sequences(exp)

	# we should now start the parts
	for (es in exp$es.li) {
		start.es(es, exp=exp)
	}

}

start.es = function(es, exp=app$glob$exp, app=getApp()) {
	restore.point("start.es")

	es$sub.em.li = vector("list",es$numSub)
	names(es$sub.em.li) = es$subIds

	start.es.part(partInd = 1, es=es)
}

start.es.part = function(partInd = es$partInd,es, exp=app$glob$exp, app=getApp()) {
	restore.point("start.es.part")

	glob = app$glob

	# All parts are finished
	if (partInd > length(es$parts)) {
	  return()
	}

	part = es$parts[[partInd]]


	es$partInd = partInd

	if (part$type == "game") {
	  start.es.game.part(es=es, part=part)
	} else {
		stop(paste0("Experiment parts of type ", part$type, " are not yet implemented."))
	}

}

start.es.game.part = function(es, part, glob=app$glob, app=getApp()) {
  restore.point("start.es.game.part")

	vg = part$vg
	n = vg$params$numPlayers

	# match subjects

	# currently only stranger matchings
	# note that sequences should
	# have a number of subjects that
	# can be exactly distributed over
	# matches
	nm = es$numSub / n

	# draw for each subject a match index
	match.inds = sample(rep(seq_len(nm), length.out=es$numSub),es$numSub)

	# create all em
	em.ind = 1

	es$em.li = vector("list", nm)
	for (em.ind in seq_len(nm)) {
		subInds = which(match.inds == em.ind)
		subIds = es$subIds[subInds]
		app.li = glob$subApps[subIds]

		em = new.em(vg=part$vg, subIds=subIds, app.li=app.li, container.ids = "mainUI",progress.handler = em.progress.handler)
    em$em.ind = em.ind
    em$es = es

    es$em.li[[em.ind]] = em
		for (subId in subIds) es$sub.em.li[[subId]] = em
	}
	# start all matches
	for (em in es$em.li) {
		em.start.match(em)
	}

}

em.progress.handler = function(em, exp=app$glob$exp, app=expAdminApp(),...) {
  exp$em = em
  app$glob$reactiveProgress$count = app$glob$reactiveProgress$count+1L
}

em.admin.progress = function(exp=app$glob$exp, app=expAdminApp(),...) {
  restore.point("em.admin.progress")

  em = exp$em
  es = em$es
  cat("\nem has progressed...")
  finished = sapply(es$em.li, function(em) em$is.finished)

  # start next part...
  if (all(finished)) {
    cat("\nGame ", em$vg$vg.id, " has finished all matches in sequence ", es$name)
    start.es.part(partInd = es$partInd+1, es=es)
  }
  show.exp.progress(exp)

}


assign.subjects.to.sequences = function(exp, app=getApp(), glob=app$glob) {

	connected = unlist(lapply(glob$subjects, function(sub) isTRUE(sub$connected)))

	restore.point("assign.subjects.to.sequences")

	subjects = glob$subjects[connected]
	n = length(subjects)

	remain = n
	n.es = length(exp$es.li)


	minSub = sapply(exp$es.li, function(es) es$minSub)

	batch = max(minSub)

	# first assign subjects equally as long
	# as maximum required size of all sequences
	# can be assigned to all sequences
	num.batches = floor(n / (batch*n.es))

	es.sub = rep(num.batches*batch,n.es)


	remain = n-sum(es.sub)
	# now distribute remaining slots
	while(remain >= min(minSub)) {
		for (i in seq_along(es.sub)) {
			if (remain >= minSub[i]) {
				es.sub[i] = es.sub[i] + minSub[i]
				remain = remain - minSub[i]
			}
		}
	}

	n.use = n-remain

	# draw subjects in a random order
	ids = sample(names(subjects),n.use)

	# distribute the subjects over sequences
	sub.li = vector("list",n.es)
	names(sub.li) = names(exp$es.li)
	start.ind = 1
	for (i in seq_len(n.es)) {
	  if (es.sub[[i]]>0) {
  		end.ind = start.ind+es.sub[[i]]-1
  		sub.li[[i]] = ids[start.ind:end.ind]
  		start.ind = end.ind+1
	  }
	}

	for (es.ind in seq_along(exp$es.li)) {
		es = exp$es.li[[es.ind]]
		es$subIds = sub.li[[es.ind]]
		es$numSub = length(es$subIds)
	}
	exp$exp.subId = ids
	exp$no.subId = setdiff(names(glob$subjects),ids)

}

exp.show.num.subjects = function(..., app=getExpAdminApp()) {
	restore.point("exp.show.num.subjects")

	if (is.null(app)) return()
	ns = app$ns

	glob = app$glob
	n = length(glob$subjects)
	dc = sum(unlist(lapply(glob$subjects, function(sub) !isTRUE(sub$connected))))
	restore.point("exp.show.num.subjects.2")


	ui = HTML(paste0(
	"<br>",	n, " subjects ", ifelse(dc >0,paste0("( ",dc," disconnected).")," (all connected)."),"<br>"
	))
	cat("\nexp.show.num.subjects ",ns("numSubjects"), "\n:",as.character(ui))

	#dsetUI(ns("numSubjects"),ui,app = app)
	setUI(ns("numSubjects"),ui,app=app)
}

exp.subject.ui = function(..., app=getApp()) {
	ui = tagList(
		p("Welcome to the experiment. Please wait until the experiment starts.")
	)
	ui
}

es.info.handlers = function() {
  checkboxChangeHandler(class="es-enable-checkbox", fun=function(id,value,checked, ...,exp=app$glob$exp, app=getApp()){
    restore.point("es-enable-checkbox-change")
    es = exp$all.es.li[[value]]
    es$enabled = checked

    if (!es$enabled) {
      es$numSub = 0
    }
    # set exp$es.li to enabled sequences
    all.enabled = sapply(exp$all.es.li, function(es) es$enabled)
    exp$es.li = exp$all.es.li[all.enabled]

    assign.subjects.to.sequences(exp=exp)
    show.admin.exp.info(exp)

  })
}

show.admin.exp.info = function(exp=app$glob$exp, app=getApp()) {
  ns = app$ns
  info.ui = tagList(
    tags$h4(exp$expId),
    lapply(exp$all.es.li,es.info.ui, exp=exp)
  )
  setUI(ns("expInfo"), info.ui)

}

es.info.ui = function(es, exp=app$glob$exp, app=getApp()) {
  restore.point("es.info.ui")
  ns = NS(paste0("sequence-", es$name))

  parts.ui = lapply(seq_along(es$parts), function(part.ind) {
    part = es$parts[[part.ind]]
    tags$table(class="es-part-info-table",
      tags$tr(tags$td(paste0(part.ind,".")),
        tags$td(paste0(part$id)),
        tags$td(part$type),
        tags$td("Rounds: ", part$rounds),
        tags$td("Players: ", part$numPlayers)
      )
    )
  })

  color = if (es$enabled) "white" else "#bbbbbb"

  div(id = ns("div"),style=list.to.style(list("background-color"=color)),
    tags$table(class="es-info-table",
      tags$tr(
        tags$td(simpleCheckbox(id=ns("checkbox"),class="es-enable-checkbox", value=es$name, checked=es$enabled)),
        tags$td(HTML(paste0('<h4>Sequence ',es$name,"</h4>"))),
        tags$td(paste0("Subjects: ", es$numSub, " (min. ",es$minSub,")")),
        tags$td(paste0("Matching: ", es$matchingMethod))
    )),
    parts.ui
  )
}

expAdminApp = function() getApp()$glob$adminApp

show.exp.progress=function(exp=app$glob$exp, app=expAdminApp()) {
	restore.point("show.exp.progress")
	ns=app$ns

	ui.li = lapply(exp$es.li,es.progress.ui)
	ui = div(
	  ui.li
	)
	restore.point("show.exp.progress2")

  setUI(ns("expProgress"),ui)
  dsetUI(ns("expProgress"),ui)


}

es.progress.ui = function(es, exp=app$glob$exp, app=getApp()) {
	restore.point("es.progress.ui")

  if (!isTRUE(es$partInd>0))
    return(HTML(paste0("Sequence ", es$name," has not yet started.")))

  part = es$parts[[es$partInd]]

  if (part$type == "game") {
    if (length(es$em.li)==0) {
      return(HTML(paste0("Sequence ", es$name, " part ", part$id," has currently no active matches.")))
    }

    val.li = lapply(es$em.li,function(em) {
      restore.point("shufhisfzdfv")
      vg = em$vg
      as_data_frame(c(
        list(
          subject=em$subIds,
          player=1:vg$params$numPlayers,
          stage = paste0(em$player.stage,". ",names(vg$stages)[em$player.stage]),
          status = c("active","waiting")[em$is.waiting+1]
        ),
        em$values
      ))
    })
    val.df = bind_rows(val.li)
    html = html.table(val.df)
    ui = tagList(
      h4(paste0("Sequence ", es$name, " in part ", es$partInd," (", part$id,")")),
      HTML(html)
    )
    return(ui)
  }

}
