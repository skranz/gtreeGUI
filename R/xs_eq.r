xs.show.eq.tab = function(gameId, xs=app$xs, app=getApp()) {
  restore.point("xs.show.eq.tab")
  cat("\nxs.show.eq.tab")
  tabId = paste0("tab_eq_",gameId)
  if (tabId %in% xs$tabs) {
    w2tabs.select("xsTabs", tabId)
    return()
  }
  xs$tabs = c(xs$tabs, tabId)

  divId = paste0("div_eq_",gameId)
  tab=list(id=tabId,caption=paste0("Eq. ", gameId), closable=TRUE,div_id = divId)
  w2tabs.add(id="xsTabs", tabs=list(tab))
  ui = xs.eq.ui(gameId)
  appendToHTML(selector="#mainDiv", as.character(hidden_div(id=divId, ui)))
  w2tabs.select("xsTabs", tabId)
  xs.show.help("eq")
}


init.xeq = function(gameId, branching.limit = 10000, sp.limit=1e6) {
	xeq = as.environment(nlist(gameId, branching.limit,sp.limit))
	xeq$rg = 	get.rg(gameId=gameId)
	xeq$variants = xeq$rg$variants
	xeq$tg.li = list()
	xeq$eq.li = list()
	xeq$eqo.li = list()
	xeq
}


xs.eq.ui = function(gameId, xs = app$xs, app=getApp()) {
  restore.point("xs.eq.ui")

	ns = NS(paste0("eq-",gameId))

	xeq = init.xeq(gameId)
  if (is.null(xs$xeq.li)) xs$xeq.li = list()
	xs$xeq.li[[gameId]] = xeq

	xeq$ns = ns
	xs$xeq = xeq

	rg = xeq$rg

	xeq$solve.modes = list(
		"All pure SPE (Gambit)"="spe",
		"All pure SPE (internal)"="spe_xs",
#		"Just Gametree"="gametree",
		"All pure NE (Gambit)"="ne",
		"All NE (including mixed)"="ne_am",
		"Some SPE (logit)"="spe_sm_logit",
		"Some SPE (lcp)"="spe_sm_lcp",
		"Some NE (ipa)"="ne_sm_ipa",
#		"Some NE (liap)"="ne_sm_liap",
		"Some NE (gnm)"="ne_sm_gnm",
#		"Some NE (simpdiv)"="ne_sm_simpdiv",
		"Some NE (lcp)"="ne_sm_lcp"
#		"QRE (Quantal Response Eq.)"="qre"
	)


	xeq$prefs = load.preferences()

	form.sel = ids2sel(c(ns("variants"),ns("prefs"),ns("reduce"), ns("branchingLimit"), ns("spLimit"),ns("solvemode"),ns("background")))

	ui = tagList(
		h5(paste0("Equilibrium analysis of ", gameId)),
		HTML("<table><tr><td valign='top' style='padding-right: 1em'>"),
		selectizeInput(ns("variants"),label="Variants",choices = xeq$variants,selected = xeq$variants, multiple=TRUE),
		selectizeInput(ns("prefs"),label="Preferences",choices = names(xeq$prefs),selected = "payoff", multiple=TRUE),
		tags$script(HTML(paste0('$("#', ns("variants"),'").selectize();'))),
		tags$script(HTML(paste0('$("#', ns("prefs"),'").selectize();'))),
	  if (isTRUE(xs$devel.features))
	    checkboxInput(ns("background"),label="Background job",value = TRUE),
		HTML("</td><td valign='top'>"),
	  selectInput(ns("solvemode"),label="Solver",choices = xeq$solve.modes),
		numericInput(ns("branchingLimit"),label="Branching limit",value = xeq$branching.limit),
		numericInput(ns("spLimit"),label="Strategy Profiles Limit",value = xeq$sp.limit),
	  if (isTRUE(xs$devel.features))
		  selectInput(ns("reduce"),label="Reduce game by Eliminating some dominated moves",choices = list("No reduction"="noreduce", "Reduce"="reduce","Both"="both")),
		HTML("</td></tr></table>"),
		smallButton(ns("gametreeBtn"),"Gametree", "data-form-selector"=form.sel),
		smallButton(ns("solveBtn"),"Solve", "data-form-selector"=form.sel),
		smallButton(ns("firstbestBtn"),"First Best", "data-form-selector"=form.sel),
    uiOutput(ns("tgmsg")),
		br(),
		uiOutput(ns("tginfo")),
		uiOutput(ns("eqJobRunningInfo")),
		uiOutput(ns("eqsUI")),
		uiOutput(ns("condEqoUI"))

	)


	buttonHandler(ns("gametreeBtn"),function(formValues,...) {
		restore.point("xeqSolveClick")
		ok = xeq.solve(xeq=xeq, formValues=formValues, clear=TRUE, never.load = xs$never.load.tg, solvemode="gametree")

		if (ok) {
			xeq.show.tg.info(xeq)
			xeq.show.eqo(xeq)
			xeq.show.conditional.eqo(xeq)
		}
	})
	buttonHandler(ns("solveBtn"),function(formValues,...) {
		restore.point("xeqSolveClick")
		ok = xeq.solve(xeq=xeq, formValues=formValues, clear=TRUE, never.load = xs$never.load.tg)

		if (ok) {
			xeq.show.tg.info(xeq)
			xeq.show.eqo(xeq)
			xeq.show.conditional.eqo(xeq)
		}
	})

	buttonHandler(ns("firstbestBtn"),function(formValues,...) {
		restore.point("xeqFirstBestClick")
		ok = xeq.first.best(xeq=xeq, formValues=formValues, clear=TRUE)
		if (ok) {
			xeq.show.tg.info(xeq)
			xeq.show.eqo(xeq)
			xeq.show.conditional.eqo(xeq)
		}
	})


	xeq.show.running.info(xeq)


	ui
}



xeq.solve = function(xeq, formValues,clear=TRUE,  never.load=TRUE, solvemode=NULL, background=NA, xs=app$xs, app=getApp()) {
	restore.point("xeq.solve")
	ns = xeq$ns

	if (clear) {
		xeq$rg = 	get.rg(gameId=xeq$gameId)
	  xeq$tg.li = xeq$eq.li = xeq$eqo.li = list()
	}
  xeq$running.jobs = list()

	variants = unlist(formValues[[ns("variants")]])
	pref_names = unlist(formValues[[ns("prefs")]])
	reduce.method = unlist(formValues[[ns("reduce")]])
	if (is.null(reduce.method))
	  reduce.method = "noreduce"


	branching.limit = unlist(formValues[[ns("branchingLimit")]])
	sp.limit = unlist(formValues[[ns("spLimit")]])
	if (is.null(solvemode))
		solvemode = unlist(formValues[[ns("solvemode")]])
	xeq$solvemode = solvemode

	if (is.na(background))
	    #background = isTRUE(formValues[[ns("background")]] == "on")
	    background = !isTRUE(formValues[[ns("background")]] == "off")

	# Internal solver is always run in foreground
	if (solvemode=="spe_xs") background=FALSE


	if (reduce.method=="reduce") {
		reduce.vec = TRUE
	} else if (reduce.method=="noreduce") {
		reduce.vec = FALSE
	} else {
		reduce.vec = c(FALSE,TRUE)
	}

	xeq$sel.variants = variants
	xeq$sel.prefs = xeq$prefs[pref_names]
	xeq$eqo.li = xeq$eq.li = list()

	timedMessage(ns("tgmsg"),msg=paste0("Solve equilibria for variants ",paste0(variants,collapse=", ")))

	msg.fun = function(...) {
		msg = paste0(...)
		timedMessage(ns("tgmsg"),msg=msg,millis = Inf)
	}
	variant = xeq$sel.variants[[1]]

	for (variant in xeq$sel.variants) {
		msg = paste0("Create or load game tree for variant ",variant,"... ")
		timedMessage(ns("tgmsg"),msg=msg)
		org.tg = get.tg(gameId=xeq$gameId, variant=variant, rg=xeq$rg, msg.fun=msg.fun, never.load=never.load, branching.limit = branching.limit)

		if (org.tg$kel$count>0) {
    	timedMessage(ns("tgmsg"),paste0("There are problems:<br>",paste0(org.tg$kel$log, collapse="<br>\n")),millis = Inf)
    	return(FALSE)
  	}


		solver = xeq.solvemode.to.solver(solvemode, n=org.tg$params$numPlayers)
		just.make.tg= (solvemode == "gametree")

		for (pref in xeq$sel.prefs) {
			tg = as.environment(as.list(org.tg))
			set.tg.pref(pref,tg)
			for (reduce in reduce.vec) {
				if (reduce) {
					msg = paste0("Solve equilibria for reduced variant ",variant," for pref ", pref$name,"... ")
					timedMessage(ns("tgmsg"),msg=msg)
					tg = reduce.tg(tg)
				} else {
					msg = paste0("Solve equilibria for variant ",variant," for pref ", pref$name,"... ")
					timedMessage(ns("tgmsg"),msg=msg)
				}
				tg.id = tg$tg.id
				xeq$tg.li[[tg.id]] = tg

				if (!just.make.tg) {
				  eq.id = get.eq.id(tg=tg, solvemode = solvemode, mixed=mixed, just.spe=just.spe)
					eq.li = get.eq(tg = tg,solver=solver, solvemode = solvemode, only.load = background)
					# need to create new equilibrium in background
          if (is.null(eq.li) & background) {
            tg.to.efg(tg=tg)

            xeq$running.jobs[[tg.id]] = eq.id
            # job is already running
            if (xs.is.job.running(eq.id)) next

            job = start.gambit.job(tg=tg,solver=solver, solvemode = solvemode, eq.id=eq.id)
            xs$job.li[[job$id]] = job
          } else {
  					xeq$eq.li[[tg.id]] = eq.li
  					eqo = eq.outcomes(eq.li, tg=tg)

  					short.id = str.right.of(tg.id, paste0(tg$gameId,"_"))
  					eqo$.id = rep(short.id,NROW(eqo))
  					eqo = select(eqo, .id, everything())
  					xeq$eqo.li[[tg.id]] = eqo
          }

				} else {
					tg.to.efg(tg=tg)
				}
			}
		}
	}


  num.eq = length(xeq$tg.li)
  num.running = length(xeq$running.jobs)
  if (solvemode=="gametree") {
    msg = "Gametree(s) are computed."
  } else {
    if (!background) {
      msg = paste0(num.eq, " equilibria have been computed...")
    } else {
      msg = ""
      if (num.eq-num.running>0)
        msg = paste0(msg,num.eq-num.running, " equilibria have been loaded. ")

      #if (num.running > 0)
      #  msg = paste0(msg,num.running, " equilibria are currently computed...")
      #
      if (num.running>0)
        xs.show.jobs.tab(select=FALSE)
    }

  }
	timedMessage(ns("tgmsg"),msg=msg)


	return(TRUE)

}


xeq.first.best = function(xeq, formValues,clear=TRUE,  never.load=TRUE, xs=app$xs, app=getApp()) {
	restore.point("xeq.first.best")
	ns = xeq$ns

	if (clear) {
		xeq$rg = 	get.rg(gameId=xeq$gameId)
	  xeq$tg.li = xeq$eq.li = xeq$eqo.li = list()
	}
  xeq$running.jobs = list()

	variants = unlist(formValues[[ns("variants")]])
	pref_names = unlist(formValues[[ns("prefs")]])

	branching.limit = unlist(formValues[[ns("branchingLimit")]])
	xeq$sel.variants = variants
	xeq$sel.prefs = xeq$prefs[pref_names]
	xeq$eqo.li = xeq$eq.li = list()

	timedMessage(ns("tgmsg"),msg=paste0("Solve first best for variants ",paste0(variants,collapse=", ")))

	msg.fun = function(...) {
		msg = paste0(...)
		timedMessage(ns("tgmsg"),msg=msg,millis = Inf)
	}
	variant = xeq$sel.variants[[1]]

	for (variant in xeq$sel.variants) {
		msg = paste0("Create or load decision tree for variant ",variant,"... ")
		timedMessage(ns("tgmsg"),msg=msg)
		org.tg = get.first.best.tg(gameId=xeq$gameId, variant=variant, rg=xeq$rg, msg.fun=msg.fun, never.load=never.load, branching.limit = branching.limit)

		if (org.tg$kel$count>0) {
    	timedMessage(ns("tgmsg"),paste0("There are problems:<br>",paste0(org.tg$kel$log, collapse="<br>\n")),millis = Inf)
    	return(FALSE)
  	}
		for (pref in xeq$sel.prefs) {
			tg = as.environment(as.list(org.tg))
			set.tg.pref(pref,tg)
      set.tg.welfare(tg)

			msg = paste0("Solve first best for variant ",variant," for pref ", pref$name,"... ")
			timedMessage(ns("tgmsg"),msg=msg)
			tg.id = tg$tg.id
			xeq$tg.li[[tg.id]] = tg

			eq.id = tg.id
			eq.li = compute.first.best(tg = tg,find.all.eq = TRUE)
  		xeq$eq.li[[tg.id]] = eq.li
  		eqo = eq.outcomes(eq.li, tg=tg)

  		short.id = str.right.of(tg.id, paste0(tg$gameId,"_"))
  		eqo$.id = rep(short.id,NROW(eqo))
  		eqo = select(eqo, .id, everything())
  		xeq$eqo.li[[tg.id]] = eqo
		}
	}

  num.eq = length(xeq$tg.li)
  num.running = length(xeq$running.jobs)
  msg = paste0(num.eq, " first best strategies have been computed...")
	timedMessage(ns("tgmsg"),msg=msg)
	return(TRUE)
}


xeq.load.eq = function(xeq, tg.id, eq.id, eq.dir=get.eq.dir(xeq$gameId), file = file.path(eq.dir, paste0(eq.id,".eq"))) {
  restore.point("xeq.load.eq")

  file = file.path(eq.dir, paste0(eq.id,".eq"))
	eq.li = readRDS(file)$eq.li

  tg = xeq$tg.li[[tg.id]]
  xeq$eq.li[[tg.id]] = eq.li
  eqo = eq.outcomes(eq.li, tg=tg)

  short.id = str.right.of(tg.id, paste0(tg$gameId,"_"))
  eqo$.id = rep(short.id,NROW(eqo))
  eqo = select(eqo, .id, everything())
  xeq$eqo.li[[tg.id]] = eqo
}


xeq.show.running.info = function(xeq) {
	ns = xeq$ns
	restore.point("xeq.show.running.info")
	num.jobs = length(xeq$running.jobs)
	if (num.jobs==0) {
	  ui = ""
	} else {
	  ui = p(style="padding-top: 1em",paste0(num.jobs," jobs running to solve equilibria..."))
	}
	setUI(ns("eqJobRunningInfo"),ui)
	dsetUI(ns("eqJobRunningInfo"),ui)

}


xeq.show.tg.info = function(xeq) {
	ns = xeq$ns
	restore.point("xeq.show.tg.info")

	info.df = xeq.tg.info.df(xeq=xeq)
	html = html.table(info.df)

	num.jobs = length(xeq$running.jobs)
  ui = tagList(
    HTML(html)
  )

	setUI(ns("tginfo"),ui)
	dsetUI(ns("tginfo"),ui)

	xeq.show.running.info(xeq)
}


xeq.show.eqo = function(xeq) {
	ns = xeq$ns
	restore.point("xeq.show.eqo")

	if (isTRUE(xeq$solvemode=="gametree")) {
		ui = p("")
		setUI(ns("eqsUI"),ui)
		dsetUI(ns("eqsUI"),ui)
		return()
	}

	eqo.df = bind_rows(xeq$eqo.li)
	if (NROW(eqo.df)==0) {
		ui = p("No equilibria computed.")
		setUI(ns("eqsUI"),ui)
		dsetUI(ns("eqsUI"),ui)
		return()
	}

	eeqo.df = expected.eq.outcomes(eqo.df,group.vars = c(".id","eqo.ind"))
	eeo.html = html.table(select(eeqo.df,-.outcome,-numPlayers,-variant))
	#eo.html = html.table(select(eqo.df,-.outcome,-eq.ind,-numPlayers,-variant))
	ui = tagList(
		h5("Expected equilibrium outcomes:"), HTML(eeo.html)
		#,h5("Equilibrium outcomes:"), HTML(html)
	)
	setUI(ns("eqsUI"),ui)
	dsetUI(ns("eqsUI"),ui)
}

xeq.tg.info.df = function(xeq,ids = names(xeq$tg.li),...) {
	restore.point("xeq.tg.info.df")

	tg = xeq$tg.li[[1]]


	no.oco = lapply(ids, function(id) {
		tg = xeq$tg.li[[id]]
		if (is.null(tg)) return("-")
		format(NROW(tg$oco.df), big.mark=" ")
	})
	no.ise = lapply(ids, function(id) {
		tg = xeq$tg.li[[id]]
		if (is.null(tg)) return("?")
		format(NROW(tg$ise.df), big.mark=" ")
	})
	avg.moves = lapply(ids, function(id) {
		tg = xeq$tg.li[[id]]
		if (is.null(tg)) return("?")
		format(round(mean(tg$ise.df$.num.moves),1))
	})

	no.sg = lapply(ids, function(id) {
		tg = xeq$tg.li[[id]]
		if (is.null(tg$sg.df)) return("?")
		format(NROW(tg$sg.df), big.mark=" ")
	})
	no.all.sp = lapply(ids, function(id) {
		tg = xeq$tg.li[[id]]
		if (is.null(tg$sg.df)) return("?")
		format(tg$sg.df$.num.strats[1],big.mark = " ", scientific = 9)
	})
	no.sp = lapply(ids, function(id) {
		tg = xeq$tg.li[[id]]
		if (is.null(tg$sg.df)) return("?")
		format(sum(tg$sg.df$.num.strats.without.desc), big.mark=" ",scientific = 9)
	})

	no.eq = lapply(ids, function(id) {
		eq.li = xeq$eq.li[[id]]
		if (is.null(eq.li)) return("?")
		format(length(eq.li), big.mark=" ")
	})

	no.eqo = lapply(ids, function(id) {
		eqo.df = xeq$eqo.li[[id]]
		if (is.null(eqo.df)) return("?")
		format(NROW(eqo.df), big.mark=" ")
	})

	solve.time = lapply(ids, function(id) {
		eq.li = xeq$eq.li[[id]]
		solve.time = attr(eq.li,"solve.time")
		if (is.null(solve.time)) return("?")
		format(solve.time,digits=3)
	})

	mat = matrix(nrow=9, byrow = TRUE,c(
		"Outcomes",no.oco,
		"Info sets (avg. moves)", paste0(no.ise," (",avg.moves,")"),
		"Subgames", no.sg,
		"Strat-profiles...",rep("",length(ids)),
		"...normal-form",no.all.sp,
		"...backward-induction", no.sp,
		"Pure SPE", no.eq,
		"Pure SPE outcomes", no.eqo,
		"Solve time", solve.time
	))
	colnames(mat) = c(".",ids)
	as.data.frame(mat)
}

xeq.solvemode.to.solver = function(solvemode, n=Inf) {
	if (solvemode == "spe") {
		solver = "gambit-enumpure -q -P"
	} else if (solvemode == "ne") {
		solver = "gambit-enumpure -q"
	} else if (solvemode == "ne_am") {
		if (n == 2) {
			solver = "gambit-enummixed -q -d 4"
		} else {
			solver = "gambit-enumpoly -q"
		}
	} else if (solvemode == "qre") {
		solver = "gambit-logit -q"
	} else if (solvemode == "spe_sm_lcp") {
		solver = "gambit-lcp -q -d 4 -P"
	} else if (solvemode == "spe_sm_logit") {
		solver = "gambit-logit -q -e"
	} else if (solvemode == "ne_sm_simpdiv") {
		solver = "gambit-simpdiv -q"
	} else if (solvemode == "ne_sm_liap") {
		solver = "gambit-liap -q -d 4"
	} else if (solvemode == "ne_sm_lcp") {
		solver = "gambit-lcp -q -d 4"
	} else if (solvemode == "ne_sm_ipa") {
		solver = "gambit-ipa -q -d 4"
	} else if (solvemode == "ne_sm_gnm") {
		solver = "gambit-gnm -q -d 4"
	} else {
		solver = ""
	}
	return(solver)
}

example.show.conditional.eqo = function() {
  app = eventsApp()
  app$ui = fluidPage(
 		uiOutput(xeq$ns("condEqoUI"))
  )
  appInitHandler(function(...) {
      xeq.show.conditional.eqo(xeq)
  })
  viewApp(app)
}

# Allow user to filter variables
# When an action or move of nature is set to a particular value,
# we assume that this variable is played,
# possible out-of-equilibrium
# What if we filter a transformed variable????
xeq.show.conditional.eqo = function(xeq, app=getApp()) {
  restore.point("xeq.show.conditional.eqo")
  ns = NS(paste0(xeq$gameId,"-eqo-filter"))

  if (isTRUE(xeq$solvemode=="gametree") | length(xeq$eqo.li) == 0) {
		ui = p("")
		setUI(ns("condEqoUI"),ui)
		dsetUI(ns("condEqoUI"),ui)
		return()
	}


  variants = xeq$sel.variants
  prefs = names(xeq$sel.prefs)
  gameId = xeq$rg$gameId

  tg.ind = 1
  tg = xeq$tg.li[[tg.ind]]
  vars = setdiff(tg$vars,names(tg$params))
  var.vals = lapply(vars, function(var){
    unique(tg$oco.df[[var]])
  })
  names(var.vals) = vars

  # need to store select values in app
  # since getInputValue does not
  # correctly work with this dynamic UI
  if (is.null(app$cond.eqo.val.list))
    app$cond.eqo.val.list = list()

  sel.li = replicate(length(vars), list())
  names(sel.li) = vars
  app$cond.eqo.val.list[[gameId]] = sel.li


  lens = sapply(vars, function(var) {
    vals = var.vals[[var]]
    len = max(3,nchar(var), nchar(as.character(vals)), na.rm = TRUE)
  })

  input.li = lapply(vars, function(var) {
    vals = c("",var.vals[[var]])
    len = max(6,nchar(var), nchar(as.character(vals)), na.rm=TRUE)
    tagList(
      selectInput(ns(var),label=var,choices = vals ,selected = NULL, multiple = TRUE,width = paste0(len,"em"),selectize = FALSE)
      #,tags$script(HTML(paste0('$("#',ns(var),'").selectize({dropdownParent: "body"});')))
    )
  })

  ui = do.call(splitLayout,c(input.li,list(
      cellWidths=paste0(lens+1,"em")
    #, cellArgs=list(style="padding-left: 5px;")
  )))

  ui = div(
    hr(),
    h5("Conditional equilibrium outcomes given an action is (unexpectedly) fixed"),
    ui,
    uiOutput(xeq$ns("condEqoOutputUI"))

  )
  setUI(xeq$ns("condEqoUI"),ui)
  dsetUI(xeq$ns("condEqoUI"),ui)
  dsetUI(xeq$ns("condEqoOutputUI"), HTML(""))

  handler.fun = function(id, value,...) {
    args = list(...)

    # need to save values in a list of app
    # since getInputValue does not work
    # nicely with dynamic UI
    var = str.right.of(id,"-eqo-filter-")
    app$cond.eqo.val.list[[gameId]][[var]] = value
    values = app$cond.eqo.val.list[[gameId]]

    restore.point("xeq.show.conditional.eqo.handler")
    cat("\nchanged")
    empty = sapply(values, function(val) length(val)==0 | isTRUE(any(nchar(val)==0)))
    values = values[!empty]
    if (length(values)==0) {
      dsetUI(xeq$ns("condEqoOutputUI"), HTML(""))
      return()
    }

    # Take grid of all combinations
    cond = expand.grid(values)

    tg.ind = 1
    c.li = lapply(seq_along(xeq$tg.li), function(tg.ind){
      tg = xeq$tg.li[[tg.ind]]
      id = str.right.of(tg$tg.id,"_")
      eqo = cond.eq.outcomes(eq.li = xeq$eq.li[[tg.ind]],cond=cond, tg=tg,expected = TRUE)
  		eqo$.id = rep(id,NROW(eqo))
  		eqo = select(eqo, .id, everything())
  		eqo
    })
    ceqo = bind_rows(c.li) %>%
      select(-eq.ind,-.outcome,-numPlayers) %>%
	    select(everything(), ceqo.ind)

    html = html.table(ceqo)
    setUI(xeq$ns("condEqoOutputUI"), HTML(html))
    dsetUI(xeq$ns("condEqoOutputUI"), HTML(html))
  }
  for (var in vars) selectChangeHandler(id=ns(var), handler.fun)
}
