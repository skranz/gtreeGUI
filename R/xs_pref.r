examples.xs.pref = function() {
	setwd("D:/libraries/gtree/myproject")
  prefs = load.preferences()

	tg = get.tg(gameId ="BunchedUltimatum")
	pref = prefs[[3]]

	set.tg.pref(pref,tg)
	tg$tg.id
	oco.df = tg$oco.df



}

xs.show.prefs.tab = function(xs=app$xs, app=getApp()) {
  restore.point("xs.show.prefs.tab")
  tabId = paste0("tab_prefs")
  if (tabId %in% xs$tabs) {
    w2tabs.select("xsTabs", tabId)
    return()
  }
  xs$tabs = c(xs$tabs, tabId)

  divId = paste0("div_prefs")
  tab=list(id=tabId,caption="Prefs", closable=TRUE,div_id = divId)
  w2tabs.add(id="xsTabs", tabs=list(tab))

  if (is.null(xs[["pref.classes"]]))
    xs$pref.classes = load.pref.classes()

  ui = xs.prefs.ui()
  appendToHTML(selector="#mainDiv", as.character(hidden_div(id=divId, ui)))
  w2tabs.select("xsTabs", tabId)

  xs.show.help("prefs")
}

xs.prefs.ui = function(xs = app$xs, app=getApp(),...) {
	restore.point("xs.prefs.ui ")

  project.dir = xs$project.dir

  file = file.path(project.dir,"preferences.yaml")
  if (!file.exists(file)) {
     file = system.file("spec","preferences.yaml",package = "gtree")
  }

  txt = merge.lines(readLines(file))
	ns = NS("prefs-edit")
  ui = list(
    h5("Preferences Classes:"),
    pref.class.info.panels(xs$pref.classes),

    h5("Specified Preferences:"),
  	HTML("<table><tr><td>"),
    smallButton(ns("saveBtn"), "Save Preferences",  form.ids = ns("ace")),
  	HTML("</td><td>"),
    smallButton(ns("defaultBtn"), "Default",form.ids = ns("ace")),
  	HTML("</td></tr></table>"),
    uiOutput(ns("msg")),
  	HTML(aceEditorHtml(ns("ace"),value = txt, mode="yaml",wordWrap = TRUE))
  )
  dsetUI(ns("msg"),"")
	buttonHandler(ns("saveBtn"),fun = save.prefs.click)
	buttonHandler(ns("defaultBtn"),fun = default.prefs.click)
  ui
}


default.prefs.click = function(...,xs=app$xs, app=getApp()) {
	if (isTRUE(xs$demo.mode)) {
		demo.mode.alert(); return();
	}
	ns = NS("prefs-edit")
  file = system.file("spec","preferences.yaml",package = "gtree")
  txt = readLines(file)
	updateAceEditor(session=getApp()$session,ns("ace"),value=merge.lines(txt))
}


save.prefs.click = function(formValues,...,xs=app$xs, app=getApp()) {
	if (isTRUE(xs$demo.mode)) {
		demo.mode.alert(); return();
	}
	ns = NS("prefs-edit")
	txt = merge.lines(formValues[[ns("ace")]])

  yaml = try(read.yaml(text=txt, check.by.row=TRUE))

  if (is(yaml, "try-error")) {
    msg = merge.lines(as.character(yaml))
    #msg = gsub("\n","<br>",msg,fixed = TRUE)
    msg = paste0("Did not save your preferences, since there were errors when parsing:<br><br><pre>",msg,"</pre>")
    timedMessage(ns("msg"),html=colored.html(msg),millis=Inf)
    return()
  }

  file = file.path(xs$project.dir,"preferences.yaml")
  writeLines(txt, file)
  timedMessage(ns("msg"),msg=paste0("Saved to ", file), millis=5000)

}




load.preferences = function(project.dir = get.project.dir(), pref.classes = load.pref.classes(project.dir=project.dir), file=NULL) {
  restore.point("load.preferences")

  if (is.null(file)) {
    file = file.path(project.dir,"preferences.yaml")
    if (!file.exists(file)) {
 	    file = system.file("spec","preferences.yaml",package = "gtree")
    }
  }
  if (is.null(file))
    return(pref.classes.default.prefs(pref.classes))

  prefs = try(read.yaml(file),silent = TRUE)
  if (is(prefs,"try-error")) {
    prefs = read.yaml(file, check.by.row = TRUE)
  }

  pref.names = names(prefs)

  prefs = lapply(seq_along(prefs), function(i) {
    yp = prefs[[i]]
    pref.name = names(prefs)[i]
    class = yp$class
    if (is.null(class)) class = ""
    if (!class %in% names(pref.classes)) {
      stop(paste0("The preference ", pref.name, " has specified an unknown class '", class,"'. Only the following preference classes are available: ", paste0(names(pref.classes), collapse=", ")))
    }

    pc = pref.classes[[class]]

    # take default parameters from preference class
    params = lapply(pc$params, function(par) par$default)
    # overwrite specified parameters
    params[names(yp$params)] = yp$params

		pref = list(
			class = class,
			params = params,
			descr = pc$descr,
			name = pref.name,
			label = pref.name,
			fun = pc$fun
		)
		pref
	})

  names(prefs) = pref.names

	prefs

}

load.pref.classes = function(project.dir = get.project.dir(), file=NULL) {
  if (is.null(file)) {
    file = file.path(project.dir,"pref_classes.yaml")
    if (!file.exists(file)) {
	    file = system.file("spec","pref_classes.yaml",package = "gtree")
    }
  }

  read.yaml(file)
}

pref.classes.default.prefs = function(precl=load.pref.classes()) {
	prefs = lapply(seq_along(precl), function(i) {
		pc = precl[[i]]
		pc = list(
			class = names(precl)[i],
			params = lapply(pc$params,function(par) {
				par$default
			}),
			descr = pc$descr,
			name = pc$defaultName,
			label = pc$defaultName,
			fun = pc$fun
		)
		#class(pc) = c("pref","list")
		pc
	})

	names(prefs) = sapply(precl, function(pc) {
		pc$defaultName
	})
	prefs

}

set.tg.pref = function(pref,tg) {
	restore.point("set.tg.pref")
	n = tg$params$numPlayers
	args = c(pref$params,list(n=n,player=1))

	util.funs = sapply(seq_len(n), function(i) {
		args$player = i
		do.call(pref$fun,args)
	})

	if (!is.null(pref$label)) {
		names(util.funs) = rep(pref$label,n)
	}

	set.tg.util(tg = tg, util.funs=util.funs)
	#oco.df = tg$oco.df
}


pref.class.info.panels = function(pref.classes) {
  restore.point("pref.class.info.panels")
  li = lapply(names(pref.classes), function(class) {
    pc = pref.classes[[class]]
    ex = pref.class.example.code(pc, class)
    pre.html = HTML(paste0('<pre style="tab-size: 4"><span class="inner-pre" style="font-size: small">',ex,'</span></pre>'))
    tabPanel(title=str.left.of(class,"Util"),value=class,
      HTML(paste0("<p style='padding-top: 4px; line-height: 1.1'>",pc$descr,"</p>")), pre.html
    )
  })
  ui = withMathJax(do.call(tabsetPanel, c(list(id="prefInfoTabset"),li)))
  ui
}

pref.class.example.code = function(pc, class=pc$class) {
  par.txt = ""
  if (NROW(pc$params)>0) {
    param.code = lapply(names(pc$params), function(param) {
      p = pc$params[[param]]
      str = paste0("\t\t", param, ": ", p$default)
      if (!is.null(p$descr))
        str = paste0(str," # ",p$descr)
      str
    })
    param.code = paste0(param.code,collapse="\n")
    par.txt = paste0("\n\tparams:\n", param.code)
  }


  name = first.non.null(pc$defaultName, class)
  yaml = paste0(name,":\n\tclass: ",class,par.txt)
  yaml
}
