
example.gambit.job = function() {
	# set working directory to project directory
  setwd("D:/libraries/gtree/myproject/")

	gameId = "Cournot"
	gameId = "UltimatumGame"
	tg = get.tg(gameId = gameId,never.load = FALSE)

	tg.to.efg(tg=tg)

	job = start.gambit.job(tg=tg)
	is_pid_running(job$pid)

	job.li = xs.load.job.li()
	job.df = job.li.to.job.df(job.li)
	format.vals(job.df$start.time)
}

#' Finds one or all mixed strategy equilibria
start.gambit.job = function(tg, mixed=FALSE, just.spe=TRUE, efg.file=tg.efg.file.name(tg), efg.dir=get.efg.dir(tg$gameId), gambit.dir="", solver=NULL, eq.dir = get.eq.dir(tg$gameId), solvemode=NULL, jobs.dir = file.path(get.project.dir(),"jobs"), eq.id =  get.eq.id(tg=tg, solvemode = solvemode, mixed=mixed, just.spe=just.spe), ...) {

  restore.point("start.gambit.job")

	# internal solver not using gambit
	if (isTRUE(solvemode=="spe_xs")) {
		# job not yet implemented
		stop("Asynchronious job not yet implemented for internal algorithm.")
		#return(solve.all.tg.spe(tg=tg, eq.dir=eq.dir,save.eq=save.eq))

	}

	solver = get.gambit.solver(solver=solver, mixed=mixed, just.spe=just.spe, solvemode=solvemode)

	solver.bin = str.left.of(solver," ")
	solver.args = str.right.of(solver," ")
	cmd = paste0(gambit.dir, solver.bin)

	args = c(solver.args, file.path(efg.dir,efg.file))

	out.file = file.path(jobs.dir, paste0(eq.id, ".out"))

  #solver = "gambit-enumpure -q -P -D"
  pid  = exec_background(cmd,args = args,std_out = out.file)


  job = gambit.job.object(id=eq.id, pid=pid, tg=tg, out.file=out.file,  state="running")

  # Save job object.
  # This allows to continue monitoring jobs
  # when the gtree gui is started again
  saveRDS(job, file.path(jobs.dir,paste0(job$id,".job")))

  job
}





gambit.job.object = function(id, pid=NA, tg, out.file, start.time=Sys.time(), state="running") {

  as.environment(nlist(id=id, pid=pid, tg.id = tg$tg.id, gameId = tg$gameId,variant=tg$variant, jg.hash=tg$jg.hash, out.file, start.time=start.time, written.time=NA, last.check.time = start.time, state, eq.li=NULL))
}



save.job.eq.li = function(job,eq.li=job$eq.li, eq.id = job$id,  eq.dir=get.eq.dir(job$gameId),...) {
	eq.id = job$id
  eq = list(
		eq.id = eq.id,
		tg.id = job$tg.id,
		gameId = job$gameId,
		variant = job$variant,
		jg.hash = job$jg.hash,
		eq.li = eq.li
	)
	file = paste0(eq.dir,"/",eq.id,".eq")
	saveRDS(eq,file)
}


xs.load.job.li = function(jobs.dir = get.jobs.dir(), old.jobs=NULL) {
  restore.point("xs.load.job.li")

  files = list.files(jobs.dir, glob2rx("*.job"),full.names = TRUE)

  job.li = lapply(files, function(file) {
    # Return already existing job
    if (length(old.jobs)>0) {
      job.id = tools::file_path_sans_ext(basename(file))
      old.job = old.jobs[[job.id]]
      if (!is.null(old.job)) return(old.job)
    }

    job = readRDS(file)
  })
  names(job.li) = sapply(job.li, function(job) job$id)

  job.li
}

job.li.to.job.df = function(job.li=xs.load.job.li()) {
  restore.point("job.li.to.job.df")


  li = lapply(job.li, function(job) {
    jl = as.list(job)
    cols = c("id", "pid","state", "start.time","last.check.time", "written.time", "out.file")
    do.call(data_frame,jl[cols])
  })
  df = bind_rows(li)
  #df$job.li = job.li
  #rownames(df) = df$id
  df
}



xs.show.jobs.tab = function(xs=app$xs, app=getApp(), select=TRUE) {
  restore.point("xs.show.jobs.tab")
  tabId = paste0("tab_jobs")
  if (tabId %in% xs$tabs) {
    if (select)
      w2tabs.select("xsTabs", tabId)
    return()
  }
  xs$tabs = c(xs$tabs, tabId)

  divId = paste0("div_jobs")
  tab=list(id=tabId,caption="Jobs", closable=TRUE,div_id = divId)
  w2tabs.add(id="xsTabs", tabs=list(tab), select=select)

  ui = xs.jobs.ui()
  appendToHTML(selector="#mainDiv", as.character(hidden_div(id=divId, ui)))

  xs$job.li = xs.load.job.li()
  set.jobs.table.ui()

  start.jobs.observer()

  if (select)
    w2tabs.select("xsTabs", tabId)
}

xs.jobs.ui = function(xs = app$xs, app=getApp(),...) {
	restore.point("xs.jobs.ui")

	ns = NS("jobs")
  ui = list(
  	#HTML("<table><tr><td>"),
    #smallButton(ns("refreshBtn"), "Refresh all"),
  	#HTML("</td><td>"),
  	#HTML("</td></tr></table>"),
    #tags$br(),
    uiOutput("jobsTableUI"),
    uiOutput(ns("msg")),
    smallButton(ns("clearBtn"), "Clear all finished jobs")
  )
  dsetUI(ns("msg"),"")
  #buttonHandler(ns("refreshBtn"),fun = refresh.jobs.click)
	buttonHandler(ns("clearBtn"),fun = clear.finished.jobs.click)
  ui
}

set.jobs.table.ui = function(xs = app$xs, app=getApp(),job.li = xs$job.li,...) {
  restore.point("set.jobs.table.ui")

  xs$job.df = df = job.li.to.job.df(job.li)

  if (NROW(df)>0) {

    df$running = sapply(1:NROW(df), function(row) {
      ifelse(df$state[row]!="running",
        format(df$written.time[row]-df$start.time[row], digits=3),
        format(Sys.time()-df$start.time[row], digits=3)
      )
    })

    df$last.check =format(Sys.time()-df$last.check.time,digits=3)

    smallButton("id","label")
    stop.btns =  paste0('<button id="job-stop-btn-',df$id,'" style="" type="button" data-jobid="', df$id,'" class="btn btn-default action-button btn-xs xs-job-stop-btn">Cancel Job</button>')
    clear.btns =  paste0('<button id="job-clear-btn-',df$id,'" style="" type="button" data-jobid="', df$id,'" class="btn btn-default action-button btn-xs xs-job-clear-btn">Clear</button>')

    df$btns = ifelse(df$state != "running", clear.btns, stop.btns)

    bg = ifelse(df$state != "running","#dddddd","#ffffff")

    df = df %>%
      arrange(desc(start.time)) %>%
      select(btns, state, running, id, start.time,pid)

    table = html.table(df,col.names = c("","State","Runtime","Name","Start","PID"),bg.color = bg)

  } else {
    table = ("<p>There are currently no jobs to solve equilibria.</p>")
  }

  classEventHandler("xs-job-stop-btn",event = "click", stop.job.click)
  classEventHandler("xs-job-clear-btn",event = "click", clear.job.click)

  setUI("jobsTableUI",HTML(table))
  dsetUI("jobsTableUI",HTML(table))
}


start.jobs.observer = function(millis=1000, xs=app$xs, app=getApp()) {
  restore.point("start.jobs.observer")

  if (is.null(app$jobs.observer)) {
    app$jobs.observer = observe({
      refresh.jobs(reload.jobs=FALSE)
      invalidateLater(millis)
    })
  }
}

stop.jobs.observer = function(..., app=getApp()) {
  if (!is.null(app$jobs.observer)) {
    app$jobs.observer$destroy()
    app$jobs.observer = NULL
  }
}

refresh.jobs.click = function(...) refresh.jobs(...)

refresh.jobs = function(..., reload.jobs = TRUE,xs=app$xs, app=getApp()) {
  restore.point("refresh.jobs.click")


  # Load new jobs
  if (reload.jobs)
    xs$job.li = xs.load.job.li(old.jobs = xs$job.li)

  job.li = xs$job.li
  updated = reload.jobs
  for (job in job.li) {
    if (job$state=="running") {
      updated = TRUE
      update.xs.job(job)
    }
  }
  if (updated)
    set.jobs.table.ui(job.li = xs$job.li)
}


is_pid_running = function(pid) {
  #res = system2( 'tasklist' , stdout = TRUE )

	res = try(exec_status(pid, wait=FALSE),silent = TRUE)
	if (is(res,"try-error")) return(FALSE)
	if (is.na(res)) return(TRUE)
	return(FALSE)

}


is.running.gambit.job = function(job) {
  fi = file.info(job$out.file)
  # outfile has been written
  if (fi$size>0) {
    return(FALSE)
  }

  # check if PID is still running
  # this function may take around a second...
  is_pid_running(job$pid)
}


update.xs.job = function(job, update.file=TRUE, jobs.dir = get.jobs.dir(), xs=app$xs) {
  #if (job$state != "running") return(job)

  running = is.running.gambit.job(job)
  job$last.check.time = Sys.time()

  # job has finished
  if (!running) {
    restore.point("finish.job")
    xs.make.job.eq.li(job)
    save.job.eq.li(job)
    if (job$state == "running")
      job$state = "finished"
    fi = file.info(job$out.file)
    job$written.time = if( fi$mtime > job$start.time ) {fi$mtime} else { job$start.time}
    saveRDS(job, file.path(jobs.dir,paste0(job$id,".job")))

    # Update equilibrium tab
    xeq = app$xs$xeq.li[[job$gameId]]
    if (!is.null(xeq)) {
      restore.point("update.eq.tab.due.to.job")
      xeq$running.jobs = setdiff(names(xeq$running.jobs), job$tg.id)


      xeq.show.running.info(xeq)
      if (job$state == "finished") {
        xeq.load.eq(xeq,eq.id = job$id, tg.id = job$tg.id)
        solve.time = job$written.time-job$start.time
        attr(xeq$eq.li[[job$tg.id]],"solve.time") <- solve.time
        xeq.show.tg.info(xeq)
        xeq.show.eqo(xeq)
        xeq.show.conditional.eqo(xeq)
      }
    }

  }

  file.info

}

xs.make.job.eq.li = function(job) {
  restore.point("xs.make.job.eq.li")
  txt = readLines(job$out.file)
  job$eq.li = gambit.out.txt.to.eq.li(txt, tg=get.tg(variant = job$variant, gameId = job$gameId))
}


xs.is.job.running = function(job.id=job$id,job = xs$job.li[[job.id]], xs= app$xs, app=getApp()) {
  if (is.null(job)) return(FALSE)
  isTRUE(job$state == "running")
}

stop.job.click = function(id,data,...,xs=app$xs, app=getApp()) {
  restore.point("stop.job.click")
  job = xs$job.li[[data$jobid]]
  update.xs.job(job)

  if (job$state == "running") {
    res = tools::pskill(job$pid)
    cat("\nKilled job ", job$pid, "? ", res)

    job$state = "cancelled"
    job$written.time = Sys.time()
    update.xs.job(job)
  }
  set.jobs.table.ui(job.li = xs$job.li)
}


clear.job.click = function(id,data,...,xs=app$xs, app=getApp()) {
  restore.point("clear.job.click")
  jobs.dir = get.jobs.dir()
  job.id = data$jobid

  file = file.path(jobs.dir,paste0(job.id,".job"))
  try(file.remove(file))

  refresh.jobs()
}


clear.finished.jobs.click = function(...,xs=app$xs, app=getApp()) {
  jobs.dir = get.jobs.dir()

  for (job in xs$job.li) {
    if (!isTRUE(job$state == "running")) {
      file = file.path(jobs.dir,paste0(job$id,".job"))
      try(file.remove(file))
    }
  }

  refresh.jobs.click()
}

