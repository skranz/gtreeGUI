example.load.addins = function() {
  ai.li = xs.load.addins()
  xs.load.ressources()

  xs_types = xs.merge.addins.into.xs_types(ai.li, xecon.glob$xs_types)
}

xs.load.addins = function(addin.dir = system.file('addins', package='gtree')) {
  restore.point("xs.load.addins")

  # multiple addin directories
  if (length(addin.dir)>1) {
    ai.li = lapply(xs.load.addins, addin.dir)
    return(do.call(c, ad.li))
  }

  files = list.files(addin.dir,glob2rx("*.json"),full.names = TRUE)
  ai.li = lapply(files, function(file) {
    restore.point("hsfhhf")
    try.with.msg(read_json(file),paste0("Error when parsing addin file ", file))
  })
  do.call(c, ai.li)

}

xs.merge.addins.into.xs_types = function(ai.li,xs_types) {
  restore.point("xs.merge.addins.into.xs_types")

  cats = sapply(xs_types$stageSpecial$fields, function(f) f$name)
  ai.cats = sapply(ai.li, function(ai) ai$category)

  # check if all addins specify a known category
  unknown = which(!ai.cats %in% cats)
  if (length(unknown)>0) {
    ai.names = sapply(ai.li[unknown], function(ai) ai$name)
    msg = paste0("The addins ", paste0(ai.names, collapse=", "), " specify unknown categories. Please set the category field in the json file to one of the following values: ", paste0('"',cats,'"', collapse=", "),".")
    stop(msg)
  }


  for (cat.ind in seq_along(cats)) {
    cat = cats[cat.ind]
    rows = which(ai.cats == cat)
    if (length(rows)==0) next

    li = lapply(ai.li[rows], function(ai) {
      list(name=ai$name, childType=ai$name, isList=TRUE, isAddinList=TRUE, expanded=TRUE)
    })
    names(li) = NULL
    xs_types[[cat]]$fields = c(xs_types[[cat]]$fields, li)
  }

  # add addin types
  types.li = lapply(ai.li, function(ai) {
    c(ai["fields"], list(isAddin=TRUE,expanded=TRUE))
  })
  xs_types[names(types.li)] = types.li
  xs_types
}

get.em.ai = function(name,  em=get.em()) {
  restore.point("get.em.ai")
  stage.num = em$player.stage[em$player]
  stage = em$vg$stages[[stage.num]]

  stage$ai.li[[name]]
}

gtree.addin.fun = function(fun=c("init.page","page.txt","submit.page")[1], args, type=ai$type,ai=NULL, envir=parent.frame()) {
  fun.name = paste0("gtree.addin.", type,".",fun)
  do.call.if.exists(fun.name, args, envir=envir)
}
