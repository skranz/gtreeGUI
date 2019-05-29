examples.parse.pref.yaml = function() {

yaml = '
name: _auto
envyUtil:
	name: _auto # envyUtil_params...
	player: _all
	prob: 1  # probabiliy of type
	alpha: 1 # the degree of envy
'
  parse.pref.yaml(yaml)

  file = system.file("spec/pref_classes.yaml")
}

parse.pref.yaml = function(yaml, li=NULL) {
  restore.point("parse.pref.yaml")
  if (is.null(li))
    li = read.yaml(text=yaml)

  li

}


gx.pref.types.panel = function(id.prefix="prefTypes_", types=get.types(), pref.types =  types$prefType$subTypes, position="below") {
  restore.point("pref.types.panel")
  #utypes =
  li = lapply(pref.types, function(pt) {
    type = types[[pt]]
    ex = make.prefs.example.yaml(pt)
    pre.html = HTML(paste0('<pre style="tab-size: 4"><span class="inner-pre" style="font-size: small">',ex,'</span></pre>'))

    tabPanel(title=str.left.of(pt,"Util"),value=pt,
      p(paste0(type$descr," Example:"), pre.html)
    )
  })
  ui = do.call(tabsetPanel, c(list(id=paste0(id.prefix,"Tabset")),position=position,li))
  ui
}



make.prefs.example.yaml = function(pref.types =  types$prefType$subTypes, types=get.types() ) {
  restore.point("make.default.prefs.yaml")

  #pname = pref.types[[2]]
  li = lapply(pref.types, function(pname) {
    pt = get.type(typeName=pname)
    if (!is.null(pt$example)) return(as.character(pt$example))
    class = str.left.of(pname,"Util")
    name = pt$defaultName
    if (is.null(name)) name = class
    params = lapply(pt$params, function(par) {
      paste0(quote.char(par$default,"'"), " # ", par$descr)
    })
    names(params) = names(pt$params)
    pref = c(list(class=class, player = "_all", prob = 1),params)
    pref

    paste0(name,":\n","\t",name,":\n",
           paste0("\t\t",names(pref),": ", pref,collapse="\n"))

  })
  yaml = paste0(li, collapse="\n\n")
  #cat(yaml)
  yaml
}

