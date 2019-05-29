# will be called when a default page for a stage is initialized
# in a match, before the whiskers of the page are rendered
gtree.addin.freetext.init.page = function(ai,em=get.em(), player=em$player, ...) {
  restore.point("gtree.addin.freetext.init.page")
  id = paste0("freetext-", player,"-",ai$name)
  add.em.page.submit.ids(id=id, em=em)
}


# will be called when a default page for a stage is created
gtree.addin.freetext.page.txt = function(ai, rg, ...) {
  txt = paste0('{{freetext_input(name="', ai$name,'", label="', ai$name,'", value="")}}')
  txt
}


# will be called when a default page for a stage is created
gtree.addin.show_freetext.page.txt = function(ai, rg, ...) {
  txt = paste0('\n<!--Show previously entered free text-->\n{{',ai$name,'}}')
  txt
}


# will be called when the submit button of a stage page is pressed
gtree.addin.freetext.submit.page = function(ai, player, stage, stage.ind, formValues=NULL, ..., em=get.em()) {
  restore.point("gtree.addin.freetext.submit.page")

  name = ai$name
  id = paste0("freetext-", player,"-",name)
  value = formValues[[id]]
  chars = nchar(str.trim(value))
  if (isTRUE(chars > as.numeric(ai$maxLength)))
    return(list(ok=FALSE,msg=paste0("Your text has ", chars, " characters, but you can enter at most ", ai$maxLength, " characters.")))
  if (isTRUE(chars < as.numeric(ai$minLength)))
    return(list(ok=FALSE,msg=paste0("Your text has ", chars, " characters, but you must enter at least ", ai$minLength, " characters.")))

  return(list(ok=TRUE, msg="", values = set.names(list(value), ai$name)))
}

# we need to get a function to get the current addin by its id
freetext_input = function(name=ai$name,label=ai$name, value="", ..., em=get.em(), app=getApp(), ai = get.em.ai(name, em=em)) {
  restore.point("freetext_input")
  id = paste0("freetext-", em$player,"-",name)
  ui = textAreaInput(id,label = label,value = value)
  ui
}
