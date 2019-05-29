match.es.game.subjects = function(es, part) {
  restore.point("match.es.game.subjects")

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

}

make.random.matching = function(num.sub=length(subIds),num.players, num.matches = floor(num.sub / num.players), subIds = 1:num.sub,...) {
  restore.point("game.random.matching")

  matchInds = 1:num.matches
  df = expand.grid(matchInd = matchInds,player=1:num.players)
  df$subjectId = sample(subIds,size = NROW(df),replace = FALSE)
  df
}

examples.rotation.matching = function() {
  rotation.matching(8,3, wide=TRUE)

  ma = jump.rotation.matching(22,10, wide=FALSE)

  max.round.perfect.stranger(ma)
}

#' A matching procedure for two player games
#'
#' Rotation matching guarantees that an action of player i will never directly or
#' indirectly influence the behavior of their future matching partner.
#' This is a stronger guarantee than perfect stranger matching, which only
#' guarantees that a player will never play again a matching partner, but
#' does not preclude the possibilities of indirect contagion effects.
#'
#' In rotation matching the even number of subjects is split in two
#' equal sized groups.
#' The first group is arranged on a circle.
#' The members of the second group are randomly assigned in the first round.
#' In each subsequent round the members of the second group
#' move on step to the right.
#'
#' A detailed discussion of rotation matching is given by
#' Ullrich Kamecke, "Rotations: Matching schemes that efficiently preserve the best reply structure of a one shot game", International Journal of Game Theory (1997)
#'
#' TO DO: One problem of rotation matching is same order
#' we can use jump.rotation matching
rotation.matching = function(num.sub, num.games, rotate.vec = NULL, wide=FALSE) {
  restore.point("rotation.matching")

  subs = 1:num.sub
  subs1 = sample(subs,floor(num.sub/2))
  subs2 = setdiff(subs, subs1)

  num.groups = length(subs1)

  df = expand.grid(group=1:num.groups,game=1:num.games)
  df = as_data_frame(df[,c("game","group")])

  # Player 1 for group g is the same in every period
  # Player 2 rotates
  if (is.null(rotate.vec))
    rotate.vec = 1:num.games

  rotate.add = rotate.vec[df$game]-1

  ind1 = df$group
  ind2 = 1+((df$group + rotate.add-1) %% length(subs2))

  if (wide) {
    df$player_1 = subs1[ind1]
    df$player_2 = subs2[ind2]
    return(df)
  }

  dat = rbind(
    cbind(df, player=1, subject=subs1[ind1]),
    cbind(df, player=2, subject=subs2[ind2])
  ) %>% arrange(game, group, player)
  dat
}


jump.rotation.matching = function(num.sub, num.games, rotate.vec = NULL, wide=FALSE, fill.with.random.rotation=TRUE) {
  restore.point("jump.rotation.matching")
  if (is.null(rotate.vec)) {
    rotate.step = c(0:(num.games-1))
    rotate.vec = cumsum(rotate.step)+1

    max.rot = floor(num.sub / 2)
    # If we exceed the maximum number of matches
    # rotate to missing numbers first
    excess = sum(rotate.vec> max.rot)
    if (excess > 0) {
      if (!fill.with.random.rotation)
        stop("Too many games for jump rotation. Perhaps set fill.with.random.rotation to TRUE")
     missing = sample(setdiff(1:max.rot, rotate.vec))
      if (length(missing)>=excess) {
        rotate.vec[rotate.vec>max.rot] = missing[1:excess]
      } else {
        stop("Too many games for jump rotation.")
      }
    }
  }
  rotation.matching(num.sub, num.games,rotate.vec, wide)
}

# Computes the maximum round until we have perfect
# stranger matching (only for two player games so far)
max.round.perfect.stranger = function(matching) {
  ma = matching

  if (max(ma$player)>2)
    stop("Works so far only for 2 player matchings.")

  df = ma %>% group_by(game, group) %>%
    summarize(id = paste0(sort(subject), collapse=";"))

  dupl = duplicated(df$id)
  if (!any(dupl))
    return(0)
  min(df$game[dupl]-1)

}
