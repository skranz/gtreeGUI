# gtree: Game-Theoretic Representation of Economic Experiments

Author: Sebastian Kranz, Ulm University

Date: 2017-04-24

## Overview

gtree is an R package with a shiny-based web GUI that allows to develop, conduct and game theoretically analyse economic experiments. The experiments run on a webserver, and can e.g. be conducted in a lecture hall, where students participate via smart phone.

There already exists two great toolboxes to develop and run economic experiments:

  1. The classic [zTree](http://www.ztree.uzh.ch/en.html), created by Urs Fischbacher (2007). It is very flexible and has allowed to realize thousands of economic experiments in economic laboratories all around the world.
  
  2. The newer [oTree](http://www.otree.org/), created by Daniel Chen, Martin Schonger and Chris Wickens (2016). Experiments can be specified in Python with Django and are conducted via the web. Like with gtree, subjects can participate with almost any device that can browse the web. 

gtree is structured with the goal in mind that typical multi-stage experiments can be created fairly easily and quickly. I would like it to be easy enough such that most economics students are able to enter such experiments as part of a seminar paper.

While not yet fully implemented, in a not-so-distant future version, there will be flexibility to specify all sorts of more complicated experiments, like real-effort games or real time auctions. Whatever you will be able to program with R & Shiny or Javascript can in principle be a part of an experiment, similar as oTree allows to employ the full flexibility of using Python & Django and Javascript.

Overall, developing and running experiments in gtree differs in some aspects, but not in too many aspects, from the two existing toolboxes.

The main contribution is that typical multi-stage experiments are entered in a format that allows direct conversion to a game tree (short a gtree!). Hence, you specify an experiment in a form that is similar to zTree or oTree but you can immediately also perform game theoretic analysis. 

You can solve equilibria for standard preferences (risk-neutral money maximizer) or specify some other preferences like inequity aversion or loss aversion.

While gtree also has a rudimentary internal pure strategy SPE solver, it mainly uses [Gambit](http://www.gambit-project.org/) (by Richard McKelvey,  Andrew McLennan und Richard D., McLennan, Andrew M., and Ted Turocy, 2016) to compute the equilibria.


The possibility of easy game theoretic analysis, can e.g. be helpful for...

  - the analysis of the experiment when comparing observed behavior with game theoretic predictions.
  
  - the development of the experiment, e.g. to search for parameters that will yield nicely separated game theoretic predictions for different treatments.
  
  - meta studies that compare a large number of economic experiments from a game theoretic perspective

# Tutorial: Create an Ultimatum Game

## 1 Create a new game

In the left panel of the gtree explorer, you see a list of all games specified in your project. To create a new game, *right-click* right on the "Games" folder (or any existing game) and choose the "New Game" option in the context menu:

![new_game.PNG](figures/new_game.PNG)

Then a window opens that asks you to enter a name (gameId) of the game. We pick "UltimatumGame" and press Ok. Note that a gameId can contain only letters and numbers, no spaces nor underscores _. That is because underscores are used in identifiers for different game variants.  

![new_game_name.PNG](figures/new_game_name.PNG)

Then a tab opens in the main window that contains a tree with the initial game structure.

![new_game_shown.PNG](figures/new_game_shown.PNG)

## 2. Add new parameters
 
We first open the node "Variants, Params". We see a table in which we can specify different parameters (columns) and different variants (rows) for the game.
The parameter "numPlayers" is given and by default set to 2. If your game has a different number of players, you can change it simple by clicking on the cell and changing the number.

To add or remove parameters or variants, we right click on a table cell and pick the corresponding option to insert or remove columns or rows.

<img src="figures/ug_varpar_1.PNG">

We want to add a new parameter "cake" that specifies the size of the cake that can be split between the propose (player 1) and responder (player 2). So we right click on the cell "numPlayers" and pick the option "insert column on the right".

We enter the parameter name "cake" in the title row and the value of the parameter in the row for our only game variant "base"

<img src="figures/ug_varpar_2.PNG">

Here, we have set the cake size to 10. 

## 3. Save Game

It is useful to save your game from time to time. To do so, simply press the "Save" Button.

<img src="figures/ug_save.PNG">

## 4. A glimpse on the file structure

The game files will be saved in the folder

\<projectdir\>/games/\<gameId\> 

That folder is automatically generated, when we create a new game. You can have a look at this folder in a file explorer

<img src="figures/ug_save_2.PNG">

The subfolders "eq","gambit" and "pages" are currently all empty:

  - "eq" will contain files for computed equilibria
  
  - "gambit" will contain gametree representations in Gambits "efg" format
  
  - "pages" will contain customizable markdown files that specify how the stages are shown to subjects.
  
The file "UltimatumGame.json" contains the game specification as a .json file. It currently looks as follows:

    {"game": {
        "gameId": "UltimatumGame",
        "gameInfo": {
            "label": "",
            "tags": "",
            "descr": "",
            "articles": "",
            "variantOf": ""
        },
        "varpar": [
            [
                "variants<U+2193> params<U+2192>",
                "numPlayers",
                "cake",
                "descr"
            ],
            [
                "base",
                "2",
                "10",
                "The base variant"
            ]
        ],
        "stages": [
            {
                "name": "actionStage1",
                "descr": "",
                "player": "1",
                "condition": "",
                "observe": "",
                "nature": [],
                "compute": [],
                "actions": [],
                "special": {
                    "beliefs": [],
                    "freetext": []
                }
            },
            {
                "name": "resultsStage",
                "descr": "",
                "player": "[1,2]",
                "condition": "",
                "observe": "[payoff_1,payoff_2]",
                "nature": [],
                "compute": [
                    {
                        "name": "payoff_1",
                        "formula": "=0"
                    },
                    {
                        "name": "payoff_2",
                        "formula": "=0"
                    }
                ],
                "actions": [],
                "special": {
                    "beliefs": [],
                    "freetext": []
                }
            }
        ]
    }}

In principle, you could specify games by manually adapting this json file. Using the gtree explorer should usually be much more convenient, however. Nevertheless, directly modifying the json file may be useful in some situations, e.g. to quickly change variable names via find and replace with a text editor.

## 5. Specifying the offer stage

We now go to the stages and open the automatically created stage with name "actionStage". We adapt it as follows:

<img src="figures/ug_offerstage.PNG">

We first change the stage name to "offerStage".

We then enter a short stage description in the field `descr`. Stage description are optional and is only used for experimenters to better understand the structure of the experiment.  

We set the `player` field to 1, since only player 1 will see this stage and act in it.

We keep the field `condition` empty, it will be explained in later examples.

Also the field `observe` is empty, since there is nothing yet to observer. Note that parameters are automatically assumed to be known to all players, i.e. players should be given that information in the instructions. 

Then we add an action: we right-click on the tree element "action" and choose the option "Add action" and enter the following specification:

<img src="figures/ug_offerstage_2.PNG">

We call the action "offer".

The field `set` specifies a finite set of choices, i.e. possible values of offer. We have entered a formula:

`=0:cake`

Reminiscent of popular spreadsheet software, a field that starts with an equal sign ("="), will be typically be parsed as an R formula. The command `0:cake` is R code that generates a sequence of integer numbers between 0 and the value of our parameter `cake`.

If we would have always fixed the cake to some specific size, e.g. 10, we could also have entered the action set as a list in a non-formula format: 

`[0,1,2,3,4,5,6,7,8,9,10]`

However, using R formulas allows much more flexibility.

The fields `strategyMethodDomain` and `labels` are not required here, and we leave them empty.





# The Vision: A structured experimental economic database

I hope that gtree is in itself useful to develop, run and analyse new economic experiments, as well as, for teaching purposes where experiments are replicated and analysed in a classroom.

My original motivation for developing gtree, however, is to use it as one of several tools to build up a database that contains the data, structure and game theoretic representation of a large number of performed economic experiments. Such a database should facilitate meta studies across a large number of experiments and hopefully provide better insights into economic behavior.

I am not yet sure of the exact further steps towards such a database. Yet, some ideas are the following:

  - Collect experimental data and background data from journal data archives and directly from colleagues.
  - Develop and test a format for Bachelor or Master seminars where students convert these experiments into the gtree format and run some replications and analyses during the seminar.
  - Have a sensible scheme of manually tagging experiments (e.g. "Ultimatum Game Variant", "neutral-framing","market-framing",...).
  - Have functions that automatically characterize and tag experiments and specific actions based on the game theoretic structure (e.g. a two-stage game, an accept-reject action, an action in which higher choices correspond to *nicer* choices, a signalling game,...)
  - Store the data, experimental structure, computed equilibria, tags, etc, in well named git repositiories, e.g. on Github.
  - Create an index database that stores tags and other main indicators for these experiments. Also develop tools with web interface that allow to search and download subsets of experiments from these repositories.
  



# References

- Fischbacher, U., 2007. "z-Tree: Zurich toolbox for ready-made economic experiments", Experimental Economics 10: 171.
- Chen, Daniel L., Martin Schonger and Chris Wickens (2016). "oTree—An open-source platform for laboratory, online, and field experiments", Journal of Behavioral and Experimental Finance.
- McKelvey, Richard D., McLennan, Andrew M., and Turocy, Theodore L. (2016). Gambit: Software Tools for Game Theory, Version 16.0.0. http://www.gambit-project.org.
