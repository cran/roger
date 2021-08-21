### roger: Automated grading of R scripts
###
### Unexported auxiliary functions to extract parents, children and
### siblings (children of the same parent) of items from a parse
### information data frame.
###
### AUTHORS: Jean-Christophe Langlois, Vincent Goulet <vincent.goulet@act.ulaval.ca>
### LICENSE: GPL 2 or later

getParseParent <- function(parseData, id)
    parseData[as.character(id), "parent"]

getParseGParent <- function(parseData, id)
    getParseParent(parseData, getParseParent(parseData, id))

getParseGGParent <- function(parseData, id)
    getParseParent(parseData, getParseGParent(parseData, id))

getParseChildren <- function(parseData, id)
    parseData$id[parseData$parent == id]

getParseSiblings <- function(parseData, id)
{
    ## First find the id of the parent...
    parent_id <- getParseParent(parseData, id)

    ## ...and then the ids of the children.
    children_id <- getParseChildren(parseData, parent_id)

    ## Siblings are children other than 'id'.
    setdiff(children_id, id)
}

getParseAdjSiblings <- function(parseData, id)
{
    ## First find the id of the parent...
    parent_id <- getParseParent(parseData, id)

    ## ...and then the ids of the children.
    children_id <- getParseChildren(parseData, parent_id)

    ## Adjacent siblings come immediately before and after 'id' in the
    ## parse information.
    children_id[c(-1L, 1L) + which(children_id == id)]
}
