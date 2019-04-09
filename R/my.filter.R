#' my.filter
#' 
#' @param orcid.input Result of a call to my.orcid() from this package
#' @param years.since Yeats of publications you want returned
#' @param max.authors Number of authors you want listed
#' @param spacer Commas between authors
#' @param journal.only Include just journal articles in output. Options 'Yes' or 'No'
#' @param order Either 'ayear' for most recent last or 'dyear' for most recent first
#' @param bold.author Option to bold author of CV in publications (TRUE or FALSE)
#' 
#' @export
my.filter = function(orcid.input, years.since=2000, max.authors=3, spacer=', ', 
                     journal.only='Yes', order='ayear', bold.author=T){

  res = data.frame(NULL)
res = orcid.input$papers
# bold author
authors = orcid.input$authors
if(bold.author==T){
  for (k in 1:nrow(authors)){
    authors[k, orcid.input$author.order[k]] = paste('**', authors[k, orcid.input$author.order[k]], '**', sep='') # add bolding
  }
}
# add authors
if(max.authors == 1){res$Authors = authors[,1]}
if(max.authors > 1){
  upper.limit = min(c(max.authors, ncol(authors)))
  res$Authors = apply(authors[, 1:upper.limit], 1, paste5, collapse=spacer) # 
} 
# add 'et al'
if(max.authors < ncol(orcid.input$authors)){ # don't add if at max author number
  index = orcid.input$authors[, max.authors+1] != '' # something in next author
  res$Authors[index] = paste(res$Authors[index], spacer, 'et al', sep='')
}
# filter by year:
res = subset(res, Year >= years.since) 
# journal articles only
if(journal.only=='Yes'){
  index = grep(pattern='journal', tolower(res$Type)) # search for journal in type
  res = res[index, ]
}

## ordering  (this part comes from server)
papers = res
papers$Year = as.numeric(papers$Year) # for sorting
if(order=='ayear'){papers = arrange(papers, Year)} #
if(order=='dyear'){papers = arrange(papers, -Year)} # 
if(order=='journal'){papers = arrange(papers, Journal, Year)} # 
papers$Year = as.character(papers$Year) # looks better as character
## return
return(papers)
}