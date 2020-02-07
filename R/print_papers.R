#' Print the publications in cv

#' @param papers Result of a call to my.filter()

#' @export
print_papers <- function(papers){
  
  # bullets (turning words into TRUE/FALSE)
  use.bullets = FALSE
  if(params$bullets=='bullets'){use.bullets = TRUE}
  # how to highlight Open Access papers:
  star.to.use = "\\+ " 
  
# no extra ordering
if(params$extra.order == 'None'){
  to.display = display.papers(papers, in.style=params$style, star.to.use=star.to.use, flag.OA=params$flag.OA, bullets=use.bullets)
  cat(to.display)
}

# ARC - different ordering
if(params$extra.order == 'ARC'){
  ## sort by output type 
  counter = 0
  # a) books
  index = grep('book', tolower(papers$Type))
  if(length(index)>0){
    cat('## Books\n', sep='\n')
    books = papers[index,]
    for (k in 1:nrow(books)){
      star = ""
      if(books$OA[k]==TRUE & params$flag.OA==TRUE){star = star.to.use} # star open access
      counter = counter + 1 
      cat(counter, '. ', star, books$Authors[k], ", ", books$Year[k], ", '", books$Title[k], "', *", books$Journal[k], '*', sep='')
      # add doi if not null
      if(is.na(books$doi[k])==F){cat(', doi:', books$doi[k], sep='')}
    }
  }
  # b) journal articles
  index = grep('journal', tolower(papers$Type))
  if(length(index)>0){
    cat('## Journal articles\n', sep='\n')
    journals = papers[index,]
    for (k in 1:nrow(journals)){
      star = ""
      if(journals$OA[k]==TRUE & params$flag.OA==TRUE){star = star.to.use} # star open access
      counter = counter + 1 
      cat(counter, '. ', star, journals$Authors[k], ", ", journals$Year[k], ", '", journals$Title[k], "', *", journals$Journal[k], '*', sep='')
      # add doi if not null
      if(is.na(journals$doi[k])==F){cat(', https://dx.doi.org/', journals$doi[k], sep='')}
      cat('  \n', sep='') # line break
    }
  }
  # c) conferences 
  index = grep('conference', tolower(papers$Type))
  if(length(index)>0){
    cat('## Journal articles\n', sep='\n')
    conferences = conferences[index,]
    for (k in 1:nrow(conferences)){
      star = ""
      if(conferences$OA[k]==TRUE & params$flag.OA==TRUE){star = star.to.use} # star open access
      counter = counter + 1 
      cat(counter, '. ', star, conferences$Authors[k], ", ", conferences$Year[k], ", '", conferences$Title[k], "', *", conferences$Journal[k], '*', sep='')
      # add doi if not null
      if(is.na(conferences$doi[k])==F){cat(', doi:', conferences$doi[k], sep='')}
      cat('  \n', sep='') # line break
    }
  }
  # d) other
  index = grep('conference|journal|book', tolower(papers$Type), invert = T)
  if(length(index)>0){
    cat('## Other\n', sep='\n')
    other = papers[index,]
    for (k in 1:nrow(other)){
      star = ""
      if(other$OA[k]==TRUE & params$flag.OA==TRUE){star = star.to.use} # star open access
      counter = counter + 1 
      cat(counter, '. ', star, other$Authors[k], ", ", other$Year[k], ", '", other$Title[k], "', *", other$Journal[k], '*', sep='')
      # add doi if not null
      if(is.na(other$doi[k])==F){cat(', doi:', other$doi[k], sep='')}
      cat('  \n', sep='') # line break
    }
  }
} # end of ARC if

# split by author order - different ordering
if(params$extra.order == 'split'){
  
  # a) First author
  if(sum(papers$First.author) > 0){
    cat('## First author\n', sep='\n')
    this = dplyr::filter(papers, First.author==1)
    to.display = display.papers(this, in.style=params$style, star.to.use=star.to.use, flag.OA=params$flag.OA, bullets=use.bullets)
    cat(to.display)
    cat('\n\n')
    counter = nrow(this) # for numbers instead of bullets
  }
  
  # b) Last author
  if(sum(papers$Last.author) > 0){
    cat('## Last author\n', sep='\n')
    this = dplyr::filter(papers, Last.author==1)
    to.display = display.papers(this, in.style=params$style, star.to.use=star.to.use, flag.OA=params$flag.OA, bullets=use.bullets, counter.start=counter)
    cat(to.display)
    cat('\n\n')
    counter = counter + nrow(this) # for numbers instead of bullets
  }
  
  # c) Neither first nor last author
  this = dplyr::filter(papers, Last.author==0 & First.author==0)
  if(nrow(this) > 0){
    cat('## Neither first nor last author\n', sep='\n')
    to.display = display.papers(this, in.style=params$style, star.to.use=star.to.use, flag.OA=params$flag.OA, bullets=use.bullets, counter.start=counter)
    cat(to.display)
    cat('\n')
  }
  
} # end of `split` if
}
