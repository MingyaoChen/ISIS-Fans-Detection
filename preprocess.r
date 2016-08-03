myClean <- function(line) {
  # Step 1: Remove the ENGLISH TRANSLATION:...
  line <- gsub("ENGLISH TRANSLATION:", "", line, fixed = TRUE)
  #line <- gsub("(f|ht)tp(s?)://(.*)[.][a-zA-Z/]+", "", line, fixed = FALSE)
  
  # Step 2: Split each line by "," and " "
  items <- unlist(strsplit(line, "[, ]+"))   #Split the line by ',' and ' '
  
  splitted <- list()
  
  # Step 3: Remove @ as well as http link
  for (i in 1:length(items)) {
    tmp <- items[i]
    if( startsWith(tmp, "@", ignore.case=TRUE, trim=TRUE)) next
    if( startsWith(tmp, "http", trim=TRUE, ignore.case = TRUE)) next
    
    splitted[length(splitted) + 1] <- tmp
  }
  filterred <- list() 
  if (length(splitted) == 0)
    return("")

  # Step 4: Remove non-english character
  for (i in 1:length(splitted)) {
    tmp <- trim(gsub("[^A-Za-z']+", " ", splitted[i], ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE))
    if (tmp == "") next
    filterred[length(filterred) + 1] <- tmp
  }
  
  if (length(filterred) < 3) 
  {  
    result = ""
    print(result)
    return(result)
  }
  
  
  # Step 5: Filter by Stopwords
  haveOrNot <- tolower(unlist(filterred)) %in% stopwords$stopword
  print(tolower(unlist(filterred)))
  print(haveOrNot)
  filterByDic <- list()
  for (i in 1:length(haveOrNot)) {
    if(!haveOrNot[i]) {
      filterByDic[length(filterByDic) + 1] <- filterred[[i]]
    }
  }
  
  # Step 6: If length of words small than 3 remove
  if (length(filterByDic) < 3) 
    result = ""
  else 
    result <-toString(filterByDic)
  print(result)
  return(result)
}

cleanDic <- function(line) {
  print(line)
  if (trim(line) == "") return("")
  if (!is.character(line)) return("")
  items <- unlist(strsplit(line, "[, ]+"))   #Split the line by ',' and ' '
  if (length(items) > 1) {
    print("Wrong dic element input, remove")
    return("")
  } else {
    result <- trim(gsub("[^A-Za-z]+", " ", line, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE))
    print(result)
    return(result)
  }
}