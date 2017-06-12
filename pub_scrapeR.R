#set local directory, this will contain downloaded files. 
setwd("D:/courses/VA/assignment_1/")

#initiate params
vispubdata_root <- 'https://www.lri.fr/~isenberg/VA/vispubdata'
vispubdata_first <- 'https://www.lri.fr/~isenberg/VA/vispubdata/pub_output_0.html'
#directory to save and read downloaded files
sub_dir<-"downloaded_file"

# Assignment 1.1 - Write a function to fetch and save all
# of the vispubdata pages found here: 
fetch_vispubdata_pages <- function(initial_page=vispubdata_first, directory_to_save=sub_dir){
  #import needed packages
  require(rvest)
  require(RCurl)
  
  #create directory if it does not exist
  if (!file.exists(directory_to_save)){
    dir.create(file.path(directory_to_save))
  }
  
  #create queue and visited list
  #each retrieval step visited page will be added visited list and "next page" will be added into queue
  queue<-NULL
  visited<-NULL
  #initiate queue with first page
  queue<-c(queue,initial_page)
  #loop if there is a page to look for in queue
  while (length(queue)>0) {
    current<-queue[1]
    html<-NULL
    tryCatch({
      if (url.exists(current)) {
        download.file(current,paste0(directory_to_save,"/",basename(current)))
        html<-read_html(current)
      }
    }, warning = function(w){
      #print("a warning was issued when opening the website")  
      #print(w)
    }, error = function(e){
      print(e)
    })
    #find next page url
    visited<-c(visited, current)
    next_page<-initial_page #inititate next page with first page due to some errors.Following lines it won't added to queue again.
    #if possible, retrieve next page url
    if (!is.null(html)) next_page<-paste0(dirname(current),"/",html_attr(html_node(html, "a"),name = "href") )
    #next page is only added to queue if it is not already visited
    if (!next_page %in% visited) queue<-c(queue, next_page)
    #delete current from page queue
    queue<-queue[-1]
  }
  cat(paste(length(visited), "vispubdata pages have been attempted to fetch and save locally"))
  cat("\n")
  }

# Assignment 1.2 - Write a function that processes all
#  of the saved vispubdata files and produces a CSV
process_vispubdata_pages <- function(root=vispubdata_root ,directory=sub_dir,
                                     max_number_of_authors=20, max_number_of_references=35,max_number_of_keywords=15){
  require(rvest)
  #max_number_of_[sth] params are used for flattening multiple fields. 
  #Related fields are retrieved up to specified number in parameter.
  
  #get html content and return desired nodes or desired nodes content
  get_node<-function(html_file, css, asText=FALSE){
    nodes<-html_file %>% html_nodes(css)
    if (asText) {
      text<-nodes%>% html_text()
      #eliminate empty nodes
      if (length(text)==0) result<-NA else result<-text
    } else result<-nodes
    return(result)
  }
  #retrieve paper details of single paper html object
  paper_details<-function(paper, max_number_of_authors=10, max_number_of_references=30,max_number_of_keywords=15){
    column_names<-c("conference", "year","paper_title","doi", "link","page_start","page_end","number_of_pages",
             "papertype","abstract","number_of_words_abstract","length_of_abstract",
             "number_of_authors", paste0("author_",1:max_number_of_authors), paste0("affiliation_",1:max_number_of_authors),
             "number_of_references",paste0("reference_",1:max_number_of_references), "number_of_keywords", paste0("keyword_",1:max_number_of_keywords))
    
    conference<-get_node(paper, "div.conference", asText = TRUE)
    year<-as.integer(get_node(paper, "div.year", asText = TRUE))
    paper_title<-get_node(paper, "div.paper_title", asText = TRUE)
    doi<-get_node(paper, "div.DOI", asText = TRUE)
    link<-get_node(paper, "div.Link", asText = TRUE)
    pages<-html_attr(get_node(paper, "span.pagenumbers"), "pp",default = "")
    if (pages!="") {
      page_start<-min(as.integer(unlist(strsplit(pages,split = ", ?"))[-(3:100)] ) )#take minimum of first two possible value 
      page_end<-max(as.integer(unlist(strsplit(pages,split = ", ?"))[-(3:100)] ) )#take maximum of first two possible value
      number_of_pages<-page_end-page_start+1
    }else{
      page_start<-NA
      page_end<-NA
      number_of_pages<-NA
    }
    papertype<-get_node(paper, "div.Papertype", asText = TRUE)
    abstract<-get_node(paper, "div.Abstract", asText = TRUE)
    number_of_words_abstract<-sapply(gregexpr("[[:alpha:]]+", abstract), function(x) sum(x > 0))
    length_of_abstract<-nchar(abstract)
    authors<-get_node(paper, "div.Authors", asText = TRUE)
    
    #number of authors seperated by semicolon
    #number_of_authors<-length(gregexpr(';',authors)[[1]])+1
    authors_vector<-unlist(strsplit(authors,split = "; ?"))
    number_of_authors<-length(authors_vector)
    authors_seperated<-rep(NA, max_number_of_authors)
    authors_seperated[1:min(number_of_authors, max_number_of_authors)]<-authors_vector[1:min(number_of_authors, max_number_of_authors)]

    affiliations<-get_node(paper, "div.Affiliation", asText = TRUE)
    affiliations_vector<-unlist(strsplit(affiliations,split = "; ?"))
    affiliations_seperated<-rep(NA, max_number_of_authors)
    affiliations_seperated[1:min(number_of_authors, max_number_of_authors)]<-affiliations_vector[1:min(number_of_authors, max_number_of_authors)]
    
    references<-get_node(paper, "div.References", asText = TRUE)
    references_vector<-unlist(strsplit(references,split = "; ?"))
    #number of references seperated by semicolon
    number_of_references<-length(references_vector)
    references_seperated<-rep(NA, max_number_of_references)
    references_seperated[1:min(number_of_references,max_number_of_references)]<- references_vector[1:min(number_of_references,max_number_of_references)]
    
    keywords<-get_node(paper, "div.Keywords", asText = TRUE)
    keywords_vector<-unlist(strsplit(keywords,split = ", ?"))
    #number of keywords seperated by comma
    number_of_keywords<-length(keywords_vector)
    keywords_seperated<-rep(NA, max_number_of_keywords)
    keywords_seperated[1:min(number_of_keywords, max_number_of_keywords)]<-keywords_vector[1:min(number_of_keywords, max_number_of_keywords)]
    
    result_df<-data.frame(conference,year,paper_title,doi,link,page_start,page_end,number_of_pages,
                          papertype,abstract,number_of_words_abstract,length_of_abstract,
                          number_of_authors,lapply(1:length(authors_seperated), function(x) authors_seperated[x]),
                          lapply(1:length(affiliations_seperated), function(x) affiliations_seperated[x]),
                          number_of_references, lapply(1:length(references_seperated), function(x) references_seperated[x]),
                          number_of_keywords, lapply(1:length(keywords_seperated), function(x) keywords_seperated[x])
                          )
    colnames(result_df)<-column_names
    return(result_df)
  }
  #initiate result dataframe
  result<-NULL
  #loop to process all files in dir
  downloaded_files<-list.files(path = directory)
  for (file_name in downloaded_files) {
    file_path<-paste(directory, file_name,sep = "/")
    html_file<-read_html(file_path)
    #papers<-html_file %>% html_nodes("div.paper_row")
    papers<-get_node(html_file, "div.paper_row")
    #loop to process all papers in one file 
    for (paper in papers) {
      source_url<-paste(root, file_name, sep = "")
      retrieved_date<-as.character(file.mtime(file_path))
      details<-paper_details(paper,max_number_of_authors,max_number_of_references,max_number_of_keywords)
      merged_details<-cbind(source_url, retrieved_date, details)
      result<-rbind(result, merged_details)
          }
    cat(paste(which(file_name==downloaded_files), "files out of", length(downloaded_files), "have been parsed."))
    cat("\n")
  }
  cat(paste(nrow(result), "paper in",length(downloaded_files),"files have benen extracted!"))
  write.csv(result,file = "vispubdata.csv", na = "",fileEncoding = "UTF-8")
  return(result)
}


fetch_vispubdata_pages()
p<-process_vispubdata_pages()
