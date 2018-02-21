
myknit <- function (inputFile, encoding) {
	# Acknowledgement: idea comes from
	# https://stackoverflow.com/questions/39885363/importing-common-yaml-in-rstudio-knitr-document
	
	library(fs)
	library(magrittr)
	library(stringr)
	
	# read in the src file
	rmd <- readLines(inputFile)
	# the line numbers of the start and end line for the yaml section
	yaml_ind <- str_which(rmd,'^---$')[1:2]
	# retrieve the yaml metadata block
	yaml_org <- rmd[do.call(seq.int, as.list(yaml_ind))]
	yaml <- yaml_org
	# remove yaml lines beginning with '#' or '  #'
	yaml      <- yaml[!str_detect(yaml,'^[ ]*#')]
	# first document type determines the output type
	doctype_line <- c(str_subset(yaml,'pdf_document|html_document'),'html_document')[1]
	doc_type     <- str_match(doctype_line,'(pdf|html)_document')[1,2]
	# specify the additional yaml options 
	hoqc_items <-
		c(
			'hoqc_output',
			'hoqc_yaml',
			'hoqc_yaml_new',
			'hoqc_force_ext',
			'hoqc_version' ,
			'hoqc_copy'
		)
	# read the values of the hoqc_write_yaml options
	values     <- list()
	for (hoqc_item in hoqc_items) {
		mks <- myknit_search(yaml, hoqc_item)
		yaml <- mks[['new_yaml']]
		values[[hoqc_item]] <- mks[['value']]
	}
	# insert params: hoqc_version
	if (!is.null(values[['hoqc_version']])) {
		hoqc_version = values[['hoqc_version']]
	}
	else {
		hoqc_version = ''
	}
	if (!is.null(values[['hoqc_force_ext']]))
		hoqc_force_ext <- values[['hoqc_force_ext']]
	else
		hoqc_force_ext <- F
	# if requested create file with copy of Rmd input
	if (!is.null(values[['hoqc_copy']])) {
		hoqc_copy <- values[['hoqc_copy']]
		hoqc_copy <-
			myknit_force_ext(hoqc_copy, 'Rmd', TRUE, hoqc_version)
		# write new input to the indicated file
		writeLines(rmd, hoqc_copy)
	}
	else 
		hoqc_copy <- ''
	# if requested create file with original yaml contents
	if (!is.null(values[['hoqc_yaml']])) {
		hoqc_yaml <- values[['hoqc_yaml']]
		hoqc_yaml <-
			myknit_force_ext(hoqc_yaml, 'txt', hoqc_force_ext, hoqc_version)
		# write new input to the indicated file
		writeLines(yaml_org, hoqc_yaml)
	}
	else 
		hoqc_yaml <- ''
	# if requested create file to contain new yaml contents
	if (!is.null(values[['hoqc_yaml_new']])) {
		hoqc_yaml_new <- values[['hoqc_yaml_new']]
		hoqc_yaml_new <-
			myknit_force_ext(hoqc_yaml_new, 'txt', hoqc_force_ext, hoqc_version)
		# because yaml will be extended with params do not yet write to the file 
		# writeLines(yaml, hoqc_yaml_new)
	}
	else 
		hoqc_yaml_new <- ''
	# determine output name for pdf or html file
	if (!is.null(values[['hoqc_output']])) {
		hoqc_output = values[['hoqc_output']]
		hoqc_output = myknit_force_ext(hoqc_output, doc_type, hoqc_force_ext, hoqc_version)
	}
	else {
		inputFileb <-strsplit(inputFile, '.', fixed = T)[[1]][1]
		hoqc_output = myknit_force_ext(inputFileb, doc_type, hoqc_force_ext, hoqc_version)
	}
	# create the additional params lines
	#   ensure that all options have the same length after padding
	hoqc_items <- str_pad(hoqc_items,max(sapply(hoqc_items,str_length)),side='right')
	#   format the param lines
	hoqc_parms <-
		paste(hoqc_items, paste0("'", lapply(hoqc_items, function(x)
			eval(parse(
				text = x
			))), "'"), sep = ' : ')
	# insert params
		# locate params line and lines not starting with space
		parmline <- str_which(yaml,'^params[ ]*:')
		nbline   <- str_which(yaml,'^[^ ]')
		if (length(parmline) == 0) {
			# when params line not found then add it with
			# hoqc_version line at the end of the yaml block
			yaml   <-
				append(yaml, c('params: ', paste0('  ', hoqc_parms)), after=tail(nbline,1)-1)
		}
		else {
			# when params line found then add hoqc_* lines at the end of the params block
			nbline <- head(nbline[nbline > parmline[1]], 1)
			lpline <- yaml[nbline - 1]
			# ensure that the hoqc_version line starts with the correct number of spaces
			lpline <-
				paste0(strrep(' ', as.numeric(regexec('[^ ]', lpline)) - 1), hoqc_parms)
			yaml   <- append(yaml, lpline, after = nbline - 1)
		}
		# write modified yaml to file if requested
		if (!is.null(values[['hoqc_yaml_new']])) {
			writeLines(yaml, hoqc_yaml_new)
		}
	# combine changed yaml with payload
	inputNew   <-
		append(yaml, rmd[(yaml_ind[2] + 1):length(rmd)])
	# write new input to a temp file
	tfile  <- fs::file_temp(pattern = 'tmpfile',
		tmp_dir = '.',
		ext = '.Rmd')
	writeLines(inputNew, tfile)
	# render adjusted file with rmarkdown.
	ofile <-
		rmarkdown::render(
			tfile,
			encoding = encoding,
			output_file = hoqc_output,
			envir = new.env()
		)
	# remove temporary file
	fs::file_delete(tfile)
}

myknit_force_ext <- function (filename, doc_type, tf, suffix) {
	# optionally give an extension or suffix to filename
	# ensure tf becomes a logical variable
	tf1       <- as.logical(tf)
	if (is.na(tf1))
		tf1 = switch(tolower(tf), yes = T, no = F, T)
	# do not consider path
	filename1 <- basename(filename)
	dirname1  <- dirname(filename)
	# split proper name and extension
	ibe <-  strsplit(filename1, '.', fixed = T)
	# unpack the list
	ibe <- ibe[[1]]
	# append suffix to proper name
	ibe[1] <- paste0(ibe[[1]][1], suffix)
	# if extension is required add the given one (will only be used when length(ibe) ==1)
	if (tf1 == TRUE)
		ibe <-  c(ibe, doc_type)
	# new filename
	if (length(ibe) < 2)
		newname <- ibe
	else
		newname <- paste(ibe[1:2], collapse = '.')
	# ensure that folders exist and retrieve the full name
	newname <- paste(dirname1, newname, sep = '/')
	newname %>% fs::path_dir() %>% fs::dir_create()
	newname %>% fs::file_create() %>% fs::path_real() %>% as.character() -> newname
	newname
}

myknit_search <- function(yaml, yaml_keyword) {
	value   <- NULL
	# prepare grep pattern
	g1      <- sprintf('%s[ ]*:[ ]*', yaml_keyword)
	# look for keyword
	ofound	<- str_which(yaml, g1)
	# if there is exactly one such line then retrieve its value
	if (length(ofound) == 1) {
		# the hoqc_output line
		keyline <- yaml[ofound]
		# the part after the colon
		after_colon <- str_match(keyline, paste0(g1, '(.*)'))[1, 2]
		# retrieve the value
		value <- myknit_search2(after_colon)
		# remove the line containing the keyword
		yaml <- yaml[-ofound]
	}
	# return the keyword value and the yaml without the keyword line
	list('new_yaml' = yaml, 'value' = value)
}

myknit_search2 <- function(after_colon) {
	# find option value
	if (str_detect(after_colon, "\'")) {
		value <- str_match(after_colon, "[\']([^\']*)[\']")
	}
	else if (str_detect(after_colon, '\"')) {
		value <- str_match(after_colon, '[\"]([^\"]*)[\"]')
	}
	else {
		value <- str_match(after_colon, '([^ #]*)')
	}
	value[1, 2]
}
