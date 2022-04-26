## Older copy of this file is at http://biostat.mc.vanderbilt.edu/RConfiguration

## Set system option in anticipation of new R default (done March 2020)
options(stringsAsFactors = FALSE)

# .libPaths(c("/usr/lib/R/library", "/usr/lib/R/site-library", "/usr/local/lib/R/site-library"))
#.libPaths(c("/usr/local/lib/R/site-library", "/usr/lib/R/site-library",
#            "/usr/lib/R/library"))

## options(help_type='html', browser='...')
installPac <- function(p, rm=FALSE, ...)
  if(rm) remove.packages(p, lib='/usr/local/lib/R/site-library', ...) else
  install.packages(p,
                   repos='http://cran.rstudio.com',
                   lib='/usr/local/lib/R/site-library', ...)
updatePac <- function(checkBuilt=FALSE, ...)
  update.packages(repos='http://cran.rstudio.com',
                  instlib='/usr/local/lib/R/site-library',
                  checkBuilt=checkBuilt, ...)

## Packages to update that are not available as R debian packages
nondebPac <- c('brms','pcaPP','tables','blogdown')

newPac <- function(...)
  intersect(nondebPac,
            new.packages(repos='http://cran.rstudio.com',
                         lib.loc='/usr/local/lib/R/site-library'))

installGithubPac <- function(p, ...)
  devtools::install_github(p,
                           lib='/usr/local/lib/R/site-library',
                           build_vignettes=TRUE, ...)


spar <- function(mar=if(!axes)
                 c(2.25+bot-.45*multi,2*(las==1)+2+left,.5+top+.25*multi,
                   .5+rt) else
                 c(3.25+bot-.45*multi,2*(las==1)+3.5+left,.5+top+.25*multi,
                   .5+rt),
                 lwd = if(multi)1 else 1.75,
                 mgp = if(!axes) mgp=c(.75, .1, 0) else
                 if(multi) c(1.5, .365, 0) else c(2.4-.4, 0.475, 0),
                 tcl = if(multi)-0.25 else -0.4, xpd=FALSE, las=1,
                 bot=0, left=0, top=0, rt=0, ps=if(multi) 14 else 18,
                 mfrow=NULL, axes=TRUE, cex.lab=1.15, cex.axis=.8,
                 ...) {
  multi <- length(mfrow) > 0
  par(mar=mar, lwd=lwd, mgp=mgp, tcl=tcl, ps=ps, xpd=xpd,
      cex.lab=cex.lab, cex.axis=cex.axis, las=las, ...)
  if(multi) par(mfrow=mfrow)
}

# To fetch a file from the internet and put it in the RStudio script
# editor

# To fetch a file from the Vanderbilt Biostatistics R script repository
# Windows users must install wget.exe; instructions are at
# http://biostat.mc.vanderbilt.edu/RConfiguration
# Usage: getRs() to get the contents of the repository,
# getRs('filename.suffix') to get a single script
# getRs(cats=TRUE) to list major and minor categories of scripts
# getRs(cats='string') to list scripts in the first category that matches
#                      'string' ignoring case
# When file is not specified, to store result in a data frame that can
# be nicely viewed with RStudio, use e.g. scripts <- getRs()
# To store a list with categories do cats <- getRs(cats=TRUE)

if(FALSE) getRs <- function(file=NULL,
                  where='https://github.com/harrelfe/rscripts/raw/master',
                  browse=c('local', 'browser'), cats=FALSE) {
  browse <- match.arg(browse)
  extra <- '--no-check-certificate'

  trim <- function(x) sub('^[[:space:]]+','',sub('[[:space:]]+$','', x))

  pc <- function(s) {
    library(Hmisc)
    wr <- function(x) {
      n <- length(x)
      z <- character(n)
      for(i in 1 : n) z[i] <- paste(strwrap(x[i], width=15), collapse='\n')
      z
    }
    s <- with(s, cbind(Major = wr(Major),
                       Minor = wr(Minor),
                       File  = wr(File),
                       Type  = wr(Type),
                       Description = wr(Description)))
    print.char.matrix(s, col.names=TRUE)
  }

  if(! length(file)) {
    s <- read.table(paste(where, 'contents.md', sep='/'),
                    sep='|', quote='', header=TRUE, as.is=TRUE)
    s <- s[-1,]
    names(s) <- c('Major', 'Minor', 'File', 'Type', 'Description')
    sd <- s; n <- nrow(s)   # sd = s with dittoed items duplicated
    for(x in c('Major', 'Minor')) {
      u <- v <- gsub('\\*\\*', '', trim(s[[x]]))
      for(i in 2 : n) if(u[i] == '"') u[i] <- u[i - 1]
      v <- gsub('"', '', v)
      s[[x]] <- v; sd[[x]] <- u
    }
    s$File        <- trim(gsub('\\[(.*)\\].*', '\\1', s$File))
    d <- trim(gsub('\\[.*\\]\\(.*\\)', '', s$Description))
    s$Description <- gsub('\\[report\\].*', '', d)

    if(is.logical(cats)) {
      if(cats) {
        ## List all major and minor categories
        maj <- sort(unique(sd$Major))
        min <- setdiff(sort(unique(sd$Minor)), '')
        cat('\nMajor categories:\n', maj,
            '\nMinor categories:\n', min, '', sep='\n')
        return(invisible(list(Major=maj, Minor=min)))
      }
    } else {  ## list all scripts whose "first hit" major category contains cats
        i <- grepl(tolower(cats), tolower(sd$Major))
        if(! any(i)) cat('No scripts with', cats, 'in major category\n')
        else pc(s[i, ])
        return(invisible(s[i, ]))
      }
    if(browse == 'local') pc(s)
    else
      browseURL('https://github.com/harrelfe/rscripts/blob/master/contents.md')
    return(invisible(s))
  }
  
  download.file(paste(where, file, sep='/'), file, method='libcurl',
                extra=extra, quiet=TRUE)
  os <- Sys.info()['sysname']
  windowsRstudio <- function() {    # Written by Cole Beck
    RSTUDIO_BIN <- file.path('C:','Program Files','RStudio','bin','rstudio.exe')
    if(file.access(RSTUDIO_BIN, mode=1) == -1) {
      opts <- system("where /r c: rstudio.exe", TRUE)
      for(i in seq_along(opts)) {
        RSTUDIO_BIN <- opts[i]
        if(file.access(RSTUDIO_BIN, mode=1) == 0) return(RSTUDIO_BIN)
      }
      stop('rstudio cannot be found')
    }
    RSTUDIO_BIN
  }
  switch(os,
         Linux   = system2('rstudio', file),
         Windows = system2(windowsRstudio(), file),
         system(paste('open -a rstudio', file)) )
         ## assume everything else is Mac
  invisible()
}


## Use caption package options to control caption font size

## See https://github.com/yihui/knitr/releases :
## Clean up figure files generated before knitr 1.7
#' 
#' This function finds figure files that may be redundant, e.g., knitr <= 1.6 
#' generates foo.pdf for the chunk foo, and knitr >= 1.7 generates foo-1.pdf. If
#' both foo.pdf and foo-1.pdf exist, foo.pdf might be redundant.
#' @param dir the figure directory
#' @param clean whether to remove the redundant figure files; make sure you take
#'   a look at the list if files detected before you clean them up
clean_figures = function(dir = '.', clean = FALSE) {
  # figure files that do not have a numeric suffix
  old = list.files(dir, '[^0-9][.][a-z]{3,4}$', full.names = TRUE)
  if (length(old) == 0) return()
  new = gsub('(.)([.][a-z]{3,4})$', '\\1-1\\2', old)
  idx = file.exists(new)
  if (!any(idx)) return()
  if (clean) file.remove(old[idx]) else cat(old[idx], sep = '\n')
}

stanSet <- function(..., test=FALSE) {
	require(rstan)
	# rstan_options(auto_write=TRUE)
	options(mc.cores = parallel::detectCores(), stancompiled='~/R/stan')
	if(test) example("cxxfunction", package = "inline", run.dontrun = TRUE)
}


## For blogdown
bdSet <- function() {
  require(blogdown)
  options(blogdown.generator.server = TRUE,
          blogdown.hugo.server = c("-D", "-F", "--navigateToChanged"),
          blogdown.author='Frank Harrell')
  invisible()
}

bdNew <- function(title, shortname, kind='Rmd', ...)
  new_post(title=title, file=paste0("post/", shortname, ".", kind), ...)

# Rscript -e "blogdown::hugo_build()" will build the site including
# generating all .md from .Rmd files. Rscript -e
# "blogdown::serve_site()" will serve the site to a local browser.
#
# Put these in ~/.bashrc as hugoBuild and serveSite

## Create hyperlink to appropriate physical page in a pdf document
## created by pdflatex given the .aux and .pag file.  Absolute and
## named page numbers are store in the .pag file created by hslide.sty

latexRef <- function(label, base, name, path='doc/',
                     blogpath='/home/harrelfe/R/blog/blogdown/static/doc/',
                     lang=c('markdown', 'latex')) {
  lang <- match.arg(lang)
  aux <- paste0(blogpath, base, '.aux')
  ## cat(getwd(), ' ', aux, file='/tmp/zz')
  if(! file.exists(aux))
    stop(paste('no file named', aux))
  path <- paste0(path, base, '.pdf')
  pag  <- paste0(blogpath, base, '.pag')
  pagemap <- NULL
  if(file.exists(pag)) {
    p <- read.table(pag, sep=',')
    pagemap        <- trimws(p[[2]])
    names(pagemap) <- trimws(p[[1]])
  }

  r <- readLines(aux)
  w <- paste0('\\\\newlabel\\{', label, '\\}')
  i <- grepl(w, r)
  if(! any(i)) stop(paste('no label =', label))
  r <- r[i][1]
  r <- gsub('\\{', ',', r)
  r <- gsub('\\}', ',', r)
  x <- scan(text=r, sep=',', what=character(0), quiet=TRUE)
  section <- x[5]
  if(section != '') section <- paste0(' Section ', section)
  page    <- trimws(x[7])
  if(length(pagemap)) page <- pagemap[page]
  url     <- paste0('http://hbiostat.org/', path, '#page=', page)
  switch(lang,
         markdown = paste0('[', name, section, '](', url, ')'),
         latex    = paste0('\\href{', url, '}{', name, section, '}')
         )
  }

## Given a LaTeX .aux file and label, look up the section, figure number,
## or equation number that corresponds to that label, and compose a
## hyperlink
##
## Example: latexSec('sec:corr-n', 'bbr', 'BBR')
## Output: [BBR Section 8.5.2](http://hbiostat.org/doc/#nameddest=sec:corr-n)

latexSec <- function(label, base, name=toupper(base),
                     what='Section', path='doc/',
                     blogpath='/home/harrelfe/R/blog/blogdown/static/doc/',
                     lang=c('markdown', 'latex')) {

  lang <- match.arg(lang)
  path <- paste0(path, base, '.pdf')
  aux <- paste0(blogpath, base, '.aux')
  if(! file.exists(aux))
    stop(paste('no file named', aux))

  if(missing(what))
    what <- switch(substring(label, 1, 3),
                   sec   = 'Section',
                   cha   = 'Chapter',
                   fig   = 'Figure',
                   pag   = 'Page',
                   'pg:' = 'Page',
                   'Section')

  r <- readLines(aux)
  w <- paste0('\\\\newlabel\\{', label, '\\}')
  i <- grepl(w, r)
  if(! any(i)) stop(paste('no label =', label))
  r <- r[i][1]
  r <- gsub('\\{', ',', r)
  r <- gsub('\\}', ',', r)
  x <- scan(text=r, sep=',', what=character(0), quiet=TRUE)
  section <- x[5]
  if(section != '') section <- paste0(' ', what, ' ', section)
  url     <- paste0('http://hbiostat.org/', path, '#nameddest=', label)
  switch(lang,
         markdown = paste0('[', name, section, '](', url, ')'),
         latex    = paste0('\\href{', url, '}{', name, section, '}')
         )
  }

## https://github.com/eddelbuettel/littler for little scripts from Dirk E.
## including installing latest RStudio preview from the command line


## See https://stackoverflow.com/questions/58383358
bdftoc <- function()  cat('<style>
{{ if .Params.toc | default true}}
  <div class="fr-ns fr-nm" id="sidebar-wrapper">
    <div id="sidebar">      
      <ul class="nav nav-pills nav-stacked fr-ns fr-nm" style="list-style:none;">
         <!-- ignore empty links with + -->
        {{ $headers := findRE "<h[1-3].*?>(.|\n])+?</h[1-3]>" .Content }}
        <!-- at least one header to link to -->
        {{ $has_headers := ge (len $headers) 1 }}
        {{ if $has_headers }}
          {{ range $headers }}
            {{ $header := . }}
            {{ range first 1 (findRE "<h[1-3]" $header 1) }}
              {{ range findRE "[1-3]" . 1 }}
                {{ $head_level := (int .) }}
                {{ $base := ($.Page.File.LogicalName) }}
                {{ $anchorId := ($header | plainify | htmlEscape | urlize | safeURL) }}
                <li class="toc-h{{ $head_level }} no-underline"><a href="#{{ $anchorId }}">
                    {{ $header | plainify | htmlEscape }}
                </a></li>
              {{end}}
            {{end}}
          {{ end }}
        {{ end }}
      </ul>
    </div>
  </div>
      {{end}}

#sidebar-wrapper {
    max-width: 20px;
}

#sidebar {
    position: fixed; 
    top: 140px;
    /*background-color:gray;*/
}

@media (--breakpoint-not-small) {
  fr-ns {float: right; _display: inline; }
}

#sidebar > ul {
    margin: 0px 0px 0px 0px;
    padding: 0px 0px 0px 0px;
}

.nav-stacked>li>a {
    list-style-type:none;
    text-decoration:none;
    margin: 0px;
    color:black;
    text-align:left;
}



.toc-h1 {
    font-size: 100%;
    font-color:orange;
    text-decoration:none;
}

.toc-h2 {
  font-size: 90%;
  padding-left: 20px;
}

.toc-h3 {
  font-size: 90%;
  padding-left: 40px;
}

@media screen and (max-width: 600px) {
  #sidebar {
    display: none;
  }
}
</style>
')
