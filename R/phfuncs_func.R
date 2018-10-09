#-----------------------------------------------------------------------
# phfuncs_func.R 
#
# PH, 10/9/18
#-----------------------------------------------------------------------

# libraries
#-----------------------------------------------------------------------
#libs <- c(
#  "tidyr",
#  "dplyr",
#  "broom",
#  "ggplot2",
#  "tidyverse",
#  "cowplot"
# )
#
#if (!require("pacman")) install.packages("pacman")
#library("pacman")
#pacman::p_load(char=libs)
#
# functions
#-----------------------------------------------------------------------

#' create_package 
#'
#' This function creates an R package from scratch.
#' @param project_name Name of the project.
#' @param folder Root folder to start the project in. Default: ~/projects
#' @keywords module R 
#' @export
#' @examples
#' create_package("myproject", "~/tmp")
create_package <- function(project_name, folder) {
  # credit:
  # hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/
  setwd(folder)
  devtools::create(project_name)
}

#' load_libraries
#'
#' This function loads all libraries for building an R package from
#' scratch.
#' @keywords module R
#' @export
#' @examples
#' load_libraries()
load_libraries <- function() {
  libs <- c("devtools", "roxygen2")
  if (!require("pacman")) install.packages("pacman")
  library("pacman")
  devtools::install_github("klutometis/roxygen")
  pacman::p_load(char=libs)
}

#' create_project
#'
#' This function sets up the typical file structure for projects
#' @param name The name of the project
#' @param title The title of the project
#' @param author The author of the project
#' @param email The email of the project's author
#' @param inst The institution of the project's author
#' @param root The root folder for the project
#' @keywords module R reproducible research
#' @export
#' @examples create_project("myproject", "My Author", "myemail@me.com",
#'     "My Institution", "rootdirectory")
create_project <- function(name, title, author, email, inst, root) {
  subdirs <- c(
    "src",
    "lib",
    "doc",
    "output/figures",
    "output/R",
    "output/tmp",
    "pub",
    "ext"
  )
  files <- c(
    "README.org",
    "LICENCE",
    "Makefile",
    "ms.org"
  )
  targets <- c(
    paste(name, "README.org", sep="/"),
    paste(name, "LICENSE", sep="/"),
    paste(name, "Makefile", sep="/"),
    paste(name, "/src/", name, "_ms.org", sep="")
  )
  
  #dir.create(project_name)
  for (i in 1:length(subdirs)) {
    fn <- paste(name, "/", subdirs[i], sep="")
    print(fn)
    dir.create(fn, recursive=TRUE)
    system(paste("touch ", fn, "/NULL", sep=""))
  }
  for (i in 1:length(files)) { 
    #from <- paste(project_name, "/templates/", files[i], sep=""))
    from <- system.file("templates", files[i], package="phfuncs")
    to <- targets[i]
    file.copy(from, to)

    # parse the file
    # replace @@name@@ by project name
    system(paste("sed -e 's/@@name@@/", name, "/g'", files[i],
                 "> tmp.txt", sep="")) 

    # replace @@title@@ by project title 
    system(paste("sed -e 's/@@title@@/", title, "/g'", "tmp.txt >",
                 files[i], sep="")) 
  
    # replace @@author@@ by project author 
    system(paste("sed -e 's/@@author@@/", author, "/g'", files[i],
                 "> tmp.txt", sep="")) 

    # replace @@email@@ by project author's email 
    system(paste("sed -e 's/@@email@@/", email, "/g'", "tmp.txt >",
                 files[i], sep="")) 

    # replace @@root@@ by project root 
    system(paste("sed -e 's/@@root@@/", root, "/g'", files[i],
                 "> tmp.txt", sep="")) 

    # replace @@inst@@ by project author's institution 
    system(paste("sed -e 's/@@inst@@/", inst, "/g'",
                 "tmp.txt >", files[i], sep="")) 
    
  }
}


#' starsfromp
#'
#' This function returns asterisks for your P-value.
#' @param pval p-value of interest.
#' @param c1 character for P < 0.1.
#' @param c2 character(s) for P < 0.05.
#' @keywords P-value 
#' @export
#' @examples
#' starsfromp(0.02, "")
starsfromp <- function(pval, c1="~", c2="*") {
  as <- vector(mode="character", length=length(pval))
  for (i in 1:length(pval)) {
    p <- pval[i]
    if (p < 0.1) as[i] <- c1 
    if (p < 0.05) as[i] <- paste(c2, sep="")
    if (p < 0.01) as[i] <- paste(c2, c2, sep="")
    if (p < 0.001) as[i] <- paste(c2, c2, c2, sep="")
  }
  return(as)
}


#' dic 
#'
#' This function calculates the Deviance Information Criterion (DIC).
#' @param stanfit Stan object.
#' @keywords stan 
#' @export
#' @examples
#' dic(stanfit)
dic <- function(stanfit) {
  p <- rstan::extract(stanfit, permuted=T)
  return(mean(p$dev) + 0.5 * (sd(p$dev))^2)
}

#' parse_vals 
#'
#' This function parses and formats a P-value.
#' @param action action string. Defaults to pval
#' @param param P-value
#' @keywords P-value
#' @export
#' @examples
#' parse_vals("pval", 0.0001)
parse_vals <- function(action="pval", param) {
switch(action,
	pval={
		if (param < 0.001) {
			 return("< 0.001")
		} else {
			 return(paste("=", round(param, 3)))
		}
	})
}

#' parse_msd 
#'
#' This function parses and formats a mean and SD.
#' @param m Mean 
#' @param sd Standard deviation 
#' @keywords format
#' @export
#' @examples
#' parse_msd(0, 1)
parse_msd <- function(m, sd) {
	print(paste("M = ", round(m, 2), ", SD = ", round(sd, 2),  sep=""))
}

#' parse_tstat 
#'
#' This function parses and formats a t statistic.
#' @param tstat t-test object 
#' @keywords stats, t-test
#' @export
#' @examples
#' tstat <- t.test(rnorm(100, 0, 1))
#' parse_tstat(tstat)
parse_tstat <- function(tstat) {
	print(paste("/t/ (", round(tstat$parameter,2), ") = ",
				round(tstat$statistic, 2), ", /P/ ", parse_vals("pval", 
				tstat$p.value), sep=""))
}

#' parse_rstat 
#'
#' This function parses and formats an r statistic.
#' @param rstat cor.test object 
#' @param method Type of correlation. Defaults to Pearson. 
#' @keywords stats, correlation
#' @export
#' @examples
#' rstat <- cor.test(rnorm(100, 0, 1), rnorm(100, 0, 1))
#' parse_rstat(rstat)
parse_rstat <- function(rstat, method="pearson") {
  switch(method,
         pearson = {
           print(paste("/r/ (", round(rstat$parameter, 2), ") = ",
                       round(rstat$estimate, 2), ", /P/ ",
                       parse_vals("pval", rstat$p.value), sep=""))
         },
         spearman = {
           print(paste("\rho = ",
                       round(rstat$estimate, 2), ", /P/ ",
                       parse_vals("pval", rstat$p.value), sep=""))
         })
}

#' parse_lm 
#'
#' This function parses and formats an linear model object.
#' @param lmfit lm object 
#' @param index Line index in data frame 
#' @param scaled Was the predicor scaled. Defaults to FALSE. 
#' @param style Output format. Defaults to beta. 
#' @keywords stats, linear model
#' @export
#' @examples
#' lmfit <- lm(rnorm(100, 0, 1) ~ rnorm(100, 0, 1))
#' parse_lm(lmfit, 1, FALSE, "beta")
parse_lm <- function(lmfit, index=2, scaled=FALSE, style="beta") {
	lsm <- as.data.frame(coef(summary(lm.beta(lmfit))))[index, ]
	if (scaled==FALSE) {
	  beta <- lm.beta::lm.beta(lmfit)$standardized.coefficients[index]
  } 
  else {
	  beta <- lsm$Estimate
  }

	def <- summary(lmfit)$df[2]
  switch(style,
         beta = {print(paste("\\beta = ",
                             round(beta, 2),
                             ", /t/ (", round(def, 2), ") = ",
                             round(lsm$"t value", 2), ", /P/ ",
                             parse_vals("pval", lsm$"Pr(>|t|)"),
                             sep=""))
         },
         ci = {print(paste("b = ",
                           round(beta, 2),
                           ", 95% CI [",
                           round(confint(lmfit, index)[1]),
                           "; ",
                           round(confint(lmfit, index)[2]),
                           "], ",
                           ", /t/ (", round(def, 2), ") = ",
                           round(lsm$"t value", 2), ", /P/ ",
                           parse_vals("pval", lsm$"Pr(>|t|)"),
                           sep=""))
         })
}

## parse_fstat <- function(anovatab, index) {
##   #
## 	# parse data frame coming from f test
##   #
##   #print(anovatab)
##   #print(index)
## 	a <- as.data.frame(anovatab)[index, ]
## 	print(paste("/F/ (", round(a$NumDF, 2), ", ", round(a$DenDF, 2), 
## 				") = ", round(a$"F value", 2), ", /P/ ", parse_vals("pval", 
## 				a$"Pr(>F)"), sep=""))
## }

## parse_chi <- function(chisq, def) {
##   #
## 	# fill in pieces of chi square test as text
##   #
## 	pval <- pchisq(chisq, def, lower=FALSE)
## 	print(paste("\\chi^{2} = ",
## 				round(chisq, 2), ", df=", def,  
## 				", /P/ ", parse_vals("pval", pval), sep=""))
## }

## parse_table <- function(table) {
##   #
## 	# remove any nil by replacing NA by whitespace
##   #
## 	table[is.na(table)] <- ""
## 	return(table)
## }


## parse_pval <- function(pval, rf=2, chs="steq") {
##   #
##   # Returns a nicely formatted pvalue string
##   #
##   if(pval < 0.001) {
##     fp <- "0.001"
##     char <- "< "
##   } else if(pval == 0) {
##     return(fpval(0.0001))
##   } else {
##     fp <- as.character(round(pval, rf))
##     if (chs == "steq") {
##       char <- "= "
##     } else {
##       char <- NULL
##     }
##   }
##   return(paste(char, fp, sep=""))
## }

  
## get_partcorr_vec <- function(lmfit, xc) {
##   #
##   # return partial correlations vectors
##   #
##   ff <- tempfile()
##   png(filename=ff)
##   a <- avPlots(lmfit)
##   dev.off()
##   unlink(ff)
##   x <- as.data.frame(a[xc])[, 1]
##   y <- as.data.frame(a[xc])[, 2]
##   return(data.frame(x=x, y=y))
## }

#df_sum_stats <- function(df) { 
# dfm <- df %>% gather(key=Characteristic, value=Value) %>%
#				group_by(Characteristic) %>%
#				dplyr::summarize(N=sum(!is.na(Value)),
#				                 Mean=round(mean(Value, na.rm=TRUE), 1),
#												 SD=round(sd(Value, na.rm=TRUE), 1),
#												 Min=round(min(Value, na.rm=TRUE), 1),
#												 Max=round(max(Value, na.rm=TRUE), 1)) 
#
#}
