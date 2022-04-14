library(stringr)
options("scipen"=100)

# Run chunks of R code in arbitrary working directory
# From Hadley Wickham
# https://github.com/yihui/knitr/issues/38
in_dir <- function(dir, code) {
  cur <- getwd()
  setwd(dir)
  on.exit(setwd(cur))
  
  force(code)
}

Percentile <- function(x){
  cut(x,breaks=c(quantile(x,probs=c(0,0.01,0.05,0.1,0.2,0.4,0.6,0.8,0.9,0.95,0.99,1),na.rm=TRUE)),
      include.lowest=TRUE,
      labels=c("<1%","1-5%","5-10%","10-20%","20-40%","40-60%","60-80%","80-90%","90-95%","95-99%",">99%"))
  
}

scale_this <- function(x){
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}

pretty_dp <- function(x, dp=0, pct=FALSE, comma=FALSE){
  if(pct){x <- 100*x}
  if(comma){
    format(round(x, dp), digits=dp, nsmall=dp, big.mark=",") %>% trimws
  } else {
    format(round(x, dp), digits=dp, nsmall=dp) %>% trimws
  }
}

pretty_confint <- function(lci, uci, dp, pct=FALSE){
  paste0("(", pretty_dp(x=lci, dp=dp, pct=pct), ", ", pretty_dp(x=uci, dp=dp, pct=pct), ")")
}

pretty_pval <- function(p, cutoff=0.001, string="<0.001", dp=3){
  ifelse(p<cutoff, string, pretty_dp(p, dp))
}

lower <- function(x){
  paste0(tolower(substring(x, 1,1)), substring(x, 2))
}

upper <- function(x){
  paste0(toupper(substring(x, 1,1)), substring(x, 2))
}

prettyfunc <- function(x, pnames=list(), upper=FALSE, bold=FALSE, flist=c()){
  out <- x
  if(x %in% names(pnames)){
    out <- pnames[[x]]
    if(upper){
      out <- upper(out)
    }
    if(bold){
      out <- paste0("**", out, "**")
    }
  }
  if(x %in% flist){
    if(!exists("footnote_no")){
      footnote_no <<- 1
    }
    out <- paste0(out, "^", footnote_no, "^")
    footnote_no <<- footnote_no + 1
  }
  return(out)
}

diagcollist <- function(colstring, sep="", ncols) {
  x <- 0:ncols
  colstr <- paste0("`",paste0(colstring, sep, x, collapse="`, `"),"`")
  return(colstr)
}

table1_standardised <- function(data, varlist, adj, stratify, strata=NULL, 
                                pretty_names=list(), singlecol=TRUE, dp=1, show_crude=FALSE){
  #' Create age-standardised Table 1
  #'
  #' @param data The data
  #' @param varlist The list of variables to include in the table
  #' @param adj The variable to adjust by, as a factor. Eg age in single-year brackets.
  #' @param stratify The variable to stratify by, as a factor
  #' @param strata If you don't want to use all levels of the stratifying variable, specify desired levels here
  #' @param pretty_names List of human readable names corresponding to variable names
  #' @param singlecol Whether to stack variable names and levels into one column (TRUE - default) or spread across two columns (FALSE)
  #' @param dp Number of decimal places to display in the table (default 1)
  #' @param show_crude Include crude proportions in the form "crude (adjusted)". Advisable for sanity check but not for final presentation.
  #' 
  #' @return A dataframe formatted appropriately to output as Table 1
  #' @export
  #'
  #' @examples
  #' 
  if(is.null(strata)) { strata <- levels(data[[stratify]]) }
  
  table <- c()
  colnames <- c()
  
  for(s in strata) {
    colnames <- c(colnames, paste0(s, " (N=", nrow(data[data[[stratify]]==s,]), ")"))
    col <- c()
    
    for(var in varlist){
      if(is.factor(data[[var]])){
        if(singlecol){ col <- c(col, "") }
        for(l in levels(data[[var]])){
          
          count <- table(data[[adj]][data[[var]]==l & data[[stratify]]==s], useNA='ifany')
          pop <- table(data[[adj]][data[[stratify]]==s], useNA='ifany')
          stdpop <- table(data[[adj]], useNA='ifany')
          
          proportions <- ageadjust.direct(count=count, pop=pop, stdpop=stdpop)
          crude.prop <- pretty_dp(100*proportions[["crude.rate"]],dp)
          adj.prop <- pretty_dp(100*proportions[["adj.rate"]],dp)
          
          if(show_crude){
            col <- c(col, paste0(crude.prop, " (", adj.prop, ")"))
          } else {
            col <- c(col, adj.prop)
          }
        }
      } else {
        col <- c(col, paste0(pretty_dp(mean(data[[var]][data[[stratify]]==s]), dp), 
                             " (", pretty_dp(sd(data[[var]][data[[stratify]]==s]), dp), ")"))
      }
    }
    table <- cbind(table, col)
  }
  coefflist <- list()
  # Prepare the list of coefficients - variables and levels for factors or blanks for continuous
  for(var in varlist){
    coefflist[[var]] <- preparecoefflist(df=data, varname=var, pretty_names=pretty_names, onecol=singlecol)
  }
  coeffnames <- do.call(rbind, coefflist)
  
  colnames(table) <- colnames
  table <- cbind(coeffnames%>%select(-IDcol), table)
  rownames(table) <- NULL
  
  return(table)
}

# Normal Table 1
# Print numbers and proportions for factors, median and IQR or mean and 95% CI for continuous variables
# Optionally provide p-values from chi-squared (categorical) and t-test (continuous)
descriptivetable <- function(df, varlist, contavg='mean', assocvar=NULL, singlecol=FALSE, 
                             pretty_names=list(), footnote_list=c()){
  if(!exists("footnote_no")){footnote_no <<- 1} # Note use of <<- instead of <- to assign this globally
  outtable <- c()
  for(var in varlist){
    if(is.factor(df[[var]])){ # Categorical variables (factors) need a row per level, n's and %'s
      n <- table(df[[var]], useNA='ifany')
      pct <- pretty_dp(prop.table(n), dp=1, pct=TRUE)
      npct <- paste0(n, " (", pct, "%)")
      variable <- c(prettyfunc(var, pnames=pretty_names, upper=TRUE, flist=footnote_list))
      levels <- names(n)
      if(!is.null(assocvar)){
        tab <- table(df[[assocvar]], df[[var]])
        chi <- chisq.test(tab)
        pval <- c(ifelse(chi$p.value<0.001, "<0.001", round(chi$p.value,3)))
        outtable <- rbind(outtable, 
                          c(var, paste0("**", variable, "**"), "", "", "", pval), 
                          cbind(paste0(var, levels), levels, n, pct, npct, ""))
      } else{
        outtable<- rbind(outtable, 
                         c(var, paste0("**", variable, "**"), "", "", ""), 
                         cbind(paste0(var, levels), levels, n, pct, npct))
      }
    } else { # Continuous variables need the mean (and SD) or median (and IQR)
      if(contavg=="mean"){
        n <- pretty_dp(mean(df[[var]], na.rm=TRUE), dp=1, comma=TRUE)
        pct <- pretty_dp(sd(df[[var]], na.rm=TRUE), dp=1, comma=TRUE)
        npct <- paste0(n, " (", pct, ")")
        variable <- paste0("Mean ", prettyfunc(var, pretty_names, upper=FALSE, flist=footnote_list), " (SD)")
      } else if (contavg=="median"){
        n <- pretty_dp(median(df[[var]], na.rm=TRUE), dp=1, comma=TRUE)
        IQR <- pretty_dp(quantile(df[[var]], na.rm=TRUE), dp=1, comma=TRUE)
        pct <- paste0("(", IQR[2], "-", IQR[4], ")")
        npct <- paste0(n, " ", pct)
        variable <- paste0("Median ", prettyfunc(var, pnames=pretty_names, upper=FALSE, flist=footnote_list), " (IQR)")
      } else if(contavg=="n"){
        n <- nrow(df[!is.na(df[[var]]),])
        pct <- NA
        npct <- NA
        variable <- prettyfunc(var, pnames=pretty_names, upper=TRUE, flist=footnote_list)
      }
      if(!is.null(assocvar)){
        tt <- t.test(df[[var]][df[[assocvar]]==TRUE], df[[var]][df[[assocvar]]==FALSE])
        p <- pretty_pval(tt$p.value)
        outtable <- rbind(outtable, cbind(var, paste0("**", variable, "**"), n, pct, npct, p))
      } else {
        outtable<- rbind(outtable, cbind(var, paste0("**", variable, "**"), n, pct, npct))
      }
    }
  }
  rownames(outtable) <- c()

  outdf <- as.data.frame(outtable, stringsAsFactors=FALSE)
  if(singlecol){
    outdf <- outdf %>% select(-c(n, pct))
  } else {
    outdf <- outdf %>% select(-npct)
  }
  return(outdf)
}

printMIresults <- function(df, varlist, modeloutput, expon=TRUE, pretty_names=list(), onecol=FALSE, IDcol=FALSE, forplot=FALSE){
  require(dplyr)
  
  coefflist <- list()
  # Prepare the list of coefficients - variables and levels for factors or blanks for continuous
  for(var in varlist){
    coefflist[[var]] <- preparecoefflist(df=df, varname=var, pretty_names=pretty_names, onecol=onecol)
  }
  coeffnames <- do.call(rbind, coefflist)
  
  modeloutput <- modeloutput %>% 
    mutate(
      LCI = estimate - 1.96*std.error,
      UCI = estimate + 1.96*std.error
    ) %>% 
    mutate(., across(.cols=c(estimate, LCI, UCI), .fns=if(expon) exp else NULL))
  
  regression <- data.frame(
    IDcol=modeloutput$term,
    HR=pretty_dp(modeloutput$estimate,dp=2),
    HR_num=modeloutput$estimate,
    CI=pretty_confint(modeloutput$LCI, modeloutput$UCI, dp=2),
    L_CI=modeloutput$LCI,
    U_CI=modeloutput$UCI,
    p=pretty_pval(modeloutput$p.value),
    stringsAsFactors=FALSE
  )
  
  results <- left_join(coeffnames, regression, by="IDcol")
  if(onecol){
    results$HR[is.na(results$HR) & (results$IDcol != results$Coefficient & !is.na(results$Coefficient))] <- "1"
    results$HR_num[is.na(results$HR_num) & (results$IDcol != results$Coefficient & !is.na(results$Coefficient))] <- 1
  } else {
    results$HR[is.na(results$HR) & !is.na(results$Coefficient)] <- "1"
    results$HR_num[is.na(results$HR_num) & !is.na(results$Coefficient)] <- 1
  }
  
  coeffcols <- colnames(coeffnames)
  if(IDcol==FALSE){
    coeffcols <- coeffcols[coeffcols != "IDcol"]
  }
  
  if(forplot==FALSE){
    results <- results[,c(coeffcols, "HR", "CI", "p")]
    names(results) <- c(coeffcols, "HR", "95% CI", "p")
  } else {
    results <- results[,c(coeffcols, "HR_num", "L_CI", "U_CI","p")]
  }
  
  rownames(results) <- NULL
  # https://www.r-bloggers.com/regression-on-categorical-variables/
  return(results)
}


# Prettyprint the results from a Cox model
# To use this, 
# model <- coxph(Surv(time_to_dementia, dementia_status) ~ age, data=data)
# kable(printcoxresults(model), caption="")
printcoxresults <- function(df, varlist, modeloutput, pretty_names=list(), onecol=FALSE, IDcol=FALSE,forplot=FALSE){
  require(dplyr)
  
  coefflist <- list()
  # Prepare the list of coefficients - variables and levels for factors or blanks for continuous
  for(var in varlist){
    coefflist[[var]] <- preparecoefflist(df=df, varname=var, pretty_names=pretty_names, onecol=onecol)
  }
  coeffnames <- do.call(rbind, coefflist)
  
  summ <- summary(modeloutput)
  coeff <- summ$coefficients
  conf <- summ$conf.int
  
  regression <- data.frame(
      IDcol=rownames(coeff),
      HR=pretty_dp(coeff[,2], dp=2), # HR
      HR_num=coeff[,2],
      LCI=conf[,3],
      UCI=conf[,4],
      CI=pretty_confint(conf[,3], conf[,4], dp=2), # 95% CI
      p=pretty_pval(coeff[,5]), # p-value
      stringsAsFactors=FALSE
    )

  
  results <- left_join(coeffnames, regression, by="IDcol")
  if(onecol){
    results$HR[is.na(results$HR) & (results$IDcol != results$Coefficient & !is.na(results$Coefficient))] <- "1"
    results$HR_num[is.na(results$HR_num) & (results$IDcol != results$Coefficient & !is.na(results$Coefficient))] <- 1
  } else {
    results$HR[is.na(results$HR) & !is.na(results$Coefficient)] <- "1"
    results$HR_num[is.na(results$HR_num) & !is.na(results$Coefficient)] <- 1
  }
  
  coeffcols <- colnames(coeffnames)
  if(IDcol==FALSE){
    coeffcols <- coeffcols[coeffcols != "IDcol"]
  }
  
  if(forplot==FALSE){
    results <- results[,c(coeffcols, "HR", "CI", "p")]
    names(results) <- c(coeffcols, "HR", "95% CI", "p")
  } else {
    results <- results[,c(coeffcols, "HR_num", "LCI", "UCI")]
  }
  
    
  
  rownames(results) <- NULL
  # https://www.r-bloggers.com/regression-on-categorical-variables/
  return(results)
}

# Pretty print the results from a logistic regression model
printlogresults <- function(model, coeffnames=NULL, IDcol=FALSE){
  summ <- summary(model)
  coeff <- summ$coefficients
  # NOMVAR <- rownames(coeff)
  regression <- data.frame(
    IDcol=(rownames(coeff)),
    OR=pretty_dp(exp(coeff[,1]), dp=2), # OR
    CI=pretty_confint(exp(coeff[,1]-(1.96*coeff[,2])),
                      exp(coeff[,1]+(1.96*coeff[,2])),
                      dp=2), # 95% CI
    p=pretty_pval(coeff[,4]), # p-value
    stringsAsFactors=FALSE
  )
  if(is.null(coeffnames)){
    results <- regression
    names(results) <- c("Coefficient", "OR", "95% CI", "p")
  } else {
    results <- merge(coeffnames, regression, all.x=TRUE)
    results$OR[is.na(results$OR)] <- "1"
    results <- results[match(coeffnames$IDcol, results$IDcol),]
    
    coeffcols <- colnames(coeffnames)
    if(IDcol==FALSE){
      coeffcols <- coeffcols[coeffcols != "IDcol"]
    }
    results <- results[,c(coeffcols, "OR", "CI", "p")]
    names(results) <- c(coeffcols, "OR", "95% CI", "p")
  }
  rownames(results) <- NULL
  # https://www.r-bloggers.com/regression-on-categorical-variables/
  return(results)
}


# Round to the nearest m
mround <- function(x, base){
  base*round(x/base)
}


# Make a pretty proportion table
# To use this,
# tab <- table(data$VIhyp, data$prevHBP, useNA='ifany')
# kable(propped(tab), caption="")
propped <- function(table, margin=NULL) {
  prop <- round(100*prop.table(table, margin=margin),2)
  tabsums <- addmargins(table)
  dimt <- dim(table)
  for(i in c(1:dimt[1])) {
    if(length(dimt)>1){
      for(j in c(1:dimt[2])) {
        tabsums[i,j] <- paste0(table[i,j], " (", prop[i,j], "%)")
      }
    }
    else{
      tabsums[i] <- paste0(table[i], " (", prop[i], "%)") 
    }
  }
  return(tabsums)
}

# Check correlation among all pairs of covariates in a given dataframe 
# (this will need extending to be more robust if I want to use it for non-categorical covariates)
corr_mat <- function(data){
  corr_matrix <- matrix(0L, nrow=ncol(data), ncol=ncol(data))
  colnames(corr_matrix) <- colnames(data)
  rownames(corr_matrix) <- colnames(data)
  for(i in c(1:(ncol(data)-1))){
    x <- data[[i]]
    for(j in c((i+1):ncol(data))){
      y <- data[[j]]
      if(is.factor(x) & is.factor(y)){
        corr_matrix[i, j] <- (cramerV(table(x, y)))
      } else if (is.numeric(x) & is.numeric(y)) {
        corr_matrix[i, j] <- cor(x, y, method="pearson")
      } else {
        print("Unanticipated type")
      }
    }
  }
  return(corr_matrix)
}

preparecoefflist_1col <- function(df, varname, pretty_names=list()){
  # Detect whether has 2-way interaction first
  if(str_detect(varname,":")){
    
    var1=strsplit(varname,":")[[1]][1];var2=strsplit(varname,":")[[1]][2]
    
    pretty_varname1 <- prettyfunc(var1, pnames=pretty_names, bold=FALSE, upper=TRUE)
    pretty_varname2 <- prettyfunc(var2, pnames=pretty_names, bold=FALSE, upper=TRUE)
    
    int_name <-paste0("**",pretty_varname1,"*",pretty_varname2,"**")
    # detect variable type before and after :
    
    if(is.factor(df[[var1]])&is.factor(df[[var2]])){
      # Levels would be combinations of levels of each var (except ref level)
      
      ref_level<-paste(levels(df[[var1]])[1],levels(df[[var2]])[1])
      rest_level<-expand.grid(levels(df[[var1]])[-1],levels(df[[var2]])[-1])
      
      levels<-c(ref_level,paste(rest_level$Var1,rest_level$Var2))  
      variable<-c(int_name,levels)
      
      
      coeffname<-c(int_name,paste0(var1,c(levels(df[[var1]])[1],as.character(rest_level$Var1)),
                                   ":",var2,c(levels(df[[var2]])[1],as.character(rest_level$Var2))))
      
    }else if(is.numeric(df[[var1]])&is.numeric(df[[var2]])){
      variable<-int_name
      coeffname<-varname
      
    }else if(is.factor(df[[var1]])&is.numeric(df[[var2]])){
      levels<-levels(df[[var1]])
      variable <- c(int_name, levels)
      coeffname <- c(int_name, paste0(var1, levels,":",var2))
      
    }else if(is.numeric(df[[var1]])&is.factor(df[[var2]])){
      levels<-levels(df[[var2]])
      variable <- c(int_name, levels)
      coeffname <- c(int_name, paste0(var1,":",var2,levels))
      
    }else{warning("Please check type of variables in interaction")}
    
  }else{
    pretty_varname <- prettyfunc(varname, pnames=pretty_names, bold=TRUE, upper=TRUE)
    if(is.factor(df[[varname]])){
      if(is.ordered(df[[varname]])){
        poly <- length(levels(df[[varname]]))
        levels <- c("Ref", ".L", ".Q", ".C")
        if(poly > 4){
          powers <- c(4:(poly-1))
          levels <- c(levels, paste0("^", powers))
        }
        levels <- levels[1:poly]
      } else {
        levels <- levels(df[[varname]])
      }
      variable <- c(pretty_varname, levels)
      coeffname <- c(pretty_varname, paste0(varname, levels))
    } else {
      variable <- pretty_varname
      coeffname <- varname
    }
    
  }
  output <- data.frame(coeffname, variable, stringsAsFactors=FALSE)
  colnames(output) <- c("IDcol", "Coefficient")
  rownames(output) <- NULL
  
  return(output)
}

preparecoefflist_2col <- function(df, varname, pretty_names=list()){
  # Detect whether has 2-way interaction first
  if(str_detect(varname,":")){
    
    var1=strsplit(varname,":")[[1]][1];var2=strsplit(varname,":")[[1]][2]
    
    pretty_varname1 <- prettyfunc(var1, pnames=pretty_names, bold=FALSE, upper=TRUE)
    pretty_varname2 <- prettyfunc(var2, pnames=pretty_names, bold=FALSE, upper=TRUE)
    
    int_name <-paste0(pretty_varname1,"*",pretty_varname2)
    # detect variable type before and after :
    
    if(is.factor(df[[var1]])&is.factor(df[[var2]])){
      # Levels would be combinations of levels of each var (except ref level)
      
      ref_level<-paste(levels(df[[var1]])[1],levels(df[[var2]])[1])
      rest_level<-expand.grid(levels(df[[var1]])[-1],levels(df[[var2]])[-1])
      
      levels<-c(ref_level,paste(rest_level$Var1,rest_level$Var2))  
      variable<-c(int_name,rep(NA,length(levels)-1))
      
      
      coeffname<-paste0(var1,c(levels(df[[var1]])[1],as.character(rest_level$Var1)),
                        ":",var2,c(levels(df[[var2]])[1],as.character(rest_level$Var2)))
      
    }else if(is.numeric(df[[var1]])&is.numeric(df[[var2]])){
      levels<-NA
      variable<-int_name
      coeffname<-varname
      
    }else if(is.factor(df[[var1]])&is.numeric(df[[var2]])){
      levels<-levels(df[[var1]])
      variable <- c(int_name, rep(NA,length(levels)-1))
      coeffname <- paste0(var1, levels,":",var2)
      
    }else if(is.numeric(df[[var1]])&is.factor(df[[var2]])){
      levels<-levels(df[[var2]])
      variable <- c(int_name, rep(NA,length(levels)-1))
      coeffname <- paste0(var1,":",var2,levels)
      
    }else{warning("Please check type of variables in interaction")}
    
  }else{
    pretty_varname <- prettyfunc(varname, pnames=pretty_names, upper=TRUE)
    if(is.factor(df[[varname]])){
      if(is.ordered(df[[varname]])){
        poly <- length(levels(df[[varname]]))
        levels <- c("Ref", ".L", ".Q", ".C")
        if(poly > 4){
          powers <- c(4:(poly-1))
          levels <- c(levels, paste0("^", powers))
        }
        levels <- levels[1:poly]
      } else {
        levels <- levels(df[[varname]])
      }
      variable <- c(pretty_varname,
                    rep(NA, length(levels)-1))
      coeffname <- paste0(varname,levels)
    } else {
      levels <- NA
      variable <- pretty_varname
      coeffname <- varname
    }
  }
  
  output <- data.frame(coeffname, variable, levels, stringsAsFactors=FALSE)
  colnames(output) <- c("IDcol", "Coefficient", "Levels")
  rownames(output) <- NULL
  return(output)
}



preparecoefflist <- function(onecol=FALSE, ...){
  if(onecol) {
    preparecoefflist_1col(...)
  } else {
    preparecoefflist_2col(...)
  }
}

regressiontable <- function(df, outcome, varlist, regresstype, adjvarlist=c("agegrp", "gender"), pretty_names=list(), IDcol=TRUE){
  coefflist <- list()
  # Prepare the list of coefficients - variables and levels for factors or blanks for continuous
  for(var in varlist){
    coefflist[[var]] <- preparecoefflist(df=df, varname=var, pretty_names=pretty_names)
  }
  
  if(regresstype=="univariable"){
    modellist <- list()
    for(var in varlist){
      coeffnames <- coefflist[[var]]
      
      # Prepare the formula and pass it to the model
      formula <- paste0(outcome, " ~ ", var)
      model <- glm(formula, data=df, family="binomial")
      
      # Add the pretty-formatted outputs to the list
      modellist[[var]] <- printlogresults(model, coeffnames, IDcol=IDcol)
    }
    # Vertically concatenate all the pretty outputs into one output table
    outdf <- do.call(rbind, modellist)
    
  } else if (regresstype=="adjusted"){
    modellist <- list()
    
    # Run the regressions for age and gender separately to go on top of the table
    for(adjvar in adjvarlist){
      coeffnames <- preparecoefflist(df=df, varname=adjvar)
      
      formula <- paste0(outcome, " ~ ", paste(adjvarlist, collapse="+"))
      model <- glm(formula, data=df, family="binomial")
      
      # Add the pretty-formatted outputs to the list
      modellist[[adjvar]] <- printlogresults(model, coeffnames, IDcol=IDcol)
    }
    
    # Putting age or gender in the regression twice would confuse it, so make sure they're not in the varlist
    varlist <- varlist[!varlist %in% adjvarlist]
    
    for(var in varlist){
      coeffnames <- coefflist[[var]]
      
      # Prepare the formula and pass it to the model
      formula <- paste0(outcome, " ~ ", paste(adjvarlist, collapse="+"), "+", var)
      model <- glm(formula, data=df, family="binomial")
      
      # Add the pretty-formatted outputs to the list
      modellist[[var]] <- printlogresults(model, coeffnames, IDcol=IDcol)
    }
    outdf <- do.call(rbind, modellist)
    
  } else if (regresstype=="multivariable"){
    coeffnames <- do.call(rbind, coefflist)
    formula <- paste0(outcome, " ~ ", paste(varlist, collapse=" + "))
    model <- glm(formula, data=df, family="binomial")
    outdf <- printlogresults(model, coeffnames, IDcol=IDcol)
  }
  
  rownames(outdf) <- NULL
  return(outdf)
}

# Correlation matrix for mixed data type

# Calculate a pairwise association between all variables in a data-frame. In particular nominal vs nominal with Chi-square, numeric vs numeric with Pearson correlation, and nominal vs numeric with ANOVA.
# Adopted from https://stackoverflow.com/a/52557631/590437
mixed_assoc = function(df, cor_method="spearman", adjust_cramersv_bias=TRUE){
  df_comb = expand.grid(names(df), names(df),  stringsAsFactors = F) %>% set_names("X1", "X2")
  
  is_nominal = function(x) class(x) %in% c("factor", "character")
  # https://community.rstudio.com/t/why-is-purr-is-numeric-deprecated/3559
  # https://github.com/r-lib/rlang/issues/781
  is_numeric <- function(x) { is.integer(x) || is_double(x)}
  
  f = function(xName,yName) {
    x =  pull(df, xName)
    y =  pull(df, yName)
    
    result = if(is_nominal(x) && is_nominal(y)){
      # use bias corrected cramersV as described in https://rdrr.io/cran/rcompanion/man/cramerV.html
      cv = cramerV(as.character(x), as.character(y), bias.correct = adjust_cramersv_bias)
      data.frame(xName, yName, assoc=cv, type="cramersV")
      
    }else if(is_numeric(x) && is_numeric(y)){
      correlation = cor(x, y, method=cor_method, use="complete.obs")
      data.frame(xName, yName, assoc=correlation, type="correlation")
      
    }else if(is_numeric(x) && is_nominal(y)){
      # from https://stats.stackexchange.com/questions/119835/correlation-between-a-nominal-iv-and-a-continuous-dv-variable/124618#124618
      r_squared = summary(lm(x ~ y))$r.squared
      data.frame(xName, yName, assoc=sqrt(r_squared), type="anova")
      
    }else if(is_nominal(x) && is_numeric(y)){
      r_squared = summary(lm(y ~x))$r.squared
      data.frame(xName, yName, assoc=sqrt(r_squared), type="anova")
      
    }else {
      warning(paste("unmatched column type combination: ", class(x), class(y)))
    }
    
    # finally add complete obs number and ratio to table
    result %>% mutate(complete_obs_pairs=sum(!is.na(x) & !is.na(y)), complete_obs_ratio=complete_obs_pairs/length(x)) %>% rename(x=xName, y=yName)
  }
  
  # apply function to each variable combination
  map2_df(df_comb$X1, df_comb$X2, f)
}


