#source functions with:
#devtools::source_url('https://raw.githubusercontent.com/mathosi/cluster_check/master/checkClusterComplete.R')

# ps.to.df #####################################################################

#' @description Returns output from ps command as a data.frame. 
#' 
#' @param simple.selection A \code{character} specifying a ps option listed as
#'                         a 'SIMPLE PROCESS SELECTION' option
#'                         (Default: simple.selection = '-A').
#' @param bylist.selection A \code{character} specifying a ps option listed as
#'                         a 'PROCESS SELECTION BY LIST' option.
#' @param process.sort     A \code{character} specifying one or multiple
#'                         keywords to be used for sorting processes. If
#'                         multiple keywords are specified, they should be comma
#'                         separated. A keyword can be preceded by a '-' sign
#'                         for decreasing order
#'                         (Default: process.sort = '-%cpu').
#' @param top.rows         An \code{integer} to specify the number of top rows
#'                         to keep in the final data.frame.
#' @param other            A \code{character} to specify a ps option not related
#'                         to the selection of processes (Support: other = 'L').
#' @value a \code{data.frame} containing processes by row, and for all processes
#'                            the percentage of CPU use, the percentage of
#'                            Memory use, the PID and PPID, the user name, the
#'                            command name, the date and time when the process
#'                            has been created, the time since the process is
#'                            running and status of the process.
#' @author Yoann Pageaud.
#' @export
#' @examples
#' @references http://man7.org/linux/man-pages/man1/ps.1.html

#TODO: Convert starts and elaps into date-time format.
#TODO: Hardcode column width following associated keyword.
#TODO: Use output format integers to automatically calculate column ranges.
#TODO: Add option to specify output format as a string.

ps.to.df<-function(simple.selection="-A", bylist.selection=NULL,
                   process.sort="-%cpu", top.rows=NULL, other=NULL){
  if(is.null(other)){ #If no 'other' argument specified, run default cmd
    if(is.null(bylist.selection)){
      #If no arg for 'bylist.selection', use 'simple.selection' arg
      base.cmd<-paste0(
        "ps ", simple.selection,
        " --no-headers -o %cpu:5,%mem:5,pid:7,ppid:7,user:36,comm:15,lstart:30,etime:30,stat:5 --sort=")  
    } else { #Use 'bylist.selection' arg
      base.cmd<-paste0(
        "ps ", bylist.selection,
        " --no-headers -o %cpu:5,%mem:5,pid:7,ppid:7,user:36,comm:15,lstart:30,etime:30,stat:5 --sort=")
    }
    if(is.null(top.rows)){ cmd<-paste0(base.cmd, process.sort) } else {
      cmd<-paste0(base.cmd, process.sort, " | head -n ", top.rows)
    }
    cmd.res<-system(command = cmd,intern = TRUE) #Get result from cmd
    df.res<-data.frame(
      perCPU = as.numeric(substr(x = cmd.res,start = 1,stop = 5)),
      perMEM = as.numeric(substr(x = cmd.res,start = 6,stop = 11)),
      PID = as.integer(substr(x = cmd.res, start = 12, stop = 19)),
      PPID = as.integer(substr(x = cmd.res, start = 20, stop = 27)),
      USER = gsub(pattern = "\\s+$", replacement = "",
                  x = substr(x = cmd.res,start = 29, stop = 65)),
      COMMAND = gsub(pattern = "\\s+$", replacement = "",
                     x = substr(x = cmd.res, start = 66, stop = 80)),
      STARTED = gsub(pattern = "^\\s+", replacement = "",
                     x = substr(x = cmd.res, start = 81, stop = 111)),
      ELAPSED = gsub(pattern = "^\\s+", replacement = "",
                     x = substr(x = cmd.res, start = 112, stop = 142)),
      STAT = substr(x = cmd.res, start = 144, stop = 149))
  } else { #If 'other' is specified, skip command to get info 
    if(other=="L"){
      cmd.res<-system(command = "ps L",intern = TRUE)
      df.res<-do.call(rbind,lapply(X = sapply(X = cmd.res, FUN = strsplit, " "),
                                   FUN = function(i){ i[grepl(".", i)] }))
      row.names(df.res)<-NULL
      colnames(df.res)<-c("CODE","HEADER")
    } else { stop("Unknown option.") }
  }
  return(df.res)
}

#' @name clusterInfo
#' @rdname clusterInfo
#'
#' @title find out how many cores and how much memory you use and how much is free 
#'
#' @description  find out how many cores and how much memory you use and how much is free 
#' @param relative  (default: TRUE)
#' @param by_process  (default: FALSE)
#' @export
#' @examples
#' clusterInfo()
#' clusterInfo(relative=FALSE, by_process=TRUE)
#' clusterInfo(bylist.selection='-C rsession')
#' 

clusterInfo <- function(relative=TRUE, by_process=FALSE, ...){
  #give possiblity to pass arguments to ps.to.df function (such as bylist.selection = '-C rsession')
  ps_df <- ps.to.df(...)
  #run without arguments to get information of overall load
  ps_total_df <- ps.to.df()
  #parallel::detectCores() function:
  parallel_detectCores <- function (all.tests = FALSE, logical = TRUE){
    systems <- list(linux = "grep ^processor /proc/cpuinfo 2>/dev/null | wc -l", 
                    darwin = if (logical) "/usr/sbin/sysctl -n hw.logicalcpu 2>/dev/null" else "/usr/sbin/sysctl -n hw.physicalcpu 2>/dev/null", 
                    solaris = if (logical) "/usr/sbin/psrinfo -v | grep 'Status of.*processor' | wc -l" else "/bin/kstat -p -m cpu_info | grep :core_id | cut -f2 | uniq | wc -l", 
                    freebsd = "/sbin/sysctl -n hw.ncpu 2>/dev/null", openbsd = "/sbin/sysctl -n hw.ncpu 2>/dev/null", 
                    irix = c("hinv | grep Processors | sed 's: .*::'", "hinv | grep '^Processor '| wc -l"))
    for (i in seq(systems)) if (all.tests || length(grep(paste0("^", 
                                                                names(systems)[i]), R.version$os))) 
      for (cmd in systems[i]) {
        if (is.null(a <- tryCatch(suppressWarnings(system(cmd, 
                                                          TRUE)), error = function(e) NULL))) 
          next
        a <- gsub("^ +", "", a[1])
        if (grepl("^[1-9]", a)) 
          return(as.integer(a))
      }
    NA_integer_
  }
  cores_total <- parallel_detectCores()
  CPU_total <- mem_total <- cores_total * 100
  CPU_occupied <- sum(ps_total_df$perCPU)
  mem_occupied <- sum(ps_total_df$perMEM)
  CPU_free <- CPU_total - CPU_occupied
  mem_free <- mem_total - mem_occupied
  cores_free <- round(CPU_free / 100)
  
  ps_user_df <- ps_df[ps_df$USER == Sys.getenv('USER'), ]
  occupied_by_user <- function(ps_df){
    CPU_occupied_by_user <- sum(ps_df$perCPU)
    mem_occupied_by_user <- sum(ps_df$perMEM)
    cores_occupied_by_user <- round(CPU_occupied_by_user / 100)
    return(cbind(CPU_occupied_by_user, mem_occupied_by_user, cores_occupied_by_user))
  }
  occupied_by_user_mat <- occupied_by_user(ps_user_df)
  
  if(relative){
    percent_cpu_free <- round(CPU_free / CPU_total * 100)
    percent_mem_free <- round(mem_free / mem_total * 100)
    cores_relative_free <- sprintf('%i/%i', cores_free, cores_total)
    percent_cpu_user <- round(occupied_by_user_mat[1,1] / CPU_total * 100)
    percent_mem_user <- round(occupied_by_user_mat[1,2] / mem_total * 100)
    cores_relative_user <- sprintf('%i/%i', occupied_by_user_mat[1,3], cores_total)
    
    summary_df <- data.frame(cores=c(cores_relative_free, cores_relative_user),
                             Percent_CPU=c(percent_cpu_free, percent_cpu_user),
                             Percent_Mem=c(percent_mem_free, percent_mem_user),
                             row.names=c('free', 'used_by_you'))
  }else{
    mem_cluster <- system('free -m', intern = T)
    mem_cluster_total <- gsub('^Mem:[ ]+([0-9]+) .*$', '\\1', mem_cluster[2])
    mem_cluster_free <- gsub('^Mem:.*[ ]+([0-9]+)$', '\\1', mem_cluster[2])
    mem_total_print <- sprintf('(%sMB) %i', mem_cluster_total, mem_total)
    mem_free_print <- sprintf('(%sMB) %i', mem_cluster_free, as.integer(mem_free) )
    summary_df <- data.frame(cores=c(cores_free, occupied_by_user_mat[1,3], cores_total),
                             Percent_CPU=c(CPU_free, occupied_by_user_mat[1,1], CPU_total),
                             Percent_Mem=c(mem_free_print, occupied_by_user_mat[1,2], mem_total_print),
                             row.names=c('free', 'used_by_you', 'total_avail'))
  }
  
  if(by_process){
    ps_user_df_split <- split(ps_user_df, f = list(ps_user_df$COMMAND)) 
    result <- lapply(ps_user_df_split, occupied_by_user)
    result_df <- as.data.frame(do.call(rbind, result))
    rownames(result_df) <- names(result)
    result_df <- result_df[order(result_df$CPU_occupied_by_user, decreasing = T), c(3,1,2)]
    result_df <- head(result_df, 10)
    if(relative){
      result_df <- result_df / c(1, CPU_total/100, mem_total/100) 
    }
    connector <- data.frame(cbind('----', '----', '----'), row.names = '----')
    names(result_df) <- names(connector) <-  names(summary_df)
    summary_df <- rbind(summary_df, connector, result_df)
  }
  return(summary_df)
}
