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