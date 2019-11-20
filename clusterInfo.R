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
    summary_df <- data.frame(cores=c(cores_free, occupied_by_user_mat[1,3], cores_total),
                             Percent_CPU=c(CPU_free, occupied_by_user_mat[1,1], CPU_total),
                             Percent_Mem=c(mem_free, occupied_by_user_mat[1,2], mem_total),
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


