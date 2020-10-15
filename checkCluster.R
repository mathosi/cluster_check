#' @name checkCluster
#' @rdname checkCluster
#'
#' @title Summarize available and occupied CPUs and memory
#'
#' @description  
#' Prints a summary with 3 sections:
#' - Row 1: total number of CPUs and memory on the system
#' - Row 2: available number of CPUs and memory on the system
#' ----------------
#' - Row 3: CPUs and memory occupied by the user
#' ----------------
#' - Row 4...: Top ten consumers ordered by CPUs and memory occupied
#' @export
#' @examples
#' checkCluster()

checkCluster <- function(){
  # whoisit <- system(paste0("ps ", ps_flag, " -o %cpu,%mem,pid,cmd"), intern = TRUE)
  # #whoisit <- unname(sapply(whoisit, strsplit, ' '))
  # percCpu <- unname(sapply(whoisit[2:length(whoisit)], function(x) as.numeric(gsub("([0-9]+\\.[0-9]+).*$", "\\1", x))))
  # percMem <- unname(sapply(whoisit[2:length(whoisit)], function(x) as.numeric(gsub("^.*  ([0-9]+\\.[0-9]+).*$", "\\1", x))))
  # usr <- unname(sapply(whoisit[2:length(whoisit)], function(x) gsub('.* -u ([A-Za-z0-9]+) .*$', '\\1', x)))
  # whoisit_df <- data.frame(user=usr, Perc_CPU=percCpu, Perc_Mem = percMem)
  
  resource_df = ps.to.df()
  # whoisit_df %>%
  #   dplyr::group_by(user) %>%
  #   dplyr::arrange(desc(percCpu)) %>% print()
  coresAvail <- parallel::detectCores()
  #https://www.inmotionhosting.com/support/website/linux/linux-check-memory-usage/#:~:text=Linux%20memory%20info.-,Linux%20free%20%2Dm,as%20MB%20instead%20of%20KB.&text=The%20free%20column%20beside%20%2D%2F%2B,free%20memory%20available%20to%20Linux.
  #in Gb:
  totalMem = as.integer(gsub('^.* ([0-9]+) .*$', '\\1', system('grep "MemTotal" /proc/meminfo', intern = T)))*0.00000095367432 
  memAvailRaw = as.integer(gsub('^.* ([0-9]+) .*$', '\\1', system('grep "MemFree" /proc/meminfo', intern = T)))*0.00000095367432 

  total_df = data.frame(USER='total',
                        total_cpus=coresAvail, 
                        total_cpu_perc=coresAvail*100,
                        total_mem_gb = round(totalMem,3),
                        total_mem_perc=round(totalMem*100,1))

  proc_summary_df = resource_df %>%
    dplyr::group_by(USER) %>%
    dplyr::summarise(total_cpus=sum(perCPU > 85),
                     total_cpu_perc=sum(perCPU),
                     total_mem_gb=round(sum(perMEM)*0.01*totalMem,3),
                     total_mem_perc=round(sum(perMEM),1)) %>%
    dplyr::arrange(desc(total_cpus), desc(total_mem_gb))
  
  you_df = proc_summary_df %>% dplyr::filter(USER==Sys.info()[["user"]])

  free_df = data.frame(USER='free',
                       total_cpus=coresAvail-sum(proc_summary_df$total_cpus), 
                       total_cpu_perc=(coresAvail-sum(proc_summary_df$total_cpus))*100,
                       total_mem_gb = round(memAvailRaw,3),
                       total_mem_perc=round(memAvailRaw/totalMem*100,1)
  )
  
  connector <- data.frame(cbind('----', '----', '----', '----', '----'))
  names(connector) <-  names(proc_summary_df)
  summary_df <- rbind(total_df, free_df, connector, you_df, connector, head(proc_summary_df, 10))
  print(summary_df)
}


