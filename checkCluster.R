#' @name checkCluster
#' @rdname checkCluster
#'
#' @title find out how many cores and how much memory is free.
#'
#' @description  Blame the person who is making rstudio server slooow.
#' @param ps_flag flags that are passed to the ps command, (default: -C rsession)
#' @export
#' @examples
#' checkCluster()
#' checkCluster('')

checkCluster <- function(ps_flag='-C rsession'){
  whoisit <- system(paste0("ps ", ps_flag, " -o %cpu,%mem,pid,cmd"), intern = TRUE)
  #whoisit <- unname(sapply(whoisit, strsplit, ' '))
  percCpu <- unname(sapply(whoisit[2:length(whoisit)], function(x) as.numeric(gsub("([0-9]+\\.[0-9]+).*$", "\\1", x))))
  percMem <- unname(sapply(whoisit[2:length(whoisit)], function(x) as.numeric(gsub("^.*  ([0-9]+\\.[0-9]+).*$", "\\1", x))))
  usr <- gsub('-u ', '', unname(sapply(whoisit[2:length(whoisit)], function(x) stringr::str_extract(x, '-u (.+)$'))))
  whoisit_df <- data.frame(user=usr, Perc_CPU=percCpu, Perc_Mem = percMem)
  # whoisit_df %>%
  #   dplyr::group_by(user) %>%
  #   dplyr::arrange(desc(percCpu)) %>% print()
  coresAvail <- parallel::detectCores()

  suppressWarnings(whoisit_df %>%
    dplyr::group_by(user) %>%
    dplyr::summarise(total_cpus=sum(Perc_CPU > 85), total_cpu_perc=sum(Perc_CPU), total_mem_perc=sum(Perc_Mem)) %>%
    dplyr::arrange(desc(total_cpus), desc(total_cpu_perc)) %>%
    dplyr::bind_rows(data.frame(user='cores_available', total_cpus=coresAvail, total_cpu_perc=coresAvail*100), .) %>%
    print())

  suppressWarnings(whoisit_df %>% dplyr::summarise(total_cpus=sum(Perc_CPU)/100, total_cpu_perc=sum(Perc_CPU),
                                                   total_mem_perc=sum(Perc_Mem)) %>%
                     dplyr::mutate(system='used') %>%
                     dplyr::bind_rows(data.frame(system='total', total_cpus=coresAvail, total_cpu_perc=coresAvail*100), .) %>%
                     dplyr::bind_rows(data.frame(system='free', total_cpus=.[1,2]-.[2,2], total_cpu_perc=.[1,3]-.[2,3])) %>%
                     print())

}


