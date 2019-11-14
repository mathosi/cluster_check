# Cluster_check
Cluster_check is a collection of functions allowing any user of RStudio/RStudio server to probe the ongoing processes on a computer or a server.  
We have developed these functions with the intention to raise awareness about resources available for those using RStudio server, and about the importance of sharing them in a friendly way.  
We encourage all users who **often execute commands in parallel** on the server, to use these functions to have a look at **the current server usage**, and to **evaluate the time they expect to need**, before setting the number of cores/threads they want to use. **Only 32 cores are available on the server!**

**Authors: SIMON M.<sup>1</sup>, PAGEAUD Y.<sup>1</sup>**  
**Contributors: All contributors are welcome to join!**  
**1-** [**DKFZ - Division of Applied Bioinformatics, Germany.**](https://www.dkfz.de/en/applied-bioinformatics/index.php)

**R Compatibility: Version 3.6.0**  
**Last Update: 14/11/2019**  

## Content
This repository contains currently 2 scrips:  
* `checkCluster.R` which contains the function `checkCluster()`.  
* `ps_to_df.R` which contains the function `ps.to.df()`.  

## Prerequesites
If you plan to use the function `checkCluster()` you need to install the package **magrittr**.
If you plan to use the function `ps.to.df()` no additional package installation is needed.

## Documentation
### checkCluster()
⚠️ **Work in progress !**  
### ps.to.df()

**Description**:  
`ps.to.df()` returns the output from a **ps** command as a R data.frame. It works with different parameters to which you can supply different values in a similar way than when running a **ps** command in a classic Linux terminal.  
**Parameters**:  
* **_simple.selection_** - A character string specifying a ps option listed as a 'SIMPLE PROCESS SELECTION' option (Default: simple.selection = '-A').  
* **_bylist.selection_** -  A character string specifying a ps option listed as a 'PROCESS SELECTION BY LIST' option.  
* **_process.sort_** - A character string specifying one or multiple keywords to be used for sorting processes. If multiple keywords are specified, they should be comma separated. A keyword can be preceded by a '-' sign for decreasing order (Default: process.sort = '-%cpu').  
* **_top.rows_** - An integer to specify the number of top rows to keep in the final data.frame.  
* **_other_** - A character string to specify a ps option not related to the selection of processes (Value supported: other = 'L').  
**Value**:  
A R **data.frame** containing processes by row, and for all processes the percentage of CPU use, the percentage of Memory use, the PID and PPID, the user name, the command name, the date and time when the process has been created, the time since the process is running and status of the process.  

**Examples**:
To list all on going processes:  
```R
ps.to.df()
```
To list all on going "rsession" processes:  
```R
ps.to.df(bylist.selection = '-C')
```
To sort all on going processes by percentage of memory used in decreasing order:  
```R
ps.to.df(process.sort='-%mem')
```
To sort the top 10 on going processes by percentage of CPU usage in decreasing order:  
```R
ps.to.df(process.sort = '-%cpu', top.rows = 10)
ps.to.df(top.rows = 10) #Simplified - Processes are sorted by decreasing order of %CPU usage by default. 
```

**References**:
Additional information can be found in the original documentation of **ps**:  [**http://man7.org/linux/man-pages/man1/ps.1.html**](http://man7.org/linux/man-pages/man1/ps.1.html)
