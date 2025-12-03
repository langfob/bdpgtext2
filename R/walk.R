#===============================================================================
#
#                               walk.R
#                               
#===============================================================================
#
#  History:  
#  
#  - 2024 05 09 - BTL
#    Cloned from:  
#    A Python-Like walk() Function for R
#    April 8, 2017 matloff
#    https://matloff.wordpress.com/2017/04/08/a-python-like-walk-function-for-r/
#   
#===============================================================================

walk <- function (top_dir,f,arg) 
    {
        #  Remember where you were when function was called, 
        #  so that you can return there when finished with the walk.
    savetop = getwd ()
    
        #  Go to top of dir tree to be walked and 
        #  get list of files in that top directory.
    setwd (top_dir)
    fls = list.files ()
    
        #  Apply the function of interest to each file in that directory.
    arg = f (top_dir, fls, arg)
    
        # Find all subdirectories of the current directory.
    dirs <- list.dirs (recursive=FALSE)
    
        #  Recurse into each of the subdirectories.
    for (cur_dir in dirs) 
        arg = walk (cur_dir, f, arg)
    
    setwd (savetop) # go back to calling directory
    
    return (arg)  
    }    

#===============================================================================

checkname <- function (drname, filelist, arg) 
    {
    cat ("\n")
    if (arg$flname %in% filelist) 
        {
        arg$tot <- arg$tot + 1
        cat ("IN LIST:  ", sep="")
    cat ("\narg$tot = ", arg$tot, ",  arg$flname = '", arg$flname, "'", sep="")
        } 
    
#    cat ("\narg$tot = ", arg$tot, ",  arg$flname = '", arg$flname, "'", sep="")
    
    return (arg)
    }

#===============================================================================

countinst <- function (startdir, flname) 
    {
    walk (startdir,
          checkname,
          list (flname = flname, tot = 0))
    }

#===============================================================================

start_dir = "/Users/bill/D/Projects/ProblemDifficulty/RnotInPkgs/bdpgtext/Data/Temp_expanded_Batch_3_500_FN"
start_dir = "/Users/bill/D/Projects/ProblemDifficulty/RnotInPkgs/bdpgtext/Data/Temp_expanded_Batch_3_500_FN/22349_default_scenario"
file_to_find = "prob_characteristics.csv"
print (countinst (start_dir, file_to_find))

if (FALSE)
    {
    walk ('mydir', 
          checkname, 
          list (flname='x',tot=0))
    }

#===============================================================================

