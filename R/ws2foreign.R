ws2foreign=function(wsfile, foreignfile, MLwiNPath = "C:\\Program Files (x86)\\MLwiN v2.27\\", x64=FALSE){
    ## Convert MLwiN worksheet file to other data file which is used in Minitab, SAS, SPSS, or Stata
    temptfile =gsub("\\", "/", tempfile("coversion_",fileext=".txt"),fixed=TRUE)
    cat(file=temptfile)
    write("ECHO     0", temptfile, append=T)
    write(paste("LOAD   '",wsfile,"'",sep=""), temptfile, append=T)
    write(paste("STOR   '",foreignfile,"'",sep=""), temptfile, append=T)
    write("EXIT", temptfile, append=T)

    WD <- getwd()
    if (x64){
        MLwiNPath1=paste(MLwiNPath,'/x64/',sep='')
    }else{
        MLwiNPath1=paste(MLwiNPath,'/i386/',sep='')
    }
    if (file.access(MLwiNPath1)==0) setwd(MLwiNPath1) else setwd(MLwiNPath)
    if (file.access("mlnscript.exe", mode=1)==0){
        cmd=paste("mlnscript.exe /run \"", temptfile, "\"", sep="")
    }else{
        cmd=paste("mlwin.exe /nogui /run \"", temptfile, "\"", sep="")
    }
    shell(cmd)
    setwd(WD)
    file.remove(temptfile)
    cat("\n")
}
