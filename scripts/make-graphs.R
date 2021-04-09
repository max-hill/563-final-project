#!/usr/bin/env Rscript


# This file is the template for automated plot creation using ggplot2. 


packages <- c("ggplot2")


# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))





## Use these if you want to include arguments for the script
## args=commandArgs(trailingOnly=TRUE)
## print(args[1])


## Make the plot
file_list = list.files(path="../data",
                       pattern="*.csv", full.names=TRUE)

for (file in file_list){
    file_name = basename(file)
    file_name_sans_ext = tools::file_path_sans_ext(file_name)
    output_name=paste(file_name_sans_ext, ".jpeg", sep="")
    df=read.csv(file,header=TRUE)

    plot1 = ggplot(df, aes(x=ρ_a, y= P.bc - P.ab)) +
        geom_point() + 
        labs(x="recombination rate in population A",y="Pr[A(BC)]-Pr[(AB)C]")
    ggsave(output_name, plot1, path="../analysis/")
    print(paste("graph made for: ",file_name,sep=""))
    print(paste("output file path: ../analysis/",output_name,sep=""))
}


