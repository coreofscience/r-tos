# r-tos

This repository contains enough code to create a Tree of Science. You just need to create an ISI Web of Knowledge seed, create a main.R file and run by using R. If you use RStudio Cloud, please follow this video.

## Steps

1. Create an ISI file. You could follow [this video](https://www.youtube.com/watch?v=EDjzs7xkzH4) for that.

2. Create a `main.R` file with the following code:
 
  ```r
  source('tos.R')
  filename <-  "savedrecs.txt"
  tos_tree <- tos(filename)
  write.csv(tos_tree, file = "output.csv")
  ```
  
  where **savedrecs.txt** is ISI filename. You could change if you have several ISI files.
    
3. Run the `main.R` file. If you use Rscript, run this command in the a terminal inside that folder:

  ```bash
  Rscript main.R
  ```
  You should get something like this:
 
  ```
  Converting your isi collection into a bibliographic dataframe

  Articles extracted   15 
  Done!
  ```
