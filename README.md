# indec-mca
R scripts to preprocess and analyze indec USU "hogar" and "individuos" microdatabases.

## How to use:
1. **Install R 4.1.3 or later**
2. **Download a dataset** from https://www.indec.gob.ar/indec/web/Institucional-Indec-BasesDeDatos, it has to be an Encuesta Permanente de Hogares xls file.
3. **Put the downloaded data into the "input" folder** of the "individuos" or "hogar" folder, depending of the data you downloaded (Encuesta Individuos o Encuesta Hogares).
4. **Look for the path to the script**.
5. **Run the script**: navigate through the directories until you are in the same directory as `script.R` file, then run `R`, and then write in R console `source("script.R")`. You can also use RStudio or other tools to open the script and run it.
6. **Check for the "result" folder**. There you can find:
a. folder **"mca"** with Multiple Correspondance Analysis results and graphs.
b. folder **"cluster"** with Hierarchical Clustering results and graphs.
7. To use an script again, **remove or cut the result folder, the Rplot[number].pdf files and replace the input file** in the "input" folder.
8. The code is commented, you can change any part at your own discretion. It is advisable to **run it and check the graphs to see if some values** (such as number of clusters, or dimensions taken into account, etc) **need to be changed**. Then run again.
9. Variable renaming was done taking into account 2017 databases. You can change it editing the files in the "index" folder.


## Process explained
1. The script reads the data
2. It changes numerical columns to intervals
3. It changes column and category names to make them more readable
4. It performs MCA and calculates the best number of dimensions to keep.
5. It makes some graphs and writes MCA results.
6. It performs HCPC and lets you decide the cutting hight in a dendogram.
7. It makes some graphs and writes HCPC results.