print("Make sure you are useing R 4.1.3 or higher")
print("Your version")
print(version)
options(warn=-1)
print("Installing dependencies...")
dependencies <- c("readxl", "readr", "data.table", "plyr",
                  "dplyr", "ggplot2", "factoextra", "FactoMineR", 
                  "corrplot", "cluster", "eeptools", "gridExtra",
                  "tidyr", "ggrepel", "stringr", "writexl")

new.packages <- dependencies[!(dependencies %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.us.r-project.org")

dependencies2 <- c("readxl")
new.packages2 <- dependencies2[!(dependencies2 %in% installed.packages()[,"Package"])]
if(length(new.packages2)) withr::with_makevars(c(PKG_LIBS = "-liconv"), install.packages("readxl", repos = "http://cran.us.r-project.org"), assignment = "+=")


dependencies3 <- c("ggplot2")
new.packages3 <- dependencies3[!(dependencies3 %in% installed.packages()[,"Package"])]
if(length(new.packages3)) install.packages(c('mgcv', 'ggplot2'), repos = "http://cran.us.r-project.org")

# Loading
library("readxl")
library(readr)
library(data.table)
library(plyr)
library(dplyr)
library(ggplot2)
library("factoextra")
library(FactoMineR)
library("corrplot")
library(cluster)
library(eeptools)
library(gridExtra)
library(tidyr)
library(ggrepel)


# READING DATA

print("Reading data ...")

filenames <- list.files("input", pattern="*.xls|*.xlsx", full.names=TRUE)

if (length(filenames) == 0) {
  stop("No input files found.")
}
df <- read_excel(filenames[1])

# CREATE DIRECTORIES FOR THE RESULTS
print("Creating result directories ...")
dir.create('result')
dir.create('result/cluster')
dir.create('result/mca')

# SELECT REGION. IF WANT TO SELECT ALL REGIONS, COMMENT THE FOLLOWING LINES
REGION_NUMBER <- 42 # example: San Juan is region 42
df <- df[df$REGION == REGION_NUMBER, ]


print("preprocessing ...")

# SELECTING COLUMNS. YOU CAN CHANGE THE SELECTED
# COLUMNS. REMEMBER TO KEEP THE ORDER SO THE
# TRANSLATION IS DONE CORRECTLY. IF YOU DO NOT USE
# A COLUMN, MAKE SURE TO CHECK IF ANY PART OF THE CODE
# USES IT.
vars <- c(
    #CARACTERÍSTICAS GENERALES
    "CH03", "CH04", "CH05", "CH06", "CH07",
    #"CH9"
    #EDUCACIÓN
    #"CH11",
    #LUGAR DE NACIMIENTO/DONDE VIVIA
    #"CH15","CH16",
    #OTRAS (IMPORTANTE ES ESTADO)
    "NIVEL_ED","ESTADO",'CAT_OCUP','CAT_INAC','PP02H', #empleo estatal o privado
    'PP04A',
    'P21', #ingresos oc ppal
    'V2_M', #jubilacion pension
    'V5_M' #subsidios
)

df<-df[vars]


# SETTING SOME VARS AS FACTORS AND DATES
df[c("CH05","CH03","CH04","CH07")] <- lapply(df[c("CH05","CH03","CH04","CH07")], factor)
df[c("CH05")]<-lapply(df[c("CH05")], function(x)as.Date(x,format="%Y-%m-%d"))


# CHECKING THAT AGE AND BORN DATE ARE OK
# OBSERVATION_DATE MUST BE SET TO THE DATE THE
# POLL WAS DONE
OBSERVATION_DATE <- "2018-01-01"

df[ , c("EDAD", "error")] <- NA
obs.date <- as.Date(OBSERVATION_DATE, format="%Y-%m-%d")
age<-age_calc(dob = na.omit(df$CH05), enddate = obs.date, units = "years", precise = TRUE)
df$EDAD[!is.na(df$CH05)] <- sapply(age, function(x)trunc(x))
df$error <- df$EDAD - df$CH06
dferror <- df[c("CH05", "CH06", "EDAD", "error")]
dferror <- filter(dferror, !is.na(dferror$CH06) & df$error != 0)
df <- transform(df, error = ifelse(is.na(df$error), 0, df$error))
df <- transform(df, CH06 = ifelse(df$error != 0, EDAD, CH06))
df <- transform(df, CH06 = ifelse(df$CH06 < 0, 0, CH06))
df <- select(df, -c("EDAD", "error", "CH05"))


# MAKING AGE INTERVALS
# YOU CAN SET YOUR OWN INTERVALS
AGE_INTERVALS <- c(0, 16, 26, 50, 75, Inf)

df$CH06 <- cut(df$CH06, breaks=AGE_INTERVALS, right = FALSE)


# CHANGING NAMES AND VALUES FOR VARS RELATED TO
# INCOME MONEY USING PP04A,P21,V2_M,V5_M
df[, c("Ing_trabajo_estatal", "Ing_trabajo_privado",
       "Ing_jubilación", "Ing_subsidio")] <- NA

df <- transform(df, Ing_trabajo_estatal = ifelse(P21>0 & PP04A == '1' , 'Si', 'No'))
df <- transform(df, Ing_trabajo_privado = ifelse(P21>0 & PP04A == '2' , 'Si', 'No'))
df <- transform(df, Ing_jubilación = ifelse(V2_M>0, 'Si', 'No'))
df <- transform(df, Ing_subsidio = ifelse(V5_M>0, 'Si', 'No'))
df <- select(df, -c('PP04A','P21','V2_M','V5_M'))


# TRANSLATE COLUMN NAMES AND VALUEST TO SOMETHING MORE READABLE
# USING THE TRANSLATION FILES. YOU CAN CHANGE THE TRANSLATION
# IN THOSE FILES IF YOU NEED TO, BUT MAKE SURE TO CHECK IF
# ANY PART OF THE CODE USES THE TRANSLATED NAMES.

dfiv <- read_excel("index/INDICE_PREGUNTAS_USU_INDIVIDUAL.xlsx")
dfir <- read_excel("index/INDICE_RESPUESTAS_USU_INDIVIDUAL.xlsx")
dfa <- df

# changing category names
for (colname in colnames(dfa)){
    values <- unlist(c(dfir[dfir$COD == colname,"VALUE" ]),use.names = FALSE)
    responses <- unlist(c(dfir[dfir$COD == colname,"RESPONSE" ]),use.names = FALSE)
    dfa[colname] <- lapply(dfa[colname], function(x) mapvalues(unlist(x), from = values, to = responses))
}

# changing var names
oldnames <- colnames(select(df, -c("Ing_trabajo_estatal", "Ing_trabajo_privado",
       "Ing_jubilación", "Ing_subsidio")))
newnames <- unlist(c(dfiv[dfiv$COD %in% oldnames, "NAME"]), use.names = FALSE)
setnames(dfa, oldnames, newnames)  
                           
df <- dfa

# TRANSFORM CHAR VARS TO FACTORS
df[] <- lapply(df[], factor)

# CHECK NANS AND COLUMNS WITH ONLY 1 CATEGORY/FACTOR
print("Check columns with nans:")
print(colnames(df)[colSums(is.na(df)) > 0])
print("Check columns with only 1 category/factor:")
print(ifelse(n <- sapply(df, function(x) length(levels(x))) == 1,'WARNING: 1 LEVEL','OK'))
print("Check categories have more levels than used:")
ifelse(n <- sapply(df, function(x) length(levels(x)) != length(unique(x))),'WARNING','OK')

# REMOVE COLUMNS WITH ONLY 1 LEVEL/CATEGORY
print("Removing columns with less than 2 levels/categories")
cols_to_remove <- names(df)[sapply(df[,names(df)], function(x) length(levels(x)) < 2)]
df <- df[, !names(df) %in% cols_to_remove]


# CREATE A TABLE TO SHOW PREPROCESSING RESULT
sample <- df[0:30,]
png("result/sample.png", height = 25*nrow(sample), width = 120*ncol(sample), )
grid.table(sample)
dev.off()


# PLOT SUMMARY OF DATA AND SAVE IN FILE
ggplot(gather(df), aes(value))+
theme(axis.text.x = element_text(angle = 45,hjust=0.9,vjust=0.9,size=12),
     axis.title=element_text(size=14,face="bold"))+
geom_bar()+
facet_wrap(~key, scales = 'free_x')
ggsave("result/summary.png",width=10,height=10)


# PERFORM MCA ANALYSIS
# TAKE A LOOK AT
# http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/114-mca-multiple-correspondence-analysis-in-r-essentials/
# TO SEE A SIMPLE TUTORIAL

print("Performing MCA analysis...")
df.for.mca<-df
mca <- MCA(df.for.mca,graph = FALSE)


# SET NUMBER OF DIMENSION TO KEEP AS THE NUMBER
# OF DIIMENSIONS THAT ACCUMULATIVELY EXPLAIN MORE THAN 60%
# OF VARIANCE
ncp <- sum(mca$eig[,3] < 60, na.rm=TRUE) + 1
print("Keeping")
print(ncp)
print("dimensions...")
mca <- MCA(df.for.mca,graph = FALSE, ncp=ncp)


print("Creating plots...")

# PLOT INERTIA DISTRIBUTION IN DIMENSIONS (SCREEPLOT) AND SAVE IN FILE
fviz_screeplot(mca, addlabels = TRUE, ylim = c(0, 15))+theme(
    axis.text=element_text(size=12),
    axis.title=element_text(size=14,face="bold"),
    plot.background = element_rect(fill = "white")
)
ggsave("result/mca/screeplot.png",width=5,height=5)



# PLOT PROJECTED DATA IN FIRST 2 DIMENSIONS AND SAVE IN FILE
# USING Estado_Social AS THE LABELING VARIABLE
fviz_mca_biplot(mca,
                label=c("var"),
                repel = TRUE,
                axes = c(1,2),
                alpha.ind=0.01,
                habillage = 'EstadoSocial',
                palette = "Dark",
                addEllipses = TRUE,# ellipse.type = "confidence",
                ellipse.level=0.95,
                #xlim = c(-1.75, 2),
                #ylim = c(-1.5,2),
               ggtheme = theme_minimal())+theme(
                   axis.text=element_text(size=12),
                   plot.background = element_rect(fill = "white"),
                   axis.title=element_text(size=14,face="bold")
                )
ggsave("result/mca/biplot_Estado_Social.png", width=14, height=7)


# PLOT PROJECTED DATA IN FIRST 2 DIMENSIONS AND SAVE IN FILE
# USING RazondeInactividad AS THE LABELING VARIABLE
df.for.mca<-df
mca1 <- MCA(df.for.mca,graph = FALSE, ncp=ncp,
           quali.sup = c(grep("RazondeInactividad", colnames(df.for.mca)))
          )
fviz_mca_biplot(mca1,
                repel = TRUE,
                label=c("var"),
                axes = c(2,1),
                alpha.ind=0.01,
                habillage = 'RazondeInactividad',
                palette = "Dark",
                addEllipses = TRUE,# ellipse.type = "confidence",
                ellipse.level=0.95,
                #xlim = c(-1.75, 2),
                #ylim = c(-1.5,2),
               ggtheme = theme_minimal()
            )+theme(
                axis.text=element_text(size=12),
                axis.title=element_text(size=14,face="bold"),
                plot.background = element_rect(fill = "white")
                )
ggsave("result/mca/biplot_Razon_Inactividad.png",width=14,height=7)


# OUTPUT MCA ANALYSIS DATA TO A TXT FILE
cat("mca", file = "result/mca/mca.txt")
capture.output(summary(mca), file = "result/mca/mca.txt", append = TRUE)


# HIERARCHICAL CLUSTERING
# YOU CAN CHOOSE A DIFFERENT NUMBER OF CLUSTERS BASED ON THE DENDOGRAM
# CHECK
# http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/117-hcpc-hierarchical-clustering-on-principal-components-essentials/
# TO SEE A SIMPLE TUTORIAL
print("Performing hierarchical clustering...")
num.of.clust <- 0  # 0 lets you select a higth in the dendogram
hcpc <- HCPC(mca, nb.clust = num.of.clust, min = 3, max = 10, graph = TRUE)

num.of.clust <- length(unique(hcpc$data.clust$clust))


# PLOT A DENDOGRAM
print("Creating dendogram, this may take some time, please wait...")
fviz_dend(hcpc, 
          cex = 0.7, # Label size
          show_labels = TRUE,
          palette = "jco",
          rect = FALSE,
          #k=2,
          #rect_fill = TRUE, # Add rectangle around groups
          #rect_border = "aaas", # Rectangle color
          labels_track_height = 0# Augment the room for labels
          )+theme_classic()
ggsave("result/cluster/dendogram.png",width=5,height=7)


# OUTPUT HCPC ANALYSIS DATA TO A TXT FILE
cat("clusters", file = "result/cluster/clusters.txt")
capture.output(hcpc$desc.var, file = "result/cluster/clusters.txt", append = TRUE)



#SUMMARY OF CLUSTERS
library(tidyr)
library(stringr)

#Strip var name from categories in graphs
strip <- function(c){
    for(i in 1:length(c)) {
          c[i] <- sapply(c[i], function(x) tail(unlist(strsplit(x, "_", fixed = TRUE)), n=1))
    }
        return (c)
}


# PLOT COMPARATIVE SUMMARIES OF CLUSTERS
print("Creating comparative summaries...")
for (n in 1:num.of.clust){
    
    df.to.sum<-hcpc$data.clust[hcpc$data.clust$clust==n,]
    df.to.sum<-df.to.sum[,!names(df.to.sum) %in% c('clust')]
    
    if(nrow(df.to.sum)<10){
        print('Ignoring outliers')
    }
    else{
        print(nrow(df.to.sum))
        image.name=paste(paste("result/cluster/cluster",toString(round(nrow(df.to.sum)/nrow(df)*100, digits = 2))),"pc.png")
        ggplot(gather(df.to.sum), aes(value))+
        scale_x_discrete(label=function(x) strip(x))+
        theme(
            axis.text.x = element_text(angle = 45,hjust=0.9,vjust=0.9,size=12),
            axis.title=element_text(size=14,face="bold"),
            plot.background = element_rect(fill = "white")
        )+
        geom_bar()+
        #scale_x_discrete(labels=function(x){sub("_", "\n", x)})+
        #coord_flip()+
        facet_wrap(~key, scales = 'free_x')
        ggsave(image.name,width=7,height=10)
    } 
}


# PLOT CLUSTERED DATA IN FIRST 2 DIMENSIONS
print("Creating clustering biplot...")
df.for.mca <- cbind(df.for.mca, Cluster = hcpc$data.clust$clust)
mca1 <- MCA(df.for.mca,graph = FALSE, ncp=ncp,
            # do not include cluster label as active variable
           quali.sup = c(grep("Cluster", colnames(df.for.mca)))
          )
fviz_mca_biplot(mca1,
                repel = TRUE,
                label=c("var"),
                axes = c(1,2),
                alpha.ind=0.01,
                habillage = 'Cluster',
                palette = "Dark",
                addEllipses = TRUE,# ellipse.type = "confidence",
                ellipse.level=0.95,
                #xlim = c(-1.75, 2),
                #ylim = c(-1.5,2),
               ggtheme = theme_minimal()
            )+theme(
                   axis.text=element_text(size=12),
                    axis.title=element_text(size=14,face="bold"),
                    plot.background = element_rect(fill = "white")
                )
ggsave("result/cluster/biplot_clusters.png",width=14,height=7)


# OUTPUT PREPROCESSED DATA TO A CSV FILE
print("Writing preprocessed data to csv file...")
library(writexl)
write.csv(df, file = "result/preprocessed_data.csv",row.names=FALSE)

print("Done!")
print("Remember to remove the result folder and Rplot files before running the next time!")