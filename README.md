# Large-scale-meta-analysis-of-seed-microorganisms
# 种子微生物的大规模荟萃分析
### Data and code required for reproducing the research results
### 重现研究结果所需的数据和代码

# File description
# 文件描述
### 1.Analysis_pipeline.Rmd is the main code part for data analysis in this study. In theory, running it in sequence can reproduce most of the figures. 
### 2.scripts contains frequently called functions during the analysis. Please place it in the same folder as Analysis_pipeline.Rmd. 
### 3.Minimum_example.7z is the minimal running example for sequence preprocessing, which includes sequence download, quality control, amplicon bioinformatics process, and code for cross-region merging. Completing the entire process requires powerful server resources. If you need the original data and code, please contact the first author.
### 4.The filtered_merge and filtered_region folder contains the preprocessed data (quality control, primer removal, and host sequence elimination). Please place it in the same folder as Analysis_pipeline.Rmd. 
### 5.The db folder contains some organized databases used to guide the analysis. Please place it in the same folder as Analysis_pipeline.Rmd. 



### 1.Analysis_pipeline.Rmd是本研究数据分析的主要代码部分，理论上按顺序运行可以复现绝大多数图片
### 2.scripts是分析中频繁调用的函数，请将其置于Analysis_pipeline.Rmd同级文件夹中
### 3.Minimum_example.7z是序列预处理的最小运行例，其中包含了序列下载，质控，扩增子生物信息流程，以及跨区合并的代码。完整运行整个流程需要强大的服务器资源，如果需要原始数据和代码可以联系第一作者
### 4.filtered_merge and filtered_region文件夹中是经过预处理后的数据（质控，切除引物，去除宿主序列），这些数据不包含测序原始数据，但是可以使用metadata中提供的下载号自行下载，请将其置于Analysis_pipeline.Rmd同级文件夹中
### 5.db文件夹是一些整理好的数据库，用于指导分析，请将其置于Analysis_pipeline.Rmd同级文件夹中。



# To ensure that you can perfectly reproduce this study, please follow the steps below to use these files
## 1.Please make sure to extract all the folders.
## 2.Download the necessary R language packages
## 3.Make sure your r version is 4.5.2 or higher.
# 为确保您能够准确地重现本研究，请按照以下步骤使用这些文件
## 1. 请确保解压所有文件夹。
## 2. 下载所需的 R 语言包
## 3. 确保您的 R 版本为 4.5.2 或更高版本。
