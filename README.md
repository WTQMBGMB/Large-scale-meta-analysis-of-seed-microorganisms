# Large-scale-meta-analysis-of-seed-microorganisms
# 种子微生物的大规模荟萃分析
### Data and code required for reproducing the research results
### 重现研究结果所需的数据和代码

# File description
# 文件描述
### 1.Analysis_pipeline.Rmd is the main code part for data analysis in this study. In theory, running it in sequence can reproduce most of the figures. 
### 2.scripts contains frequently called functions during the analysis. Please place it in the same folder as Analysis_pipeline.Rmd. 
### 3.The filtered_merge and filtered_region folder contains the data needed to reproduce this study. These data do not include the original sequencing data, but you can download them using the download numbers provided in the metadata. Please place it in the same folder as Analysis_pipeline.Rmd. 
### 4.The db folder contains some organized databases used to guide the analysis. Please place it in the same folder as Analysis_pipeline.Rmd. 
## note: It should be noted that the SILVA_138.2_DNA.fa file is too large to upload. Its content is the DNA format of the SILVA V138.2 database (with U bases replaced by T bases). This file is mainly used to provide full-length sequences for constructing the evolutionary tree (it does not affect the operation of other parts). You can choose to skip some code or contact the first author (wangzl2025@lzu.edu.cn) to obtain the file. 
### 5.The Map heatmap folder contains the Python code for drawing map heatmaps.

### 1.Analysis_pipeline.Rmd是本研究数据分析的主要代码部分，理论上按顺序运行可以复现绝大多数图片
### 2.scripts是分析中频繁调用的函数，请将其置于Analysis_pipeline.Rmd同级文件夹中
### 3.filtered_merge and filtered_region文件夹中是复现本研究需要的数据，这些数据不包含测序原始数据，但是可以使用metadata中提供的下载号自行下载，请将其置于Analysis_pipeline.Rmd同级文件夹中
### 4.db文件夹是一些整理好的数据库，用于指导分析，请将其置于Analysis_pipeline.Rmd同级文件夹中。
### 说明：需要说明的是SILVA_138.2_DNA.fa文件体积过大无法上传，内容为SILVA V138.2数据库的DNA格式（将U碱基替换为T碱基），这个文件主要用于提供构建进化树的全长序列（不影响其他部分运行），你可以选择跳过部分代码或者第一作者（wangzl2025@lzu.edu.cn）获取文件
### 5.Map heatmap文件夹中是绘制地图热图的Python代码

# To ensure that you can perfectly reproduce this study, please follow the steps below to use these files
## 1.Please make sure to extract all the folders.
## 2.Download the necessary R language packages
## 3.Make sure your r version is 4.5.2 or higher.
# 为确保您能够准确地重现本研究，请按照以下步骤使用这些文件
## 1. 请确保解压所有文件夹。
## 2. 下载所需的 R 语言包
## 3. 确保您的 R 版本为 4.5.2 或更高版本。
