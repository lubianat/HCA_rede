library(data.table)
library(Seurat)

mesh <- as.data.frame(t(fread("../data/mesh_hca.txt")))
mesh <- mesh[-1,]
pbmc <- CreateSeuratObject(counts = mesh, project = "pbmc3k", min.cells = 1, min.features = 0)
pbmc

pbmc <- NormalizeData(pbmc)

pbmc <- FindVariableFeatures(pbmc, selection.method = "vst", nfeatures = 2000)

# Identify the 10 most highly variable genes
top10 <- head(VariableFeatures(pbmc), 10)

# plot variable features with and without labels
plot1 <- VariableFeaturePlot(pbmc)
plot2 <- LabelPoints(plot = plot1, points = top10, repel = TRUE)
CombinePlots(plots = list(plot1, plot2))

all.genes <- rownames(pbmc)
pbmc <- ScaleData(pbmc, features = all.genes)
pbmc <- RunPCA(pbmc, features = VariableFeatures(object = pbmc))
VizDimLoadings(pbmc, dims = 1:2, reduction = "pca")


DimPlot(pbmc, reduction = "pca")
DimHeatmap(pbmc, dims = 1:4, cells = 500, balanced = TRUE)


pbmc <- JackStraw(pbmc, num.replicate = 100)
pbmc <- ScoreJackStraw(pbmc, dims = 1:20)
JackStrawPlot(pbmc, dims = 1:15)


# True dimensionality ~ 7
ElbowPlot(pbmc)

pbmc <- FindNeighbors(pbmc, dims = 1:7)
pbmc <- FindClusters(pbmc, resolution = 1.2)

pbmc <- RunUMAP(pbmc, dims = 1:7)

DimPlot(pbmc, reduction = "umap",pt.size = 6)

library(dplyr)
pbmc.markers <- FindAllMarkers(pbmc, only.pos = TRUE, min.pct = 0.15, logfc.threshold = 0.25)
pbmc.markers %>% group_by(cluster) %>% top_n(n = 4, wt = avg_logFC)

VlnPlot(pbmc, features = c("hearing-loss"))
VlnPlot(pbmc, features = c("parasite"))
VlnPlot(pbmc, features = c("neuropathic-pain"))
VlnPlot(pbmc, features = c("adenovirus"))


FeaturePlot(pbmc, features = c("hearing-loss", "parasite", "neuropathic-pain", "adenovirus"))
