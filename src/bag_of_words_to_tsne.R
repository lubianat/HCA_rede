library(data.table)


bag <- fread("outputs/bag_of_words_3_abs.tsv")

bag2 <- bag
bag[is.na(bag)] <- 0


bag <- bag[, -1]
tbag <- t(bag)

colnames(tbag) <- bag2$researcher

library(Seurat)

hca <-
  CreateSeuratObject(
    counts = tbag,
    project = "HCA",
    min.cells = 2,
    min.features = 0
  )


hca <-
  NormalizeData(hca,
                normalization.method = "LogNormalize",
                scale.factor = 10000)
pbmc <-
  FindVariableFeatures(hca, selection.method = "vst", nfeatures = 1000)

top10 <- head(VariableFeatures(pbmc), 10)
plot1 <- VariableFeaturePlot(pbmc)

pdf(file = 'outputs/variable_words_plot.pdf')
print(LabelPoints(
  plot = plot1,
  points = top10,
  repel = TRUE
))
dev.off()

all.genes <- rownames(pbmc)
pbmc <- ScaleData(pbmc, features = all.genes)

pbmc <- RunPCA(pbmc, features = VariableFeatures(object = pbmc))
print(pbmc[["pca"]], dims = 1:5, nfeatures = 5)



pbmc <- JackStraw(pbmc, num.replicate = 100)
pbmc <- ScoreJackStraw(pbmc, dims = 1:20)
JackStrawPlot(pbmc, dims = 1:15)
ElbowPlot(pbmc)


pbmc <- FindNeighbors(pbmc, dims = 9)
pbmc <- FindClusters(pbmc, resolution = 1.0)

pbmc <- RunUMAP(pbmc, dims = 1:9)
pdf('outputs/seurat_cluster_plot.pdf', width = 3 , height = 3)
DimPlot(pbmc, reduction = "umap")
dev.off()

pbmc.markers <- FindAllMarkers(pbmc, only.pos = TRUE, min.pct = 0.25, logfc.threshold = 0.25)

Idents(pbmc)


cluster_researchers <- data.frame(researcher_id = names(Idents(pbmc)), cluster_id = Idents(pbmc))
write.table(cluster_researchers, "outputs/cluster_researchers_10_abs.tsv", sep = '\t', row.names = F)


data <- GetAssayData(pbmc, slot = "scale.data")

data <- t(data)
distance_data <- as.matrix(dist(data,method = 'manhattan'))

rownames(distance_data) <- rownames(data)

distance_data[distance_data<mean(distance_data)] <- 0

write.table(distance_data, "outputs/manhattan_distances_higher_than mean_10_abs.tsv", sep = '\t', row.names = F)
write.table(pbmc.markers, "outputs/word_markers_for_clusters_10_abs.tsv", sep = '\t', row.names = F)


