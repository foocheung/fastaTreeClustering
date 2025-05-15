# fastaTreeClustering

https://github.com/user-attachments/assets/3b4399b2-4ea0-480d-be69-1af5ad9f6186

---



An interactive Shiny application for clustering and visualizing CDR3 amino acid sequences from FASTA files using edit-distance–based phylogenetic trees.

Built with [`{golem}`](https://engineering-shiny.org/golem.html) for modularity and production-ready deployment.

---

##  Features

*  Upload FASTA files containing CDR3 or other immune-related sequences
*  Compute Levenshtein (edit) distance between sequences
*  Build phylogenetic clustering trees using the neighbor-joining algorithm
*  Visualize trees with group-specific colors and shapes (e.g., BEAM, Dual, Shared)
*  Detect singletons and label shared/clustered clones
*  Export plots and cluster summary tables as `.pdf` and `.csv`

---

##  Usage

1. Launch the app locally:

```r
# Install required packages
remotes::install_github("foocheung/fastaTreeClustering")

# Run the app
fastaTreeClustering::run_app()
```

2. Upload a FASTA file and optional metadata CSV (for coloring/grouping)
3. View clustering tree plots and download cluster summaries

---

##  Input Format

* **FASTA file**: Amino acid sequences, ideally from immune receptor CDR3 regions

---

##  Output

* `tree_plot.pdf`: Phylogenetic tree colored and shaped by group
* `tree_labeled.pdf`: Tree with CDR3 labels
* `cluster_summary.csv`: Table of clusters, sizes, and group composition
* `singleton_summary.csv`: List of singleton sequences and their group origin

---

##  Tech Stack

* `{shiny}` + `{golem}`: Interactive interface
* `{Biostrings}`, `{ape}`, `{stringdist}`: Sequence handling and clustering
* `{ggplot2}`, `{dendextend}`: Visualization
* `{dynamicTreeCut}`: Tree-based clustering

---
