# Figure sources

Display items in `displays/` are produced by the pipeline from the MEPS public-use
files, with one exception: **eFigure 1**, the sample flow diagram, is drawn by hand.

- `sample_flow_diagram.drawio` — the editable source (diagrams.net / draw.io).
- `sample_flow_diagram.pdf` — the exported figure, copied to `displays/eFigure1.pdf` by
  the display-export step.

To change the diagram, edit the `.drawio` file, export to PDF over
`sample_flow_diagram.pdf`, and re-run the display-export step. Counts in the diagram
must match `data/clean/sample_flow_counts.csv`.
