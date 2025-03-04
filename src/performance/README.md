This directory contains a Haskell cabal project, comprising:
1. A literate Haskell file `app/PraosModel.lhs`;
2. A `Main.hs` entry point;
3. A subdirectory `app/Inserts` containing LaTeX bibliogrpahy files
A pdf document of the performance model can be generated using the following commands:
1. Execute the cabal project using `cabal run` from the `src/performance` directory (generates a number of pdf files in the `Inserts` subdirectory)
2. `cd app` 
3. `lhs2TeX PraosModel.lhs > PraosModel.tex`   
4. `pdflatex PraosModel.tex` (note: this step may need to be repeated for the list of contents, figures and tables to render correctly)
This generates a file `PraosModel.pdf`