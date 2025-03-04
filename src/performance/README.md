This directory contains a Haskell cabal project, comprising:
1. A literate Haskell file `app/PraosModel.lhs`;
2. A `Main.hs` entry point;
3. A subdirectory `app/Inserts` containing LaTeX bibliogrpahy files
A pdf document of the performance model can be generated using the following commands:
1. Execute the cabal project using `cabal run` from the `src/performance` directory (generates a number of pdf files in the `Inserts` subdirectory)
2. `cd app` 
3. `lhs2TeX PraosModel.lhs > PraosModel.tex`   
4. `pdflatex PraosModel.tex` 
5. `bibtex PraosModel`
6. `pdflatex PraosModel.tex`
78. `pdflatex PraosModel.tex`
(note: the repetition of `pdflatex` is needed for the contents, figures, tables and citations to render correctly)
This generates a file `PraosModel.pdf`