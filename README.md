# C-Turkey Calibration Tool

A fast, browser-based tool for calibrating a single radiocarbon date and downloading publication-ready figures (PNG at 300 dpi, vector PDF, editable SVG).

**Use it online:** <https://altinisik.ezgilab.com/cturkey-app/>

Everything runs in your browser; no data leave your computer. The page also works offline: download the `docs/` folder and open `index.html`.

## About this version

The original tool was an R Shiny app (`app/app.R`) deployed with Shinylive/WebAssembly, which required the browser to download the full R runtime and loaded slowly. The web version was rewritten in plain JavaScript with the assistance of Claude Opus 5.5 (Anthropic) and now opens in a second or two.

The calibration code is a direct port of `rcarbon::calibrate()` and `rcarbon::hpdi()` with their default settings (normalised densities, eps = 1e-5). It was validated against rcarbon on 400 randomly generated dates across all four curves; probability distributions, 95.4% ranges and mean ages were identical in every case. The figure layout reproduces the original R plotting function.

Available calibration curves: IntCal20, SHCal20, Marine20 (no local ΔR applied) and IntCal13.

## Running the original R Shiny app

The original Shiny app is kept in `app/app.R` for anyone who prefers to work in R or wants to adapt the code. To run it locally:

``` r
install.packages(c("shiny", "rcarbon", "scales", "stringi"))
shiny::runApp("app")
```

## Repository structure

- `docs/`: the web version served on GitHub Pages (`index.html`, calibration engine, plotting code, calibration curves).
- `app/`: the original R Shiny app.
- `docs_shiny/`: the previous Shinylive (WebAssembly) export of the Shiny app, kept for reference only. It is no longer deployed or maintained.

## How to cite

> Altınışık, N.E. (2026) “C-Turkey: A Comprehensive Radiocarbon Dataset from Türkiye,” Journal of Open Archaeology Data, 14. [doi:[10.5334/joad.213](doi:%5B10.5334/joad.213){.uri}](<https://doi.org/10.5334/joad.213>)

Please also cite the calibration method and curves:

- Crema, E.R. & Bevan, A. (2021) Inference from large sets of radiocarbon dates: software and methods. *Radiocarbon* 63(1): 23–39.
- Reimer et al. (2020) IntCal20; Hogg et al. (2020) SHCal20; Heaton et al. (2020) Marine20; Reimer et al. (2013) IntCal13. *Radiocarbon*.

## Licences

Code and data: CC0-1.0. The bundled figure font (`docs/fonts/`) is a subset of Liberation Sans, distributed under the SIL Open Font License 1.1 (see `docs/fonts/OFL.txt`). jsPDF and svg2pdf.js (`docs/vendor/`) are MIT-licensed.
