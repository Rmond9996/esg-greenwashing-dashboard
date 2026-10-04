# ESG Greenwashing Monitor

An interactive R Shiny dashboard for exploring ESG performance, emissions and potential greenwashing indicators across 30 companies in the Energy, Utilities and Industrials sectors from 2010–2024.

## Dashboard features

- Filters for year, sector, country, company and greenwashing status
- KPI cards for companies, revenue, Scope 1 + 2 emissions and flag rate
- Interactive emissions/ESG trends and sector comparisons
- ESG score versus carbon-intensity analysis
- Disclosure and third-party assurance indicators
- Company-level emissions explorer
- Searchable table and filtered CSV download

## Run locally

Install R 4.3 or later, then run:

```r
install.packages(c(
  "shiny", "bslib", "bsicons", "dplyr", "tidyr", "readr",
  "ggplot2", "plotly", "DT", "scales"
))

shiny::runApp()
```

## Publish on GitHub

Create an empty GitHub repository named `esg-greenwashing-dashboard`, then run from this folder:

```bash
git init
git add .
git commit -m "Build ESG greenwashing dashboard"
git branch -M main
git remote add origin https://github.com/YOUR-USERNAME/esg-greenwashing-dashboard.git
git push -u origin main
```

GitHub stores the source code; it does not run Shiny apps directly. For a live dashboard, deploy the same repository to [shinyapps.io](https://www.shinyapps.io/) or Posit Connect.

## Data

The dataset contains 450 company-year observations covering 30 companies, 3 sectors and 9 countries. The original variable names are retained in `data/esg_greenwashing.csv`.

The `greenwashing_flag` is treated as a screening indicator supplied by the dataset. It should not be interpreted as proof of misconduct. Missing year-on-year Scope 1 values occur for each company's first observation because no prior year is available.

## Project structure

```text
esg-greenwashing-dashboard/
├── app.R
├── DESCRIPTION
├── data/
│   └── esg_greenwashing.csv
├── www/
│   └── styles.css
├── .gitignore
├── LICENSE
└── README.md
```

## License

Code is released under the MIT License. Confirm that you have permission to redistribute the included dataset before making the repository public.
