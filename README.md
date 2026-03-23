# Sampling Distribution Explorer

Synthetic social-science populations for learning; not real survey estimates.

This repository contains a standalone R Shiny teaching app that lets students compare the sampling behavior of the sample mean and sample median across five synthetic pseudo-populations. The app is designed to run locally, export with `shinylive`, and deploy to GitHub Pages.

## Requirements

Install a recent R release, then install the small package set used by the project:

```r
install.packages(c("shiny", "shinylive", "httpuv"))
```

The app deliberately keeps dependencies minimal for `webR` and `shinylive` compatibility.

## Run Locally

From the repository root:

```r
shiny::runApp("app")
```

Or from a shell:

```bash
Rscript -e 'shiny::runApp("app")'
```

## Export With Shinylive

From the repository root:

```bash
Rscript scripts/export_site.R
```

This exports the Shiny app in `app/` to a static site in `site/`.

## Preview The Exported Site

After export:

```bash
Rscript scripts/preview_site.R
```

This runs:

```r
httpuv::runStaticServer("site")
```

Open the local URL shown in the console to preview the GitHub Pages build.

## Smoke Test

Run the lightweight base-R smoke test from the repository root:

```bash
Rscript scripts/smoke_test.R
```

The smoke test sources `app/helpers.R`, generates each synthetic population, runs small simulations for both estimators, and checks the CI helper output structure.

## CI Methods Used In The App

- `Mean`: each of the last 20 samples gets a standard t-based 95% confidence interval using `mean +/- t * sd(x) / sqrt(n)`.
- `Median`: each of the last 20 samples gets a bootstrap percentile 95% confidence interval using 300 bootstrap resamples from that sample.

The binary variable allows the median for teaching purposes, but the interface notes that the mean is usually more informative because it is the sample proportion.

## Synthetic Data Note

All five populations are synthetic, illustrative pseudo-populations generated from fixed seeds. They are intended to behave like plausible social-science variables, but they are not real datasets and should not be interpreted as official empirical facts.

## GitHub Pages Deployment

1. Push this repository to GitHub with the default branch named `main`.
2. In the repository settings, enable GitHub Pages and set the source to `GitHub Actions`.
3. Push to `main` again, or re-run the workflow manually from the Actions tab.
4. The workflow exports the app with `shinylive`, uploads `site/` as the Pages artifact, and deploys it.

The workflow file is at [`.github/workflows/deploy-pages.yml`](/home/danie/.github/workflows/deploy-pages.yml).

## Manual QA Checklist

- Launch `shiny::runApp("app")` without errors.
- Confirm that all five population choices render correctly.
- Confirm that both estimators work for every population.
- Change `n` and verify the sampling distribution usually narrows as sample size increases.
- Compare mean versus median for `Household net worth` and verify the behavior differs.
- Confirm the last-20 CI plot updates and shows the most recent draw at the top.
- Export with `shinylive` and preview the generated `site/`.
- Confirm the app text clearly says the populations are synthetic and illustrative.
