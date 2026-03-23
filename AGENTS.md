# AGENTS.md

## Project Purpose

- Build and maintain a small R Shiny teaching app about sampling distributions for undergraduate social statistics.

## Stack

- Plain R Shiny
- Base R plotting and helper functions
- `shinylive` for static export
- GitHub Pages for deployment

## Key Commands

- `Rscript -e 'shiny::runApp("app")'`
- `Rscript scripts/smoke_test.R`
- `Rscript scripts/export_site.R`
- `Rscript scripts/preview_site.R`

## Guardrails

- Preserve the exact five synthetic teaching populations unless the specification changes.
- Keep dependencies minimal and compatible with `webR` and `shinylive`.
- Do not add filesystem writes, APIs, or real datasets to the running app.
- Do not casually change the app framing that the populations are synthetic and illustrative.

## Standing Instruction

- Use `/home/danie/.venvs/gss/bin/python` for all Python commands by default.
