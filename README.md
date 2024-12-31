# Compass - Internal Use Only

This repository stores branded Quarto html and pdf templates.

## Installation

To use the template, create a new project or folder with either RStudio or Positron. Then, copy the following command into the Terminal:

`quarto use template Compass-Boussole-Akii-Izhinoogan/compassQuarto`

In most cases you don't want to create a subdirectory. Instead, you'll open the index.qmd file and edit that. 


## Customization

-   To customize Quarto functioning specific to this document (e.g., navbar versus sidebar; toc controls etc.), see **\_quarto.yml**

-   To customize typography and other brand elements, modify the **\_brand.yml**

-   To customize the Typst (pdf) layout see **\_extensions\\typst-template.typ**

-   To create custom divs or modify css/scss see **theme.scss**