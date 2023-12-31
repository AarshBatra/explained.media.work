
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Data visualization focussed story telling

<!-- badges: start -->
<!-- badges: end -->

## Story 1: Understanding the state of global air quality data, research and funding infrastructure

- All code for the story can be found in the
  `global.air.quality.infra.story.Rmd` script at the root of this repo.
  Each story corresponds to a one Rmd and each chunk of any given Rmd
  corresponds to one visualization, within a given story.

- The helper script for the code can be found in the `R/` folder at the
  root.

- The datasets used in the code are not pushed to GitHub due to space
  limitations. Those can be downloaded from the following [drive
  folder](https://drive.google.com/drive/folders/18RnLiSeRdXKAe8EIZhLjbjxxYELpZj6u?usp=drive_link).
  Read notes on the datasets below.

- Visualizations are stored in the `output/` folder at the root of the
  repo.

- Notes on data files present in the drive folder above:

  - There are 3 final processed files that come out of the AQLI data
    pipeline. Each of these files contain **population weighted annual
    average PM2.5** data and corresponding life years lost (or potential
    gains in life expectancy: see the last point) data at country (gadm0
    file), state (gadm1 file) and district/county (gadm2 file). These
    are present in the `final processed CSVs` sub-folder in the drive
    folder above.

  - Corresponding to the 3 files above there are 3 shapefiles, which can
    be found in the `shapefiles` sub-folder.

  - Apart from this, there are couple other files that the helper script
    loads, all of which are also present in the folder above.

  - The helper script loads the
    [aqverse](https://github.com/aqli-epic/aqverse) pacakge, which is
    used in the plotting code.

  - The code book for AQLI datasets is uploaded as a text file named
    `aqli_code_book` at the root of this repo.

  - All AQLI pollution data is measured in micrograms per cubic meter.

  - The AQLI data story can be told in 2 ways that convey the same
    message and a mix of both are used in the [AQLI Annual Report
    2023](https://aqli.epic.uchicago.edu/reports/). Here is an example
    of Delhi, India:

    - Potential gains in years of life expectancy for a Delhi resident
      is 11.9 years if WHO PM2.5 guideline is met (default for the the
      [AQLI interactive map](https://aqli.epic.uchicago.edu/the-index/)
      and the data visualization in the output folder of this repo).
    - An average Delhi resident is losing 11.9 years of life as a result
      sustained long term exposure to breathing air that is not in
      compliance with the WHO guideline.
