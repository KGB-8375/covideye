# Shared variables between the UI and Server
# Use sparingly!

library(bslib)

light <- bs_theme(bootswatch = "cosmo")
dark  <- bs_theme(
  bootswatch = "cosmo",
  bg = "#161616",
  fg = "#D5D5D7",
  danger = "#E12952")