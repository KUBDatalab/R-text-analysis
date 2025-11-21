---
title: 'Whats-next'
teaching: 10
exercises: 2
---

:::::::::::::::::::::::::::::::::::::: questions 

- What is the next step?

::::::::::::::::::::::::::::::::::::::::::::::::

::::::::::::::::::::::::::::::::::::: objectives

- Explain how to use markdown with the new lesson template
- Demonstrate how to include pieces of code, figures, and nested challenge blocks

::::::::::::::::::::::::::::::::::::::::::::::::



For general techniques and approaches to working with
text in a tidy manner, we recommend the [Text Mining with R](https://www.tidytextmining.com/){target="_blank"} book by Julia Silge and David Robinson.

Julia Silge have her own [youtube channel](https://www.youtube.com/juliasilge){target="_blank"} where she, among other things, do text analysis. You will learn a lot by listening to her thoughts while she code.

Text can be found everywhere. Rather than scraping text yourself, you can find R-packages containing text. 

`janeaustenr` contains the text of Jane Austens novels. Get it by running `install.packages("janeaustenr")`

The `sherlock` package contains the complete Sherlock Holmes collection by Arthur Conan Doyle. It must be installed from github, running 
`devtools::install_github("EmilHvitfeldt/sherlock")`. 

::::::::::::::::::::::::::::::::::::: keypoints 

- Use `.md` files for episodes when you want static content
- Use `.Rmd` files for episodes when you need to generate output
- Run `sandpaper::check_lesson()` to identify any issues with your lesson
- Run `sandpaper::build_lesson()` to preview your lesson locally

::::::::::::::::::::::::::::::::::::::::::::::::

