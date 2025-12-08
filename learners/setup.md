---
title: Setup
---

## Software Setup

::::::::::::::::::::::::: callout

### Warning

Please do NOT install R, Positron and RStudio on Onedrive or other cloud drives.
R will work but you will not be able to install the extensions to R needed in this course!

::::::::::::

::::::::::::::::::::::::::::::::::::::: discussion

### Installing R and Positron

**R** and **Positron** are separate downloads and installations. R is the
underlying statistical computing environment, but using R alone is no
fun. Positron is a graphical Integrated Development Environment (IDE) that makes
using R much easier and more interactive. You need to install R before you
install Positron. Once installed, because Positron is an IDE, Positron will run R in 
the background. You do not need to run R separately.

In the course we will be using **Positron**. If you are already using RStudio and would prefer to keep doing so, that is not a problem.

Please note that the short cut (ctrl + shift + m) in Positron will give you the base-R pipe (|>), whereas in RStudio the same short cut will give you the magrittr pipe (%>%). They work the same way.

:::::::::::::::::::::::::::::::::::::::::::::::::::

:::::::::::::::: solution

### Online
Rather than installing R and Positron on your personal computer, 
[Posit Cloud](https://posit.cloud//) offers a free, online alternative,
where you will be able to run R and RStudio in your browser. 
Sign up with your email address.

The free version of RStudio Cloud places limitations on the number of projects you
can work on, and the amount of memory and processing power you can access. For the 
purposes of following these lessons, RStudio Cloud is perfectly adequate, and what we
recommend if you have any problems installing R and Positron on your personal computer.

:::::::::::::::::::::::::

:::::::::::::::: solution

### Windows

#### If you already have R and RStudio installed

In the course we will be using Positron. If you prefer to continue using RStudio, that is not a problem, but please do the following:

* Open RStudio, and click on "Help" > "Check for updates". If a new version is
	available, quit RStudio, and download the latest version of RStudio.
* To check which version of R you are using, start RStudio and the first thing
  that appears in the console indicates the version of R you are
  running. Alternatively, you can type `sessionInfo()`, which will also display
  which version of R you are running. Go to
  the [CRAN website](https://cran.r-project.org/bin/windows/base/) and check
  if a more recent version is available. If so, please download and install
  it. You can [check here](https://cran.r-project.org/bin/windows/base/rw-FAQ.html#How-do-I-UNinstall-R_003f) for
  more information on how to remove old versions from your system if you wish to do so.
* In any case, make sure you have at least R 4.5 installed.

#### If you already have R and Positron installed

* Open Positron, and click on the "cogwheel" (lower left corner) > "Check for updates". If a new version is available, it will be installed in the background.
* To check which version of R you are using, start Positron and the first thing
  that appears in the console indicates the version of R you are
  running. Alternatively, you can type `sessionInfo()`, which will also display
  which version of R you are running. Go to
  the [CRAN website](https://cran.r-project.org/bin/windows/base/) and check
  if a more recent version is available. If so, please download and install
  it. You can [check here](https://cran.r-project.org/bin/windows/base/rw-FAQ.html#How-do-I-UNinstall-R_003f) for
  more information on how to remove old versions from your system if you wish to do so.
* In any case, make sure you have at least R 4.5 installed.

#### If you don't have R and Positron installed

* Download R from
  the [CRAN website](https://cran.r-project.org/bin/windows/base/release.htm).
* Run the `.exe` file that was just downloaded.
* Go to the [Positron download page](https://positron.posit.co/download.html).
* First accept the user agreement
* Choose the windows user level install option.
* Double click the file to install it.
* Once it's installed, open Positron to make sure it works and you don't get any
  error messages.

:::::::::::::::::::::::::

:::::::::::::::: solution

### MacOS

#### If you already have R and RStudio installed

In the course we will be using Positron. If you prefer to continue using RStudio, that is not a problem, but please do the following.

* Open RStudio, and click on "Help" > "Check for updates". If a new version is available, quit RStudio, and download the latest version for RStudio.
* To check the version of R you are using, start RStudio and the first thing
  that appears on the terminal indicates the version of R you are running. Alternatively, you can type `sessionInfo()`, which will also display which version of R you are running. Go to
  the [CRAN website](https://cran.r-project.org/bin/macosx/) and check
  if a more recent version is available. If so, please download and install
  it. 
* In any case, make sure you have at least R 4.5 installed.

### If you already have R and Positron installed
* Open Positron, and click on the "cogwheel" (lower left corner) > "Check for updates". If a new version is available, it will be installed in the backgound.
* To check the version of R you are using, start RStudio and the first thing
  that appears on the terminal indicates the version of R you are running. Alternatively, you can type `sessionInfo()`, which will also display which version of R you are running. Go to
  the [CRAN website](https://cran.r-project.org/bin/macosx/) and check
  if a more recent version is available. If so, please download and install
  it. 
* In any case, make sure you have at least R 4.5 installed.


#### If you don't have R and Positron installed

* Download R from
  the [CRAN website](https://cran.r-project.org/bin/macosx/).
* Select the `.pkg` file for the latest R version.
* Double click on the downloaded file to install R.
* It is also a good idea to install [XQuartz](https://www.xquartz.org/) (needed
  by some packages).
* Go to the [Positron download page](https://positron.posit.co/download.html).
* First accept the user agreement
* Choose the version appropriate for your version of Mac.
* Double click the file to install it.
* Once it's installed, open Positron to make sure it works and you don't get any
  error messages.

:::::::::::::::::::::::::


:::::::::::::::: solution

### Linux

* Follow the instructions for your distribution
  from [CRAN](https://cloud.r-project.org/bin/linux), they provide information
  to get the most recent version of R for common distributions. For most
  distributions, you could use your package manager (e.g., for Debian/Ubuntu run
  `sudo apt-get install r-base`, and for Fedora `sudo yum install R`), but we
  don't recommend this approach as the versions provided by this approach are
  usually out of date. 
* In any case, make sure you have at least R 4.5.
* Go to the
  [Positron download page](https://positron.posit.co/download.html).
* First accept the user agreement
* Choose the version appropriate for your distribution, and
  install it with your preferred method (e.g., with Debian/Ubuntu `sudo dpkg -i
  Positron-2025.12.0-167-x64.deb` at the terminal).
* Once it's installed, open Positron to make sure it works and you don't get any
  error messages.


:::::::::::::::::::::::::


