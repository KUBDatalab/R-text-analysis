---
title: Setup
---

## Data Sets

We will work with this [data set](data/articles.csv) in the course.

You do not need to download the file. We will do that together during the course.

## Software Setup

In the course we will use the Positron IDE. You might be more used to RStudio. 
Many differences exist, but the functionality we are going to actually use are
almost identical. Therefore you do not need to install Positron if you
already have RStudio installed. If not, we recommend that you install Positron.

One difference is that the default pipe "%>%" in RStudio is replaced with "|>"
in Positron. The way they work is identical

::::::::::::::::::::::::: callout

### Warning

Please do NOT install R and Positron on Onedrive or other clouddrives.
R will work but you will not be able to install the extensions to R
that you will need in this course!

::::::::::::

::::::::::::::::::::::::::::::::::::::: discussion

### Installing R and Positron

**R** and **Positron** are separate downloads and installations. R is the
underlying statistical computing environment, but using R alone is no
fun. Positron is a graphical integrated development environment (IDE) that makes
using R much easier and more interactive. You need to install R before you
install Positron. Once installed, because Positron is an IDE, Positron will run R in 
the background.  You do not need to run it separately. 



:::::::::::::::::::::::::::::::::::::::::::::::::::

:::::::::::::::: solution

### Online
Rather than installing R and Positron on your personal computer, 
[Posit Cloud](https://posit.cloud//){target="_blank"} offers a free, online alternative,
where you will be able to run R and RStudio in your browser. 
Sign up with your Google/Gmail account if you have one, or with any other email.

The free version of RStudio Cloud places limitations on the number of projects you
can work on, and the amount of memory and processing power you can access. For the 
purposes of following these lessons, RStudio Cloud is perfectly adequate, and what we
recommend if you have any problems installing R and Positron on your personal computer.

:::::::::::::::::::::::::

:::::::::::::::: solution

### Windows

#### If you already have R and RStudio/Positron installed

* Open RStudio, and click on "Help" > "Check for updates". If a new version is
	available, quit RStudio, and download the latest version for RStudio.
* Or, open Positron, and click on "Help" > "Installing update".
* To check which version of R you are using, start RStudio or Positron and the first thing
  that appears in the console indicates the version of R you are
  running. Alternatively, you can type `sessionInfo()`, which will also display
  which version of R you are running. Go on
  the [CRAN website](https://cran.r-project.org/bin/windows/base/){target="_blank"} and check
  whether a more recent version is available. If so, please download and install
  it. You can [check here](https://cran.r-project.org/bin/windows/base/rw-FAQ.html#How-do-I-UNinstall-R_003f){target="_blank"} for
  more information on how to remove old versions from your system if you wish to do so.

#### If you don't have R and RStudio or Positron installed

* Download R from
  the [CRAN website](https://cran.r-project.org/bin/windows/base/release.htm){target="_blank"}.
* Run the `.exe` file that was just downloaded.
* Go to the [Positron download page](https://positron.posit.co/download.html){target="_blank"}
* Accept the license agreement, and download the appropriate windows installer, probably Windows 10, 11 x64 (user level install)
* Double click the file to install it.
* Once it's installed, open Positron to make sure it works and you don't get any
  error messages.

:::::::::::::::::::::::::

:::::::::::::::: solution

### MacOS

#### If you already have R and RStudio installed

* Open RStudio, and click on "Help" > "Check for updates". If a new version is
	available, quit RStudio, and download the latest version for RStudio.
* Or, open Positron, and click on "Help" > "Installing update".
* To check the version of R you are using, start RStudio and the first thing
  that appears on the terminal indicates the version of R you are running. Alternatively, you can type `sessionInfo()`, which will also display which version of R you are running. Go on
  the [CRAN website](https://cran.r-project.org/bin/macosx/){target="_blank"} and check
  whether a more recent version is available. If so, please download and install
  it. In any case, make sure you have at least R 3.2.

#### If you don't have R and RStudio installed

* Download R from
  the [CRAN website](https://cran.r-project.org/bin/macosx/){target="_blank"}.
* Select the `.pkg` file for the latest R version.
* Double click on the downloaded file to install R.
* It is also a good idea to install [XQuartz](https://www.xquartz.org/){target="_blank"} (needed
  by some packages).
* Go to the [Positron download page](https://positron.posit.co/download.html){target="_blank"}
* Accept the license agreement, and download the appropriate MacOs installer, probably MacOS 11.0+ (universal)
* Double click the file to install Positron.
* Once it's installed, open Positron to make sure it works and you don't get any
  error messages.

:::::::::::::::::::::::::


:::::::::::::::: solution

### Linux

* Follow the instructions for your distribution
  from [CRAN](https://cloud.r-project.org/bin/linux){target="_blank"}, they provide information
  to get the most recent version of R for common distributions. For most
  distributions, you could use your package manager (e.g., for Debian/Ubuntu run
  `sudo apt-get install r-base`, and for Fedora `sudo yum install R`), but we
  don't recommend this approach as the versions provided by this approach are
  usually out of date. In any case, make sure you have at least R 3.2.
* Go to the [Positron download page](https://positron.posit.co/download.html){target="_blank"}
* Accept the license agreement.
* Under *Installers* select the version that matches your distribution, and
  install it with your preferred method (e.g., with Debian/Ubuntu `sudo dpkg -i
  Positron-2025.11.0-234-arm64.deb	` at the terminal).
* Once it's installed, open Positron to make sure it works and you don't get any
  error messages.

:::::::::::::::::::::::::