<span class="badge-patreon"><a href="https://patreon.com/mmlacak" title="Thank you for your donations!"><img src="https://img.shields.io/badge/Patreon-Thank_you_for_your_donations!-F96854.svg?logo=patreon" alt="Thank you for your donations!" /></a></span>
<span class="badge-blogger"><a href="https://croatian-chess.blogspot.com/" title="Croatian chess blog"><img src="https://img.shields.io/badge/Blogger-Croatian_chess_blog-FF5722.svg?logo=blogger" alt="Croatian chess blog" /></a></span>
<span class="badge-adobe-acrobat-reader"><a href="https://github.com/mmlacak/crochess/raw/master/crochess.pdf" title="Download the book!"><img src="https://img.shields.io/badge/Download-Croatian_chess_book-3333FF.svg?logo=adobe-acrobat-reader" alt="Download the book!" /></a></span>
<span class="badge-adobe-acrobat-reader"><a href="https://github.com/mmlacak/crochess/blob/master/crochess.pdf" title="Read the book!"><img src="https://img.shields.io/badge/Read-Croatian_chess_book-EC1C24.svg?logo=adobe-acrobat-reader" alt="Read the book!" /></a></span>
<!-- span class="badge-adobe-acrobat-reader"><a href="https://raw.githubusercontent.com/mmlacak/crochess/master/crochess.pdf" title="Download the book!"><img src="https://img.shields.io/badge/Read-Croatian_chess_book-9999FF.svg?logo=adobe-acrobat-reader" alt="Download the book!" /></a></span -->

<!--- ![Lines of code](https://img.shields.io/tokei/lines/github/mmlacak/crochess) -->
<!--- ![GitHub code size in bytes](https://img.shields.io/github/languages/code-size/mmlacak/crochess) -->
<!--- ![GitHub commit activity](https://img.shields.io/github/commit-activity/m/mmlacak/crochess) -->
<!--- ![GitHub last commit](https://img.shields.io/github/last-commit/mmlacak/crochess) -->
<!--- ![GitHub](https://img.shields.io/github/license/mmlacak/crochess) -->


# Croatian chess - console application

Croatian Chess is a collection of various chess variants,
starting as a simple and natural enhancement to classical
chess and growing ever more complex with each new variant.

## Versions

**Book version** can be seen in a colophon (4th page), under *Source* section. \
The most recent book version is:

Version: 20210723.135706 <!--- readme-new-book-version-squished-utc-date-time-place-marker -->

Large number is actually date and time of the last commit,
converted to UTC for easier comparison.

**Application** and **library versions** can be seen in an application, using `version` command. \
The latest versions are:

Application: 0.0.1.58:162+20210723.141306 <!--- readme-new-app-version-major-minor-feature-commit+meta~breaks-place-marker --> \
Library: 0.0.1.58:162+20210723.141306 <!--- readme-new-lib-version-major-minor-feature-commit+meta~breaks-place-marker -->

Versioning used is [Natural Versioning 1.2](https://croatian-chess.blogspot.com/p/natver.html),
with meta data containing the same format as the book version.

## Getting Started

If you just want to read the book, grab [crochess.pdf](crochess.pdf)
file available in the project root. Alternatively, if you
want updated version with the latest changes, fixes, you
can download it from:
[Croatian Chess Blog](http://croatian-chess.blogspot.com/p/preview.html).

This version is compiled with normal sized images, so it
takes almost 20 MB. Updated versions will be compiled with
draft image quality, resulting in images and PDF file about
half the size, at 8+ MB. Other than lower resolution images,
minor updates, fixes will be the only difference between
book posted here and updated version available through blog.

If you still want to compile PDF yourself, you'll need these:

### Prerequisites

To make images used in the book, install these:
- Python 3 (3.8.5)
- pycairo (1.18.2)
- GTK+ cairo (1.16.0)

Versions in brackets are those I'm using. Older versions
should also work since nothing too fancy was used.

To compile book itself into PDF, you'll need minimal LaTeX
installation, with following packages:
  - inputenc
  - charter
  - helvet
  - geometry
  - graphicx
  - wrapfig
  - hyperref
  - multirow
  - booktabs
  - alltt

### Installing

First, clone repository with:

```
git clone https://github.com/mmlacak/crochess.git
```

Or, you could just unpack downloaded zip file into a folder.

### Compiling the book

Open terminal in folder where you've cloned repository, and type:

```
./gfx.sh
```

This will generate all the images used in the book.

Next, compile PDF file:

```
./pdf.sh
```

If everything goes well, the new book will be created under `book` folder.

## Deployment

Now that you have PDF book, just copy it over to other PC, tablet, phone, ...

## Contributing

Please contact me via email.

## Authors

* **Mario Mlačak** - *Initial work* - [mmlacak](https://github.com/mmlacak)

## Blog

https://croatian-chess.blogspot.com

## License

This project is licensed under the BSD 3-clause, modified license,
see the [LICENSE](LICENSE) file for details.

The book is published as Public Domain work, see
[Wikipedia on public domain](https://en.wikipedia.org/wiki/Public_domain)
for details.

## Acknowledgments

* Dennis
* Ken
* Alessandro
* Tomasso
* and many, many others

Thank you all!
