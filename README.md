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


# Croatian chess - book and console application

Croatian Chess is a collection of various chess variants,
starting as a simple and natural enhancement to classical
chess and growing ever more complex with each new variant.

## Versions

**Book version** can be seen in a colophon (4th page), under *Source* section. \
The most recent book version is:

Version: 20220630.203048 <!--- readme-new-book-version-squished-utc-date-time-place-marker -->

Large number is actually date and time of the last commit,
converted to UTC for easier comparison.

**Application** and **library versions** can be seen in an application, using `version` command. \
The latest versions are:

Application: 0.0.1.61:493+20220701.214617 <!--- readme-new-app-version-major-minor-feature-commit+meta~breaks-place-marker --> \
Library: 0.0.1.61:493+20220701.214617 <!--- readme-new-lib-version-major-minor-feature-commit+meta~breaks-place-marker -->

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

This project is licensed under the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or (at your
option) any later version, see the [LICENSING](LICENSING), [COPYING](COPYING)
files for details.

The book is published as Public Domain work, see
[Wikipedia on public domain](https://en.wikipedia.org/wiki/Public_domain)
for details.

## Acknowledgments

* Dennis R.
* Ken T.
* Simon P. J.
* Alessandro M.
* Tomaso G. A.
* and many, many others

Thank you all!

## Playlist

A collection of songs for your listening pleasure while reading [the book](crochess.pdf):

* Main theme: [Christopher Tin - Sogno di Volare ("The Dream of Flight")](https://www.youtube.com/watch?v=WQYN2P3E06s)
* Introduction: [A. Marcello - Oboe Concerto in d minor (Marcel Ponseele, baroque oboe / Il Gardellino)](https://www.youtube.com/watch?v=vE2O_yfgtBU)
* Prerequisites: [Johann Sebastian Bach - Bouree In E Minor](https://www.youtube.com/watch?v=APNI2CC0k6A)
* Classical Chess: [Radetzky Marsch [Austrian march]](https://www.youtube.com/watch?v=Y1yvzngMqu4)
* Croatian Ties: [Ride of the Valkyries (by Wagner)](https://www.youtube.com/watch?v=UCO8DQ9ocos)
* Mayan Ascendancy: [Carl Orff - O Fortuna ~ Carmina Burana](https://www.youtube.com/watch?v=GXFSK0ogeg4)
* Age of Aquarius: [Handel - La Réjouissance](https://www.youtube.com/watch?v=KMslsg-NWWU)
* Miranda’s veil: [Vangelis - La petite fille de la mer](https://www.youtube.com/watch?v=UdPOCQGYwrk)
* Nineteen: [L'Achéron - Muy Linda, Anthony Holborne](https://www.youtube.com/watch?v=-3wgZZ9qu34)
* Hemera’s Dawn: [Tiësto - Elements Of Life (Live In Copenhagen)](https://www.youtube.com/watch?v=r0lvZuZeEMw)
* Tamoanchan Revisited: [The Atlantics - Adventures In Paradise](https://www.youtube.com/watch?v=J9c8acZnfnQ)
* Conquest of Tlalocan: [The Ecstasy of Gold - Ennio Morricone (The Good, the Bad and the Ugly)](https://www.youtube.com/watch?v=PYI09PMNazw)
* Discovery: [The Doors - Break On Through (To The Other Side)](https://www.youtube.com/watch?v=-r679Hhs9Zs)
* One: [Vangelis - L'enfant](https://www.youtube.com/watch?v=VwLtjnNIS9M)
* Outro: [Chris Christodoulou - Coalescence | Risk of Rain (2013)](https://www.youtube.com/watch?v=ysPtBjY8o_A)

YouTube playlist featuring all listed songs: [Croatian chess soundtrack](https://www.youtube.com/watch?v=WQYN2P3E06s&list=PLcLt6RezizPpCuR2Om646OVN1e3-xSiGj).

Outtakes:

* [Vivaldi - Chamber Concerto in D Minor, RV 96: I. Allegro](https://www.youtube.com/watch?v=sQOegZhWJmI) [^rv96]
* [Albinoni - Oboe Concerto #2 in D Minor Op. 9](https://www.youtube.com/watch?v=LjgndGuy77o)
* [Mozart Lacrimosa](https://www.youtube.com/watch?v=t6Hz6oscEPc)
* [Jean-Michel Jarre - Oxygene, Pt. 4](https://www.youtube.com/watch?v=kSIMVnPA994)
* [The Tornados - Telstar](https://www.youtube.com/watch?v=SQdDjy1UtW4)
* [La bourrée d'Avignon - Philidor (Ens. Le Banquet du Roy)](https://www.youtube.com/watch?v=GrtryY5xZjM)
* [Andreas Waldetoft - To The Ends of the Galaxy](https://www.youtube.com/watch?v=FuELLd3Ec4U)

YouTube playlist featuring all listed songs + outtakes: [Croatian chess soundtrack + outtakes](https://www.youtube.com/watch?v=WQYN2P3E06s&list=PLcLt6RezizPqG2NZhkRy0CO68jH7MDXdg).

[^rv96]: Unfortunately, complete concerto (which also works on Linux) is not available anymore.
