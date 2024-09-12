<span class="badge-adobe-acrobat-reader"><a href="https://github.com/mmlacak/crochess/raw/master/crochess.pdf" title="Download the book!"><img src="https://img.shields.io/badge/The_book-Download!-EC1C24.svg?logo=adobe-acrobat-reader&style=for-the-badge" alt="Download the book!" /></a></span>
<span class="github"><a href="https://github.com/sponsors/mmlacak" title="Thank you for your donations!"><img src="https://img.shields.io/badge/GitHub-Thank_you!-F5F5F5.svg?logo=github&style=for-the-badge" alt="Thank you for your donations!" /></a></span>
<span class="badge-blogger"><a href="https://croatian-chess.blogspot.com/" title="Read it all on the blog!"><img src="https://img.shields.io/badge/Blogger-Read_it_all!-FF5722.svg?logo=blogger&style=for-the-badge" alt="Read it all on the blog!" /></a></span>
<!-- span class="badge-patreon"><a href="https://patreon.com/mmlacak" title="Thank you for your donations!"><img src="https://img.shields.io/badge/Patreon-Thank_you!-F96854.svg?logo=patreon&style=for-the-badge" alt="Thank you for your donations!" /></a></span -->

<!-- span class="badge-adobe-acrobat-reader"><a href="https://github.com/mmlacak/crochess/raw/master/crochess.pdf" title="Download the book!"><img src="https://img.shields.io/badge/The_book-Download!-EC1C24.svg?logo=adobe-acrobat-reader&style=flat-square" alt="Download the book!" /></a></span -->
<!-- span class="github"><a href="https://github.com/sponsors/mmlacak" title="Thank you for your donations!"><img src="https://img.shields.io/badge/GitHub-Thank_you!-F5F5F5.svg?logo=github&style=flat-square" alt="Thank you for your donations!" /></a></span -->
<!-- span class="badge-blogger"><a href="https://croatian-chess.blogspot.com/" title="Read it all on the blog!"><img src="https://img.shields.io/badge/Blogger-Read_it_all!-FF5722.svg?logo=blogger&style=flat-square" alt="Read it all on the blog!" /></a></span -->
<!-- span class="badge-patreon"><a href="https://patreon.com/mmlacak" title="Thank you for your donations!"><img src="https://img.shields.io/badge/Patreon-Thank_you!-F96854.svg?logo=patreon&style=flat-square" alt="Thank you for your donations!" /></a></span -->

# Croatian Chess - book and console application

Croatian Chess is a collection of various chess variants,
starting as a simple and natural enhancement to classical
chess and growing ever more complex with each new variant.

## Versions

**Book version** can be seen in a colophon (4th page), under *Source* section. \
The most recent book version is:

Version: 20240901.232536 <!--- readme-new-book-version-squished-utc-date-time-place-marker -->

Large number is actually date and time of the last commit,
converted to UTC for easier comparison.

**Application** and **library versions** can be seen in an application, using `version` command. \
The latest versions are:

Application: 0.0.1.696:1128+20240912.063542 <!--- readme-new-app-version-major-minor-feature-commit+meta~breaks-place-marker --> \
Library: 0.0.1.696:1128+20240912.063542 <!--- readme-new-lib-version-major-minor-feature-commit+meta~breaks-place-marker -->

Versioning used is [Natural Versioning 1.2](https://croatian-chess.blogspot.com/p/natver.html),
with meta data containing the same format as the book version.

## Getting Started

To read the book, click [download PDF file](https://github.com/mmlacak/crochess/raw/master/crochess.pdf).

<!-- To read book in your web-browser, click [GitHub's PDF viewer](https://github.com/mmlacak/crochess/blob/master/crochess.pdf). -->

To compile book yourself, you'll need to generate images, and then compile text and those images into PDF file.

### Prerequisites

To make images used in the book, install these:
- Python 3 (3.8.5)
- pycairo (1.18.2)
- GTK+ cairo (1.16.0)

Versions in brackets are those I was using, newer versions also work. \
Older versions should work, since nothing too fancy was used from those dependencies.

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

Open terminal in folder where you've cloned/unzipped repository, and type:

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

## Authors

* **Mario Mlačak** - *Initial work* - [mmlacak](https://github.com/mmlacak)

## Contributing

[GitHub sponsors](https://github.com/sponsors/mmlacak)

[Patreon](https://patreon.com/mmlacak)

Thank you for your donations!

For non-monetary contributions, please contact me via email first.

## Socials

<!-- [Discord](https://discord.gg/PJ2dtRa5AU) -->

[Croatian Chess Blog](https://croatian-chess.blogspot.com)

## License

This project is licensed under the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or (at your
option) any later version, see
[LICENSING](https://raw.githubusercontent.com/mmlacak/crochess/master/LICENSING),
[COPYING](https://raw.githubusercontent.com/mmlacak/crochess/master/COPYING)
files in root folder for details.

The book is published as Public Domain work, under CC0 1.0 Universal Public Domain
Dedication, see
[LICENSING](https://raw.githubusercontent.com/mmlacak/crochess/master/book/LICENSING),
[COPYING](https://raw.githubusercontent.com/mmlacak/crochess/master/book/COPYING)
files in `book` folder for details.

## Acknowledgments

* Dennis R.
* Ken T.
* Simon P. J.
* Alessandro M.
* Tomaso G. A.
* O. Claude M.
* and many, many others

Thank you all!

## Playlist

A collection of songs for your listening pleasure while reading [the book](https://github.com/mmlacak/crochess/raw/master/crochess.pdf):

* Main theme: [Christopher Tin - Sogno di Volare ("The Dream of Flight")](https://www.youtube.com/watch?v=WQYN2P3E06s)
* Introduction: [Renaissance - Wanderer](https://www.youtube.com/watch?v=AwJugy5Soto)
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

YouTube playlist featuring all listed songs: [Croatian Chess soundtrack](https://www.youtube.com/watch?v=WQYN2P3E06s&list=PLcLt6RezizPpCuR2Om646OVN1e3-xSiGj).

Outtakes:

* [Vivaldi - Chamber Concerto in D Minor, RV 96: I. Allegro](https://www.youtube.com/watch?v=sQOegZhWJmI) [^rv96]
* [Albinoni - Oboe Concerto #2 in D Minor Op. 9](https://www.youtube.com/watch?v=LjgndGuy77o)
* [Mozart Lacrimosa](https://www.youtube.com/watch?v=t6Hz6oscEPc)
* [Jean-Michel Jarre - Oxygene, Pt. 4](https://www.youtube.com/watch?v=kSIMVnPA994)
* [The Tornados - Telstar](https://www.youtube.com/watch?v=SQdDjy1UtW4)
* [La bourrée d'Avignon - Philidor (Ens. Le Banquet du Roy)](https://www.youtube.com/watch?v=GrtryY5xZjM)
* [Andreas Waldetoft - To The Ends of the Galaxy](https://www.youtube.com/watch?v=FuELLd3Ec4U)
* [A. Marcello - Oboe Concerto in d minor (Marcel Ponseele, baroque oboe / Il Gardellino)](https://www.youtube.com/watch?v=vE2O_yfgtBU)

YouTube playlist featuring all listed songs + outtakes: [Croatian chess soundtrack + outtakes](https://www.youtube.com/watch?v=WQYN2P3E06s&list=PLcLt6RezizPqG2NZhkRy0CO68jH7MDXdg).

[^rv96]: Unfortunately, complete concerto (which also works on Linux) is not available anymore.
