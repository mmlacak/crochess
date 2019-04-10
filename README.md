# Croatian chess - console application

Croatian Chess is a collection of various chess variants,
starting as a simple and natural enhancement to classical
chess and growing ever more complex with each new variant.

## Getting Started

These instructions will get you a copy of the project up and
running on your local machine for development and testing
purposes. See deployment for notes on how to deploy the
project on a live system.

### Prerequisites

To make images used in the book, install these:
- GTK+ 2+, but not 3+
- pygtk 2+, but not 3+
- Python 2.7+, but not 3+

To compile book itself into PDF, you'll need:
- minimal LaTeX installation, with following packages:
  - inputenc
  - charter
  - helvet
  - geometry
  - graphicx
  - wrapfig
  - hyperref

Alternatively, you can just install all of LaTeX,
but it'll take 2+ Gb on HDD.

To compile application, install:
- GHC 7.6+
- parsec with its' dependencies

### Installing

First, clone repository with:

```
git clone https://github.com/mmlacak/crochess.git
```

Other possibility is to just download zip file instead.

### Compiling the book

Open terminal in folder where you cloned repository, and type:

```
./gfx.sh
```

This will buid images as used in the book.

Next, compile PDF file:

```
./pdf.sh
```

If everything goes well, the new book will be created under `book` folder.

### Compiling the application

In the same terminal, and in the same folder, type in:

```
./compile.sh
```

This will compile Haskell code into `crochess` executable in `src` folder.

## Deployment

Now that you have PDF book and/or executable, just copy them over to
other PC into folder of your chosing.

## Contributing

Please contact me via email.

## Versioning

Too deep in pre-alpha stage to do any.

## Authors

* **Mario Mlačak** - *Initial work* - [mmlacak](https://github.com/mmlacak)

## Blog

https://croatian-chess.blogspot.com

## License

This project is licensed under the BSD 3-clause, modified license,
see the [LICENSE](LICENSE) file for details.

## Acknowledgments

* Linus
* Guido
* Simon
* Sean
* and many, many others

Thank you all!
