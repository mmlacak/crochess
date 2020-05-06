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

To compile book itself into PDF, you'll need minimal LaTeX
installation, with following packages:
  - inputenc
  - charter
  - helvet
  - geometry
  - graphicx
  - wrapfig
  - hyperref
  - enumitem

To compile application, you'll need C compiler:
- gcc (7.4.0 works fine)
- clang (6.0.0 works fine, too)
- or any other adhering to C99 standard

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

This will compile C code into `crochess` executable in `bin` folder.

One day, that is, when that [functionality is provided](https://croatian-chess.blogspot.com/2019/06/goodbye-haskell-hello-c.html).

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

* Dennis
* Ken
* Alessandro
* Tomasso
* and many, many others

Thank you all!
