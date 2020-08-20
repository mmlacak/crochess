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

This will buid images as used in the book.

Next, compile PDF file:

```
./pdf.sh
```

If everything goes well, the new book will be created under `book` folder.

## Deployment

Now that you have PDF book, just copy it over to other PC, tablet, phone, ...

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
