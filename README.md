## Fork this repository for your own thesis.

It's designed to be compliant with the University of Utah's thesis office, so probably a minimal amount of tweaking is appropriate.
Feel free to issue merge/pull requests, especially ones that update the files to the latest specs.

If you want to keep track of changes in this repository, you might find the following links helpful. 
They're for github, but the same instructions apply:
- [Configuring a remote for a fork](https://help.github.com/articles/configuring-a-remote-for-a-fork/)
- [Syncing a fork](https://help.github.com/articles/syncing-a-fork/)

Below is what's in the official README.html from [the official LaTeX templates](http://ftp.math.utah.edu/pub/uuthesis/).

<!-- Converting to markdown, John Moeller, 2016-7-19 -->
<!-- Edit by Nelson H. F. Beebe <beebe@math.utah.edu> -->

# README for 2016-04-04 University of Utah sample thesis files

The file [uuthesis-2016-guide.pdf](uuthesis-2016-guide/uuthesis-2016-guide.pdf)
documents the new practices and typographical features.
It has associated source, style, and spelling dictionary
files as well.  A template for a new thesis can be
trivially scraped out of those files if needed, but it
is better to start with one of the three sample theses,
each in its own subdirectory.  Each has its own
`Makefile` to provide a from-scratch-to-PDF
production process with a single invocation of the
`make` command.  Each also has a `README`
file that sketches the steps needed to compile the thesis
into a final PDF file.

The file `Makefile`
is a sample Unix Makefile for automating the handling of
all of the steps described in the guide.  The only user
customizations that should be necessary are the lists of
dependent files.  Tab characters are
*significant* in Makefiles, and are not
equivalent to spaces, so do _not_ copy their
contents via cut-and-paste from a Web browser window:
download the file instead!

The subdirectory `bibtex`
contains all of the public BibTeX style files long
available at the University of Utah Mathematics
Department that are not also in recent TeX Live
distributions.  However, you are strongly advised to
stick with one of the basic four styles in original 1986
LaTeX if you are to avoid formatting disputes with the
Thesis Office.

The three sample theses are:
- [0](sample-thesis-0): a bare-bones black-and-white-only thesis;
- [1](sample-thesis-1): a thesis enhanced with color and multiple indexes;
- [2](sample-thesis-2): a thesis enhanced with color and multiple indexes, and a chapter containing a published paper.
