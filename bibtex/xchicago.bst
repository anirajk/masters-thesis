%%% -*-bst-*-
%%% ====================================================================
%%%  @BibTeX-style-file{
%%%     author-1        = "Nelson H. F. Beebe",
%%%     author-2        = "Glenn Paulley",
%%%     version         = "5.0 (beta)",
%%%     date            = "23 December 1999",
%%%     time            = "09:25:20 MST",
%%%     filename        = "xchicago.bst",
%%%     address-1       = "University of Utah
%%%                        Department of Mathematics, 322 INSCC
%%%                        155 S 1400 E RM 233
%%%                        Salt Lake City, UT 84112-0090
%%%                        USA",
%%%     address-2       = "Data Structuring Group
%%%                        Department of Computer Science
%%%                        University of Waterloo
%%%                        Waterloo, Ontario, Canada
%%%                        N2L 3G1",
%%%     telephone-1     = "+1 801 581 5254",
%%%     telephone-2     = "+1 519 885-1211",
%%%     FAX-1           = "+1 801 585 1640, +1 801 581 4148",
%%%     FAX-2           = "+1 519 885-1208",
%%%     checksum        = "49407 4159 15665 117112",
%%%     email-1         = "beebe@math.utah.edu",
%%%     email-2         = "gnpaulle@bluebox.uwaterloo.ca",
%%%     URL-1           = "http://www.math.utah.edu/~beebe",
%%%     codetable       = "ISO/ASCII",
%%%     keywords        = "bibliography style, BibTeX, Chicago
%%%                        Manual of Style bibliography style",
%%%     supported       = "yes",
%%%     docstring       = "This is BibTeX bibliography style that
%%%                        follows the bibliography style of the 14th
%%%                        edition of the Chicago Manual of Style
%%%                        (1993), with extensions for several
%%%                        additional BibTeX entry keywords (bibdate,
%%%                        bibsource, bookpages, CODEN, day, fjournal,
%%%                        ISBN, ISSN, LCCN, MRclass, MRnumber,
%%%                        MRreviewer, price, and URL), and the
%%%                        Periodical document type.  There is also a
%%%                        secondary set of keywords (annote, bibdate,
%%%                        bibsource, classcodes, classification,
%%%                        fjournal, journalabr, keywords, language,
%%%                        subject, thesaurus, and treatment) that are
%%%                        recognized, but not normally typeset.
%%%
%%%                        Version 5.0 of xchicago.bst is a major
%%%                        overhaul by Nelson H. F. Beebe of Glenn
%%%                        N. Paulley's chicago.bst, version 4.0.  That
%%%                        version in turn was based on earlier work by
%%%                        Peter F. Patel-Schneider (named.bst), Stephen
%%%                        Spencer (newapa.bst), Young Ryu and Brian
%%%                        Reiser (apalike.bst), and Oren Patashnik
%%%                        (abbrv.bst, alpha.bst, plain.bst, and
%%%                        unsrt.bst).  Version 5.0 also includes
%%%                        features, and some code, from NHFB's
%%%                        is-abbrv.bst, is-alpha.bst, is-plain.bst, and
%%%                        is-unsrt.bst styles.
%%%
%%%                        Many, but certainly not all, of the
%%%                        recommendations of the 14th edition of the
%%%                        Chicago Manual of Style (hereafter referred
%%%                        to as CMoS-14) have been implemented, with
%%%                        two notable, and intentional, omissions: (1)
%%%                        elided leading digits in page number ranges
%%%                        are not provided, and (2) consecutive leading
%%%                        identical authors are not replaced by a 3-em
%%%                        dash (CMoS-14 Section 15.66).  Both of these
%%%                        hinder the reader, and make machine
%%%                        processing unnecessarily difficult.
%%%
%%%                        CMoS-14 offers on pp. 487--635 a
%%%                        ``reference list'' description, and on
%%%                        pp. 637--699, an ``author-date
%%%                        bibliography'' description.  It is the
%%%                        latter kind that is implemented by
%%%                        xchicago.bst.
%%%
%%%                        These more than 200 pages devoted to the
%%%                        construction of bibliographies have many
%%%                        fine points and special cases that would
%%%                        require manual tweaking of bibliographic
%%%                        data, and even worse, would prevent reuse
%%%                        of that data for other bibliography styles.
%%%                        In NHFB's view, it is far more important to
%%%                        develop shared bibliographic data with
%%%                        standardized markup, than it is to cater to
%%%                        whimsical and illogical bibliographic
%%%                        traditions.
%%%
%%%                        Version 5.0 marks a major departure from
%%%                        all earlier BibTeX styles (of which there
%%%                        were 176 in Fall 1999 scattered about the
%%%                        CTAN archives), and is the first is a
%%%                        series of planned styles, each prefixed
%%%                        with the letter x, and accompanied by a
%%%                        companion TeX/LaTeX/... x*.sty file
%%%                        defining macros needed by TeX.
%%%
%%%                        The new x-series offers two important new
%%%                        features:
%%%
%%%                        (1) The number of recognized and supported
%%%                            BibTeX keywords is increased by about a
%%%                            half (from 23 in version 4.0 of
%%%                            chicago.bst, to 38 in version 5.0 of
%%%                            xchicago.bst).
%%%
%%%                            The additional keywords (listed above)
%%%                            have been found useful, and are widely
%%%                            used, in the TeX User Group and BibNet
%%%                            bibliography archive projects, offering
%%%                            over 250,000 BibTeX entries in Fall 1999.
%%%
%%%                        (2) The output .bbl files contain wrapper
%%%                            macros around every field value, allowing
%%%                            user control over whether, and how, the
%%%                            extended fields are typeset in the
%%%                            bibliography, without the need to modify
%%%                            the .bst file, and preserving most of the
%%%                            markup that was present in the original
%%%                            BibTeX file.
%%%
%%%                            As more journals become available in
%%%                            electronic form, this additional markup
%%%                            will be increasingly useful and
%%%                            essential, since it is the key to
%%%                            providing automatic hypertext links to
%%%                            cited publications, and to supporting
%%%                            directed database searching.
%%%
%%%                            The wrappers also make it possible to
%%%                            recover a usable BibTeX .bib file from
%%%                            the output .bbl file.  A demonstration
%%%                            program for doing that is provided with
%%%                            the distribution of the x-styles.
%%%
%%%                            Recovery from the same .bbl file data of
%%%                            other bibliographic database formats,
%%%                            such as bibIX, EndNote, nroff/troff,
%%%                            SGML, and Tib, would require only
%%%                            relatively minor modifications of that
%%%                            program.
%%%
%%%                        To accomplish these changes, a major
%%%                        overhaul of chicago.bst version 4.0 was
%%%                        required.  The number of FUNCTION
%%%                        definitions has doubled, from 86 to
%%%                        172, and the number of lines of code
%%%                        (excluding comments) has also almost
%%%                        doubled (from 1400 to 2790 lines).  Despite
%%%                        the larger size, however, the new styles
%%%                        have considerably more internal
%%%                        documentation, and the code has been
%%%                        simplified in many places, so that
%%%                        derivation of related styles should be much
%%%                        easier than it was in the past.
%%%
%%%                        The checksum field above contains a CRC-16
%%%                        checksum as the first value, followed by the
%%%                        equivalent of the standard UNIX wc (word
%%%                        count) utility output of lines, words, and
%%%                        characters.  This is produced by Robert
%%%                        Solovay's checksum utility.",
%%%  }
%%% ====================================================================
%%%
%%% ====================================================================
%%%                      PROGRAMMING CONVENTIONS
%%%
%%% Comments in this file follow a venerable practice from the Lisp
%%% programming language:
%%%
%%%     (1) inline comments begin with a single comment character
%%%         (percent in BibTeX styles), and are lined up at a common
%%%         indentation column to highlight their visibility.  The must
%%%         be preceded by at least one space.
%%%
%%%     (2) block comments within functions are indented a few spaces,
%%%         and begin with two comment characters.
%%%
%%%     (3) outer-level comments are flush left, and begin with three
%%%         comment characters.
%%%
%%% ENTRY, EXECUTE, FUNCTION, INTEGERS, ITERATE, MACRO, READ, REVERSE,
%%% SORT, and STRINGS objects are separated by two blank lines, and
%%% trailing blanks are stripped.
%%%
%%% FUNCTIONs contain a brief comment describing their purpose (if not
%%% already obvious from their descriptive names), the state of the
%%% BibTeX stack at entry and exit, and what global variables are
%%% modified.  The lack of such critical documentation in virtually all
%%% existing styles before this one has severely handicapped difficult
%%% debugging tasks: one should not have to reverse engineer code to
%%% debug it!
%%%
%%% Where possible, FUNCTIONs should be ordered alphabetically by
%%% name, ignoring lettercase.  Regrettably, BibTeX sometimes seems to
%%% need definition-before-use, preventing this only sensible and
%%% universally-recognized order for large collections of objects.
%%%
%%% The logical indentation increment is two blanks, and conditionals
%%% are preferably written in the forms
%%%
%%%     Boolean-condition
%%%       { short-then-part }
%%%       { short-else-part }
%%%     if$
%%%
%%% and
%%%
%%%     Boolean-condition
%%%       {
%%%         long-then-part
%%%       }
%%%       {
%%%         long-else-part
%%%       }
%%%     if$
%%%
%%% with their two statement blocks indented one additional level, in
%%% order to enhance visibility and understanding.
%%%
%%% While-loops should be written like this:
%%%
%%%       { short-Boolean-condition }
%%%       { short-body }
%%%     while$
%%%
%%% and
%%%
%%%       {                             % while-loop Boolean condition
%%%         long-Boolean-condition
%%%       }
%%%       {                             % while-loop body
%%%         long-body
%%%       }
%%%     while$
%%%
%%% Line length should normally be limited to about 72 characters, and
%%% preferably less.
%%%
%%% Use spaces, not tabs, because tabs are not handled properly in
%%% some systems.
%%%
%%% Style file developers are strongly urged to adhere to these
%%% guidelines, for at least these reasons:
%%%
%%% (1) BibTeX style files are written in a reverse-Polish (postfix or
%%%     PostScript-like or Forth-Like) language with many functions and few
%%%     temporary variables, all of which are globally visible.  Data is
%%%     mostly passed around on the stack, and all operators work only with
%%%     data on the stack.  Languages like this are very hard to read and
%%%     debug, because all data operated on is anonymous.  Stack corruption
%%%     is far too easy to cause, and far too difficult to find and repair.
%%%
%%% (2) The programmer's life is made even harder because almost all
%%%     functions are executed from inside BibTeX in response to the final
%%%     ITERATE {call.type$}; any errors that occur are diagnosed at the
%%%     line number of that ITERATE statement, when in reality, the
%%%     erroneous code is 2000 lines away.
%%%
%%% (3) There is no BibTeX style file debugger, and no formatted print
%%%     statements, so debugging must be done with print statements of the
%%%     form
%%%
%%%         "string" top$
%%%
%%% (4) Worse, there is no nondestructive way to print the stack contents.
%%%     It is a serious design flaw in BibTeX 0.99 that the built-in stack$
%%%     function prints and destroys the stack; PostScript's pstack operator
%%%     just prints the stack, without modifying it.
%%%
%%% (5) There is no way to test if the stack is empty, or how many items are
%%%     on it.
%%%
%%% (6) There is no way to test the type of stack items (integer, strings,
%%%     or procedures), yet all operators require operands of specific
%%%     types.  It is therefore impossible to provide datatype coercion or
%%%     to issue sensible error messages for operand type errors.
%%%
%%% (7) There is no way to get a function call stack trace, which is a
%%%     critical feature for speedy determination of `how did I get here,
%%%     and why?'.
%%%
%%% (8) There are insufficient string primitives, no regular
%%%     expression pattern matching, no arrays, and nothing like the
%%%     fantastically useful match() and split() functions in awk and
%%%     perl.
%%%
%%% We can hope that BibTeX 1.0 will remove these misfeatures.
%%%
%%% ====================================================================

%%% ====================================================================
%%%
%%% "Chicago" BibTeX style, chicago.bst
%%% ===================================
%%%
%%% BibTeX `chicago' style file for BibTeX version 0.99c, LaTeX version 2.09
%%% Place it in a file called chicago.bst in the BibTeX search path.
%%% You need to include chicago.sty as a \documentstyle option.
%%% (Placing it in the same directory as the LaTeX document should also work.)
%%% This "chicago" style is based on newapa.bst (American Psych. Assoc.)
%%% found at ymir.claremont.edu.
%%%
%%%   Citation format: (author-last-name year)
%%%             (author-last-name and author-last-name year)
%%%             (author-last-name, author-last-name, and author-last-name year)
%%%             (author-last-name et al. year)
%%%             (author-last-name)
%%%             author-last-name (year)
%%%             (author-last-name and author-last-name)
%%%             (author-last-name et al.)
%%%             (year) or (year,year)
%%%             year or year,year
%%%
%%%   Reference list ordering: alphabetical by author or whatever passes
%%%    for author in the absence of one.
%%%
%%% This BibTeX style has support for abbreviated author lists and for
%%%    year-only citations.  This is done by having the citations
%%%    actually look like
%%%
%%%    \citeauthoryear{full-author-info}{abbrev-author-info}{year}
%%%
%%% The LaTeX style has to have the following (or similar)
%%%
%%%     \let\@internalcite\cite
%%%     \def\fullcite{\def\citeauthoryear##1##2##3{##1, ##3}\@internalcite}
%%%     \def\fullciteA{\def\citeauthoryear##1##2##3{##1}\@internalcite}
%%%     \def\shortcite{\def\citeauthoryear##1##2##3{##2, ##3}\@internalcite}
%%%     \def\shortciteA{\def\citeauthoryear##1##2##3{##2}\@internalcite}
%%%     \def\citeyear{\def\citeauthoryear##1##2##3{##3}\@internalcite}
%%%
%%% These TeX macro definitions are found in chicago.sty. Additional
%%% commands to manipulate different components of a citation can be defined
%%% so that, for example, you can list author's names without parentheses
%%% if using a citation as a noun or object in a sentence.
%%%
%%% This file was originally copied from newapa.bst at ymir.claremont.edu.
%%%
%%% Features of chicago.bst:
%%% =======================
%%%
%%% - full names used in citations, but abbreviated citations are available
%%%   (see above)
%%% - if an entry has a "month", then the month and year are also printed
%%%   as part of that bibitem.
%%% - all conjunctions use "and" instead of "\&"
%%% - major modification from Chicago Manual of Style (13th ed.) is that
%%%   only the first author in a reference appears last name first-
%%%   additional authors appear as J. Q. Public.
%%% - pages are listed as "pp. xx-xx" in all entry types except
%%%   article entries.
%%% - book, inbook, and manual use "location: publisher" (or organization)
%%%   for address and publisher. All other types list publishers separately.
%%% - "pp." are used to identify page numbers for all entry types except
%%%   articles.
%%% - organization is used as a citation label if neither author nor editor
%%%   is present (for manuals).
%%% - "et al." is used for long author and editor lists, or when "others"
%%%   is used.
%%%
%%% Modifications and bug fixes from newapa.bst:
%%% ===========================================
%%%
%%%   - added month, year to bib entries if month is present
%%%   - fixed bug with In proceedings, added necessary comma after title
%%%   - all conjunctions changed to "and" from "\&"
%%%   - fixed bug with author labels in my.full.label: "et al." now is
%%%        generated when "others" is an author name
%%%   - major modification from Chicago Manual of Style (13th ed.) is that
%%%     only the first author in a reference appears last name first-
%%%     additional authors appear as J. Q. Public.
%%%   - pages are listed as "pp. xx-xx" in all entry types except
%%%     article entries. Unnecessary (IMHO) "()" around page numbers
%%%     were removed, and page numbers now don't end with a period.
%%%   - created chicago.sty for use with this bibstyle (required).
%%%   - fixed bugs in FUNCTION {format.vol.num.pages} for missing volume,
%%%     number, and /or pages. Renamed to format.journal.volume.number.pages.
%%%   - fixed bug in formatting booktitles: additional period an error if
%%%     book has a volume.
%%%   - fixed bug: editors usually given redundant period before next clause
%%%     (format.editors.dot) removed.
%%%   - added label support for organizations, if both author and editor
%%%     are missing (from alpha.bst). If organization is too long, then
%%%     the key field is used for abbreviated citations.
%%%   - In proceedings or books of several volumes, no comma was written
%%%     between the "Volume x" and the page numbers (this was intentional
%%%     in newapa.bst). Fixed.
%%%   - Some journals may not have volumes/numbers, only month/year (eg.
%%%     IEEE Computer). Fixed bug in article style that assumed volume/number
%%%     was always present.
%%%
%%% Original documentation for newapa.sty:
%%% =====================================
%%%
%%% This version was made by modifying the master file made by
%%% Oren Patashnik (PATASHNIK@SCORE.STANFORD.EDU), and the 'named' BibTeX
%%% style of Peter F. Patel-Schneider.
%%%
%%% Copyright (C) 1985, all rights reserved.
%%% Copying of this file is authorized only if either
%%% (1) you make absolutely no changes to your copy, including name, or
%%% (2) if you do make changes, you name it something other than 'newapa.bst'.
%%% There are undoubtably bugs in this style.  If you make bug fixes,
%%% improvements, etc.  please let me know.  My e-mail address is:
%%%    spencer@cgrg.ohio-state.edu or 71160.3141@compuserve.com
%%%
%%% This style was made from 'plain.bst', 'named.bst', and 'apalike.bst',
%%% with lots of tweaking to make it look like APA style, along with tips
%%% from Young Ryu and Brian Reiser's modifications of 'apalike.bst'.
%%% ====================================================================

ENTRY
  {
    %% Keywords marked [HIDDEN] in attached comments are recognized and output
    %% wrapped by macros that by default discard their arguments.
    %%
    %% Observation: there are 46 keywords listed here.  Each may have a
    %% value which is empty, unknown, or known.  Thus, there are 3^{46}
    %% == 8.86*10^{21} paths through the formatting of an entry for
    %% typesetting, so exhaustive testing of BibTeX styles with all
    %% possible inputs with keywords having each of the three possible
    %% string values is impossible.

    address
    annote                              % [22-Dec-1999] NHFB: new for version 5.0 [HIDDEN]
    author
    bibdate                             % [22-Dec-1999] NHFB: new for version 5.0 [HIDDEN]
    bibsource                           % [22-Dec-1999] NHFB: new for version 5.0 [HIDDEN]
    booktitle
    bookpages                           % [22-Dec-1999] NHFB: new for version 5.0
    chapter
    classcodes                          % [22-Dec-1999] NHFB: new for version 5.0 [HIDDEN]
    classification                      % [22-Dec-1999] NHFB: new for version 5.0 [HIDDEN]
    CODEN                               % [22-Dec-1999] NHFB: new for version 5.0
    day                                 % [22-Dec-1999] NHFB: new for version 5.0
    edition
    editor
    fjournal                            % [22-Dec-1999] NHFB: new for version 5.0 [HIDDEN]
    howpublished
    ISBN                                % [22-Dec-1999] NHFB: new for version 5.0
    ISSN                                % [22-Dec-1999] NHFB: new for version 5.0
    institution
    journal
    journalabr                          % [22-Dec-1999] NHFB: new for version 5.0 [HIDDEN]
    key
    keywords                            % [22-Dec-1999] NHFB: new for version 5.0 [HIDDEN]
    language                            % [22-Dec-1999] NHFB: new for version 5.0 [HIDDEN]
    LCCN                                % [22-Dec-1999] NHFB: new for version 5.0
    month
    MRclass                             % [22-Dec-1999] NHFB: new for version 5.0
    MRnumber                            % [22-Dec-1999] NHFB: new for version 5.0
    MRreviewer                          % [22-Dec-1999] NHFB: new for version 5.0
    note
    number
    organization
    originalref                         % [TEMPORARY] for tcbook checking
    pages
    price                               % [22-Dec-1999] NHFB: new for version 5.0
    publisher
    school
    series
    subject                             % [22-Dec-1999] NHFB: new for version 5.0 [HIDDEN]
    thesaurus                           % [22-Dec-1999] NHFB: new for version 5.0 [HIDDEN]
    title
    treatment                           % [22-Dec-1999] NHFB: new for version 5.0 [HIDDEN]
    type
    URL                                 % [22-Dec-1999] NHFB: new for version 5.0
    volume
    year
  }
  {}
  { label.year extra.label sort.year sort.label }


INTEGERS
{
  output.state                          % set to one of the following four
  before.all                            % enumeration value of output.state
  mid.sentence                          % enumeration value of output.state
  after.sentence                        % enumeration value of output.state
  after.block                           % enumeration value of output.state
}

STRINGS
{
  %% Here are all of the global string variables used in this style
  %% file.  You cannot add any more, because of this ridiculously small
  %% limit in standard BibTeX:
  %%
  %% ``Sorry---you've exceeded BibTeX's number of string global-variables 10''

  %% This global variable contains either "author" or "editor"; it is
  %% used in format.and.wrap.name, and set in begin.bib, format.authors,
  %% format.editors, and format.editors.familiar:
  author.or.editor

  last.label % shared between initialize.extra.label.stuff and forward.pass
  next.extra % shared between initialize.extra.label.stuff and reverse.pass

  %% These variables are temporary within functions:
  b
  e
  new.pages
  s
  t
  u

  w                                     % private for wrap.top
}


FUNCTION {and}
{
  %% Compute the Boolean AND of the top two stack arguments, replacing
  %% them by the result (0 or 1).
  %%
  %% At entry: stack = ... top-2:[some-value] top-1:[integer] top:[integer]
  %% At exit:  stack = ... top-1:[some-value] top:[0 or 1]

    { }
    { pop$ #0 }
  if$
}


FUNCTION {not}
{
  %% Replace the top stack element by its Boolean negation (0 ==
  %% false, 1 == true).  A zero is coerced to Boolean false, and a
  %% non-zero to Boolean true.
  %%
  %% At entry: stack = ... top-1:[some-value] top:[integer]
  %% At exit:  stack = ... top-1:[some-value] top:[0 or 1]

    { #0 }
    { #1 }
  if$
}


FUNCTION {or}
{
  %% Compute the Boolean OR of the top two stack arguments, replacing them
  %% by the result (0 or 1).
  %%
  %% At entry: stack = ... top-2:[some-value] top-1:[integer] top:[integer]
  %% At exit:  stack = ... top-1:[some-value] top:[0 or 1]

    { pop$ #1 }
    { }
  if$
}


FUNCTION {empty.or.unknown}
{
  %% Examine the top stack entry, and push 1 if it is empty, or
  %% consists only of whitespace, or is a string beginning with two
  %% queries (??), and otherwise, push 0.
  %%
  %% This function provides a replacement for empty$, with the
  %% convenient feature that unknown values marked by two leading
  %% queries are treated the same as missing values, and thus, do not
  %% appear in the output .bbl file, and yet, their presence in .bib
  %% file(s) serves to mark values which are temporarily missing, but
  %% are expected to be filled in eventually once more data is
  %% obtained.  The TeX User Group and BibNet bibliography archives
  %% make extensive use of this practice.
  %%
  %% An empty string cannot serve the same purpose, because just as in
  %% statistics data processing, an unknown value is not the same as an
  %% empty value.
  %%
  %% At entry: stack = ... top:[string]
  %% At exit:  stack = ... top:[0 or 1]

  duplicate$ empty$
    { pop$ #1 }
    { #1 #2 substring$ "??" = }
  if$
}


FUNCTION {wrap.top}
{
  %% At entry: stack = ... top-1:[keyvalue] top:[keyname]
  %% At exit:  stack = ... top:["\bblkeyname{keyvalue}"]
  %%
  %% Global variables set: w

  'w :=                                 % save keyname
  duplicate$ empty.or.unknown
    { "" }
    { "\bbl" w * "{" * swap$ * "}" * }
  if$
}


%%% ====================================================================
%%% Each of the wrap.xxx functions below pushes one new string onto
%%% the top of the stack.
%%%
%%% If the keyword's value is empty or unknown, an empty string is left
%%% on the stack.  Otherwise, the function wraps it, leaving
%%% "\bblkeyname{keyvalue}" on the stack.
%%%
%%% That way, the output .bbl file preserves all of the input markup,
%%% and can be used to reconstruct a .bib file (without, of course, any
%%% additional key/values that were ignored by this style file, and
%%% without any of the comments in the input .bib files, and also, with
%%% loss of all string abbreviation names).
%%%
%%% In summary:
%%%
%%% At entry: stack = ... top:[some-value]
%%% At exit:  stack = ... top-1:[some-value] top:["\bblkeyname{keyvalue}"]
%%%      or:  stack = ... top-1:[some-value] top:[""]
%%% ====================================================================


FUNCTION {wrap.address}
{
  address empty.or.unknown
    { "" }
    { address "address" wrap.top }
  if$
}


FUNCTION {wrap.annote}
{
  annote empty.or.unknown
    { "" }
    { annote "annote" wrap.top }
  if$
}


FUNCTION {wrap.author}
{
  author empty.or.unknown
    { "" }
    { author "author" wrap.top }
  if$
}


FUNCTION {wrap.bibdate}
{
  bibdate empty.or.unknown
    { "" }
    { bibdate "bibdate" wrap.top }
  if$
}


FUNCTION {wrap.bibsource}
{
  bibsource empty.or.unknown
    { "" }
    { bibsource "bibsource" wrap.top }
  if$
}


FUNCTION {wrap.bookpages}
{
  bookpages empty.or.unknown
    { "" }
    { bookpages "bookpages" wrap.top }
  if$
}


FUNCTION {wrap.booktitle}
{
  booktitle empty.or.unknown
    { "" }
    { booktitle "booktitle" wrap.top }
  if$
}


FUNCTION {wrap.chapter}
{
  chapter empty.or.unknown
    { "" }
    { chapter "chapter" wrap.top }
  if$
}


FUNCTION {wrap.cite$}
{
  cite$ empty.or.unknown
    { "" }
    { cite$ "cite" wrap.top }
  if$
}


FUNCTION {wrap.classcodes}
{
  classcodes empty.or.unknown
    { "" }
    { classcodes "classcodes" wrap.top }
  if$
}


FUNCTION {wrap.classification}
{
  classification empty.or.unknown
    { "" }
    { classification "classification" wrap.top }
  if$
}


FUNCTION {wrap.CODEN}
{
  CODEN empty.or.unknown
    { "" }
    { CODEN "CODEN" wrap.top }
  if$
}


FUNCTION {wrap.crossref}
{
  crossref empty.or.unknown
    { "" }
    { crossref "crossref" wrap.top }
  if$
}


FUNCTION {wrap.day}
{
  day empty.or.unknown
    { "" }
    { day "day" wrap.top }
  if$
}


FUNCTION {wrap.edition}
{
  edition empty.or.unknown
    { "" }
    { edition "edition" wrap.top }
  if$
}


FUNCTION {wrap.editor}
{
  editor empty.or.unknown
    { "" }
    { editor "editor" wrap.top }
  if$
}


FUNCTION {wrap.fjournal}
{
  fjournal empty.or.unknown
    { "" }
    { fjournal "fjournal" wrap.top }
  if$
}


FUNCTION {wrap.howpublished}
{
  howpublished empty.or.unknown
    { "" }
    { howpublished "howpublished" wrap.top }
  if$
}


FUNCTION {wrap.institution}
{
  institution empty.or.unknown
    { "" }
    { institution "institution" wrap.top }
  if$
}


FUNCTION {wrap.ISBN}
{
  ISBN empty.or.unknown
    { "" }
    { ISBN "ISBN" wrap.top }
  if$
}


FUNCTION {wrap.ISSN}
{
  ISSN empty.or.unknown
    { "" }
    { ISSN "ISSN" wrap.top }
  if$
}


FUNCTION {wrap.journal}
{
  journal empty.or.unknown
    { "" }
    { journal "journal" wrap.top }
  if$
}


FUNCTION {wrap.journalabr}
{
  journalabr empty.or.unknown
    { "" }
    { journalabr "journalabr" wrap.top }
  if$
}


FUNCTION {wrap.key}
{
  key empty.or.unknown
    { "" }
    { key "key" wrap.top }
  if$
}


FUNCTION {wrap.keywords}
{
  keywords empty.or.unknown
    { "" }
    { keywords "keywords" wrap.top }
  if$
}


FUNCTION {wrap.language}
{
  language empty.or.unknown
    { "" }
    { language "language" wrap.top }
  if$
}


FUNCTION {wrap.LCCN}
{
  LCCN empty.or.unknown
    { "" }
    { LCCN "LCCN" wrap.top }
  if$
}


FUNCTION {wrap.month}
{
  month empty.or.unknown
    { "" }
    { month "month" wrap.top }
  if$
}


FUNCTION {wrap.MRclass}
{
  MRclass empty.or.unknown
    { "" }
    { MRclass "MRclass" wrap.top }
  if$
}


FUNCTION {wrap.MRnumber}
{
  MRnumber empty.or.unknown
    { "" }
    { MRnumber "MRnumber" wrap.top }
  if$
}


FUNCTION {wrap.MRreviewer}
{
  MRreviewer empty.or.unknown
    { "" }
    { MRreviewer "MRreviewer" wrap.top }
  if$
}


FUNCTION {wrap.note}
{
  note empty.or.unknown
    { "" }
    { note "note" wrap.top }
  if$
}


FUNCTION {wrap.number}
{
  number empty.or.unknown
    { "" }
    { number "number" wrap.top }
  if$
}


FUNCTION {wrap.organization}
{
  organization empty.or.unknown
    { "" }
    { organization "organization" wrap.top }
  if$
}


FUNCTION {wrap.pages}
{
  pages empty.or.unknown
    { "" }
    { pages "pages" wrap.top }
  if$
}


FUNCTION {wrap.price}
{
  price empty.or.unknown
    { "" }
    { price "price" wrap.top }
  if$
}


FUNCTION {wrap.publisher}
{
  publisher empty.or.unknown
    { "" }
    { publisher "publisher" wrap.top }
  if$
}


FUNCTION {wrap.school}
{
  school empty.or.unknown
    { "" }
    { school "school" wrap.top }
  if$
}


FUNCTION {wrap.series}
{
  series empty.or.unknown
    { "" }
    { series "series" wrap.top }
  if$
}


FUNCTION {wrap.subject}
{
  subject empty.or.unknown
    { "" }
    { subject "subject" wrap.top }
  if$
}


FUNCTION {wrap.thesaurus}
{
  thesaurus empty.or.unknown
    { "" }
    { thesaurus "thesaurus" wrap.top }
  if$
}


FUNCTION {wrap.title}
{
  title empty.or.unknown
    { "" }
    { title "title" wrap.top }
  if$
}


FUNCTION {wrap.treatment}
{
  treatment empty.or.unknown
    { "" }
    { treatment "treatment" wrap.top }
  if$
}


FUNCTION {wrap.type}
{
  type empty.or.unknown
    { "" }
    { type "type" wrap.top }
  if$
}


FUNCTION {wrap.type$}
{
  type$ empty.or.unknown
    { "" }
    { type$ "entry" wrap.top }
  if$
}


FUNCTION {wrap.volume}
{
  volume empty.or.unknown
    { "" }
    { volume "volume" wrap.top }
  if$
}


FUNCTION {wrap.year}
{
  year empty.or.unknown
    { "" }
    { year "year" wrap.top }
  if$
}


FUNCTION {format.and.wrap.name}
{
  %% Format and wrap an author/editor name.
  %%
  %% At entry: stack = ... top:[name]
  %% At exit:  stack = ... top:[formatted and wrapped name]

  format.name$ author.or.editor wrap.top
}


FUNCTION {init.state.consts}
{
  %% Initialize output.state enumeration constants.
  %%
  %% At entry: stack = ... top:[top-string]
  %% At exit:  stack = ... top:[top-string]
  %%
  %% Global variables set: before.all, mid.sentence, after.sentence, after.block

  #0 'before.all :=
  #1 'mid.sentence :=
  #2 'after.sentence :=
  #3 'after.block :=
}


FUNCTION {output.nonnull}
{
  %% Write the top-1 stack entry to the output file, with any needed
  %% following punctuation (determined by the value of output.state),
  %% and possibly, a newline and \newblock command, and remove it from
  %% the stack.
  %%
  %% At entry: stack = ... top-2:[top-2 string] top-1:[string] top:[string]
  %% At exit:  stack = ... top-1:[top-2 string] top:[string],
  %% and [top-1 string] has been written out, possibly with trailing
  %% punctuation determined by the value of output.state.
  %%
  %% Global variables set: output.state, s

  's :=                                 % save current top-of-stack
  output.state mid.sentence =
    { ", " * }
    {
      output.state after.block =
        {
          add.period$ write$
          newline$
          "\newblock "
        }
        {
          output.state before.all =
            { }
            { add.period$ " " * }
          if$
        }
      if$
      mid.sentence 'output.state :=
    }
  if$
  write$
  s                                     % restore incoming top-of-stack
}


FUNCTION {output.nonnull.colon}
{
  %% Use a colon to separate output. Used only for address/publisher
  %% combination in book/inbook/incollection/inproceedings/proceedings
  %% types, address/organization for manuals.
  %%
  %% At entry: stack = ... top-2:[top-2 string] top-1:[string] top:[string]
  %% At exit:  stack = ... top-1:[top-2 string] top:[string],
  %% and [top-1 string] has been written out, possibly with a trailing
  %% colon.
  %%
  %% Global variables set: output.state, s

  's :=
  output.state mid.sentence =
    { ": " * write$ }
    {
      output.state after.block =
        {
          add.period$ write$
          newline$
          "\newblock " write$
        }
        {
          output.state before.all =
            { write$ }
            { add.period$ " " * write$ }
          if$
        }
      if$
      mid.sentence 'output.state :=
    }
  if$
  s
}


FUNCTION {output}
{
  %% If the string at top-of-stack is empty or unknown, discard it.
  %% Otherwise, output the string at stack position top-1, followed by
  %% any needed punctuation, and remove it from the stack, leaving the
  %% initial top-of-stack unchanged on exit, except that the stack
  %% pointer has reduced by 1.
  %%
  %% At entry: stack = ... top-1:[string] top:[top-string]
  %% At exit:  stack = ... top:[top-string]
  duplicate$ empty.or.unknown
    { pop$ }
    { output.nonnull }
  if$
}


FUNCTION {output.check}
{
  %% Output a string, issuing a warning if it is empty or unknown.
  %%
  %% Because string output is delayed so that proper end-of-sentence
  %% or end-of-block punctuation can be supplied, it is actually the
  %% PREVIOUS stacked string, at stack slot top-2, that is output. The
  %% new string takes its place, with the stack top reduced by 2.
  %%
  %% At entry: stack = ... top-2:[some-string] top-1:[string-value] top:[string-name]
  %% At exit:  stack = ... top:[string-value]
  %%
  %% Global variables set: t

  't :=
  duplicate$ empty.or.unknown
    { pop$ "empty " t * " in " * cite$ * warning$ }
    { output.nonnull }
  if$
}


FUNCTION {output.colon}
{
  %% If the string at top-of-stack is empty or unknown, discard it
  %% silently.
  %%
  %% Otherwise, output the string at stack position top-1, followed by a
  %% colon (instead of the normal period), and remove it from the stack,
  %% leaving the initial top-of-stack unchanged on exit, except that the
  %% stack pointer has reduced by 1.
  %%
  %% At entry: stack = ... top-1:[string] top:[top-string]
  %% At exit:  stack = ... top:[top-string]
  duplicate$ empty.or.unknown
    { pop$ }
    { output.nonnull.colon }
  if$
}


FUNCTION {output.check.colon}
{
  %% If the string at top-of-stack is empty or unknown, discard it, and
  %% warn that it was empty.
  %%
  %% Otherwise, output the string at stack position top-1, followed by a
  %% colon (instead of the normal period), and remove it from the stack,
  %% leaving the initial top-of-stack unchanged on exit, except that the
  %% stack pointer has reduced by 1.
  %%
  %% At entry: stack = ... top-1:[string] top:[top-string]
  %% At exit:  stack = ... top:[top-string]

  't :=
  duplicate$ empty.or.unknown
    { pop$ "empty " t * " in " * cite$ * warning$ }
    { output.nonnull.colon }
  if$
}


FUNCTION {output.year.month.day.check}
{
  %% Write the current top-of-stack immediately with no trailing
  %% punctuation, removing it from the stack. Then push the wrapped year
  %% on the stack, warning if it is empty or unknown.  Append to it a
  %% distinguishing suffix letter if needed.  Then append a space and
  %% the wrapped month and wrapped day, if available, but do not warn if
  %% they are empty or unknown, unless of (year, month, day), a
  %% missing-or-unknown one is followed by a non-missing-or-unknown one.
  %%
  %% At entry: stack = ... top:[string]
  %% At exit:  stack = ... top:[" wrapped-year-suffix, wrapped-month, wrapped-day"]
  %%      or:  stack = ... top:[" wrapped-year-suffix, wrapped-month"]
  %%      or:  stack = ... top:[" wrapped-year-suffix"]
  %%
  %% Global variables set: output.state

  year empty.or.unknown
    { "empty year in " cite$ * warning$ }
    {
      write$                            % write top-of-stack immediately

      %% [18-Dec-1999] NHFB: The 14th edition of the Chicago Manual
      %% of Style says years appear without surrounding parentheses:
      %% " (" year * extra.label *
      %% month empty.or.unknown
      %%   { ")" * }
      %%   { ", " * month * ")" * }
      %% if$

      " " wrap.year * extra.label *
      month empty.or.unknown
        { }
        { ", " * wrap.month * }
      if$
      day empty.or.unknown              % [20-Dec-1999] NHFB: new
        {  }
        {
            month empty.or.unknown
                { "there's a day but no month in " cite$ * warning$ }
                { }
            if$
            " " * wrap.day *
        }
      if$
      mid.sentence 'output.state :=
    }
  if$
}


%%% ====================================================================
%%% This section, which extends to just before the FUNCTION {fin.entry}
%%% definition, adds support for bibdate, bibsource, bookpages, CODEN,
%%% day, fjournal, ISBN, ISSN, LCCN, MRclass, MRnumber, MRreviewer,
%%% price, and URL key/value pairs.
%%%
%%% We handle them differently than all other entry types for the
%%% following reason.  It must be easy for users to control the
%%% appearance of these values (though not their relative position in
%%% the .bbl file entries), without the need for users to modify this
%%% BibTeX style file, because such modifications require a substantial
%%% investment in learning time that few users are willing to make, and
%%% only produce additional minor style variants that increase the
%%% already burdensome BibTeX style file maintenance task.
%%%
%%% Since these new fields have not been conventional in
%%% manually-produced bibliographies, there are few guidelines about
%%% where in the .bbl file entry they should be typeset.
%%%
%%% Correspondence with developers at the American Mathematical Society
%%% in November 1999 revealed that the AMS has begun the practice of
%%% typesetting MRclass, MRreviewer, and MRnumber data as follows.
%%% Given a BibTeX entry (from the AMS MathSciNet database) of the form
%%%
%%%   @article {MR98h:05132,
%%%      AUTHOR = {Knuth, Donald E.},
%%%       TITLE = {Aztec diamonds, checkerboard graphs, and spanning trees},
%%%     JOURNAL = {J. Algebraic Combin.},
%%%    FJOURNAL = {Journal of Algebraic Combinatorics. An International Journal},
%%%      VOLUME = {6},
%%%        YEAR = {1997},
%%%      NUMBER = {3},
%%%       PAGES = {253--257},
%%%        ISSN = {0925-9899},
%%%       CODEN = {JAOME7},
%%%     MRCLASS = {05C50 (05A18)},
%%%    MRNUMBER = {98h:05132},
%%%  MRREVIEWER = {D. Cvetkovic},
%%%   }
%%%
%%% AMS journals would produce a .bbl file entry like this:
%%%
%%%      D. E. Knuth, Aztec diamonds, checkerboard graphs, and spanning
%%%      trees, J. Algebraic Combin. {\bf 6} (1997), no.~3,
%%%      253--257. (D. Cvetkovic) {\bf MR~98h:05132} 05C50 (05A18).
%%%
%%% That is, the additional material follows the conventional entry,
%%% which is standardly ended by a period.  The Math Reviews reviewer
%%% goes in parentheses, then the Math Reviews number in bold, followed
%%% by the Math Reviews primary and secondary classification in normal
%%% text.
%%%
%%% This suggests that the appropriate place in BibTeX style files to
%%% typeset this additional material is in the fin.entry function, which
%%% has the job of finishing off each entry.  That way, the individual
%%% functions for each document entry type do not need any modification
%%% at all.  Indeed, support for this additional material could be
%%% automatically added to the hundreds of available BibTeX style files
%%% with a simple edit insertion.
%%%
%%% The functions in this section follow a different naming convention
%%% than is usual for the key/value functions: instead of being called
%%% format.KEYNAME, they are called write.KEYNAME.  The former leave
%%% data on the stack for later output, while the latter do immediate
%%% output, so the name change is both desirable, and significant.
%%%
%%% We implement the design requirement of TeX-time user control over
%%% whether or not the extended keywords are typeset from .bbl entries
%%% by outputting each of the additional key values wrapped in a TeX
%%% macro that can be easily redefined by the user as needed, even to
%%% make the macro discard its argument entirely.
%%%
%%% The originally design used for several years in NHFB's
%%% is-abbrv.bst, is-alpha.bst, is-plain.bst, and is-unsrt.bst
%%% produced output sequences of
%%%
%%%     \newblock \showCODEN{...}.
%%%     \newblock \showISSN{...}.
%%%     ...
%%%
%%% This is satisfactory as long as the \showXXX{...} macros typeset
%%% their arguments.  However, when the arguments are to be discarded,
%%% the user has to redefine them as TeX delimited argument macros with
%%% an empty expansion, or as LaTeX two-argument macros that discard
%%% both arguments.
%%%
%%% Doing so has an unpleasant side effect, because the default
%%% definition of \newblock is either `\par' (for the openbib style), or
%%% `\hskip .11em plus .33em minus .07em' (for the default style).
%%% Thus, a sequence of \newblock macros will produce unwanted
%%% horizontal space.
%%%
%%% While expert TeX users might discover that they could define these
%%% macros like this:
%%%
%%%     \def \showCODEN #1.{\unskip}
%%%
%%% LaTeX users are not expected to know such low-level details, and in
%%% any event, such a definition could be broken by a change to the
%%% definition of \newblock.  This is a fragile, and therefore,
%%% unacceptable, solution.
%%%
%%% In this new design, we write out all of the non-empty extra
%%% supported fields, each preceded by a newline, and wrapped inside a
%%% macro, with no intervening punctuation (the macros must supply any
%%% needed punctuation).  The stack contents are not modified.  After
%%% the last call to `write.xxx' below in write.extra.fields, there
%%% will be a newline missing.  It will be provided either by a
%%% subsequent `output' call, or by `fin.entry'.
%%%
%%% In summary:
%%%
%%% At entry: stack = ... top:[top-value]
%%% At exit:  stack = ... top:[top-value]  (i.e., unchanged)
%%% ====================================================================


FUNCTION {format.MRclass}
{
  %% Push a wrapped MRclass value onto the stack, or an empty string
  %% if MRclass is empty or unknown.
  %%
  %% At entry: stack = ... top:[top-string]
  %% At exit:  stack = ... top-1:[top-string] top:["\bblMRclass{MRclass-value}"]
  %%      or:  stack = ... top-1:[top-string] top:[""]

  MRclass empty.or.unknown
    { "" }
    { wrap.MRclass }
  if$
}


FUNCTION {format.MRnumber}
{
  %% Push a wrapped MRnumber value onto the stack, or an empty string
  %% if MRnumber is empty or unknown.
  %%
  %% At entry: stack = ... top:[top-string]
  %% At exit:  stack = ... top-1:[top-string] top:["\bblMRnumber{MRnumber-value}"]
  %%      or:  stack = ... top-1:[top-string] top:[""]

  MRnumber empty.or.unknown
    { "" }
    { wrap.MRnumber }
  if$
}


FUNCTION {format.MRreviewer}
{
  %% Push a wrapped MRreviewer value onto the stack, or an empty string
  %% if MRreviewer is empty or unknown.
  %%
  %% At entry: stack = ... top:[top-string]
  %% At exit:  stack = ... top-1:[top-string] top:["\bblMRreviewer{MRreviewer-value}"]
  %%      or:  stack = ... top-1:[top-string] top:[""]

  MRreviewer empty.or.unknown
    { "" }
    { wrap.MRreviewer }
  if$
}


INTEGERS { n }                % private `local' variable for pathify only


FUNCTION {pathify}
{
  %% Wrap "\path|...|" around each semicolon- or space-separated
  %% substring of the top-of-stack string, replacing that string with
  %% the result.  If a semicolon separator is found, any following space
  %% is collapsed to a single space.
  %%
  %% At entry: stack = ... top:["s1 s2 s3 ... sn"]
  %%       or: stack = ... top:["s1; s2; s3; ... sn"]
  %% At exit:  stack = ... top:["\path|s1| \path|s2| \path|s3| ... \path|sn|"]
  %%      or:  stack = ... top:["\path|s1|; \path|s2|; \path|s3|; ... \path|sn|"]
  %%
  %% Global variables set: n, t

  duplicate$ empty.or.unknown
    { pop$ "" }
    {
      't :=                             % save top-of-stack string in t
      #1 'n :=                          % n = 1 is initial index into t[]
      "\path|"                          % push initial output string prefix

      %% The scan of the string is not robust, because BibTeX does not
      %% have a length$ primitive to get a true string length. The
      %% best it offers is text.length$, which ignores braces, and
      %% reduces backslashed-accents.  However, URLs should not
      %% contain either, so the while-loop test should work in practice,
      %% and not cause premature termination.

        {                               % while-loop Boolean condition
          t text.length$ #0 >
          n global.max$ <
          and
        }
        {                               % while-loop body
          t n #1 substring$ ";" =       % if (t[n] == ";")
            {                           % then can end current \path|...| here
              t #1 n #1 - substring$ * "|; \path|" * % form "t[1:n-1]|; \path|"
              t n #1 + global.max$ substring$ 't := % reset t = t[n+1:*]
              #1 'n :=                  % reset n = 1

              %% discard all spaces after semicolon
                {                       % while-loop Boolean condition
                  t n #1 substring$ " " =
                  n global.max$ <
                  and                   % (t[n] == " ") && (n < global.max$)
                }
                {                       % while-loop body
                  n #1 + 'n :=          % n = n + 1
                }
              while$

              %% At this point, the remaining string in t[n:*] begins
              %% with a character other than space.
              t n global.max$ substring$ 't := % reset t = t[n:*]
              #1 'n :=                  % n = 1
            }
            {                           % else (t[n] != ";")
              t n #1 substring$ " " =   % if (t[n] == " ")
                {                       % then create "... \path|...| \path|..." on stack
                  t #1 n #1 - substring$ * "| \path|" *
                  t n #1 + global.max$ substring$ 't := % reset t = t[n+1:*]
                  #1 'n :=              % reset n = 1
                }
                { n #1 + 'n := }        % else n = n + 1
              if$
            }
          if$
        }
      while$
      t * "|" *                             % finish off last "\path|...|"
    }
  if$
}



FUNCTION {write.annote}
{
  annote empty.or.unknown
    { }
    { newline$ "\showannote{" wrap.annote * "}" * write$ }
  if$
}


FUNCTION {write.bibdate}
{
  bibdate empty.or.unknown
    { }
    { newline$ "\showbibdate{" wrap.bibdate * "}" * write$ }
  if$
}


FUNCTION {write.bibsource}
{
  bibsource empty.or.unknown
    { }
    { newline$ "\showbibsource{" wrap.bibsource * "}" * write$ }
  if$
}


FUNCTION {write.classcodes}
{
  classcodes empty.or.unknown
    { }
    { newline$ "\showclasscodes{" wrap.classcodes * "}" * write$ }
  if$
}


FUNCTION {write.classification}
{
  classification empty.or.unknown
    { }
    { newline$ "\showclassification{" wrap.classification * "}" * write$ }
  if$
}


FUNCTION {write.CODEN}
{
  CODEN empty.or.unknown
    { }
    { newline$ "\showCODEN{" wrap.CODEN * "}" * write$ }
  if$
}


FUNCTION {write.fjournal}
{
  fjournal empty.or.unknown
    { }
    { newline$ "\showfjournal{" wrap.fjournal * "}" * write$ }
  if$
}


FUNCTION {write.ISBN}
{
  ISBN empty.or.unknown
    { }
    { newline$ "\showISBN{" wrap.ISBN * "}" * write$ }
  if$
}


FUNCTION {write.ISSN}
{
  ISSN empty.or.unknown
    { }
    { newline$ "\showISSN{" wrap.ISSN * "}" * write$ }
  if$
}


FUNCTION {write.journalabr}
{
  journalabr empty.or.unknown
    { }
    { newline$ "\showjournalabr{" wrap.journalabr * "}" * write$ }
  if$
}


FUNCTION {write.keywords}
{
  keywords empty.or.unknown
    { }
    { newline$ "\showkeywords{" wrap.keywords * "}" * write$ }
  if$
}


FUNCTION {write.language}
{
  language empty.or.unknown
    { }
    { newline$ "\showlanguage{" wrap.language * "}" * write$ }
  if$
}


FUNCTION {write.LCCN}
{
  LCCN empty.or.unknown
    { }
    { newline$ "\showLCCN{" wrap.LCCN * "}" * write$ }
  if$
}


FUNCTION {write.MathReviews}
{
  %% Develop and write a string that looks like
  %%
  %% \showMR{\showMRREVIEWER{...}}{\showMRNUMBER{...}}{\showMRclass{...}}
  %%
  %% However, because of BibTeX's nasty gratuitous line wrapping with
  %% percent-newline, we don't create this as one long line, but rather
  %% as five separate lines.  We precompute all three value strings and
  %% store them in temporary (global) variables, so that we can measure
  %% their cumulative length, and skip output entirely if they are all
  %% empty.
  %%
  %% At entry: stack = ... top:[top-string]
  %% At exit:  stack = ... top:[top-string]
  %%
  %% Global variables set: s t u

  format.MRreviewer 's :=
  format.MRnumber   't :=
  format.MRclass    'u :=

  u text.length$ t text.length$ + s text.length$ + #0 >
    {
      newline$
      "\showMR"   write$ newline$
      "       {" s * "}%" * write$ newline$
      "       {" t * "}%" * write$ newline$
      "       {" u * "}" * write$
    }
    { }
  if$
}


FUNCTION {write.price}
{
  price empty.or.unknown
    { }
    { newline$ "\showprice{" wrap.price * "}" * write$ }
  if$
}


FUNCTION {write.subject}
{
  subject empty.or.unknown
    { }
    { newline$ "\showsubject{" wrap.subject * "}" * write$ }
  if$
}


FUNCTION {write.thesaurus}
{
  thesaurus empty.or.unknown
    { }
    { newline$ "\showthesaurus{" wrap.thesaurus * "}" * write$ }
  if$
}


FUNCTION {write.treatment}
{
  treatment empty.or.unknown
    { }
    { newline$ "\showtreatment{" wrap.treatment * "}" * write$ }
  if$
}


FUNCTION {write.URL}
{
  URL empty.or.unknown
    { }
    % OLD: { newline$ "\ifshowURL {\newblock {\showURL \path|" URL * "|}}\fi" * write$ }
    { newline$ "\ifshowURL {\newblock {\showURL " URL pathify * "}}\fi" * write$ }
  if$
}


FUNCTION {write.extra.fields}
{
  %% This function is normally invoked from only a single place in the
  %% BibTeX style file: the start of fin.entry.  Thus, the new
  %% features can be disabled by commenting out that single call, or
  %% better, by redefining the outer wrapper macro like this:
  %%
  %%     \def \showEXTRA #1{}
  %%
  %% At entry: stack = ... top:[top-value]
  %% At exit:  stack = ... top-1:[top-value] top:[""]

  %% Start out with the same code used in fin.entry to complete the
  %% current entry, omitting the newline$ call:

  add.period$
  write$

  newline$ "\showEXTRA{%" write$

  %% Now write all of the additional values; the functions expect
  %% nothing on the stack, and leave nothing there either.
  write.fjournal
  write.CODEN                           % keep CODEN and ISSN together because
  write.ISSN                            % they both identify a periodical
  write.ISBN                            % keep ISBN and LCCN together because
  write.LCCN                            % they are useful in library catalogs
  write.MathReviews
  write.price
  write.bibsource
  write.bibdate

  %% These values are not displayed by default, but are included because
  %% they are of use in converting the .bbl file back to BibTeX (and
  %% other bibliography database) formats, and for searching of online
  %% document text.
  write.annote
  write.classcodes
  write.classification
  write.journalabr
  write.keywords
  write.language
  write.subject
  write.thesaurus
  write.treatment

  %% Finish up with the URL last, because it is long and distinctive
  write.URL

  %% For tcbook checking:
  originalref empty.or.unknown
      { }
      { newline$ "\showoriginalref{" originalref * "}" * write$ }
  if$

  newline$ "}" write$                   % end of \showEXTRA

  %% Finally, fin.entry expects to find ONE string on the stack, so
  %% leave it one that will not cause addition of a final period.
  ""
}

%%% ====================================================================

FUNCTION {fin.entry}
{
  %% Finish off an entry by writing the extra fields, any needed
  %% punctuation, and a newline.  On return, the BibTeX output buffer
  %% should be empty, and the output .bbl file should end with a
  %% non-blank line followed by an end-of-line character, character
  %% sequence, or state.
  %%
  %% At entry: stack = empty
  %% At exit:  stack = empty

  write.extra.fields                    % this supplies all of the new fields
  add.period$ write$
  newline$
}


FUNCTION {new.block}
{
  %% Update the output.state to indicate that we are either in state
  %% before.all (after the \bibitem, but before any other data), or in
  %% state after.block, ready to start a new block.  A new block is
  %% later identified by a \newblock macro, which expands to a bit of
  %% horizontal space (default closebib style), or to a paragraph
  %% break (openbib style).
  %%
  %% At entry: stack = ... top:[top-string]
  %% At exit:  stack = ... top:[top-string]
  %%
  %% Global variables set: output.state

  output.state before.all =
    { }
    { after.block 'output.state := }
  if$
}


FUNCTION {new.sentence}
{
  %% Update the output.state to indicate that we are either in state
  %% before.all (after the \bibitem, but before any other data), or in
  %% state after.sentence, ready to start a new sentence or new block.
  %%
  %% At entry: stack = ... top:[top-string]
  %% At exit:  stack = ... top:[top-string]
  %%
  %% Global variables set: output.state

  output.state after.block =
    { }
    {
      output.state before.all =
        { }
        { after.sentence 'output.state := }
      if$
    }
  if$
}


FUNCTION {new.block.checka}
{
  %% Pop the top-of-stack.  If it was empty or unknown, start a new
  %% block in the .bbl file.
  %%
  %% At entry: stack = ... top-1:[top-1-string] top:[top-string]
  %% At exit:  stack = ... top:[top-1 string]

  empty.or.unknown
    { }
    { new.block }
  if$
}


FUNCTION {new.block.checkb}
{
  %% Pop the top two elements.  If both were empty or unknown, start a
  %% new block in the .bbl file.
  %%
  %% At entry: stack = ... top-2:[top-2-string] top-1:[top-1-string] top:[top-string]
  %% At exit:  stack = ... top:[top-2-string]

  empty.or.unknown
  swap$ empty.or.unknown
  and
    { }
    { new.block }
  if$
}


FUNCTION {new.sentence.checka}
{
  %% Pop the top-of-stack.  If it was empty or unknown, start a new
  %% sentence in the .bbl file.
  %%
  %% At entry: stack = ... top-1:[top-1-string] top:[top-string]
  %% At exit:  stack = ... top:[top-1 string]

  empty.or.unknown
    { }
    { new.sentence }
  if$
}


FUNCTION {new.sentence.checkb}
{
  %% Pop the top two elements.  If both were empty or unknown, start a
  %% new sentence in the .bbl file.
  %%
  %% At entry: stack = ... top-2:[top-2-string] top-1:[top-1-string] top:[top-string]
  %% At exit:  stack = ... top:[top-2-string]

  empty.or.unknown
  swap$ empty.or.unknown
  and
    { }
    { new.sentence }
  if$
}


FUNCTION {field.or.null}
{
  %% If the top stack element is empty or unknown, replace it by a
  %% null string. Otherwise, leave it alone.
  %%
  %% At entry: stack = ... top:[string]
  %% At exit:  stack = ... top:[string]
  %%      or:  stack = ... top:[""]

  duplicate$ empty.or.unknown
    { pop$ "" }
    { }
  if$
}


FUNCTION {emphasize.without.italic.correction}
{
  %% Emphasize the top string on the stack, WITHOUT an italic
  %% correction (because a period or comma will follow).
  %%
  %% At entry: stack = ... top:["string"]
  %% At exit:  stack = ... top:["{\em string}"]

  duplicate$ empty.or.unknown
    { pop$ "" }
    { "{\em " swap$ * "}" * }
  if$
}


FUNCTION {emphasize.with.italic.correction}
{
  %% Emphasize the top string on the stack, WITH an italic correction
  %% (because something other than a period or comma will follow).
  %%
  %% At entry: stack = ... top:["string"]
  %% At exit:  stack = ... top:["{\em string\/}"]

  duplicate$ empty.or.unknown
    { pop$ "" }
    { "{\em " swap$ * "\/}" * }
  if$
}


INTEGERS { nameptr namesleft numnames }


FUNCTION {format.names}
{
  %% Format bibliographical entries with the first author last name
  %% first, and subsequent authors with initials followed by last name.
  %% All names are formatted in this routine.
  %%
  %% At entry: stack = ... top:[name1 and name2 and name3 and ...]
  %% At exit:  stack = ... top:[list of formatted names]
  %%
  %% Global variables set: nameptr, namesleft, numnames, s, t

  's :=                                 % save the name list
  #1 'nameptr :=                        % nameptr = 1;
  s num.names$ 'numnames :=             % numnames = num.name$(s);
  numnames 'namesleft :=
    { namesleft #0 > }                  % while-loop Boolean condition
    {                                   % while-loop body
      nameptr #1 =
        {s nameptr "{vv~}{ll}{, jj}{, f.}" format.and.wrap.name 't := }
        {s nameptr "{f.~}{vv~}{ll}{, jj}" format.and.wrap.name 't := }
      if$
      nameptr #1 >
        { namesleft #1 >
              { ", " * t * }
              { numnames #2 >
                  { "," * }
                  { }
                if$
                t "others" =
                    { " et~al." * }
                    { " and " * t * } % from Chicago Manual of Style
                  if$
               }
               if$
             }
            't
        if$
        nameptr #1 + 'nameptr :=        % nameptr += 1;
        namesleft #1 - 'namesleft :=    % namesleft =- 1;
    }
  while$
}


FUNCTION {my.full.label}
{
  %% Given a list of author or editor names at stack top, replace it by
  %% a shorter list for use in in-text citations.
  %%
  %% At entry: stack = ... top:[name1 and name2 and name3 and ...]
  %% At exit:  stack = ... top:[name1, name2, et al]
  %%
  %% Global variables set: nameptr, numnames, namesleft, s, t

  's :=                                 % save the name list
  #1 'nameptr :=                        % nameptr = 1;
  s num.names$ 'numnames :=             % numnames = num.name$(s);
  numnames 'namesleft :=
    { namesleft #0 > }                  % while-loop Boolean condition
    {                                   % while-loop body
      s nameptr "{vv~}{ll}" format.name$ 't :=  % get the next name
      nameptr #1 >
        { namesleft #1 >
              { ", " * t * }
              { numnames #2 >
                  { "," * }
                  { }
                if$
                t "others" =
                    { " et~al." * }
                    { " and " * t * }   % from Chicago Manual of Style
                  if$
               }
               if$
             }
            't
        if$
        nameptr #1 + 'nameptr :=        % nameptr += 1;
        namesleft #1 - 'namesleft :=    % namesleft =- 1;
    }
  while$

}


FUNCTION {format.names.familiar}
{
  %% Format names in `familiar' format, with first initial followed by
  %% last name. Like format.names, ALL names are formatted.
  %%
  %% At entry: stack = ... top:[name1 and name2 and name3 and ...]
  %% At exit:  stack = ... top:[wrapped-name1 and wrapped-name2 and wrapped-name3 and ...]
  %%
  %% Global variables set: nameptr, numnames, namesleft, s, t

  's :=                                 % save the name list
  #1 'nameptr :=                        % nameptr = 1;
  s num.names$ 'numnames :=             % numnames = num.name$(s);
  numnames 'namesleft :=
    { namesleft #0 > }                  % while-loop Boolean condition
    {                                   % while-loop body
      s nameptr "{f.~}{vv~}{ll}{, jj}" format.and.wrap.name 't :=
      nameptr #1 >
        { namesleft #1 >
              { ", " * t * }
               { numnames #2 >
                    { "," * }
                    { }
                  if$
                  t "others" =
                        { " et~al." * }
                        { " and " * t * } % formerly " \& " instead of " and "
                      if$
                }
               if$
             }
            't
        if$
        nameptr #1 + 'nameptr :=        % nameptr += 1;
        namesleft #1 - 'namesleft :=    % namesleft =- 1;
    }
  while$
}


FUNCTION {format.note}
{
  %% Format a note for typesetting.
  %%
  %% At entry: stack = ... top:[note]
  %% At exit:  stack = ... top:[formatted and wrapped note]

  wrap.note
}

FUNCTION {format.organization}
{
  %% Format an organization for typesetting.
  %%
  %% At entry: stack = ... top:[organization]
  %% At exit:  stack = ... top:[formatted and wrapped organization]

  wrap.organization
}

FUNCTION {format.authors}
{
  %% Format an author list for typesetting.  An empty or unknown list is
  %% converted to a null string.
  %%
  %% At entry: stack = ... top:[name1 and name2 and name3 and ...]
  %% At exit:  stack = ... top:[formatted and wrapped author list]
  %%      or:  stack = ... top:[""]
  %%
  %% Global variables set: author.or.editor

  author empty.or.unknown
    { "" }
    {
      "author" 'author.or.editor :=
      author format.names
    }
  if$
}


FUNCTION {format.bookpages}
{
  %% [16-Dec-1999] New keyword, bookpages, for recording the number of
  %% pages in the full volume in a @InBook{}, @InCollection{}, or
  %% @InProceedings{} entry.  It is formatted following the pages
  %% value like this: pages (of bookpages).  For example,
  %% "302--347 (of xxxiv + 971)".
  %%
  %% At entry: stack = ... top:[bookpages]
  %% At exit:  stack = ... top:[wrapped-bookpages]

  bookpages empty.or.unknown
    { "" }
    { " (of " wrap.bookpages * ")" * }
  if$
}


FUNCTION {format.book.pages}
{
  %% Format a list of book pages for typesetting.  An empty or unknown
  %% list is converted to a null string.
  %%
  %% At entry: stack = ... top:[book-pages]
  %% At exit:  stack = ... top:[wrapped-book-pages]

  pages empty.or.unknown
    { "" }
    { new.block wrap.pages " pp." * }
  if$
}


FUNCTION {format.date}
{
  %% Format a date (day, month, year) for typesetting.  Warnings are
  %% issued if these values are inconsistent.
  %%
  %% At entry: stack = ... top:[top-string]
  %% At exit:  stack = ... top-1:[top-string] ["wrapped-month wrapped-day, wrapped-year"]
  %%      or:  stack = ... top-1:[top-string] ["wrapped-month, wrapped-year"]
  %%      or:  stack = ... top-1:[top-string] ["wrapped-year"]
  %%      or:  stack = ... top-1:[top-string] [""]

  year empty.or.unknown
    { month empty.or.unknown
        {
          day empty.or.unknown
            {  }
            { "there's a day but no month or year in " cite$ * warning$ }
          if$
        }
        { "there's a month but no year in " cite$ * warning$
          wrap.month
          day empty.or.unknown
            { }
            { " " * wrap.day * }
          if$
        }
      if$
    }
    { month empty.or.unknown
        {
          day empty.or.unknown
            { wrap.year }
            { "there's a day and year but no month in " cite$ * warning$ }
          if$
        }
        {
          wrap.month " " *
          day empty.or.unknown
            { }
            { wrap.day * ", " * }
          if$
          wrap.year *
        }
      if$
    }
  if$
}


FUNCTION {format.abbreviated.edition}
{
  %% Format an edition value suffixed by "ed." for typesetting.
  %%
  %% At entry: stack = ... top:[top-string]
  %% At exit:  stack = ... top-1:[top-string] top:[formatted and wrapped edition]

  edition empty.or.unknown
    { "" }
    {
      " (" * wrap.edition * " ed.)"
    }
  if$
}

FUNCTION {format.full.edition}
{
  %% Format an edition value suffixed by "edition" for typesetting.
  %%
  %% At entry: stack = ... top:[top-string]
  %% At exit:  stack = ... top-1:[top-string] top:[formatted and wrapped edition]

  edition empty.or.unknown
    { "" }
    {
      output.state mid.sentence =
        { edition "l" change.case$ "edition" wrap.top " edition" * }
        { edition "t" change.case$ "edition" wrap.top " edition" * }
      if$
    }
  if$
}


FUNCTION {format.key}
{
  %% Format a BibTeX key for typesetting.  If the top-of-stack value is
  %% empty or unknown, replace it by the wrapped key value; otherwise,
  %% replace it by a null string.
  %%
  %% At entry: stack = ... top:[author-or-organization]
  %% At exit:  stack = ... top:[formatted and wrapped key]
  %%      or:  stack = ... top:[""]

  empty.or.unknown
    { wrap.key }
    { "" }
  if$
}


FUNCTION {format.editors.familiar}
{
  %% Format editor names for use in the "in" types: inbook, incollection,
  %% inproceedings: first initial, then last names. When editors are the
  %% LABEL for an entry, then format.editor is used which lists editors
  %% by last name first.
  %%
  %% At entry: stack = ... top:[top-string]
  %% At exit:  stack = ... top-1:[top-string] top:[formatted and wrapped editor list]
  %%      or:  stack = ... top-1:[top-string] top:[""]
  %%
  %% Global variables set: author.or.editor

  editor empty.or.unknown
    { "" }
    {
     "editor" 'author.or.editor :=
      editor format.names.familiar
      editor num.names$ #1 >
        { " (eds.)" * }                 % [18-Dec-1999] NHFB: Change Eds to eds to match CMoS-14
        { " (ed.)" * }                  % [18-Dec-1999] NHFB: Change Ed to ed to match CMoS-14
      if$
    }
  if$
}


FUNCTION {format.editors}
{
  %% Format editor names for use in labels, last names first.
  %%
  %% At entry: stack = ... top:[top-string]
  %% At exit:  stack = ... top-1:[top-string] top:[formatted and wrapped editor list]
  %%      or:  stack = ... top-1:[top-string] top:[""]
  %%
  %% Global variables set: author.or.editor

  editor empty.or.unknown
    { "" }
    {
     "editor" 'author.or.editor :=
      editor format.names
      editor num.names$ #1 >
        { " (eds.)" * }                 % [18-Dec-1999] NHFB: Change Eds to eds to match CMoS-14
        { " (ed.)" * }                  % [18-Dec-1999] NHFB: Change Ed to ed to match CMoS-14
      if$
    }
  if$
}


FUNCTION {format.title}
{
  %% Format a title for typesetting.  An empty or unknown title is
  %% converted to a null string.
  %%
  %% At entry: stack = ... top:[top-string]
  %% At exit:  stack = ... top-1:[top-string] top:[formatted and wrapped title]
  %%      or:  stack = ... top-1:[top-string] top:[""]

  title empty.or.unknown
    { "" }
    { title "t" change.case$ "title" wrap.top }
  if$
}


%%% Note that the APA and CMoS-14 styles require lettercase changes in
%%% article titles. The following preserves lettercase. If you prefer
%%% it, uncomment the following and comment out the above, and give
%%% your modified style file a NEW name.

%%% FUNCTION {format.title}
%%% {
%%%   wrap.title
%%% }


FUNCTION {collapse.pagerange}
{
  %% In a page range list, convert entries of the form n--n to n, and
  %% m--n to m--n.  This allows BibTeX databases to distinguish between
  %% one-page documents (e.g., pages = "123--123"), and documents where
  %% only the initial page number is known (e.g., pages = "123" and
  %% pages = "123--??"); the case "123--123" will be reduced to "123" in
  %% the typeset bibliography.
  %%
  %% At entry: stack = ... top:[page range]
  %% At exit:  stack = ... top:[converted page range]
  %%
  %% Global variables set: b, e, s, t

  't :=                                 % save input page range string
  t 's :=                               % save original page range
  "" 'b :=                              % beginning page number
  "" 'e :=                              % ending page number
    { t empty.or.unknown not }          % while-loop Boolean condition
    {                                   % while-loop body
      t #1 #2 substring$ "--" =         % have we found the en-dash?
        {
           t #3 global.max$ substring$ 'e := % yes, save ending page number
           "" 't :=                     % and set t to empty string to terminate loop
        }
        {
           b t #1 #1 substring$ * 'b := % no, just accumulate this character
           t #2 global.max$ substring$ 't := % and set t to the remaining string
        }
      if$
    }
  while$                                % on exit, t has ending page number string
  e b =                                 % are beginning and ending numbers the same?
    { b }                               % yes, return the beginning page number string
    { s }                               % no, return the original page number string
  if$
}


FUNCTION {en.dashify}
{
  %% Convert hyphens to en dashes.  Existing en dashes are preserved.
  %%
  %% At entry: stack = ... top:[arg-arg-arg...]
  %% At exit:  stack = ... top:[arg--arg--arg...]
  %%
  %% Global variables set: t

  't :=
  ""
    { t empty.or.unknown not }          % while-loop condition
    {                                   % while-loop body
      t #1 #1 substring$ "-" =
      {
        t #1 #2 substring$ "--" = not
          {
            "--" *
            t #2 global.max$ substring$ 't :=
          }
          {
            { t #1 #1 substring$ "-" = }
            {
              "-" *
              t #2 global.max$ substring$ 't :=
            }
            while$
          }
        if$
      }
      {
        t #1 #1 substring$ *
        t #2 global.max$ substring$ 't :=
      }
      if$
    }
  while$
}


FUNCTION {format.btitle}
{
  %% Format a title in @Book{...}: this is followed either by a volume,
  %% and possibly, abbreviated edition, or a newblock.  In the case of a
  %% volume, format.bvolume.bseries will supply a comma.
  %%
  %% At entry: stack = ... top:[top-stack]
  %% At exit:  stack = ... top-1:[top-stack] top:[formatted-and-wrapped-title-volume-edition]

  edition empty.or.unknown
    { wrap.title emphasize.without.italic.correction }% handles empty title correctly
    {
      title empty.or.unknown
      { wrap.title emphasize.without.italic.correction }% returns ""
      {
        wrap.title emphasize.with.italic.correction format.abbreviated.edition *
        volume empty.or.unknown         % gnp - if no volume
          { "." * }                     % gnp - then finish off sentence
          { }                           % gnp - else format.bvolume.bseries will finish it
        if$
      }
      if$
    }
  if$
}


FUNCTION {format.other.title}
{
  %% Format a title and possibly, abbreviated edition, in @InBook{},
  %% @InCollection{}, @InProceedings{}, @Manual{}, and @Proceedings{}:
  %% this is followed by other field types that automatically supply a
  %% separating comma.
  %%
  %% At entry: stack = ... top:[top-stack]
  %% At exit:  stack = ... top-1:[top-stack] top:[formatted-and-wrapped-title-edition]

  edition empty.or.unknown
    { wrap.title emphasize.without.italic.correction } % NHFB: apparent bug: final punctuation missing if volume is empty
    {
      title empty.or.unknown
      { wrap.title emphasize.without.italic.correction } % NHFB: Why?: there is no title to emphasize!
      { wrap.title emphasize.with.italic.correction format.abbreviated.edition * }
      if$
    }
  if$
}


FUNCTION {format.emphasize.booktitle}
{
  %% Format a title, and possibly, abbreviated edition and volume, in
  %% @InCollection{...} and @InProceedings{...}.
  %%
  %% At entry: stack = ... top:[top-stack]
  %% At exit:  stack = ... top-1:[top-stack] top:[formatted-and-wrapped-booktitle-edition-volume]

  edition empty.or.unknown
    { wrap.booktitle emphasize.without.italic.correction }
    {
      booktitle empty.or.unknown
        { wrap.booktitle emphasize.without.italic.correction }
        {
          wrap.booktitle emphasize.with.italic.correction
          format.abbreviated.edition *
          volume empty.or.unknown       % gnp - if no volume
            { "." * }                   % gnp - the finish off sentence
            { }                         % gnp - else format.bvolume.bseries will finish it
          if$
        }
      if$
    }
  if$
}


FUNCTION {tie.or.space.connect}
{
  %% Join the top two stack entries with a space or tie.  The tie
  %% is used only if the top entry is one or two characters long.
  %%
  %% At entry: stack = ... top-1:[top-1-string] top:[top-string]
  %% At exit:  stack = ... top:[top-1-string top-string]
  %%      or:  stack = ... top:[top-1-string~top-string]

  duplicate$ text.length$ #3 <
    { "~" }
    { " " }
  if$
  swap$ * *
}


FUNCTION {wrap.tie.or.space.connect}
{
  %% Join the second and third stack entries with a space or tie.  The
  %% tie is used only if the second entry is one or two characters long.
  %% The top of stack is a name to be used in wrapping the result.
  %%
  %% At entry: stack = ... top-2:[keyword] top-1:[value] top:[name]
  %% At exit:  stack = ... top: [keyword~\bblname{value}]
  %%      or:  stack = ... top: [keyword \bblname{value}]
  %%
  %% Global variables set: t, u

  't :=                                 % save name in t
  'u :=                                 % save value in u
  u
  duplicate$ text.length$ #3 <
    { "~" }
    { " " }
  if$
  swap$ pop$ * u t wrap.top *
}


FUNCTION {either.or.check}
{
  %% Check that just one of the top two stack entries is defined, and
  %% issue a warning if both are defined.
  %%
  %% At entry: stack = ... top-2:[top-2-string] top-1:[key1] top:[key2]
  %% At exit:  stack = ... top:[top-2-string]

  empty.or.unknown
    { pop$ }
    { "can't use both " swap$ * " fields in " * cite$ * warning$ }
  if$
}


FUNCTION {format.bvolume.bseries}
{
  %% Format a book volume and series for typesetting, and check that
  %% number is not also defined.
  %%
  %% At entry: stack = ... top:[top-string]
  %% At exit:  stack = ... top-1:[top-string] top:[formatted and wrapped book volume and series]

  volume empty.or.unknown
    { "" }
    { "Volume" volume "volume" wrap.tie.or.space.connect % gnp - changed to mixed case
      series empty.or.unknown
        { }
        { " of " * wrap.series emphasize.without.italic.correction * }
      if$
      "volume and number" number either.or.check
    }
  if$
}


FUNCTION {format.number.series}
{
  %% Format a number and series for typesetting, and check that series
  %% is defined if the number is.
  %%
  %% At entry: stack = ... top:[top-string]
  %% At exit:  stack = ... top-1:[top-string] top:[formatted and wrapped number and series]

  volume empty.or.unknown
    {
      number empty.or.unknown
        { wrap.series }
        {
          output.state mid.sentence =
            { "Number" }                % gnp - changed to mixed case always
            { "Number" }
          if$
          number "number" wrap.tie.or.space.connect series empty.or.unknown
            { "there's a number but no series in " cite$ * warning$ }
            { " in " * wrap.series * }
          if$
        }
      if$
    }
    { "" }
  if$
}


INTEGERS { multiresult }                % private `local' variable for multi.page.check only


FUNCTION {multi.page.check}
{
  %% Given a page list on the top-of-stack, examine it to see if it
  %% references multiple pages (as evidenced by a comma, hyphen, or
  %% plus).  Replace the top-of-stack by 0 (single page) or 1 (multiple
  %% pages).
  %%
  %% At entry: stack = ... top:[page list]
  %% At exit:  stack = ... top:[0 or 1]
  %%
  %% Global variables set: multiresult, t

  't :=
  #0 'multiresult :=
    {                                   % while-loop Boolean condition
      multiresult not
      t empty.or.unknown not
      and
    }
    {                                   % while-loop body
      t #1 #1 substring$
      duplicate$ "-" =
      swap$ duplicate$ "," =
      swap$ "+" =
      or or
        { #1 'multiresult := }
        { t #2 global.max$ substring$ 't := }
      if$
    }
  while$
  multiresult
}


FUNCTION {format.pages}
{
  %% Format a pages list for typesetting.
  %%
  %% At entry: stack = ... top:[top-string]
  %% At exit:  stack = ... top-1:[top-string] top:[formatted and wrapped pages]
  %%      or:  stack = ... top-1:[top-string] top:[""]
  %%
  %% Global variables set: new.pages

  pages empty.or.unknown
  { "" }
  {
    pages multi.page.check
      { pages collapse.pagerange 'new.pages := }
      { pages 'new.pages := }
    if$
    new.pages multi.page.check
      { "pp.\ " new.pages en.dashify "pages" wrap.tie.or.space.connect } % gnp - removed ()
      { "p.\ " new.pages "pages" wrap.tie.or.space.connect }
    if$
  }
  if$
}


FUNCTION {format.journal.volume.number.pages}
{
  %% Format journal, volume, number, pages for @Article{...}, and warn
  %% about missing or inconsistent data.
  %%
  %% By Young (and Spencer)
  %% GNP - fixed bugs with missing volume, number, and/or pages
  %%
  %% At entry: stack = ... top:[top-string]
  %% At exit:  stack = ... top-1:[top-string] top:[wrapped-journal wrapped-volume\penalty0 (wrapped-number):\penalty0 wrapped-pages]
  %%      or:  stack = ... top-1:[top-string] top:[wrapped-journal wrapped-volume:\penalty0 wrapped-pages]
  %%      or:  stack = ... top-1:[top-string] top:[wrapped-journal wrapped-volume]
  %%      or:  stack = ... top-1:[top-string] top:[wrapped-journal (wrapped-number)\penalty0 wrapped-pages]
  %%      or:  stack = ... top-1:[top-string] top:[wrapped-journal (wrapped-number)]
  %%      or:  stack = ... top-1:[top-string] top:[wrapped-journal]
  %%      or:  stack = ... top-1:[top-string] top:[""]

  journal empty.or.unknown
    {
      "no journal in " cite$ * warning$
      ""
    }
    { wrap.journal emphasize.with.italic.correction }
  if$
  number empty.or.unknown
  {
    volume empty.or.unknown
      { "no number and no volume in " cite$ * warning$ }
      {
        %% [18-Dec-1999] NHFB: removed italics on volume number: (CMoS-14)
        " " * wrap.volume *
      }
    if$
  }
  {
    %% [16-Dec-1999] NHFB: replace unneeded ties before volume number by space
    volume empty.or.unknown
      {
        "no volume for " cite$ * warning$
        " (" * wrap.number * ")" *
      }
      {
        %% [18-Dec-1999] NHFB: removed italics on volume number: (CMoS-14)
        " " * wrap.volume * "\penalty0 (" * wrap.number * ")" *
      }
    if$
  }
  if$
  pages empty.or.unknown
    {
      "page numbers missing in " cite$ * warning$
      "" *                              % gnp - place a null string on the stack for output
    }
    {
      %% [18-Dec-1999] NHFB: changed comma-space to colon-\penalty0 (CMoS-14)
      duplicate$ empty.or.unknown
        { pop$ format.pages }
        { ":\penalty0 " * pages en.dashify "pages" wrap.top * }% gnp - removed pp. for articles
      if$
    }
  if$
}


FUNCTION {format.chapter.pages}
{
  %% Format chapter and pages for typesetting for @InBook{...} and
  %% @InCollection{...} entries.  Warn if the pages are missing.
  %%
  %% At entry: stack = ... top:[top-string]
  %% At exit:  stack = ... top-1:[top-string] top:[formatted and wrapped chapter and pages]

  chapter empty.or.unknown
    { format.pages }
    { type empty.or.unknown
        { "Chapter" }                   % gnp - changed to mixed case
        { type "t" change.case$ "type" wrap.top}
      if$
      chapter "chapter" wrap.tie.or.space.connect
      pages empty.or.unknown
        {"page numbers missing in " cite$ * warning$} % gnp - added check
        { ", " * format.pages * }
      if$
    }
  if$
}


FUNCTION {format.in.ed.booktitle}
{
  %% Format editors and booktitle for @InCollection{...} and
  %% @InProceedings{...} entries.
  %%
  %% At entry: stack = ... top:[top-string]
  %% At exit:  stack = ... top-1:[top-string] top:[In wrapped-editors, wrapped-booktitle]
  %%      or:  stack = ... top-1:[top-string] top:[In wrapped-booktitle]

  booktitle empty.or.unknown
    { "" }
    {
      editor empty.or.unknown
        { "In " format.emphasize.booktitle * }
        { "In " format.editors.familiar * ", " * format.emphasize.booktitle * }
      if$
    }
  if$
}


FUNCTION {format.thesis.type}
{
  %% Format a thesis type for typesetting.  The top-of-stack contains a
  %% default type to use if the type is not specified in the BibTeX
  %% entry.
  %%
  %% At entry: stack = ... top: [default-type]
  %% At exit:  stack = ... top: [wrapped-non-empty-known-type-or-else-default-type]

  type empty.or.unknown
    { "type" wrap.top }
    {
      pop$
      wrap.type % [21-Dec-1999]: NHFB removed horrid change.case$, which is ALWAYS wrong
    }
  if$
}


FUNCTION {format.tr.number}
{
  %% Format a technical report name and number for typesetting.
  %%
  %% At entry: stack = ... top:[top-string]
  %% At exit:  stack = ... top-1:[top-string] top:[wrapped-type wrapped-number]
  %%      or:  stack = ... top-1:[top-string] top:[wrapped-type]

  type empty.or.unknown
    { "Technical Report" "type" wrap.top}
    { wrap.type }
  if$
  number empty.or.unknown
    { "t" change.case$ }
    { number "number" wrap.tie.or.space.connect }
  if$
}


FUNCTION {format.vol.num.pages}
{
  %% Format volume, number, and pages for @Periodical{...}, and warn
  %% about missing or inconsistent data.
  %%
  %% At entry: stack = ... top:[top-string]
  %% At exit:  stack = ... top-1:[top-string] top:[wrapped-volume\penalty0 (wrapped-number):\penalty0 wrapped-pages]
  %%      or:  stack = ... top-1:[top-string] top:[wrapped-volume:\penalty0 wrapped-pages]
  %%      or:  stack = ... top-1:[top-string] top:[wrapped-volume]
  %%      or:  stack = ... top-1:[top-string] top:[(wrapped-number)\penalty0 wrapped-pages]
  %%      or:  stack = ... top-1:[top-string] top:[(wrapped-number)]
  %%      or:  stack = ... top-1:[top-string] top:[""]

  wrap.volume
  number empty.or.unknown
    { }
    { "\penalty0 (" wrap.number * ")" *
      volume empty.or.unknown
        { "there's a number but no volume in " cite$ * warning$ }
        { }
      if$
    }
  if$
  pages empty.or.unknown
    { }
    { duplicate$ empty.or.unknown
        { pop$ format.pages }
        { ":\penalty0 " * pages collapse.pagerange en.dashify "pages" wrap.top * }
      if$
    }
  if$
}


FUNCTION {format.article.crossref}
{
  %% Format an article cross reference for typesetting.
  %%
  %% At entry: stack = ... top:[top-string]
  %% At exit:  stack = ... top-1:[top-string] top:["See \citeN{wrapped-label}"]

  "See \citeN{" wrap.crossref * "}" *
}


FUNCTION {format.crossref.editor}
{
  %% Format an editor list for @Book{...} and @InBook{...}  entries.
  %%
  %% At entry: stack = ... top:[top-string]
  %% At exit:  stack = ... top-1:[top-string] top:[formatted and wrapped list]

  editor #1 "{vv~}{ll}" format.and.wrap.name
  editor num.names$
  duplicate$ #2 >
    { pop$ " et~al." * }
    {
      #2 <
        { }
        {
          editor #2 "{ff }{vv }{ll}{ jj}" format.and.wrap.name "others" =
            { " et~al." * }
            { " and " * editor #2 "{vv~}{ll}" format.and.wrap.name * }
          if$
        }
      if$
    }
  if$
}


FUNCTION {format.book.crossref}
{
  %% Format a volume and author/editor list cross reference for
  %% @Book{...} and @InBook{...}  entries.
  %%
  %% At entry: stack = ... top:[top-string]
  %% At exit:  stack = ... top-1:[top-string] top:[formatted and wrapped list]

  volume empty.or.unknown
    { "empty volume in " cite$ * "'s crossref of " * crossref * warning$
      "In "
    }
    { "Volume" volume "volume" wrap.tie.or.space.connect % gnp - changed to mixed case
      " of " *
    }
  if$
  editor empty.or.unknown
  editor field.or.null author field.or.null =
  or
    { key empty.or.unknown
    { series empty.or.unknown
        { "need editor, key, or series for " cite$ * " to crossref " *
          crossref * warning$
          "" *
        }
        { wrap.series emphasize.with.italic.correction }
      if$
    }
    { key * }
      if$
    }
    { format.crossref.editor * }
  if$
  " \citeN{" * crossref * "}" *
}


FUNCTION {format.incoll.inproc.crossref}
{
  %% Format an @InCollection{...} or @InProceedings{...} cross reference
  %% for typesetting.
  %%
  %% At entry: stack = ... top:[top-string]
  %% At exit:  stack = ... top-1:[top-string] top:["See \citeN{wrapped-label}"]

   format.article.crossref              % reuse common code
}


FUNCTION {format.lab.names}
{
  %% Determine "short" names for the abbreviated author information.
  %% "Long" labels are created in calc.label, using the routine
  %% my.full.label to format author and editor fields.
  %%
  %% There are 4 cases for labels.   (n=3 in the example)
  %% a) one author             Foo
  %% b) one to n               Foo, Bar and Baz
  %% c) use of "and others"    Foo, Bar et al.
  %% d) more than n            Foo et al.
  %%
  %% At entry: stack = ... top:[name1 and name2 and name3 and ...]
  %% At exit:  stack = ... top:[formatted name list]
  %%
  %% Global variables set: nameptr, namesleft, numnames, s

  's :=
  s num.names$ 'numnames :=
  numnames #2 > % change number to number of others allowed before forcing "et al".
    { s #1 "{vv~}{ll}" format.name$ " et~al." * }
    {
      numnames #1 - 'namesleft :=
      #2 'nameptr :=
      s #1 "{vv~}{ll}" format.name$
        { namesleft #0 > }              % while-loop Boolean condition
        {                               % while-loop body
            nameptr numnames =
            {
              s nameptr "{ff }{vv }{ll}{ jj}" format.name$ "others" =
                { " et~al." * }
                { " and " * s nameptr "{vv~}{ll}" format.name$ * }
              if$
            }
            { ", " * s nameptr "{vv~}{ll}" format.name$ * }
          if$
          nameptr #1 + 'nameptr :=
          namesleft #1 - 'namesleft :=
        }
      while$
    }
  if$
}


FUNCTION {author.key.label}
{
  %% Compute a label from the author or key values.
  %%
  %% At entry: stack = ... top:[top-string]
  %% At exit:  stack = ... top-1:[top-string] top:[label]

  author empty.or.unknown
    {
      key empty.or.unknown
        {
          "no key, author in " cite$ * warning$
          cite$ #1 #3 substring$
        }
        { key }
      if$
    }
    { author format.lab.names }
  if$
}


FUNCTION {editor.key.label}
{
  %% Compute a label from the editor or key values.
  %%
  %% At entry: stack = ... top:[top-string]
  %% At exit:  stack = ... top-1:[top-string] top:[label]

  editor empty.or.unknown
    {
      key empty.or.unknown
        {
          "no key, editor in " cite$ * warning$
          cite$ #1 #3 substring$
        }
        { key }
      if$
    }
    { editor format.lab.names }
  if$
}


FUNCTION {author.key.organization.label}
{
  %% Compute a label from the author or organization or key values.
  %%
  %% added - gnp.
  %%
  %% At entry: stack = ... top:[top-string]
  %% At exit:  stack = ... top-1:[top-string] top:[label]

  author empty.or.unknown
    {
      organization empty.or.unknown
        {
          key empty.or.unknown
            {
              "no key, author or organization in " cite$ * warning$
              cite$ #1 #3 substring$
            }
            { key }
          if$
        }
        { organization }
      if$
    }
    { author format.lab.names }
  if$
}


FUNCTION {editor.key.organization.label}
{
  %% Compute a label from the editor or organization or key values.
  %%
  %% added - gnp.
  %%
  %% At entry: stack = ... top:[top-string]
  %% At exit:  stack = ... top-1:[top-string] top:[label]

  editor empty.or.unknown
    {
      organization empty.or.unknown
        {
          key empty.or.unknown
            {
              "no key, editor or organization in " cite$ * warning$
              cite$ #1 #3 substring$
            }
            { key }
          if$
        }
        { organization }
      if$
    }
    { editor format.lab.names }
  if$
}


FUNCTION {author.editor.key.label}
{
  %% Compute a label from the author or editor or key values.
  %%
  %% At entry: stack = ... top:[top-string]
  %% At exit:  stack = ... top-1:[top-string] top:[label]

  author empty.or.unknown
    {
      editor empty.or.unknown
        {
          key empty.or.unknown
             {
               "no key, author, or editor in " cite$ * warning$
               cite$ #1 #3 substring$
             }
             { key }
          if$
        }
        { editor format.lab.names }
      if$
    }
    { author format.lab.names }
  if$
}


FUNCTION {calc.label}
{
  %% Calculate a long label for in-text citation.  Also, save the year,
  %% and any needed suffix letter, in the global variable label.year for
  %% later sort processing.
  %%
  %% Changed - GNP. See also author.organization.sort and
  %% editor.organization.sort.  Form label for BibTeX entry. The
  %% classification of which fields are used for which type of entry
  %% (book, inbook, etc.) are taken from alpha.bst.  The change here
  %% from newapa is to also include organization as a citation label if
  %% author or editor is missing.
  %%
  %% At entry: stack = ... top:[top-string]
  %% At exit:  stack = ... top-1:[top-string] top:[label]
  %%
  %% Global variables set: label.year

  type$ "book" =
  type$ "inbook" =
  or
    { author.editor.key.label }
    {
      type$ "proceedings" =
        { editor.key.organization.label }
        {
          type$ "manual" =
            { author.key.organization.label }
            { author.key.label }
          if$
        }
      if$
    }
  if$

  author empty.or.unknown               % generate the full label citation information.
    {
      editor empty.or.unknown
        {
          organization empty.or.unknown
            {
              "no author, editor, or organization in " cite$ * warning$
              "??"
            }
            { organization }
          if$
        }
        { editor my.full.label }
        if$
    }
    { author my.full.label }
  if$

  %% leave label on the stack, to be popped when required.

  "}{" * swap$ * "}{" *

  %%  year field.or.null purify$ #-1 #4 substring$ *
  %%
  %% save the year for sort processing afterwards (adding a, b, c, etc.)
  %%
  year field.or.null purify$ #-1 #4 substring$
  'label.year :=
}


FUNCTION {output.bibitem}
{
  %% Begin the output of a bibliography item, producing
  %%
  %% At entry: stack = empty
  %% At exit:  stack = top(==1):[\bibitem[...]{...}...]
  %%
  %% Global variables set: output.state

  newline$

  "\bibitem[\protect\citeauthoryear{" write$
  calc.label write$
  sort.year write$
  "}]{" write$

  cite$ write$
  "}" write$
  newline$
  "% " wrap.type$ * write$ newline$     % must preserve document type in comment
  "% " wrap.cite$ * write$ newline$     % must wrap citation label in comment, NOT in bibitem argument
  ""
  before.all 'output.state :=
}


FUNCTION {article}
{
  %% Write an @Article{...} entry.
  %%
  %% Typical value (CMoS-14, p. 647):
  %%
  %%     Arnold, M. L., R. L. Honeycutt, R. J. Baker, V. M. Sarich, and
  %%       J. K. Jones, Jr. 1982. Resolving a phylogeny with multiple
  %%       data sets: A systematic study of phyllostomid bats. {\em
  %%       Occas. Pap. Museum, Texas Tech Univ.\/} 77:1--15.
  %%
  %% At entry: stack = empty
  %% At exit:  stack = empty

  output.bibitem
  format.authors "author" output.check
  author format.key output              % added
  output.year.month.day.check           % added
  new.block
  format.title "title" output.check
  new.block
  crossref missing$
    { format.journal.volume.number.pages output }
    {
      format.article.crossref output.nonnull
      format.pages output
    }
  if$
  new.block
  format.note output
  fin.entry
}


FUNCTION {book}
{
  %% Write a @Book{...} entry.
  %%
  %% At entry: stack = empty
  %% At exit:  stack = empty

  output.bibitem
  author empty.or.unknown
    { format.editors "author and editor" output.check }
    {
      format.authors output.nonnull
      crossref missing$
        { "author and editor" editor either.or.check }
        { }
      if$
    }
  if$
  output.year.month.day.check           % added
  new.block
  format.btitle "title" output.check
  crossref missing$
    {
      format.bvolume.bseries output
      new.block
      format.number.series output
      new.sentence
      wrap.address output
      wrap.publisher "publisher" output.check.colon
    }
    {
      new.block
      format.book.crossref output.nonnull
    }
  if$
  format.book.pages output              % [18-Dec-1999] NHFB: new feature
  new.block
  format.note output
  fin.entry
}


FUNCTION {booklet}
{
  %% Write a @Booklet{...} entry.
  %%
  %% At entry: stack = empty
  %% At exit:  stack = empty

  output.bibitem
  format.authors output
  author format.key output              % added
  output.year.month.day.check           % added
  new.block
  format.title "title" output.check
  new.block
  howpublished output
  wrap.address output
  format.book.pages output              % [18-Dec-1999] NHFB: new feature
  new.block
  format.note output
  fin.entry
}


FUNCTION {inbook}
{
  %% Write an @InBook{...} entry.
  %%
  %% Typical value (CMoS-14, p. 647):
  %%
  %%     August, P. V. 1979.  Distress calls in {\em Artibeus
  %%       jamaicensis\/}: Ecology and evolutionary implications. In
  %%       {\em Vertebrate ecology in northern Neotropics\/},
  %%       ed. J. F. Eisenberg, 151--59.  Washington, D.C.: Smithsonian
  %%       Institute Press.
  %%
  %% NB: The elided leading digits in the page number range in this
  %%     example are intentionally NOT supported by this BibTeX style:
  %%     they are a senseless economization.
  %%
  %% At entry: stack = empty
  %% At exit:  stack = empty

  output.bibitem
  author empty.or.unknown
    { format.editors "author and editor" output.check }
    {
      format.authors output.nonnull
      crossref missing$
        { "author and editor" editor either.or.check }
        { }
      if$
    }
  if$
  output.year.month.day.check           % added
  new.block
  format.other.title "title" output.check
  crossref missing$
    {
      format.bvolume.bseries output
      format.chapter.pages format.bookpages * "chapter and pages" output.check
      new.block
      format.number.series output
      new.sentence
      wrap.address output
      wrap.publisher "publisher" output.check.colon
    }
    { 
      format.chapter.pages format.bookpages * "chapter and pages" output.check
      new.block
      format.book.crossref output.nonnull
    }
  if$
  new.block
  format.note output
  fin.entry
}


FUNCTION {incollection}
{
  %% Write an @InCollection{...} entry.
  %%
  %%
  %% At entry: stack = empty
  %% At exit:  stack = empty

  output.bibitem
  format.authors "author" output.check
  author format.key output              % added
  output.year.month.day.check           % added
  new.block
  format.title "title" output.check
  new.block
  crossref missing$
  {
    format.in.ed.booktitle "booktitle" output.check
    format.bvolume.bseries output
    format.number.series output
    %% leave out comma before page numbers:
    format.chapter.pages format.bookpages * "chapter and pages" output.check
    new.sentence
    wrap.address output
    wrap.publisher "publisher" output.check.colon
  }
  { 
    format.incoll.inproc.crossref output.nonnull
    format.chapter.pages format.bookpages * "chapter and pages" output.check
  }
  if$
  new.block
  format.note output
  fin.entry
}


FUNCTION {inproceedings}
{
  %% Write an @InProceedings{...} entry.
  %%
  %% At entry: stack = empty
  %% At exit:  stack = empty

  output.bibitem
  format.authors "author" output.check
  author format.key output              % added
  output.year.month.day.check           % added
  new.block
  format.title "title" output.check
  new.block
  crossref missing$
    {
      format.in.ed.booktitle "booktitle" output.check
      format.bvolume.bseries output
      format.number.series output
      format.pages format.bookpages * "pages" output.check
      new.sentence
      format.organization output
      wrap.address output               % [18-Dec-1999]: NHFB: address is
                                        % publisher address, NOT conference address
      wrap.publisher "publisher" output.check.colon
    }
    {
      format.incoll.inproc.crossref output.nonnull
      format.pages format.bookpages * "pages" output.check
    }
  if$
  new.block
  format.note output
  fin.entry
}


FUNCTION {conference}                   % Scribe alias for BibTeX equivalent
{
  %% Write a @Conference{...} (== @InProceedings{...}) entry.
  %%
  %% At entry: stack = empty
  %% At exit:  stack = empty

  inproceedings                         % reuse common code
}


FUNCTION {manual}
{
  %% Write a @Manual{...} entry.
  %%
  %% At entry: stack = empty
  %% At exit:  stack = empty

  output.bibitem
  author empty.or.unknown
    {
      editor empty.or.unknown
        {                               % if all else fails, use key
          organization "organization" output.check
          organization format.key output
        }
        { format.editors "author and editor" output.check }
      if$
    }
    { format.authors output.nonnull }
    if$
  output.year.month.day.check           % added
  new.block
  format.other.title "title" output.check
  organization address new.block.checkb
  %% Reversed the order of "address" and "organization", added the ":".
  wrap.address output
  organization "organization" output.check.colon
  %%  wrap.address output
  %%  ":" output
  %%  organization output
  new.block
  format.full.edition output
  format.date output
  format.book.pages output
  new.block
  format.note output
  fin.entry
}


FUNCTION {mastersthesis}
{
  %% Write a @MastersThesis{...} entry.
  %%
  %% At entry: stack = empty
  %% At exit:  stack = empty

  output.bibitem
  format.authors "author" output.check
  author format.key output              % added
  output.year.month.day.check           % added
  new.block
  format.title "title" output.check
  new.block
  "Master's thesis" format.thesis.type output.nonnull
  school "school" output.check
  wrap.address output
  format.date "year" output.check
  format.book.pages output
  new.block
  format.note output
  fin.entry
}


FUNCTION {misc}
{
  %% Write a @Misc{...} entry.
  %%
  %% At entry: stack = empty
  %% At exit:  stack = empty

  output.bibitem
  format.authors output
  author format.key output              % added
  output.year.month.day.check           % added
  title howpublished new.block.checkb
  format.title output
  new.block
  howpublished output
  format.book.pages output              % [18-Dec-1999] NHFB: new feature
  new.block
  format.note output
  fin.entry
}


FUNCTION {periodical}
{
  %% Write a @Periodical{...} entry.
  %%
  %% At entry: stack = empty
  %% At exit:  stack = empty

  output.bibitem
  editor empty.or.unknown
    { organization output }
    { format.editors output.nonnull }
  if$
  new.block
  title emphasize.without.italic.correction "title" output.check
  format.vol.num.pages output
  format.date output
  new.sentence
  wrap.publisher output
  wrap.address output
  howpublished new.block.checka
  wrap.howpublished output
  new.block
  format.note output
  fin.entry
}


FUNCTION {phdthesis}
{
  %% Write a @PhDThesis{...} entry.
  %%
  %% At entry: stack = empty
  %% At exit:  stack = empty

  output.bibitem
  format.authors "author" output.check
  author format.key output              % added
  output.year.month.day.check           % added
  new.block
  format.title "title" output.check     % [18-Dec-1999] NHFB: CMoS-14 16.132: thesis titles in roman, NOT italic
  new.block
  "Ph.D. thesis" format.thesis.type output.nonnull % [18-Dec-1999]  NHFB: CMoS-14 16.132: changed "Ph.\ D." to "Ph.D"
  school "school" output.check
  wrap.address output
  format.book.pages output
  new.block
  format.note output
  fin.entry
}


FUNCTION {proceedings}
{
  %% Write a @Proceedings{...} entry.
  %%
  %% At entry: stack = empty
  %% At exit:  stack = empty

  output.bibitem
  editor empty.or.unknown
    { 
      organization output
      organization format.key output 
    }                                   % gnp - changed from author format.key
    { format.editors output.nonnull }
  if$
  %% author format.key output           % gnp - removed (should be either
  %%                                        editor or organization
  output.year.month.day.check           % added (newapa)
  new.block
  format.other.title "title" output.check
  format.bvolume.bseries output
  format.number.series output
  wrap.address output
  new.sentence
  organization output
  publisher output.colon
  format.book.pages output              % [18-Dec-1999] NHFB: new feature
  new.block
  format.note output
  fin.entry
}


FUNCTION {techreport}
{
  %% Write a @TechReport{...} entry.
  %%
  %% At entry: stack = empty
  %% At exit:  stack = empty

  output.bibitem
  format.authors "author" output.check
  author format.key output              % added
  output.year.month.day.check           % added
  new.block
  format.title "title" output.check
  new.block
  format.tr.number output.nonnull
  institution "institution" output.check
  wrap.address output
  new.block
  format.book.pages output
  new.block
  format.note output
  fin.entry
}


FUNCTION {unpublished}
{
  %% Write an @Unpublished{...} entry.
  %%
  %% At entry: stack = empty
  %% At exit:  stack = empty

  output.bibitem
  format.authors "author" output.check
  author format.key output              % added
  output.year.month.day.check           % added
  new.block
  format.title "title" output.check
  format.book.pages output              % [18-Dec-1999] NHFB: new feature
  new.block
  format.note "note" output.check
  fin.entry
}


FUNCTION {default.type}
{
  %% Write all other unrecognized entry types as if they were @Misc{...}
  %%
  %% At entry: stack = empty
  %% At exit:  stack = empty

  misc
}


MACRO {jan} {"January"}

MACRO {feb} {"February"}

MACRO {mar} {"March"}

MACRO {apr} {"April"}

MACRO {may} {"May"}

MACRO {jun} {"June"}

MACRO {jul} {"July"}

MACRO {aug} {"August"}

MACRO {sep} {"September"}

MACRO {oct} {"October"}

MACRO {nov} {"November"}

MACRO {dec} {"December"}

MACRO {acmcs}           {"ACM Computing Surveys"}

MACRO {acta}            {"Acta Informatica"}

MACRO {ai}              {"Artificial Intelligence"}

MACRO {cacm}            {"Communications of the ACM"}

MACRO {ibmjrd}          {"IBM Journal of Research and Development"}

MACRO {ibmsj}           {"IBM Systems Journal"}

MACRO {ieeese}          {"IEEE Transactions on Software Engineering"}

MACRO {ieeetcad}        {"IEEE Transactions on Computer-Aided Design of Integrated Circuits"}

MACRO {ieeetc}          {"IEEE Transactions on Computers"}

MACRO {ipl}             {"Information Processing Letters"}

MACRO {jacm}            {"Journal of the ACM"}

MACRO {jcss}            {"Journal of Computer and System Sciences"}

MACRO {scp}             {"Science of Computer Programming"}

MACRO {sicomp}          {"SIAM Journal on Computing"}

MACRO {tcs}             {"Theoretical Computer Science"}

MACRO {tocs}            {"ACM Transactions on Computer Systems"}

MACRO {tods}            {"ACM Transactions on Database Systems"}

MACRO {tog}             {"ACM Transactions on Graphics"}

MACRO {toms}            {"ACM Transactions on Mathematical Software"}

MACRO {toois}           {"ACM Transactions on Office Information Systems"}

MACRO {toplas}          {"ACM Transactions on Programming Languages and Systems"}


READ


FUNCTION {sortify}
{
  %% Convert a string to a sort key.  `Purification' ``removes
  %% nonalphanumeric characters except for white-space characters and
  %% hyphens and ties (these all get converted to a space), and removes
  %% certain alphabetic characters contained in the control sequences
  %% associated with a ``special character''$\!$'' [from btxhak.tex].
  %%
  %% At entry: stack = ... top:[string]
  %% At exit:  stack = ... top:[purified-lowercase-string]
  purify$
  "l" change.case$
}


INTEGERS { len }


FUNCTION {chop.word}
{
  %% Reduce a string to a limited number of characters, truncating on
  %% the left. From xbtxbst.doc: ``The chop.word(w,len,s) function
  %% returns either s or, if the first len letters of s equals w, it
  %% returns that part of s after w.''
  %%
  %% At entry: stack = top-3:[top-3-string] top-2:[word] top-1:[length] top:[string]
  %% At exit:  stack = top-1:[top-3-string] top:[chopped-string]
  %%
  %% Global variables set: len, s

  's :=
  'len :=
  s #1 len substring$ =
    { s len #1 + global.max$ substring$ }
    { s }
  if$
}


FUNCTION {sort.format.names}
{
  %% Prepare a list of author/editor names for sorting.  This function
  %% is quite similar to format.names.
  %%
  %% At entry: stack = ... top:[name1 and name2 and name3 and ...]
  %% At exit:  stack = ... top:[list of sortified formatted names]
  %%
  %% Global variables set: nameptr, namesleft, numnames, s, t

  's :=
  #1 'nameptr :=
  ""
  s num.names$ 'numnames :=
  numnames 'namesleft :=
    { namesleft #0 > }                  % while-loop Boolean condition
    {                                   % while-loop body
      nameptr #1 >
          { "   " * }
         { }
      if$
      s nameptr "{vv{ } }{ll{ }}{  f{ }}{  jj{ }}" format.name$ 't :=
      nameptr numnames = t "others" = and
          { " et~al" * }
          { t sortify * }
      if$
      nameptr #1 + 'nameptr :=
      namesleft #1 - 'namesleft :=
    }
  while$
}


FUNCTION {sort.format.title}
{
  %% Remove leading articles from the top-of-stack string, convert for
  %% sorting, and leave the result on the top-of-stack.
  %%
  %% At entry: stack = ... top:[title]
  %% At exit:  stack = ... top:[sortified-title]
  %%
  %% Global variables set: t

  't :=
  "A " #2
    "An " #3
      "The " #4 t chop.word
    chop.word
  chop.word
  sortify
  #1 global.max$ substring$
}


FUNCTION {author.sort}
{
  %% Prepare a sort key based on the author or key.
  %%
  %% At entry: stack = ... top:[top-string]
  %% At exit:  stack = ... top-1:[top-string] top:[sort key]

  author empty.or.unknown
    {
      key empty.or.unknown
        {
          "to sort, need author or key in " cite$ * warning$
          ""
        }
        { key sortify }
      if$
    }
    { author sort.format.names }
  if$
}


FUNCTION {editor.sort}
{
  %% Prepare a sort key based on the editor or key.
  %%
  %% At entry: stack = ... top:[top-string]
  %% At exit:  stack = ... top-1:[top-string] top:[sort key]

  editor empty.or.unknown
    { key empty.or.unknown
         { "to sort, need editor or key in " cite$ * warning$
           ""
         }
         { key sortify }
      if$
    }
    { editor sort.format.names }
  if$
}


FUNCTION {author.editor.sort}
{
  %% Prepare a sort key based on the author/editor or key.
  %%
  %% At entry: stack = ... top:[top-string]
  %% At exit:  stack = ... top-1:[top-string] top:[sort key]

  author empty.or.unknown
    {
      %% [16-Dec-1999] NHFB: disable this unwanted warning: a book can have
      %% EITHER an author OR an editor: if both are missing, a suitable
      %% warning is generated 4 lines down.
      %%  "missing author in " cite$ * warning$
      editor empty.or.unknown
         { key empty.or.unknown
             { "to sort, need author, editor, or key in " cite$ * warning$
               ""
             }
             { key sortify }
           if$
         }
         { editor sort.format.names }
      if$
    }
    { author sort.format.names }
  if$
}


FUNCTION {author.organization.sort}
{
  %% Prepare a sort key based on the author or organization or key.
  %%
  %% added - GNP. Stack author or organization for sorting (from
  %% alpha.bst).  Unlike alpha.bst, we need entire names, not
  %% abbreviations.
  %%
  %% At entry: stack = ... top:[top-string]
  %% At exit:  stack = ... top-1:[top-string] top:[sort key]

  author empty.or.unknown
    { organization empty.or.unknown
        { key empty.or.unknown
            { "to sort, need author, organization, or key in " cite$ * warning$
              ""
            }
            { key sortify }
          if$
        }
        { organization sortify }
      if$
    }
    { author sort.format.names }
  if$
}


FUNCTION {editor.organization.sort}
{
  %% Prepare a sort key based on the author or organization or key.
  %%
  %% added - GNP. Stack editor or organization for sorting (from
  %% alpha.bst).  Unlike alpha.bst, we need entire names, not
  %% abbreviations.
  %%
  %% At entry: stack = ... top:[top-string]
  %% At exit:  stack = ... top-1:[top-string] top:[sort key]

  editor empty.or.unknown
    { organization empty.or.unknown
        { key empty.or.unknown
            { "to sort, need editor, organization, or key in " cite$ * warning$
              ""
            }
            { key sortify }
          if$
        }
        { organization sortify }
      if$
    }
    { editor sort.format.names }
  if$
}


FUNCTION {presort}
{
  %% Create the bibentry's label via a call to calc.label, and then sort
  %% the entries based on entry type.
  %%
  %% [x]chicago.bst adds support for including organizations as the sort
  %% key; the following is stolen from alpha.bst.
  %%
  %% At entry: stack = ... top:[top-string]
  %% At exit:  stack = ... top:[top-string]
  %%
  %% Global variables set: sort.key sort.label

  calc.label sortify                    % recalculate bibitem label
  year field.or.null purify$ #-1 #4 substring$ * % add year
  "    "
  *
  type$ "book" =
  type$ "inbook" =
  or
    { author.editor.sort }
    {
      type$ "proceedings" =
        { editor.organization.sort }
        {
          type$ "manual" =
            { author.organization.sort }
            { author.sort }
          if$
        }
      if$
    }
  if$
  #1 entry.max$ substring$              % added for newapa
  'sort.label :=                        % added for newapa
  sort.label                            % added for newapa
  *
  "    "
  *
  title field.or.null
  sort.format.title
  *
  #1 entry.max$ substring$
  'sort.key$ :=
}


ITERATE {presort}


SORT                                    % by label, year, author/editor, title


INTEGERS { last.extra.num }


FUNCTION {initialize.extra.label.stuff}
{
  %% Initialize global variables for sorting.
  %%
  %% At entry: stack = ... top:[top-string]
  %% At exit:  stack = ... top:[top-string]
  %%
  %% Global variables set: last.label, next.extra, last.extra.num

  #0 int.to.chr$ 'last.label :=
  "" 'next.extra :=
  #0 'last.extra.num :=
}


FUNCTION {forward.pass}
{
  %% Pass through all entries, comparing the current entry to the last
  %% one.  We need to concatenate year to the stack (done by calc.label)
  %% to determine if two entries are the same (see presort)
  %%
  %% At entry: stack = ... top:[top-string]
  %% At exit:  stack = ... top:[top-string]
  %%
  %% Global variables set: extra.label, last.extra.num, last.label

  last.label
  calc.label year field.or.null purify$ #-1 #4 substring$ * % add year
  #1 entry.max$ substring$ =            % are they equal?
     { last.extra.num #1 + 'last.extra.num :=
       last.extra.num int.to.chr$ 'extra.label :=
     }
     { "a" chr.to.int$ 'last.extra.num :=
       "" 'extra.label :=
       calc.label year field.or.null purify$ #-1 #4 substring$ * % add year
       #1 entry.max$ substring$ 'last.label := % assign to last.label
     }
  if$
}


FUNCTION {reverse.pass}
{
  %% Pass through all entries in reverse order, comparing the current
  %% entry to previous one.  This is required in order to supply a
  %% suffix "a" to the first of a sequence of entries that were suffixed
  %% "none", "b", "c", ... on the forward pass.
  %%
  %% At entry: stack = ... top:[top-string]
  %% At exit:  stack = ... top:[top-string]
  %%
  %% Global variables set: extra.label, nextr.extra,  sort.year

  next.extra "b" =
    { "a" 'extra.label := }
     { }
  if$
  label.year extra.label * 'sort.year :=
  extra.label 'next.extra :=
}


EXECUTE {initialize.extra.label.stuff}


ITERATE {forward.pass}


REVERSE {reverse.pass}


FUNCTION {bib.sort.order}
{
  %% Compute a sort key from the sort label, year, and title, preserving
  %% no more than the leading entry.max$ characters, and store the
  %% result in the global variable sort.key.
  %%
  %% At entry: stack = ... top:[top-string]
  %% At exit:  stack = ... top:[top-string]
  %%
  %% Global variables set: sort.key

  sort.label "    " * wrap.year sortify
  * "    " * wrap.title sort.format.title
  *
  #1 entry.max$ substring$
  'sort.key$ :=
}


ITERATE {bib.sort.order}


SORT                                    % by sort.label, year, title --- giving final bib. order.


FUNCTION {begin.bib}
{
  %% Write a comment identifying the style and version, suitably wrapped
  %% for possible later use in recovering a BibTeX .bib file (or other
  %% bibliography data base format file) from the .bbl file.
  %%
  %% At entry: stack = ... top:[top-string]
  %% At exit:  stack = ... top:[top-string]
  %%
  %% Global variables set: author.or.editor

  "% -*-TeX-*-" write$ newline$
  "%======================================================================="  write$ newline$
  "% Do NOT edit this file.  It was produced automatically from the" write$ newline$
  "% corresponding .aux file and one or more .bib files using" write$ newline$
  "% \bblstylename{xchicago.bst}" write$ newline$
  "% \bblstyleversion{5.0 (beta)}" write$ newline$
  "% \bblstyledate{23 Dec 1999}" write$ newline$
  "%======================================================================="  write$ newline$
  newline$

  preamble$ empty.or.unknown
    { }
    { preamble$ write$ newline$ }
  if$

  "\begin{thebibliography}{}" write$ newline$
  newline$

  "\ifx \xbblversion \undefined \input xbbl.sty \fi" write$ newline$

  "UNKNOWN" 'author.or.editor :=        % initialize to nonsense value, for safety and debugging
}


EXECUTE {begin.bib}


EXECUTE {init.state.consts}


ITERATE {call.type$}


FUNCTION {end.bib}
{
  %% Complete the output .bbl file with a newline, end-of-bibliography
  %% command, and a final newline.
  %%
  %% At entry: stack = empty
  %% At exit:  stack = empty

  newline$
  "\end{thebibliography}" write$ newline$
}


EXECUTE {end.bib}
