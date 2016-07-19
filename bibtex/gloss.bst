%%
%% This is file `gloss.bst',
%% generated with the docstrip utility.
%%
%% The original source files were:
%%
%% gloss.dtx  (with options: `bst')
%% 
%% This file is part of gloss, a package that uses BibTeX for creating
%% and sorting glossaries for LaTeX.
%% 
%% (c) 1998 Jose Luis Diaz
%% 
%%%%%%%%%%%%%%%%%%%%%   gloss.bst follows   %%%%%%%%%%%%%%%%%%%%%
%% This is a quick-and-dirty glossary style.  This style has one entry
%% type, GLOSSDEF, with `word' and `definition' as the two required
%% fields and `sort-word' as an optional field.  Among other
%% deficiencies, it doesn't give any warning messages for any required
%% fields that are missing.
%%
%% History:
%%   25-jun-97  (JLDdA) Original version, by Jose Luis Diaz de Arriba (and an
%%                      unindicted co-conspirator)

ENTRY
  { word
    sort-word
    definition
  }
  {}
  { label }

FUNCTION {output.glossitem}
{ newline$
  "\glossitem[" write$
  label write$
  "]{" write$
  cite$ write$
  "}" write$
  newline$
}

FUNCTION {fin.entry}
{ %add.period$
  write$
  newline$
}

FUNCTION {field.or.null}
{ duplicate$ empty$
    { pop$ "" }
    'skip$
  if$
}

FUNCTION {glossdef}
{ output.glossitem                      % this outputs the `word' via the label
  definition field.or.null
  fin.entry
}

FUNCTION {gd}                           % Synonim for "glossdef"
{ output.glossitem                      % this outputs the `word' via the label
  definition field.or.null
  fin.entry
}

FUNCTION {default.type} { glossdef }

READ

FUNCTION {sortify}
{ purify$
  "l" change.case$
}

FUNCTION {calc.label}                   % use the `word' to make the label
{ word field.or.null
  'label :=
}

FUNCTION {presort}
{ calc.label
  sort-word empty$                      % to sort,
    { word field.or.null }              % use `word' if `sort-word' is empty
    'sort-word
  if$
  sortify
  'sort.key$ :=
}

ITERATE { presort }

SORT

FUNCTION {inicio.gloss}
{ preamble$ empty$
    'skip$
    { preamble$ write$ newline$ }
  if$
  "\begin{thegloss}{}"               % label length irrelevant(?), hence {}
  write$ newline$
}

EXECUTE {inicio.gloss}

ITERATE {call.type$}

FUNCTION {fin.gloss}
{ newline$
  "\end{thegloss}" write$ newline$
}

EXECUTE {fin.gloss}
%%%%%%%%%%%%%%%%%%%%   gloss.bst has ended   %%%%%%%%%%%%%%%%%%%%
%% 
%%
%% End of file `gloss.bst'.
