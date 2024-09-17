% =====================================================================
%% OUP-EJ.cls 2021/02/02 version V1.0
%%
%% The Economic Journal document class file for LaTeX2e
%%
%% Copyright (C) Oxford University Press
%% created by Integra Software Services Pvt. Ltd., Pondicherry, India 
%% 
%% OUP-EJ.cls is based on article.cls
%%
%% OUP-EJ.cls is created for manuscript preparation for Authors 
%% 
%%
%% =====================================================================

%%
\NeedsTeXFormat{LaTeX2e}[1995/12/01]
\ProvidesClass{OUP-EJ}
[2020/02/02 v1.0
Standard LaTeX document class]
\newcommand\@ptsize{}
\newif\if@restonecol

\DeclareOption{a4paper}
{\setlength\paperheight {297mm}%
\setlength\paperwidth  {210mm}}
\DeclareOption{a5paper}
{\setlength\paperheight {210mm}%
\setlength\paperwidth  {148mm}}
\DeclareOption{b5paper}
{\setlength\paperheight {250mm}%
\setlength\paperwidth  {176mm}}
\DeclareOption{letterpaper}
{\setlength\paperheight {11in}%
\setlength\paperwidth  {8.5in}}
\DeclareOption{legalpaper}
{\setlength\paperheight {14in}%
\setlength\paperwidth  {8.5in}}
\DeclareOption{executivepaper}
{\setlength\paperheight {10.5in}%
\setlength\paperwidth  {7.25in}}
\DeclareOption{landscape}
{\setlength\@tempdima   {\paperheight}%
\setlength\paperheight {\paperwidth}%
\setlength\paperwidth  {\@tempdima}}



\DeclareOption{twoside}{\@twosidetrue  \@mparswitchtrue}
\DeclareOption{draft}{\setlength\overfullrule{5pt}}
\DeclareOption{final}{\setlength\overfullrule{0pt}}
\DeclareOption{onecolumn}{\@twocolumnfalse}
\DeclareOption{twocolumn}{\@twocolumntrue}
\DeclareOption{leqno}{\input{leqno.clo}}
\DeclareOption{fleqn}{\input{fleqn.clo}}

\DeclareOption{openbib}{%
    \AtEndOfPackage{%
        \renewcommand\@openbib@code{%
            \advance\leftmargin\bibindent
            \itemindent -\bibindent
            \listparindent \itemindent
            \parsep \z@
        }%
        \renewcommand\newblock{\par}}%
}
\ExecuteOptions{letterpaper,twoside,onecolumn,final}
\ProcessOptions

%%%%%%%==============================
\renewcommand\normalsize{%
    \@setfontsize\normalsize\@xpt\@xiipt
    \abovedisplayskip 10\p@ \@plus2\p@ \@minus5\p@
    \abovedisplayshortskip \z@ \@plus3\p@
    \belowdisplayshortskip 6\p@ \@plus3\p@ \@minus3\p@
    \belowdisplayskip \abovedisplayskip
    \let\@listi\@listI}

\normalsize

\newcommand\small{%
    \@setfontsize\small\@ixpt{11}%
    \abovedisplayskip 8.5\p@ \@plus3\p@ \@minus4\p@
    \abovedisplayshortskip \z@ \@plus2\p@
    \belowdisplayshortskip 4\p@ \@plus2\p@ \@minus2\p@
    \def\@listi{\leftmargin\leftmargini
    \topsep 4\p@ \@plus2\p@ \@minus2\p@
    \parsep 2\p@ \@plus\p@ \@minus\p@
    \itemsep \parsep}%
    \belowdisplayskip \abovedisplayskip
}

\newcommand\footnotesize{%
    \@setfontsize\footnotesize\@viiipt{9.5}%
    \abovedisplayskip 6\p@ \@plus2\p@ \@minus4\p@
    \abovedisplayshortskip \z@ \@plus\p@
    \belowdisplayshortskip 3\p@ \@plus\p@ \@minus2\p@
    \def\@listi{\leftmargin\leftmargini
    \topsep 3\p@ \@plus\p@ \@minus\p@
    \parsep 2\p@ \@plus\p@ \@minus\p@
    \itemsep \parsep}%
    \belowdisplayskip \abovedisplayskip
}

\newcommand\scriptsize{\@setfontsize\scriptsize\@viipt\@viiipt}
\newcommand\tiny{\@setfontsize\tiny\@vpt\@vipt}
\newcommand\large{\@setfontsize\large\@xiipt{14}}
\newcommand\Large{\@setfontsize\Large\@xivpt{18}}
\newcommand\LARGE{\@setfontsize\LARGE\@xviipt{22}}
\newcommand\huge{\@setfontsize\huge\@xxpt{25}}
\newcommand\Huge{\@setfontsize\Huge\@xxvpt{30}}

\if@twocolumn
\setlength\parindent{1em}
\else
\setlength\parindent{15\p@}
\fi

\setlength\smallskipamount{3\p@ \@plus 1\p@ \@minus 1\p@}
\setlength\medskipamount{6\p@ \@plus 2\p@ \@minus 2\p@}
\setlength\bigskipamount{12\p@ \@plus 4\p@ \@minus 4\p@}
\setlength\headheight{12\p@}
\setlength\headsep   {25\p@}
\setlength\topskip   {10\p@}
\setlength\footskip{30\p@}
\setlength\maxdepth{.5\topskip}


\setlength\@tempdima{\paperwidth}
\addtolength\@tempdima{-2in}
\setlength\@tempdimb{400\p@}

\if@twocolumn
\ifdim\@tempdima>2\@tempdimb\relax
\setlength\textwidth{2\@tempdimb}
\else
\setlength\textwidth{\@tempdima}
\fi
\else
\ifdim\@tempdima>\@tempdimb\relax
\setlength\textwidth{\@tempdimb}
\else
\setlength\textwidth{\@tempdima}
\fi
\fi

\@settopoint\textwidth


\setlength\@tempdima{\paperheight}
\addtolength\@tempdima{-2in}
\addtolength\@tempdima{-1.5in}
\divide\@tempdima\baselineskip
\@tempcnta=\@tempdima
\setlength\textheight{\@tempcnta\baselineskip}

\addtolength\textheight{\topskip}
\if@twocolumn
\setlength\marginparsep {10\p@}
\else
\setlength\marginparsep{11\p@}
\fi

\setlength\marginparpush{5\p@}

\if@twoside
\setlength\@tempdima        {\paperwidth}
\addtolength\@tempdima      {-\textwidth}
\setlength\oddsidemargin    {.4\@tempdima}
\addtolength\oddsidemargin  {-1in}
\setlength\marginparwidth   {.6\@tempdima}
\addtolength\marginparwidth {-\marginparsep}
\addtolength\marginparwidth {-0.4in}
\else
\setlength\@tempdima        {\paperwidth}
\addtolength\@tempdima      {-\textwidth}
\setlength\oddsidemargin    {.5\@tempdima}
\addtolength\oddsidemargin  {-1in}
\setlength\marginparwidth   {.5\@tempdima}
\addtolength\marginparwidth {-\marginparsep}
\addtolength\marginparwidth {-0.4in}
\addtolength\marginparwidth {-.4in}
\fi
\ifdim \marginparwidth >2in
\setlength\marginparwidth{2in}
\fi
\@settopoint\oddsidemargin
\@settopoint\marginparwidth
\setlength\evensidemargin  {\paperwidth}
\addtolength\evensidemargin{-2in}
\addtolength\evensidemargin{-\textwidth}
\addtolength\evensidemargin{-\oddsidemargin}
\@settopoint\evensidemargin

\setlength\topmargin{\paperheight}
\addtolength\topmargin{-2in}
\addtolength\topmargin{-\headheight}
\addtolength\topmargin{-\headsep}
\addtolength\topmargin{-\textheight}
\addtolength\topmargin{-\footskip}     % this might be wrong!
\addtolength\topmargin{-.5\topmargin}
\@settopoint\topmargin

\setlength\footnotesep{6.65\p@}
\setlength{\skip\footins}{9\p@ \@plus 4\p@ \@minus 2\p@}
\setlength\floatsep    {12\p@ \@plus 2\p@ \@minus 2\p@}
\setlength\textfloatsep{20\p@ \@plus 2\p@ \@minus 4\p@}
\setlength\intextsep   {12\p@ \@plus 2\p@ \@minus 2\p@}
\setlength\dblfloatsep    {12\p@ \@plus 2\p@ \@minus 2\p@}
\setlength\dbltextfloatsep{20\p@ \@plus 2\p@ \@minus 4\p@}
\setlength\@fptop{0\p@ \@plus 1fil}
\setlength\@fpsep{8\p@ \@plus 2fil}
\setlength\@fpbot{0\p@ \@plus 1fil}
\setlength\@dblfptop{0\p@ \@plus 1fil}
\setlength\@dblfpsep{8\p@ \@plus 2fil}
\setlength\@dblfpbot{0\p@ \@plus 1fil}
\setlength\partopsep{2\p@ \@plus 1\p@ \@minus 1\p@}
\def\@listi{\leftmargin\leftmargini
\parsep 4\p@ \@plus2\p@ \@minus\p@
\topsep 8\p@ \@plus2\p@ \@minus4\p@
\itemsep4\p@ \@plus2\p@ \@minus\p@}
\let\@listI\@listi
\@listi
\def\@listii {\leftmargin\leftmarginii
\labelwidth\leftmarginii
\advance\labelwidth-\labelsep
\topsep    4\p@ \@plus2\p@ \@minus\p@
\parsep    2\p@ \@plus\p@  \@minus\p@
\itemsep   \parsep}
\def\@listiii{\leftmargin\leftmarginiii
\labelwidth\leftmarginiii
\advance\labelwidth-\labelsep
\topsep    2\p@ \@plus\p@\@minus\p@
\parsep    \z@
\partopsep \p@ \@plus\z@ \@minus\p@
\itemsep   \topsep}
\def\@listiv {\leftmargin\leftmarginiv
\labelwidth\leftmarginiv
\advance\labelwidth-\labelsep}
\def\@listv  {\leftmargin\leftmarginv
\labelwidth\leftmarginv
\advance\labelwidth-\labelsep}
\def\@listvi {\leftmargin\leftmarginvi
\labelwidth\leftmarginvi
\advance\labelwidth-\labelsep}


%%%%%%%==============================


\setlength\lineskip{1\p@}
\setlength\normallineskip{1\p@}
\renewcommand\baselinestretch{}
\setlength\parskip{0\p@ \@plus \p@}
\@lowpenalty   51
\@medpenalty  151
\@highpenalty 301
\setcounter{topnumber}{2}
\renewcommand\topfraction{.7}
\setcounter{bottomnumber}{1}
\renewcommand\bottomfraction{.3}
\setcounter{totalnumber}{3}
\renewcommand\textfraction{.2}
\renewcommand\floatpagefraction{.5}
\setcounter{dbltopnumber}{2}
\renewcommand\dbltopfraction{.7}
\renewcommand\dblfloatpagefraction{.5}


\def\ps@myheadings{%
    \let\@oddfoot\@empty\let\@evenfoot\@empty
    \def\@evenhead{\thepage\hfil\slshape\leftmark}%
    \def\@oddhead{{\slshape\rightmark}\hfil\thepage}%
    \let\@mkboth\@gobbletwo
    \let\sectionmark\@gobble
    \let\subsectionmark\@gobble
}


\setcounter{secnumdepth}{3}
\newcounter {part}
\newcounter {section}
\newcounter {subsection}[section]
\newcounter {subsubsection}[subsection]
\newcounter {paragraph}[subsubsection]
\newcounter {subparagraph}[paragraph]
\renewcommand \thepart {\@Roman\c@part}
\renewcommand \thesection {\@arabic\c@section}
\renewcommand\thesubsection   {\thesection.\@arabic\c@subsection}
\renewcommand\thesubsubsection{\thesubsection.\@arabic\c@subsubsection}
\renewcommand\theparagraph    {\thesubsubsection.\@arabic\c@paragraph}
\renewcommand\thesubparagraph {\theparagraph.\@arabic\c@subparagraph}
\newcommand\part{%
    \if@noskipsec \leavevmode \fi
    \par
    \addvspace{4ex}%
    \@afterindentfalse
    \secdef\@part\@spart}

\def\@part[#1]#2{%
    \ifnum \c@secnumdepth >\m@ne
    \refstepcounter{part}%
    \addcontentsline{toc}{part}{\thepart\hspace{1em}#1}%
    \else
    \addcontentsline{toc}{part}{#1}%
    \fi
    {\parindent \z@ \raggedright
    \interlinepenalty \@M
    \normalfont
    \ifnum \c@secnumdepth >\m@ne
    \Large\bfseries \partname\nobreakspace\thepart
    \par\nobreak
    \fi
    \huge \bfseries #2%
    \markboth{}{}\par}%
    \nobreak
    \vskip 3ex
    \@afterheading}
\def\@spart#1{%
        {\parindent \z@ \raggedright
    \interlinepenalty \@M
    \normalfont
    \huge \bfseries #1\par}%
    \nobreak
    \vskip 3ex
    \@afterheading}
\newcommand\section{\@startsection {section}{1}{\z@}%
{-3.5ex \@plus -1ex \@minus -.2ex}%
{2.3ex \@plus.2ex}%
{\normalfont\Large\bfseries}}
\newcommand\subsection{\@startsection{subsection}{2}{\z@}%
{-3.25ex\@plus -1ex \@minus -.2ex}%
{1.5ex \@plus .2ex}%
{\normalfont\large\bfseries}}
\newcommand\subsubsection{\@startsection{subsubsection}{3}{\z@}%
{-3.25ex\@plus -1ex \@minus -.2ex}%
{1.5ex \@plus .2ex}%
{\normalfont\normalsize\bfseries}}
\newcommand\paragraph{\@startsection{paragraph}{4}{\z@}%
{3.25ex \@plus1ex \@minus.2ex}%
{-1em}%
{\normalfont\normalsize\bfseries}}
\newcommand\subparagraph{\@startsection{subparagraph}{5}{\parindent}%
{3.25ex \@plus1ex \@minus .2ex}%
{-1em}%
{\normalfont\normalsize\bfseries}}
\if@twocolumn
\setlength\leftmargini  {2em}
\else
\setlength\leftmargini  {2.5em}
\fi
\leftmargin  \leftmargini
\setlength\leftmarginii  {2.2em}
\setlength\leftmarginiii {1.87em}
\setlength\leftmarginiv  {1.7em}
\if@twocolumn
\setlength\leftmarginv  {.5em}
\setlength\leftmarginvi {.5em}
\else
\setlength\leftmarginv  {1em}
\setlength\leftmarginvi {1em}
\fi
\setlength  \labelsep  {.5em}
\setlength  \labelwidth{\leftmargini}
\addtolength\labelwidth{-\labelsep}
\@beginparpenalty -\@lowpenalty
\@endparpenalty   -\@lowpenalty
\@itempenalty     -\@lowpenalty
\renewcommand\theenumi{\@arabic\c@enumi}
\renewcommand\theenumii{\@alph\c@enumii}
\renewcommand\theenumiii{\@roman\c@enumiii}
\renewcommand\theenumiv{\@Alph\c@enumiv}
\newcommand\labelenumi{\theenumi.}
\newcommand\labelenumii{(\theenumii)}
\newcommand\labelenumiii{\theenumiii.}
\newcommand\labelenumiv{\theenumiv.}
\renewcommand\p@enumii{\theenumi}
\renewcommand\p@enumiii{\theenumi(\theenumii)}
\renewcommand\p@enumiv{\p@enumiii\theenumiii}
\newcommand\labelitemi{\textbullet}
\newcommand\labelitemii{\normalfont\bfseries \textendash}
\newcommand\labelitemiii{\textasteriskcentered}
\newcommand\labelitemiv{\textperiodcentered}
\newenvironment{description}
{\list{}{\labelwidth\z@ \itemindent-\leftmargin
\let\makelabel\descriptionlabel}}
{\endlist}
\newcommand*\descriptionlabel[1]{\hspace\labelsep
\normalfont\bfseries #1}

\newenvironment{verse}
{\let\\\@centercr
\list{}{\itemsep      \z@
\itemindent   -1.5em%
\listparindent\itemindent
\rightmargin  \leftmargin
\advance\leftmargin 1.5em}%
\item\relax}
{\endlist}
\newenvironment{quotation}
{\list{}{\listparindent 1.5em%
\itemindent    \listparindent
\rightmargin   \leftmargin
\parsep        \z@ \@plus\p@}%
\item\relax}
{\endlist}
\newenvironment{quote}
{\list{}{\rightmargin\leftmargin}%
\item\relax}
{\endlist}

\newenvironment{titlepage}
{%
    \if@twocolumn
    \@restonecoltrue\onecolumn
    \else
    \@restonecolfalse\newpage
    \fi
    \thispagestyle{empty}%
    \setcounter{page}\@ne
    }%
    {\if@restonecol\twocolumn \else \newpage \fi
    \if@twoside\else
    \setcounter{page}\@ne
    \fi
}

\newcommand\appendix{\par
\setcounter{section}{0}%
\setcounter{figure}{0}%
\setcounter{equation}{0}%
\setcounter{subsection}{0}%
\setcounter{table}{0}%
\gdef\thesection{\@Alph\c@section}%
\gdef\thefigure{\thesection\@arabic\c@figure}%
\gdef\thetable{\thesection\@arabic\c@table}%
\gdef\theequation{\thesection.\@arabic\c@equation}}
\setlength\arraycolsep{5\p@}
\setlength\tabcolsep{6\p@}
\setlength\arrayrulewidth{.4\p@}
\setlength\doublerulesep{2\p@}
\setlength\tabbingsep{\labelsep}
\skip\@mpfootins = \skip\footins
\setlength\fboxsep{3\p@}
\setlength\fboxrule{.4\p@}
\renewcommand \theequation {\@arabic\c@equation}
\newcounter{figure}
\renewcommand \thefigure {\@arabic\c@figure}
\def\fps@figure{tbp}
\def\ftype@figure{1}
\def\ext@figure{lof}
\def\fnum@figure{\figurename\nobreakspace\thefigure}
\newenvironment{figure}
{\@float{figure}}
{\end@float}
\newenvironment{figure*}
{\@dblfloat{figure}}
{\end@dblfloat}
\newcounter{table}
\renewcommand\thetable{\@arabic\c@table}
\def\fps@table{tbp}
\def\ftype@table{2}
\def\ext@table{lot}
\def\fnum@table{\tablename\nobreakspace\thetable}
\newenvironment{table}
{\@float{table}}
{\end@float}
\newenvironment{table*}
{\@dblfloat{table}}
{\end@dblfloat}
\newlength\abovecaptionskip
\newlength\belowcaptionskip
\setlength\abovecaptionskip{10\p@}
\setlength\belowcaptionskip{10\p@}
\long\def\@makecaption#1#2{%
    \vskip\abovecaptionskip
    \sbox\@tempboxa{#1.\enspace {\itshape\selectfont#2}}%
    \ifdim \wd\@tempboxa >\hsize
    #1.\enspace {\itshape\selectfont#2}\par
    \else
    \global \@minipagefalse
    \hb@xt@\hsize{\hfil\box\@tempboxa\hfil}%
    \fi
    \vskip\belowcaptionskip}
\DeclareOldFontCommand{\rm}{\normalfont\rmfamily}{\mathrm}
\DeclareOldFontCommand{\sf}{\normalfont\sffamily}{\mathsf}
\DeclareOldFontCommand{\tt}{\normalfont\ttfamily}{\mathtt}
\DeclareOldFontCommand{\bf}{\normalfont\bfseries}{\mathbf}
\DeclareOldFontCommand{\it}{\normalfont\itshape}{\mathit}
\DeclareOldFontCommand{\sl}{\normalfont\slshape}{\@nomath\sl}
\DeclareOldFontCommand{\sc}{\normalfont\scshape}{\@nomath\sc}
\DeclareRobustCommand*\cal{\@fontswitch\relax\mathcal}
\DeclareRobustCommand*\mit{\@fontswitch\relax\mathnormal}
\newcommand\@pnumwidth{1.55em}
\newcommand\@tocrmarg{2.55em}
\newcommand\@dotsep{4.5}
\setcounter{tocdepth}{3}
\newcommand\tableofcontents{%
    \section*{\contentsname
    \@mkboth{%
        \MakeUppercase\contentsname}{\MakeUppercase\contentsname}}%
    \@starttoc{toc}%
}
\newcommand*\l@part[2]{%
    \ifnum \c@tocdepth >-2\relax
    \addpenalty\@secpenalty
    \addvspace{2.25em \@plus\p@}%
    \setlength\@tempdima{3em}%
    \begingroup
    \parindent \z@ \rightskip \@pnumwidth
    \parfillskip -\@pnumwidth
    {\leavevmode
    \large \bfseries #1\hfil
    \hb@xt@\@pnumwidth{\hss #2%
    \kern-\p@\kern\p@}}\par
    \nobreak
    \endgroup
    \fi}
\newcommand*\l@section[2]{%
    \ifnum \c@tocdepth >\z@
    \addpenalty\@secpenalty
    \addvspace{1.0em \@plus\p@}%
    \setlength\@tempdima{1.5em}%
    \begingroup
    \parindent \z@ \rightskip \@pnumwidth
    \parfillskip -\@pnumwidth
    \leavevmode \bfseries
    \advance\leftskip\@tempdima
    \hskip -\leftskip
    #1\nobreak\hfil
    \nobreak\hb@xt@\@pnumwidth{\hss #2%
    \kern-\p@\kern\p@}\par
    \endgroup
    \fi}
\newcommand*\l@subsection{\@dottedtocline{2}{1.5em}{2.3em}}
\newcommand*\l@subsubsection{\@dottedtocline{3}{3.8em}{3.2em}}
\newcommand*\l@paragraph{\@dottedtocline{4}{7.0em}{4.1em}}
\newcommand*\l@subparagraph{\@dottedtocline{5}{10em}{5em}}
\newcommand\listoffigures{%
    \section*{\listfigurename}%
    \@mkboth{\MakeUppercase\listfigurename}%
    {\MakeUppercase\listfigurename}%
    \@starttoc{lof}%
}
\newcommand*\l@figure{\@dottedtocline{1}{1.5em}{2.3em}}
\newcommand\listoftables{%
    \section*{\listtablename}%
    \@mkboth{%
        \MakeUppercase\listtablename}%
    {\MakeUppercase\listtablename}%
    \@starttoc{lot}%
}
\let\l@table\l@figure
\newdimen\bibindent
\setlength\bibindent{1.5em}
\newenvironment{thebibliography}[1]
{\enddoc@text
\newpage%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\section*{\refname}%
\@mkboth{\MakeUppercase\refname}{\MakeUppercase\refname}%
\list{\@biblabel{\@arabic\c@enumiv}}%
{\settowidth\labelwidth{\@biblabel{#1}}%
\leftmargin\labelwidth
\advance\leftmargin\labelsep
\@openbib@code
\usecounter{enumiv}%
\let\p@enumiv\@empty
\renewcommand\theenumiv{\@arabic\c@enumiv}}%
\sloppy
\clubpenalty4000
\@clubpenalty \clubpenalty
\widowpenalty4000%
\sfcode`\.\@m}
{\def\@noitemerr
{\@latex@warning{Empty `thebibliography' environment}}%
\endlist}
\newcommand\newblock{\hskip .11em\@plus.33em\@minus.07em}
\let\@openbib@code\@empty
\newenvironment{theindex}
{\if@twocolumn
\@restonecolfalse
\else
\@restonecoltrue
\fi
\twocolumn[\section*{\indexname}]%
\@mkboth{\MakeUppercase\indexname}%
{\MakeUppercase\indexname}%
\thispagestyle{plain}\parindent\z@
\parskip\z@ \@plus .3\p@\relax
\columnseprule \z@
\columnsep 35\p@
\let\item\@idxitem}
{\if@restonecol\onecolumn\else\clearpage\fi}
\newcommand\@idxitem{\par\hangindent 40\p@}
\newcommand\subitem{\@idxitem \hspace*{20\p@}}
\newcommand\subsubitem{\@idxitem \hspace*{30\p@}}
\newcommand\indexspace{\par \vskip 10\p@ \@plus5\p@ \@minus3\p@\relax}
\renewcommand\footnoterule{%
    \kern-3\p@
    \hrule\@width.4\columnwidth
    \kern2.6\p@}
\newcommand\@makefntext[1]{%
    \parindent 1em%
    \noindent
    {\@makefnmark}#1}
\newcommand\contentsname{Contents}
\newcommand\listfigurename{List of Figures}
\newcommand\listtablename{List of Tables}
\newcommand\refname{References}
\newcommand\indexname{Index}
\newcommand\figurename{Figure}
\newcommand\tablename{Table}
\newcommand\partname{Part}
\newcommand\appendixname{Appendix}
\newcommand\abstractname{Abstract}
\def\today{\ifcase\month\or
January\or February\or March\or April\or May\or June\or
July\or August\or September\or October\or November\or December\fi
\space\number\day, \number\year}
\setlength\columnsep{10\p@}
\setlength\columnseprule{0\p@}
\pagenumbering{arabic}
\if@twoside
\else
\raggedbottom
\fi
\if@twocolumn
\twocolumn
\sloppy
\flushbottom
\else
\onecolumn
\fi



\RequirePackage{etex}
\RequirePackage{amsmath}
\RequirePackage{amsfonts}
\RequirePackage{amssymb}
\RequirePackage{amsthm}
\RequirePackage{latexsym}
\RequirePackage{graphicx}
\RequirePackage{calc}
\RequirePackage{multicol}
\RequirePackage{array}
\RequirePackage{multirow}
\RequirePackage{booktabs}
\RequirePackage{fnpos}
\RequirePackage{framed}
\RequirePackage[figuresright]{rotating}
\RequirePackage{floatpag}
\RequirePackage{dcolumn}



\usepackage{setspace}
\usepackage{natbib}
\linespread{2}


%\def\@title{}
\def\@shorttitle{}
%\def\shorttitle#1{\gdef\@shorttitle{#1}}
\renewcommand\title[2][]{\def\@argt{#1}\ifx\@argt\@empty\gdef\@shorttitle{}\else\gdef\@shorttitle{#1}\fi\gdef\@title{#2}}

\def\ps@headings{%
    \let\@oddfoot\@empty\let\@evenfoot\@empty
    \def\@evenhead{\thepage\hfil \textit{The Economic Journal}}%
    \def\@oddhead{{\rightmark}\hfil\thepage}%
    \let\@mkboth\markboth
}

\pagestyle{headings}

%==========================================


%%====================================
%%=======AUTHOR and AFFIL DEFN========
%%====================================

\let\@xp=\expandafter
\let\@nx=\noexpand



\let\@xp=\expandafter
\let\@nx=\noexpand

\def\author{\@ifnextchar[{\@@author}{\@author}}


\def\@@author[#1]#2{%
    \ifx\@empty\authors
    \gdef\authors{#2$^{#1}$\kern-1pt}%
    \else
    \g@addto@macro\authors{\and#2$^{#1}$\kern-1pt}%
    \fi
}
%

\def\@author#1{%
    \ifx\@empty\authors%
    \gdef\authors{#1}%
    \else%
    \g@addto@macro\authors{\and#1}%
    \fi
}


\let\authors\@empty


\newcommand{\xandlist}[4]{\@andlista{{#1}{#2}{#3}}#4\and\and}
\def\@andlista#1#2\and#3\and{\@andlistc{#2}\@ifnotempty{#3}{%
    \@andlistb#1{#3}}}
\def\@andlistb#1#2#3#4#5\and{%
    \@ifempty{#5}{%
        \@andlistc{#2#4}%
    }{%
        \@andlistc{#1#4}\@andlistb{#1}{#3}{#3}{#5}%
    }}

\let\@andlistc\@iden

\newcommand{\nxandlist}[4]{%
    \def\@andlistc##1{\toks@\@xp{\the\toks@##1}}%
    \toks@{\toks@\@emptytoks \@andlista{{#1}{#2}{#3}}}%
    \the\@xp\toks@#4\and\and
    \edef#4{\the\toks@}%
    \let\@andlistc\@iden}


\def\@@and{and}

\newcommand{\andify}{%
    \nxandlist{\unskip, }{\unskip{} \@@and~}{\unskip, \@@and~}}
\def\and{\unskip{ }\@@and{ }\ignorespaces}


\def\@setauthors{\def\thanks{\protect\thanks@warning}%%
\begingroup
\author@andify\authors
\def\\{\protect\linebreak}%
{\authors}%
\endgroup
}

\def\author@andify{%
    \nxandlist {\unskip ,\penalty-1 \space\ignorespaces}%
    {\unskip {} \@@and~}%
    {\unskip ,\penalty-2 \space \@@and~}%
}



\def\affil{\@ifnextchar[{\@@affil}{\@affil}}

\def\@@affil[#1]#2{%
    \g@addto@macro\affils{\addsep\noindent$^{#1}$#2\def\addsep{\par\vskip0pt}}%
}

\def\@affil#1{%
    \g@addto@macro\affils{\addsep\noindent#1\def\addsep{\endgraf}}
}
%
\let\affils\@empty
\let\addsep\@empty


\def\corres{\@ifnextchar[{\@@corresp}{\@corresp}}

\newcounter{crspcount}

\def\@@corresp[#1]#2{%
    \g@addto@macro\corresps{\stepcounter{crspcount}\ifnum\c@crspcount=1$^{#1}$\crsptext\fi\corsep\noindent\ifnum\c@crspcount=1\else$^{#1}$\fi#2\def\corsep{\par\vskip0pt\hspace*{0.8em}}}%
}

\def\@corresp#1{\g@addto@macro\corresps{\corsep\stepcounter{crspcount}\ifnum\c@crspcount=1\crsptext\fi\noindent#1\def\corsep{\endgraf\hspace*{1em}}}
}


\def\crsptext{\noindent\textbf{Correspondence address:\ }}
\def\corsep{}
\def\corresps{}



\def\aunotes{\@ifnextchar[{\@@aunotes}{\@aunotes}}


\def\@@aunotes[#1]#2{%
    \g@addto@macro\aunotess{\aunotesep\noindent$^{#1}$#2\def\aunotesep{\par\vskip0pt}}%
}

\def\@aunotes#1{%
    \g@addto@macro\aunotess{\aunotesep\noindent#1\def\aunotesep{\endgraf}}
}

\def\aunotesep{}
\def\aunotess{}


%%====================================
%%=======AUTHOR and AFFIL DEFN========
%%====================================


\def\@sect#1#2#3#4#5#6[#7]#8{%
    \ifnum #2>\c@secnumdepth
    \let\@svsec\@empty
    \else
    \refstepcounter{#1}%
    \protected@edef\@svsec{\@seccntformat{#1}\relax}%
    \fi
    \@tempskipa #5\relax
    \ifdim \@tempskipa>\z@
    \begingroup
    #6{%
        \@hangfrom{\hskip #3\relax\@svsec}%
        \interlinepenalty \@M \raggedright#8\@@par}%
    \endgroup
    %\csname #1mark\endcsname{#7}%
    \addcontentsline{toc}{#1}{%
        \ifnum #2>\c@secnumdepth \else
        \protect\numberline{\csname the#1\endcsname}%
        \fi
        #7}%
    \else
    \def\@svsechd{%
        #6{\hskip #3\relax
        \@svsec #8}%
    %\csname #1mark\endcsname{#7}%
        \addcontentsline{toc}{#1}{%
            \ifnum #2>\c@secnumdepth \else
            \protect\numberline{\csname the#1\endcsname}%
            \fi
            #7}}%
    \fi
    \@xsect{#5}}

\def\@subjclass{}

\newcommand*\subjclass[2][]{\gdef\@subjclass{#2}}

\long\def\thanks@warning#1{%
    \ClassError{OUP-EJ}{%
        \protect\thanks\space should be given separately, not inside author name.%
        make it just before the \string\maketitle}\@ehb
}
\renewcommand{\thanks}[1]{%\thanks@warning{}%
    \g@addto@macro\thankses{#1}%
}

\let\thankses\@empty

\newcommand\maketitle{\par\thispagestyle{plain}
\begingroup
\renewcommand\thefootnote{\@fnsymbol\c@footnote}%
\def\@makefnmark{\rlap{\@textsuperscript{\normalfont\@thefnmark}}}%
\long\def\@makefntext##1{\parindent 1em\noindent
\hb@xt@1.8em{%
    \hss\@textsuperscript{\normalfont\@thefnmark}}##1}%
    \if@twocolumn
    \ifnum \col@number=\@ne
    \@maketitle
    \else
    \twocolumn[\@maketitle]%
    \fi
    \else
    \newpage
    \global\@topnum\z@   % Prevents figures from going at top of page.
    \@maketitle
    \ifx\@shorttitle\@empty\@latex@error{Short title not given^^J^^J include a short title (40 characters or less) beneath the full title^^J through the command \string\shorttitle{}}{}\fi
    \markright{\ifx\@shorttitle\@empty\@title\else\@shorttitle\fi}
    \fi
    \@thanks
    \endgroup
    \@afterheading\@afterindentfalse
    \setcounter{footnote}{0}%
    \global\let\thanks\relax
    \global\let\maketitle\relax
    \global\let\@maketitle\relax
    \global\let\@thanks\@empty
    \global\let\@author\@empty
    \global\let\@date\@empty
    \global\let\@title\@empty
    \global\let\title\relax
    \global\let\author\relax
    \global\let\date\relax
    \global\let\and\relax
}


\newif\ifnoshorttitle

\def\@maketitle{%
    \newpage
    {\centering%\begin{center}%
    \let \footnote \thanks
    {\linespread{1.5}\LARGE\bfseries\selectfont\mathversion{bold}\@title\par}%
    \vspace*{12pt}
    {\ifnoshorttitle\hbox{}\else\@shorttitle\endgraf\fi}
    \vspace*{12pt}
    {\ifx\corresps\@empty\else\def\thefootnote{}\footnotetext{\noindent\corresps}\fi%
    \ifx\aunotess\@empty\else\def\thefootnote{}\footnotetext{\aunotess}\fi%
    \ifx\thankses\@empty\else\def\thefootnote{}\footnotetext{\thankses}\fi
    }
    \vskip 1.5em%
            {\large
        %\lineskip .5em%
        \@setauthors\endgraf
        %\vspace*{12pt}
        \def\authors{}
        \def\affils{}
        \def\corresps{}
        }%
    }
    \linespread{2}
    \ifvoid\abstractbox%
    \else%
    \vspace*{24pt}
    {\noindent\noindent\small\unvbox\abstractbox\endgraf}%
    {\ifx\@keywords\@empty\else\vspace*{12pt}\small\noindent\textbf{Keywords:}\ \@keywords\endgraf\vspace*{12pt}\fi}
    \fi%
    \ifx\@subjclass\@empty\else\noindent\textbf{Classification:}\ \@subjclass\endgraf\fi
    \ifx\@dedication\@empty\else\vspace*{12pt}\noindent\textbf{\textit{\@dedication}}\endgraf\fi%
    \addvspace{24pt}
    \@afterheading\@afterindentfalse%
}


\newbox\abstractbox
\newenvironment{abstract}{%
    \ifx\maketitle\relax
    \ClassError{OUP-EJ}{Abstract coded after the \protect\maketitle\space it should be code^^J before the \protect\maketitle\space}%
    \fi
    \global\setbox\abstractbox=\vbox\bgroup
    \normalfont\noindent\ignorespaces\textbf{\abstractname:}\ \ignorespaces%
    }%
    {\egroup}%

\def\dedication#1{\ifx\maketitle\relax\ClassError{OUP-EJ}{Dedication coded after the \protect\maketitle\space it should be code^^J before the \protect\maketitle\space}\fi
\def\@dedication{#1}}
\let\@dedication=\@empty

\def\keywords#1{\ifx\maketitle\relax\ClassError{OUP-EJ}{Keywords coded after the \protect\maketitle\space it should be code^^J before the \protect\maketitle\space}%
\fi%
\def\@keywords{#1}}
\let\@keywords=\@empty


\emergencystretch12pt

\def\enddoc@text{
    \ifx\affils\@empty\endgraf\else{\par\bigskip\noindent\section*{Affiliations}\vspace*{12pt}\endgraf{\raggedright\affils}}\fi}
\AtEndDocument{}


\newenvironment{ack}{\section*{Acknowledgments}}
{}
\newenvironment{ackn}{\section*{Acknowledgment}}
{}


\thm@headfont{\scshape}

\theoremstyle{plain}
\newtheorem{assumption}{Assumption}
\newtheorem{theorem}{Theorem}
\newtheorem{corollary}{Corollary}
\newtheorem{lemma}{Lemma}
\newtheorem{proposition}{Proposition}
\newtheorem{conjecture}{Conjecture}
\newtheorem{definition}{Definition}


\theoremstyle{remark}
\newtheorem{remark}{Remark}
\newtheorem{example}{Example}
\newtheorem{note}{Note}[section]
\newtheorem{exercise}{Exercise}

\def\th@remark{%
    \thm@headfont{\scshape}%
    \normalfont % body font
    \thm@preskip\topsep \divide\thm@preskip\tw@
    \thm@postskip\thm@preskip
}



\AtBeginDocument{\renewcommand\bibsection{%
    \enddoc@text\let\affils\@empty\clearpage\section*{\refname}%
}}


\hbadness=10000
\vbadness=10000
\brokenpenalty=10000
\doublehyphendemerits=1000000
\finalhyphendemerits=1000000
\clubpenalty=10000
\widowpenalty=10000
\lefthyphenmin=2
\righthyphenmin=3



\endinput
%%
%% End of file `article.cls'.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%% Version History %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%Date%%          %%Description
===========================================================================    
02-02-2021| Initial release

