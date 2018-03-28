
%%
%% This is file `prismdoc.cls'; there should be no need for modifications
%%

\def\fileversion{0.8}
\def\filedate{2004/07/26}
\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{prismdoc}[2004/07/26 For all prismdoc manuals]
\RequirePackage{ifthen}

\newboolean{IsItaIB}\setboolean{IsItaIB}{false}
\newboolean{IsItaRS}\setboolean{IsItaRS}{false}
\newboolean{IsItaDOCU}\setboolean{IsItaDOCU}{false}
\newboolean{IsItaZIK}\setboolean{IsItaZIK}{false}
\newboolean{IsItaZHB}\setboolean{IsItaZHB}{false}
\newboolean{OptSectoChap}\setboolean{OptSectoChap}{false}
\newboolean{OptChaptoSec}\setboolean{OptChaptoSec}{false}
\newboolean{OptToc}\setboolean{OptToc}{false}
\newboolean{OptNoArabic}\setboolean{OptNoArabic}{false}
\newboolean{OptIndex}\setboolean{OptIndex}{false}
\newboolean{OptFigureList}\setboolean{OptFigureList}{false}
\newboolean{OptTableList}\setboolean{OptTableList}{false}
\newboolean{OptExampleList}\setboolean{OptExampleList}{false}
\newboolean{OptProgramList}\setboolean{OptProgramList}{false}
\newboolean{OptJobList}\setboolean{OptJobList}{false}
\newboolean{OptOldFloat}\setboolean{OptOldFloat}{false}
\newboolean{OptLow}\setboolean{OptLow}{false}
\newboolean{OptWide}\setboolean{OptWide}{false}
\newboolean{OptNocopy}\setboolean{OptNocopy}{false}
\newboolean{OptTitleonly}\setboolean{OptTitleonly}{false}

\newcommand{\PRISMDOCZikTitle}{PRISM-interne Kurzinformation}
\newcommand{\PRISMDOCZhbTitle}{PRISM-internes Handbuch}
\newcommand{\PRISMDOCIbTitle}{Interner Bericht}
\newcommand{\PRISMDOCtype}{}
\newcommand{\PRISMDOCDebugBox}[1]{}
\newcommand{\PRISMDOCFont}[6]{\fontencoding{#1}\fontfamily{#2}\fontseries{#3}
                               \fontshape{#4}\fontsize{#5}{#6}\selectfont}

\DeclareOption*{%
  \typeout{PRISMDOC: \CurrentOption\ passed to base class}
  \PassOptionsToClass{\CurrentOption}{article}%
  \PassOptionsToClass{\CurrentOption}{report}%
  }
\DeclareOption{a4paper}{\typeout{PRISMDOC:
    {\CurrentOption}  will not be used}}
\DeclareOption{a5paper}{\typeout{PRISMDOC:
    {\CurrentOption}  will not be used}}
\DeclareOption{sectochap}{
  \setboolean{OptSectoChap}{true}
  }
\DeclareOption{chaptosec}{
  \setboolean{OptChaptoSec}{true}
  }
\DeclareOption{rs}{
% middle part of Doc Type:e.g.Report
  \renewcommand{\PRISMDOCtype}{}
  \setboolean{IsItaRS}{true}
  \setboolean{OptChaptoSec}{false}
  }
\DeclareOption{tki}{
  \renewcommand{\PRISMDOCtype}{TKI}
  }

\DeclareOption{zik}{
  \renewcommand{\PRISMDOCtype}{ZIK}
  \setboolean{IsItaZIK}{true}
  }
\DeclareOption{zhb}{
  \renewcommand{\PRISMDOCtype}{ZHB}
  \setboolean{IsItaRS}{true}
  \setboolean{IsItaZHB}{true}
  }
\DeclareOption{ib}{
  \renewcommand{\PRISMDOCtype}{IB}
  \setboolean{IsItaRS}{true}
  }
\DeclareOption{toc}{
  \setboolean{OptToc}{false}
  }
\DeclareOption{noarabic}{
  \setboolean{OptNoArabic}{true}
  }
\DeclareOption{lof}{
  \setboolean{OptFigureList}{false}
  }
\DeclareOption{lot}{
  \setboolean{OptTableList}{false}
  }
\DeclareOption{loe}{
  \setboolean{OptExampleList}{false}
  }
\DeclareOption{lop}{
  \setboolean{OptProgramList}{false}
  }
\DeclareOption{loj}{
  \setboolean{OptJobList}{false}
  }
\DeclareOption{english}{
  \renewcommand{\PRISMDOCZikTitle}{PRISM internal Technical Reference}
  \renewcommand{\PRISMDOCZhbTitle}{PRISM Internal User's Manual}
  \renewcommand{\PRISMDOCIbTitle}{Technical Report}
  }
\DeclareOption{index}{
  \setboolean{OptIndex}{true}
  }
\DeclareOption{nocopy}{
  \setboolean{OptNocopy}{true}
  }
\DeclareOption{docurs}{
  \renewcommand{\PRISMDOCtype}{Report}
  \setboolean{IsItaRS}{true}
  \setboolean{IsItaDOCU}{true}
  }
\DeclareOption{docu}{
  \setboolean{IsItaDOCU}{true}
  }
\DeclareOption{titleonly}{
  \setboolean{OptTitleonly}{true}
  }
\DeclareOption{overfull}{
  \setlength{\overfullrule}{2pt}
  }
\DeclareOption{debug}{
  \renewcommand{\PRISMDOCDebugBox}[1]{#1}
  }
\DeclareOption{oldfloat}{
  \setboolean{OptOldFloat}{true}
  }
\DeclareOption{low}{
  \setboolean{OptLow}{true}
  }
\DeclareOption{wide}{
  \setboolean{OptWide}{true}
  }
\ProcessOptions\relax
\ifthenelse{\boolean{IsItaRS}}
{%
  \LoadClass[twoside,11pt]{report}
}% else
{
  {
    \typeout{PRISMDOC: No document type found, assuming RS}
    \LoadClass[twoside,11pt]{report}
    }
}
\RequirePackage{varioref}
\RequirePackage{times}
\RequirePackage{tabularx}
\RequirePackage{fancyheadings}
\RequirePackage{float}
\RequirePackage[hang,bf,small,nooneline]{caption}
\RequirePackage{expdlist}
\RequirePackage{varioref}
\RequirePackage{multicol}
\RequirePackage{changebar}
\RequirePackage{alltt}
\newcolumntype{C}{>{\centering\arraybackslash}X}%
\newcommand{\htmladdnormallink}[2]{#1}
%\RequirePackage[dvips]{epsfig}
\ifthenelse{\boolean{OptIndex}}%
{
  \RequirePackage{makeidx}
}{}
\ifthenelse{\boolean{OptChaptoSec}}
{%
\newcommand\chapter{\@startsection {section}{1}{\z@}%
                                   {-3.5ex \@plus -1ex \@minus -.2ex}%
                                   {2.3ex \@plus.2ex}%
                                   {\normalfont\Large\bfseries}}
\renewcommand\section{\@startsection{subsection}{2}{\z@}%
                                     {-3.25ex\@plus -1ex \@minus -.2ex}%
                                     {1.5ex \@plus .2ex}%
                                     {\normalfont\large\bfseries}}
\renewcommand\subsection{\@startsection{subsubsection}{3}{\z@}%
                                     {-3.25ex\@plus -1ex \@minus -.2ex}%
                                     {1.5ex \@plus .2ex}%
                                     {\normalfont\normalsize\bfseries}}
\renewcommand\subsubsection{\@startsection{paragraph}{4}{\z@}%
                                    {3.25ex \@plus1ex \@minus.2ex}%
                                    {-1em}%
                                    {\normalfont\normalsize\bfseries}}
\renewcommand\paragraph{\@startsection{subparagraph}{5}{\parindent}%
                                       {3.25ex \@plus1ex \@minus .2ex}%
                                       {-1em}%
                                      {\normalfont\normalsize\bfseries}}
\renewcommand\subparagraph{\@startsection{subparagraph}{5}{\parindent}%
                                       {3.25ex \@plus1ex \@minus .2ex}%
                                       {-1em}%
                                      {\normalfont\normalsize\bfseries}}
}{}
\ifthenelse{\boolean{OptSectoChap}}
{%
\renewcommand{\section}{\if@openright\cleardoublepage\else\clearpage\fi
                    \thispagestyle{plain}%
                    \global\@topnum\z@
                    \@afterindentfalse
                    \secdef\@chapter\@schapter}
\renewcommand{\subsection}{\@startsection {section}{1}{\z@}%
                                   {-3.5ex \@plus -1ex \@minus -.2ex}%
                                   {2.3ex \@plus.2ex}%
                                   {\normalfont\Large\bfseries}}
\renewcommand{\subsubsection}{\@startsection{subsection}{2}{\z@}%
                                     {-3.25ex\@plus -1ex \@minus -.2ex}%
                                     {1.5ex \@plus .2ex}%
                                     {\normalfont\large\bfseries}}
\renewcommand{\paragraph}{\@startsection{subsubsection}{3}{\z@}%
                                     {-3.25ex\@plus -1ex \@minus -.2ex}%
                                     {1.5ex \@plus .2ex}%
                                     {\normalfont\normalsize\bfseries}}
\renewcommand{\subparagraph}{\@startsection{paragraph}{4}{\z@}%
                                    {3.25ex \@plus1ex \@minus.2ex}%
                                    {-1em}%
                                    {\normalfont\normalsize\bfseries}}
}{}
\voffset=-1in
\hoffset=-1in
\ifthenelse{\boolean{OptWide}}%
{ %
  \textwidth=16.2cm
  \oddsidemargin=2.4cm
  \evensidemargin=2.4cm
}{ % else
  \textwidth=15.2cm
  \oddsidemargin=2.9cm
  \evensidemargin=2.9cm
}
\topmargin=1.4cm
\headheight=0.8cm
\headsep=0.3cm
\textheight=24.5cm
\footskip=1.3cm
\marginparwidth=1cm
\setcounter{topnumber}{3}
\setcounter{bottomnumber}{3}
\setcounter{totalnumber}{6}
\renewcommand{\topfraction}{0.8}
\renewcommand{\bottomfraction}{0.8}
\renewcommand{\textfraction}{0.1}
\setlength\parskip{0.5ex \@plus 0.2ex}
\setlength\parindent{0pt}
\def\@listI{\leftmargin\leftmargini
            \parsep \parskip
            \topsep \z@
            \itemsep\z@}
\let\@listi\@listI
\@listi
\def\@listii {\leftmargin\leftmarginii
              \labelwidth\leftmarginii
              \advance\labelwidth-\labelsep
              \topsep    \z@
              \parsep    \parskip
              \itemsep   \z@}
\def\@listiii{\leftmargin\leftmarginiii
              \labelwidth\leftmarginiii
              \advance\labelwidth-\labelsep
              \topsep    \z@
              \parsep    \parskip
              \partopsep \z@
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
\hfuzz=2mm
\newcommand{\PRISMDOCnum}{}
\newcommand{\PRISMDOCautor}{}
\newcommand{\PRISMDOCfname}{}
\newcommand{\PRISMDOCsname}{}
\newcommand{\PRISMDOCfphone}{}
\newcommand{\PRISMDOCsphone}{}
\newcommand{\PRISMDOCfwp}{}
\newcommand{\PRISMDOCfaff}{}
\newcommand{\PRISMDOCsaff}{}
\newcommand{\PRISMDOCfem}{}
\newcommand{\PRISMDOCswp}{}
\newcommand{\PRISMDOCdate}{}
\newcommand{\PRISMDOCversion}{}
\newcommand{\PRISMDOCtitel}{}
\newcommand{\PRISMDOCremark}{}
\newcommand{\PRISMDOCfigsize}{}
\newcommand{\prismdocType}[1]{\renewcommand{\PRISMDOCtype}{#1}}
\newcommand{\prismdocNum}[1]{\renewcommand{\PRISMDOCnum}{#1}}
\newcommand{\prismdocAutor}[1]{\renewcommand{\PRISMDOCautor}{#1}}
\newcommand{\prismdocFName}[1]{\renewcommand{\PRISMDOCfname}{#1}}
\newcommand{\prismdocSName}[1]{\renewcommand{\PRISMDOCsname}{#1}}
\newcommand{\prismdocFPhone}[1]{\renewcommand{\PRISMDOCfphone}{#1}}
\newcommand{\prismdocSPhone}[1]{\renewcommand{\PRISMDOCsphone}{#1}}
\newcommand{\prismdocFWp}[1]{\renewcommand{\PRISMDOCfwp}{#1}}
\newcommand{\prismdocSWp}[1]{\renewcommand{\PRISMDOCswp}{#1}}
\newcommand{\prismdocFAff}[1]{\renewcommand{\PRISMDOCfaff}{#1}}
\newcommand{\prismdocSAff}[1]{\renewcommand{\PRISMDOCsaff}{#1}}
\newcommand{\prismdocFEm}[1]{\renewcommand{\PRISMDOCfem}{#1}}
\newcommand{\prismdocVersion}[1]{\renewcommand{\PRISMDOCversion}{#1}}
\newcommand{\prismdocDate}[1]{\renewcommand{\PRISMDOCdate}{#1}}
\newcommand{\prismdocTitel}[1]{\renewcommand{\PRISMDOCtitel}{#1}}
\newcommand{\prismdocRemark}[1]{\renewcommand{\PRISMDOCremark}{#1}}
\newcommand{\prismdocFigSize}[1]{\renewcommand{\PRISMDOCfigsize}{#1}}

\newcommand{\prismdocHeader}{}
\newcommand{\prismdocTOC}{}

\ifthenelse{\boolean{IsItaRS}}
{%
  \pagestyle{fancy}
  \lhead[{\bfseries\thepage}]{\bfseries\rightmark}
  \rhead[{\bfseries\leftmark}]{\bfseries\thepage}
  \lfoot[{}]{}
  \cfoot{}{}
  \setlength{\headrulewidth}{0.4pt}
  \ifthenelse{\boolean{OptIndex}}
  {%
    \makeindex
    }{}
  \renewcommand{\prismdocHeader}{%
    \ifthenelse{\boolean{IsItaDOCU}}{
    %  Title page
      \thispagestyle{empty}
      \setlength{\unitlength}{1cm}
      \ifthenelse{\boolean{OptWide}}%
      {\hspace*{5mm}}{}%
      \newpage
      \thispagestyle{empty}
      \ \ \\
      \newpage
      }{}
    \thispagestyle{empty}
    \setlength{\unitlength}{1cm}
    \ifthenelse{\boolean{OptWide}}%
    {\hspace*{5mm}}{}%
    \begin{picture}(15.2,24.5)
      \ifthenelse{\not\(\boolean{IsItaIB}\or \boolean{IsItaDOCU}\)}%
      {
        \PRISMDOCDebugBox{\put(0,0){\dashbox{0.1}(15.2,24.5){}}}
        \PRISMDOCDebugBox{\put(2.5,8.1){\dashbox{0.1}(10.3,10.3){}}}
        \PRISMDOCDebugBox{\put(1.6,22.1){\dashbox{0.1}(12,2){}}}
        % \put(1.6,22.1){\makebox(12,2){\parbox{12cm}{
        %       \begin{center}
        %         \vspace*{-\topsep}
        %       {\PRISMDOCFont{T1}{ptm}{b}{n}{14pt}{18pt}
        %         PRISM \\} 
        %         % Project for Integrated Earth System Modelling \\ }
        %       {\PRISMDOCFont{T1}{ptm}{b}{n}{12pt}{14pt}
        %         An Infrastructure Project for Climate Research in Europe \par}
        %         %funded by the European Commission \\
        %         %under Contract  EVR1-CT2001-40012 \par
        %         %}
        %       \vspace*{-\topsep}
        %     \end{center}}}}
                                % Logo
      \PRISMDOCDebugBox{\put(4.35,18.8){\dashbox{0.1}(6.5,0.5)[t]{}}}
      \put(4.35,18.8){\makebox(6.5,0.5)[t]{
          \parbox{6.5cm}{
             \begin{center}
               \vspace*{-\topsep}
                \centering
                 \includegraphics[width=8cm]{figures/logo_cerfacs_cnrs}
               \vspace*{-\topsep}
             \end{center}
                 }}}
                                % Title
        \PRISMDOCDebugBox{\put(2.8,9.3){\dashbox{0.1}(9.6,2.5)[t]{}}}
        \put(2.8,9.3){\makebox(9.6,2.5)[t]{
            \parbox{9cm}{
              \begin{center}
                \vspace*{-\topsep}
              {\PRISMDOCFont{T1}{ptm}{b}{n}{14pt}{18pt}
                \PRISMDOCtitel\par}
              \vspace*{-\topsep}
            \end{center}}}}
                                % Authors
      \PRISMDOCDebugBox{\put(3.1,7.3){\dashbox{0.1}(9,1.5)[t]{}}}
      \put(3.1,7.3){\makebox(9,1.5)[t]{
          \parbox{15cm}{
            \begin{center}
              \vspace*{-\topsep}
              {\PRISMDOCFont{T1}{ptm}{m}{sl}{11pt}{12pt}
                \PRISMDOCautor\par}
              \vspace*{-\topsep}
            \end{center}}}}
%                                 % Volume
%       \PRISMDOCDebugBox{\put(5.35,3.3){\dashbox{0.1}(4.5,0.4)[t]{}}}
%       \put(5.35,3.3){\makebox(4.5,0.4)[t]{
%           \parbox{4.5cm}{
%             \begin{center}
%               \vspace*{-\topsep}
%               {\PRISMDOCFont{T1}{ptm}{m}{n}{12pt}{14pt}
%                 PRISM-\PRISMDOCtype-\PRISMDOCnum}
%               \vspace*{-\topsep}
%             \end{center}}}}
                                % Edition
      \PRISMDOCDebugBox{\put(4.35,1.1){\dashbox{0.1}(6.5,0.4)[t]{}}}
      \put(4.35,1.1){\makebox(6.5,0.4)[t]{
          \parbox{6.5cm}{
            \begin{center}
              \vspace*{-\topsep}
              {\PRISMDOCFont{T1}{ptm}{m}{n}{12pt}{14pt}
                \PRISMDOCversion}
              \vspace*{-\topsep}
            \end{center}}}}
                                % Last changed: ??
      \PRISMDOCDebugBox{\put(4.35,2.3){\dashbox{0.1}(6.5,0.5)[t]{}}}
%S.L. left coordinate of box=\put(N.N,...)
%S.L. height of lower frame=\put(...,N.N)
%S.L. width of text box= \parbox{N.Ncm} 
\put(4.35,0.3){\makebox(6.5,0.5)[t]{ 
          \parbox{6.5cm}{   
            \begin{center}
              \vspace*{-\topsep}
              {\PRISMDOCFont{T1}{ptm}{m}{n}{12pt}{14pt}
                \PRISMDOCdate }
              \vspace*{-\topsep}
            \end{center}}}}
                                % Notes
      \PRISMDOCDebugBox{\put(0,1.4){\dashbox{0.1}(13,1.5)[t]{}}}
      \put(0,1.4){\makebox(13,1.5)[t]{
          \parbox{13cm}{
            {\PRISMDOCFont{T1}{ptm}{m}{n}{10pt}{12pt}\PRISMDOCremark}}}}
      }{ % else
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                                % IB: RS oder ZHB
      \PRISMDOCDebugBox{\put(3.1,14.3){\dashbox{0.1}(9,0.9)[t]{}}}
      \put(3.1,14.3){\makebox(9,0.9)[t]{
          \parbox{9cm}{
            \begin{center}
              \vspace*{-\topsep}
              {\PRISMDOCFont{T1}{ptm}{m}{n}{12pt}{14pt}
                \ifthenelse{\boolean{IsItaZHB}}%
                  {\PRISMDOCZhbTitle}%
                  {%
                    \ifthenelse{\boolean{IsItaIB}}%
                    {\PRISMDOCIbTitle}%
                    }%
                  }%
                \vspace*{-\topsep}
              \end{center}}}}
                                % IB: Title
        \PRISMDOCDebugBox{\put(2.6,10.8){\dashbox{0.1}(10.0,2.5)[t]{}}}
        \put(2.6,10.8){\makebox(10.0,2.5)[t]{
            \parbox{9cm}{
              \begin{center}
                \vspace*{-\topsep}
              {\PRISMDOCFont{T1}{ptm}{b}{n}{14pt}{18pt}
                \PRISMDOCtitel\par}
              \vspace*{-\topsep}
            \end{center}}}}
                                % IB: Authors
      \PRISMDOCDebugBox{\put(3.1,8.0){\dashbox{0.1}(9,1.5)[t]{}}}
      \put(3.1,8.0){\makebox(9,1.5)[t]{
          \parbox{9cm}{
            \begin{center}
              \vspace*{-\topsep}
              {\PRISMDOCFont{T1}{ptm}{m}{sl}{11pt}{12pt}
                \PRISMDOCautor\par}
              \vspace*{-\topsep}
            \end{center}}}}
%                                 % IB: Volume
%       \PRISMDOCDebugBox{\put(5.35,7.0){\dashbox{0.1}(4.5,0.4)[t]{}}}
%       \put(5.35,7.0){\makebox(4.5,0.4)[t]{
%           \parbox{4.5cm}{
%             \begin{center}
%               \vspace*{-\topsep}
%               {\PRISMDOCFont{T1}{ptm}{m}{n}{12pt}{14pt}
%                 PRISM-\PRISMDOCtype-\PRISMDOCnum}
%               \vspace*{-\topsep}
%             \end{center}}}}
                                % IB: Edition
      \PRISMDOCDebugBox{\put(4.35,5.1){\dashbox{0.1}(6.5,0.4)[t]{}}}
      \put(4.35,5.1){\makebox(6.5,0.4)[t]{
          \parbox{6.5cm}{
            \begin{center}
              \vspace*{-\topsep}
              {\PRISMDOCFont{T1}{ptm}{m}{n}{12pt}{14pt}
                \PRISMDOCversion}
              \vspace*{-\topsep}
            \end{center}}}}
                                % IB: Last changed
      \PRISMDOCDebugBox{\put(4.35,6.3){\dashbox{0.1}(6.5,0.5)[t]{}}}
      \put(4.35,6.3){\makebox(6.5,0.5)[t]{
          \parbox{6.5cm}{
            \begin{center}
              \vspace*{-\topsep}
              {\PRISMDOCFont{T1}{ptm}{m}{n}{12pt}{14pt}
                (last change: \PRISMDOCdate)}
              \vspace*{-\topsep}
            \end{center}}}}
                                % IB: Remark
      \PRISMDOCDebugBox{\put(0,1.4){\dashbox{0.1}(13,1.5)[t]{}}}
      \put(0,1.4){\makebox(13,1.5)[t]{
          \parbox{15cm}{
            {\PRISMDOCFont{T1}{ptm}{m}{n}{10pt}{12pt}\PRISMDOCremark}}}}
      }
      \end{picture}
\newpage
    \thispagestyle{empty}
    \ifthenelse{\boolean{OptWide}}%
    {\hspace*{5mm}}{}
    \begin{picture}(15.2,24.5)
                                % Body for Text
    \PRISMDOCDebugBox{\put(0,0){\dashbox{0.1}(15.2,24.9){}}}

    \ifthenelse{\boolean{IsItaIB}}{}%else
    {
    \PRISMDOCDebugBox{\put(0,21.4){\dashbox{0.1}(15,3.4)[t]{}}}
    \ifthenelse{\boolean{OptNocopy}}{}%else
    {
      \put(0,21.4){\makebox(15,3.4)[t]{
          \parbox{15cm}{
            {\PRISMDOCFont{T1}{ptm}{b}{n}{12pt}{14pt}Copyright Notice\\[0.5ex]}
            \PRISMDOCFont{T1}{ptm}{m}{n}{11pt}{13pt}
            \copyright\ Copyright 2015 by CERFACS \\
            All rights reserved.\\
            No parts of this document should be either reproduced or 
            commercially used without prior agreement 
            by CERFACS representatives.  \\
            }}}
      }

      \PRISMDOCDebugBox{\put(0,9){\dashbox{0.1}(15,5.2)[t]{}}}
      \put(0,9){\makebox(15,5.2)[t]{
          \parbox{15cm}{
            {\PRISMDOCFont{T1}{ptm}{b}{n}{12pt}{14pt}
              How to get documentation ?\\[0.5ex]}
            \PRISMDOCFont{T1}{ptm}{m}{n}{11pt}{13pt}
            The documentation can be downloaded from the OASIS web site under the URL : \\
            http://oasis.enes.org \\
            }}}

      \PRISMDOCDebugBox{\put(0,16.2){\dashbox{0.1}(15,5.2)[t]{}}}
      \put(0,16.2){\makebox(15,5.2)[t]{
          \parbox{15cm}{
            {\PRISMDOCFont{T1}{ptm}{b}{n}{12pt}{14pt}
              How to get assistance?\\[0.5ex]}
            \PRISMDOCFont{T1}{ptm}{m}{n}{11pt}{13pt}
            Assistance can be obtained as listed below. 
            }}}

      \PRISMDOCDebugBox{\put(0,14.4){\dashbox{0.1}(15,2.2)[t]{}}}
      \put(0,16.6){\makebox(15,2.2)[t]{
          \parbox{15cm}{
            {\PRISMDOCFont{T1}{ptm}{b}{n}{12pt}{14pt}
              Phone Numbers and Electronic Mail Adresses
              \\[0.5ex]}
%            \PRISMDOCFont{T1}{ptm}{m}{n}{11pt}{13pt}
%            Electronic mail adresses of the individual work packages are composed as follows :\\
%            \textbf{prism\_ \textit{work package} @prism.enes.org}
            }}}

      \PRISMDOCDebugBox{\put(0,4.1){\dashbox{0.1}(15,11.0)[t]{}}}
      \put(0,7.1){\makebox(15,11.0)[t]{
          \parbox{15cm}{
            \newcommand{\PRISMDOCtblbf}{\PRISMDOCFont{T1}{ptm}{b}{n}{9.8pt}{11pt}}
            \newcommand{\PRISMDOCtblit}{\PRISMDOCFont{T1}{ptm}{m}{it}{9.8pt}{11pt}}
            \newcommand{\PRISMDOCtblbi}{\PRISMDOCFont{T1}{ptm}{b}{it}{9.8pt}{11pt}}
            \PRISMDOCFont{T1}{ptm}{m}{n}{9.8pt}{11pt}
            \renewcommand{\arraystretch}{1.11}
            \setlength{\tabcolsep}{4pt}
            \begin{tabular}[t]{|p{4.6cm}|p{3.1cm}|p{2.2cm}|p{3.1cm}|}
              \hline%\rowcolor[gray]{0.2}
              \PRISMDOCtblbf Name&
              \PRISMDOCtblbf Phone &
              \PRISMDOCtblbf Affiliation & 
              \PRISMDOCtblbf e-mail \\ 
              \hline\hline
              \PRISMDOCfname\par & \PRISMDOCfphone\par & \PRISMDOCfaff\par & \PRISMDOCfem\par \\
              \PRISMDOCsname\par & \PRISMDOCsphone\par & \PRISMDOCsaff\par & \par \\
              \hline
            \end{tabular}
            }}}

     \PRISMDOCDebugBox{\put(0,-0.5){\dashbox{0.1}(15,4.0)[b]{}}}
      \put(0,-0.5){\makebox(15,4.0)[b]{
          \parbox{15cm}{
            \renewcommand{\arraystretch}{1.10}
            \PRISMDOCFont{T1}{ptm}{m}{n}{9.8pt}{11pt}
            }}}
      } % IsItaIB
    \end{picture}
    \newpage
    \pagenumbering{roman}
    \setcounter{page}{1}
    \ifthenelse{\boolean{OptTitleonly}}{}%
    {
      {
        \parskip=0pt
        \tableofcontents
        \ifthenelse{\boolean{OptFigureList}}{
          \cleardoublepage
          \listoffigures
          }{}
        \ifthenelse{\boolean{OptTableList}}{
          \cleardoublepage
          \listoftables
          }{}
        \ifthenelse{\boolean{OptExampleList}}{
          \cleardoublepage
          \listof{Example_int}{List of Examples}
          }{}
        \ifthenelse{\boolean{OptProgramList}}{
          \cleardoublepage
          \listof{Program_int}{List of Programs}
          }{}
        \ifthenelse{\boolean{OptJobList}}{
          \cleardoublepage
          \listof{Job_int}{List of Jobs}
          }{}
        }
      \cleardoublepage
      \ifthenelse{\not\boolean{OptNoArabic}}{
        \pagenumbering{arabic}
      }{}
      \cleardoublepage
      }
    }
}{}


\newcommand{\Indextt}[1]{\index{#1@\texttt{#1}}}
\newlength{\PRISMDOCFrameskip}
\setlength{\PRISMDOCFrameskip}{3pt}
\newsavebox{\ffbox}
\newlength{\ffboxl}
\newenvironment{Frame}[1][\linewidth]{ %
\begin{lrbox}{\ffbox}
  \setlength{\ffboxl}{#1}
  \addtolength{\ffboxl}{-4\fboxrule}
  \addtolength{\ffboxl}{-4\fboxsep}
  \begin{minipage}[t]{\ffboxl}
}                                    %
{                                    %
  \end{minipage}
\end{lrbox}
\ifthenelse{\boolean{hmode}}{
  \hskip\parfillskip\\[\PRISMDOCFrameskip]
}{\vspace{\PRISMDOCFrameskip}}
\noindent\fbox{\usebox{\ffbox}\\}%
\vspace{\PRISMDOCFrameskip}%
}%
\newcommand{\Mail}[1]{\textbf{#1}}
\newcommand{\Internet}[1]{\textbf{\texttt{#1}}}
\newcommand{\Url}[1]{\textbf{\texttt{#1}}}
\newcommand{\Ftp}[1]{\textbf{\texttt{#1}}}
\newcommand{\Unixcmd}[1]{\textbf{\texttt{#1}}}
\newcommand{\Unixopt}[1]{\textbf{\texttt{#1}}}
\newcommand{\Varunixopt}[1]{\textbf{\texttt{\textit{#1}}}}
\newcommand{\Shellvar}[1]{\textbf{\texttt{\$#1}}}
\newcommand{\Path}[1]{\textbf{\texttt{#1}}}
\newcommand{\File}[1]{\texttt{#1}}
\newcommand{\Varfile}[1]{\texttt{\textit{#1}}}
\newcommand{\Prompt}[1]{\texttt{#1}}
\newcommand{\Unixmessage}[1]{\texttt{#1}}
\newcommand{\Queue}[1]{\texttt{\textbf{#1}}}
\newcommand{\Keyword}[1]{\texttt{#1}}
\newcommand{\Keybrd}[1]{\fbox{\texttt{#1}}}
%\newcommand{\Tki}[1]{\htmladdnormallink{TKI-#1}%
%  {http://prism.enes.org}}
\newenvironment{Indent}{ %
\begin{list}{}{\topsep=0pt}\item[]
}%
{%
\end{list}
}
\newcommand{\Myindent}{\hspace*{\leftmargin}}
\newcommand{\Crayfilesrv}{\/Cray file server\/}
\newcommand{\Crayfs}{\/Cray file server\/}
\newcommand{\Craycs}{\/CRAY J90\/}
\newcommand{\Craympp}{\/CRAY T3E\/}
\newcommand{\Craypvp}{\/CRAY T90\/}
\newcommand{\CrayfilesrvT}{Cray file server}
\newcommand{\CrayfsT}{Cray file server}
\newcommand{\CraycsT}{CRAY J90}
\newcommand{\CraymppT}{CRAY T3E}
\newcommand{\CraypvpT}{CRAY T90}
\newcommand{\Jump}{\/Jump\/}
\newcommand{\Homechar}{$\sim$}
\newcommand{\Backslash}{$\backslash$}
\newcommand{\Kea}{[}
\newcommand{\Kez}{]}
\newcommand\fs@Boxed{\def\@fs@cfont{\bfseries}\let\@fs@capt\floatc@plain
  \def\@fs@pre{
    \setbox\@currbox\vbox{%
      \hbadness10000%
      \vbox{%
        \hrule%
        \hbox to\hsize{%
          \vrule\kern2pt%
          \vbox{%
            \kern5pt\box\@currbox\kern5pt%
            }%
          \kern-2.7pt\vrule}\hrule}%
      }%
    }%
  \def\@fs@mid{\kern3pt}%
  \def\@fs@post{}\let\@fs@iftopcapt\iffalse}
\floatstyle{Boxed}
\setlength{\floatsep}{6pt plus 2pt minus2pt}
\setlength{\textfloatsep}{10pt plus 2pt minus2pt}
\setlength{\intextsep}{6pt plus 2pt minus2pt}
\setlength{\abovecaptionskip}{3pt}

\ifthenelse{\boolean{IsItaRS}}{
  \newfloat{Program_int}{htbp}{lop}[chapter]
  \newfloat{Job_int}{htbp}{loj}[chapter]
  \newfloat{Example_int}{htbp}{loe}[chapter]
}{
  \newfloat{Program_int}{htbp}{lop}
  \newfloat{Job_int}{htbp}{loj}
  \newfloat{Example_int}{htbp}{loe}
}
\floatname{Example_int}{Example}
\floatname{Program_int}{Program}
\floatname{Job_int}{Job}
\ifthenelse{\boolean{OptOldFloat}}
{%
  \newenvironment{Example}%
  {\begin{Example_int}[H]\small\begin{alltt}}%
  {\end{alltt}\vspace*{-2ex}\end{Example_int}}%
  \newenvironment{Longexample}%
  {\begin{Example_int}\small}%
  {\end{Example_int}}%
  %
  \newenvironment{Job}%
  {\begin{Job_int}[H]\small\begin{alltt}}%
  {\end{alltt}\vspace*{-2ex}\end{Job_int}}
  \newenvironment{Longjob}%
  {\begin{Job_int}\small}%
  {\end{Job_int}}%
  \newenvironment{Program}%
  {\begin{Program_int}[H]\small\begin{alltt}}%
  {\end{alltt}\vspace*{-2ex}\end{Program_int}}%
  \newenvironment{Longprogram}%
  {\begin{Program_int}\small}%
  {\end{Program_int}}%
}{ %else
  \newenvironment{Shortexample}%
  {\begin{Frame}\small\begin{alltt}}%
  {\end{alltt}\end{Frame}}%
  \newenvironment{Example}[1][htbp]%
  {\begin{Example_int}[#1]\small}%
  {\end{Example_int}}%
  %
  \newenvironment{Shortjob}%
  {\begin{Frame}\small\begin{alltt}}%
  {\end{alltt}\end{Frame}}
  \newenvironment{Job}[1][htbp]%
  {\begin{Job_int}[#1]\small}%
  {\end{Job_int}}%
  \newenvironment{Shortprogram}%
  {\begin{Frame}\small\begin{alltt}}%
  {\end{alltt}\end{Frame}}%
  \newenvironment{Program}[1][htbp]%
  {\begin{Program_int}[#1]\small}%
  {\end{Program_int}}%
}
\ifthenelse{\boolean{IsItaRS}}
{%
  \floatstyle{Boxed}
% Comment line below if you dont want a frome around the figures
  \restylefloat{figure}
}{}
\newenvironment{Itemize*}%
{ \partopsep=0pt%
  \begin{itemize}%
    \vspace*{-\topsep}%
    \parskip=0pt\itemsep=0pt\partopsep=0pt%%
}%
{%
  \vspace*{-\topsep}%
\end{itemize}%
}
\newenvironment{Enumerate*}%
{ \partopsep=0pt%
  \begin{enumerate}%
    \vspace*{-\topsep}%
    \parskip=0pt\itemsep=0pt\partopsep=0pt%%
}%
{%
  \vspace*{-\topsep}%
\end{enumerate}%
}
\newcommand{\longpage}%
{\enlargethispage{\baselineskip}\PRISMDOCDebugBox{\marginpar{\fbox{+}}}}
\newcommand{\shortpage}%
{\enlargethispage{-\baselineskip}\PRISMDOCDebugBox{\marginpar{\fbox{-}}}}

\ifthenelse{\boolean{OptLow}}%
{%
\newenvironment{itemize*}%
{\begin{Itemize*}}%
{\end{Itemize*}}
\newenvironment{enumerate*}%
{\begin{Enumerate*}}%
{\end{Enumerate*}}
}{}
\AtEndDocument{

  \ifthenelse{\boolean{OptIndex}}%
  {%
    \appendix
    \addcontentsline{toc}{chapter}{Index}
    {\small
      \printindex
      }
    }{}
    % Katalog
  \ifthenelse{\boolean{IsItaRS} \and \not \boolean{IsItaIB} \and \not \boolean{IsItaZHB}}%
  {%
    \ifthenelse{\boolean{OptTitleonly}}
    {}{%
      \cleardoublepage%
      \pagestyle{empty}%
     % \input{./styles/bhb-katalog.tex}%
      }%
    }{}%
  }%

\endinput
%%
%% End of file `prismdoc.cls'.
