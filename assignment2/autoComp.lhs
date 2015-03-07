\documentclass[12pt]{article}

% \usepackage[utf8]{inputenc}
% \usepackage[T1]{fontenc}
% \usepackage{lmodern}
% \usepackage[swedish]{babel}

\usepackage{fancyvrb}
\DefineVerbatimEnvironment{code}{Verbatim}{fontsize=\small}
\DefineVerbatimEnvironment{example}{Verbatim}{fontsize=\small}

\title{autoComp \\ EDAN40}
\author{Alexander Ekdahl, D12 (dat12sek@student.lu.se)}
\date{}

\begin{document}
\maketitle

\begin{code}
module AutoComp where
import Haskore
\end{code}

Hej åäö

\begin{code}
type BassStyle =[(Int, Dur)]
basic, calypso, boogie :: BassStyle
basic   = [(0, hn), (4, hn)]
calypso = [(-1, qn), (0, en), (2, en), (-1, qn), (0, en), (2, en)]
boogie  = [(0, en), (4, en), (5, en), (4, en), (0, en), (4, en), (5, en), (4, en)]
\end{code}

\begin{code}
autoBass :: BassStyle -> Key -> ChordProgression -> Music
\end{code}

\begin{code}
--autoChord :: Key -> ChordProgression -> Music
\end{code}

\end{document}
