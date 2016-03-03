% Sample MetaPost with embedded LaTeX, used as unit test for ohcount
%
% The MetaPost code is taken of LaTeXLabels.mp from
% the excellent MetaPost tutorial by Urs Oswald
% http://www.ursoswald.ch/metapost/tutorial.html
%
verbatimtex
%&latex
\documentclass{article}
\newcommand{\uB}{\upshape{B\'ezier}}   % up: upright
\newcommand{\iB}{\itshape{B\'ezier}}   % it: italic
\newcommand{\lB}{\slshape{B\'ezier}}   % sl: slanted
\newcommand{\cB}{\scshape{B\'ezier}}   % sc: small caps
\newfont{\cyr}{wncyr10}
\begin{document}
etex

u:=25;                    % 25 = 25bp = 25 PostScript points = 30/72 in
wi:=10;                   % width  in units u   
he:=7;                    % height in units u
hoehe:=he*u;              % height
breite:=wi*u;             % width
picture lab;

beginfig(1)
  % --- Grid ---
  for i=0 upto he:
    draw (0, i*u)--(breite, i*u) withcolor .7white;
  endfor
  for j=0 upto wi:
    draw (j*u, 0)--(j*u, hoehe) withcolor .7white;
  endfor
  % --- End Grid ---
  
  draw (0, 0)--(breite, 0)--(breite, hoehe)--(0, hoehe)--cycle;
  
  for i=0 upto 5:
    draw .5(u, u){dir 20i}..{dir 20i}(9.5u, 4u);
  endfor  
  
  lab:=\thelabel(
    btex 
      \begin{tabular}{|r|l|l|l|l|}
	\hline
	\textbf{md} & upright      & italic       & slanted      & smallcaps   \\ 
	\hline
	rm          & \textrm{\uB} & \textrm{\iB} & \textrm{\lB} &\textrm{\cB} \\
	sf          & \textsf{\uB} & \textsf{\iB} &\textsf{\lB}  &\textsf{\cB} \\
	tt          & \texttt{\uB} & \texttt{\iB} &\texttt{\lB}  &\texttt{\cB} \\
	\hline
      \end{tabular}
    etex, 
    (.5breite, hoehe-1.5u)
  );
    
  unfill bbox lab;
  draw lab; 
  
  label.ulft(btex \cyr C\char24 rih, 08.09.2002 etex, (breite, 0));   
endfig;

end
