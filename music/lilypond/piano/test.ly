\version "2.18.2"


\paper {
  indent = 0\mm
  line-width = 160\mm
  % offset the left padding, also add 1mm as lilypond creates cropped
  % images with a little space on the right
  line-width = #(- line-width (* mm  3.000000) (* mm 1))
  line-width = 160\mm - 2.0 * 10.16\mm
  % offset the left padding, also add 1mm as lilypond creates cropped
  % images with a little space on the right
  line-width = #(- line-width (* mm  3.000000) (* mm 1))
}
\layout {
}


\relative c'' {
  g'8_"sol" e16_"mi" f_"fa" g8_"sol" e16_"mi" f_"fa" g_"sol" g,_"sol" a_"la" b_"si" c_"do" d_"re" e_"mi" f_"fa" |
  e8_"mi" c16_"do" d_"re" e8_"mi" e,16_"mi" f_"fa" g_"sol" a_"la" g_"sol" f_"fa" g_"sol" e_"mi" f_"fa" g_"sol" |
  f8_"fa" a16_"la" g_"sol" f8_"fa" e16_"mi" d_"re" e_"mi" d_"re" c_"do" d_"re" e_"mi" f_"fa" g_"sol" a_"la" |
  f8_"fa" a16_"la" g_"sol" a8_"la" b16_"si" c_"do" g_"sol" a_"la" b_"si" c_"do" d_"re" e_"mi" f_"fa" g_"sol" |
}

\relative c { \clef "bass"
  <c' e>2 <g d'> |
  <a c> <e b'> |
  <f a> <c g'> |
  <f a> <g b> |
}