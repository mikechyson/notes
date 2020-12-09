\version "2.18.2"

\header {
  title = "Canon"
  composer = "Yang"
}

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

cchord = \markup { \bold \box C }
amchord = \markup { \bold \box Am }
emchord = \markup { \bold \box Em }
fchord = \markup { \bold \box F }
gchord = \markup { \bold \box G }



\relative c' {
  \new PianoStaff <<
    \new Staff {
      \time 4/4 \clef "treble"
      r8 e^1_"mi" g^2_"sol" c^5_"do" r b,^1_"si" d^2_"re" g^5_"sol" |
      r c,^1_"do" e^2_"mi" a^5_"la" r g,^1_"sol" b^2_"si" e^5_"mi" |
      r a,^1_"la" c^2_"do" f^5_"fa" r e^1_"mi" g^2_"sol" c^5_"do" |
      r a,^1_"la" c^2_"do" f^5_"fa" r b^1_"si" d^2_"re" g^5_"sol" \bar ".|:"

      c2 g2 |
      a e |
      f c |
      f g \bar ":|."


    }
    \new Staff {
      \time 4/4 \clef "bass"
      c,,2-1_"do" g^2_"sol" |
      a^1_"la" e^3_"mi" |
      f^4_"fa" c^1_"do" |
      f^4_"fa" g^5_"sol" \bar ".|:"


    }
    \addlyrics {
      \cchord \gchord |
      \amchord \emchord |
      \fchord \cchord |
      \fchord \gchord |


    }
  >>
}


