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

C = \markup { \bold \box C }
Am = \markup { \bold \box Am }
Em = \markup { \bold \box Em }
F = \markup { \bold \box F }
G = \markup { \bold \box G }





{
  \new PianoStaff <<
    \new Staff {
      \time 4/4 \clef "treble"

    }
    \new Staff {
      \time 4/4 \clef "bass"


    }
    \addlyrics {


    }
  >>
}




