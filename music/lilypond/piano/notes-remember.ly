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


<<
  \relative c {
    f' a c e
  }
  \addlyrics {
    F A C E
  }
>>

<<
  \relative c { \time 5/4
    e' g b d f
  }
  \addlyrics {
    Every Good Boy Does Fine
  }
>>


<<
  \relative c { \clef "bass"
    a c e g
  }
  \addlyrics {
    All Cows Eat Grass
  }
>>

<<
  \relative c { \time 5/4 \clef "bass"
    g b d f a
  }
  \addlyrics {
    Good Boys Do Fine Always
  }
>>





