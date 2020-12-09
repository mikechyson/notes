\version "2.18.2"

Treble = \relative c {
c'1 c' c' 
}

Bass = \relative c {
c, c' c' 
}


\relative c{
  \new PianoStaff <<
    \new Staff {
      \time 4/4 \clef "treble"
      \Treble
    }
    \new Staff {
      \time 4/4 \clef "bass"
      \Bass
    }
  >>
}
