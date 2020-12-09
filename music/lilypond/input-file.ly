{
  \time 2/4
  \clef bass
  c4 c g g a a g2
}

\relative c'' {
  \key c \minor
  g(
  <ees c'>)
  <d f gis b>-.
  <ees g bes>-.
}

<<
  \chords {
    c1:m7 f2:7 c2
  }
  \relative c'' {
    g2 es8( c4) es8
    f8 es d c~ c2
  }
  \addlyrics {
    You are
    the sky and my rain,
  }
>>

hornNotes =
\relative c {
  \time 2/4
  R2*3
  r4 f8 a cis4 f e d
}

bassoonNotes =
\relative c {
  \clef bass
  r4 d,8 f gis4 g b bes
  a8 e f4 g d gis f
}

{
  \set Score.skipBars = ##t
  \transpose f c' \hornNotes
}

<<
  \new Staff \hornNotes
  \new Staff \bassoonNotes
>>