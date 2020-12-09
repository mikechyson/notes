\version "2.18.2"

\header {
  title = "Canon"
  composer = "YTH"
  tagline = \markup { \bold
    Canon
  }
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




C = \markup { \bold \box C }
Am = \markup { \bold \box Am }
Em = \markup { \bold \box Em }
F = \markup { \bold \box F }
G = \markup { \bold \box G }


ATreble = \relative c' {
  r8 e g c r b, d g |
  r c, e a r g, b e |
  r a, c f r e, g c |
  r a c f r b, d g |
}

ABass = \relative c' {
  c2-1 g |
  a e |
  f c |
  f g |
}



% B prelude treble
BTreble = \relative c'' {
  e2-3 d |
  c b |
  a g |
  a b |
}

% B prelude bass
BBass = \relative c' {
  c8 e g c g, b d g |
  a, c e a e, g b e |
  f, a c f c, e g c |
  f, a c f g, b d g |
}

CTreble = \relative c'' {
  c8-5 b c c, b-1 g' d e |
  c-1 c' b a b-1 e-2 g a |
  f-4 e d f e d c b |
  a g f-4 e d f e d |
  c-1 d e f b,-1 d-2 g f |
  e-2 a g f g f e d |
  c-2 a a'-3 b c b a g |
  f-1 e-2 d-1 a' g a g4
}

CBass = \relative c {
  c2-2 <g d'> |
  <a e'> <e e'> |
  <f f'> c'-2 |
  <f, f'> <g g'> |
  <c g'> <g g'> |
  <a a'> <e e'> |
  <f f'> <c c'> |
  <f f'> <g g'> |
}

DTreble = \relative c'' {
  g'8-5 e16 f g8 e16 f g g, a b c d e f |
  e8 c16 d e8 e,16 f g a g f g e f g |
  f8-2 a16 g f8 e16 d e d c d e f g a |
  f8 a16 g a8 b16 c g-1 a b c d e f g |
}

DBass = \relative c {
  <c' e>2 <g d'> |
  <a c> <e b'> |
  <f a> <c g'> |
  <f a> <g b> |
}

{
  \new PianoStaff <<
    \new Staff {
      \time 4/4 \clef "treble"
      \ATreble \bar ".|:"
      \BTreble \bar ":|."
      \CTreble \bar ".|:"
      \DTreble \bar ":|."
      \BTreble \bar "|."


    }
    \new Staff {
      \time 4/4 \clef "bass"
      \ABass \bar ".|:"
      \BBass \bar ":|."
      \CBass \bar ".|:"
      \DBass \bar ":|."
      \BBass \bar "|."

    }
    \addlyrics {
      \C \G |
      \Am \Em |
      \F \C |
      \F \G |

      \C _ _ _ \G _ _ _ |
      \Am _ _ _ \Em _ _ _ |
      \F _ _ _ \C _ _ _ |
      \F _ _ _ \G _ _ _ |

      \C \G |
      \Am \Em |
      \F \C |
      \F \G |

      \C \G |
      \Am \Em |
      \F \C |
      \F \G |

      \C \G |
      \Am \Em |
      \F \C |
      \F \G |

      \C _ _ _ \G _ _ _ |
      \Am _ _ _ \Em _ _ _ |
      \F _ _ _ \C _ _ _ |
      \F _ _ _ \G _ _ _ |
    }
  >>
}



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






