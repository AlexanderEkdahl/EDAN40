module Twinkle where
import Haskore

fd d n   = n d v
vol  n   = n   v
v        = [Volume 80]
lmap f l = line (map f l)

p1  = p1a :+: p1b :+: p1c :+: p1d
p1a = lmap (fd qn) [c 4, c 4, g 4, g 4]
p1b = lmap vol [a 5 qn, a 5 qn, g 4 hn]
p1c = lmap (fd qn) [f 4, f 4, e 4, e 4]
p1d = lmap vol [d 4 qn, d 4 qn, c 4 hn]

p2  = p2a :+: p2b :+: p2c :+: p2b
p2a = lmap (fd qn) [g 4, g 4, f 4, f 4]
p2b = lmap vol [e 4 qn, e 4 qn, d 4 hn]
p2c = lmap (fd qn) [g 4, g 4, f 4, f 4]

mainVoice = p1 :+: p2 :+: p1

twinkle = Instr "piano" (Tempo 2 (Phrase [Dyn SF] mainVoice))
