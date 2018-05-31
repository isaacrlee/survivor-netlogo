extensions [
  cf
  csv
  nw
  rnd
  table
]

breed [ contestants contestant ]
undirected-link-breed [ alliances alliance ]

contestants-own [
  eliminated?

  tribe

  mental
  physical
  social

  target ;; most recent target
  vote  ;; most recent vote

  finish ;; for resume output
  individual-challenge-wins ;; for resume output

  elimination-score ;; for voting-history output
  voting-history ;; for voting-history output
]

globals [
  merged?
  phase ;; 0:challenge -> 1:alliances -> 2:tribal council -> repeat

  eliminated-contestant ;; most recent eliminated contestant
  winning-contestant ;; most recent individual challenge winner
  winning-tribe ;; most recent tribal challenge winner

  challenge-eliminated-list ;; list of challenge winner, eliminated contestant pairs

  my-contestant ;; created contestant
  contestant-experiment-finishes ;; list of my-contestant's finishes
  repeat-experiment-finishes-table ;; table of each contestants finishes
]

to setup
  ca
  set-default-shape contestants "person"

  create-contestants num-contestants / 2 [ ;; create first tribe
    set tribe 0
    contestant-constructor
    setxy (- (random max-pxcor / 2)) - max-pxcor / 2  random-ycor
  ]

  create-contestants num-contestants / 2 [ ;; create second tribe
    set tribe 1
    contestant-constructor
    setxy (random max-pxcor / 2) + max-pxcor / 2 random-ycor
  ]

  if create-a-contestant? [
    ask one-of contestants [
      set my-contestant self
      set mental custom-mental
      set physical custom-physical
    ]
    watch my-contestant
  ]


  set phase -1
  set merged? false
  set challenge-eliminated-list (list)
  update-alliances
  reset-ticks
end

to go
  set phase (phase + 1) mod 3

  if count contestants with [eliminated? = false] = 2
  [
    log-challenge-eliminated-list-to-file
    log-contestant-resumes-to-file
    log-voting-histories-to-file
    stop
  ]

  if count contestants with [eliminated? = false] = num-contestants / 2 + 2 and phase = 0 [ merge ]
  (cf:ifelse
    phase = 0 [
      challenge
    ]
    phase = 1 [
      update-alliances
    ]
    phase = 2 [
      tribal-council
    ]
    [ print phase ])

  ;; log
  tick
end

to contestant-constructor  ;; turtle procedure
  set mental random 100 + 1
  set physical random 100 + 1
  set social random 100 + 1
  set eliminated? false
  set individual-challenge-wins 0
  set voting-history (list)
  set elimination-score ""
end

to merge
  set merged? true
  ask contestants with [eliminated? = false] [
    setxy (random max-pxcor) -  max-pxcor / 2 random-ycor
  ]
end

;; EXPERIMENT PROCEDURES

;; Procedure to run 250 runs with my-contestant
to contestant-experiment
  contestant-experiment-setup
  repeat 250 [ contestant-experiment-go ]
end

to contestant-experiment-setup
  ca
  set-default-shape contestants "person"

  set contestant-experiment-finishes (list)

  reset-ticks
end

to contestant-experiment-go
  clear-turtles
  clear-patches
  clear-drawing

  create-contestants num-contestants / 2 [ ;; create first tribe
    set tribe 0
    contestant-constructor
    setxy (- (random max-pxcor / 2)) - max-pxcor / 2  random-ycor
  ]

  create-contestants num-contestants / 2 [ ;; create second tribe
    set tribe 1
    contestant-constructor
    setxy (random max-pxcor / 2) + max-pxcor / 2 random-ycor
  ]

  if create-a-contestant? [
    ask one-of contestants [
      set my-contestant self
      set mental custom-mental
      set physical custom-physical
    ]
    watch my-contestant
  ]

  set merged? false
  set challenge-eliminated-list (list)
  update-alliances

  while [count contestants with [eliminated? = false] != 2] [
    if count contestants with [eliminated? = false] = num-contestants / 2 + 2 [ merge ]
    challenge
    update-alliances
    tribal-council
  ]
  set contestant-experiment-finishes lput [finish] of my-contestant contestant-experiment-finishes
  tick
end

to repeat-experiment
  repeat-experiment-setup
  repeat 250 [ repeat-experiment-go ]
  log-repeat-experiment-finishes-table-to-file
end

to repeat-experiment-setup
  ca
  set-default-shape contestants "person"

  create-contestants num-contestants / 2 [ ;; create first tribe
    set tribe 0
    contestant-constructor
    setxy (- (random max-pxcor / 2)) - max-pxcor / 2  random-ycor
  ]

  create-contestants num-contestants / 2 [ ;; create second tribe
    set tribe 1
    contestant-constructor
    setxy (random max-pxcor / 2) + max-pxcor / 2 random-ycor
  ]

  set repeat-experiment-finishes-table table:make
  ask contestants [
    table:put repeat-experiment-finishes-table who (list)
  ]

  reset-ticks
end

to repeat-experiment-go
  ask contestants [
    st
    set eliminated? false
    set individual-challenge-wins 0
    set voting-history (list)
    set elimination-score ""
    ifelse tribe = 0 [
      setxy (- (random max-pxcor / 2)) - max-pxcor / 2  random-ycor
    ]
    [
      setxy ((random max-pxcor / 2)) + max-pxcor / 2  random-ycor
    ]
  ]

  set merged? false
  set challenge-eliminated-list (list)
  update-alliances

  while [count contestants with [eliminated? = false] != 2] [
    if count contestants with [eliminated? = false] = num-contestants / 2 + 2 [ merge ]
    challenge
    update-alliances
    tribal-council
  ]

  ;show repeat-experiment-finishes-table
  ask contestants [
    table:put repeat-experiment-finishes-table who lput finish table:get repeat-experiment-finishes-table who
  ]

  if log?
  [
    log-challenge-eliminated-list-to-file
    log-contestant-resumes-to-file
    log-voting-histories-to-file
    stop
  ]

  tick
end

;; CHALLENGE PROCEDURES

;; Procedure to run correct challenge procedure
to challenge
  ifelse not merged? [
    let tribe-0-physical-mental (tribe-0-physical + tribe-0-mental) ^ 2
    let tribe-1-physical-mental (tribe-1-physical + tribe-1-mental) ^ 2
    let tribes list (list 0 tribe-0-physical-mental) (list 1 tribe-1-physical-mental)
    set winning-tribe first rnd:weighted-one-of-list tribes [ [t] -> last t ]

    ;; log
    if log? [ print word "Tribe " word winning-tribe " Won Challenge" ]
  ]
  [
    set winning-contestant rnd:weighted-one-of contestants with [eliminated? = false] [(physical + mental) ^ 2]

    ask winning-contestant [
      set individual-challenge-wins individual-challenge-wins + 1
    ]

    ;; log
    if log? [ print word "Contestant " word [who] of winning-contestant " Won Challenge" ]
  ]
end

;; TRIBAL COUNCIL PROCEDURES

;; Procedure to run correct tribal council procedure
to tribal-council
  let c contestants with [eliminated? = false]
  set-vote
  ifelse not merged? [
    ;; pick contestant to eliminate
    let to-eliminate max-one-of c with [tribe != winning-tribe] [votes-against]

    ;; log
    if log? [
      print word "Tribe " word (1 - winning-tribe) " Tribal Council:"
      foreach sort-on [(- [who] of vote)] contestants with [eliminated? = false and vote != nobody][ the-contestant ->
        ask the-contestant [
          let vh last voting-history
          print word who word ": " vh
        ]
      ]
    ]

    ;; eliminate contestant
    ask to-eliminate [ eliminate ]
    set challenge-eliminated-list lput (list winning-tribe eliminated-contestant) challenge-eliminated-list
  ]
  [
    ;; pick contestant to eliminate
    let to-eliminate max-one-of c [votes-against]

    ;; log
    if log? [
      print "Tribal Council:"
      foreach sort-on [(- [who] of vote)] contestants with [eliminated? = false][ the-contestant ->
        ask the-contestant [
          let vh last voting-history
          print word who word ": " vh
        ]
      ]
    ]

    ;; eliminate contestant
    ask to-eliminate [ eliminate ]
    set challenge-eliminated-list lput (list winning-contestant eliminated-contestant) challenge-eliminated-list
  ]
end

;; Procedure to eliminate contestant
to eliminate  ;; turtle procedure
  if log? [ print word "Contestant " word who " Eliminated" ]
  set-elimination-score
  set eliminated? true
  set eliminated-contestant self
  hide-turtle
  ask my-alliances[ die ]
  set finish count contestants - count contestants with [eliminated? = true] + 1
end

;; Procedure to set each contestants elimination vote (ex. contestant eliminated 5 votes to 3 votes, output: (5-3))
to set-elimination-score ;; turtle procedure
  set elimination-score (word "(" first tribal-council-score)
  foreach but-first tribal-council-score [ num ->
    set elimination-score (word elimination-score "-" num)
  ]
  set elimination-score (word elimination-score ")")
end

;; ALLIANCE PROCEDURES

;; Procedure to run correct procedure to update allies
to update-alliances
  ask contestants with [eliminated? = false] [
    set social social + random 2 * social-wiggle - social-wiggle
  ]

  if not merged? [ nw:set-context contestants with [eliminated? = false and tribe != winning-tribe] alliances with [member? end1 contestants with [tribe != winning-tribe]] ]

  ;; Line In The Sand: if contestant did not vote with their ally, they are no longer allies
  ask contestants with [eliminated? = false] [
    ask my-alliances [
      if [vote] of end2 != [vote] of myself [ die ]
    ]

    ifelse not merged? [
      create-alliances-with other contestants with [eliminated? = false and tribe = [tribe] of myself and (social-difference myself) < max-alliance-social-difference]
    ]
    [
      create-alliances-with other contestants with [eliminated? = false and (social-difference myself) < max-alliance-social-difference]
    ]


  ]

  ;; layout
  if layout? [
    layout
  ]
end

;; VOTE PROCEDURES

to set-vote
  ask contestants [
      set target nobody
      set vote nobody
    ]
  ifelse not merged? [
    ask contestants with [eliminated? = false and tribe != winning-tribe] [

      let who-of-target [who] of rnd:weighted-one-of other contestants with [ eliminated? = false and tribe = [tribe] of myself ] [(social-difference myself) / (mental + physical)]

      set target contestant who-of-target

      ;table:put target-table who-of-target (table:get-or-default target-table who-of-target 0) + 1
    ]

    let target-table table:counts [[who] of target] of contestants with [eliminated? = false and tribe != winning-tribe]

    let primary-target key-with-max-value target-table
    table:remove target-table primary-target
    let secondary-target key-with-max-value target-table

    ask contestants with [eliminated? = false and tribe != winning-tribe] [
      set vote max-one-of (turtle-set contestant primary-target contestant secondary-target) [ social-difference myself ]
    ]

    ask contestants with [eliminated? = false] [
      ifelse vote = nobody [
        set voting-history lput "-" voting-history
      ]
      [
        set voting-history lput [who] of vote voting-history
      ]
    ]
  ]
  [
    ask contestants with [eliminated? = false] [

      let who-of-target [who] of rnd:weighted-one-of other contestants with [ eliminated? = false and self != winning-contestant ] [social-difference myself + perceived-threat]

      set target contestant who-of-target
    ]

    let target-table table:counts [[who] of target] of contestants with [eliminated? = false]

    let primary-target key-with-max-value target-table
    table:remove target-table primary-target
    let secondary-target key-with-max-value target-table

    ask contestants with [eliminated? = false] [
      set vote max-one-of other (turtle-set contestant primary-target contestant secondary-target) [social-difference myself]
    ]

    ask contestants with [eliminated? = false] [
      ifelse vote = nobody [
        set voting-history lput "-" voting-history
      ]
      [
        set voting-history lput [who] of vote voting-history
      ]
    ]
  ]
end

;; REPORTERS

to-report absolute-value [number]
  ifelse number >= 0
    [ report number ]
    [ report (- number) ]
end

to-report key-with-max-value [ t ]
  let l table:to-list t ; convert to list of key/value pairs
  report first reduce [ [a b] -> ifelse-value (last a > last b) [a] [b]] l ; find pair with max value, report key
end

to-report merged-reporter
  report merged?
end

to-report perceived-threat  ;; turtle reporter
  report (
    (mental + physical)
    + individual-challenge-wins * 20
    )
end

to-report social-difference [ ms ] ;; turtle reporter
  report (absolute-value (social - [social] of ms))
end

to-report tribal-council-score
  let l (list)
  ;; iterates through contestants who had votes cast against them
  foreach sort-on [(- votes-against)] contestants with [votes-against > 0][ the-contestant ->
    set l lput [votes-against] of the-contestant l  ]
  report l
end

to-report tribe-0-mental
  if count contestants with [tribe = 0 and eliminated? = false] = 0 [ report 0 ]
  report sum [mental] of contestants with [tribe = 0 and eliminated? = false] / count contestants with [tribe = 0 and eliminated? = false]
end

to-report tribe-0-physical
  if count contestants with [tribe = 0 and eliminated? = false] = 0 [ report 0 ]
  report sum [physical] of contestants with [tribe = 0 and eliminated? = false] / count contestants with [tribe = 0 and eliminated? = false]
end

to-report tribe-1-mental
  if count contestants with [tribe = 0 and eliminated? = false] = 0 [ report 0 ]
  report sum [mental] of contestants with [tribe = 1 and eliminated? = false] / count contestants with [tribe = 1 and eliminated? = false]
end

to-report tribe-1-physical
  if count contestants with [tribe = 0 and eliminated? = false] = 0 [ report 0 ]
  report sum [physical] of contestants with [tribe = 1 and eliminated? = false] / count contestants with [tribe = 1 and eliminated? = false]
end

to-report votes-against  ;; turtle reporter
  ifelse not merged?
  [ report count contestants with [eliminated? = false and tribe = [tribe] of myself and vote = myself] ]
  [ report count contestants with [eliminated? = false and vote = myself] ]
end

to-report winning-contestant-reporter
  report winning-contestant
end

to-report winning-tribe-reporter
  report winning-tribe
end

;; LAYOUT PROCEDURE

to layout
  repeat 30 [
    layout-spring turtles links 0.2 2 1
    display
  ]
end

;; CSV PROCEDURES

to log-challenge-eliminated-list-to-file
  csv:to-file "challenge-eliminated-list.csv" fput (list "challenge-winner" "eliminated-contestant") challenge-eliminated-list
end

to log-contestant-resumes-to-file
  let l (list)
  set l lput (list "contestant" "tribe" "finish" "individual-challenge-wins" "mental" "physical" "social") l
  foreach sort-on [(- finish)] contestants[ the-contestant ->
    ask the-contestant [
      set l lput (list who tribe finish individual-challenge-wins mental physical social) l
    ]
  ]
  csv:to-file "contestant-resumes.csv" l
end

to log-repeat-experiment-finishes-table-to-file
  let l (list)
  set l lput (list "contestant" "tribe" "finish" "mental" "physical" "social") l
  foreach sort-on [(- mean table:get repeat-experiment-finishes-table who)] contestants[ the-contestant ->
    ask the-contestant [
      set l lput (list who tribe mean table:get repeat-experiment-finishes-table who mental physical social) l
    ]
  ]
  csv:to-file "repeat-experiment-finishes-table.csv" l
end

to log-voting-histories-to-file
  let l (list)

  let r (list "contestant")
  foreach sort-on [(- finish)] contestants [ the-contestant ->
    ask the-contestant [
      set r lput (word who " " elimination-score) r
    ]
  ]

  set l lput r l

  foreach sort-on [finish] contestants[ the-contestant ->
    ask the-contestant [
      set l lput fput who voting-history l
    ]
  ]
  csv:to-file "voting-histories.csv" l
end
@#$#@#$#@
GRAPHICS-WINDOW
210
10
647
448
-1
-1
13.0
1
10
1
1
1
0
0
0
1
-16
16
-16
16
1
1
1
ticks
30.0

BUTTON
10
80
95
113
go-once
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
10
40
76
73
setup
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
10
120
73
153
go
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
10
165
182
198
num-contestants
num-contestants
0
1000
20.0
2
1
NIL
HORIZONTAL

BUTTON
10
210
77
243
NIL
layout
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
90
210
193
243
layout?
layout?
0
1
-1000

SWITCH
10
255
113
288
log?
log?
0
1
-1000

SWITCH
655
10
842
43
create-a-contestant?
create-a-contestant?
1
1
-1000

SLIDER
655
80
827
113
custom-physical
custom-physical
1
100
1.0
1
1
NIL
HORIZONTAL

SLIDER
655
45
827
78
custom-mental
custom-mental
1
100
1.0
1
1
NIL
HORIZONTAL

PLOT
655
150
855
300
mean finish of my-contestant
NIL
NIL
0.0
10.0
0.0
20.0
true
true
"" "if contestant-experiment-finishes = 0 [ stop ]"
PENS
"mean" 1.0 0 -16777216 true "" "plot mean contestant-experiment-finishes"
"-1 sd" 1.0 0 -7500403 true "" "plot mean contestant-experiment-finishes - standard-deviation contestant-experiment-finishes"
"+1 sd" 1.0 0 -2674135 true "" "plot mean contestant-experiment-finishes + standard-deviation contestant-experiment-finishes"

MONITOR
865
195
1112
240
NIL
mean contestant-experiment-finishes
17
1
11

MONITOR
865
240
1037
285
-1 sd experiment-finishes
mean contestant-experiment-finishes - standard-deviation contestant-experiment-finishes
17
1
11

MONITOR
865
150
1042
195
+1 sd experiment-finishes
mean contestant-experiment-finishes + standard-deviation contestant-experiment-finishes
17
1
11

BUTTON
5
495
337
528
NIL
repeat-experiment
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
655
115
827
148
NIL
contestant-experiment\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
655
305
855
455
Tribal Council Scores
Time
Votes Against
0.0
10.0
0.0
10.0
true
false
"" "if phase != 2 [ stop ]\nlet target-table table:counts [[who] of vote] of contestants with [eliminated? = false and tribe != winning-tribe]\nif merged? [\nset target-table table:counts [[who] of vote] of contestants with [eliminated? = false]\n]\n\nlet primary-target key-with-max-value target-table\nif primary-target = nobody [ stop ]\ntable:remove target-table primary-target\nlet secondary-target key-with-max-value target-table\n\nlet total 0\nlet primary-votes count contestants with [ eliminated? = false and vote = primary-target ]\nlet secondary-votes count contestants with [ eliminated? = false and vote = secondary-target]\n\nset-current-plot-pen \"Primary Vote\"\nplot-pen-up plotxy ticks total\nset total total + primary-votes\nplot-pen-down plotxy ticks total\n\nset-current-plot-pen \"Secondary Vote\"\nplot-pen-up plotxy ticks total\nset total total + secondary-votes\nplot-pen-down plotxy ticks total"
PENS
"Primary Vote" 1.0 1 -955883 true "" ""
"Secondary Vote" 1.0 1 -8630108 true "" ""

SLIDER
5
450
247
483
max-alliance-social-difference
max-alliance-social-difference
0
50
15.0
1
1
NIL
HORIZONTAL

SLIDER
5
400
177
433
social-wiggle
social-wiggle
0
5
0.0
1
1
NIL
HORIZONTAL

@#$#@#$#@
## WHAT IS IT?

This model of alliances  
(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.0.3
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="experiment" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>repeat-experiment-go</go>
    <metric>[who] of contestants with [finish = 0]</metric>
    <enumeratedValueSet variable="num-contestants">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="log?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="create-a-contestant?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="custom-mental">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="custom-physical">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
1
@#$#@#$#@
