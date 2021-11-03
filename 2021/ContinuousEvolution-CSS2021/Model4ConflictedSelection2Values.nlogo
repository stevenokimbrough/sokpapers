globals [L-scale ;; but not used
  meanmeaninitv2 meanmeaninitv1 p-accept
  p-reject reject? thedraw
  ]
turtles-own [exemplars2 exemplars2-mean exemplars2-sd
exemplars1 exemplars1-mean exemplars1-sd
]

to setup
  ;; Now we have two vowels and two exemplars lists for each turtle
  ;; We initiate everything from the same distribution.
  clear-all
  create-turtles num-turtles [
    setxy random-xcor random-ycor
    set exemplars1  []
    repeat num-exemplars [ set exemplars1 lput (random-normal damean1 dasd) exemplars1
    ]
    set exemplars1-mean mean exemplars1
    set exemplars1-sd standard-deviation exemplars1

    set exemplars2  []
    repeat num-exemplars [ set exemplars2 lput (random-normal damean2 dasd) exemplars2
    ]
    set exemplars2-mean mean exemplars2
    set exemplars2-sd standard-deviation exemplars2

  ]
  set meanmeaninitv1 meanmean1
  set meanmeaninitv2 meanmean2
  reset-ticks
end

to go
  ;let hearer one-of turtles
  ask one-of turtles [
    let vowel one-of ["vowel1" "vowel2"]
    let utterance  0
    ifelse vowel = "vowel1"
    [set utterance random-normal exemplars1-mean exemplars1-sd]
    [set utterance random-normal exemplars2-mean exemplars2-sd]

    let hearer one-of other turtles

;;;; here
;    let distance-value abs (utterance - idealpoint)
;    if random-float 1 <= mirrored-positive-logistic(distance-value)(k)

;
    set reject? false
    if avoidance
    [ let d abs (utterance - avoidancepoint1)
      set p-accept (1 - mirrored-positive-logistic(d)(k))
      set p-reject mirrored-positive-logistic(d)(k)
      set d abs (utterance - avoidancepoint2)
      set p-accept max (list 0 ((1 - mirrored-positive-logistic(d)(k)) - p-accept))
      ;print p-accept
      set p-reject p-reject + mirrored-positive-logistic(d)(k)
      set p-reject p-reject ^ 1.5
      set reject? random-float 1 <= p-reject
      if reject? [stop]

    ]
;    if not reject?  [
      ask hearer [ifelse vowel = "vowel1"
      ;;;; if
      [let d1 abs (utterance - exemplars1-mean)
        let d2 abs (utterance - exemplars2-mean)
        let prob-keep 1 - (d1 / (d1 + d2)) ^ 1 / 2 ;;;^ 3
      if random-float 1 <= prob-keep ;;max (list 0 (prob-keep - p-reject))
        [set exemplars1 lput utterance exemplars1
      set exemplars1 but-first exemplars1
    set exemplars1-mean mean exemplars1
    set exemplars1-sd standard-deviation exemplars1]
      ]
      ;;; else
      [let d1 abs (utterance - exemplars2-mean)
        let d2 abs (utterance - exemplars1-mean)
        let prob-keep 1 - (d1 / (d1 + d2)) ^ 1 / 2 ;;;^ 3

        if random-float 1 <=  prob-keep  ;; max (list 0 (prob-keep - p-reject))  ;;
        [set exemplars2 lput utterance exemplars2
      set exemplars2 but-first exemplars2
    set exemplars2-mean mean exemplars2
    set exemplars2-sd standard-deviation exemplars2]
      ]

      ] ;;; end of ifelse

;    ] ;;; end of ask hearer

  ]
  tick
;  if mean [exemplars-mean] of turtles > stopat or mean [exemplars-mean] of turtles < -1 * stopat [
;    stop]
end

;to go1
;
;  ask one-of turtles  [
;    let utterance random-normal exemplars-mean exemplars-sd
;    let hearer one-of  turtles in-radius radius  ;; other
;    ask hearer [set exemplars lput utterance exemplars
;      set exemplars but-first exemplars
;    set exemplars-mean mean exemplars
;    set exemplars-sd standard-deviation exemplars
;    set color exemplars-mean * 12
;    ]
;
;  ]
;  tick
;  if mean [exemplars-mean] of turtles > stopat or mean [exemplars-mean] of turtles < -1 * stopat [
;    stop]
;end

to-report meansd2
  report mean [exemplars2-sd] of turtles
end

to-report meansd1
  report mean [exemplars1-sd] of turtles
end

to-report meanmean2
  report mean [exemplars2-mean] of turtles
end

to-report meanmean1
  report mean [exemplars1-mean] of turtles
end

to-report reject [x]
  report 2
end


;;;;;;;;;;;;;;;;;;;; Imported from Model2SimpleSelection.nlogo . ;;;;;;;;;;;;;

to display-mirrored-positive-logistic
;;  The general form of the logistic is
;;  y = f(x) = L / (1 + e^(-k*x))
;;  where L is a scaling constant, which we set to 1 so we get a probability,
;;  and k is a free parameter that affects the slope of the curve.
;;  Notice that at x = 0, f(x) = 0.5 (assuming as we shall that L = 1).
;;  And we want g(x),
;;  g(x) = 1 - f(x).
;;  We need to introduce more parameters here, as follows:
;;  x = cs - pu - ne
;;  With the following interpretations:
;;  cs = cost of solar PV power in cents per kWH
;;  pu = price of utility power in cents per kWH
;;  Finally, ne is the neighborhood effect (a positive number),
;;  which serves to reduce the value of x, and hence increase the probability
;;  of adoption (as it increases).

  ;; So now we initialize the variables.
  ;; current-price  taken from the Interface slider corresponds to pu, price from the utility
  ;; neighborhood-effect corresponds to  ne and is taken from the Interface slider.

  set-current-plot "Probability of Adoption Function"
  clear-plot
  plot-simple-mirrored-logistic(L-scale)
  clear-output
  let considered-costs linspace(considered-low)(considered-high)(51)
  output-print (word "distance" "   " "net score (x)" "      " "p(x)")
  output-print (word "--------------------------------------")
  foreach considered-costs [ [?1] ->
    ;let x  the-score(?1)(current-price)(0) ;; neighborhood-effect
  ;output-print (word precision ?1 2 "\t\t" precision x 2 "\t\t" precision (simple-mirrored-logistic(x)(k) * L-scale) 3) ]
    output-print (word precision ?1 2 "\t\t" precision ?1 2 "\t\t" precision (mirrored-positive-logistic(?1)(k) ) 3) ]
end


to display-positive-logistic
;;  The general form of the logistic is
;;  y = f(x) = L / (1 + e^(-k*x))
;;  where L is a scaling constant, which we set to 1 so we get a probability,
;;  and k is a free parameter that affects the slope of the curve.
;;  Notice that at x = 0, f(x) = 0.5 (assuming as we shall that L = 1).
;;  And we want g(x),
;;  g(x) = 1 - f(x).
;;  We need to introduce more parameters here, as follows:
;;  x = cs - pu - ne
;;  With the following interpretations:
;;  cs = cost of solar PV power in cents per kWH
;;  pu = price of utility power in cents per kWH
;;  Finally, ne is the neighborhood effect (a positive number),
;;  which serves to reduce the value of x, and hence increase the probability
;;  of adoption (as it increases).

  ;; So now we initialize the variables.
  ;; current-price  taken from the Interface slider corresponds to pu, price from the utility
  ;; neighborhood-effect corresponds to  ne and is taken from the Interface slider.

  set-current-plot "Probability of Adoption Function"
  clear-plot
  ;plot-simple-mirrored-logistic(L-scale)
  plot-simple-logistic(L-scale)
  clear-output
  let considered-costs linspace(considered-low)(considered-high)(51)
  output-print (word "distance" "   " "net score (x)" "      " "p(x)")
  output-print (word "--------------------------------------")
  foreach considered-costs [ [?1] ->
    ;let x  the-score(?1)(current-price)(0) ;; neighborhood-effect
  ;output-print (word precision ?1 2 "\t\t" precision x 2 "\t\t" precision (simple-mirrored-logistic(x)(k) * L-scale) 3) ]
    output-print (word precision ?1 2 "\t\t" precision ?1 2 "\t\t" precision (positive-logistic(?1)(k) ) 3) ]
end

to plot-simple-mirrored-logistic [da-scale]
  let the-considered-costs linspace(considered-low)(considered-high)(51)

  let davalues []
  let a-score 0
  foreach the-considered-costs [ [?1] ->
    set davalues lput (mirrored-positive-logistic(?1)(k) ) davalues ]
  plotvectors(the-considered-costs)(davalues)

end

to plot-simple-logistic [da-scale]
  let the-considered-costs linspace(considered-low)(considered-high)(51)

  let davalues []
  let a-score 0
  foreach the-considered-costs [ [?1] ->
    set davalues lput (positive-logistic(?1)(k) ) davalues ]
  plotvectors(the-considered-costs)(davalues)

end

to-report linspace [dastart dastop  numpoints]
  ;; Modeled after MATLAB's linspace.
  ;; Returns a list numpoints long consisting
  ;; of evenly spaced floating point numbers,
  ;; the first of which is start and the last
  ;; of which is stop.
  ;; Note that this reporter does no error checking.
  ;; However, it works properly with negative numbers
  ;; and going from both high to low and low to high.
  let toreport (list dastart)
  let increment ((dastop - dastart)) / (numpoints - 1)
  repeat numpoints - 1
  [set toreport lput ((last toreport) + increment) toreport]
  report toreport
end

to-report mirrored-positive-logistic [x the-k]
  ;; Given the inputs, returns a probability value.
  ;; Fixing base and temperature, varying score yields
  ;; an "s"-shaped (sigmoid) curve, increasing in score.
  ;; See logistic curve.
  ;; See SOK's book Agents, Games, and Evolution, appendix B.4
  ;; for explanation. See also:
  ;; http://mathworld.wolfram.com/LogisticEquation.html
  ;; Most directly, the logistic function:
  ;; http://en.wikipedia.org/wiki/Logistic_function
  ;; f(x) = 1 / (1 + e^(-k*x))
  ;; where L is a proportionality constant. k is a constant
  ;; and x0 is a constant at which f(x) = 0.5 * L.
  ;; Because we here want to score for distinces, when x = 0
  ;; we should report 1 so we double the value
  report (1 - (1 / ((1 + e ^ (-1 *  the-k *  x))))) * 2
  ;; Notice we have 1 minus the normal function's value, because
  ;; as the cost of solar decreases, the probability of installation
  ;; increases.
end

to-report positive-logistic [x the-k]
  ;; Given the inputs, returns a probability value.
  ;; Fixing base and temperature, varying score yields
  ;; an "s"-shaped (sigmoid) curve, increasing in score.
  ;; See logistic curve.
  ;; See SOK's book Agents, Games, and Evolution, appendix B.4
  ;; for explanation. See also:
  ;; http://mathworld.wolfram.com/LogisticEquation.html
  ;; Most directly, the logistic function:
  ;; http://en.wikipedia.org/wiki/Logistic_function
  ;; f(x) = 1 / (1 + e^(-k*x))
  ;; where L is a proportionality constant. k is a constant
  ;; and x0 is a constant at which f(x) = 0.5 * L.
  ;; Because we here want to score for distinces, when x = 0
  ;; we should report 1 so we double the value
  report (1 / ((1 + e ^ (-1 *  the-k *  x)))) - 0.5
  ;; Notice we have 1 minus the normal function's value, because
  ;; as the cost of solar decreases, the probability of installation
  ;; increases.
end

to plotvectors [x y]
;  Assumes a plot has been selected and
;  made the current plot and been cleared.
  let dalength length x
  let dacount 1
  while [dacount < dalength]
  [plotxy item dacount x item dacount y
    set dacount (dacount + 1)]
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
1
1
1
-16
16
-16
16
0
0
1
ticks
30.0

BUTTON
4
48
70
81
NIL
setup\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
20
92
192
125
damean1
damean1
-10
10
-3.9
0.1
1
NIL
HORIZONTAL

SLIDER
25
164
200
197
dasd
dasd
1
10
1.5
0.1
1
NIL
HORIZONTAL

MONITOR
21
261
116
306
NIL
meanmean1
4
1
11

MONITOR
116
261
209
306
NIL
meansd1
4
1
11

BUTTON
73
49
136
82
NIL
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

PLOT
21
306
434
456
plot 1
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"meanmean2" 1.0 0 -16777216 true "" "plot mean [exemplars2-mean] of turtles"
"meansd2" 1.0 0 -2674135 true "" "plot mean [exemplars2-sd] of turtles"
"meanmean1" 1.0 0 -7500403 true "" "plot mean [exemplars1-mean] of turtles"
"meansd1" 1.0 0 -13840069 true "" "plot mean [exemplars1-sd] of turtles"

SLIDER
49
10
221
43
num-turtles
num-turtles
2
200
37.0
1
1
NIL
HORIZONTAL

SLIDER
656
295
828
328
stopat
stopat
0
10000
5000.0
1
1
NIL
HORIZONTAL

MONITOR
209
261
303
306
NIL
meanmean2
4
1
11

MONITOR
303
261
397
306
NIL
meansd2
4
1
11

SLIDER
15
463
187
496
num-exemplars
num-exemplars
1
100
20.0
1
1
NIL
HORIZONTAL

MONITOR
354
403
425
448
NIL
ticks
17
1
11

SLIDER
662
47
834
80
considered-low
considered-low
0
100
0.0
1
1
NIL
HORIZONTAL

SLIDER
662
86
834
119
considered-high
considered-high
considered-low + 1
100
50.0
1
1
NIL
HORIZONTAL

SLIDER
662
10
834
43
k
k
0
1
0.178
0.001
1
NIL
HORIZONTAL

PLOT
840
10
1318
160
Probability of Adoption Function
Distance
Prob. Accept
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" ""

OUTPUT
839
160
1317
509
10

BUTTON
663
125
833
158
Mirrored Logistic
display-mirrored-positive-logistic
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
662
165
834
198
IdealPoint
IdealPoint
-20
20
10.0
1
1
NIL
HORIZONTAL

TEXTBOX
670
217
820
273
Note that k is a kind of selection coefficient. The larger it is, the more intense is selection.
11
0.0
1

MONITOR
21
216
135
261
NIL
meanmeaninitv1
3
1
11

MONITOR
117
216
231
261
NIL
meanmeaninitv2
3
1
11

SLIDER
205
458
377
491
avoidancepoint1
avoidancepoint1
-10
0
-7.8
0.1
1
NIL
HORIZONTAL

SLIDER
205
491
377
524
avoidancepoint2
avoidancepoint2
0
10
6.9
0.1
1
NIL
HORIZONTAL

SLIDER
20
124
192
157
damean2
damean2
-10
10
3.4
0.1
1
NIL
HORIZONTAL

SWITCH
55
497
174
530
avoidance
avoidance
0
1
-1000

@#$#@#$#@
## WHAT IS IT?

What we want to model here is mergers of vowels (or concepts or behavior). Will stick to 
one dimension for the sake of simplicity and time constraints. Idea is:

--- avoidance point----------------v1---v2------avoidance point ------

That is, either extreme on the spectrum is to be avoided, say it is hard to 
pronounce. So when an agent picks, the closer the utterance is to the middle
the mor likely it is to stick. This should drive both v1 and v2 to the middle.
Once that is working we can add some confusion noise and see what happens.

/1/ Have copied model 3 , done some renaming and gotten it to work as a model 3
/2/ Next is to take into account avoidancepoint1 and avoidancepoint2

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
NetLogo 6.1.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="e1 4 turtles varying exemplars" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>ticks &gt;= 500000</exitCondition>
    <metric>meanmean</metric>
    <metric>meansd</metric>
    <enumeratedValueSet variable="num-turtles">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dasd">
      <value value="1.5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="num-exemplars" first="10" step="5" last="25"/>
    <enumeratedValueSet variable="stopat">
      <value value="5605"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="damean">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="radius">
      <value value="3"/>
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
0
@#$#@#$#@
