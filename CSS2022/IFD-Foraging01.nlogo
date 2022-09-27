globals [ yellow-patch green-patch
count-at-green count-at-yellow
running-memory-size
green-rate yellow-rate ; empirical rates. Mean from the last number of segment ticks.
y-bottom y-top
x1 x2
yellow-list green-list
yellow-source green-source
source-locations ; This will be a list of patches that are sources.
  transit-patch
  green-bread-history
  yellow-bread-history
  green-pin
  yellow-pin
]
breed [ducks duck]
breed [sources source]
; these are food sources which we treat as special agents
sources-own [bread-present?
  bread-rate ; bread every n ticks
  history
]
ducks-own [own-source other-source
to-move
;; accumulators for marking time by location:
at-green   ; total number of ticks a duck spends at green
at-yellow  ; total number of ticks a duck spends at yellow
at-transit ; at-transit is the total number of ticks a duck spends undergoing transit.
in-transit ; in-transit is the time remaining to spend transiting
total-bread-yellow ; total bread collected at the yellow source
total-bread-green  ; total bread collected at the green source
total-bread-collected ; total bread collected over simulation
prob-move ; likelihood a duck will move
]

to setup
  clear-all
  ;;random-seed 17
  initialize-globals
  ; create food sources as turtles and initialize them.
  make-sources
  make-transit-patches
  make-ducks
  adjust-prob-move
  reset-ticks
  ; Create a history of bread-tossing at the two sources.
  warmup
  collect-warmup-statistics
end

to adjust-prob-move
  if pin-ducks? [
    ask green-patch [ask one-of ducks-here [set prob-move 0
       set green-pin [who] of self ]]
    ask yellow-patch [ask one-of ducks-here [set prob-move 0
       set yellow-pin [who] of self ]]
  ]
end

to initialize-globals
  ; the next four things are for calculating the linear probability of a transition
  set y-bottom 0
  set y-top 1
  set x1 -2
  set x2 4
  set running-memory-size 100 ; for remembering bread availability at different patches in the recent past
  ; the next two variables track the number of agents at the yellow or green patch at each tick. Initially
  ; set to 1 to avoid division by 0.
  set yellow-list [ 1 ]
  set green-list [ 1 ]
end

to make-sources
  create-sources 2
  set yellow-source source 0
  set green-source source 1
  set yellow-bread-history []
  set green-bread-history []

  ask sources [set bread-present? false
    set history []]
  ask source 0 [ set xcor 5
    set ycor 5
    set shape "face happy"
    set size 4
    set color yellow
    set bread-rate yellow-bread-rate]

  ask source 1 [ set xcor 27
    set ycor 27
  set shape "face neutral"
  set size 4
  set color green
  set bread-rate green-bread-rate
  ]
  set source-locations []
  ; Place the sources, (0,0) is in the southwest corner
  set yellow-patch patch 5 2
  set source-locations lput yellow-patch source-locations
  set green-patch patch 27 24
  set source-locations lput green-patch source-locations
  ask yellow-patch [set pcolor white]
  ask green-patch [set pcolor white]
end

to make-transit-patches
  ;; Initialize the global variable locations as a patch-set
  ;; of the destination patches.
  ;; set source-locations (patch-set yellow-patch green-patch)
  set transit-patch patch 0 0
  ask transit-patch [set pcolor white]
end

to make-ducks
  create-ducks population-size [
    ; assign the ducks randomly to one of the two patches
    set own-source one-of (list yellow-patch green-patch)
    ifelse own-source = yellow-patch [ set other-source green-patch ]
    [set other-source yellow-patch]
    set in-transit 0
    ; Put the turtles on their assigned source patches.
    move-to own-source
    ; These next variables track how long the ducks are at each patch (or in transit)
    set at-green 0
    set at-yellow 0
    set at-transit 0
    set prob-move p-move
  ]
  ask yellow-patch [set count-at-yellow count turtles-here]
  ask green-patch [set count-at-green count turtles-here]
end

to warmup
  ; see the call in setup
  ; Create a history of bread-tossing at the two sources.
  let bread-amount 0
  repeat segment [
    ask yellow-source [ set bread-amount toss-bread
      set yellow-bread-history lput bread-amount yellow-bread-history]
    ask green-source [ set bread-amount toss-bread
      set green-bread-history lput bread-amount green-bread-history]

  ; ask sources [toss]

 
  tick
  ]
  ask yellow-source [ set history yellow-bread-history ]
  ask green-source [set history green-bread-history ]
end

to collect-warmup-statistics
  ; turtle 0 and turtle 1 are the two sources.
  ; yellow-bread-rate and green-bread-rate set the rate of tossing, but yellow-rate and green-rate track the empirical rate
  ; of tossing.
  set yellow-rate mean [history] of turtle 0
  set green-rate mean [history] of turtle 1
end

to go
  if ticks = ticklimit [
    print random-float 100
    stop ]
    ;; (1)
  collect-statistics
  ; ask each source to toss (or not) a bread ball, depending on their schedule.
  ;; (2)
  ask sources [ toss ]
  ; Managing the ducks in transit between patches.  When transit? is false then the transit between patches is
  ; instantaneous and costless [M. economicus]
  ;; (3)
  if transit? [process-in-transit]
  ;; (4)
  process-ducks-not-in-transit
  ;; (5) do statistical bookkeeping
  accounting-department
  tick
end

to collect-statistics
  ; next two lines are for tracking the number of ducks at each patch.
  ask yellow-patch [ set count-at-yellow count turtles-here ]
  ask green-patch [ set count-at-green count turtles-here ]
end

to process-in-transit
  ; Decrement the remaining time for ducks in transit.
  ask ducks with [in-transit > 0] [set in-transit (in-transit - 1)
    ifelse in-transit = 0 [move-to own-source] ; in-transit is the time spent transiting
    [set at-transit (at-transit + 1)]]         ; at-transit is the count of ducks undergoing transit.
end

to process-ducks-not-in-transit
    ask ducks with [in-transit = 0] [
    ;; Increment the count of the patch you're at, so we can track how much
    ;; time each duck spends at each patch
    ;; (4a)
    ifelse own-source = yellow-patch ;patch 5 2
    [set at-yellow (at-yellow + 1)]
    [set at-green (at-green + 1)]
    ; Determine if the duck is to move to the other source. True if 1, False if 0.
    ;; (4b)
    set to-move move-immediate
    ;; (5)
    if to-move = true [
    ;; (5a)
       switch-source
    ;; (5b)
      ifelse transit?
      [ ; let temp own-source
        ; set own-source other-source
        ; set other-source temp
        ifelse transit-time > 0 [
        move-to transit-patch
          set in-transit transit-time ]
           ;;(5c)
          [move-to own-source]]
     ;;(6)
      [move-to own-source]
      ask yellow-patch [set count-at-yellow count turtles-here]
      ask green-patch [set count-at-green count turtles-here]
    ]
  ]
end

to accounting-department
    set yellow-list fput count-at-yellow yellow-list
  set green-list fput count-at-green green-list
  if length yellow-list > segment [
    set yellow-list sublist yellow-list 0 segment ]
  if length green-list > segment [
    set green-list sublist green-list 0 segment ]
;  if ticks > segment [
;  set-current-plot "bread per duck"
;  set-current-plot-pen "default"
;  plotxy ticks 100 * yellow-rate / mean yellow-list
;  ;plotxy ticks 10 * ((100 / yellow-bread-rate) / (mean yellow-list))
;  set-current-plot-pen "pen-1"
;    plotxy ticks 100 * green-rate / mean green-list
;   ;plotxy ticks 10 * ((100 / green-bread-rate) / (mean green-list))
;  ]
  set green-bread-history lput bread-per-duck-green green-bread-history 
  set green-bread-history but-first green-bread-history 
  set yellow-bread-history lput bread-per-duck-yellow yellow-bread-history 
  set yellow-bread-history but-first yellow-bread-history 
end

to switch-source
  ;; 2020-07-17 We went over this carefully and think it is now correct.
  ;; At least we hope so.
   let temp own-source
   set own-source other-source
   set other-source temp
  ;let new-source own-source
  ;let newlist remove own-source source-locations
  ;set own-source one-of newlist
  ;set other-source new-source
  ; move-to own-source
end
;to go-to [where]
;move-to where ;other-source
;  let temp own-source
;  set own-source other-source
;  set other-source temp
;  ;;set to-move 3 . ;; 2020-04-24 sok verified that this line isn't needed.
;end

to go-from [where]
  set own-source other-source
  set other-source where
  move-to  own-source
end


to toss
  ;; We need to do two things: (1) figure out how much bread is tossed
  ;; toss-bread returns a scalar equal to the bread tossed
  let bread-tossed toss-bread
  ;; (2) allocate the food tossed to the ducks present at the source
  allocate-bread bread-tossed

  set history fput bread-tossed history
  if length history > segment [
    set history sublist history 0 segment ]

  ;;let to-add 1 + (random bread-balls)
  ;;ifelse ticks mod bread-rate = 0
  ;;[set bread-present? true
  ;;set history fput to-add history]
  ;;[set bread-present? false
  ;;  set history  fput 0 history]
  ;;if length history > segment [
  ;; set history sublist history 0 segment ]

end

to-report toss-bread
  let tossbread 0
  if [ who ] of self = 0 [set tossbread yellow-bread-rate ]
  if [ who ] of self = 1 [set tossbread green-bread-rate ]
  report tossbread
end

to allocate-bread [ amount ]
  let ducks-present -1
  if [who] of self = 0 [ask yellow-patch [ set ducks-present count ducks-here ]]
  if [who] of self = 1 [ask green-patch [ set ducks-present count ducks-here ]]

  let unit 0
  if [ who ] of self = 0 and ducks-present > 0 [ask yellow-source [ask yellow-patch [ ask ducks-here [
    set unit amount / ducks-present ;;num-ducks
    set total-bread-collected total-bread-collected + unit
    set total-bread-yellow total-bread-yellow + unit
  ]]]]
  if [ who ] of self = 1 and ducks-present > 0 [ask green-source [ask green-patch [ ask ducks-here [
    set unit amount / ducks-present ;;num-ducks
    set total-bread-collected total-bread-collected + unit
    set total-bread-green total-bread-green + unit
  ]]]]
end

;to-report probability-of-change [other-value own-value]
;;  let high 10
;;  let low -10
;;  let a 0.5
;;  let b 0.05
;;  if other-value - own-value >= high [ report 1 ]
;;  if other-value - own-value <= low [ report 0 ]
;;  report a + (b * (other-value - own-value))
;  let x other-value - own-value
;  report linear-growth-segment x
;end

;to test
;;  let x probability-of-change (.1) (.3)
;;  print x
;  let candidates 0
;  ask yellow-patch [set candidates other source-locations]
;  print candidates
;end

to-report move-immediate
  ifelse own-source = yellow-patch
  ;; (1-yellow)
  [ifelse bread-per-duck-yellow > (bread-per-duck-green) ; - (movement-threshold * 0.001))
    [report False]
    ;; (2-yellow)
    [ifelse random-float 1 < prob-move and count other turtles-here > 0
      [report true] 
      [report false] ]

  ]

  ;; (1-green)
  [ifelse bread-per-duck-yellow > (bread-per-duck-green) ; + (movement-threshold * 0.001))
    [ifelse random-float 1 < prob-move and count other turtles-here > 0
      [report true] 
      [report false] ]
      ;; (2-green)
    [report False]]
end

;to-report linear-growth-segment [ x ] ; y-bottom y-top x1 x2]
;  ;; Given the inputs, returns a probability value if the-C = 1.
;  ;; Here we follow the formulation in
;  ;; https://people.richland.edu/james/lecture/m116/logs/models.html
;  if x < x1 [report y-bottom]
;  if x > x2 [report y-top]
;  let rise y-top - y-bottom
;  let da-run x2 - x1
;  let slope rise / da-run
;  let y-intercept y-bottom + slope * abs(0 - x1)
;  report  y-intercept + slope * x
;end

to-report bread-per-duck-yellow
  let decount 0
  ask yellow-patch [set decount count ducks-here]
  let toreport 0
  ifelse decount > 0 [
    set toreport (yellow-rate / decount)
    report toreport]
  [report 0]
;  ifelse transit?
;  [ifelse count-at-yellow > 0 [
;    report (mean [ history ] of yellow-source) / count-at-yellow]
;    [ report (mean [ history ] of yellow-source) ]]
;
;    [report (mean [ history ] of yellow-source) / (count-at-yellow)]
;  ;  report 100 * yellow-rate / max (list 1 mean yellow-list)
end

to-report bread-per-duck-green
  let decount 0
  ask green-patch [set decount count ducks-here]
  let toreport 0
  ifelse decount > 0 [
    set toreport (green-rate / decount)
    report toreport]
  [report 0]

;  ifelse transit?
;  [ifelse count-at-green > 0 [
;    report (mean [ history ] of green-source) / count-at-green]
;    [report (mean [ history ] of green-source) ]]
;  [report (mean [ history ] of green-source) / count-at-green]
;
;  ;100 * green-rate / max (list 1 mean green-list)
end

;to-report percent-time-yellow
;  report mean [at-yellow] of ducks / (mean [at-yellow] of ducks + mean [at-green] of ducks)
;end
;
;to-report recent-percent-time-yellow
;  report mean yellow-list / (mean yellow-list + mean green-list)
;end

to-report ducks-in-transit
  report count ducks with [in-transit > 0]
end

;to-report p-pop-yellow
;  report mean [at-yellow] of ducks / (ticks - segment)
;end
;
;to-report p-pop-green
;  report mean [at-green] of ducks / (ticks - segment)
;end
;
;to-report p-pop-transit
;  report mean [at-transit] of ducks / (ticks - segment)
;end

to-report count-at-transit
  report count ducks with [in-transit > 0]
end

to-report yellow-green-ratio
  report (count-at-yellow / count-at-green) / (yellow-bread-rate / green-bread-rate)
end

to-report pin-green
  report [total-bread-collected] of duck green-pin
end

to-report pin-yellow
  report [total-bread-collected] of duck yellow-pin
end

to-report total-bread
  report mean [total-bread-collected] of ducks with [prob-move > 0]
end
@#$#@#$#@
GRAPHICS-WINDOW
453
10
890
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
0
32
0
32
0
0
1
ticks
30.0

BUTTON
88
69
154
102
NIL
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

MONITOR
1001
266
1116
311
NIL
count-at-yellow
0
1
11

MONITOR
892
266
1002
311
NIL
count-at-green
0
1
11

MONITOR
893
312
1002
357
NIL
yellow-rate
17
1
11

MONITOR
1002
312
1115
357
NIL
green-rate
17
1
11

BUTTON
205
70
273
105
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
0
451
406
658
ducks per patch
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -1184463 true "" "plot mean yellow-list"
"pen-1" 1.0 0 -13840069 true "" "plot mean green-list"

MONITOR
890
175
1003
220
NIL
mean yellow-list
3
1
11

MONITOR
1005
175
1116
220
NIL
mean green-list
3
1
11

SLIDER
286
222
458
255
yellow-bread-rate
yellow-bread-rate
0
20
19.0
1
1
NIL
HORIZONTAL

SLIDER
286
255
458
288
green-bread-rate
green-bread-rate
0
20
5.0
1
1
NIL
HORIZONTAL

MONITOR
893
358
1063
403
NIL
bread-per-duck-yellow
5
1
11

MONITOR
893
403
1063
448
NIL
bread-per-duck-green
5
1
11

TEXTBOX
25
152
264
412
The yellow-rate and green-rate are in units of bread per tick, as are the yellow-bread-rate and the green-bread-rate.
13
0.0
1

SLIDER
286
288
458
321
segment
segment
0
5000
500.0
1
1
NIL
HORIZONTAL

MONITOR
892
220
1115
265
NIL
count-at-transit
17
1
11

SWITCH
286
121
459
154
transit?
transit?
1
1
-1000

SLIDER
286
155
458
188
transit-time
transit-time
0
120
5.0
1
1
NIL
HORIZONTAL

SLIDER
286
188
458
221
population-size
population-size
24
250
250.0
1
1
NIL
HORIZONTAL

SLIDER
286
320
458
353
p-move
p-move
0
1
0.2
0.01
1
NIL
HORIZONTAL

INPUTBOX
286
355
456
416
ticklimit
3500.0
1
0
Number

PLOT
413
451
832
766
bread per duck yellow
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -1184463 true "" "plot bread-per-duck-yellow"

PLOT
833
451
1226
766
bread per duck green
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -11085214 true "" "plot bread-per-duck-green"

MONITOR
890
39
1138
84
NIL
variance [total-bread-yellow] of ducks
3
1
11

MONITOR
890
85
1137
130
NIL
variance [total-bread-green] of ducks
3
1
11

MONITOR
890
129
1028
174
NIL
yellow-green-ratio
4
1
11

SWITCH
286
415
455
448
pin-ducks?
pin-ducks?
1
1
-1000

@#$#@#$#@
## Notes on the model

We have setup (which creates the resource sources and foragers) and warmup (which allows the foragers to observe the rate of distribution by the sources).  We are going to try a variety of methods that the foragers will use to decide which source to exploit; we'll do this by writing a variety of "go"  commands.

In the first version of the model, 001, the ducks were capable of instantaneous travel between patches.  In this version, we impose a cost in terms of travel time between patches: When a duck decides to switch patches, it must take  some amount of time between patches to get from one to the other.  We model this by transferring ducks to a "transit patch" where they must cool their heels for some number of ticks.  There is a transit? switch; when it's off, it is model 001 with instantaneous transit time and when it's on a fixed transit time cost is imposed on the ducks.

## Development Log

01/04/2021

Features we intend to add:

1. Switch scale of bread distribution.  A higher number is faster.

2. Actual collection with equal distribution.

3. Track where (which source) they got the bread.

4. Pin ducks

5. Dynamic rate change 

6. Rate vs. volume (of bread balls)


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
NetLogo 6.2.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="Pure-IDF-01" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>bread-per-duck-yellow</metric>
    <metric>bread-per-duck-green</metric>
    <metric>mean yellow-list</metric>
    <metric>mean green-list</metric>
    <metric>p-pop-yellow</metric>
    <metric>p-pop-green</metric>
    <metric>p-pop-transit</metric>
    <enumeratedValueSet variable="transit-time">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-move">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="green-bread-rate">
      <value value="5"/>
      <value value="7"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="segment">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="transit?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ticklimit">
      <value value="3500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population-size">
      <value value="250"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="yellow-bread-rate">
      <value value="3"/>
      <value value="5"/>
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bread-balls">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Pure-IFD-Transit-On-01" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>bread-per-duck-yellow</metric>
    <metric>bread-per-duck-green</metric>
    <metric>mean yellow-list</metric>
    <metric>mean green-list</metric>
    <metric>p-pop-yellow</metric>
    <metric>p-pop-green</metric>
    <metric>p-pop-transit</metric>
    <metric>transit-time</metric>
    <enumeratedValueSet variable="transit-time">
      <value value="5"/>
      <value value="20"/>
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-move">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="green-bread-rate">
      <value value="5"/>
      <value value="7"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="segment">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="transit?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ticklimit">
      <value value="3500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population-size">
      <value value="250"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="yellow-bread-rate">
      <value value="3"/>
      <value value="5"/>
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bread-balls">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="TemporalVariationExperiment" repetitions="200" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>yellow-green-ratio</metric>
    <metric>yellow-bread-rate / green-bread-rate</metric>
    <enumeratedValueSet variable="transit-time">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-move">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="green-bread-rate">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="segment">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="transit?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ticklimit">
      <value value="3500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population-size">
      <value value="250"/>
    </enumeratedValueSet>
    <steppedValueSet variable="yellow-bread-rate" first="3" step="2" last="21"/>
  </experiment>
  <experiment name="TemporalVariationExperiment-NoTransit" repetitions="200" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>yellow-green-ratio</metric>
    <metric>yellow-bread-rate / green-bread-rate</metric>
    <enumeratedValueSet variable="transit-time">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-move">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="green-bread-rate">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="segment">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="transit?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ticklimit">
      <value value="3500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population-size">
      <value value="250"/>
    </enumeratedValueSet>
    <steppedValueSet variable="yellow-bread-rate" first="3" step="2" last="21"/>
  </experiment>
  <experiment name="Pure-IFD-experiment" repetitions="30" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>count-at-yellow</metric>
    <metric>count-at-green</metric>
    <metric>bread-per-duck-yellow</metric>
    <metric>bread-per-duck-green</metric>
    <enumeratedValueSet variable="transit-time">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pin-ducks?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-move">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="green-bread-rate">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="segment">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="transit?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ticklimit">
      <value value="3500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population-size">
      <value value="250"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="yellow-bread-rate">
      <value value="3"/>
      <value value="5"/>
      <value value="10"/>
      <value value="15"/>
      <value value="20"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Transit-Experiment" repetitions="30" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>count-at-yellow</metric>
    <metric>count-at-green</metric>
    <metric>bread-per-duck-yellow</metric>
    <metric>bread-per-duck-green</metric>
    <enumeratedValueSet variable="transit-time">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pin-ducks?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-move">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="green-bread-rate">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="segment">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="transit?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ticklimit">
      <value value="3500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population-size">
      <value value="250"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="yellow-bread-rate">
      <value value="3"/>
      <value value="5"/>
      <value value="10"/>
      <value value="15"/>
      <value value="20"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Pure-IFD-Pinning-experiment" repetitions="30" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>count-at-yellow</metric>
    <metric>count-at-green</metric>
    <metric>bread-per-duck-yellow</metric>
    <metric>bread-per-duck-green</metric>
    <metric>pin-yellow</metric>
    <metric>pin-green</metric>
    <metric>total-bread</metric>
    <enumeratedValueSet variable="transit-time">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pin-ducks?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-move">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="green-bread-rate">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="segment">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="transit?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ticklimit">
      <value value="3500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population-size">
      <value value="250"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="yellow-bread-rate">
      <value value="3"/>
      <value value="5"/>
      <value value="10"/>
      <value value="15"/>
      <value value="20"/>
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
