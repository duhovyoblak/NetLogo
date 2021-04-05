;;============================================================================
;; Travel's Salesman Problem by Evolution
;;----------------------------------------------------------------------------
globals      [ pocet_agentov       ;; pocet agentov = koeficient_agentov * pocet_miest  
               pocet_smrti         ;; absolutny pocet bankrotov
                      ;; absolutny pocet susedov
               max_vek             ;; maximalny vek dozitia agentov
               hranica_expanzie    ;; hranica vyuzitia potencialu zony, od ktorej je mozna expanzia
               n_opt               ;; pocet vykonanych optimalizacii
               n_exp               ;; pocet vykonanych expanzii
               n_org               ;; pocet vykonanych organickych predlzeni
               n_win               ;; pocet vitazov - ukoncenych ciest
               SM_who              ;; identifikacia salesmana
               SM_pot 
               SM_len              ;; dlzka najkratsej cesty
             ] 

;;----------------------------------------------------------------------------
breed        [ towns town        ] ;; trieda miest
towns-own    [ 
               paths               ;; pocet ciest, prechadzajucich mestom
             ]

breed        [ salesmans salesman] ;; trieda obchodnych cestujucich
salesmans-own[ 
               gen_path            ;; list segmentov trasy (joins) podla poradia
               dev_towns           ;; zoznam miest v gen_path

               fen_age             ;; vek agenta
               fen_length          ;; celkova dlzka trasy
               
               fen_xmin            ;; xmin of gen_path
               fen_xmax            ;; xmax of gen_path
               fen_ymin            ;; ymin of gen_path
               fen_ymax            ;; ymax of gen_path
               fen_T_pot           ;; potencialny pocet miest v zone
               fen_T_act           ;; pocet miest gen_path

               fen_Q_avg           ;; kvalita trasy

               fen_value           ;; komplexna hodnota trasy
             ]

breed        [ winners winner    ] ;; trieda obchodnych cestujucich, ktori dospeli do ciela
winners-own  [ 
               gen_path            ;; list segmentov trasy (joins) podla poradia
               dev_towns           ;; zoznam miest v gen_path

               fen_age             ;; vek agenta
               fen_length          ;; celkova dlzka trasy

               fen_xmin            ;; xmin of gen_path
               fen_xmax            ;; xmax of gen_path
               fen_ymin            ;; ymin of gen_path
               fen_ymax            ;; ymax of gen_path
               fen_T_pot           ;; potencialny pocet miest v zone
               fen_T_act           ;; pocet miest gen_path

               fen_Q_avg           ;; kvalita trasy

               fen_value           ;; komplexna hodnota trasy
             ]

;;----------------------------------------------------------------------------
directed-link-breed[ joins join ]  ;; trieda spojeni
joins-own    [
               quality             ;; kvalita joinu - poradie od najmensieho 
             ]

;;============================================================================
;; Inicializacia modelu
;;----------------------------------------------------------------------------
to setup
  
  ;;--------------------------------------------------------------------------
  ;; vseobecne nastavenie
  ;;--------------------------------------------------------------------------
  clear-all                      ;; vsetko zmaz
  random-seed new-seed           ;; generuj rovnake nahodne cisla
  
  ;; nastav cierny podklad
  ask patches [ set pcolor black ] 
  
  ;;--------------------------------------------------------------------------
  ;; Globalne parametre modelu
  ;;--------------------------------------------------------------------------
  set pocet_agentov ( koeficient_agentov * pocet_miest )
  if pocet_agentov < 1 [ set pocet_agentov 1 ]
  
  if pocet_susedov >= pocet_miest [set pocet_susedov pocet_miest - 1]
  
  set max_vek           2 * pocet_miest
  set hranica_expanzie  100

  ;;--------------------------------------------------------------------------
  ;; vytvorenie miest
  ;;--------------------------------------------------------------------------
  towns_create     pocet_miest     ;; vytvor mnozinu miest
  
  ;;--------------------------------------------------------------------------
  ;; Reset vypoctu
  ;;--------------------------------------------------------------------------
  reset 

end
;;----------------------------------------------------------------------------
to reset
  
  ask winners   [ die ]
  ask salesmans [ die ]
  
  random-seed new-seed               ;; generuj ine nahodne cisla
  
  ;;--------------------------------------------------------------------------
  ;; vytvorenie agentov
  ;;--------------------------------------------------------------------------
  salesmans_create pocet_agentov 

  set n_opt    0
  set n_exp    0
  set n_org    0
  set n_win    0
  set SM_who   0
  set SM_pot   0
  set SM_len   0

  ;;--------------------------------------------------------------------------
  ;; prvotne vykreslenie
  ;;--------------------------------------------------------------------------
  reset-ticks                      ;; vynuluj pocitadlo tiknuti hodin 
  model_show
  
  
end
;;============================================================================
;; Metody riadenia simulacie
;;----------------------------------------------------------------------------
to krok                            ;; jeden krok simulacie
  
  ;;--------------------------------------------------------------------------
  ;; odlozenie vitazov
  ;;--------------------------------------------------------------------------
  ask salesmans with [ fen_T_act >= pocet_miest ] [ set breed winners ] 
  set n_win count winners
  
  if n_win > 0 [ set SM_len [fen_length] of min-one-of winners [ fen_length ] ]                             
  
  ;;--------------------------------------------------------------------------
  ;; narodenie novych salesmanov
  ;;--------------------------------------------------------------------------
  let pocet_novych pocet_agentov - count salesmans
  salesmans_create pocet_novych 
  
  ;;--------------------------------------------------------------------------
  ;; vyvoj a krizenie
  ;;--------------------------------------------------------------------------
  ask salesmans 
  [ 
    ;; rozhodni o expanzii
    ifelse (fen_T_act / fen_T_pot * 100) < hranica_expanzie [ optimalizacia ]
                                                            [ expanzia      ]
 
    ;; vyhodnot fenotyp
    fenotyp
  ] 
  
  ;;--------------------------------------------------------------------------
  ;; smrt vekom
  ;;--------------------------------------------------------------------------
  ask salesmans with [ fen_age > max_vek ] [ die ]

  ;;--------------------------------------------------------------------------
  ;; smrt nedostatocnou obchodnou hodnotou
  ;;--------------------------------------------------------------------------
  set pocet_smrti   round (percento_bankrotov * count salesmans / 100)
  ask min-n-of pocet_smrti salesmans[ fen_value ] [ die ]
  
  ;;--------------------------------------------------------------------------
  ;; zobrazenie vysledkov
  ;;--------------------------------------------------------------------------
  model_show
  tick                             ;; pohni rucickami vnutornych hodin

end
;;----------------------------------------------------------------------------
to go                              ;; spustenie simulacie - stale dookola
  krok                             ;; vykonaj jeden krok simulacie
end
;;============================================================================
;; Metody TS agentov
;;----------------------------------------------------------------------------
to salesmans_create[ cnt ]

  create-salesmans cnt [
    
    hide-turtle

    ;; toto je finta, vytvorenie listu, obsahujuceho jeden item
    set gen_path but-first list 1 one-of joins
  
    set fen_age 0
  
    fenotyp
    
  ]
end  
;;----------------------------------------------------------------------------
to fenotyp  ;; vyhodnot fenotyp v kontexte Salesmana

  ;; zvys vek Salesmana
  set fen_age fen_age + 1
    
  ;; urcenie celkovej dlzky trasy  
  set fen_length path_length gen_path

  ;; urcenie zoznamu towns v gen_path
  set dev_towns sentence ( [end1] of item 0 gen_path ) (map [[end2] of ?] gen_path)

  ;; urcenie koordinatov hranic zony gen_path
  set fen_xmin min map [[xcor] of ?] dev_towns
  set fen_xmax max map [[xcor] of ?] dev_towns
  set fen_ymin min map [[ycor] of ?] dev_towns
  set fen_ymax max map [[ycor] of ?] dev_towns
  
  ;; urcenie poctu miest v zone
  set fen_T_pot zone_towns fen_xmin fen_xmax fen_ymin fen_ymax

  ;; urcenie poctu miest v path  
  set fen_T_act length gen_path + 1
    
  ;; urcenie priemernej kvality cesty
  set fen_Q_avg path_quality gen_path
    
  ;; urcenie obchodnej hodnoty cesty
;  set fen_value   fen_T_act * ( fen_T_act / fen_length )
;  set fen_value   fen_T_act                                                          / fen_Q_avg
   set fen_value   fen_T_act *                              ( fen_T_act / fen_T_pot ) * ( 1 / fen_Q_avg )
;  set fen_value   fen_T_act * ( fen_T_act / fen_length ) * ( fen_T_act / fen_T_pot ) / fen_Q_avg

end
;;----------------------------------------------------------------------------
to optimalizacia ;; optimalizuj obchodnu trasu kontext Salesman

  ;; Ziskaj zoznam vsetkych miest v zone mimo gen_path
  let extowns towns with [ xcor >= [fen_xmin] of myself and 
                           xcor <= [fen_xmax] of myself and 
                           ycor >= [fen_ymin] of myself and
                           ycor <= [fen_ymax] of myself and
                           not member? self [dev_towns] of myself ]
  
  ;;--------------------------------------------------------------------------
  ;; Prechadzaj cez celu path

  let gen_sort sort-by [ [quality] of ?1 > [quality] of ?2 ] gen_path
    
  foreach gen_sort
  [
    ;; Mesta, tvoriace spracovavany join gen_sort-u
    let TA [end1] of ?
    let TB [end2] of ?
    
    ;; Zoznam miest vhodnych pre optimalizaciu
    let opttowns extowns with [ in-join-from TA != nobody and
                                out-join-to  TB != nobody     ]

    ;; Ak existuje aspon jedno vhodne miesto, potom optimalizuj
    if any? opttowns [
      
      ;; Vyber si jedno z vhodnych miest
      let opt one-of opttowns
      
      ;; Urci joiny
      let JAO (join [who] of TA  [who] of opt)
      let JOB (join [who] of opt [who] of TB )
      let JAB (join [who] of TA  [who] of TB )
  
      ;; Ak su opt-joiny dost kvalitne, potom optimalizacia
      if ( [quality] of JAO + [quality] of JOB ) <= ( 1.5 * [quality] of JAB ) [
      
        ;; urci poziciu JAB
        let ind position JAB gen_path
      
        ;; Vyber z gen_path casti po mesto TA a po meste TB     
        let SA sublist gen_path        0              ind
        let SB sublist gen_path (ind + 1) length gen_path 
      
        ;; Pripoj join TA - opt
        set gen_path lput JAO SA

        ;; Pripoj join opt - TB
        set gen_path lput JOB gen_path
      
        ;; Pripoj cast do konca
        set gen_path sentence gen_path  SB

        ;; Zvys pocitadlo optimalizacii
        set n_opt n_opt + 1
        
        stop  ;vyskoc z optimalizacie
      ] 
    ]
  ]
    
  ;;--------------------------------------------------------------------------                                 
  ;; inak organicky rast

  ;; Zisti posledne mesto
  let ET last dev_towns

  ;; Ziskaj zoznam vsetkych joinov z posledneho mesta
  let exjoins joins with [ end1 = ET and not member? end2 [dev_towns] of myself ]  
      
  ifelse any? exjoins [
        
    ;; Pripoj jeden join
    set gen_path lput min-one-of exjoins[ quality ] gen_path
    
    set n_opt n_opt + 1

  ]
  [ set n_opt n_opt + 0.1 
  ]

end
;;----------------------------------------------------------------------------
to expanzia ;; expanduj obchodnu trasu kontext Salesman

  ;; vyber potencialnych partnerov na krizenie podla max prekryvu

  let source other salesmans with [ ( not prekryv [fen_xmin] of myself [fen_xmax] of myself fen_xmin fen_xmax
                                      and prekryv [fen_ymin] of myself [fen_ymax] of myself fen_ymin fen_ymax)
                                    or
                                    ( not prekryv [fen_ymin] of myself [fen_ymax] of myself fen_ymin fen_ymax
                                      and prekryv [fen_xmin] of myself [fen_xmax] of myself fen_xmin fen_xmax ) ]

;  let source other salesmans with [ ( not prekryv fen_xmin fen_xmax [fen_xmin] of myself [fen_xmax] of myself 
;                                      and prekryv fen_ymin fen_ymax [fen_ymin] of myself [fen_ymax] of myself )
;                                    or
;                                    ( not prekryv fen_ymin fen_ymax [fen_ymin] of myself [fen_ymax] of myself
;                                      and prekryv fen_xmin fen_xmax [fen_xmin] of myself [fen_xmax] of myself ) ]


  ;; vyber potencialnych partnerov na krizenie
  let potential source with [ xing [dev_towns] of myself [dev_towns] of self != -1 ]
 
  ;; vyber podla fenotypu
  ;let pot count potential
  ;let vyb round count potential / 2
    
  let my_lover max-one-of potential[ fen_value ]

  ;; ak som si niekoho vybral, nasleduje krizenie
  ifelse my_lover != nobody 
    [ 
      sex_with my_lover 

      ;; krizenec je novy salesman - vek sa resetne na 0
      set fen_age 0
      set n_exp   n_exp + 1
    ]
  
    ;; inak organicky rast
    [
      ;; Zisti posledne mesto
      let ET last dev_towns

      ;; Ziskaj zoznam vsetkych joinov z posledneho mesta
      let exjoins joins with [ end1 = ET and not member? end2 [dev_towns] of myself ]  
      
      ifelse any? exjoins [
        
        ;; Pripoj jeden join
        set gen_path lput min-one-of exjoins[ quality ] gen_path
    
        set n_org   n_org + 1
      ]
      [ set n_org   n_org + 0.1 
      ]
    ]

end;
;;----------------------------------------------------------------------------
to sex_with [lover]
  
  ;; zistime index mesta prveho krizenia v gen_path
  let indA xing dev_towns [dev_towns] of lover

  ;; zistime mesto krizenia
  let XT item indA dev_towns
  
  ;; zistime index mesta prveho krizenia v lover_path
  let indB position XT [dev_towns] of lover
  
  ;; zapamatame si gen_path lovera
  let lover_path [gen_path] of lover
  
  ;; Vyber z gen_path casti po mesto TX     
  let SA sublist   gen_path    0        indA
  let SB sublist lover_path indB length lover_path 
      
  ;; Pripoj cast do konca
  set gen_path sentence SA SB
end  
;;----------------------------------------------------------------------------
to-report xing[ lA lB ]

  let ind 0
  
  ;; Pre vsetky mesta zoznamu A
  foreach lA
  [
    ;; existuje spolocne mesto ? 
    if member? ? lB [ 
      
      ;;za xing povazujeme krizenie az od druheho mesta lA
      ifelse ind > 0 [report ind] [report -1] ]
    
    set ind ind + 1
  ]
  report -1
end
;;----------------------------------------------------------------------------
to-report non_prekryv[ amin amax bmin bmax ]

  let la amax - amin
  let lb bmax - bmin
  
  if la > lb [ set lb la ]
  
  ifelse amin <= bmin
  
    [ ifelse ( amax - bmin ) / lb * 100 < max_prekryv [report True] [report False] ]
    [ ifelse ( bmax - amin ) / lb * 100 < max_prekryv [report True] [report False] ]
    
end
;;----------------------------------------------------------------------------
to-report prekryv[ amin amax bmin bmax ]

  let la amax - amin
  
  ifelse amin > bmin
  
    [ ifelse ( bmax - amin ) / la * 100 > max_prekryv [report True] [report False] ]
    [ ifelse ( amax - bmin ) / la * 100 > max_prekryv [report True] [report False] ]
    
end
;;============================================================================
;; Metody miest
;;----------------------------------------------------------------------------
to towns_create[ cnt ]             ;; vytvor mnozinu miest

  ;;--------------------------------------------------------------------------
  ;; vytvor [cnt] miest
  
  create-towns cnt
  ask towns[ setxy random-xcor random-ycor 
             set shape "dot" 
             set color  yellow  
             ; set label who
           ]
  
  ;;--------------------------------------------------------------------------
  ;; vytvor [pocet_susedov] joins k najblizsim mestam
  let neigh pocet_susedov + 1

  ask towns [ create-joins-to other min-n-of neigh towns[distance myself] ]
  
  ask towns [ let q 1
              foreach sort-on[link-length] my-out-joins 
              [ 
                ask ? [ set quality q 
                        if q < 8 [ set color violet  ]
                        if q < 7 [ set color magenta ]
                        if q < 6 [ set color red     ]
                        if q < 5 [ set color orange  ]
                        if q < 4 [ set color yellow  ]
                        if q < 3 [ set color green   ]
                        if q < 2 [ set color lime    ]
                      ]
                set q (q + 1)
              ]
            ]
end
;;============================================================================
;; Metody brute force
;;----------------------------------------------------------------------------
to brute_force
  
  ask winners [ die ]
  
  let init_joins sort joins
  
  foreach init_joins [
    
    let init_path but-first list 1 ?
    
    do_path init_path
    
  ]
  
end
;;----------------------------------------------------------------------------
to do_path[ path ]
  
  
  ;; urcenie zoznamu towns v gen_path
  let X_towns sentence ( [end1] of item 0 path ) (map [[end2] of ?] path)

  ;; rozhodnutie o rieseni
  ifelse length X_towns = count towns
  
    ;; trivialne riesenie
    [
      add_winner path
      
    ]
    ;; netrivialne riesenie
    [
      ;; Posledne mesto
      let ET last X_towns
      
      ;; Ziskaj zoznam vsetkych joinov z posledneho mesta
      let NJ sort joins with [ end1 = ET and not member? end2 X_towns  ]  
      
      foreach NJ [
        do_path lput ? path
      ]
    ]
  
end
;;----------------------------------------------------------------------------
to add_winner[ path ]
  
  salesmans_create 1
  let W max-one-of salesmans [who]

  ask W [ 
    set breed     winners
    set gen_path  path
    fenotyp
  ]

  set n_win count winners 
  set SM_len [fen_length] of min-one-of winners [ fen_length ]                             

  ;;--------------------------------------------------------------------------
  ;; zobrazenie vysledkov
  ;;--------------------------------------------------------------------------
  model_show
  tick                             ;; pohni rucickami vnutornych hodin
  
end
;;============================================================================
;; Pomocne metody
;;----------------------------------------------------------------------------
to-report zone_towns[ xmin xmax ymin ymax ]
  
  report count towns with [ xcor >= xmin and 
                            xcor <= xmax and 
                            ycor >= ymin and
                            ycor <= ymax ]

end  
;;----------------------------------------------------------------------------
to-report path_length[ jlist ]
  
  let dist 0

  foreach jlist [
    ask ? [ set dist dist + link-length ]
  ]
  
  report dist
end  
;;----------------------------------------------------------------------------
to-report path_quality[ jlist ]
  
  let qual 0
  
  foreach jlist [
    ask ? [ set qual qual + quality * quality ]
  ]
  
  set qual qual / length jlist
  
  report qual
end  
;;============================================================================
;; Metody zobrazenia
;;----------------------------------------------------------------------------
to model_show
  
  joins_clear
  
  if zobraz = "Joins"           [ joins_show    ]
  if zobraz = "Best value"      [ value_show    ]
  if zobraz = "Max towns path"  [ count_show    ]
  if zobraz = "Top 5 winners"   [ winners_show  ]
  if zobraz = "Shortest path"   [ short_show    ]

end
;;----------------------------------------------------------------------------
to joins_clear
  ask joins [hide-link]
end
;;----------------------------------------------------------------------------
to joins_show
  ask joins [show-link] 
end  
;;----------------------------------------------------------------------------
to value_show
  
  ask max-one-of salesmans [fen_value] [
  
    foreach gen_path [
      ask ? [ show-link ]
    ]
    
    set SM_who self
    set SM_pot [fen_T_act] of self / [fen_T_pot] of self * 100
  ]
  
end  
;;----------------------------------------------------------------------------
to count_show
  
  ask max-one-of salesmans [fen_T_act] [
  
    foreach gen_path [
      ask ? [ show-link ]
    ]
    set SM_who self
    set SM_pot [fen_T_act] of self / [fen_T_pot] of self * 100  
  ]
end  
;;----------------------------------------------------------------------------
to short_show
  
  if count winners > 0 
  [ 
    ask min-one-of winners [fen_length] 
    [ 
      foreach gen_path [ ask ? [ show-link ]      ]
    ]  
    set SM_who -1
    set SM_pot -1
  ]
end  
;;----------------------------------------------------------------------------
to winners_show
  
  let nn count winners
  if nn > 5 [ set nn 5 ]
  
  if  nn > 0 
  [ 
    ask min-n-of nn winners [ fen_length ] 
    [ 
      foreach gen_path [ ask ? [ show-link ] ]
    ]  
  ]
end  
;;============================================================================
;; Koniec suboru
;;----------------------------------------------------------------------------
@#$#@#$#@
GRAPHICS-WINDOW
200
10
715
546
-1
-1
5.0
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
100
0
100
1
1
1
ticks
30.0

BUTTON
11
142
191
175
Setup
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
70
243
132
276
One step
krok
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
723
381
1312
545
Convergence of shortests paths to minimum
Ticks
Shortest's Path
0.0
500.0
0.0
100.0
true
true
"set-plot-x-range 0 500\nset-plot-y-range 0 30 * count towns\n\nset-current-plot-pen \"Length\"" "if any? winners \n[\n  let yy [fen_length] of max-one-of winners [fen_length]\n  set-plot-y-range 0 round yy\n]"
PENS
"Length" 1.0 0 -8330359 true "" "if count winners > 0 [plot min [fen_length ] of winners]"

BUTTON
136
243
191
276
Go !
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
11
29
191
62
pocet_miest
pocet_miest
3
200
100
1
1
NIL
HORIZONTAL

SLIDER
11
103
191
136
koeficient_agentov
koeficient_agentov
0
10
2
1
1
NIL
HORIZONTAL

TEXTBOX
14
10
164
28
Static settings
11
0.0
1

SLIDER
14
384
186
417
max_prekryv
max_prekryv
0
20
3
1
1
NIL
HORIZONTAL

SLIDER
13
346
185
379
percento_bankrotov
percento_bankrotov
0
50
10
1
1
NIL
HORIZONTAL

SLIDER
11
66
191
99
pocet_susedov
pocet_susedov
3
30
25
1
1
NIL
HORIZONTAL

TEXTBOX
15
327
165
345
Dynamic settings
11
0.0
1

PLOT
721
10
1011
130
Distribution: age
Age
Agents
1.0
50.0
0.0
10.0
false
true
"set-plot-x-range 1 max_vek\nset-plot-y-range 0 round ( count salesmans / 4 ) + 1\n\nset-current-plot-pen \"Age\"\nset-histogram-num-bars 20" ""
PENS
"Age" 1.0 1 -5298144 true "" "histogram [fen_age ] of salesmans"

PLOT
721
134
1012
254
Distribution: Number od towns in paths
Number of Towns in Path
Agents
1.0
10.0
0.0
10.0
false
true
"set-plot-x-range 1 pocet_miest\nset-plot-y-range 0 round ( count salesmans / 5 ) + 1\n\nset-current-plot-pen \"Count\"\nset-histogram-num-bars pocet_miest" ""
PENS
"Count" 1.0 1 -16777216 true "" "histogram [fen_T_act ] of salesmans"

PLOT
1015
10
1311
130
Distribution: Quality of Paths
Quality of Path
Agents
0.0
1.0
0.0
10.0
true
true
";set-plot-x-range 1 pocet_susedov\nset-plot-y-range 0 round ( count salesmans / 5 ) + 1\n\nset-current-plot-pen \"Quality\"\nset-histogram-num-bars 40" ""
PENS
"Quality" 1.0 1 -16777216 true "" "histogram [ 1 / fen_Q_avg ] of salesmans"

PLOT
722
258
1311
378
Distribution: Value of Paths
 Value of Path
Agents
0.0
10.0
0.0
10.0
false
true
"set-plot-x-range 0 round ( pocet_miest / 4 )\nset-plot-y-range 0 round ( count salesmans / 2 )\n\nset-current-plot-pen \"Value\"\nset-histogram-num-bars 50" ""
PENS
"Value" 1.0 1 -16777216 true "" "histogram [fen_value ] of salesmans"

CHOOSER
11
182
191
227
Zobraz
Zobraz
"Joins" "Best value" "Max towns path" "Top 5 winners" "Shortest path"
4

PLOT
1015
133
1311
253
Distribution: Joined Towns in the Zone of Path
Joined Towns in the Zone [%]
Agents
0.0
100.0
0.0
10.0
false
true
"set-plot-x-range 1 100\nset-plot-y-range 0 round ( count salesmans / 5 ) + 1\n\nset-current-plot-pen \"Potencial\"\nset-histogram-num-bars 100" ""
PENS
"Potencial" 1.0 1 -16777216 true "" "histogram [fen_T_act / fen_T_pot * 100 ] of salesmans"

MONITOR
14
429
71
474
NIL
n_opt
0
1
11

MONITOR
75
429
128
474
NIL
n_exp
0
1
11

MONITOR
130
429
187
474
NIL
n_org
0
1
11

MONITOR
14
480
71
525
Winners
n_win
0
1
11

MONITOR
75
480
164
525
Shortest path
SM_len
0
1
11

BUTTON
13
282
192
315
Brute Force
brute_force
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
12
243
67
276
Reset
Reset
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

@#$#@#$#@
## WHAT IS IT?
Simulácia podvádzania pri morálnej dileme v prostredí školských testov.
Model obsahuje populáciu študentov a učiteľov.
Cieľom je odhadnúť vplyv premenných parametrov na mieru podvádzania
a určiť stratégiu na minimalizáciu podvádzania. 

Model berie do úvahy:

vrodený  stav študenta (strach, pokušenie, pracovitosť)
pkamžitý stav študenta (strach, pokušenie, pracovitosť)

počet študentov
frekvenciu testovania celej populácie študentov

pravdepodobnosť odhalenia podvodu
mieru trestu pri odhalení
mieru zisku pri neodhalenom podvode

vplyv trestu/zisku z podvodu na okamžitý stav študenta
mieru prenosu stavu medzi študentami po prekonaní prahu prenosu
zabúdanie úprav stavu s postupným návratom do vrodeného stavu

## HOW IT WORKS
Opakuj hlavnú slučku simulácie

> Vyrieš priebeh testovania

  1. Ak nastal krok s testovanim 

    * Ak má študent strach >= pokušenie, potom nepodvádza

    * Ak má študent strach <  pokušenie, potom podvádza
        * Ak študenta chytili zvýš strach podľa miery trestu
        * inak zníž strach podľa miery zisku a zvýš pokušenie podľa miery zisku

> Vyrieš sociálne interakcie

  1. vsetci studenti, ktorych nechytili, zvysia svojim susedom pokusenie podla miery zisku a socialnej interakcie;

  2. vsetci studenti, ktorych prichytili, zvysia svojim susedom strach podla miery trestu a socialnej interakcie

  3. vsetci studenti odovzdaju svojim susedom svoj stav podla miery socialnej interakcie

> Vyrieš zabudanie 

3. memo

> Vyrieš fyzicky pohyb študentov a učiteľov 

4. move

> Vyrieš vyfarbenie diagramu 

5. color

Koniec hlavnej slučky

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

Všetky lokálne premenné sú bezrozmerné intenzity v škále od 0 po 100.

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
NetLogo 5.0.4
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 1.0 0.0
0.0 1 1.0 0.0
0.2 0 1.0 0.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

@#$#@#$#@
0
@#$#@#$#@
