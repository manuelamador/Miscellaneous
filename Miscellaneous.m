(* Mathematica Package *)

BeginPackage["Miscellaneous`"]

importCodeFromSE::usage="
Code extractor using the StackExchange API. Link:
http://mathematica.stackexchange.com/a/3537/209. The following code uses the 2.0 \
version of the SE API and has also been cleaned \
up a bit (place it in your kernel's init.m or your custom functions package if \ 
you'd like to be able to use it anytime). The function takes a single string \ 
argument, which is the URL obtained from the 'share' link under a question/answer."


monitorParallelTable::usage=
"monitorParallelTable[expr_, iter__List, updatethreshold_] runs ParallelTable[expr, iter] while \
monitoring a counter that increases each time a kernel runs for updatethreshold cycles. "


listPlotSelectedLabels::usage=
"listPlotSelectedLabels[data_, A_, options_, labelPosition_: Automatic] makes a scatter plot \
with only a subset of labels selected. `A` determines the density of the labels, and \
labelPosition, the position of the labels. This function allows for ex-post customization \
of the plot by clicking in the points in the graph". 


Begin["`Private`"] (* Begin Private Context *) 

ClearAll[importCodeFromSE];
importCodeFromSE[url_String] := 
        With[{filterCode = 
                StringCases[#, ("<pre><code>" ~~ ("\n" ...) ~~ x__ ~~ ("\n" ...) ~~
                        "</code></pre>") /; 
                        StringFreeQ[x, "<pre><code>" | "</code></pre>"] :> x] &, 
                convertEntities = 
                        StringReplace[#, {"&gt;" -> ">", "&lt;" -> "<", 
                                "&amp;" -> "&"}] &, 
                makeCodeCell = 
                        Scan[CellPrint@Cell[Defer@#, "Input", CellTags -> "Ignore"] &, 
                                Flatten@{#}] &, 
                postInfo = 
                        Import[ToString@
                                StringForm["http://api.stackexchange.com/2.1/posts/`1`?site=`2`&"<>
                                        "filter=!9hnGsretg", #3, #1] & @@ {First@
                                    StringCases[#, Shortest[s__] ~~ "." ~~ ___ :> s], #2, #3} & @@ 
                        StringSplit[StringDrop[url, 7], "/"][[;; 3]], "JSON"]
                }, 
                OptionValue["items" /. postInfo, "body"] // filterCode // 
                        convertEntities // makeCodeCell
        ];


ClearAll[monitorParallelTable];
SetAttributes[monitorParallelTable, HoldAll];
monitorParallelTable[expr_, iter__List, updatethreshold_]:=
    Module[{counter=1, thresh=updatethreshold},
        SetSharedVariable[counter];
        ParallelEvaluate[localcounter=1;];
        Monitor[
            ParallelTable[
                If[localcounter>=thresh, 
                    counter=counter+localcounter; 
                    localcounter=1,
                    localcounter++];
                expr,
            iter],
        counter]
    ];


(* prepares a function for a sequential algorithm *)
ClearAll[addLabelOrNot];
addLabelOrNot[stack_List, point_, dist_, scaling_] := 
    If[
        Min[ChessboardDistance[
            Dot[point[[1 ;; 2]], scaling],
            Dot[#, scaling]] & /@ stack[[All, 1]]] < dist,
        Append[stack, {point[[1 ;; 2]], point[[3]],  False}],
        Append[stack, {point[[1 ;; 2]], point[[3]], True}]
    ]

(* gets the scaling right *)
ClearAll[scaling];
scaling[data_] := {
    {1/6 (Max[#] - Min[#])^(-1) &@data[[All, 1]], 0},
    {0,  (Max[#] - Min[#])^(-1) &@data[[All, 2]]}
}

(* Plots the data using a sequential algorithm to select the labels *)
(* Allows you to interactively change the graph as user clicks on country data points *)
listPlotSelectedLabels[data_, A_, options_, labelPosition_: Automatic]:= 
    With[{scaling = scaling[data]}, 
        DynamicModule[
            {dataToPlot = Fold[
                (addLabelOrNot[#1, #2, A, scaling] &), 
                    {{data[[1, 1 ;; 2]], data[[1, 3]], True}}, 
                    Rest@data]
            },
            Dynamic@Show@Table[
                ListPlot[x[[3]][#[[1]], #[[2]]] & /@ x[[1]], 
                    Frame -> True, 
                    x[[2]]],
                {x, {
                    {
                        dataToPlot, {
                            PlotStyle -> Gray,
                            options, 
                            PlotMarkers -> Automatic
                        },
                        #1 &
                    }, 
                    {
                        Cases[dataToPlot, {_, _, True}],{
                            PlotStyle ->  Darker[Gray], 
                            PlotMarkers -> Automatic
                        }, 
                        Labeled[#1, Style[#2, Darker[Gray]], labelPosition] &
                    },
                    {
                        dataToPlot, {
                            PlotStyle -> Opacity[0], 
                            PlotMarkers -> Automatic
                        }, 
                        EventHandler[
                            Tooltip[#1, #2],  
                            "MouseClicked" :> (dataToPlot = dataToPlot /.
                                {#1, #2, state_} :> {#1, #2, Not[state]})] &
                    }} /. {{}, _, _} :> Sequence[]
                }
            ]
            (*// (Join[#, {RegionPlot[ChessboardDistance[
                                Dot[{Log[1/16],.25},scaling], 
                                Dot[{x,y},scaling]
                            ]<A,{x, Log[1/128], Log[1]}, {y, 0.1, 0.4}]}
                    ]&)*)
        ]
    ]
    
End[] (* End Private Context *)

EndPackage[]