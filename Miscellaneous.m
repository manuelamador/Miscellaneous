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
monitoring a counter that increases each time a kernel runs for updatethreshold cycles."


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

End[] (* End Private Context *)

EndPackage[]