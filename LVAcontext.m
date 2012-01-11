(*   -*-mode: Mathematica; mode: outline-minor-*-   *)

BeginPackage["LVAcontext`"];

setPrivate::usage = "setPrivate[] sets up private context and evaluates Needs[\"LVAcontext`\"];
  setPrivate[\"foo`\"] or setPrivate[{\"foo`\",\"bar`\"}] expose contexts;
  setPrivate[42] exposes context \"Notebook$$42`\""

setAutosave::usage = "setAutosave[True|False] just sets SetOptions[SelectedNotebook[],NotebookAutoSave->True|False]"
spawnScratch::usage = "spawnScratch[] opens a new notebook and issues setPrivate[\"CurrentContext`\"]"
ExposeContexts::usage = "ExposeContexts[{\"Global`\",\"MyContext`\"}
       Exposed contexts are shadowed by the previously defined contexts.
       Options: SaveNotebookContext->True - save currenct $ContextPath minus any contexts
       matching $Notebook$xx` and System into the notebook's metadata;
       RestoreFromSaved->True - restored this"
ExposeContext::usage = "See ExposeContexts"

(**************  private context ******************)
Begin["`Private`"];

setPrivate[exposedCtxts : ({__String} | Null) : Null]:=
  SelfDestruct@Module[{nb,contBox,currContStrList,lvaBox,expCtxtBox,defaultExpCtxtBox,assembledStatements,strList},
    nb = InputNotebook[];
    currContStrList = Riffle[("\"" <> # <> "\"") & /@ (ToString/@$ContextPath), ","];
    defaultExpCtxtBox = RowBox[{"ExposeContexts", "[", RowBox[{"{", RowBox[currContStrList], "}"}], "]"}];
      (* But we really don't want to use the default exposed context without hiding Global`
         so we won't do anything with it for now. It might be useful in the future 
         to explicitly delete Global` from the string and use this *)
    contBox = MakeBoxes[SetOptions[EvaluationNotebook[], CellContext -> Notebook]];
    lvaBox = MakeBoxes[Needs["LVAcontext`"]];

    writeAndEval[nb,contBox];
    writeAndEval[nb,lvaBox];
    
    If[exposedCtxts =!= Null,
       strList = Riffle[("\"" <> # <> "\"") & /@ exposedCtxts, ","];
       expCtxtBox = RowBox[{"ExposeContexts", "[", RowBox[{"{", RowBox[strList], "}"}], "]"}];
       writeAndEval[InputNotebook[],expCtxtBox];
      ];
]

setPrivate[x_String]:=setPrivate[{x}];
setPrivate[x_Integer]:= setPrivate["Notebook$$" <> ToString[x] <> "`"];


SetAttributes[SelfDestruct, HoldAllComplete];
SelfDestruct[e_] := (If[$FrontEnd =!= $Failed,
   SelectionMove[EvaluationNotebook[], All, EvaluationCell]; 
   NotebookDelete[]]; e)


writeAndEval[nb_,boxExpr_]:=(
    NotebookWrite[nb,  CellGroupData[{Cell[BoxData[boxExpr],"Input"]}]];
    SelectionMove[nb, Previous, Cell]; 
    SelectionMove[nb, Next, Cell];
    SelectionEvaluate[nb];
)
    (* the version before SelfDestruct is in commit c6ccb4cbd4828bec6edb5dac15622e5ee6dd4211 *)
spawnScratch[] := SelfDestruct@Module[{nb,boxExpr,strList},
    strList = Riffle[("\"" <> # <> "\"") & /@ $ContextPath, ","];
    boxExpr = RowBox[{"setPrivate", "[",
        RowBox[{"{", RowBox[strList], "}"}], "]"}];
    nb = CreateDocument[];
    writeAndEval[nb,boxExpr];
]


setAutosave[] := setAutosave[True];
setAutosave[True] := SetOptions[EvaluationNotebook[],NotebookAutoSave->True];
setAutosave[False] := SetOptions[EvaluationNotebook[],NotebookAutoSave->False];


(* TODO: make sure context path order is sensible (e.g. wrt system context) when adding
  new elements to the path *)
ExposeContexts[list___,OptionsPattern[{UseSaved->False,SaveNotebookContext->False,RestoreSaved->False,Prepend->False}]]:=
    Module[{ctList, args, savedCt},
	   ctList = Flatten@List@list; args = ctList;
	   If[OptionValue[UseSaved] || ToLowerCase@ToString@OptionValue[UseSaved] === "existing", 
	      (* context list must be zero and old context must be saved *)
	      If[Length[ctList]==0 && Length[oldct]>0,  (* check the above *)
		 ctList = oldct,     
		 If[Length[args] > 0,
		    Message[ExposeContexts::badargs2],Message[ExposeContexts::badargs3]];
		];
	     ];
	   If[! MemberQ[ctList,Except[_String]]
           , If[OptionValue[Prepend]
                 , PrependTo[$ContextPath,#]&/@ctList;
                 , AppendTo[$ContextPath,#]&/@ctList];
           , Message[ExposeContexts::badargs];];
           $ContextPath = DeleteDuplicates[$ContextPath];
	   If[OptionValue[SaveNotebookContext],	      
	      savedCt = DeleteCases[$ContextPath, (* lose System/Notebook private strings *)
		       x_ /; StringMatchQ[x, {"Notebook$$" ~~ __?DigitQ ~~ "`", "System`"}]];
	      SetOptions[EvaluationNotebook[],LVANotebookOptions->{ContextPath->savedCt}]];
	   If[OptionValue[RestoreSaved],	      (* this is all sketchy and doesn't work well *)
	      AppendTo[$ContextPath,#]&/@(LVAutils`Private`ContextPath /. (LVAutils`Private`LVANotebookOptions /. Options[EvaluationNotebook[]]));
	      $ContextPath = DeleteDuplicates[$ContextPath];
	     ];
	   
	   $ContextPath
 ]
ExposeContexts::badargs = "Exposed contexts should be given as a list of strings."
ExposeContexts::badargs2 = "Use either a list or an option"
ExposeContexts::badargs3 = "UseSaved takes values from previous calls of SetPrivate[]; these values are missing."
ExposeContexts[x_Integer]:=ExposeContexts["Notebook$$" <> ToString[x] <> "`"];
ExposeContext = ExposeContexts;




End[];
EndPackage[];