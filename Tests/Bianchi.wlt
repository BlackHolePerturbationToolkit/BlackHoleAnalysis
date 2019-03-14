(* Mathematica Test File *)


VerificationTest[
	bi=SourceConservation[Parity -> "Both", SourceExpansion -> "Full", Indices -> "Up"];
 	biEval = RemoveSHDots[#] & /@ bi;
 	biEval1 = EvaluateDeltas[#] & /@ biEval;
 	RemoveRpDots[#] & /@ biEval1,
 
	 {0,0,0,0},
 
 	TestID->"BianchiFullUp",
 	
 	SameTest -> (If[MatchQ[Head[#1],List],Simplify[#1-#2]==Table[0,{Length@#1}],Simplify[#1-#2]== 0]&)
	
]

VerificationTest[
	bi=SourceConservation[Parity -> "Both", SourceExpansion -> "Full", Indices -> "Down"];
 	biEval = RemoveSHDots[#] & /@ bi;
 	biEval1 = EvaluateDeltas[#] & /@ biEval;
 	RemoveRpDots[#] & /@ biEval1,
 
	 {0,0,0,0},
 
 	TestID->"BianchiFullDown",
 	
 	SameTest -> (If[MatchQ[Head[#1],List],Simplify[#1-#2]==Table[0,{Length@#1}],Simplify[#1-#2]== 0]&)
]
