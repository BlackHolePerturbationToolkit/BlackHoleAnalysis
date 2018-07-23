(* Mathematica Test File *)


Test[
	Module[{oddFE, oddFEcomb, Psisym, hrsym, psiFromMP, dthr, oddFEcomb1, t, r, M,la},
 		t = TSymbol[];
 		r = RSymbol[];
 		M = BlackHoleMassSymbol[];
 		la = LambdaSymbol[];
 		Psisym = MasterFunction[Parity -> "Odd", ReturnSymbol -> True];
 		hrsym = HrAmplitude[ReturnSymbol -> True, Gauge -> "RWZ"];
 		oddFE = FieldEquations[Parity -> "Odd", SourceExpansion -> "Full", Gauge -> "RWZ"];
 		oddFEcomb = (r f[r, M])/la (1/f[r, M] D[oddFE[[2]], t] + f[r, M] D[oddFE[[1]], r] + (2 M)/r^2 oddFE[[1]]);
 		psiFromMP = MasterFunction[Parity -> "Odd", Gauge -> "RWZ", MPs -> True];
 		dthr[tt_, rr_] := Solve[psiFromMP == Psisym[tt, rr], Derivative[1, 0][hrsym][tt, rr]][[1, 1, 2]];
 		oddFEcomb1 = oddFEcomb /. (Derivative[n_, m_][hrsym][t, r] :> D[dthr[t, r], {t, n - 1}, {r, m}]);
 		EvaluateDeltas[Collect[oddFEcomb1, {Derivative[__][_][t, r], _[t, r]}, Simplify]]
 	],

 
	MasterEquation[Parity -> "Odd", SourceExpansion -> "Full"],
 
 	TestID->"MasterEqCPMDerive",
 	
 	EquivalenceFunction -> ((Simplify[#1-#2] == 0) &)
	
]


Test[
	
	Module[{evenFE, evenFEcomb, Psisym, hrrsym, psiFromMP, evenFEcomb1, t,r, M, la, sol, hrrSol},
 		t = TSymbol[];
 		r = RSymbol[];
 		M = BlackHoleMassSymbol[];
 		la = LambdaSymbol[];
 		Psisym = MasterFunction[Parity -> "Even", ReturnSymbol -> True];
 		hrrsym = HrrAmplitude[ReturnSymbol -> True, Gauge -> "RWZ"];
 		evenFE =  FieldEquations[Parity -> "Even", SourceExpansion -> "Full", Gauge -> "RWZ"];
 		evenFEcomb = EvaluateDeltas[
 	  					1/((la + 1) CapitalLambda[r, M, la]) (r^2 f[r,M] (f[r, M]^2 D[evenFE[[1]], r] - D[evenFE[[3]], r]) + 
 	      				r (CapitalLambda[r, M, la] - f[r, M]) evenFE[[3]] +  r f[r, M]^2 evenFE[[6]] 
			 	      	- f[r, M]^2/(r CapitalLambda[r, M, la]) (la (la - 1) r^2 + (4 la - 9) M r + 15 M^2) evenFE[[1]]) 
 	      				+ (2 f[r, M])/CapitalLambda[r, M, la] evenFE[[5]] - f[r, M]/r evenFE[[7]]];
		psiFromMP = MasterFunction[Parity -> "Even", Gauge -> "RWZ", MPs -> True];
 		sol = First@Solve[psiFromMP == Psisym[t, r], hrrsym[t, r]];
 		hrrSol[tt_, rr_] := sol[[1, 2]] /. {t -> tt, r -> rr};
 		evenFEcomb1 = evenFEcomb /. {hrrsym[t, r] -> hrrSol[t, r], Derivative[n_, m_][hrrsym][t, r] :>  D[hrrSol[t, r], {t, n}, {r, m}]};
 		EvaluateDeltas[Collect[evenFEcomb1, {Derivative[__][_][t, r], _[t, r]}, Simplify]]
 	],

 
	MasterEquation[Parity -> "Even", SourceExpansion -> "Full"],
 
 	TestID->"MasterEqZMDerive",
 	
	EquivalenceFunction -> ((Simplify[#1-#2] == 0) &)
]

Test[
	
	Module[{zerVar1, EE, rp, mu, dd, evenFEY, KSym, YshBar, t, r, M, la},
 
		r = RSymbol[];
 		t = TSymbol[];
 		EE = SpecificEnergySymbol[];
 		mu = ParticleMassSymbol[];
		YshBar = YSymbol[Conjugate -> True];
 		r = RSymbol[];
 		rp = RpSymbol[];
 		M = BlackHoleMassSymbol[];
 		la = LambdaSymbol[];
 		dd = DiracDeltaSymbol[];
 		evenFEY = FieldEquations[Parity -> "Even", Gauge -> "RWZ", Homogeneous -> False, SourceExpansion -> "Full"];
 
		KSym = KAmplitude[Gauge -> "RWZ", ReturnSymbol -> True];
 		zerVar1 = D[MasterFunction[Variable -> "ZM", MPs -> True], t] + (8 EE mu \[Pi] rp[t] YshBar[t] rp'[t])/((1 + la) (3 M + la rp[t])) dd[r - rp[t]];
 
 		Collect[EvaluateDeltas[zerVar1 /. Solve[evenFEY[[2]] == 0, D[KSym[t, r], t, r]][[1, 1]]], _[t, r], Simplify]
 	],

 
	MasterFunction[Variable -> "ZM1", MPs -> True],
 
 	TestID->"ZerilliFromZM",
 	
	EquivalenceFunction -> ((Simplify[#1-#2] == 0) &)
]

Test[
	Module[{hrrSym, htrSym, httSym, rp, evenFEY, KSym, t, r,dd},
 
 		hrrSym = HrrAmplitude[Gauge -> "RWZ", ReturnSymbol -> True];
 		httSym = HttAmplitude[Gauge -> "RWZ", ReturnSymbol -> True];
 		htrSym = HtrAmplitude[Gauge -> "RWZ", ReturnSymbol -> True];
 		KSym = KAmplitude[Gauge -> "RWZ", ReturnSymbol -> True];
 		t = TSymbol[];
 		r = RSymbol[];
 		rp = RpSymbol[];
 		dd=DiracDeltaSymbol[];
 		evenFEY = FieldEquations[Parity -> "Even", Gauge -> "RWZ", Homogeneous -> False, SourceExpansion -> "Full"];
 
		Collect[RemoveRpDots[
				Collect[EvaluateDeltas[
					MasterEquation[Homogeneous -> False, Variable -> "ZM1", MPs -> True, SourceExpansion -> "Full"] /.
					Solve[D[evenFEY[[3]], t] == 0, D[KSym[t, r], t, t, t]][[1,1]] /.
					Solve[D[evenFEY[[1]], t] == 0, D[KSym[t, r], t, r, r]][[1,1]] /.
					Solve[D[evenFEY[[4]], r] == 0, D[htrSym[t, r], r, r]][[1,1]] /.
					Solve[D[evenFEY[[5]], t] == 0, D[htrSym[t, r], t, t]][[1,1]] /.
					Solve[evenFEY[[2]] == 0, D[KSym[t, r], r, t]][[1, 1]] /.
					Solve[evenFEY[[4]] == 0, D[KSym[t, r], t]][[1, 1]] /.
					Solve[D[evenFEY[[7]], t] == 0, D[httSym[t, r], t]][[1, 1]]], 
					Derivative[_][rp][t]]], 
						{_[t, r], dd[_], Derivative[_][dd][_], Derivative[_][rp][t]}, Simplify]
 	],

 
	0,
 
 	TestID->"ZerilliFromMPsCheck",
 	
	EquivalenceFunction -> ((Simplify[#1-#2] == 0) &)
]


Test[
	Module[{hrrSym, httSym, rp, evenFEY, KSym, t, r, dd},
		hrrSym = HrrAmplitude[Gauge -> "RWZ", ReturnSymbol -> True];
		httSym = HttAmplitude[Gauge -> "RWZ", ReturnSymbol -> True];
		KSym = KAmplitude[Gauge -> "RWZ", ReturnSymbol -> True];
		t = TSymbol[];
		r = RSymbol[];
		rp = RpSymbol[];
		dd = DiracDeltaSymbol[];
		evenFEY = FieldEquations[Parity -> "Even", Gauge -> "RWZ", Homogeneous -> False, SourceExpansion -> "Full"];

		Collect[RemoveRpDots[Collect[EvaluateDeltas[
				MasterEquation[Homogeneous -> False, Variable -> "ZM2", MPs -> True, SourceExpansion -> "Full"] /. 
    	             Solve[D[evenFEY[[1]], r] == 0, D[KSym[t, r], r, r, r]][[1,1]] /.                
        	        Solve[evenFEY[[6]] == 0, D[KSym[t, r], r, r]][[1,1]] /.               
            	   Solve[D[evenFEY[[3]], r] == 0, D[KSym[t, r], t, t, r]][[1,1]] /.          
      	        Solve[D[evenFEY[[7]], r, r] == 0, D[hrrSym[t, r], r, r]][[1,1]] /.             
        	     Solve[D[evenFEY[[7]], t, t] == 0, D[httSym[t, r], t, t]][[1,1]] /.
            	Solve[D[evenFEY[[4]], t] == 0, D[hrrSym[t, r], t, t]][[1,1]] /.
	           Solve[D[evenFEY[[5]], r] == 0, D[httSym[t, r], r, r]][[1,1]] /.          
    	      Solve[evenFEY[[1]] == 0, D[KSym[t, r], r, r]][[1,1]] /.   
        	 Solve[evenFEY[[3]] == 0, D[KSym[t, r], t, t]][[1,1]] /.        
	        Solve[evenFEY[[5]] == 0, D[KSym[t, r], r]][[1,1]] /.       
	       Solve[D[evenFEY[[7]], r] == 0, D[httSym[t, r], r]][[1,1]] /. 
	      Solve[evenFEY[[7]] == 0, httSym[t, r]][[1, 1]]], 
 		Derivative[_][rp][t]]], {_[t, r], dd[_], Derivative[_][dd][_], Derivative[_][rp][t]}, Simplify]
 		],
 
	0,
 
 	TestID->"ZDotFromMPsCheck",
 	
	EquivalenceFunction -> ((Simplify[#1-#2] == 0) &)
]

Test[
	Module[{oddFE, RWComb, Psisym, hrsym, psiFromMP, RWComb1, t, r, M, la, sol, hrSol},
 		t = TSymbol[];
 		r = RSymbol[];
 		M = BlackHoleMassSymbol[];
 		la = LambdaSymbol[];
 		Psisym = MasterFunction[Variable -> "CPM1", ReturnSymbol -> True];
 		hrsym = HrAmplitude[ReturnSymbol -> True, Gauge -> "RWZ"];
 		oddFE = FieldEquations[Parity -> "Odd", SourceExpansion -> "Full", Gauge -> "RWZ"];
 		RWComb = 2(f[r, M]/r (-oddFE[[2]] + f[r, M] D[oddFE[[3]], r] - 2/r (1 - (3 M)/r) oddFE[[3]]));
 		psiFromMP =  MasterFunction[Variable -> "CPM1", Gauge -> "RWZ", MPs -> True];
 		sol = First@Solve[psiFromMP == Psisym[t, r], hrsym[t, r]];
 		hrSol[tt_, rr_] := sol[[1, 2]] /. {t -> tt, r -> rr};
 		RWComb1 = EvaluateDeltas[RWComb /. {hrsym[t, r] -> hrSol[t, r], Derivative[n_,m_][hrsym][t, r] :> D[hrSol[t, r], {t, n}, {r, m}]}];
 		EvaluateDeltas[Collect[RWComb1, {Derivative[__][_][t, r], _[t, r]}, Simplify]]
 	],
 
	MasterEquation[Variable -> "CPM1", SourceExpansion -> "Full"],
 
 	TestID->"MasterEqRWDerive",
 	
 	EquivalenceFunction -> ((Simplify[#1-#2] == 0) &)
]

Test[
	Module[{eqs, eqsx},
 	eqs = FieldEquations[Parity -> "Both", Gauge -> "RWZ",SourceExpansion->"None"];
 	eqsx = RToRStar[eqs];
 	RStarToR[eqsx] - eqs // Simplify
 	],
 
	{0,0,0,0,0,0,0,0,0,0},
 
 	TestID->"RStarRWZ"
	
]

Test[
	Module[{eqs, eqsx},
 	eqs = FieldEquations[Parity -> "Both", Gauge -> "Lorenz",SourceExpansion->"None"];
 	eqsx = RToRStar[eqs];
 	RStarToR[eqsx] - eqs // Simplify
 	],
 
	{0,0,0,0,0,0,0,0,0,0},
 
 	TestID->"RStarLorenz"
	
]

Test[
	Module[{eqs, eqsx},
 	eqs = FieldEquations[Parity -> "Both", Gauge -> "Lorenz",SourceExpansion->"Full"];
 	eqsx = RToRStar[eqs];
 	RStarToR[eqsx] - eqs // Simplify
 	],
 
	{0,0,0,0,0,0,0,0,0,0},
 
 	TestID->"RStarLorenzSE"
	
]

Test[
	masterEqEven = ToFrequencyDomain[MasterEquation[Homogeneous -> True]];
	psiE = EvenMasterFunctionAsOdd[];
	psiESym = MasterFunctionSymbol[];
	masterEqOdd = masterEqEven /. {psiESym[r] -> psiE, Derivative[n_][psiESym][r] :> D[psiE, {r, n}]};
	RemoveMasterFunctionRDerivatives[masterEqOdd, Parity -> "Odd", FD -> True, Homogeneous -> True] // Simplify,
	0,
	TestID->"EvenMasterFunctionToOdd"
]

Close[stdStream]
Close[msgStream]
