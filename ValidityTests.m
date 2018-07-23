(* ::Package:: *)

BeginPackage["BlackHoleAnalysis`ValidityTests`",
				{"BlackHoleAnalysis`Utils`","BlackHoleAnalysis`OverloadedSymbols`"}];


(*SymbolQ;
BooleanQ;
RealPosQ;
RealNatQ;
*)
(*ValidDTQ;*)

VerifyNonRadiativeL;
VerifyNonRadiativeM;
VerifyNonRadiativeLM;
VerifyRadiativeL;
VerifyRadiativeLM;
VerifyL;
VerifyM;
VerifyN;
VerifyLM;
VerifyNRange;
VerifyMRange
VerifyLRange;
VerifyRange;
VerifySameListElements;
VerifyBlackHoleSpin::usage="VerifyBlackHoleSpin[syms,{a,M}] aborts with an error message if the spin of the black hole a \
is not between -M and M.";
VerifyOrbit::usage="VerifyOrbit[syms,{p,e,M}] aborts with an error message if an orbit parametrized by semilatus rectum p, eccentricity e, and mass M
is invalid.\n\
VerifyOrbit[syms,{r,M}] aborts with an error message if a circular orbit parametrized by radius r and mass M
is invalid.";



Begin["`Private`"];


(*def@
SymbolQ[x_]:=MatchQ[x,_Symbol|_Symbol[___]|_Symbol[___][___]|_Symbol[___][___][___]|_Symbol[___][___][___][___]]*)


(*def@
BooleanQ[x_]:=(x === True || x === False);*)


(*def@
RealQ[num_]:=NumericQ[num]&&Element[num,Reals]*)


(*def@
RealPosQ[num_]:=NumericQ[num]&& Element[num,Reals]&&num>0*)


(*def@
RealNatQ[num_]:=NumericQ[num]&& Element[num,Reals]&&(num>=0)*)


(*ValidDTQ=SchwAnalysis`DataTable`Private`validQ*)


testDef@
VerifyNRange[syms_Association,{nMin_,nMax_}]:=
Module[{},
	VerifyN[syms,nMin];
	VerifyN[syms,nMax];
	VerifyRange[syms,{nMin,nMax}]
]


testDef@
VerifyLRange[syms_Association,{lMin_,lMax_}]:=
Module[{},
	VerifyL[syms,lMin];
	VerifyL[syms,lMax];
	VerifyRange[syms,{lMin,lMax}];
]


testDef@
VerifyMRange[syms_Association,{lN_,{mMin_,mMax_}}]:=
Module[{},
	VerifyLM[syms,{lN,mMin}];
	VerifyLM[syms,{lN,mMax}];
	VerifyRange[syms,{mMin,mMax}];
]


testDef@
VerifyRange[syms_Association,{x1_?RealQ,x2_?RealQ}]:=
If[x1>x2,
	OutputMessage[syms,"Invalid range. "<>ToString[x1]<>" should not be greater than "<>ToString[x2]<>".",1,Style->"Error"];
	Aborting[syms]
];


testDef@
VerifyL[syms_Association,lN_]:=
Module[{},
	If[!MatchQ[lN,_Integer],OutputMessage[syms,"Invalid l = "<>ToString[lN]<>" is not an integer.",1,Style->"Error"];Aborting[syms]];
	If[lN<0,OutputMessage[syms,"Invalid l = "<>ToString[lN]<>" is less than 0",1,Style->"Error"]; Aborting[syms]];
]


testDef@
VerifyM[syms_Association,mN_]:=If[!MatchQ[mN,_Integer],OutputMessage[syms,"Invalid m = "<>ToString[mN]<>" is not an integer.",1,Style->"Error"];Aborting[syms]];


testDef@
VerifyN[syms_Association,nN_]:=If[!MatchQ[nN,_Integer],OutputMessage[syms,"Invalid n = "<>ToString[nN]<>" is not an integer.",1,Style->"Error"];Aborting[syms]];


testDef@
VerifyLM[syms_Association,{lN_,mN_}]:=
Module[{},
	VerifyL[syms,lN];
	VerifyM[syms,mN];
	If[Abs[mN]>lN,OutputMessage[syms,"Invalid {l, m} combination "<>ToString[{lN, mN}]<>". m not between -l and l.",1,Style->"Error"]; Aborting[syms]]
]


testDef@
VerifyRadiativeL[syms_Association,lN_]:=
Module[{},
	VerifyL[syms,lN];
	If[lN<2,OutputMessage[syms,"Invalid radiative l = "<>ToString[lN]<>" is less than 2.",1,Style->"Error"]; Aborting[syms]]
]


testDef@
VerifyRadiativeLM[syms_Association,{lN_,mN_}]:=
Module[{},
	VerifyRadiativeL[syms,lN];
	VerifyM[syms,mN];
	VerifyLM[syms,{lN,mN}];
]


testDef@
VerifyNonRadiativeL[syms_Association,lN_]:=
Module[{},
	VerifyL[syms,lN];
	If[lN>=2,OutputMessage[syms,"Invalid non-radiative l = "<>ToString[lN]<>" is greater than 1.",1,Style->"Error"]; Aborting[syms]]
]


testDef@
VerifyNonRadiativeM[syms_Association,mN_]:=
Module[{},
	VerifyM[syms,mN];
	If[Abs[mN]>=2,OutputMessage[syms,"Invalid non-radiative m = "<>ToString[mN]<>" is outside the range -1 - 1.",1,Style->"Error"]; Aborting[syms]]
]


testDef@
VerifyNonRadiativeLM[syms_Association,{lN_,mN_}]:=
Module[{},
	VerifyNonRadiativeL[syms,lN];
	VerifyNonRadiativeM[syms,mN];
	VerifyLM[syms,{lN,mN}];
]


testDef@
VerifySameListElements[syms_Association,{l1_List,l2_List}]:=
If[Union[l1]=!=Union[l2],
	OutputMessage[syms,"Lists "<>ToString[l1]<> " and "<>ToString[l2]<>" have different elements.",1,Style->"Error"];
	Aborting[syms]
]


testDef@
VerifyBlackHoleSpin[syms_Association,{aN_,MN_}]:=
Module[{},
	If[!NumericQ[aN],OutputMessage[syms,"Spin parameter a must be numeric",1,Style->"Error"]; Aborting[syms]];
	If[!NumericQ[MN],OutputMessage[syms,"Mass parameter M must be numeric",1,Style->"Error"];Aborting[syms]];
	If[Abs[aN]>MN,
		OutputMessage[syms,"a must be between -M and M",1,Style->"Error"];
		OutputMessage[syms,"Here a = "<> ToString[aN]<> " and M = "<> ToString[MN]<>"",1,Style->"Error"];
		Aborting[syms]
	];
]


testDef@
VerifyOrbit[syms_Association,{pN_?RealQ,eN_?RealQ,aN_?RealQ,MN_?RealQ}]:=
Module[{a,MM,p,e},
	a = BlackHoleSpinSymbol[syms];
	p = SemiLatusRectumSymbol[syms];
	e = OrbitalEccentricitySymbol[syms];
	MM = BlackHoleMassSymbol[syms];
	If[MN<=0,OutputMessage[syms,"Black hole must have positive mass. Here M = "<>ToString[MN]<>".",1,Style->"Error"];Aborting[syms]];
	If[eN>=1||eN<0,OutputMessage[syms,"Eccentrcity must be in range 0<=e<1. Here e = "<>ToString[eN]<>".",1,Style->"Error"];Aborting[syms]];
	If[aN>MN||aN<-MN,OutputMessage[syms,"Black hole spin must be in range -M<=a<=M. Here a = "<>ToString[aN]<>".",1,Style->"Error"];Aborting[syms]];
	If[Separatrix["Metric"->"Kerr"]/.{a->aN,MM->MN,p->pN,e->eN},
		OutputMessage[syms,"{p, e, a, M} = " <> ToString[{pN,eN, aN, MN}] <> " specify an orbit beyond the separatrix.",1,Style->"Error"];Aborting[syms]];
]


testDef@
VerifyOrbit[syms_Association,{pN_?RealQ,eN_?RealQ,MN_?RealQ}]:=
Module[{},
	If[MN<=0,OutputMessage[syms,"Black hole must have positive mass. Here M = "<>ToString[MN]<>".",1,Style->"Error"];Aborting[syms]];
	If[eN>=1||eN<0,OutputMessage[syms,"Eccentrcity must be in range 0<=e<1. Here e = "<>ToString[eN]<>".",1,Style->"Error"];Aborting[syms]];
	If[pN<=2eN+6,OutputMessage[syms,"{p, e} = " <> ToString[{pN,eN}] <> " specify an orbit beyond the separatrix, i.e. p<=2e+6.",1,Style->"Error"];Aborting[syms]];
]


testDef@
VerifyOrbit[syms_Association,{rN_?RealQ,MN_?RealQ}]:=
Module[{},
	If[MN<=0,OutputMessage[syms,"Black hole must have positive mass. Here M = "<>ToString[MN]<>".",1,Style->"Error"];Aborting[syms]];
	If[rN<6,OutputMessage[syms,"Circular orbits cannot be inside the ISCO, r = 6M. Here {r,M} = "<> ToString[{rN,MN}]<>".",1,Style->"Error"];Aborting[syms]];
]


End[];

EndPackage[];
