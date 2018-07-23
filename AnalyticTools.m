(* ::Package:: *)

BeginPackage["BlackHoleAnalysis`AnalyticTools`",
				{"BlackHoleAnalysis`OverloadedSymbols`",
				"BlackHoleAnalysis`Utils`",
				"BlackHoleAnalysis`Coordinates`",
				"BlackHoleAnalysis`ValidityTests`",
				"BlackHoleAnalysis`Symbols`"}];


(*******************************************************************)
(* Public functions *)
(*******************************************************************)
RemoveRpDotDot::usage="RemoveRpDotDot[expr] will replace second derivatives of r_p with \
respect to coordinate time t with an expression involving r_p, the black hole mass, and the E and J of the orbit in expr.";
RemoveRpDotSquared::usage="RemoveRpDotSquared[expr] will replace the square of the first derivative \
of r_p with respect to coordinate time t with an expression involving r_p, the black hole mass, and the E and J of the orbit in expr.";
RemoveRpDots::usage="RemoveRpDots[expr] calls RemoveRpDotDot[expr] followed by RemoveRpDotSquared[expr].";
RemovePhiPDots::usage="RemovePhiPDots[expr] will replace coordinate time derivatives of the azimuthal position phi_p of the \
particle with expressions involving r_p[t], its derivatives, and the specific energy and angular momentum, E and J, of the orbit.";

RemoveSHDots::usage="RemoveSHDots[expr] replaces coordinate time derivatives of scalar and vector spherical harmonics \
in expr with higher-order vector or tensor harmonics.";
ReduceLegendreP::usage="ReduceLegendreP[expr] lowers the order of the first argument of LegendreP if possible.";

EvaluateDiscontinuities::usage="EvaluateDiscontinuities[expr] calls EvaluateHeavisides, EvaluateDeltas and EvaluateJumps on expr.";
EvaluateDeltas::usage="EvaluateDeltas[expr] evaluates coefficients of Dirac delta distributions and their derivatives \
in expr at r=r_p[t].";
EvaluateHeavisides::usage="EvaluateHeavisides[expr] converts derivatives of Heaviside distributions in expr \
into Dirac delta distributions and their derivatives.";
EvaluateJumps::usage="EvaluateJumps[expr] converts functions in expr of the form fn_[t,rp[t]] on the 'plus' and 'minus'  \
side of the particle into a 'Jump[fn][t]' at the particle.";
RemoveJumpDots::usage="RemoveJumpDots[expr] converts time derivatives of jumps in expr to jumps in t derivatives and r derivatives.";
AddJumpDots::usage="AddJumpDots[expr] converts jumps in t derivatives in expr to time derivatives of jumps and jumps in r derivatives.";
HeavisideCoefficientsToJumps::usage="HeavisideCoefficientsToJumps[expr] removes singular terms in expr, and then computes \
the jumps in the remaining extended expressions at the location of the particle.";

RemoveExtended::usage="RemoveExtended[expr] removes all terms in expr proportional to Heaviside functions.";
RemoveSingular::usage="RemoveSingular[expr] removes all terms in expr not proportional to Heaviside functions (this includes \
derivatives of Heavisides).";

MakeFieldWeak::usage="MakeFieldWeak[field] replaces the smooth field with a weak expression that is (by default) C^-1.";
WeakFormToList::usage="WeakFormToList[expr] replaces the weak forms in expr with a list. Each element in the list has a higher \
degree of discontinuity.";

RToRStar::usage="RToRStar[expr] converts all appearances of Schwarzschild r in expr to r[r_*] and adjusts all r \
derivatives to be with respect to r_*.";
RStarToR::usage="RStarToR[expr] converts all appearances of the Tortoise coordinate r_* in expr to its value in \
terms of Schwarzschild r, and r_* derivatives to be with respect to r.";

LambdaToL::usage="LambdaToL[expr] converts all lambdas in expr to (l+2)(l-1)/2";

ToFrequencyDomain::usage="ToFrequencyDomain[expr] attempts to convert expr from the time domain to the frequency domain."


Begin["`Private`"];


Options[RemoveRpDots]=Options[FourVelocity];
DocumentationBuilder`OptionDescriptions["RemoveRpDots"] = DocumentationBuilder`OptionDescriptions["FourVelocity"];
Options[RemoveRpDotDot]=Options[FourVelocity];
DocumentationBuilder`OptionDescriptions["RemoveRpDotDot"] = DocumentationBuilder`OptionDescriptions["FourVelocity"];
Options[RemoveRpDotSquared]=Options[FourVelocity];
DocumentationBuilder`OptionDescriptions["RemoveRpDotSquared"] = DocumentationBuilder`OptionDescriptions["FourVelocity"];
Options[RemovePhiPDots]=Options[FourVelocity];
DocumentationBuilder`OptionDescriptions["RemovePhiPDots"] = DocumentationBuilder`OptionDescriptions["FourVelocity"];

Options[EvaluateDeltas]={"Simplify"->True};
DocumentationBuilder`OptionDescriptions["EvaluateDeltas"] = 
{
    "Simplify" -> "Collect and Simplify the final output."
};

Options[EvaluateJumps]=Options[EvaluateDeltas];
DocumentationBuilder`OptionDescriptions["EvaluateJumps"] = DocumentationBuilder`OptionDescriptions["EvaluateDeltas"];

Options[EvaluateHeavisides]=Options[EvaluateDeltas];
DocumentationBuilder`OptionDescriptions["EvaluateHeavisides"] = DocumentationBuilder`OptionDescriptions["EvaluateDeltas"];

Options[EvaluateDiscontinuities]=Options[EvaluateDeltas];
DocumentationBuilder`OptionDescriptions["EvaluateDiscontinuities"] = DocumentationBuilder`OptionDescriptions["EvaluateDeltas"];

Options[MakeFieldWeak]={"DiscontinuityOrder"->-1};
DocumentationBuilder`OptionDescriptions["MakeFieldWeak"] = 
{
    "DiscontinuityOrder" -> "The order of the discontinuity (-2 = \[Delta], -3 = \[Delta]', ...)."
};

Options[WeakFormToList]=Options[EvaluateDeltas];
DocumentationBuilder`OptionDescriptions["WeakFormToList"] = DocumentationBuilder`OptionDescriptions["EvaluateDeltas"];

Options[RToRStar]={"ExpandROfRStar"->False};
DocumentationBuilder`OptionDescriptions["RToRStar"] = 
{
    "ExpandROfRStar" -> "Express r[r_*] in terms of a ProductLog"
};


metricQ="Metric" ->Function[x,MemberQ[{"Schwarzschild","Kerr"},x]];


def@
RemoveRpDotDot[syms_Association,eq_,opts:OptionsPattern[]]:=
Module[{rp,t,rpDotDot,rule1},

	t=TSymbol[syms];
	rp=RpSymbol[syms];
	
	rpDotDot[tt_]:=(RpDotDot[syms,opts]/.t->tt);

	rule1=Derivative[n_?(#>=2&)][rp]:>Derivative[n-2][rpDotDot];

	eq/.rule1
]
reDef@
RemoveRpDotDot[eq_,opts:OptionsPattern[]]:=RemoveRpDotDot[DefaultSymbols[],eq,opts]


def@
RemoveRpDotSquared[syms_Association,eq_,opts:OptionsPattern[]]:=
Module[{rp,t,rule1,rpDotSq,rule2},

	TestOptions[{metricQ},{opts}];

	t=TSymbol[syms];
	rp=RpSymbol[syms];
	
	rpDotSq[tt_]:=(RpDotSquared[syms,opts]/.t->tt);
	rule1=Power[rp'[t],n_?(#>=2&)]:>rp'[t]^(n-2) rpDotSq[t];
	rule2=Power[rp'[t],n_?(#<=-2&)]:>rp'[t]^(n+2) rpDotSq[t]^-1;

	eq/.rule1/.rule2
]
reDef@
RemoveRpDotSquared[eq_,opts:OptionsPattern[]]:=RemoveRpDotSquared[DefaultSymbols[],eq,opts]


def@
RemoveRpDots[syms_Association,eq_,opts:OptionsPattern[]]:=
Module[{expr},

	expr=RemoveRpDotDot[syms,eq,opts];
	RemoveRpDotSquared[syms,expr,opts]
]
reDef@
RemoveRpDots[eq_,opts:OptionsPattern[]]:=RemoveRpDots[DefaultSymbols[],eq,opts]


def@
RemovePhiPDots[syms_Association,expr_,opts:OptionsPattern[]]:=
Module[{phip,JJ,t,EE,M,rp,phipDot,a,optionsRules},
	optionsRules = {"Metric" ->Function[x, x==="Schwarzschild" || x==="Kerr"]};
	TestOptions[optionsRules,{opts}];

	phip=PhiPSymbol[syms];
	t=TSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	a=BlackHoleSpinSymbol[syms];
	JJ=SpecificAngularMomentumSymbol[syms];
	EE=SpecificEnergySymbol[syms];
	rp=RpSymbol[syms];

	Which[OptionValue["Metric"]==="Schwarzschild",
		expr/.Derivative[n_][phip][t]:>D[(JJ f[rp[t],M])/(EE rp[t]^2),{t,n-1}],

		OptionValue["Metric"]==="Kerr",
		phipDot = (2 a EE M-2 JJ M+JJ rp[t])/(2 a (a EE-JJ) M+a^2 EE rp[t]+EE rp[t]^3);
		expr/.Derivative[n_][phip][t]:>D[phipDot,{t,n-1}],

		True,
		Print["No metric ", OptionValue["Metric"]];
		Aborting[syms]
	]
]
reDef@
RemovePhiPDots[expr_,opts:OptionsPattern[]]:=RemovePhiPDots[DefaultSymbols[],expr,opts]


def@
RemoveSHDots[syms_Association,eq_]:=
Module[{rp,t,r,
		M,JJ,En,la,
		YBar,YphiBar,YphiphiBar,
		XphiBar,XphiphiBar,Y,Yphi,Yphiphi,
		Xphi,Xphiphi,SHDotRules,SHDotRules2},

	t=TSymbol[syms];
	r=RSymbol[syms];
	rp=RpSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	En=SpecificEnergySymbol[syms];
	JJ=SpecificAngularMomentumSymbol[syms];
	la=LambdaSymbol[syms];
	YBar=YSymbol[syms,Conjugate->True];
	YphiBar=YPhiSymbol[syms,Conjugate->True];
	YphiphiBar=YPhiPhiSymbol[syms,Conjugate->True];
	XphiBar=XPhiSymbol[syms,Conjugate->True];
	XphiphiBar=XPhiPhiSymbol[syms,Conjugate->True];
	Y=YSymbol[syms,Conjugate->False];
	Yphi=YPhiSymbol[syms,Conjugate->False];
	Yphiphi=YPhiPhiSymbol[syms,Conjugate->False];
	Xphi=XPhiSymbol[syms,Conjugate->False];
	Xphiphi=XPhiPhiSymbol[syms,Conjugate->False];

	SHDotRules={Derivative[n_][YBar][t]:>D[YphiBar[t] JJ/En f[rp[t],M]/rp[t]^2,{t,n-1}],
				Derivative[n_][XphiBar][t]:>D[XphiphiBar[t] JJ/En f[rp[t],M]/rp[t]^2,{t,n-1}],
				Derivative[n_][YphiBar][t]:>D[(YphiphiBar[t]-(la+1)YBar[t]) JJ/En (1-(2M)/rp[t]) 1/rp[t]^2,{t,n-1}]};

	SHDotRules2={Derivative[n_][Y][t]:>D[Yphi[t] JJ/En f[rp[t],M]/rp[t]^2,{t,n-1}],
				Derivative[n_][Xphi][t]:>D[Xphiphi[t] JJ/En f[rp[t],M]/rp[t]^2,{t,n-1}],
				Derivative[n_][Yphi][t]:>D[(Yphiphi[t]-(la+1)Y[t]) JJ/En (1-(2M)/rp[t]) 1/rp[t]^2,{t,n-1}]};
	
	reduceTimeDerivs[syms,eq//.SHDotRules//.SHDotRules2]
]
reDef@
RemoveSHDots[eq_]:=RemoveSHDots[DefaultSymbols[],eq]


def@
reduceTimeDerivs[syms_Association,expr_]:=
Module[{m,t,phip,rp,yPP,yPPC,xPP,xPPC},
	m=MSymbol[syms];
	t=TSymbol[syms];
	rp=RpSymbol[syms];
	phip=PhiPSymbol[syms];
	yPP=YPhiPhiSymbol[syms,Conjugate->False];
	yPPC=YPhiPhiSymbol[syms,Conjugate->True];
	xPP=XPhiPhiSymbol[syms,Conjugate->False];
	xPPC=XPhiPhiSymbol[syms,Conjugate->True];

	Collect[RemoveRpDots[syms,RemovePhiPDots[syms,expr//.{Derivative[a_][yPPC][t]:>D[-I m phip'[t]yPPC[t],{t,a-1}],Derivative[a_][yPP][t]:>D[I m phip'[t]yPP[t],{t,a-1}],
												Derivative[a_][xPPC][t]:>D[-I m phip'[t]xPPC[t],{t,a-1}],Derivative[a_][xPP][t]:>D[I m phip'[t]xPP[t],{t,a-1}]}]],{yPPC[t],yPP[t],xPPC[t],xPP[t],Derivative[_][rp][t]}]
];
reDef@reduceTimeDerivs[expr_]:=reduceTimeDerivs[DefaultSymbols[],expr];


def@
ReduceLegendreP[syms_Association,expr_]:=
Module[{l},
	l=LSymbol[syms];
	expr//.LegendreP[l+a_,m_,0]/;a>=2:>((1+(l+a-2)+m) LegendreP[(l+a-2),m,0])/(-2-(l+a-2)+m)
]
reDef@
ReduceLegendreP[expr_]:=ReduceLegendreP[DefaultSymbols[],expr]


def@
EvaluateDiscontinuities[syms_Association,expr_,opts:OptionsPattern[]]:=
Module[{expr1,simp,optionsRules,DD,th},

	optionsRules = {"Simplify" ->BooleanQ};
	TestOptions[optionsRules,{opts}];

	simp=OptionValue["Simplify"];

	DD=DiracDeltaSymbol[syms];
	th=HeavisideSymbol[syms];

	expr1=EvaluateJumps[syms,
			EvaluateDeltas[syms,
				EvaluateHeavisides[syms,expr,Simplify->False],Simplify->False],Simplify->False];
	
	If[simp,
		Collect[expr1,{Derivative[_][DD][_], DD[_], th[_]}, Simplify],
		expr1
	]
]
reDef@
EvaluateDiscontinuities[expr_,opts:OptionsPattern[]]:=EvaluateDiscontinuities[DefaultSymbols[],expr,opts]


def@
EvaluateDeltas[syms_Association,eq_,opts:OptionsPattern[]]:=
Module[{eval,mMax,sol,z,tempEq,r,optionsRules,simp,
		rp,t,DD,arg,nonDeltas},

	optionsRules = {"Simplify" ->BooleanQ};
	TestOptions[optionsRules,{opts}];

	simp=OptionValue["Simplify"];

	t=TSymbol[syms];
	r=RSymbol[syms];
	rp=RpSymbol[syms];
	DD=DiracDeltaSymbol[syms];

	arg=r-rp[t];

	nonDeltas=eq/.{DD[arg]->0,Derivative[_][DD][arg]->0};

	sol=Solve[arg==z,r][[1]];
	tempEq=eq/.sol;
	mMax=Max@{Cases[tempEq,Derivative[n_][DD][z]:>n,Infinity],0};

	eval=Sum[(-1)^n m!/(n!(m-n)!) (D[Coefficient[tempEq,Derivative[m][DD][z]],{z,n}]/.z->0)Derivative[m-n][DD][arg],{m,0,mMax},{n,0,m}];
	
	If[simp,
		nonDeltas+Collect[eval,{DD[arg],Derivative[_][DD][arg]},Simplify],
		nonDeltas+eval
	]
]
reDef@
EvaluateDeltas[eq_,opts:OptionsPattern[]]:=EvaluateDeltas[DefaultSymbols[],eq,opts]


reDef@
EvaluateDeltas[expr_,vars_List,opts:OptionsPattern[]]:=
Module[{optionsRules,simp,derivArgs,deriv0Args,argsDeltaPrimesSubset,
	argsDeltaPrimes,argsDeltas,eqs,argsDeltasSubset,
	argRules,argsSubset,n,evalDeltas,exprA},

	optionsRules = {"Simplify" ->BooleanQ};
	TestOptions[optionsRules,{opts}];

	simp=OptionValue["Simplify"];

	exprA=expr/.{DiracDelta[x__]:>Product[DiracDelta[i],{i,{x}}],Derivative[a__][DiracDelta][b__]:>Product[Derivative[i[[1]]][DiracDelta][i[[2]]],{i,Thread[{{a},{b}}]}]};
	argsDeltas=Flatten@DeleteDuplicates@Cases[{exprA}, a_DiracDelta:>Sequence@@a,Infinity];
	argsDeltaPrimes=Flatten[DeleteDuplicates@Cases[{exprA}, Derivative[a_][DiracDelta][b_]:>Thread[{{b},{a}}],Infinity],1];
	argsDeltasSubset=Join@@Table[If[Cases[{#},var,Infinity]=!={},#,##&[]]&/@argsDeltas,{var,vars}];
	argsDeltaPrimesSubset=Join@@Table[If[Cases[{#},var,Infinity]=!={},#,##&[]]&/@argsDeltaPrimes,{var,vars}];
	argsSubset=DeleteDuplicates@Join[argsDeltasSubset,argsDeltaPrimesSubset[[All,1]]];
	eqs=#==0&/@argsSubset;
	argRules=Solve[eqs,vars][[1]];

	deriv0Args={#,#/.Thread[argsSubset->argRules],0}&/@argsDeltasSubset;
	derivArgs={#[[1]],#[[1]]/.Thread[argsSubset->argRules],#[[2]]}&/@argsDeltaPrimesSubset;

	evalDeltas[expr1_,{arg_,rule_,nDs_}]:=
	Module[{rest},
		rest=Coefficient[expr1,Derivative[nDs][DiracDelta][arg],0];
		Sum[(-1)^n nDs!/(n!(nDs-n)!)( D[Coefficient[expr1,Derivative[nDs][DiracDelta][arg]],{rule[[1]],n}]/.rule)Derivative[nDs-n][DiracDelta][arg],{n,0,nDs}]+rest
	];

	Collect[Fold[evalDeltas,exprA,Join[deriv0Args,derivArgs]],{_DiracDelta,Derivative[_][DiracDelta][_]},If[simp,Simplify,Identity]]
]


def@
EvaluateHeavisides[syms_Association,expr_,opts:OptionsPattern[]]:=
Module[{t,r,rp,DD,th,expr1,expr2,
		argVal,tempArg,expr3,simp,optionsRules},

	optionsRules = {"Simplify" ->BooleanQ};
	TestOptions[optionsRules,{opts}];

	simp=OptionValue["Simplify"];

	t=TSymbol[syms];
	r=RSymbol[syms];
	rp=RpSymbol[syms];
	DD=DiracDeltaSymbol[syms];
	th=HeavisideSymbol[syms];

	argVal=r-rp[t];

	expr1=expr/.{argVal->tempArg,-argVal->-tempArg};
	expr2=expr1/. {Derivative[n_][th][-tempArg] :> (-1)^(n + 1) D[DD[tempArg], {tempArg, n - 1}],
								Derivative[n_][th][tempArg] :> D[DD[tempArg], {tempArg, n - 1}]};
	expr3=expr2/.tempArg->argVal;

	If[simp,
		Collect[expr3,{Derivative[_][DD][_], DD[_], th[_]}, Simplify],
		expr3
	]
]
reDef@
EvaluateHeavisides[expr_,opts:OptionsPattern[]]:=EvaluateHeavisides[DefaultSymbols[],expr,opts]


def@
EvaluateJumps[syms_Association,expr_,opts:OptionsPattern[]]:=
Module[{optionsRules,t,r,rp,DD,th,tRpFuncs,expr1,
		tRpFuncsDM,tRpFuncsDP,ruleDP,ruleDM,
	ruleP,ruleM,simp,jumpRules},

	optionsRules = {"Simplify" ->BooleanQ};
	TestOptions[optionsRules,{opts}];

	simp=OptionValue["Simplify"];

	t=TSymbol[syms];
	r=RSymbol[syms];
	rp=RpSymbol[syms];
	DD=DiracDeltaSymbol[syms];
	th=HeavisideSymbol[syms];

	tRpFuncs=Union@Cases[expr,fn_[t,rp[t]]:>fn,Infinity];
	tRpFuncsDP=Cases[tRpFuncs,Derivative[_,_][_["Tags"->{___,"Up"->{___,"PlusTag"}}]]];
	tRpFuncsDM=Cases[tRpFuncs,Derivative[_,_][_["Tags"->{___,"Up"->{___,"MinusTag"}}]]];

	ruleP=#[t,rp[t]]->Jump[0,0][DropTag[syms,#,"PlusTag"]][t]&/@Cases[tRpFuncs,_["Tags"->{___,"Up"->{___,"PlusTag"}}]];
	ruleM=#[t,rp[t]]->0&/@Cases[tRpFuncs,_["Tags"->{___,"Up"->{___,"MinusTag"}}]];
	
	ruleDP=(Derivative[(Sequence@@#[[1]])][(#[[2]])][t,rp[t]]->
			Jump[(Sequence@@#[[1]])][DropTag[syms,#[[2]],"PlusTag"]][t]&)/@Cases[tRpFuncsDP,Derivative[m_,n_][fn_]:>{{m,n},fn}];
	ruleDM=(Derivative[(Sequence@@#[[1]])][(#[[2]])][t,rp[t]]->0&)/@Cases[tRpFuncsDM,Derivative[m_,n_][fn_]:>{{m,n},fn}];

	jumpRules=Join[ruleP,ruleM,ruleDP,ruleDM];
	
	expr1=expr/.jumpRules;
	If[simp,
		Collect[expr1,{Derivative[_][DD][_], DD[_], th[_]}, Simplify],
		expr1
	]
]
reDef@
EvaluateJumps[expr_,opts:OptionsPattern[]]:=EvaluateJumps[DefaultSymbols[],expr,opts]


def@
RemoveJumpDots[syms_Association,expr_]:=
Module[{t,rp,rule},

	t=TSymbol[syms];
	rp=RpSymbol[syms];

	rule=Derivative[a_/;a>=1][Jump[m_,n_][fn_]][t_]:>D[Jump[m+1,n][fn][t]+rp'[t]Jump[m,n+1][fn][t],{t,a-1}];

	expr//.rule
]
reDef@
RemoveJumpDots[expr_]:=RemoveJumpDots[DefaultSymbols[],expr]


def@
AddJumpDots[syms_Association,expr_]:=
Module[{t,rp,addDots},

	t=TSymbol[syms];
	rp=RpSymbol[syms];

	addDots={Jump[m_/;m>=1,n_][fn_][t_]:>Jump[m-1,n][fn]'[t]-rp'[t]Jump[m-1,n+1][fn][t],
				Derivative[a_][Jump[m_/;m>=1,n_][fn_]][t_]:>D[Jump[m-1,n][fn][t],{t,a+1}]
				-D[rp'[t]Jump[m-1,n+1][fn][t],{t,a}]};

	expr//.addDots
]
reDef@
AddJumpDots[expr_]:=AddJumpDots[DefaultSymbols[],expr]


def@
HeavisideCoefficientsToJumps[syms_Association,expr_]:=
Module[{r,rp,t,th,exprNoSing},

	r=RSymbol[syms];
	rp=RpSymbol[syms];
	t=TSymbol[syms];
	th=HeavisideSymbol[syms];
	
	exprNoSing=RemoveSingular[syms,expr];
	EvaluateJumps[syms,Coefficient[exprNoSing,th[r-rp[t]]]-Coefficient[exprNoSing,th[rp[t]-r]]/.r->rp[t]]
]
reDef@
HeavisideCoefficientsToJumps[expr_]:=HeavisideCoefficientsToJumps[DefaultSymbols[],expr]


def@
RemoveExtended[syms_Association,expr_]:=
Module[{t,r,rp,DD,th},

	t=TSymbol[syms];
	r=RSymbol[syms];
	rp=RpSymbol[syms];
	DD=DiracDeltaSymbol[syms];
	th=HeavisideSymbol[syms];

	Collect[expr/.th[__]->0,{Derivative[__][DD][r-rp[t]],DD[r-rp[t]],th[r-rp[t]],th[rp[t]-r]},Simplify]
]
reDef@
RemoveExtended[expr_]:=RemoveExtended[DefaultSymbols[],expr]


def@
RemoveSingular[syms_Association,expr_]:=
Module[{t,r,rp,DD,th,expr1},

	t=TSymbol[syms];
	r=RSymbol[syms];
	rp=RpSymbol[syms];
	DD=DiracDeltaSymbol[syms];
	th=HeavisideSymbol[syms];

	expr1=EvaluateHeavisides[syms,expr];
	Collect[expr1/.{Derivative[__][DD][__]->0,DD[__]->0},{Derivative[__][DD][r-rp[t]],DD[r-rp[t]],th[r-rp[t]],th[rp[t]-r]},Simplify]
]
reDef@
RemoveSingular[expr_]:=RemoveSingular[DefaultSymbols[],expr]


def@
MakeFieldWeak[syms_Association,field_,opts:OptionsPattern[]]:=
Module[{heav,r,rp,t,DD,discO,fieldP,fieldM,G,
		head,optionsRules},

	optionsRules = {"DiscontinuityOrder" ->IntegerQ};
	TestOptions[optionsRules,{opts}];
		
	discO=OptionValue[DiscontinuityOrder];
	
	DD=DiracDeltaSymbol[syms];
	heav=HeavisideSymbol[syms];
	r=RSymbol[syms];
	rp=RpSymbol[syms];
	t=TSymbol[syms];
	G =GCoefficientSymbol[syms];
	
	head=Head@field;
	
	If[head[t,r]=!=field,
		OutputMessage[syms,"Field must be called as a function of "<>ToString[t]<>" and "<>ToString[r]<>", e.g.",1,Style->"Error"];
		OutputMessage[syms,"func["<>ToString[t]<>","<>ToString[r]<>"]",1,Style->"Error"];
		Aborting[syms]
	];

	fieldP=AddTag[syms,head,"PlusTag",TagPosition->"Up"];
	fieldM=AddTag[syms,head,"MinusTag",TagPosition->"Up"];
	
	fieldP[t,r] heav[r-rp[t]]+fieldM[t,r] heav[rp[t]-r]
		+ Sum[G[n][0,0][head][t]D[DD[r-rp[t]],{r,n}],{n,0,Abs@discO-2}]
]
reDef@
MakeFieldWeak[field_,opts:OptionsPattern[]]:=MakeFieldWeak[DefaultSymbols[],field,opts]


def@
WeakFormToList[syms_Association,expr_,opts:OptionsPattern[]]:=
Module[{th,DD,r,rp,t,thPCoeff,optionsRules,simp,
		thMCoeff,ddCoeff,ddCases,dddCoeffs},

	optionsRules = {"Simplify" ->BooleanQ};
	TestOptions[optionsRules,{opts}];

	simp=OptionValue["Simplify"];

	r=RSymbol[syms];
	rp=RpSymbol[syms];
	t=TSymbol[syms];
	th=HeavisideSymbol[syms];
	DD=DiracDeltaSymbol[syms];
	
	thPCoeff=Collect[Coefficient[expr,th[r-rp[t]]],{_[t,r]}];
	thMCoeff=Collect[Coefficient[expr,th[rp[t]-r]],{_[t,r]}];
	
	ddCoeff={Collect[Coefficient[expr,DD[r-rp[t]]],{Jump[__][_][t]}]};
	ddCases=Sort@Cases[expr,Derivative[n_][DD][r-rp[t]]:>n,Infinity];
	dddCoeffs=Coefficient[expr,Derivative[#][DD][r-rp[t]]]&/@ddCases;
	
	If[simp,Simplify,Identity]@Join[{{thPCoeff ,thMCoeff}},ddCoeff,dddCoeffs]
]
reDef@
WeakFormToList[expr_,opts:OptionsPattern[]]:=WeakFormToList[DefaultSymbols[],expr,opts]


def@
RToRStar[syms_Association, expr_,opts:OptionsPattern[]] :=
Module[{expr1, t, r, M, x, argsToXOfR,noRStarPrimes,argsToX,rOfRStar,optionsRules},
  
	optionsRules={"ExpandROfRStar"->BooleanQ};
	TestOptions[optionsRules,{opts}];

	t = TSymbol[syms];
	r = RSymbol[syms];
	M = BlackHoleMassSymbol[syms];
	x = RStarSymbol[syms];
	rOfRStar=If[OptionValue[ExpandROfRStar],ROfRStar[x,M],r[x]];

	argsToXOfR={Derivative[n_][fn_][r]:>D[fn[x[r]],{r,n}],Derivative[m_,n_][fn_][t,r]:> D[fn[t,x[r]],{t,m},{r,n}],fn_[t, r]:> fn[t, x[r]],fn_[r]:>fn[x[r]]};
	noRStarPrimes=Derivative[n_][x][r]:>D[RStarOfR[r,M],{r,n}];
	argsToX={Derivative[n_][fn_][x[r]]:>D[fn[x],{x,n}],Derivative[m_,n_][fn_][t,x[r]]:> D[fn[t,x],{t,m},{x,n}],fn_[t, x[r]]:> fn[t, x],fn_[x[r]]:>fn[x],r->rOfRStar};
	expr1 = expr /.argsToXOfR/.noRStarPrimes/.argsToX;
 	
	Collect[expr1, {Derivative[__][__][__]}, Simplify]
]
reDef@
RToRStar[expr_,opts:OptionsPattern[]]:=RToRStar[DefaultSymbols[],expr,opts]


def@
RStarToR[syms_Association,expr_]:=
Module[{expr1,t,r,M,argsToROfX,noRPrimes,noProductLog,x},

	t=TSymbol[syms];
	r=RSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	x=RStarSymbol[syms];

	noProductLog={RhoOfRStar[x,M]/(2M)->(r-2M)/(2M)};

	argsToROfX={Derivative[n_][fn_][x]:>D[fn[r[x]],{x,n}],Derivative[m_,n_][fn_][t,x]:> D[fn[t,r[x]],{t,m},{x,n}],fn_[t, x]:> fn[t, r[x]],fn_[x]/;fn!=r:>fn[r[x]]};
	noRPrimes={Derivative[n_][r][x]:>Nest[Function[x,f[r,M]D[x,r]],r,n],r[x]:>r};
	expr1=expr/.noProductLog/.argsToROfX/.noRPrimes;
	Collect[expr1, {Derivative[__][__][__]}, Simplify]
];
reDef@
RStarToR[expr_]:=RStarToR[DefaultSymbols[],expr]


def@
LambdaToL[syms_Association,expr_]:=
Module[{l,la},
	
	l =LSymbol[syms];
	la =LambdaSymbol[syms];

	expr/.{la->LambdaOfL[l]}
];
reDef@
LambdaToL[expr_]:=LambdaToL[DefaultSymbols[],expr]


reDef@
Parity[l_Integer,m_Integer]:=If[EvenQ[l+m],"Even","Odd"]


def@
ToFrequencyDomain[syms_Association,DE_] := 
Module[{DE1,t, omega, rp, r},
	t = TSymbol[syms];
	r = RSymbol[syms];
	rp = RpSymbol[syms];
	omega=FrequencySymbol[syms];

	DE1 = DE /. {Derivative[m_, n_][func_][tt_, x_] :> (-I omega)^m Derivative[n][func][x],fn_[t, x_] :> fn[x],rp[t]->r};
    Collect[DE1, {Derivative[___][_][_],_[_]}, Simplify]
];
reDef@
ToFrequencyDomain[DE_]:=ToFrequencyDomain[DefaultSymbols[],DE]


End[];

EndPackage[];
