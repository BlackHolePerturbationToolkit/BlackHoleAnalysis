(* ::Package:: *)

BeginPackage["BlackHoleAnalysis`Harmonics`",
				{"BlackHoleAnalysis`Utils`",
					"BlackHoleAnalysis`Labels`",
					"BlackHoleAnalysis`ValidityTests`",
					"BlackHoleAnalysis`Symbols`"}];


(*******************************************************************)
(* Public functions *)
(*******************************************************************)


YHarmonic::usage="YHarmonic[{l,m},{theta,phi}] returns the scalar spherical harmonic Y_lm[theta,phi]. 
YHarmonic[{l,m},{theta,phi},s] returns the spin-weighted spherical harmonic _s Y_lm[theta,phi].";
YTheta::usage="YTheta[{l,m},{theta,phi}] returns the vector spherical harmonic component Y_lm_Theta[theta,phi].";
YPhi::usage="YPhi[{l,m},{theta,phi}] returns the vector spherical harmonic component Y_lm_Phi[theta,phi].";
YThetaTheta::usage="YThetaTheta[{l,m},{theta,phi}] returns the tensor spherical harmonic component Y_lm_ThetaTheta[theta,phi].";
YThetaPhi::usage="YThetaPhi[{l,m},{theta,phi}] returns the tensor spherical harmonic component Y_lm_ThetaPhi[theta,phi].";
YPhiPhi::usage="YPhiPhi[{l,m},{theta,phi}] returns the tensor spherical harmonic component Y_lm_PhiPhi[theta,phi].";
XTheta::usage="XTheta[{l,m},{theta,phi}] returns the vector spherical harmonic component X_lm_Theta[theta,phi].";
XPhi::usage="XPhi[{l,m},{theta,phi}] returns the vector spherical harmonic component Y_lm_Phi[theta,phi].";
XThetaTheta::usage="XThetaTheta[{l,m},{theta,phi}] returns the tensor spherical harmonic component X_lm_ThetaTheta[theta,phi].";
XThetaPhi::usage="XThetaPhi[{l,m},{theta,phi}] returns the tensor spherical harmonic component X_lm_ThetaPhi[theta,phi].";
XPhiPhi::usage="XPhiPhi[{l,m},{theta,phi}] returns the tensor spherical harmonic component X_lm_PhiPhi[theta,phi].";


SphericalHarmonicFunction::usage="SphericalHarmonicFunction[label] returns the function for returning a spherical harmonic associated with label.";

ExpandHarmonics::usage="ExpandHarmonics[expr] expands symbolic scalar, vector and tensor spherical harmonic in terms of trigonometric terms.";


Begin["`Private`"];


Options[YHarmonic]={"Phi"->True};
DocumentationBuilder`OptionDescriptions["YHarmonic"] = 
{
    "Phi" -> "Boolean specifying whether to include the phi factor with the spherical harmonic"
};
Options[YTheta]=Options[YHarmonic];
DocumentationBuilder`OptionDescriptions["YTheta"] = DocumentationBuilder`OptionDescriptions["YHarmonic"];
Options[YPhi]=Options[YHarmonic];
DocumentationBuilder`OptionDescriptions["YPhi"] = DocumentationBuilder`OptionDescriptions["YHarmonic"];
Options[YThetaTheta]=Options[YHarmonic];
DocumentationBuilder`OptionDescriptions["YThetaTheta"] = DocumentationBuilder`OptionDescriptions["YHarmonic"];
Options[YThetaPhi]=Options[YHarmonic];
DocumentationBuilder`OptionDescriptions["YThetaPhi"] = DocumentationBuilder`OptionDescriptions["YHarmonic"];
Options[YPhiPhi]=Options[YHarmonic];
DocumentationBuilder`OptionDescriptions["YPhiPhi"] = DocumentationBuilder`OptionDescriptions["YHarmonic"];
Options[XTheta]=Options[YHarmonic];
DocumentationBuilder`OptionDescriptions["XTheta"] = DocumentationBuilder`OptionDescriptions["YHarmonic"];
Options[XPhi]=Options[YHarmonic];
DocumentationBuilder`OptionDescriptions["XPhi"] = DocumentationBuilder`OptionDescriptions["YHarmonic"];
Options[XThetaTheta]=Options[YHarmonic];
DocumentationBuilder`OptionDescriptions["XThetaTheta"] = DocumentationBuilder`OptionDescriptions["YHarmonic"];
Options[XThetaPhi]=Options[YHarmonic];
DocumentationBuilder`OptionDescriptions["XThetaPhi"] = DocumentationBuilder`OptionDescriptions["YHarmonic"];
Options[XPhiPhi]=Options[YHarmonic];
DocumentationBuilder`OptionDescriptions["XPhiPhi"] = DocumentationBuilder`OptionDescriptions["YHarmonic"];
Options[ExpandHarmonics]={"Phi"->True,"EvaluateTheta"->True};
DocumentationBuilder`OptionDescriptions["ExpandHarmonics"] = Join[DocumentationBuilder`OptionDescriptions["YHarmonic"],{"EvaluateTheta"->"Boolean stating whether to evaluate theta and its derivatives at pi/2"}];


def@
SphericalHarmonicFunction[label_String]:=
Module[{assoc},

	assoc=Association[
		YLabel[]->YHarmonic,
		YThetaLabel[]->YTheta,
		YPhiLabel[]->YPhi,
		YThetaThetaLabel[]->YThetaTheta,
		YThetaPhiLabel[]->YThetaPhi,
		YPhiPhiLabel[]->YPhiPhi,
		XThetaLabel[]->XTheta,
		XPhiLabel[]->XPhi,
		XThetaThetaLabel[]->XThetaTheta,
		XThetaPhiLabel[]->XThetaPhi,
		XPhiPhiLabel[]->XPhiPhi];

	assoc[label]
]


phiTest={"Phi"->BooleanQ};


def@
YHarmonic[{l_,m_},{th_,ph_},opts:OptionsPattern[]]:=
YHarmonic[{l,m},{th,ph},opts]=
Module[{expVal},

	TestOptions[phiTest,{opts}];

	If[Abs@m>l, Return[0]];

	expVal = If[OptionValue[Phi]===True,Exp[I m ph],1];

	Sqrt[((2l+1)Factorial[l-m])/(4\[Pi] Factorial[l+m])]LegendreP[l,m,Cos[th]] expVal
];


reDef@YHarmonic[{l_,m_},{th_,ph_},s_,opts:OptionsPattern[]]:=
YHarmonic[{l,m},{th,ph},s,opts]=
Module[{r,c,expVal},

	expVal = If[OptionValue[Phi]===True,Exp[I m ph],1];
	(-1)^m expVal Sqrt[((l+m)! (l-m)! (2 l+1))/(4 \[Pi] (l+s)! (l-s)!)] Sin[th/2]^(2 l) \!\(
\*UnderoverscriptBox[\(\[Sum]\), \(r = 0\), \(l - s\)]\(Binomial[l - s, r]\ Binomial[l + s, r + s - m]\ 
\*SuperscriptBox[\((\(-1\))\), \(l - r - s\)]\ 
\*SuperscriptBox[\(Cot[
\*FractionBox[\(th\), \(2\)]]\), \(2\ r + s - m\)]\)\)
]


def@
YTheta[{l_,m_},{th_,ph_},opts:OptionsPattern[]]:=
YTheta[{l,m},{th,ph},opts]=
Module[{ll,mm,tth,pph,expVal},

	TestOptions[phiTest,{opts}];

	expVal = If[OptionValue[Phi]===True,1,Exp[-I m ph]];

	Cancel[expVal D[YHarmonic[{ll,mm},{tth,pph}],tth]/.{ll->l,mm->m,tth->th,pph->ph}]
]


def@
YPhi[{l_,m_},{th_,ph_},opts:OptionsPattern[]]:=
YPhi[{l,m},{th,ph},opts]=
Module[{ll,mm,tth,pph,expVal},

	TestOptions[phiTest,{opts}];

	expVal = If[OptionValue[Phi]===True,1,Exp[-I m ph]];

	Cancel[expVal D[YHarmonic[{ll,mm},{tth,pph}],pph]/.{ll->l,mm->m,tth->th,pph->ph}]
]


def@
XTheta[{l_,m_},{th_,ph_},opts:OptionsPattern[]]:=
XTheta[{l,m},{th,ph},opts]=
Module[{expVal},

	TestOptions[phiTest,{opts}];

	expVal = If[OptionValue[Phi]===True,1,Exp[-I m ph]];

	Cancel[-(1/Sin[th])YPhi[{l,m},{th,ph}] expVal]
]


def@
XPhi[{l_,m_},{th_,ph_},opts:OptionsPattern[]]:=
XPhi[{l,m},{th,ph},opts]=
Module[{expVal},

	TestOptions[phiTest,{opts}];

	expVal = If[OptionValue[Phi]===True,1,Exp[-I m ph]];

	Cancel[Sin[th]YTheta[{l,m},{th,ph}] expVal]
]


def@
YThetaTheta[{l_,m_},{th_,ph_},opts:OptionsPattern[]]:=
YThetaTheta[{l,m},{th,ph},opts]=
Module[{ll,mm,tth,pph,expVal},

	TestOptions[phiTest,{opts}];

	expVal = If[OptionValue[Phi]===True,1,Exp[-I m ph]];

	Cancel[expVal (D[YHarmonic[{ll,mm},{tth,pph}],tth,tth]+(ll(ll+1))/2 YHarmonic[{ll,mm},{tth,pph}])/.{ll->l,mm->m,tth->th,pph->ph}]
]


def@
YThetaPhi[{l_,m_},{th_,ph_},opts:OptionsPattern[]]:=
YThetaPhi[{l,m},{th,ph},opts]=
Module[{ll,mm,tth,pph,expVal},

	TestOptions[phiTest,{opts}];

	expVal = If[OptionValue[Phi]===True,1,Exp[-I m ph]];

	Cancel[expVal (D[YHarmonic[{ll,mm},{tth,pph}],tth,pph]-Cot[tth]D[YHarmonic[{ll,mm},{tth,pph}],pph])/.{ll->l,mm->m,tth->th,pph->ph}]
]


def@
YPhiPhi[{l_,m_},{th_,ph_},opts:OptionsPattern[]]:=
YPhiPhi[{l,m},{th,ph},opts]=
Module[{expVal},

	TestOptions[phiTest,{opts}];

	expVal = If[OptionValue[Phi]===True,1,Exp[-I m ph]];

	Cancel[-Sin[th]^2 YThetaTheta[{l,m},{th,ph}] expVal]
]


def@
XThetaTheta[{l_,m_},{th_,ph_},opts:OptionsPattern[]]:=
XThetaTheta[{l,m},{th,ph},opts]=
Module[{ll,mm,tth,pph,expVal},

	TestOptions[phiTest,{opts}];

	expVal = If[OptionValue[Phi]===True,1,Exp[-I m ph]];

	Cancel[expVal (-1/Sin[tth]D[YHarmonic[{ll,mm},{tth,pph}],tth,pph]+Cos[tth]/Sin[tth]^2 D[YHarmonic[{ll,mm},{tth,pph}],pph])/.{ll->l,mm->m,tth->th,pph->ph}]
]


def@
XThetaPhi[{l_,m_},{th_,ph_},opts:OptionsPattern[]]:=
XThetaPhi[{l,m},{th,ph},opts]=
Module[{ll,mm,tth,pph,expVal},

	TestOptions[phiTest,{opts}];

	expVal = If[OptionValue[Phi]===True,1,Exp[-I m ph]];

	Cancel[expVal Sin[tth](D[YHarmonic[{ll,mm},{tth,pph}],tth,tth]+(ll(ll+1))/2 YHarmonic[{ll,mm},{tth,pph}])/.{ll->l,mm->m,tth->th,pph->ph}]
]


def@
XPhiPhi[{l_,m_},{th_,ph_},opts:OptionsPattern[]]:=
XPhiPhi[{l,m},{th,ph},opts]=
Module[{ll,mm,tth,pph,expVal},

	TestOptions[phiTest,{opts}];

	expVal = If[OptionValue[Phi]===True,1,Exp[-I m ph]];

	Cancel[expVal (Sin[tth]D[YHarmonic[{ll,mm},{tth,pph}],tth,pph]-Cos[tth]D[YHarmonic[{ll,mm},{tth,pph}],pph])/.{ll->l,mm->m,tth->th,pph->ph}]
]


def@
ExpandHarmonics[syms_Association,expr_,opts:OptionsPattern[]]:=
Module[{YBar,YthBar,YphBar,YththBar,YthphBar,YphphBar,
	XthBar,XphBar,XththBar,XthphBar,XphphBar,
	YBarSub,YthBarSub,YphBarSub,YththBarSub,YthphBarSub,YphphBarSub,
	XthBarSub,XphBarSub,XththBarSub,XthphBarSub,XphphBarSub,
	Y,Yth,Yph,Ythth,Ythph,Yphph,
	Xth,Xph,Xthth,Xthph,Xphph,evalTh,
	YSub,YthSub,YphSub,YththSub,YthphSub,YphphSub,
	XthSub,XphSub,XththSub,XthphSub,XphphSub,
	t,l,m,th,ph,asmps,subs,optionsRules,PhiO,refined},

	optionsRules = {"Phi" -> BooleanQ,
					"EvaluateTheta" -> BooleanQ};
	TestOptions[optionsRules,{opts}];
	PhiO=OptionValue[Phi];
	evalTh=OptionValue[EvaluateTheta];

	t=TSymbol[syms];
	l=LSymbol[syms];
	m=MSymbol[syms];
	th=ThetaSymbol[syms];
	ph=PhiPSymbol[syms];


	YBar=YSymbol[syms,Conjugate->True];
	YthBar=YThetaSymbol[syms,Conjugate->True];
	YphBar=YPhiSymbol[syms,Conjugate->True];
	YththBar=YThetaThetaSymbol[syms,Conjugate->True];
	YthphBar=YThetaPhiSymbol[syms,Conjugate->True];
	YphphBar=YPhiPhiSymbol[syms,Conjugate->True];
	XthBar=XThetaSymbol[syms,Conjugate->True];
	XphBar=XPhiSymbol[syms,Conjugate->True];
	XththBar=XThetaThetaSymbol[syms,Conjugate->True];
	XthphBar=XThetaPhiSymbol[syms,Conjugate->True];
	XphphBar=XPhiPhiSymbol[syms,Conjugate->True];

	YBarSub[tt_]:=conjugateRefine[syms,YHarmonic[{l,m},{th[tt],ph[tt]},Phi->PhiO]]/.t->tt;
	YthBarSub[tt_]:=conjugateRefine[syms,YTheta[{l,m},{th[tt],ph[tt]},Phi->PhiO]]/.t->tt;
	YphBarSub[tt_]:=conjugateRefine[syms,YPhi[{l,m},{th[tt],ph[tt]},Phi->PhiO]]/.t->tt;
	YththBarSub[tt_]:=conjugateRefine[syms,YThetaTheta[{l,m},{th[tt],ph[tt]},Phi->PhiO]]/.t->tt;
	YthphBarSub[tt_]:=conjugateRefine[syms,YThetaPhi[{l,m},{th[tt],ph[tt]},Phi->PhiO]]/.t->tt;
	YphphBarSub[tt_]:=conjugateRefine[syms,YPhiPhi[{l,m},{th[tt],ph[tt]},Phi->PhiO]]/.t->tt;
	XthBarSub[tt_]:=conjugateRefine[syms,XTheta[{l,m},{th[tt],ph[tt]},Phi->PhiO]]/.t->tt;
	XphBarSub[tt_]:=conjugateRefine[syms,XPhi[{l,m},{th[tt],ph[tt]},Phi->PhiO]]/.t->tt;
	XththBarSub[tt_]:=conjugateRefine[syms,XThetaTheta[{l,m},{th[tt],ph[tt]},Phi->PhiO]]/.t->tt;
	XthphBarSub[tt_]:=conjugateRefine[syms,XThetaPhi[{l,m},{th[tt],ph[tt]},Phi->PhiO]]/.t->tt;
	XphphBarSub[tt_]:=conjugateRefine[syms,XPhiPhi[{l,m},{th[tt],ph[tt]},Phi->PhiO]]/.t->tt;

	Y=YSymbol[syms,Conjugate->False];
	Yth=YThetaSymbol[syms,Conjugate->False];
	Yph=YPhiSymbol[syms,Conjugate->False];
	Ythth=YThetaThetaSymbol[syms,Conjugate->False];
	Ythph=YThetaPhiSymbol[syms,Conjugate->False];
	Yphph=YPhiPhiSymbol[syms,Conjugate->False];
	Xth=XThetaSymbol[syms,Conjugate->False];
	Xph=XPhiSymbol[syms,Conjugate->False];
	Xthth=XThetaThetaSymbol[syms,Conjugate->False];
	Xthph=XThetaPhiSymbol[syms,Conjugate->False];
	Xphph=XPhiPhiSymbol[syms,Conjugate->False];

	YSub[tt_]:=YHarmonic[{l,m},{th[tt],ph[tt]},Phi->PhiO]/.t->tt;
	YthSub[tt_]:=YTheta[{l,m},{th[tt],ph[tt]},Phi->PhiO]/.t->tt;
	YphSub[tt_]:=YPhi[{l,m},{th[tt],ph[tt]},Phi->PhiO]/.t->tt;
	YththSub[tt_]:=YThetaTheta[{l,m},{th[tt],ph[tt]},Phi->PhiO]/.t->tt;
	YthphSub[tt_]:=YThetaPhi[{l,m},{th[tt],ph[tt]},Phi->PhiO]/.t->tt;
	YphphSub[tt_]:=YPhiPhi[{l,m},{th[tt],ph[tt]},Phi->PhiO]/.t->tt;
	XthSub[tt_]:=XTheta[{l,m},{th[tt],ph[tt]},Phi->PhiO]/.t->tt;
	XphSub[tt_]:=XPhi[{l,m},{th[tt],ph[tt]},Phi->PhiO]/.t->tt;
	XththSub[tt_]:=XThetaTheta[{l,m},{th[tt],ph[tt]},Phi->PhiO]/.t->tt;
	XthphSub[tt_]:=XThetaPhi[{l,m},{th[tt],ph[tt]},Phi->PhiO]/.t->tt;
	XphphSub[tt_]:=XPhiPhi[{l,m},{th[tt],ph[tt]},Phi->PhiO]/.t->tt;

	subs={YBar[t]->YBarSub[t],
		Derivative[n_][YBar][t]:>D[YBarSub[t],{t,n}],
		YthBar->YthBarSub,
		Derivative[n_][YthBar][t]:>D[YthBarSub[t],{t,n}],
		YphBar->YphBarSub,
		Derivative[n_][YphBar][t]:>D[YphBarSub[t],{t,n}],
		YththBar->YththBarSub,
		Derivative[n_][YththBar][t]:>D[YththBarSub[t],{t,n}],
		YthphBar->YthphBarSub,
		Derivative[n_][YthphBar][t]:>D[YthphBarSub[t],{t,n}],
		YphphBar->YphphBarSub,
		Derivative[n_][YphphBar][t]:>D[YphphBarSub[t],{t,n}],
		XthBar->XthBarSub,
		Derivative[n_][XthBar][t]:>D[XthBarSub[t],{t,n}],
		XphBar->XphBarSub,
		Derivative[n_][XphBar][t]:>D[XphBarSub[t],{t,n}],
		XththBar->XththBarSub,
		Derivative[n_][XththBar][t]:>D[XththBarSub[t],{t,n}],
		XthphBar->XthphBarSub,
		Derivative[n_][XthphBar][t]:>D[XthphBarSub[t],{t,n}],
		XphphBar->XphphBarSub,
		Derivative[n_][XphphBar][t]:>D[XphphBarSub[t],{t,n}],
		Y[t]->YSub[t],
		Derivative[n_][Y][t]:>D[YSub[t],{t,n}],
		Yth->YthSub,
		Derivative[n_][Yth][t]:>D[YthSub[t],{t,n}],
		Yph->YphSub,
		Derivative[n_][Yph][t]:>D[YphSub[t],{t,n}],
		Ythth->YththSub,
		Derivative[n_][Ythth][t]:>D[YththSub[t],{t,n}],
		Ythph->YthphSub,
		Derivative[n_][Ythph][t]:>D[YthphSub[t],{t,n}],
		Yphph->YphphSub,
		Derivative[n_][Yphph][t]:>D[YphphSub[t],{t,n}],
		Xth->XthSub,
		Derivative[n_][Xth][t]:>D[XthSub[t],{t,n}],
		Xph->XphSub,
		Derivative[n_][Xph][t]:>D[XphSub[t],{t,n}],
		Xthth->XththSub,
		Derivative[n_][Xthth][t]:>D[XththSub[t],{t,n}],
		Xthph->XthphSub,
		Derivative[n_][Xthph][t]:>D[XthphSub[t],{t,n}],
		Xphph->XphphSub,
		Derivative[n_][Xphph][t]:>D[XphphSub[t],{t,n}]};
	
	asmps={l\[Element]Integers,m\[Element]Integers,(l+m)!>=1,(l-m)!>=1,(1+2l)>=1,ph[t]\[Element]Reals,Derivative[_][ph][t]\[Element]Reals,0<=th[t]<=\[Pi],-1<=Cos[th[t]]<=1,
			Derivative[_][th][t]\[Element]Reals,LegendreP[_,_,Cos[th[t]]]\[Element]Reals};
	refined=Refine[expr/.subs,asmps];

	If[evalTh,
		refined/.{th[t]->\[Pi]/2,Derivative[_][th][t]->0},
		refined
	]
];
reDef@
ExpandHarmonics[expr_,opts:OptionsPattern[]]:=ExpandHarmonics[DefaultSymbols[],expr,opts]


def@
conjugateRefine[syms_Association,expr_]:=
Module[{l,m,th,php,t,asmps},

	t=TSymbol[syms];
	l=LSymbol[syms];
	m=MSymbol[syms];
	th=ThetaSymbol[syms];
	php=PhiPSymbol[syms];

	asmps={l\[Element]Integers,m\[Element]Integers,(l+m)!>=1,(l-m)!>=1,(1+2l)>=1,php[t]\[Element]Reals,Derivative[_][php][t]\[Element]Reals,0<=th[t]<=\[Pi],-1<=Cos[th[t]]<=1,
			Derivative[_][th][t]\[Element]Reals,LegendreP[_,_,Cos[th[t]]]\[Element]Reals};

	Refine[Distribute@Conjugate[expr],asmps]
]


End[];

EndPackage[];
