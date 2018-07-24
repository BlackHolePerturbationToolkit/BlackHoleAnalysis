(* ::Package:: *)

BeginPackage["BlackHoleAnalysis`Discontinuities`",
				{"BlackHoleAnalysis`Utils`",
					"BlackHoleAnalysis`Coordinates`",
					"BlackHoleAnalysis`Symbols`",
					"BlackHoleAnalysis`OverloadedSymbols`",
					"BlackHoleAnalysis`AnalyticTools`",
					"BlackHoleAnalysis`Fields`",
					"BlackHoleAnalysis`Labels`",
					"BlackHoleAnalysis`ValidityTests`",
					"BlackHoleAnalysis`Harmonics`",
					"GeneralUtilities`"}];


(*******************************************************************)
(* Public functions *)
(*******************************************************************)
DiscontinuitiesFunction::usage="DiscontinuitiesFunction[label] returns the function that provides \
discontinuities for the field associated with label.";
MasterFunctionDiscontinuities::usage="MasterFunctionDiscontinuities[] returns an Association with discontinuities \
in the master function and its first three r derivatives.
MasterFunctionDiscontinuities[{m,n}] returns an Association with discontinuities \
in the m-th time derivative and n-th r derivative of the master function.";

HttDiscontinuities::usage="HttDiscontinuities[] returns an Association with discontinuities in htt and its derivatives.
HttDiscontinuities[{m,n}] returns an Association with discontinuities in the m-th time derivative and n-th r derivative of htt.";
HtrDiscontinuities::usage="HtrDiscontinuities[] returns an Association with discontinuities in htr and its derivatives.
HtrDiscontinuities[{m,n}] returns an Association with discontinuities in the m-th time derivative and n-th r derivative of htr.";
HrrDiscontinuities::usage="HrrDiscontinuities[] returns an Association with discontinuities in hrr and its derivatives.
HrrDiscontinuities[{m,n}] returns an Association with discontinuities in the m-th time derivative and n-th r derivative of hrr.";
JtDiscontinuities::usage="JtDiscontinuities[] returns an Association with discontinuities in jt and its derivatives.
JtDiscontinuities[{m,n}] returns an Association with discontinuities in the m-th time derivative and n-th r derivative of jt.";
JrDiscontinuities::usage="JrDiscontinuities[] returns an Association with discontinuities in jr and its derivatives.
JrDiscontinuities[{m,n}] returns an Association with discontinuities in the m-th time derivative and n-th r derivative of jr.";
KDiscontinuities::usage="KDiscontinuities[] returns an Association with discontinuities in K and its derivatives.
KDiscontinuities[{m,n}] returns an Association with discontinuities in the m-th time derivative and n-th r derivative of K.";
GDiscontinuities::usage="GDiscontinuities[] returns an Association with discontinuities in G and its derivatives.
GDiscontinuities[{m,n}] returns an Association with discontinuities in the m-th time derivative and n-th r derivative of G.";

HtDiscontinuities::usage="HtDiscontinuities[] returns an Association with discontinuities in ht and its derivatives.
HtDiscontinuities[{m,n}] returns an Association with discontinuities in the m-th time derivative and n-th r derivative of ht.";
HrDiscontinuities::usage="HrDiscontinuities[] returns an Association with discontinuities in hr and its derivatives.
HrDiscontinuities[{m,n}] returns an Association with discontinuities in the m-th time derivative and n-th r derivative of hr.";
H2Discontinuities::usage="H2Discontinuities[] returns an Association with discontinuities in h2 and its derivatives.
H2Discontinuities[{m,n}] returns an Association with discontinuities in the m-th time derivative and n-th r derivative of h2.";

(*XiEvenDiscontinuities::usage="XiEvenDiscontinuities[] returns an Association with discontinuities in the gauge vector amplitude xiEven and its derivatives.
XiEvenDiscontinuities[{m,n}] returns an Association with discontinuities in the m-th time derivative and n-th r derivative of xiEven.";
XiEvenTDiscontinuities::usage="XiEvenTDiscontinuities[] returns an Association with discontinuities in the gauge vector amplitude xiEvenT and its derivatives.
XiEvenTDiscontinuities[{m,n}] returns an Association with discontinuities in the m-th time derivative and n-th r derivative of xiEvenT.";
XiEvenRDiscontinuities::usage="XiEvenRDiscontinuities[] returns an Association with discontinuities in the gauge vector amplitude xiEvenR and its derivatives.
XiEvenRDiscontinuities[{m,n}] returns an Association with discontinuities in the m-th time derivative and n-th r derivative of xiEvenR.";
XiOddDiscontinuities::usage="XiOddDiscontinuities[] returns an Association with discontinuities in the gauge vector amplitude xiOdd and its derivatives.
XiOddDiscontinuities[{m,n}] returns an Association with discontinuities in the m-th time derivative and n-th r derivative of xiOdd.";*)


Begin["`Private`"];


Options[mpRadDiscs]={"Gauge"->"RWZ"};
Options[mpMonopoleDiscs]=Options[mpRadDiscs];
Options[mpDipoleDiscs]=Options[mpRadDiscs];
Options[mpDiscs]=Join[Options[mpRadDiscs],{"Mode"->"Radiative","Labels"->True}];


Options[MasterFunctionDiscontinuities]={"Parity"->"Even","Labels"->True};
DocumentationBuilder`OptionDescriptions["MasterFunctionDiscontinuities"] = 
{
    "Parity" -> "The discontinuities in the ZM (\"Even\") or CPM (\"Odd\") master function",
    "Labels" -> "Boolean specifying whether the Keys of the Association should be formatted labels or \
the order of discontinuity (-1 for jump, -2 for \[Delta], -3 for \[Delta]', etc.)"
};

Options[HttDiscontinuities]=Options[mpDiscs];
DocumentationBuilder`OptionDescriptions["HttDiscontinuities"] = 
{
    "Labels" -> "Boolean specifying whether the Keys of the Association should be formatted labels or \
the order of discontinuity (-1 for jump, -2 for \[Delta], -3 for \[Delta]', etc.)",
	"Mode" -> "Discontinuities for which class of modes (\"Radiative\", \"Dipole\", or \"Monopole\")",
	"Gauge" -> "Gauge of the metric perturbation amplitudes (\"RWZ\", \"Lorenz\", or \"ModRWZ\")"
};
Options[HtrDiscontinuities]=Options[HttDiscontinuities];
DocumentationBuilder`OptionDescriptions["HtrDiscontinuities"] = DocumentationBuilder`OptionDescriptions["HttDiscontinuities"];
Options[HrrDiscontinuities]=Options[HttDiscontinuities];
DocumentationBuilder`OptionDescriptions["HrrDiscontinuities"] = DocumentationBuilder`OptionDescriptions["HttDiscontinuities"];
Options[JtDiscontinuities]=Options[HttDiscontinuities];
DocumentationBuilder`OptionDescriptions["JtDiscontinuities"] = DocumentationBuilder`OptionDescriptions["HttDiscontinuities"];
Options[JrDiscontinuities]=Options[HttDiscontinuities];
DocumentationBuilder`OptionDescriptions["JrDiscontinuities"] = DocumentationBuilder`OptionDescriptions["HttDiscontinuities"];
Options[KDiscontinuities]=Options[HttDiscontinuities];
DocumentationBuilder`OptionDescriptions["KDiscontinuities"] = DocumentationBuilder`OptionDescriptions["HttDiscontinuities"];
Options[GDiscontinuities]=Options[HttDiscontinuities];
DocumentationBuilder`OptionDescriptions["GDiscontinuities"] = DocumentationBuilder`OptionDescriptions["HttDiscontinuities"];
Options[HtDiscontinuities]=Options[HttDiscontinuities];
DocumentationBuilder`OptionDescriptions["HtDiscontinuities"] = DocumentationBuilder`OptionDescriptions["HttDiscontinuities"];
Options[HrDiscontinuities]=Options[HttDiscontinuities];
DocumentationBuilder`OptionDescriptions["HrDiscontinuities"] = DocumentationBuilder`OptionDescriptions["HttDiscontinuities"];
Options[H2Discontinuities]=Options[HttDiscontinuities];
DocumentationBuilder`OptionDescriptions["H2Discontinuities"] = DocumentationBuilder`OptionDescriptions["HttDiscontinuities"];

Options[xiDiscs]={"InitialGauge"->"RWZ","FinalGauge"->"ModRWZ","Labels"->True};
Options[XiEvenDiscontinuities]=Options[xiDiscs];
Options[XiEvenTDiscontinuities]=Options[XiEvenDiscontinuities];
Options[XiEvenRDiscontinuities]=Options[XiEvenDiscontinuities];
Options[XiOddDiscontinuities]=Options[XiEvenDiscontinuities];


def@
DiscontinuitiesFunction[label_String]:=
Module[{assoc},

	assoc=Association[
		MasterFunctionLabel[]->MasterFunctionDiscontinuities,
		HttLabel[]->HttDiscontinuities,
		HtrLabel[]->HtrDiscontinuities,
		HrrLabel[]->HrrDiscontinuities,
		JtLabel[]->JtDiscontinuities,
		JrLabel[]->JrDiscontinuities,
		KLabel[]->KDiscontinuities,
		GLabel[]->GDiscontinuities,
		HtLabel[]->HtDiscontinuities,
		HrLabel[]->HrDiscontinuities,
		H2Label[]->H2Discontinuities,
		XiEvenLabel[]->XiEvenDiscontinuities,
		XiEvenTLabel[]->XiEvenTDiscontinuities,
		XiEvenRLabel[]->XiEvenRDiscontinuities,
		XiOddLabel[]->XiOddDiscontinuities];


	assoc[label]
]


parityQ[x_]:=x==="Even" || x==="Odd" || x==="RW";
labelsQ=BooleanQ;


def@
MasterFunctionDiscontinuities[syms_Association,{td_,sd_},opts:OptionsPattern[]]:=
Module[{optionsRules,parity,t,labels,
		rp,la,M,En,JJ,mu,YBar,YphiBar,dl,
		YphiphiBar,XphiBar,XphiphiBar,discList,func},

	optionsRules = {"Parity" -> parityQ,
					"Labels"->labelsQ};
	
	TestOptions[optionsRules,{opts}];
	
	parity=OptionValue[Parity];
	labels=OptionValue[Labels];

	t=TSymbol[syms];
	rp=RpSymbol[syms];
	mu=ParticleMassSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	En=SpecificEnergySymbol[syms];
	JJ=SpecificAngularMomentumSymbol[syms];
	la=LambdaOfL[syms];
	YBar=YSymbol[syms,Conjugate->True];
	YphiBar=YPhiSymbol[syms,Conjugate->True];
	YphiphiBar=YPhiPhiSymbol[syms,Conjugate->True];
	XphiBar=XPhiSymbol[syms,Conjugate->True];
	XphiphiBar=XPhiPhiSymbol[syms,Conjugate->True];
	func=MasterFunction[syms,Parity->parity,ReturnSymbol->True];

	discList=
	Switch[parity,
		"Even",
			{-1->(8 En mu \[Pi] rp[t] YBar[t])/((1+la) (3 M+la rp[t])),
				0->(8 En mu \[Pi] (6 M^2+3 la M rp[t]+la (1+la) rp[t]^2) YBar[t])/((1+la) (2 M-rp[t]) (3 M+la rp[t])^2)+(8 En JJ^2 mu \[Pi] YphiphiBar[t])/(la (1+la) (2 M-rp[t]) (JJ^2+rp[t]^2)),
				1->-((8 En mu \[Pi] (3 (-3+5 la) M^3+6 (-3+la) la M^2 rp[t]+3 (-1+la) la^2 M rp[t]^2-2 la^2 (1+la) rp[t]^3) YBar[t])/((1+la) (-2 M+rp[t])^2 (3 M+la rp[t])^3))
					-(8 En JJ^2 mu \[Pi] (15 JJ^4 M^2+JJ^4 (-9+7 la) M rp[t]+(-4 JJ^4 la+48 JJ^2 M^2) rp[t]^2+JJ^2 (-27+6 En^2+20 la) M rp[t]^3+((-11+2 En^2) JJ^2 la+33 M^2) rp[t]^4+(-18+18 En^2+13 la) M rp[t]^5+(-7+6 En^2) la rp[t]^6) YphiphiBar[t])/(la (1+la) rp[t] (-2 M+rp[t])^2 (3 M+la rp[t]) (JJ^2+rp[t]^2)^3)
					+(16 En^2 JJ mu \[Pi] rp[t]^2 YphiBar[t] Derivative[1][rp][t])/((1+la) (-2 M+rp[t])^3 (JJ^2+rp[t]^2))
					+(16 En^3 JJ^2 mu \[Pi] rp[t]^5 Derivative[1][rp][t] Derivative[1][YphiphiBar][t])/(la (1+la) (-2 M+rp[t])^4 (JJ^2+rp[t]^2)^2),
				2->(8 En mu \[Pi] (405 JJ^4 (1+la) M^5+9 JJ^4 (-27+42 la+53 la^2) M^4 rp[t]+9 JJ^2 M^3 (JJ^2 la (-39+14 la+29 la^2)+36 (1+la) M^2) rp[t]^2+3 JJ^2 M^2 (JJ^2 la^2 (-72-19 la+17 la^2)+3 (-27+39 la+34 la^2+18 En^2 (1+la)) M^2) rp[t]^3-3 M (JJ^4 la^3 (11+3 la-2 la^2)-6 JJ^2 la (-21+14 la+11 la^2+12 En^2 (1+la)) M^2+27 (1+la) M^4) rp[t]^4-3 (JJ^4 la^3 (2+3 la+la^2)-2 JJ^2 la^2 (-45-4 la+5 la^2+18 En^2 (1+la)) M^2+(54 En^2 (1+la)+3 la (3+19 la)) M^4) rp[t]^5-3 la M (-2 JJ^2 la^2 (-5+2 la+la^2+4 En^2 (1+la))+3 (3-14 la+7 la^2+24 En^2 (1+la)) M^2) rp[t]^6-la^2 (-JJ^2 la (1+la) (-12+(-3+2 En^2) la)+3 (18-11 la+7 la^2+36 En^2 (1+la)) M^2) rp[t]^7-3 la^3 (-1-7 la+8 En^2 (1+la)) M rp[t]^8-2 la^3 (1+la) (3+En^2 la) rp[t]^9) YBar[t])/((1+la) rp[t] (-2 M+rp[t])^3 (3 M+la rp[t])^4 (JJ^2+rp[t]^2)^2)-(8 En JJ^2 mu \[Pi] (405 JJ^8 M^4+45 JJ^8 (-9+8 la) M^3 rp[t]+3 JJ^6 M^2 (JJ^2 (36-111 la+35 la^2)+612 M^2) rp[t]^2+3 JJ^6 M (JJ^2 la (27-29 la+2 la^2)+6 (-105+16 En^2+90 la) M^2) rp[t]^3+3 (-JJ^8 (-6+la) la^2+JJ^6 (171-516 la+152 la^2+En^2 (-36+86 la)) M^2+1206 JJ^4 M^4) rp[t]^4+3 JJ^4 M (JJ^2 la (129-130 la+8 la^2+En^2 (-26+24 la))+12 (-105+28 En^2+85 la) M^2) rp[t]^5+2 (JJ^6 la^2 (-6 (-7+la)+En^2 (-7+3 la))+3 JJ^4 (171-501 la+133 la^2+En^2 (-72+149 la)) M^2+1674 JJ^2 M^4) rp[t]^6+3 JJ^2 M (JJ^2 la (255-236 la+12 la^2+2 En^2 (-53+40 la))+6 (-195+124 En^2+150 la) M^2) rp[t]^7+(JJ^4 la^2 (159-18 la+2 En^2 (-29+9 la))+3 JJ^2 (En^2 (-360+578 la)+9 (35-100 la+24 la^2)) M^2+1161 M^4) rp[t]^8+3 M (JJ^2 la (231-198 la+8 la^2+2 En^2 (-127+64 la))+(-405+504 En^2+300 la) M^2) rp[t]^9+(2 JJ^2 la^2 (69-6 la+En^2 (-67+9 la))+3 (108+144 En^4-303 la+67 la^2+6 En^2 (-42+61 la)) M^2) rp[t]^10+3 la (78+96 En^4-63 la+2 la^2+6 En^2 (-29+12 la)) M rp[t]^11+3 (15+16 En^4+2 En^2 (-15+la)-la) la^2 rp[t]^12) YphiphiBar[t])/(la (1+la) rp[t]^2 (-2 M+rp[t])^3 (3 M+la rp[t])^2 (JJ^2+rp[t]^2)^5)+(16 En^2 JJ mu \[Pi] rp[t] (15 JJ^4 M^2+JJ^4 (-12+7 la) M rp[t]+(-5 JJ^4 la+36 JJ^2 M^2) rp[t]^2+JJ^2 (-27+6 En^2+16 la) M rp[t]^3+((-11+2 En^2) JJ^2 la+21 M^2) rp[t]^4+3 (-5+6 En^2+3 la) M rp[t]^5+6 (-1+En^2) la rp[t]^6) YphiBar[t] Derivative[1][rp][t])/((1+la) (-2 M+rp[t])^4 (3 M+la rp[t]) (JJ^2+rp[t]^2)^3)+(16 En^3 JJ^2 mu \[Pi] rp[t]^5 (2 JJ^4 (9-2 la) M^2+(JJ^4 (-9+10 la) M-72 JJ^2 M^3) rp[t]+4 JJ^2 (-JJ^2 la+(27+3 En^2-8 la) M^2) rp[t]^2+4 M (JJ^2 (-9+(11+En^2) la)-18 M^2) rp[t]^3-2 (7 JJ^2 la+(-45+30 En^2+14 la) M^2) rp[t]^4+(-27+34 la-4 En^2 (-9+5 la)) M rp[t]^5+2 (-5+6 En^2) la rp[t]^6) Derivative[1][rp][t] Derivative[1][YphiphiBar][t])/(la (1+la) (-2 M+rp[t])^6 (3 M+la rp[t]) (JJ^2+rp[t]^2)^4)-(8 En^3 JJ^2 mu \[Pi] rp[t]^5 (6 JJ^2 M-3 JJ^2 rp[t]+6 M rp[t]^2+(-3+4 En^2) rp[t]^3) YphiphiBar''[t])/(la (1+la) (-2 M+rp[t])^5 (JJ^2+rp[t]^2)^3)
			},
		
		"Odd",
			{-1->(8 JJ mu \[Pi] XphiBar[t])/((la+la^2) rp[t]),
				0->-((8 JJ mu \[Pi] XphiBar[t])/(la (1+la) rp[t]^2))-(8 En JJ^2 mu \[Pi] rp[t] XphiphiBar[t] Derivative[1][rp][t])/(la (1+la) (-2 M+rp[t])^2 (JJ^2+rp[t]^2)),
				1->(16 JJ mu \[Pi] (4 JJ^2 M^2-4 JJ^2 M rp[t]+(JJ^2+4 M^2) rp[t]^2-4 M rp[t]^3+(1+En^2 la) rp[t]^4) XphiBar[t])/(la (1+la) rp[t]^3 (-2 M+rp[t])^2 (JJ^2+rp[t]^2))
					+(8 En JJ^2 mu \[Pi] (5 JJ^4 M-3 JJ^4 rp[t]+12 JJ^2 M rp[t]^2+(-7+2 En^2) JJ^2 rp[t]^3+7 M rp[t]^4+(-4+6 En^2) rp[t]^5) XphiphiBar[t] Derivative[1][rp][t])/(la (1+la) (2 M-rp[t])^3 (JJ^2+rp[t]^2)^3)
					-(8 En JJ^2 mu \[Pi] rp[t] (2 JJ^2 M-JJ^2 rp[t]+2 M rp[t]^2+(-1+2 En^2) rp[t]^3) Derivative[1][XphiphiBar][t])/(la (1+la) (2 M-rp[t])^3 (JJ^2+rp[t]^2)^2),
				2->(16 JJ mu \[Pi] (24 JJ^6 M^3-36 JJ^6 M^2 rp[t]+18 JJ^4 M (JJ^2+4 M^2) rp[t]^2-3 (JJ^6+36 JJ^4 M^2) rp[t]^3+9 JJ^2 M (JJ^2 (6+En^2 la)+8 M^2) rp[t]^4-3 (JJ^4 (3+2 En^2 la)+36 JJ^2 M^2) rp[t]^5+6 M (JJ^2 (9+4 En^2 la)+4 M^2) rp[t]^6-(JJ^2 (9+15 En^2 la-2 En^4 la)+36 M^2) rp[t]^7+3 (6+5 En^2 la) M rp[t]^8+(-3-9 En^2 la+6 En^4 la) rp[t]^9) XphiBar[t])/(la (1+la) rp[t]^4 (-2 M+rp[t])^3 (JJ^2+rp[t]^2)^3)-(8 En JJ^2 mu \[Pi] (33 JJ^8 M^2-37 JJ^8 M rp[t]+(11 JJ^8+144 JJ^6 M^2) rp[t]^2+18 (-9+2 En^2) JJ^6 M rp[t]^3+2 (JJ^6 (24+En^2 (-7+3 la))+123 JJ^4 M^2) rp[t]^4+12 (-23+11 En^2) JJ^4 M rp[t]^5+(JJ^4 (81+2 En^2 (-29+9 la))+192 JJ^2 M^2) rp[t]^6+2 (-107+114 En^2) JJ^2 M rp[t]^7+(2 JJ^2 (31+En^2 (-55+9 la))+57 M^2) rp[t]^8+3 (-21+44 En^2) M rp[t]^9+6 (3+8 En^4+En^2 (-11+la)) rp[t]^10) XphiphiBar[t] Derivative[1][rp][t])/(la (1+la) rp[t] (-2 M+rp[t])^4 (JJ^2+rp[t]^2)^5)-(8 En JJ^2 mu \[Pi] (12 JJ^6 M^3-28 JJ^6 M^2 rp[t]+(19 JJ^6 M+60 JJ^4 M^3) rp[t]^2-4 (JJ^6+30 JJ^4 M^2) rp[t]^3+JJ^2 M ((75-16 En^2) JJ^2+84 M^2) rp[t]^4+JJ^2 ((-15+8 En^2) JJ^2+12 (-13+8 En^2) M^2) rp[t]^5+M ((93-128 En^2-8 En^4) JJ^2+36 M^2) rp[t]^6+2 ((-9+20 En^2) JJ^2+16 (-2+3 En^2) M^2) rp[t]^7+(37-112 En^2+40 En^4) M rp[t]^8+(-7+32 En^2-24 En^4) rp[t]^9) Derivative[1][XphiphiBar][t])/(la (1+la) (-2 M+rp[t])^5 (JJ^2+rp[t]^2)^4)-(8 En^3 JJ^2 mu \[Pi] rp[t]^6 (2 JJ^2 M-JJ^2 rp[t]+2 M rp[t]^2+(-1+4 En^2) rp[t]^3) Derivative[1][rp][t] XphiphiBar''[t])/(la (1+la) (-2 M+rp[t])^6 (JJ^2+rp[t]^2)^3)
			},		
		"RW",
			{-1->(4 En JJ^2 mu \[Pi] XphiphiBar[t])/(la (1+la) rp[t] (JJ^2+rp[t]^2)),
				0->(4 En^2 mu \[Pi] (-((JJ^2 rp[t] (-2 M+rp[t])^2 XphiphiBar[t])/(En (la+la^2)))+(JJ^2 (-2 M+rp[t])^2 (5 JJ^2 M-JJ^2 rp[t]+3 M rp[t]^2) XphiphiBar[t])/(En la (1+la) (JJ^2+rp[t]^2))+(2 En (JJ-(2 JJ M)/rp[t])^2 rp[t]^5 (JJ^2+3 rp[t]^2) (1-((-2 M+rp[t]) (JJ^2+rp[t]^2))/(En^2 rp[t]^3)) XphiphiBar[t])/(la (1+la) (JJ^2+rp[t]^2)^2)-(2 JJ rp[t]^3 Derivative[1][rp][t] (-(2 M-rp[t]) XphiBar[t]+(En JJ rp[t]^3 Derivative[1][XphiphiBar][t])/(la (JJ^2+rp[t]^2))))/(1+la)))/(rp[t]^2 (-2 M+rp[t])^3 (JJ^2+rp[t]^2)),
				1->(4 En JJ^2 mu \[Pi] (99 JJ^8 M^2+4 JJ^8 (-21+2 la) M rp[t]+(JJ^8 (17-4 la)+456 JJ^6 M^2) rp[t]^2+2 JJ^6 (-195+22 En^2+16 la) M rp[t]^3+2 (JJ^6 (-8 (-5+la)+En^2 (-7+3 la))+417 JJ^4 M^2) rp[t]^4+2 JJ^4 (-363+86 En^2+24 la) M rp[t]^5+(JJ^4 (153-24 la+2 En^2 (-29+9 la))+696 JJ^2 M^2) rp[t]^6+2 JJ^2 (-309+166 En^2+16 la) M rp[t]^7+(2 JJ^2 (67-8 la+En^2 (-67+9 la))+219 M^2) rp[t]^8+2 (-99+102 En^2+4 la) M rp[t]^9+(44+48 En^4+6 En^2 (-15+la)-4 la) rp[t]^10) XphiphiBar[t])/(la (1+la) rp[t]^3 (-2 M+rp[t])^2 (JJ^2+rp[t]^2)^5)-(8 En^2 JJ mu \[Pi] (11 JJ^4 M-5 JJ^4 rp[t]+24 JJ^2 M rp[t]^2+(-11+2 En^2) JJ^2 rp[t]^3+13 M rp[t]^4+6 (-1+En^2) rp[t]^5) XphiBar[t] Derivative[1][rp][t])/((1+la) (-2 M+rp[t])^3 (JJ^2+rp[t]^2)^3)-(16 En^3 JJ^2 mu \[Pi] rp[t]^3 (-6 JJ^4 M^2+7 JJ^4 M rp[t]-2 (JJ^4+12 JJ^2 M^2) rp[t]^2+2 (13+En^2) JJ^2 M rp[t]^3-(7 JJ^2+18 M^2) rp[t]^4+(19-10 En^2) M rp[t]^5+(-5+6 En^2) rp[t]^6) Derivative[1][rp][t] Derivative[1][XphiphiBar][t])/(la (1+la) (-2 M+rp[t])^5 (JJ^2+rp[t]^2)^4)+(4 En^3 JJ^2 mu \[Pi] rp[t]^4 (6 JJ^2 M-3 JJ^2 rp[t]+6 M rp[t]^2+(-3+4 En^2) rp[t]^3) XphiphiBar''[t])/(la (1+la) (-2 M+rp[t])^4 (JJ^2+rp[t]^2)^3),
				2->-(4 En JJ^2 mu \[Pi] (-1287 JJ^12 M^3-6 JJ^12 (-275+24 la) M^2 rp[t]-6 JJ^10 M (JJ^2 (113-24 la)+1422 M^2) rp[t]^2-3 JJ^10 (JJ^2 (-29+12 la)+(-3657+346 En^2+304 la) M^2) rp[t]^3+JJ^8 M (JJ^2 (-4527+En^2 (738-148 la)+912 la)-23895 M^2) rp[t]^4+JJ^8 (JJ^2 (585-228 la+2 En^2 (-54+37 la))-15 (-2055+422 En^2+160 la) M^2) rp[t]^5-2 JJ^6 M (JJ^2 (6390+90 En^4-1200 la+En^2 (-2277+446 la))+18420 M^2) rp[t]^6+JJ^6 (JJ^2 (1665-24 En^4 (-1+la)-600 la+En^2 (-684+446 la))-30 (-1595+506 En^2+112 la) M^2) rp[t]^7-3 JJ^4 M (8 JJ^2 (835+45 En^4-140 la+En^2 (-454+87 la))+11055 M^2) rp[t]^8-3 JJ^4 (JJ^2 (-885+En^2 (542-348 la)+56 En^4 (-1+la)+280 la)+20 (-726+355 En^2+44 la) M^2) rp[t]^9-4 JJ^2 M (JJ^2 (4635+312 En^4-660 la+En^2 (-4026+598 la))+4113 M^2) rp[t]^10-JJ^2 (2 JJ^2 (En^2 (1347-598 la)+30 (-42+11 la)+12 En^4 (7+15 la))+3 (-7305+5730 En^2+368 la) M^2) rp[t]^11-M (JJ^2 (9513+3336 En^4-1104 la+2 En^2 (-7023+674 la))+3477 M^2) rp[t]^12+(2 JJ^2 (666+240 En^6-156 En^4 (-3+la)-138 la+En^2 (-1353+337 la))-3 (-1567+1926 En^2+64 la) M^2) rp[t]^13-6 (347+498 En^4-32 la+En^2 (-837+50 la)) M rp[t]^14-6 (-50+80 En^6+En^2 (177-25 la)+16 En^4 (-13+la)+8 la) rp[t]^15) XphiphiBar[t])/(la (1+la) rp[t]^4 (-2 M+rp[t])^3 (JJ^2+rp[t]^2)^7)-(8 En^2 JJ mu \[Pi] (135 JJ^8 M^2-123 JJ^8 M rp[t]+3 (9 JJ^8+188 JJ^6 M^2) rp[t]^2+12 (-43+6 En^2) JJ^6 M rp[t]^3+2 (JJ^6 (57+En^2 (-11+la))+447 JJ^4 M^2) rp[t]^4+6 (-137+44 En^2) JJ^4 M rp[t]^5+3 (JJ^4 (61+2 En^2 (-15+la))+212 JJ^2 M^2) rp[t]^6+12 (-49+32 En^2) JJ^2 M rp[t]^7+3 (2 JJ^2 (22+En^2 (-25+la))+57 M^2) rp[t]^8+3 (-53+64 En^2) M rp[t]^9+2 (18+24 En^4+En^2 (-41+la)) rp[t]^10) XphiBar[t] Derivative[1][rp][t])/((1+la) rp[t] (-2 M+rp[t])^4 (JJ^2+rp[t]^2)^5)-(8 En^3 JJ^2 mu \[Pi] rp[t]^2 (372 JJ^8 M^4+12 JJ^8 (-59+2 la) M^3 rp[t]+3 JJ^6 M^2 (3 JJ^2 (57-4 la)+544 M^2) rp[t]^2+6 JJ^6 M (JJ^2 (-28+3 la)+4 (-133+10 En^2+4 la) M^2) rp[t]^3+(-3 JJ^8 (-7+la)+8 JJ^6 (297-18 la+En^2 (-31+4 la)) M^2+3384 JJ^4 M^4) rp[t]^4+2 JJ^4 M (JJ^2 (-399+En^2 (44-16 la)+36 la)+72 (-47+En^2+la) M^2) rp[t]^5+2 (JJ^6 (51-6 la+En^2 (-6+4 la))+3 JJ^4 (855+8 En^4-36 la+4 En^2 (-3+4 la)) M^2+1680 JJ^2 M^4) rp[t]^6+2 JJ^2 M (JJ^2 (-4 En^4+En^2 (12-48 la)+9 (-97+6 la))+12 (-283+62 En^2+4 la) M^2) rp[t]^7+3 (JJ^4 (75-6 la+En^2 (-4+8 la))-8 JJ^2 (20 En^4+En^2 (95-4 la)+6 (-36+la)) M^2+412 M^4) rp[t]^8+2 M (JJ^2 (-885+232 En^4+En^2 (612-48 la)+36 la)+6 (-209+132 En^2+2 la) M^2) rp[t]^9+(-12 JJ^2 (-19+8 En^4+En^2 (19-2 la)+la)+(1917+432 En^4-36 la+8 En^2 (-307+4 la)) M^2) rp[t]^10-2 (327+244 En^4-9 la+4 En^2 (-161+4 la)) M rp[t]^11+(84+144 En^4-3 la+4 En^2 (-57+2 la)) rp[t]^12) Derivative[1][rp][t] Derivative[1][XphiphiBar][t])/(la (1+la) (-2 M+rp[t])^7 (JJ^2+rp[t]^2)^6)+(4 En^3 JJ^2 mu \[Pi] rp[t]^3 (24 JJ^6 M^3+12 JJ^6 M^2 rp[t]-6 (5 JJ^6 M+28 JJ^4 M^3) rp[t]^2+JJ^4 (9 JJ^2+44 (9+4 En^2) M^2) rp[t]^3-2 JJ^2 M ((135+52 En^2) JJ^2+204 M^2) rp[t]^4+JJ^2 ((57+8 En^2) JJ^2+(756-128 En^2) M^2) rp[t]^5+(2 (-225+136 En^2+48 En^4) JJ^2 M-216 M^3) rp[t]^6+((87-104 En^2-24 En^4) JJ^2-4 (-93+76 En^2) M^2) rp[t]^7-2 (105-188 En^2+48 En^4) M rp[t]^8+(39-112 En^2+72 En^4) rp[t]^9) XphiphiBar''[t])/(la (1+la) (-2 M+rp[t])^6 (JJ^2+rp[t]^2)^5)-(16 En^5 JJ^2 mu \[Pi] rp[t]^9 (2 JJ^2 M-JJ^2 rp[t]+2 M rp[t]^2+(-1+2 En^2) rp[t]^3) Derivative[1][rp][t] XphiphiBar'''[t])/(la (1+la) (-2 M+rp[t])^7 (JJ^2+rp[t]^2)^4)
			}
	];

	dl=getModifiedDiscList[syms,discList,{td,sd}];
	
	Association[If[labels,dl/.getDiscFormatRules[syms,func,{td,sd}],dl]]
]
reDef@
MasterFunctionDiscontinuities[{td_,sd_},opts:OptionsPattern[]]:=MasterFunctionDiscontinuities[DefaultSymbols[],{td,sd},opts]
reDef@
MasterFunctionDiscontinuities[syms_Association,opts:OptionsPattern[]]:=MasterFunctionDiscontinuities[syms,{0,0},opts];
reDef@
MasterFunctionDiscontinuities[opts:OptionsPattern[]]:=MasterFunctionDiscontinuities[DefaultSymbols[],{0,0},opts];


def@HttDiscontinuities[syms_Association,{td_,sd_},opts:OptionsPattern[]]:=mpDiscs[syms,HttLabel[],{td,sd},opts];
reDef@
HttDiscontinuities[{td_,sd_},opts:OptionsPattern[]]:=HttDiscontinuities[DefaultSymbols[],{td,sd},opts]
reDef@
HttDiscontinuities[syms_Association,opts:OptionsPattern[]]:=HttDiscontinuities[syms,{0,0},opts];
reDef@
HttDiscontinuities[opts:OptionsPattern[]]:=HttDiscontinuities[DefaultSymbols[],{0,0},opts];


def@HtrDiscontinuities[syms_Association,{td_,sd_},opts:OptionsPattern[]]:=mpDiscs[syms,HtrLabel[],{td,sd},opts];
reDef@
HtrDiscontinuities[{td_,sd_},opts:OptionsPattern[]]:=HtrDiscontinuities[DefaultSymbols[],{td,sd},opts]
reDef@
HtrDiscontinuities[syms_Association,opts:OptionsPattern[]]:=HtrDiscontinuities[syms,{0,0},opts];
reDef@
HtrDiscontinuities[opts:OptionsPattern[]]:=HtrDiscontinuities[DefaultSymbols[],{0,0},opts];


def@HrrDiscontinuities[syms_Association,{td_,sd_},opts:OptionsPattern[]]:=mpDiscs[syms,HrrLabel[],{td,sd},opts];
reDef@
HrrDiscontinuities[{td_,sd_},opts:OptionsPattern[]]:=HrrDiscontinuities[DefaultSymbols[],{td,sd},opts]
reDef@
HrrDiscontinuities[syms_Association,opts:OptionsPattern[]]:=HrrDiscontinuities[syms,{0,0},opts];
reDef@
HrrDiscontinuities[opts:OptionsPattern[]]:=HrrDiscontinuities[DefaultSymbols[],{0,0},opts];


def@JtDiscontinuities[syms_Association,{td_,sd_},opts:OptionsPattern[]]:=mpDiscs[syms,JtLabel[],{td,sd},opts];
reDef@
JtDiscontinuities[{td_,sd_},opts:OptionsPattern[]]:=JtDiscontinuities[DefaultSymbols[],{td,sd},opts]
reDef@
JtDiscontinuities[syms_Association,opts:OptionsPattern[]]:=JtDiscontinuities[syms,{0,0},opts];
reDef@
JtDiscontinuities[opts:OptionsPattern[]]:=JtDiscontinuities[DefaultSymbols[],{0,0},opts];


def@JrDiscontinuities[syms_Association,{td_,sd_},opts:OptionsPattern[]]:=mpDiscs[syms,JrLabel[],{td,sd},opts];
reDef@
JrDiscontinuities[{td_,sd_},opts:OptionsPattern[]]:=JrDiscontinuities[DefaultSymbols[],{td,sd},opts]
reDef@
JrDiscontinuities[syms_Association,opts:OptionsPattern[]]:=JrDiscontinuities[syms,{0,0},opts];
reDef@
JrDiscontinuities[opts:OptionsPattern[]]:=JrDiscontinuities[DefaultSymbols[],{0,0},opts];


def@GDiscontinuities[syms_Association,{td_,sd_},opts:OptionsPattern[]]:=mpDiscs[syms,GLabel[],{td,sd},opts];
reDef@
GDiscontinuities[{td_,sd_},opts:OptionsPattern[]]:=GDiscontinuities[DefaultSymbols[],{td,sd},opts]
reDef@
GDiscontinuities[syms_Association,opts:OptionsPattern[]]:=GDiscontinuities[syms,{0,0},opts];
reDef@
GDiscontinuities[opts:OptionsPattern[]]:=GDiscontinuities[DefaultSymbols[],{0,0},opts];


def@KDiscontinuities[syms_Association,{td_,sd_},opts:OptionsPattern[]]:=mpDiscs[syms,KLabel[],{td,sd},opts];
reDef@
KDiscontinuities[{td_,sd_},opts:OptionsPattern[]]:=KDiscontinuities[DefaultSymbols[],{td,sd},opts]
reDef@
KDiscontinuities[syms_Association,opts:OptionsPattern[]]:=KDiscontinuities[syms,{0,0},opts];
reDef@
KDiscontinuities[opts:OptionsPattern[]]:=KDiscontinuities[DefaultSymbols[],{0,0},opts];


def@HtDiscontinuities[syms_Association,{td_,sd_},opts:OptionsPattern[]]:=mpDiscs[syms,HtLabel[],{td,sd},opts];
reDef@
HtDiscontinuities[{td_,sd_},opts:OptionsPattern[]]:=HtDiscontinuities[DefaultSymbols[],{td,sd},opts]
reDef@
HtDiscontinuities[syms_Association,opts:OptionsPattern[]]:=HtDiscontinuities[syms,{0,0},opts];
reDef@
HtDiscontinuities[opts:OptionsPattern[]]:=HtDiscontinuities[DefaultSymbols[],{0,0},opts];


def@HrDiscontinuities[syms_Association,{td_,sd_},opts:OptionsPattern[]]:=mpDiscs[syms,HrLabel[],{td,sd},opts];
reDef@
HrDiscontinuities[{td_,sd_},opts:OptionsPattern[]]:=HrDiscontinuities[DefaultSymbols[],{td,sd},opts]
reDef@
HrDiscontinuities[syms_Association,opts:OptionsPattern[]]:=HrDiscontinuities[syms,{0,0},opts];
reDef@
HrDiscontinuities[opts:OptionsPattern[]]:=HrDiscontinuities[DefaultSymbols[],{0,0},opts];


def@H2Discontinuities[syms_Association,{td_,sd_},opts:OptionsPattern[]]:=mpDiscs[syms,H2Label[],{td,sd},opts];
reDef@
H2Discontinuities[{td_,sd_},opts:OptionsPattern[]]:=H2Discontinuities[DefaultSymbols[],{td,sd},opts]
reDef@
H2Discontinuities[syms_Association,opts:OptionsPattern[]]:=H2Discontinuities[syms,{0,0},opts];
reDef@
H2Discontinuities[opts:OptionsPattern[]]:=H2Discontinuities[DefaultSymbols[],{0,0},opts];


gaugeQ[x_]:=MemberQ[{"RWZ","Lorenz","ModRWZ"},x];
modeQ[x_]:=MemberQ[{"Monopole","Dipole","Radiative"},x];


mpDiscTests={"Gauge"->gaugeQ};


fullMPDiscTests={"Labels"->labelsQ,"Gauge"->gaugeQ,"Mode"->modeQ};


def@
mpDiscs[syms_Association,label_String,{td_,sd_},opts:OptionsPattern[]]:=
Module[{mode,gauge,labels,dl,dlMod,mpSymbol},

	TestOptions[fullMPDiscTests,{opts}];
	
	gauge=OptionValue[Gauge];
	mode=OptionValue[Mode];
	labels=OptionValue[Labels];

	dl =
	Switch[mode,
		"Radiative",
		mpRadDiscs[syms,label,Gauge->gauge],

		"Dipole",
		mpDipoleDiscs[syms,label,Gauge->gauge],

		"Monopole",
		mpMonopoleDiscs[syms,label,Gauge->gauge]
	];

	dlMod=getModifiedDiscList[syms,dl,{td,sd}];
	
	mpSymbol=AmplitudeFunction[label][syms,Gauge->gauge,ReturnSymbol->True];
	Association[If[labels,dlMod/.getDiscFormatRules[syms,mpSymbol,{td,sd}],dlMod]]

]


def@
mpRadDiscs[syms_Association,label_String,opts:OptionsPattern[]]:=
Module[{gauge,t,rp,la,M,En,JJ,mu,YBar,YphiBar,YphiphiBar,XphiBar,XphiphiBar},

	TestOptions[mpDiscTests,{opts}];
	
	gauge=OptionValue[Gauge];

	t=TSymbol[syms];
	rp=RpSymbol[syms];
	mu=ParticleMassSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	En=SpecificEnergySymbol[syms];
	JJ=SpecificAngularMomentumSymbol[syms];
	la=LambdaOfL[syms];
	YBar=YSymbol[syms,Conjugate->True];
	YphiBar=YPhiSymbol[syms,Conjugate->True];
	YphiphiBar=YPhiPhiSymbol[syms,Conjugate->True];
	XphiBar=XPhiSymbol[syms,Conjugate->True];
	XphiphiBar=XPhiPhiSymbol[syms,Conjugate->True];

	Switch[label,
		HttLabel[],
		Switch[gauge,
			"Lorenz"|"ModRWZ",
				{-1->0,
					0->(8 En mu \[Pi] (2 M+(-1+2 En^2) rp[t]) YBar[t])/((2 M-rp[t]) (JJ^2+rp[t]^2))},
		
			"RWZ",
				{-2->-((8 JJ^2 mu \[Pi] (-2 M+rp[t]) (2 JJ^2 M-JJ^2 rp[t]+2 M rp[t]^2+(-1+En^2) rp[t]^3) YphiphiBar[t])/(En la (1+la) rp[t]^4 (JJ^2+rp[t]^2))),
					-1->-((8 En JJ^2 mu \[Pi] (6 JJ^4 M-3 JJ^4 rp[t]+18 JJ^2 M rp[t]^2+(-9+2 En^2) JJ^2 rp[t]^3+12 M rp[t]^4+6 (-1+En^2) rp[t]^5) YphiphiBar[t])/(la (1+la) rp[t]^2 (JJ^2+rp[t]^2)^3))-(16 En^2 JJ mu \[Pi] rp[t] YphiBar[t] Derivative[1][rp][t])/((1+la) (2 M-rp[t]) (JJ^2+rp[t]^2))+(16 En^3 JJ^2 mu \[Pi] rp[t]^4 Derivative[1][rp][t] Derivative[1][YphiphiBar][t])/(la (1+la) (-2 M+rp[t])^2 (JJ^2+rp[t]^2)^2),
					0->(8 En mu \[Pi] (4 JJ^4 M-2 JJ^4 rp[t]+2 JJ^2 M rp[t]^2+(-1+2 En^2) JJ^2 rp[t]^3-2 M rp[t]^4+(1-2 En^2) rp[t]^5) YBar[t])/(rp[t]^2 (-2 M+rp[t]) (JJ^2+rp[t]^2)^2)-(8 En JJ^2 mu \[Pi] (66 JJ^8 M^2+JJ^8 (-51+8 la) M rp[t]+(JJ^8 (9-4 la)+306 JJ^6 M^2) rp[t]^2+JJ^6 (-237+38 En^2+32 la) M rp[t]^3+2 (JJ^6 (21-8 la+En^2 (-5+3 la))+291 JJ^4 M^2) rp[t]^4+JJ^4 (-465+142 En^2+48 la) M rp[t]^5+(JJ^4 (87-24 la+2 En^2 (-19+9 la))+510 JJ^2 M^2) rp[t]^6+JJ^2 (-423+290 En^2+32 la) M rp[t]^7+2 (JJ^2 (42-8 la+En^2 (-53+9 la))+84 M^2) rp[t]^8+2 (-72+93 En^2+4 la) M rp[t]^9+(30+48 En^4+6 En^2 (-13+la)-4 la) rp[t]^10) YphiphiBar[t])/(la (1+la) rp[t]^3 (-2 M+rp[t]) (JJ^2+rp[t]^2)^5)+(16 En^2 JJ mu \[Pi] (8 JJ^4 M-3 JJ^4 rp[t]+18 JJ^2 M rp[t]^2+(-7+2 En^2) JJ^2 rp[t]^3+10 M rp[t]^4+(-4+6 En^2) rp[t]^5) YphiBar[t] Derivative[1][rp][t])/((1+la) (-2 M+rp[t])^2 (JJ^2+rp[t]^2)^3)+(16 En^3 JJ^2 mu \[Pi] rp[t]^3 (-6 JJ^4 M^2+7 JJ^4 M rp[t]-2 (JJ^4+18 JJ^2 M^2) rp[t]^2+2 (19+2 En^2) JJ^2 M rp[t]^3-10 (JJ^2+3 M^2) rp[t]^4+(31-20 En^2) M rp[t]^5+4 (-2+3 En^2) rp[t]^6) Derivative[1][rp][t] Derivative[1][YphiphiBar][t])/(la (1+la) (-2 M+rp[t])^4 (JJ^2+rp[t]^2)^4)-(8 En^3 JJ^2 mu \[Pi] rp[t]^4 (6 JJ^2 M-3 JJ^2 rp[t]+6 M rp[t]^2+(-3+4 En^2) rp[t]^3) YphiphiBar''[t])/(la (1+la) (-2 M+rp[t])^3 (JJ^2+rp[t]^2)^3)}	
		],

		HtrLabel[],
		Switch[gauge,
			"Lorenz"|"ModRWZ",
				{-1->0,
					0->(16 En^3 mu \[Pi] rp[t]^3 YBar[t] Derivative[1][rp][t])/((-2 M+rp[t])^3 (JJ^2+rp[t]^2))},
		
			"RWZ",
				{-2->-((8 En JJ^2 mu \[Pi] rp[t] YphiphiBar[t] Derivative[1][rp][t])/(la (1+la) (2 M-rp[t]) (JJ^2+rp[t]^2))),
					-1->(8 JJ mu \[Pi] (2 JJ^2 M-JJ^2 rp[t]+2 M rp[t]^2+(-1+2 En^2) rp[t]^3) YphiBar[t])/((1+la) (2 M-rp[t]) rp[t]^2 (JJ^2+rp[t]^2))+(8 En JJ^2 mu \[Pi] (4 JJ^4 M-2 JJ^4 rp[t]+10 JJ^2 M rp[t]^2+(-5+2 En^2) JJ^2 rp[t]^3+6 M rp[t]^4+(-3+6 En^2) rp[t]^5) YphiphiBar[t] Derivative[1][rp][t])/(la (1+la) (-2 M+rp[t])^2 (JJ^2+rp[t]^2)^3)+(8 En JJ^2 mu \[Pi] rp[t] (-2 JJ^2 M+JJ^2 rp[t]-2 M rp[t]^2+(1-2 En^2) rp[t]^3) Derivative[1][YphiphiBar][t])/(la (1+la) (-2 M+rp[t])^2 (JJ^2+rp[t]^2)^2),
					0->-(16 JJ mu \[Pi] (2 JJ^6 M^2-3 JJ^6 M rp[t]+(JJ^6+6 JJ^4 M^2) rp[t]^2+(-9+8 En^2) JJ^4 M rp[t]^3+((3-4 En^2) JJ^4+6 JJ^2 M^2) rp[t]^4+(-9+22 En^2) JJ^2 M rp[t]^5+((3-11 En^2+2 En^4) JJ^2+2 M^2) rp[t]^6+(-3+14 En^2) M rp[t]^7+(1-7 En^2+6 En^4) rp[t]^8) YphiBar[t])/((1+la) rp[t]^3 (-2 M+rp[t])^2 (JJ^2+rp[t]^2)^3)+(8 En mu \[Pi] (6 JJ^4 M-3 JJ^4 rp[t]+6 JJ^2 M rp[t]^2+(-3+2 En^2) JJ^2 rp[t]^3-2 En^2 rp[t]^5) YBar[t] Derivative[1][rp][t])/((2 M-rp[t])^3 (JJ^2+rp[t]^2)^2)-(8 En JJ^2 mu \[Pi] (20 JJ^8 M^2+6 JJ^8 (-3+la) M rp[t]+(JJ^8 (4-3 la)+90 JJ^6 M^2) rp[t]^2+JJ^6 (-81+34 En^2+24 la) M rp[t]^3+2 (JJ^6 (9-6 la+En^2 (-5+3 la))+81 JJ^4 M^2) rp[t]^4+JJ^4 (-147+122 En^2+36 la) M rp[t]^5+(JJ^4 (33-18 la+2 En^2 (-19+9 la))+134 JJ^2 M^2) rp[t]^6+JJ^2 (-123+214 En^2+24 la) M rp[t]^7+2 (JJ^2 (14-6 la+En^2 (-41+9 la))+21 M^2) rp[t]^8+3 (-13+42 En^2+2 la) M rp[t]^9+(9+48 En^4+6 En^2 (-9+la)-3 la) rp[t]^10) YphiphiBar[t] Derivative[1][rp][t])/(la (1+la) (2 M-rp[t])^3 rp[t] (JJ^2+rp[t]^2)^5)-(16 En mu \[Pi] (-M (2 M-rp[t]) (JJ^2+rp[t]^2) Derivative[1][YBar][t]+En JJ rp[t]^3 Derivative[1][rp][t] Derivative[1][YphiBar][t]))/((1+la) (2 M-rp[t])^3 (JJ^2+rp[t]^2))-(8 En JJ^2 mu \[Pi] (-8 JJ^6 M^3+16 JJ^6 M^2 rp[t]-2 (5 JJ^6 M+24 JJ^4 M^3) rp[t]^2+2 JJ^4 (JJ^2+2 (21+En^2) M^2) rp[t]^3-6 JJ^2 M (-(-8+En^2) JJ^2+12 M^2) rp[t]^4+JJ^2 ((9-4 En^2) JJ^2+(120-88 En^2) M^2) rp[t]^5+2 M ((-33+54 En^2+4 En^4) JJ^2-16 M^2) rp[t]^6-4 ((-3+8 En^2) JJ^2+(-13+23 En^2) M^2) rp[t]^7-2 (14-51 En^2+20 En^4) M rp[t]^8+(5-28 En^2+24 En^4) rp[t]^9) Derivative[1][YphiphiBar][t])/(la (1+la) (-2 M+rp[t])^4 (JJ^2+rp[t]^2)^4)-(8 En^3 JJ^2 mu \[Pi] rp[t]^6 (2 JJ^2 M-JJ^2 rp[t]+2 M rp[t]^2+(-1+4 En^2) rp[t]^3) Derivative[1][rp][t] YphiphiBar''[t])/(la (1+la) (2 M-rp[t])^5 (JJ^2+rp[t]^2)^3)}		
		],

		HrrLabel[],
		Switch[gauge,
			"Lorenz"|"ModRWZ",
				{-1->0,
					0->(8 En mu \[Pi] (4 JJ^2 M-2 JJ^2 rp[t]+2 M rp[t]^2+(-1+2 En^2) rp[t]^3) YBar[t])/((2 M-rp[t])^3 (JJ^2+rp[t]^2))},
		
			"RWZ",
				{-2->(8 En JJ^2 mu \[Pi] rp[t] YphiphiBar[t])/(la (1+la) (2 M-rp[t]) (JJ^2+rp[t]^2)),
					-1->-((8 En JJ^2 mu \[Pi] (6 JJ^4 M-3 JJ^4 rp[t]+18 JJ^2 M rp[t]^2+(-9+2 En^2) JJ^2 rp[t]^3+12 M rp[t]^4+6 (-1+En^2) rp[t]^5) YphiphiBar[t])/(la (1+la) (-2 M+rp[t])^2 (JJ^2+rp[t]^2)^3))+(16 En^2 JJ mu \[Pi] rp[t]^3 YphiBar[t] Derivative[1][rp][t])/((1+la) (-2 M+rp[t])^3 (JJ^2+rp[t]^2))+(16 En^3 JJ^2 mu \[Pi] rp[t]^6 Derivative[1][rp][t] Derivative[1][YphiphiBar][t])/(la (1+la) (-2 M+rp[t])^4 (JJ^2+rp[t]^2)^2),
					0->-((8 En mu \[Pi] (4 JJ^4 M-2 JJ^4 rp[t]+2 JJ^2 M rp[t]^2+(-1+2 En^2) JJ^2 rp[t]^3-2 M rp[t]^4+(1-2 En^2) rp[t]^5) YBar[t])/((2 M-rp[t])^3 (JJ^2+rp[t]^2)^2))-(8 En JJ^2 mu \[Pi] (42 JJ^8 M^2+JJ^8 (-39+8 la) M rp[t]+(JJ^8 (9-4 la)+186 JJ^6 M^2) rp[t]^2+JJ^6 (-177+30 En^2+32 la) M rp[t]^3+2 (JJ^6 (21-8 la+En^2 (-5+3 la))+183 JJ^4 M^2) rp[t]^4+3 JJ^4 (-119+34 En^2+16 la) M rp[t]^5+(JJ^4 (87-24 la+2 En^2 (-19+9 la))+342 JJ^2 M^2) rp[t]^6+JJ^2 (-339+234 En^2+32 la) M rp[t]^7+2 (JJ^2 (42-8 la+En^2 (-53+9 la))+60 M^2) rp[t]^8+2 (-60+81 En^2+4 la) M rp[t]^9+(30+48 En^4+6 En^2 (-13+la)-4 la) rp[t]^10) YphiphiBar[t])/(la (1+la) rp[t] (-2 M+rp[t])^3 (JJ^2+rp[t]^2)^5)+(16 En^2 JJ mu \[Pi] rp[t]^2 (4 JJ^4 M-3 JJ^4 rp[t]+10 JJ^2 M rp[t]^2+(-7+2 En^2) JJ^2 rp[t]^3+6 M rp[t]^4+(-4+6 En^2) rp[t]^5) YphiBar[t] Derivative[1][rp][t])/((1+la) (-2 M+rp[t])^4 (JJ^2+rp[t]^2)^3)+(16 En^3 JJ^2 mu \[Pi] rp[t]^5 (2 JJ^4 M^2+3 JJ^4 M rp[t]-2 (JJ^4+10 JJ^2 M^2) rp[t]^2+2 (15+2 En^2) JJ^2 M rp[t]^3-2 (5 JJ^2+11 M^2) rp[t]^4+(27-20 En^2) M rp[t]^5+4 (-2+3 En^2) rp[t]^6) Derivative[1][rp][t] Derivative[1][YphiphiBar][t])/(la (1+la) (-2 M+rp[t])^6 (JJ^2+rp[t]^2)^4)+(8 En^3 JJ^2 mu \[Pi] rp[t]^6 (6 JJ^2 M-3 JJ^2 rp[t]+6 M rp[t]^2+(-3+4 En^2) rp[t]^3) YphiphiBar''[t])/(la (1+la) (2 M-rp[t])^5 (JJ^2+rp[t]^2)^3)}
		],

		JtLabel[],
		Switch[gauge,
			"Lorenz"|"ModRWZ",
				{-1->0,
					0->-((8 En^2 JJ mu \[Pi] rp[t] YphiBar[t])/((1+la) (2 M-rp[t]) (JJ^2+rp[t]^2)))},
			"RWZ",
				{-2->0, -1->0, 0->0}	
		],
		
		JrLabel[],
		Switch[gauge,
			"Lorenz"|"ModRWZ",
				{-1->0,
					0->-((8 En^2 JJ mu \[Pi] rp[t]^3 YphiBar[t] Derivative[1][rp][t])/((1+la) (-2 M+rp[t])^3 (JJ^2+rp[t]^2)))},
			"RWZ",
				{-2->0, -1->0, 0->0}
		],

		KLabel[],
		Switch[gauge,
			"Lorenz"|"ModRWZ",
				{-1->0,
					0->(8 En mu \[Pi] YBar[t])/(2 M rp[t]-rp[t]^2)},

			"RWZ",
				{-1->-((8 En JJ^2 mu \[Pi] YphiphiBar[t])/(la (1+la) rp[t] (JJ^2+rp[t]^2))),
					0->(8 En mu \[Pi] YBar[t])/(2 M rp[t]-rp[t]^2)+(8 En JJ^2 mu \[Pi] (9 JJ^4 M+JJ^4 (-3+la) rp[t]+24 JJ^2 M rp[t]^2+JJ^2 (-9+2 En^2+2 la) rp[t]^3+15 M rp[t]^4+(-6+6 En^2+la) rp[t]^5) YphiphiBar[t])/(la (1+la) (2 M-rp[t]) rp[t]^2 (JJ^2+rp[t]^2)^3)+(16 En^2 JJ mu \[Pi] rp[t] YphiBar[t] Derivative[1][rp][t])/((1+la) (-2 M+rp[t])^2 (JJ^2+rp[t]^2))+(16 En^3 JJ^2 mu \[Pi] rp[t]^4 Derivative[1][rp][t] Derivative[1][YphiphiBar][t])/(la (1+la) (-2 M+rp[t])^3 (JJ^2+rp[t]^2)^2),
					1->(8 En mu \[Pi] (-JJ^4 M+JJ^4 rp[t]-8 JJ^2 M rp[t]^2+(5+2 En^2) JJ^2 rp[t]^3-7 M rp[t]^4-2 (-2+En^2) rp[t]^5) YBar[t])/(rp[t]^2 (-2 M+rp[t])^2 (JJ^2+rp[t]^2)^2)-(8 En JJ^2 mu \[Pi] (99 JJ^8 M^2+JJ^8 (-75+17 la) M rp[t]+3 (JJ^8 (4-3 la)+152 JJ^6 M^2) rp[t]^2+2 JJ^6 (-174+22 En^2+37 la) M rp[t]^3+(JJ^6 (57-39 la+4 En^2 (-3+2 la))+834 JJ^4 M^2) rp[t]^4+2 JJ^4 (-327+86 En^2+60 la) M rp[t]^5+(JJ^4 (114-63 la+4 En^2 (-12+7 la))+696 JJ^2 M^2) rp[t]^6+2 JJ^2 (-282+166 En^2+43 la) M rp[t]^7+(JJ^2 (105-45 la+8 En^2 (-15+4 la))+219 M^2) rp[t]^8+(-183+204 En^2+23 la) M rp[t]^9+12 (-1+En^2) (-3+4 En^2+la) rp[t]^10) YphiphiBar[t])/(la (1+la) rp[t]^3 (-2 M+rp[t])^2 (JJ^2+rp[t]^2)^5)-(16 En^2 JJ mu \[Pi] (11 JJ^4 M+JJ^4 (-4+la) rp[t]+24 JJ^2 M rp[t]^2+JJ^2 (-9+2 En^2+2 la) rp[t]^3+13 M rp[t]^4+(-5+6 En^2+la) rp[t]^5) YphiBar[t] Derivative[1][rp][t])/((1+la) (2 M-rp[t])^3 (JJ^2+rp[t]^2)^3)-(16 En^3 JJ^2 mu \[Pi] rp[t]^3 (-12 JJ^4 M^2-2 JJ^4 (-6+la) M rp[t]+(JJ^4 (-3+la)-48 JJ^2 M^2) rp[t]^2+4 JJ^2 (12+En^2-la) M rp[t]^3-2 (-JJ^2 (-6+la)+18 M^2) rp[t]^4-2 (-18+10 En^2+la) M rp[t]^5+(-9+12 En^2+la) rp[t]^6) Derivative[1][rp][t] Derivative[1][YphiphiBar][t])/(la (1+la) (2 M-rp[t])^5 (JJ^2+rp[t]^2)^4)-(8 En^3 JJ^2 mu \[Pi] rp[t]^4 (6 JJ^2 M-3 JJ^2 rp[t]+6 M rp[t]^2+(-3+4 En^2) rp[t]^3) YphiphiBar''[t])/(la (1+la) (-2 M+rp[t])^4 (JJ^2+rp[t]^2)^3)}	
		],

		GLabel[],
		Switch[gauge,
			"Lorenz"|"ModRWZ",
				{-1->0,
					0->(8 En JJ^2 mu \[Pi] YphiphiBar[t])/(la (1+la) (2 M-rp[t]) rp[t] (JJ^2+rp[t]^2))},
			"RWZ",
				{-2->0, -1->0, 0->0}
		],
		
		HtLabel[],
		Switch[gauge,
			"Lorenz"|"ModRWZ",
				{-1->0,
					0->-((8 En^2 JJ mu \[Pi] rp[t] XphiBar[t])/((1+la) (2 M-rp[t]) (JJ^2+rp[t]^2)))},
		
			"RWZ",
				{-1->-((4 En JJ^2 mu \[Pi] (1-(2 M)/rp[t]) rp[t]^2 XphiphiBar[t] Derivative[1][rp][t])/(la (1+la) (-2 M+rp[t])^2 (JJ^2+rp[t]^2))),
					0->-((8 En^2 JJ mu \[Pi] rp[t] XphiBar[t])/((1+la) (2 M-rp[t]) (JJ^2+rp[t]^2)))-(4 En JJ^2 mu \[Pi] (3 JJ^4 M-JJ^4 rp[t]+8 JJ^2 M rp[t]^2+(-3+2 En^2) JJ^2 rp[t]^3+5 M rp[t]^4+(-2+6 En^2) rp[t]^5) XphiphiBar[t] Derivative[1][rp][t])/(la (1+la) (-2 M+rp[t])^2 (JJ^2+rp[t]^2)^3)+(4 En JJ^2 mu \[Pi] rp[t] (2 JJ^2 M-JJ^2 rp[t]+2 M rp[t]^2+(-1+2 En^2) rp[t]^3) Derivative[1][XphiphiBar][t])/(la (1+la) (-2 M+rp[t])^2 (JJ^2+rp[t]^2)^2)}
		],

		HrLabel[],
		Switch[gauge,
			"Lorenz"|"ModRWZ",
			{-1->0,
				0->-((8 En^2 JJ mu \[Pi] rp[t]^3 XphiBar[t] Derivative[1][rp][t])/((1+la) (-2 M+rp[t])^3 (JJ^2+rp[t]^2)))},
		
			"RWZ",
			{-1->(4 En JJ^2 mu \[Pi] (1-(2 M)/rp[t]) rp[t]^2 XphiphiBar[t])/(la (1+la) (-2 M+rp[t])^2 (JJ^2+rp[t]^2)),
				0->(4 En JJ^2 mu \[Pi] (5 JJ^4 M-3 JJ^4 rp[t]+16 JJ^2 M rp[t]^2+(-9+2 En^2) JJ^2 rp[t]^3+11 M rp[t]^4+6 (-1+En^2) rp[t]^5) XphiphiBar[t])/(la (1+la) (-2 M+rp[t])^2 (JJ^2+rp[t]^2)^3)-(8 En^2 JJ mu \[Pi] rp[t]^3 XphiBar[t] Derivative[1][rp][t])/((1+la) (-2 M+rp[t])^3 (JJ^2+rp[t]^2))-(8 En^3 JJ^2 mu \[Pi] rp[t]^6 Derivative[1][rp][t] Derivative[1][XphiphiBar][t])/(la (1+la) (-2 M+rp[t])^4 (JJ^2+rp[t]^2)^2)}
		],

		H2Label[],
		Switch[gauge,
			"Lorenz"|"ModRWZ",
				{-1->0,
					0->(8 En JJ^2 mu \[Pi] rp[t] XphiphiBar[t])/(la (1+la) (2 M-rp[t]) (JJ^2+rp[t]^2))},
			"RWZ",
				{-2->0, -1->0, 0->0}	
		],

		__,
		Print["Label ", label, " not found in discontinuity list."];
		Aborting[syms];
	]
]


def@
mpMonopoleDiscs[syms_Association,label_String,opts:OptionsPattern[]]:=
Module[{gauge,la,phip,t,r,yBarSym,mp},

	TestOptions[mpDiscTests,{opts}];
	
	gauge=OptionValue[Gauge];
	
	r=RSymbol[syms];
	t=TSymbol[syms];
	phip=PhiPSymbol[syms];
	yBarSym=YSymbol[syms,Conjugate->True];
	la=LambdaOfL[syms];

	Switch[gauge,
		"Lorenz"|"ModRWZ",
			mpRadDiscs[syms,label,Gauge->gauge]/.{la->LambdaOfL[0],yBarSym[t]->Conjugate@YHarmonic[0,0,\[Pi]/2,phip[t]]},

		"RWZ",
			mp=AmplitudeFunction[label][syms,Gauge->gauge,Mode->"Monopole",ExpandZerilli->True];
			(#->HeavisideCoefficientsToJumps[syms,D[mp,{r,#+1}]])&/@Range[-1,1]			
	]
]


def@
mpDipoleDiscs[syms_Association,label_String,opts:OptionsPattern[]]:=
Module[{gauge,la,phip,t,r,yBarSym,yPhiBarSym,xPhiBarSym,yBar11,yPhiBar11,xPhiBar10,mp},

	TestOptions[mpDiscTests,{opts}];
	
	gauge=OptionValue[Gauge];
	
	r=RSymbol[syms];
	t=TSymbol[syms];
	phip=PhiPSymbol[syms];
	yBarSym=YSymbol[syms,Conjugate->True];
	yPhiBarSym=YPhiSymbol[syms,Conjugate->True];
	xPhiBarSym=XPhiSymbol[syms,Conjugate->True];
	la=LambdaOfL[syms];

	yBar11=Conjugate@YHarmonic[1,1,\[Pi]/2,phip[t]];
	yPhiBar11=Conjugate@YPhi[1,1,\[Pi]/2,phip[t]];
	xPhiBar10=Conjugate@XPhi[1,0,\[Pi]/2,phip[t]];


	Switch[gauge,
		"Lorenz"|"ModRWZ",
			Refine[mpRadDiscs[syms,label,Gauge->gauge]/.{la->LambdaOfL[1],yBarSym[t]->yBar11,yPhiBarSym[t]->yPhiBar11,xPhiBarSym[t]->xPhiBar10},phip[t]\[Element]Reals],
					
		"RWZ",
			mp=AmplitudeFunction[label][syms,Gauge->gauge,Mode->"Dipole",ExpandZerilli->True];
			(#->HeavisideCoefficientsToJumps[syms,D[mp,{r,#+1}]])&/@Range[-1,1]
	]
]


def@
getModifiedDiscList[syms_Association,discs_List,{td_Integer/;td>=0,sd_Integer/;sd>=0}]:=
Module[{l1},

	l1=Nest[getTimeDerivDiscList[syms,#]&,discs,td];
	Nest[getSpaceDerivDiscList[syms,#]&,l1,sd]

]


getTimeDerivDisc[syms_Association,discs_List,order_Integer]:=
Module[{t,rp,discN,discNp1},

	t=TSymbol[syms];
	rp=RpSymbol[syms];

	discN=getDiscOfOrder[discs,order];
	discNp1=getDiscOfOrder[discs,order+1];

	If[discN===Null||discNp1===Null,
		OutputMessage[syms,"The requested order of discontinuity, "<> ToString[order]<>", is not available.",1,Style->"Error"];
		Aborting[syms],
		order->D[discN,t]-rp'[t]discNp1
	]	
]



getSpaceDerivDisc[syms_Association,discs_List,order_Integer]:=
Module[{discNp1},

	discNp1=getDiscOfOrder[discs,order+1];

	If[discNp1===Null,
		OutputMessage[syms,"The requested order of discontinuity, "<> ToString[order]<>", is not available.",1,Style->"Error"];
		Aborting[syms],
		order->discNp1
	]
]



getSpaceDerivDiscList[syms_Association,discs_List]:=getSpaceDerivDisc[syms,discs,#]&/@(discs[[All,1]]-1)
getTimeDerivDiscList[syms_Association,discs_List]:=getTimeDerivDisc[syms,discs,#]&/@(discs[[All,1]]-1)


getDiscOfOrder[discs_List,order_Integer]:=
Module[{},
	Which[order<Min@discs[[All,1]],
		0,
		order>Max@discs[[All,1]],
		Null,
		True,
		order/.discs
	]
]


getDiscFormatRules[syms_Association,sym_,{td_Integer,sd_Integer}]:=
Module[{jumpRule,GCRule,t,r,GC},

	t=TSymbol[syms];
	r=RSymbol[syms];
	GC=GCoefficientSymbol[syms];

	jumpRule=HoldPattern[order_->disc_]/;order>-2:>Jump[td,order+sd+1][sym][t]->disc;
	GCRule=HoldPattern[order_->disc_]/;order<=-2:>GC[Abs@(order+2)][td,sd][sym][t]->disc;

	{jumpRule,GCRule}
]


pushTest[x_]:=x==="RWZ" || x==="ModRWZ";


xiDiscsTests={"Labels"->labelsQ,"InitialGauge"->pushTest,"FinalGauge"->pushTest};


def@XiEvenDiscontinuities[syms_Association,{td_,sd_},opts:OptionsPattern[]]:=xiDiscs[syms,XiEvenLabel[],{td,sd},opts];
reDef@XiEvenDiscontinuities[syms_Association,opts:OptionsPattern[]]:=XiEvenDiscontinuities[syms,{0,0},opts];


def@XiEvenTDiscontinuities[syms_Association,{td_,sd_},opts:OptionsPattern[]]:=xiDiscs[syms,XiEvenTLabel[],{td,sd},opts];
reDef@XiEvenTDiscontinuities[syms_Association,opts:OptionsPattern[]]:=XiEvenTDiscontinuities[syms,{0,0},opts];


def@XiEvenRDiscontinuities[syms_Association,{td_,sd_},opts:OptionsPattern[]]:=xiDiscs[syms,XiEvenRLabel[],{td,sd},opts];
reDef@XiEvenRDiscontinuities[syms_Association,opts:OptionsPattern[]]:=XiEvenRDiscontinuities[syms,{0,0},opts];


def@XiOddDiscontinuities[syms_Association,{td_,sd_},opts:OptionsPattern[]]:=xiDiscs[syms,XiOddLabel[],{td,sd},opts];
reDef@XiOddDiscontinuities[syms_Association,opts:OptionsPattern[]]:=XiOddDiscontinuities[syms,{0,0},opts];


def@
xiDiscs[syms_Association,label_String,{td_,sd_},opts:OptionsPattern[]]:=
Module[{gI,gF,t,rp,la,M,En,JJ,mu,YBar,YphiBar,YphiphiBar,XphiBar,XphiphiBar,discListTemp,dl,dlMod,mpSymbol,labels},

	TestOptions[xiDiscsTests,{opts}];
	
	gI=OptionValue[InitialGauge];
	gF=OptionValue[FinalGauge];
	labels=OptionValue[Labels];

	t=TSymbol[syms];
	rp=RpSymbol[syms];
	mu=ParticleMassSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	En=SpecificEnergySymbol[syms];
	JJ=SpecificAngularMomentumSymbol[syms];
	la=LambdaOfL[syms];
	YBar=YSymbol[syms,Conjugate->True];
	YphiBar=YPhiSymbol[syms,Conjugate->True];
	YphiphiBar=YPhiPhiSymbol[syms,Conjugate->True];
	XphiBar=XPhiSymbol[syms,Conjugate->True];
	XphiphiBar=XPhiPhiSymbol[syms,Conjugate->True];

	dl=
	Switch[label,

		XiEvenLabel[],
		If[gI===gF,
			{-1->0,0->0,1->0},
			
			discListTemp = {-1->0,0->-((4 En JJ^2 mu \[Pi] rp[t] YphiphiBar[t])/(la (1+la) (2 M-rp[t]) (JJ^2+rp[t]^2))),1->0};
			If[gI==="RWZ",
				discListTemp,
				Thread[discListTemp[[All,1]]->-discListTemp[[All,2]]]
			]
		],

		XiEvenTLabel[],
		If[gI===gF,
			{-1->0,0->0,1->0},
			
			discListTemp = {-1->(4 JJ^2 mu \[Pi] (-2 M+rp[t]) (2 JJ^2 M-JJ^2 rp[t]+2 M rp[t]^2+(-1+En^2) rp[t]^3) YphiphiBar[t])/(En la (1+la) rp[t]^4 (JJ^2+rp[t]^2) Derivative[1][rp][t]),
							0->(4 JJ mu \[Pi] ((2 En^3 rp[t]^6 (JJ^2+rp[t]^2)^2 YphiBar[t])/(2 M-rp[t])+1/(la Derivative[1][rp][t])JJ (2 JJ^2 M-JJ^2 rp[t]+2 M rp[t]^2+(-1+En^2) rp[t]^3) (3 JJ^4 M-JJ^4 rp[t]+8 JJ^2 M rp[t]^2+(-3+2 En^2) JJ^2 rp[t]^3+5 M rp[t]^4+(-2+6 En^2) rp[t]^5) YphiphiBar[t]-(En^2 JJ rp[t]^6 (JJ^2+rp[t]^2) (2 JJ^2 M-JJ^2 rp[t]+2 M rp[t]^2+(-1+2 En^2) rp[t]^3) Derivative[1][YphiphiBar][t])/(la (-2 M+rp[t])^2)))/(En (1+la) rp[t]^5 (JJ^2+rp[t]^2)^3),
							1->0};
			If[gI==="RWZ",
				discListTemp,
				Thread[discListTemp[[All,1]]->-discListTemp[[All,2]]]
			]
		],

		XiEvenRLabel[],
		If[gI===gF,
			{-1->0,0->0,1->0},
			
			discListTemp = {-1->(4 En JJ^2 mu \[Pi] rp[t] YphiphiBar[t])/(la (1+la) (2 M-rp[t]) (JJ^2+rp[t]^2)),
							0->(4 En JJ mu \[Pi] (-1/la JJ (-2 M+rp[t])^2 (5 JJ^4 M-3 JJ^4 rp[t]+16 JJ^2 M rp[t]^2+(-9+2 En^2) JJ^2 rp[t]^3+11 M rp[t]^4+6 (-1+En^2) rp[t]^5) YphiphiBar[t]+2 En rp[t]^3 (-2 M+rp[t]) (JJ^2+rp[t]^2)^2 YphiBar[t] Derivative[1][rp][t]+(2 En^2 JJ rp[t]^6 (JJ^2+rp[t]^2) Derivative[1][rp][t] Derivative[1][YphiphiBar][t])/la))/((1+la) (-2 M+rp[t])^4 (JJ^2+rp[t]^2)^3),
							1->0};
			If[gI==="RWZ",
				discListTemp,
				Thread[discListTemp[[All,1]]->-discListTemp[[All,2]]]
			]
		],

		XiOddLabel[],
		If[gI===gF,
			{-1->0,0->0,1->0},

			discListTemp = {-1->0,0->-((4 En JJ^2 mu \[Pi] rp[t] XphiphiBar[t])/(la (1+la) (2 M-rp[t]) (JJ^2+rp[t]^2))),1->0};
			If[gI==="RWZ",
				discListTemp,
				Thread[discListTemp[[All,1]]->-discListTemp[[All,2]]]
			]
		],

		__,
		Print["Label ", label, " not found in discontinuity list."];
		Aborting[syms];
	];

	dlMod=getModifiedDiscList[syms,dl,{td,sd}];
	
	mpSymbol=AmplitudeFunction[label][syms,InitialGauge->gI,FinalGauge->gF,ReturnSymbol->True];
	Association[If[labels,dlMod/.getDiscFormatRules[syms,mpSymbol,{td,sd}],dlMod]]

]


End[];

EndPackage[];
