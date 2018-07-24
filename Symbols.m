(* ::Package:: *)

BeginPackage["BlackHoleAnalysis`Symbols`",
				{"BlackHoleAnalysis`Utils`",
				"BlackHoleAnalysis`Labels`",
				"BlackHoleAnalysis`ValidityTests`"}];


SymbolFunction::usage="SymbolFunction[label] returns the function for returning a symbol associated with label.";
DefaultSymbols::usage="DefaultSymbols[] returns an Association of correspondences between labels and symbols.";


Jump::usage="Jump[a,b][sym] returns a formatted expression for the jump in the a-th t derivative and b-th r dertivative in sym."


TSymbol::usage="TSymbol[] returns the symbol for Schwarzschild t.";
RSymbol::usage="RSymbol[] returns the symbol for Schwarzschild r.";
RpSymbol::usage="RpSymbol[] returns the (formatted) symbol for the particle location r_p.";
ThetaSymbol::usage="ThetaSymbol[] returns the symbol for the angular coordinate theta.";
ThetaPSymbol::usage="ThetaPSymbol[] returns the (formatted) symbol for the polar particle location theta_p.";
PhiSymbol::usage="PhiSymbol[] returns the symbol for the azimuthal coordinate phi.";
PhiPSymbol::usage="PhiPSymbol[] returns the (formatted) symbol for the azimuthal particle location phi_p.";
DeltaPhiSymbol::usage="DeltaPhiSymbol[] returns the symbol for 'Delta phi', the azimuthal particle location phi with the average phi advance subtracted.";
RStarSymbol::usage="RStarSymbol[] returns the (formatted) symbol for the tortoise coordinate.";
FrequencySymbol::usage="FrequencySymbol[] returns the symbol for wave frequency omega.";
ChiSymbol::usage="ChiSymbol[] returns the symbol for Darwin parameter chi.";
PeriapsisSymbol::usage="PeriapsisSymbol[] returns the (formatted) symbol for periapsis (rMin).";
ApoapsisSymbol::usage="ApoapsisSymbol[] returns the (formatted) symbol for apoapsis (rMax).";
SemiLatusRectumSymbol::usage="SemiLatusRectumSymbol[] returns the symbol for semi-latus rectum.";
OrbitalEccentricitySymbol::usage="OrbitalEccentricitySymbol[] returns the symbol for eccentricity.";
SpecificEnergySymbol::usage="SpecificEnergySymbol[] returns the symbol for specific energy.";
SpecificAngularMomentumSymbol::usage="SpecificAngularMomentumSymbol[] returns the symbol for specific angular momentum.";
BlackHoleMassSymbol::usage="BlackHoleMassSymbol[] returns the symbol for the mass of the central black hole.";
ParticleMassSymbol::usage="ParticleMassSymbol[] returns the symbol for the mass of the particle.";
BlackHoleSpinSymbol::usage="BlackHoleSpinSymbol[] returns the symbol for the spin of the central black hole.";
ImpactParameterSymbol::usage="ImpactParameterSymbol[] returns the symbol for the impact parameter of a particle in a hyperbolic trajectory relative \
to the central black hole.";

LSymbol::usage="LSymbol[] returns the symbol for mode number l.";
MSymbol::usage="MSymbol[] returns the symbol for mode number m.";
NSymbol::usage="NSymbol[] returns the symbol for mode number n.";
LambdaSymbol::usage="LambdaSymbol[] returns the symbol for mode number combination lambda.";
SpinWeightSymbol::usage="SpinWeightSymbol[] returns the symbol for spin of a field.";
LambdaSpinSymbol::usage="LambdaSpinSymbol[] returns the (formatted) symbol for lambda associated with the spin of a field.";


MasterFunctionSymbol::usage="MasterFunctionSymbol[] returns the (formatted) symbol for the RWZ-style master function.";
TeukolskyFunctionSymbol::usage="TeukolskyFunctionSymbol[] returns the symbol for the Teukolsky function.";
RadialTeukolskyFunctionSymbol::usage="RadialTeukolskyFunctionSymbol[] returns the symbol for the radial Teukolsky function.";
SpinWeightedSpheroidalHarmonicSymbol::usage="SpinWeightedHarmonicSymbol[] returns the (formatted) symbol for the spin-weighted spheroidal harmonic.";

SasakiNakamuraFunctionSymbol::usage="SasakiNakamuraFunctionSymbol[] returns the symbol for the Sasaki-Nakamura function.";

HttSymbol::usage="HttSymbol[] returns the (formatted) symbol for metric perturbation amplitude htt.";
HtrSymbol::usage="HtrSymbol[] returns the (formatted) symbol for metric perturbation amplitude htr.";
HrrSymbol::usage="HrrSymbol[] returns the (formatted) symbol for metric perturbation amplitude hrr.";
JtSymbol::usage="JtSymbol[] returns the (formatted) symbol for metric perturbation amplitude jt.";
JrSymbol::usage="JrSymbol[] returns the (formatted) symbol for metric perturbation amplitude jr.";
GSymbol::usage="GSymbol[] returns the (formatted) symbol for metric perturbation amplitude G.";
KSymbol::usage="KSymbol[] returns the (formatted) symbol for metric perturbation amplitude K.";
HtSymbol::usage="HtSymbol[] returns the (formatted) symbol for metric perturbation amplitude ht.";
HrSymbol::usage="HrSymbol[] returns the (formatted) symbol for metric perturbation amplitude hr.";
H2Symbol::usage="H2Symbol[] returns the (formatted) symbol for metric perturbation amplitude h2.";

HBarackSagoSymbol::usage="HBarackSagoSymbol[] returns the (formatted) symbol for Barack and Sago metric perturbation amplitude h.";

QttSymbol::usage="QttSymbol[] returns the (formatted) symbol for metric perturbation amplitude source term Qtt.";
QtrSymbol::usage="QtrSymbol[] returns the (formatted) symbol for metric perturbation amplitude source term Qtr.";
QrrSymbol::usage="QrrSymbol[] returns the (formatted) symbol for metric perturbation amplitude source term Qrr.";
QtSymbol::usage="QtSymbol[] returns the (formatted) symbol for metric perturbation amplitude source term Qt.";
QrSymbol::usage="QrSymbol[] returns the (formatted) symbol for metric perturbation amplitude source term Qr.";
QSharpSymbol::usage="QSharpSymbol[] returns the (formatted) symbol for metric perturbation amplitude source term QSharp.";
QFlatSymbol::usage="QFlatSymbol[] returns the (formatted) symbol for metric perturbation amplitude source term QFlat.";
PtSymbol::usage="PtSymbol[] returns the (formatted) symbol for metric perturbation amplitude source term Pt.";
PrSymbol::usage="PrSymbol[] returns the (formatted) symbol for metric perturbation amplitude source term Pr.";
PSymbol::usage="PSymbol[] returns the (formatted) symbol for metric perturbation amplitude source term P.";

XiEvenSymbol::usage="XiEvenSymbol[] returns the (formatted) symbol for gauge vector amplitude xiEven.";
XiEvenTSymbol::usage="XiEvenTSymbol[] returns the (formatted) symbol for gauge vector amplitude xiEvenT.";
XiEvenRSymbol::usage="XiEvenRSymbol[] returns the (formatted) symbol for gauge vector amplitude xiEvenR.";
XiOddSymbol::usage="XiOddSymbol[] returns the (formatted) symbol for gauge vector amplitude xiOdd.";

YSymbol::usage="YSymbol[] returns the symbol for spherical harmonic Y_lm.";
YThetaSymbol::usage="YThetaSymbol[] returns the (formatted) symbol for vector spherical harmonic Y_lm_Theta.";
YPhiSymbol::usage="YPhiSymbol[syms] returns the (formatted) symbol for vector spherical harmonic Y_lm_Phi.";
YThetaThetaSymbol::usage="YThetaThetaSymbol[] returns the (formatted) symbol for vector spherical harmonic Y_lm_ThetaTheta.";
YThetaPhiSymbol::usage="YThetaPhiSymbol[] returns the (formatted) symbol for vector spherical harmonic Y_lm_ThetaPhi.";
YPhiPhiSymbol::usage="YPhiPhiSymbol[] returns the (formatted) symbol for vector spherical harmonic Y_lm_PhiPhi.";
XThetaSymbol::usage="XThetaSymbol[] returns the (formatted) symbol for vector spherical harmonic X_lm_Theta.";
XPhiSymbol::usage="XPhiSymbol[] returns the (formatted) symbol for vector spherical harmonic X_lm_Phi.";
XThetaThetaSymbol::usage="XThetaThetaSymbol[] returns the (formatted) symbol for vector spherical harmonic X_lm_ThetaTheta.";
XThetaPhiSymbol::usage="XThetaPhiSymbol[] returns the (formatted) symbol for vector spherical harmonic X_lm_ThetaPhi.";
XPhiPhiSymbol::usage="XPhiPhiSymbol[] returns the (formatted) symbol for vector spherical harmonic X_lm_PhiPhi.";

DiracDeltaSymbol::usage="DiracDeltaSymbol[] returns the symbol for the Dirac delta function.";
HeavisideSymbol::usage="HeavisideSymbol[] returns the symbol for the Heaviside step function.";

GCoefficientSymbol::usage="GCoefficientSymbol[] returns the symbol for G, the coefficient of the Dirac delta function.";
FCoefficientSymbol::usage="FCoefficientSymbol[] returns the symbol for F, the coefficient of the derivative of the Dirac delta function.";
ECoefficientSymbol::usage="ECoefficientSymbol[] returns the symbol for E, the coefficients of the higher-order derivatives of the Dirac delta function.";


CSymbol::usage="CSymbol[] returns the symbol for the normalization constant.";
XSymbol::usage="XSymbol[] returns the symbol for x used in the formatting of xPN.";
XPNSymbol::usage="XPNSymbol[] returns the (formatted) symbol for xPN, the post-Newtonian parameter.";

CapitalOmegaSymbol::usage="CapitalOmegaSymbol[] returns the symbol for capital Omega. This is used in formatting OmegaPhiSymbol and OmegaRSymbol.";
OmegaPhiSymbol::usage="OmegaPhiSymbol[] returns the (formatted) symbol for OmegaPhi, the mean rate of azimuthal advance.";
OmegaRSymbol::usage="OmegaPhiSymbol[] returns the (formatted) symbol for OmegaR, the frequency of radial libration.";


ParityAndVariable::usage="ParityAndVariable[par,var] checks consistency between par and var and then returns a list with the parity and variable of a master function. \
The arguments par and var may be Default."


Begin["`Private`"];


Options[MasterFunctionSymbol]={"Parity"->Default,"Variable"->Default};
DocumentationBuilder`OptionDescriptions["MasterFunctionSymbol"] = 
{
	"Parity" -> "Parity of the master function (Default, \"Even\", or \"Odd\"). Choosing Default allows \"Parity\" to be set by the \"Variable\" Option. \
When both Options are Default, the \"Parity\" is \"Even\".",
	"Variable" -> "The specific master function variable to use (Default, \"ZM\", \"ZM1\", ... \"ZM7\" and \"CPM\", \"CPM1\", ... \"CPM6\"). \
Choosing Default is equivalent to \"ZM\" when \"Parity\"->\"Even\" and \"CPM\" when \"Parity\"->\"Odd\". When both Options are Default, the \"Parity\" is \"Even\"."
};


Options[TeukolskyFunctionSymbol]={"SpinWeight"->-2};
DocumentationBuilder`OptionDescriptions["TeukolskyFunctionSymbol"] = 
{
	"SpinWeight" -> "Spin of the field (integer from -2 to 2, as well as -1/2 and 1/2)"
};
Options[RadialTeukolskyFunctionSymbol]=Options[TeukolskyFunctionSymbol];
DocumentationBuilder`OptionDescriptions["RadialTeukolskyFunctionSymbol"] = DocumentationBuilder`OptionDescriptions["TeukolskyFunctionSymbol"];
Options[LambdaSpinSymbol]=Options[TeukolskyFunctionSymbol];
DocumentationBuilder`OptionDescriptions["LambdaSpinSymbol"] = DocumentationBuilder`OptionDescriptions["TeukolskyFunctionSymbol"];
Options[SpinWeightedSpheroidalHarmonicSymbol]=Join[Options[TeukolskyFunctionSymbol],{"Conjugate"->False}];
DocumentationBuilder`OptionDescriptions["SpinWeightedSpheroidalHarmonicSymbol"] = Join[DocumentationBuilder`OptionDescriptions["TeukolskyFunctionSymbol"],
{"Conjugate" -> "Sepcifies whether to return the symbol for the complex conjugate of the harmonic"}];

Options[SasakiNakamuraFunctionSymbol]=Options[TeukolskyFunctionSymbol];
DocumentationBuilder`OptionDescriptions["SasakiNakamuraFunctionSymbol"] = DocumentationBuilder`OptionDescriptions["TeukolskyFunctionSymbol"];


Options[KSymbol]={"Tag"->Null,"TagPosition"->"Up","Indices"->"Down"};
DocumentationBuilder`OptionDescriptions["KSymbol"] = 
{
	"Tag" -> "Specifies the Tag (referred to by its label in the symbols Association) to attach to the symbol",
	"TagPosition" -> "Specifies the position of the Tag, (\"Up\" or \"Down\")",
	"Indices"->"Position of the indices, (\"Up\" or \"Down\")"
};
Options[GSymbol]=Options[KSymbol];
DocumentationBuilder`OptionDescriptions["GSymbol"]=DocumentationBuilder`OptionDescriptions["KSymbol"];
Options[JtSymbol]=Options[KSymbol];
DocumentationBuilder`OptionDescriptions["JtSymbol"]=DocumentationBuilder`OptionDescriptions["KSymbol"];
Options[JrSymbol]=Options[KSymbol];
DocumentationBuilder`OptionDescriptions["JrSymbol"]=DocumentationBuilder`OptionDescriptions["KSymbol"];
Options[HttSymbol]=Options[KSymbol];
DocumentationBuilder`OptionDescriptions["HttSymbol"]=DocumentationBuilder`OptionDescriptions["KSymbol"];
Options[HtrSymbol]=Options[KSymbol];
DocumentationBuilder`OptionDescriptions["HtrSymbol"]=DocumentationBuilder`OptionDescriptions["HttSymbol"];
Options[HrrSymbol]=Options[KSymbol];
DocumentationBuilder`OptionDescriptions["HrrSymbol"]=DocumentationBuilder`OptionDescriptions["HttSymbol"];
Options[HtSymbol]=Options[KSymbol];
DocumentationBuilder`OptionDescriptions["HtSymbol"]=DocumentationBuilder`OptionDescriptions["KSymbol"];
Options[HrSymbol]=Options[KSymbol];
DocumentationBuilder`OptionDescriptions["HrSymbol"]=DocumentationBuilder`OptionDescriptions["KSymbol"];
Options[H2Symbol]=Options[KSymbol];
DocumentationBuilder`OptionDescriptions["H2Symbol"]=DocumentationBuilder`OptionDescriptions["KSymbol"];


Options[QFlatSymbol]={"Capital"->True,"Indices"->"Down"};
DocumentationBuilder`OptionDescriptions["QFlatSymbol"] = 
{
	"Capital" -> "Sepcifies whether to return the uppercase symbol (which includes the Dirac delta) or the lowercase symbol (which does not)",
	"Indices"->"Position of the indices, (\"Up\" or \"Down\")"
};
Options[QSharpSymbol]=Options[QFlatSymbol];
DocumentationBuilder`OptionDescriptions["QSharpSymbol"]=DocumentationBuilder`OptionDescriptions["QFlatSymbol"]
Options[QtSymbol]=Options[QFlatSymbol];
DocumentationBuilder`OptionDescriptions["QtSymbol"] = DocumentationBuilder`OptionDescriptions["QFlatSymbol"];
Options[QrSymbol]=Options[QFlatSymbol];
DocumentationBuilder`OptionDescriptions["QrSymbol"] = DocumentationBuilder`OptionDescriptions["QFlatSymbol"];
Options[QttSymbol]=Options[QFlatSymbol];
DocumentationBuilder`OptionDescriptions["QttSymbol"] = DocumentationBuilder`OptionDescriptions["QFlatSymbol"];
Options[QtrSymbol]=Options[QFlatSymbol];
DocumentationBuilder`OptionDescriptions["QtrSymbol"] = DocumentationBuilder`OptionDescriptions["QFlatSymbol"];
Options[QrrSymbol]=Options[QFlatSymbol];
DocumentationBuilder`OptionDescriptions["QrrSymbol"] = DocumentationBuilder`OptionDescriptions["QFlatSymbol"];

Options[PtSymbol]=Options[QFlatSymbol];
DocumentationBuilder`OptionDescriptions["PtSymbol"]=DocumentationBuilder`OptionDescriptions["QFlatSymbol"];
Options[PrSymbol]=Options[QFlatSymbol];
DocumentationBuilder`OptionDescriptions["PrSymbol"]=DocumentationBuilder`OptionDescriptions["QFlatSymbol"];
Options[PSymbol]=Options[QFlatSymbol];
DocumentationBuilder`OptionDescriptions["PSymbol"]=DocumentationBuilder`OptionDescriptions["QFlatSymbol"]


Options[XiEvenSymbol]={"TagPosition"->"Up","Indices"->"Down"};
DocumentationBuilder`OptionDescriptions["XiEvenSymbol"]=FilterRules[DocumentationBuilder`OptionDescriptions["KSymbol"],{"TagPosition","Indices"}];
Options[XiEvenTSymbol]=Options[XiEvenSymbol];
DocumentationBuilder`OptionDescriptions["XiEvenTSymbol"]=DocumentationBuilder`OptionDescriptions["XiEvenSymbol"];
Options[XiEvenRSymbol]=Options[XiEvenSymbol];
DocumentationBuilder`OptionDescriptions["XiEvenRSymbol"]=DocumentationBuilder`OptionDescriptions["XiEvenSymbol"];
Options[XiOddSymbol]=Options[XiEvenSymbol];
DocumentationBuilder`OptionDescriptions["XiOddSymbol"]=DocumentationBuilder`OptionDescriptions["XiEvenSymbol"];


Options[YSymbol]={"Conjugate"->False};
DocumentationBuilder`OptionDescriptions["YSymbol"] = 
{"Conjugate" -> "Sepcifies whether to return the symbol for the complex conjugate of the harmonic"};
Options[YThetaSymbol]=Options[YSymbol];
DocumentationBuilder`OptionDescriptions["YThetaSymbol"]=DocumentationBuilder`OptionDescriptions["YSymbol"];
Options[YPhiSymbol]=Options[YSymbol];
DocumentationBuilder`OptionDescriptions["YPhiSymbol"]=DocumentationBuilder`OptionDescriptions["YSymbol"];
Options[YThetaThetaSymbol]=Options[YSymbol];
DocumentationBuilder`OptionDescriptions["YThetaThetaSymbol"]=DocumentationBuilder`OptionDescriptions["YSymbol"];
Options[YThetaPhiSymbol]=Options[YSymbol];
DocumentationBuilder`OptionDescriptions["YThetaPhiSymbol"]=DocumentationBuilder`OptionDescriptions["YSymbol"];
Options[YPhiPhiSymbol]=Options[YSymbol];
DocumentationBuilder`OptionDescriptions["YPhiPhiSymbol"]=DocumentationBuilder`OptionDescriptions["YSymbol"];
Options[XThetaSymbol]=Options[YSymbol];
DocumentationBuilder`OptionDescriptions["XThetaSymbol"]=DocumentationBuilder`OptionDescriptions["YSymbol"];
Options[XPhiSymbol]=Options[YSymbol];
DocumentationBuilder`OptionDescriptions["XPhiSymbol"]=DocumentationBuilder`OptionDescriptions["YSymbol"];
Options[XThetaThetaSymbol]=Options[YSymbol];
DocumentationBuilder`OptionDescriptions["XThetaThetaSymbol"]=DocumentationBuilder`OptionDescriptions["YSymbol"];
Options[XThetaPhiSymbol]=Options[YSymbol];
DocumentationBuilder`OptionDescriptions["XThetaPhiSymbol"]=DocumentationBuilder`OptionDescriptions["YSymbol"];
Options[XPhiPhiSymbol]=Options[YSymbol];
DocumentationBuilder`OptionDescriptions["XPhiPhiSymbol"]=DocumentationBuilder`OptionDescriptions["YSymbol"];


zmMax=7;
cpmMax=6;
jtMax=5;
zmVars=Join[{"ZM"},"ZM"<>ToString[#]&/@Range[0,zmMax]];
cpmVars=Join[{"CPM"},"CPM"<>ToString[#]&/@Range[0,cpmMax]];
jtVars=Join[{"JT"},"JT"<>ToString[#]&/@Range[0,jtMax]];
masterVars=Join[{Default},zmVars,cpmVars,jtVars];
evenVars=Join[zmVars,jtVars];
oddVars=cpmVars;


Format[Jump[m_Integer,n_Integer][fn_]]:="\[LeftDoubleBracket]"<>MakePDString[fn,{m,n}]<>"\[RightDoubleBracket]";
Format[Jump[n_Integer?(#===1&)][fn_]]:=\!\(\*
TagBox[
StyleBox["\"\<\\[LeftDoubleBracket]\\!\\(\\*SubscriptBox[\\(\\[PartialD]\\), \\(r\\)]\\)\>\"",
ShowSpecialCharacters->False,
ShowStringCharacters->True,
NumberMarks->True],
FullForm]\)<>ToExpression@ToBoxes@fn<>"\[RightDoubleBracket]";
Format[Jump[n_Integer?(#>1&)][fn_]]:=ToString@StringForm[\!\(\*
TagBox[
StyleBox["\"\<\\[LeftDoubleBracket]\\!\\(\\*SuperscriptBox[SubscriptBox[\\(\\[PartialD]\\), \\(r\\)], \\(`1`\\)]\\)\>\"",
ShowSpecialCharacters->False,
ShowStringCharacters->True,
NumberMarks->True],
FullForm]\),n]<>ToExpression@ToBoxes@fn<>"\[RightDoubleBracket]";
Format[Jump[0][fn_]]:="\[LeftDoubleBracket]"<>ToExpression@ToBoxes@fn<>"\[RightDoubleBracket]";


spinWeightTest=Function[x,MemberQ[Join[Range[-2,2],{1/2,-1/2}],x]];


(*             *)
(* Coordiantes *)
(*             *)


def@
TSymbol[syms_Association]:=GetSymbol[syms,"t"]
reDef@
TSymbol[]:=TSymbol[DefaultSymbols[]];


def@
RSymbol[syms_Association]:=GetSymbol[syms,"r"]
reDef@
RSymbol[]:=RSymbol[DefaultSymbols[]];


def@
RpSymbol[syms_Association]:=
Module[{symRp,symR},
	
	symRp=GetSymbol[syms,"rp"];
	symR=RSymbol[syms];

	Format[symRp]=ToString[Subscript[symR, "p"],StandardForm];

	symRp
]
reDef@
RpSymbol[]:=RpSymbol[DefaultSymbols[]];


def@
RhoSymbol[syms_Association]:=GetSymbol[syms,"rho"]
reDef@
RhoSymbol[]:=RhoSymbol[DefaultSymbols[]];


def@
ThetaSymbol[syms_Association]:=GetSymbol[syms,"theta"]
reDef@
ThetaSymbol[]:=ThetaSymbol[DefaultSymbols[]];


def@
ThetaPSymbol[syms_Association]:=
Module[{symThetaP,symTheta},
	
	symThetaP=GetSymbol[syms,"thetap"];
	symTheta=ThetaSymbol[syms];

	Format[symThetaP]=ToString[Subscript[symTheta, "p"],StandardForm];

	symThetaP
]
reDef@
ThetaPSymbol[]:=ThetaPSymbol[DefaultSymbols[]];


def@
PhiSymbol[syms_Association]:=GetSymbol[syms,"phi"]
reDef@
PhiSymbol[]:=PhiSymbol[DefaultSymbols[]];


def@
PhiPSymbol[syms_Association]:=
Module[{symPhiP,symPhi},
	
	symPhiP=GetSymbol[syms,"phip"];
	symPhi=PhiSymbol[syms];

	Format[symPhiP]=ToString[Subscript[symPhi, "p"],StandardForm];

	symPhiP
]
reDef@
PhiPSymbol[]:=PhiPSymbol[DefaultSymbols[]];


def@
DeltaPhiSymbol[syms_Association]:=GetSymbol[syms,"DeltaPhi"]
reDef@
DeltaPhiSymbol[]:=DeltaPhiSymbol[DefaultSymbols[]];


def@
RStarSymbol[syms_Association]:=
Module[{symRStar,symR},
	
	symRStar=GetSymbol[syms,"rStar"];
	symR=RSymbol[syms];
	
	Format[symRStar]=ToString[Subscript[symR, "*"],StandardForm];

	symRStar
]
reDef@
RStarSymbol[]:=RStarSymbol[DefaultSymbols[]];


def@
FrequencySymbol[syms_Association]:=GetSymbol[syms,"Frequency"]
reDef@
FrequencySymbol[]:=FrequencySymbol[DefaultSymbols[]];


(*              *)
(* Mode numbers *)
(*              *)


def@
LSymbol[syms_Association]:=GetSymbol[syms,"l"]
reDef@
LSymbol[]:=LSymbol[DefaultSymbols[]];


def@
MSymbol[syms_Association]:=GetSymbol[syms,"m"]
reDef@
MSymbol[]:=MSymbol[DefaultSymbols[]];


def@
NSymbol[syms_Association]:=GetSymbol[syms,"n"]
reDef@
NSymbol[]:=NSymbol[DefaultSymbols[]];


def@
LambdaSymbol[syms_Association]:=GetSymbol[syms,"lambda"]
reDef@
LambdaSymbol[]:=LambdaSymbol[DefaultSymbols[]];


(*               *)
(* Orbit symbols *)
(*               *)


def@
PeriapsisSymbol[syms_Association]:=
Module[{symRMin,symR},
	
	symRMin=GetSymbol[syms,"Periapsis"];
	symR=RSymbol[syms];

	Format[symRMin]=ToString[Subscript[symR, "min"],StandardForm];

	symRMin
]
reDef@
PeriapsisSymbol[]:=PeriapsisSymbol[DefaultSymbols[]];


def@
ApoapsisSymbol[syms_Association]:=
Module[{symRMax,symR},
	
	symRMax=GetSymbol[syms,"Apoapsis"];
	symR=RSymbol[syms];

	Format[symRMax]=ToString[Subscript[symR, "max"],StandardForm];

	symRMax
]
reDef@
ApoapsisSymbol[]:=ApoapsisSymbol[DefaultSymbols[]];


def@
SemiLatusRectumSymbol[syms_Association]:=GetSymbol[syms,"SemiLatusRectum"]
reDef@
SemiLatusRectumSymbol[]:=SemiLatusRectumSymbol[DefaultSymbols[]];


def@
OrbitalEccentricitySymbol[syms_Association]:=GetSymbol[syms,"OrbitalEccentricity"]
reDef@
OrbitalEccentricitySymbol[]:=OrbitalEccentricitySymbol[DefaultSymbols[]];


def@
SpecificEnergySymbol[syms_Association]:=GetSymbol[syms,"SpecificEnergy"]
reDef@
SpecificEnergySymbol[]:=SpecificEnergySymbol[DefaultSymbols[]];


def@
SpecificAngularMomentumSymbol[syms_Association]:=GetSymbol[syms,"SpecificAngularMomentum"]
reDef@
SpecificAngularMomentumSymbol[]:=SpecificAngularMomentumSymbol[DefaultSymbols[]];


def@
BlackHoleMassSymbol[syms_Association]:=GetSymbol[syms,"BlackHoleMass"]
reDef@
BlackHoleMassSymbol[]:=BlackHoleMassSymbol[DefaultSymbols[]];


def@
ParticleMassSymbol[syms_Association]:=GetSymbol[syms,"ParticleMass"]
reDef@
ParticleMassSymbol[]:=ParticleMassSymbol[DefaultSymbols[]];


def@
ChiSymbol[syms_Association]:=GetSymbol[syms,"chi"]
reDef@
ChiSymbol[]:=ChiSymbol[DefaultSymbols[]];


def@
BlackHoleSpinSymbol[syms_Association]:=GetSymbol[syms,"BlackHoleSpin"]
reDef@
BlackHoleSpinSymbol[]:=BlackHoleSpinSymbol[DefaultSymbols[]];


def@
ImpactParameterSymbol[syms_Association]:=GetSymbol[syms,"ImpactParameter"]
reDef@
ImpactParameterSymbol[]:=ImpactParameterSymbol[DefaultSymbols[]];


def@
SpinWeightSymbol[syms_Association]:=GetSymbol[syms,"SpinWeight"]
reDef@
SpinWeightSymbol[]:=SpinWeightSymbol[DefaultSymbols[]];


def@
LambdaSpinSymbol[syms_Association,opts:OptionsPattern[]]:=
Module[{symLaS,symLa,spin,spinStr,sym},

	TestOptions[{"SpinWeight" ->spinWeightTest},{opts}];

	spin = OptionValue["SpinWeight"];

	symLaS=GetSymbol[syms,"LambdaSpin"];
	symLa=LambdaSymbol[syms];

	sym =Symbol[ToString[symLaS]<>Which[Positive[spin],"p",Negative[spin],"m",True,""]<>If[MatchQ[spin,_Rational],"Half",ToString[Abs[spin]]]];
	spinStr = If[MatchQ[spin,_Rational],ToString[Numerator[spin]]<>"/"<>ToString[Denominator[spin]],ToString[spin]];
	Format[sym]=ToString[Subscript[symLa, spinStr],StandardForm];

	sym
]
reDef@
LambdaSpinSymbol[opts:OptionsPattern[]]:=LambdaSpinSymbol[DefaultSymbols[],opts];


(*        *)
(* Fields *)
(*        *)


def@
MasterFunctionSymbol[syms_Association,opts:OptionsPattern[]]:=
Module[{optionsRules,tag,variable,parity,tagList,par,var,psi,symbol},

	optionsRules = {"Parity" -> Function[x,MemberQ[{Default,"Even","Odd"},x]],
					"Variable" -> Function[x,MemberQ[masterVars,x]]};
	TestOptions[optionsRules,{opts}];
	variable=OptionValue["Variable"];
	parity=OptionValue["Parity"];

	{par,var}=ParityAndVariable[parity,variable];
	psi=GetSymbol[syms,"MasterFunction"];
	
	symbol = Symbol[ToString[psi]<>ToString[var]];
	Format[symbol] = ToString[Superscript[psi,var],StandardForm];
		
	symbol
]
reDef@
MasterFunctionSymbol[opts:OptionsPattern[]]:=MasterFunctionSymbol[DefaultSymbols[],opts];


def@
TeukolskyFunctionSymbol[syms_Association,opts:OptionsPattern[]]:=
Module[{spin,optionsRules,baseSym,subscript,sym},

	TestOptions[{"SpinWeight" ->spinWeightTest},{opts}];

	spin = OptionValue["SpinWeight"];
	
	{baseSym,subscript} = Switch[spin,
					-2,
					{GetSymbol[syms,"TeukolskyGravity"],4},
					2,
					{GetSymbol[syms,"TeukolskyGravity"],0},
					-1,
					{GetSymbol[syms,"TeukolskyEM"],2},
					1,
					{GetSymbol[syms,"TeukolskyEM"],0},
					0,
					{GetSymbol[syms,"TeukolskyScalar"],False},
					-1/2,
					{GetSymbol[syms,"TeukolskyNeutrino"],1},
					1/2,
					{GetSymbol[syms,"TeukolskyNeutrino"],0}
				];
	
	sym =If[subscript=!=False, Symbol[ToString[baseSym]<>ToString[Abs[subscript]]],baseSym];
	If[subscript=!=False,Format[sym]=ToString[Subscript[baseSym, subscript],StandardForm]];
	sym
]
reDef@
TeukolskyFunctionSymbol[opts:OptionsPattern[]]:=TeukolskyFunctionSymbol[DefaultSymbols[],opts];


def@
RadialTeukolskyFunctionSymbol[syms_Association,opts:OptionsPattern[]]:=
Module[{spin,baseSym,sym,spinStr},

	TestOptions[{"SpinWeight" ->spinWeightTest},{opts}];

	spin = OptionValue["SpinWeight"];
	
	baseSym = GetSymbol[syms,"RadialTeukolsky"];
	
	sym =Symbol[ToString[baseSym]<>Which[Positive[spin],"p",Negative[spin],"m",True,""]<>If[MatchQ[spin,_Rational],"Half",ToString[Abs[spin]]]];
	spinStr = If[MatchQ[spin,_Rational],ToString[Numerator[spin]]<>"/"<>ToString[Denominator[spin]],ToString[spin]];
	If[subscript=!=False,Format[sym]=ToString[Subscript[baseSym, spinStr],StandardForm]];
	sym
]
reDef@
RadialTeukolskyFunctionSymbol[opts:OptionsPattern[]]:=RadialTeukolskyFunctionSymbol[DefaultSymbols[],opts];


def@
SpinWeightedSpheroidalHarmonicSymbol[syms_Association,opts:OptionsPattern[]]:=
Module[{sym,symBase,symBaseConj,conj,spin,spinStr},

	TestOptions[{"Conjugate"->BooleanQ,
				"SpinWeight" ->spinWeightTest},{opts}];

	conj=OptionValue[Conjugate];
	spin=OptionValue["SpinWeight"];
	
	spinStr = If[MatchQ[spin,_Rational],ToString[Numerator[spin]]<>"/"<>ToString[Denominator[spin]],ToString[spin]];
	symBase = GetSymbol[syms,"SpinWeightedHarmonic"];
	symBaseConj = If[conj,Symbol[ToString[symBase]<>"Bar"],symBase];
	sym = Symbol[ToString[symBaseConj]<>Which[Positive[spin],"p",Negative[spin],"m",True,""]<>If[MatchQ[spin,_Rational],"Half",ToString[Abs[spin]]]];
	
	Format[sym]=ToString[Subscript["",spinStr],StandardForm]<>If[conj,ToString[Overscript["S","_"],StandardForm],"S"];
	
	sym
];
reDef@
SpinWeightedSpheroidalHarmonicSymbol[opts:OptionsPattern[]]:=SpinWeightedSpheroidalHarmonicSymbol[DefaultSymbols[],opts];


def@
SasakiNakamuraFunctionSymbol[syms_Association,opts:OptionsPattern[]]:=
Module[{spin,baseSym,sym,spinStr},

	TestOptions[{"SpinWeight" ->spinWeightTest},{opts}];

	spin = OptionValue["SpinWeight"];
	If[spin=!=-2, 
		Print["The SasakiNakamura equations currently only support spin-weight = -2."];
		Print["Feel free to improve this with this citation: arxiv.org/abs/gr-qc/0002043."]; 
		Abort[]
	];
	
	baseSym = GetSymbol[syms,"SasakiNakamuraFunction"];
	
	sym =Symbol[ToString[baseSym]<>Which[Positive[spin],"p",Negative[spin],"m",True,""]<>If[MatchQ[spin,_Rational],"Half",ToString[Abs[spin]]]];
	spinStr = If[MatchQ[spin,_Rational],ToString[Numerator[spin]]<>"/"<>ToString[Denominator[spin]],ToString[spin]];
	Format[sym]=ToString[Subsuperscript["X", spinStr,"SN"],StandardForm];
	sym
];
reDef@
SasakiNakamuraFunctionSymbol[opts:OptionsPattern[]]:=SasakiNakamuraFunctionSymbol[DefaultSymbols[],opts];


indicesQ[x_]:=MemberQ[{"Up","Down","UpDown","DownUp"},x];
tagPosQ[x_]:=MemberQ[{"Up","Down"},x];
tagQ[x_]:=StringQ[x]||x===Null;


fieldTestsIndices={"Tag"->tagQ,"TagPosition"->tagPosQ,"Indices" -> indicesQ};


def@
HttSymbol[syms_Association,opts:OptionsPattern[]]:=
Module[{tagStr,inds,tp,sym},

	TestOptions[fieldTestsIndices,{opts}];
	
	inds=OptionValue[Indices];
	tagStr=OptionValue[Tag];
	tp=OptionValue[TagPosition];
	
	sym=formSymbol["hEven",{"t","t"},Indices->inds,Tag->tagStr,TagPosition->tp];
	GetSymbol[syms,sym]
]
reDef@
HttSymbol[opts:OptionsPattern[]]:=HttSymbol[DefaultSymbols[],opts];


def@
HtrSymbol[syms_Association,opts:OptionsPattern[]]:=
Module[{tagStr,tp,inds,sym},

	TestOptions[fieldTestsIndices,{opts}];
	
	inds=OptionValue[Indices];
	tagStr=OptionValue[Tag];
	tp=OptionValue[TagPosition];
	
	sym=formSymbol["hEven",{"t","r"},Indices->inds,Tag->tagStr,TagPosition->tp];
	GetSymbol[syms,sym]
]
reDef@
HtrSymbol[opts:OptionsPattern[]]:=HtrSymbol[DefaultSymbols[],opts];


def@
HrrSymbol[syms_Association,opts:OptionsPattern[]]:=
Module[{tagStr,tp,inds,sym},

	TestOptions[fieldTestsIndices,{opts}];
	
	inds=OptionValue[Indices];
	tagStr=OptionValue[Tag];
	tp=OptionValue[TagPosition];
	
	sym=formSymbol["hEven",{"r","r"},Indices->inds,Tag->tagStr,TagPosition->tp];
	GetSymbol[syms,sym]
]
reDef@
HrrSymbol[opts:OptionsPattern[]]:=HrrSymbol[DefaultSymbols[],opts];


def@
JtSymbol[syms_Association,opts:OptionsPattern[]]:=
Module[{tagStr,tp,inds,sym},

	TestOptions[fieldTestsIndices,{opts}];
	
	inds=OptionValue[Indices];
	tagStr=OptionValue[Tag];
	tp=OptionValue[TagPosition];
	
	sym=formSymbol["j",{"t"},Indices->inds,Tag->tagStr,TagPosition->tp];
	GetSymbol[syms,sym]
]
reDef@
JtSymbol[opts:OptionsPattern[]]:=JtSymbol[DefaultSymbols[],opts];


def@
JrSymbol[syms_Association,opts:OptionsPattern[]]:=
Module[{tagStr,tp,inds,sym},

	TestOptions[fieldTestsIndices,{opts}];
	
	inds=OptionValue[Indices];
	tagStr=OptionValue[Tag];
	tp=OptionValue[TagPosition];
	
	sym=formSymbol["j",{"r"},Indices->inds,Tag->tagStr,TagPosition->tp];
	GetSymbol[syms,sym]
]
reDef@
JrSymbol[opts:OptionsPattern[]]:=JrSymbol[DefaultSymbols[],opts];


def@
GSymbol[syms_Association,opts:OptionsPattern[]]:=
Module[{tagStr,tp,sym},

	TestOptions[fieldTestsIndices,{opts}];
	
	tagStr=OptionValue[Tag];
	tp=OptionValue[TagPosition];
	
	sym=formSymbol["G",Tag->tagStr,TagPosition->tp];
	GetSymbol[syms,sym]
]
reDef@
GSymbol[opts:OptionsPattern[]]:=GSymbol[DefaultSymbols[],opts];


def@
KSymbol[syms_Association,opts:OptionsPattern[]]:=
Module[{tagStr,tp,sym},

	TestOptions[fieldTestsIndices,{opts}];
	
	tagStr=OptionValue[Tag];
	tp=OptionValue[TagPosition];
	
	sym=formSymbol["K",Tag->tagStr,TagPosition->tp];
	GetSymbol[syms,sym]
]
reDef@
KSymbol[opts:OptionsPattern[]]:=KSymbol[DefaultSymbols[],opts];


def@
HtSymbol[syms_Association,opts:OptionsPattern[]]:=
Module[{tagStr,tp,inds,sym},

	TestOptions[fieldTestsIndices,{opts}];
	
	inds=OptionValue[Indices];
	tagStr=OptionValue[Tag];
	tp=OptionValue[TagPosition];
	
	sym=formSymbol["hOdd",{"t"},Indices->inds,Tag->tagStr,TagPosition->tp];
	GetSymbol[syms,sym]
]
reDef@
HtSymbol[opts:OptionsPattern[]]:=HtSymbol[DefaultSymbols[],opts];


def@
HrSymbol[syms_Association,opts:OptionsPattern[]]:=
Module[{tagStr,tp,inds,sym},

	TestOptions[fieldTestsIndices,{opts}];
	
	inds=OptionValue[Indices];
	tagStr=OptionValue[Tag];
	tp=OptionValue[TagPosition];
	
	sym=formSymbol["hOdd",{"r"},Indices->inds,Tag->tagStr,TagPosition->tp];
	GetSymbol[syms,sym]
]
reDef@
HrSymbol[opts:OptionsPattern[]]:=HrSymbol[DefaultSymbols[],opts];


def@
H2Symbol[syms_Association,opts:OptionsPattern[]]:=
Module[{tagStr,tp,tagOpt},

	TestOptions[fieldTestsIndices,{opts}];
	
	tagStr=OptionValue[Tag];
	tp=OptionValue[TagPosition];
		
	tagOpt=
		Which[tagStr===Null,
			{"Down"->{"2Tag"}},
			tp==="Down",
			{"Down"->{"2Tag",tagStr}},
			tp==="Up",
			{"Down"->{"2Tag"},"Up"->{tagStr}}
		];
	GetSymbol[syms,"hOdd"["Tags"->tagOpt]]
]
reDef@
H2Symbol[opts:OptionsPattern[]]:=H2Symbol[DefaultSymbols[],opts];


def@
HBarackSagoSymbol[syms_Association]:=
Module[{symHBS},
	
	symHBS=GetSymbol[syms,"hBarackSago"];
	symHBS
]
reDef@
HBarackSagoSymbol[]:=HBarackSagoSymbol[DefaultSymbols[]];


(*                 *)
(* MP source terms *)
(*                 *)


sourceTestsIndices={"Capital"->BooleanQ,"Indices"->indicesQ};


def@
QttSymbol[syms_Association,opts:OptionsPattern[]]:=
Module[{inds,qSym,sym},

	TestOptions[sourceTestsIndices,{opts}];
	
	inds=OptionValue[Indices];
	qSym=If[OptionValue[Capital],"Q","q"];

	sym=formSymbol[qSym,{"t","t"},Indices->inds];
	GetSymbol[syms,sym]
]
reDef@
QttSymbol[opts:OptionsPattern[]]:=QttSymbol[DefaultSymbols[],opts];


def@
QtrSymbol[syms_Association,opts:OptionsPattern[]]:=
Module[{inds,qSym,sym},

	TestOptions[sourceTestsIndices,{opts}];
	
	inds=OptionValue[Indices];
	qSym=If[OptionValue[Capital],"Q","q"];

	sym=formSymbol[qSym,{"t","r"},Indices->inds];
	GetSymbol[syms,sym]
]
reDef@
QtrSymbol[opts:OptionsPattern[]]:=QtrSymbol[DefaultSymbols[],opts];


def@
QrrSymbol[syms_Association,opts:OptionsPattern[]]:=
Module[{inds,qSym,sym},

	TestOptions[sourceTestsIndices,{opts}];
	
	inds=OptionValue[Indices];
	qSym=If[OptionValue[Capital],"Q","q"];

	sym=formSymbol[qSym,{"r","r"},Indices->inds];
	GetSymbol[syms,sym]
]
reDef@
QrrSymbol[opts:OptionsPattern[]]:=QrrSymbol[DefaultSymbols[],opts];


def@
QtSymbol[syms_Association,opts:OptionsPattern[]]:=
Module[{inds,qSym,sym},

	TestOptions[sourceTestsIndices,{opts}];
	
	inds=OptionValue[Indices];
	qSym=If[OptionValue[Capital],"Q","q"];

	sym=formSymbol[qSym,{"t"},Indices->inds];
	GetSymbol[syms,sym]
]
reDef@
QtSymbol[opts:OptionsPattern[]]:=QtSymbol[DefaultSymbols[],opts];


def@
QrSymbol[syms_Association,opts:OptionsPattern[]]:=
Module[{inds,qSym,sym},

	TestOptions[sourceTestsIndices,{opts}];
	
	inds=OptionValue[Indices];
	qSym=If[OptionValue[Capital],"Q","q"];

	sym=formSymbol[qSym,{"r"},Indices->inds];
	GetSymbol[syms,sym]
]
reDef@
QrSymbol[opts:OptionsPattern[]]:=QrSymbol[DefaultSymbols[],opts];


def@
QSharpSymbol[syms_Association,opts:OptionsPattern[]]:=
Module[{qSym,sym},

	TestOptions[sourceTestsIndices,{opts}];
	
	qSym=If[OptionValue[Capital],"Q","q"];

	sym=formSymbol[qSym,Tag->"Sharp"];
	GetSymbol[syms,sym]
]
reDef@
QSharpSymbol[opts:OptionsPattern[]]:=QSharpSymbol[DefaultSymbols[],opts];


def@
QFlatSymbol[syms_Association,opts:OptionsPattern[]]:=
Module[{qSym,sym},

	TestOptions[sourceTestsIndices,{opts}];
	
	qSym=If[OptionValue[Capital],"Q","q"];

	sym=formSymbol[qSym,Tag->"Flat"];
	GetSymbol[syms,sym]
]
reDef@
QFlatSymbol[opts:OptionsPattern[]]:=QFlatSymbol[DefaultSymbols[],opts];


def@
PtSymbol[syms_Association,opts:OptionsPattern[]]:=
Module[{inds,pSym,sym},

	TestOptions[sourceTestsIndices,{opts}];
	
	inds=OptionValue[Indices];
	pSym=If[OptionValue[Capital],"P","p"];

	sym=formSymbol[pSym,{"t"},Indices->inds];
	GetSymbol[syms,sym]
]
reDef@
PtSymbol[opts:OptionsPattern[]]:=PtSymbol[DefaultSymbols[],opts];


def@
PrSymbol[syms_Association,opts:OptionsPattern[]]:=
Module[{inds,pSym,sym},

	TestOptions[sourceTestsIndices,{opts}];
	
	inds=OptionValue[Indices];
	pSym=If[OptionValue[Capital],"P","p"];

	sym=formSymbol[pSym,{"r"},Indices->inds];
	GetSymbol[syms,sym]
]
reDef@
PrSymbol[opts:OptionsPattern[]]:=PrSymbol[DefaultSymbols[],opts];


def@
PSymbol[syms_Association,opts:OptionsPattern[]]:=
Module[{pSym},

	TestOptions[sourceTestsIndices,{opts}];
	
	pSym=If[OptionValue[Capital],"P","p"];

	GetSymbol[syms,pSym]
]
reDef@
PSymbol[opts:OptionsPattern[]]:=PSymbol[DefaultSymbols[],opts];


(*                     *)
(* Gauge transf fields *)
(*                     *)


xiTests={"TagPosition"->tagPosQ,"Indices"->indicesQ};


def@
XiEvenSymbol[syms_Association,opts:OptionsPattern[]]:=
Module[{tp,sym},

	TestOptions[xiTests,{opts}];
	
	tp=OptionValue[TagPosition];
	
	sym=formSymbol["xi",Tag->"EvenTag",TagPosition->tp];
	GetSymbol[syms,sym]
]
reDef@
XiEvenSymbol[opts:OptionsPattern[]]:=XiEvenSymbol[DefaultSymbols[],opts];


def@
XiEvenTSymbol[syms_Association,opts:OptionsPattern[]]:=
Module[{tp,inds,sym},

	TestOptions[xiTests,{opts}];
	
	inds=OptionValue[Indices];
	tp=OptionValue[TagPosition];
	
	sym=formSymbol["xi",{"t"},Indices->inds,Tag->"EvenTag",TagPosition->tp];
	GetSymbol[syms,sym]
]
reDef@
XiEvenTSymbol[opts:OptionsPattern[]]:=XiEvenTSymbol[DefaultSymbols[],opts];


def@
XiEvenRSymbol[syms_Association,opts:OptionsPattern[]]:=
Module[{tp,inds,sym},

	TestOptions[xiTests,{opts}];
	
	inds=OptionValue[Indices];
	tp=OptionValue[TagPosition];
	
	sym=formSymbol["xi",{"r"},Indices->inds,Tag->"EvenTag",TagPosition->tp];
	GetSymbol[syms,sym]
]
reDef@
XiEvenRSymbol[opts:OptionsPattern[]]:=XiEvenRSymbol[DefaultSymbols[],opts];


def@
XiOddSymbol[syms_Association,opts:OptionsPattern[]]:=
Module[{tp,sym},

	TestOptions[xiTests,{opts}];
	
	tp=OptionValue[TagPosition];
	
	sym=formSymbol["xi",Tag->"OddTag",TagPosition->tp];
	GetSymbol[syms,sym]
]
reDef@
XiOddSymbol[opts:OptionsPattern[]]:=XiOddSymbol[DefaultSymbols[],opts];


(*                     *)
(* Spherical Harmonics *)
(*                     *)


def@
YSymbol[syms_Association,opts:OptionsPattern[]]:=
Module[{bar,sym},

	TestOptions[{"Conjugate"->BooleanQ},{opts}];

	bar=If[OptionValue[Conjugate],"Bar",Null];

	sym=formSymbol["Y"];
	GetSymbol[syms,sym,OverStr->bar]
]
reDef@
YSymbol[opts:OptionsPattern[]]:=YSymbol[DefaultSymbols[],opts];


def@
YThetaSymbol[syms_Association,opts:OptionsPattern[]]:=
Module[{bar,sym},

	TestOptions[{"Conjugate"->BooleanQ},{opts}];

	bar=If[OptionValue[Conjugate],"Bar",Null];

	sym=formSymbol["Y",{"theta"},Indices->"Down"];
	GetSymbol[syms,sym,OverStr->bar]
]
reDef@
YThetaSymbol[opts:OptionsPattern[]]:=YThetaSymbol[DefaultSymbols[],opts];


def@
YPhiSymbol[syms_Association,opts:OptionsPattern[]]:=
Module[{bar,sym},

	TestOptions[{"Conjugate"->BooleanQ},{opts}];

	bar=If[OptionValue[Conjugate],"Bar",Null];

	sym=formSymbol["Y",{"phi"},Indices->"Down"];
	GetSymbol[syms,sym,OverStr->bar]
]
reDef@
YPhiSymbol[opts:OptionsPattern[]]:=YPhiSymbol[DefaultSymbols[],opts];


def@
YThetaThetaSymbol[syms_Association,opts:OptionsPattern[]]:=
Module[{bar,sym},

	TestOptions[{"Conjugate"->BooleanQ},{opts}];

	bar=If[OptionValue[Conjugate],"Bar",Null];

	sym=formSymbol["Y",{"theta","theta"},Indices->"Down"];
	GetSymbol[syms,sym,OverStr->bar]
]
reDef@
YThetaThetaSymbol[opts:OptionsPattern[]]:=YThetaThetaSymbol[DefaultSymbols[],opts];


def@
YThetaPhiSymbol[syms_Association,opts:OptionsPattern[]]:=
Module[{bar,sym},

	TestOptions[{"Conjugate"->BooleanQ},{opts}];

	bar=If[OptionValue[Conjugate],"Bar",Null];

	sym=formSymbol["Y",{"theta","phi"},Indices->"Down"];
	GetSymbol[syms,sym,OverStr->bar]
]
reDef@
YThetaPhiSymbol[opts:OptionsPattern[]]:=YThetaPhiSymbol[DefaultSymbols[],opts];


def@
YPhiPhiSymbol[syms_Association,opts:OptionsPattern[]]:=
Module[{bar,sym},

	TestOptions[{"Conjugate"->BooleanQ},{opts}];

	bar=If[OptionValue[Conjugate],"Bar",Null];

	sym=formSymbol["Y",{"phi","phi"},Indices->"Down"];
	GetSymbol[syms,sym,OverStr->bar]
]
reDef@
YPhiPhiSymbol[opts:OptionsPattern[]]:=YPhiPhiSymbol[DefaultSymbols[],opts];


def@
XThetaSymbol[syms_Association,opts:OptionsPattern[]]:=
Module[{bar,sym},

	TestOptions[{"Conjugate"->BooleanQ},{opts}];

	bar=If[OptionValue[Conjugate],"Bar",Null];

	sym=formSymbol["X",{"theta"},Indices->"Down"];
	GetSymbol[syms,sym,OverStr->bar]
]
reDef@
XThetaSymbol[opts:OptionsPattern[]]:=XThetaSymbol[DefaultSymbols[],opts];


def@
XPhiSymbol[syms_Association,opts:OptionsPattern[]]:=
Module[{bar,sym},

	TestOptions[{"Conjugate"->BooleanQ},{opts}];

	bar=If[OptionValue[Conjugate],"Bar",Null];

	sym=formSymbol["X",{"phi"},Indices->"Down"];
	GetSymbol[syms,sym,OverStr->bar]
]
reDef@
XPhiSymbol[opts:OptionsPattern[]]:=XPhiSymbol[DefaultSymbols[],opts];


def@
XThetaThetaSymbol[syms_Association,opts:OptionsPattern[]]:=
Module[{bar,sym},

	TestOptions[{"Conjugate"->BooleanQ},{opts}];

	bar=If[OptionValue[Conjugate],"Bar",Null];

	sym=formSymbol["X",{"theta","theta"},Indices->"Down"];
	GetSymbol[syms,sym,OverStr->bar]
]
reDef@
XThetaThetaSymbol[opts:OptionsPattern[]]:=XThetaThetaSymbol[DefaultSymbols[],opts];


def@
XThetaPhiSymbol[syms_Association,opts:OptionsPattern[]]:=
Module[{bar,sym},

	TestOptions[{"Conjugate"->BooleanQ},{opts}];

	bar=If[OptionValue[Conjugate],"Bar",Null];

	sym=formSymbol["X",{"theta","phi"},Indices->"Down"];
	GetSymbol[syms,sym,OverStr->bar]
]
reDef@
XThetaPhiSymbol[opts:OptionsPattern[]]:=XThetaPhiSymbol[DefaultSymbols[],opts];


def@
XPhiPhiSymbol[syms_Association,opts:OptionsPattern[]]:=
Module[{bar,sym},

	TestOptions[{"Conjugate"->BooleanQ},{opts}];

	bar=If[OptionValue[Conjugate],"Bar",Null];

	sym=formSymbol["X",{"phi","phi"},Indices->"Down"];
	GetSymbol[syms,sym,OverStr->bar]
]
reDef@
XPhiPhiSymbol[opts:OptionsPattern[]]:=XPhiPhiSymbol[DefaultSymbols[],opts];


(*       *)
(* Other *)
(*       *)


def@
DiracDeltaSymbol[syms_Association]:=GetSymbol[syms,"DiracDelta"]
reDef@
DiracDeltaSymbol[]:=DiracDeltaSymbol[DefaultSymbols[]];


def@
HeavisideSymbol[syms_Association]:=GetSymbol[syms,"Heaviside"]
reDef@
HeavisideSymbol[]:=HeavisideSymbol[DefaultSymbols[]];


def@
GCoefficientSymbol[syms_Association]:=GetSymbol[syms,"GCoefficient"]
reDef@
GCoefficientSymbol[]:=GCoefficientSymbol[DefaultSymbols[]];


def@
FCoefficientSymbol[syms_Association]:=GetSymbol[syms,"FCoefficient"]
reDef@
FCoefficientSymbol[]:=FCoefficientSymbol[DefaultSymbols[]];


def@
ECoefficientSymbol[syms_Association]:=GetSymbol[syms,"ECoefficient"]
reDef@
ECoefficientSymbol[]:=ECoefficientSymbol[DefaultSymbols[]];


def@
XSymbol[syms_Association]:=GetSymbol[syms,"x"]
reDef@
XSymbol[]:=XSymbol[DefaultSymbols[]];


def@
XPNSymbol[syms_Association]:=
Module[{symxPN,symX},
	
	symxPN=GetSymbol[syms,"xPN"];
	symX=XSymbol[syms];

	Format[symxPN]=ToString[Subscript[symX, "PN"],StandardForm];

	symxPN
]
reDef@
XPNSymbol[]:=XPNSymbol[DefaultSymbols[]];


def@
CapitalOmegaSymbol[syms_Association]:=GetSymbol[syms,"CapitalOmega"]
reDef@
CapitalOmegaSymbol[]:=CapitalOmegaSymbol[DefaultSymbols[]];


def@
OmegaPhiSymbol[syms_Association]:=
Module[{symOmPhi,symOm,symPhi},
	
	symOmPhi=GetSymbol[syms,"OmegaPhi"];
	symOm=CapitalOmegaSymbol[syms];
	symPhi=PhiSymbol[syms];

	Format[symOmPhi]=ToString[Subscript[symOm, symPhi],StandardForm];

	symOmPhi
]
reDef@
OmegaPhiSymbol[]:=OmegaPhiSymbol[DefaultSymbols[]];


def@
OmegaRSymbol[syms_Association]:=
Module[{symOmR,symOm,symR},
	
	symOmR=GetSymbol[syms,"OmegaR"];
	symOm=CapitalOmegaSymbol[syms];
	symR=RSymbol[syms];

	Format[symOmR]=ToString[Subscript[symOm, symR],StandardForm];

	symOmR
]
reDef@
OmegaRSymbol[]:=OmegaRSymbol[DefaultSymbols[]];


def@
ParityAndVariable[par_,var_]:=
If[MemberQ[masterVars,var],
	Switch[par,
		"Even",
		{"Even",Which[var===Default||var==="ZM0","ZM",MemberQ[evenVars,var],var,True,Print["Variable ", var, " is not Even parity."];Abort[]]},
		"Odd",
		{"Odd",Which[var===Default||var==="CPM0","CPM",MemberQ[oddVars,var],var,True,Print["Variable ", var, " is not Odd parity."];Abort[]]},
		Default,
		Which[var===Default,{"Even","ZM"},MemberQ[evenVars,var],{"Even",var},MemberQ[oddVars,var],{"Odd",var}],
		_,
		Print["Unknown parity ", par];
		Abort[]
	]/.{"ZM0"->"ZM","JT0"->"JT","CPM0"->"CPM"},
	Print["Unknown variable ", var];
	Abort[]
]


def@
DefaultSymbols[]:=
Module[{symbols,symbolsGlobal,strings,symbolsStrs,syms},

	symbols={"Apoapsis"->"rMax","BlackHoleMass"->"M","BlackHoleSpin"->"a","CapitalOmega"->"\[CapitalOmega]","chi"->"\[Chi]",
				"DeltaPhi"->"\[CapitalDelta]\[Phi]","DiracDelta"->"\[Delta]", "EvenTag"->"e", "OddTag"->"o","SpinWeight"->"s","Flat"->"\[Flat]","Frequency"->"\[Omega]","G"->"\[ScriptCapitalG]",
				"GCoefficient"->"G","GUTag"->"GU","hBarackSago"->"hBS","Heaviside"->"\[Theta]","hEven"->"h","hOdd"->"h","ImpactParameter"->"b",
				"InvariantTag"->"GI","j"->"j","K"->"\[ScriptCapitalK]","l"->"l","lambda"->"\[Lambda]","LambdaSpin"->"\[Lambda]s","LorenzTag"->"L",
				"m"->"m","MasterFunction"->"X","ModRWZTag"->"MRWZ","n"->"n","OmegaPhi"->"\[CapitalOmega]\[Phi]","OmegaR"->"\[CapitalOmega]r","OrbitalEccentricity"->"e","p"->"p",
				"P"->"P","ParticleMass"->"\[Mu]","Periapsis"->"rMin","phi"->"\[Phi]","phip"->"\[Phi]p", "q"->"q","Q"->"Q",
				"r"->"r","RadialTeukolsky"->"R","rho"->"\[Rho]","rp"->"rp",
				"rStar"->"rStar","RWZTag"->"RWZ","SasakiNakamuraFunction"->"XSN",
				"SemiLatusRectum"->"p","Sharp"->"\[Sharp]","SpecificAngularMomentum"->"\[ScriptCapitalL]",
				"SpecificEnergy"->"\[ScriptCapitalE]","SpinWeightedHarmonic"->"Slm","t"->"t",
				"TeukolskyGravity"->"\[Psi]","TeukolskyEM"->"\[Phi]","TeukolskyScalar"->"\[CapitalPhi]","TeukolskyNeutrino"->"\[Chi]","theta"->"\[Theta]","thetap"->"\[Theta]p",
				"x"->"x","X"->"X","xi"->"\[Xi]","xPN"->"xPN","Y"->"Y"};
				
	strings={"ECoefficient"->"E","FCoefficient"->"F","Bar"->"_","MinusTag"->"-","PlusTag"->"+","2Tag"->"2"};

	symbolsStrs=symbols/.(str_String->symStr_String):>str->"Global`"<>symStr;
	symbolsGlobal=symbolsStrs/.(str_String->symStr_String):>str->Symbol[symStr];
	
	syms=Association[Join[symbolsGlobal,strings]];
	DefineFormats[syms];
	syms
]


Clear[formSymbol];
Options[formSymbol]={"Indices"->"Down","TagPosition"->"Up","Tag"->Null};
formSymbol[head_String,indices_List:{},opts:OptionsPattern[]]:=
formSymbol[head,indices,opts]=
Module[{tagStr,indPos,tp,partSym},

	TestOptions[fieldTestsIndices,{opts},formSymbol];
	
	indPos=OptionValue[Indices];
	tagStr=OptionValue[Tag];
	tp=OptionValue[TagPosition];

	If[Length@indices>2,
		Print["More than two indices given to formSymbol."];
		Aborting[]
	];

	partSym=
	If[Length@indices==0,
		head,
		head["Indices"->{indPos->indices}]
	];

	If[tagStr===Null,
		partSym,
		partSym["Tags"->{tp->{tagStr}}]
	]
]


def@
SymbolFunction[label_String]:=
Module[{assoc},

	assoc=Association[HttLabel[]->HttSymbol,
		HtrLabel[]->HtrSymbol,
		HrrLabel[]->HrrSymbol,
		JtLabel[]->JtSymbol,
		JrLabel[]->JrSymbol,
		KLabel[]->KSymbol,
		GLabel[]->GSymbol,
		HtLabel[]->HtSymbol,
		HrLabel[]->HrSymbol,
		H2Label[]->H2Symbol,
		QttLabel[]->QttSymbol,
		QtrLabel[]->QtrSymbol,
		QrrLabel[]->QrrSymbol,
		QtLabel[]->QtSymbol,
		QrLabel[]->QrSymbol,
		QSharpLabel[]->QSharpSymbol,
		QFlatLabel[]->QFlatSymbol,
		PtLabel[]->PtSymbol,
		PrLabel[]->PrSymbol,
		PLabel[]->PSymbol,
		XiEvenLabel[]->XiEvenSymbol,
		XiEvenTLabel[]->XiEvenTSymbol,
		XiEvenRLabel[]->XiEvenRSymbol,
		XiOddLabel[]->XiOddSymbol,
		YLabel[]->YSymbol,
		YThetaLabel[]->YThetaSymbol,
		YPhiLabel[]->YPhiSymbol,
		YThetaThetaLabel[]->YThetaThetaSymbol,
		YThetaPhiLabel[]->YThetaPhiSymbol,
		YPhiPhiLabel[]->YPhiPhiSymbol,
		XThetaLabel[]->XThetaSymbol,
		XPhiLabel[]->XPhiSymbol,
		XThetaThetaLabel[]->XThetaThetaSymbol,
		XThetaPhiLabel[]->XThetaPhiSymbol,
		XPhiPhiLabel[]->XPhiPhiSymbol];

	assoc[label]
]


End[];

EndPackage[];
