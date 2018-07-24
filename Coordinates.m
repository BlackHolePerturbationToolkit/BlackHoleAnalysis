(* ::Package:: *)

BeginPackage["BlackHoleAnalysis`Coordinates`",
				{"BlackHoleAnalysis`OverloadedSymbols`",
					"BlackHoleAnalysis`Utils`",
					"BlackHoleAnalysis`Symbols`",
					"BlackHoleAnalysis`Labels`",
					"BlackHoleAnalysis`ValidityTests`",
					"GeneralUtilities`"}];


(*******************************************************************)
(* Public functions *)
(*******************************************************************)
SchwarzschildMetric::usage="SchwarzschildMetric[] returns the Schwarzschild metric in Schwarzschild coordinates.";
KerrMetric::usage="KerrMetric[] returns the Kerr metric in Boyer-Lindquist coordinates.";
RpDotDot::usage="RpDotDot[] returns the second coordinate time derivative of the r position of \
a particle in geodesic motion about a black hole.";
RpDotSquared::usage="RpDotSquared[] returns the square of the first coordinate time derivative of the r position of \
a particle in geodesic motion about a black hole.";
USquared::usage="USquared[] returns U^2, the effective potential that shows up in the Schwarzschild geodesic equations.";


RStarOfR::usage="RStarOfR[r,M] returns the usual tortoise coordinate in terms of Schwarzschild r and \
black hole mass M, normalized such that r=r_*=4M.
RStarOfR[r,M,a] returns the Kerr metric tortoise coordinate in terms of Boyer-Lindquist r, \
mass M, and spin parameter a.
RStarOfR[] returns RStarOfR[r,M] using built-in symbols.";
ROfRStar::usage="ROfRStar[r_*,M] returns Schwarzschild r in terms of the usual tortoise coordinate and \
black hole mass M, normalized such that r=r_*=4M.
ROfRStar[] returns ROfRStar[r_*,M] using built-in symbols.";

LambdaOfL::usage="LambdaOfL[l] returns (l+2)(l-1)/2
LambdaOfL[] returns LambdaOfL[l] using built-in symbols.";

SchwarzschildF::usage="SchwarzschildF[r,M] returns the Schwarzschild redshift 1-2M/r.
SchwarzschildF[] returns SchwarzschildF[r,M] using built-in symbols.";
CapitalLambda::usage="CapitalLambda[r,M,lambda] returns the Capital Lambda quantity lambda + 3M/r  where \
M is the black hole mass, r the Schwarzschild radius and lambda is (l+2)(l-1)/2.
CapitalLambda[] returns CapitalLambda[r,M,lambda] using built-in symbols.";


RPlusKerr::usage="RPlusKerr[] returns r+ in terms of the Kerr black hole's mass and spin.";
RMinusKerr::usage="RMinusKerr[] returns r- in terms of the Kerr black hole's mass and spin.";


DeltaKerr::usage="DeltaKerr[] returns the Kerr quantity Capital Delta.";
SigmaKerr::usage="SigmaKerr[] returns the Kerr quantity Capital Sigma.";
KKerr::usage="KKerr[] returns the Kerr quantity K.";


DPhiDChi::usage="DPhiDChi[] returns the Darwin's expression for dphi/dchi.";
DTDChi::usage="DTDChi[] returns the Darwin's expression for dt/dchi.";
DTauDChi::usage="DTauDChi[] returns the Darwin's expression for dtau/dchi.";


Separatrix::usage="Separatrix[] returns the an expression for the separatrix, the border between \
bound and unbound orbits."


Begin["`Private`"];


Options[SchwarzschildMetric]={"Indices"->"Down","AtParticle"->False,"Equatorial"->True};
DocumentationBuilder`OptionDescriptions["SchwarzschildMetric"] = 
{
    "Indices"->"States whether the indices should be covariant (\"Down\") or contravariant (\"Up\")",
	"AtParticle"->"Boolean stating whether to evaluate the metric components at the location of the particle, \
r=rp[t] and theta = \[Theta]p[t]",
"Equatorial"->"Boolean stating whether the particle will be confined to \[Theta]p[t] = \[Pi]/2"
};


Options[TwoSphereMetric]={"Indices"->"Down","AtParticle"->False,"Equatorial"->True};
DocumentationBuilder`OptionDescriptions["TwoSphereMetric"] = 
{
    "Indices"->"States whether the indices should be covariant (\"Down\") or contravariant (\"Up\")",
	"AtParticle"->"Boolean stating whether to evaluate the metric components at the location of the particle, \
r=rp[t] and theta = \[Theta]p[t]",
"Equatorial"->"Boolean stating whether the particle will be confined to \[Theta]p[t] = \[Pi]/2"
};


Options[KerrMetric]={"Indices"->"Down","AtParticle"->False,"Equatorial"->False};
DocumentationBuilder`OptionDescriptions["KerrMetric"] = 
{
    "Indices"->"States whether the indices should be covariant (\"Down\") or contravariant (\"Up\")",
	"AtParticle"->"Boolean stating whether to evaluate the metric components at the location of the particle, \
r=rp[t] and theta = \[Theta]p[t]",
"Equatorial"->"Boolean stating whether the particle will be confined to \[Theta]p[t] = \[Pi]/2"
};


Options[FourVelocity]={"Metric"->"Schwarzschild"};
DocumentationBuilder`OptionDescriptions["FourVelocity"] = {"Metric" -> "Which spacetime the particle orbits in (\"Schwarzschild of \"Kerr\"\")."};
Options[CoordinateVelocity]=Options[FourVelocity];
DocumentationBuilder`OptionDescriptions["CoordinateVelocity"] = DocumentationBuilder`OptionDescriptions["FourVelocity"];


Options[DPhiDChi]=Options[FourVelocity];
DocumentationBuilder`OptionDescriptions["DPhiDChi"] = DocumentationBuilder`OptionDescriptions["FourVelocity"];
Options[DTDChi]=Options[FourVelocity];
DocumentationBuilder`OptionDescriptions["DTDChi"] = DocumentationBuilder`OptionDescriptions["FourVelocity"];
Options[DTauDChi]=Options[FourVelocity];
DocumentationBuilder`OptionDescriptions["DTauDChi"] = DocumentationBuilder`OptionDescriptions["FourVelocity"];


Options[Separatrix]=Options[FourVelocity];
DocumentationBuilder`OptionDescriptions["Separatrix"] = DocumentationBuilder`OptionDescriptions["FourVelocity"];


Options[RStarOfRho]={"IntegrationConstant"->{4 BlackHoleMassSymbol[DefaultSymbols[]],4 BlackHoleMassSymbol[DefaultSymbols[]]}};
DocumentationBuilder`OptionDescriptions["RStarOfRho"] = {"IntegrationConstant" -> "Constant chosen when defining Schwarzschild tortoise coordinate. Either given explicitly, or \
as a list {r,r_*} specifying where r=r_*."};


Options[RStarOfR]=Join[Options[FourVelocity],Options[RStarOfRho]];
DocumentationBuilder`OptionDescriptions["RStarOfR"] = Join[DocumentationBuilder`OptionDescriptions["FourVelocity"],
DocumentationBuilder`OptionDescriptions["RStarOfRho"]];


Options[RhoOfRStar]=Options[RStarOfRho];
DocumentationBuilder`OptionDescriptions["RhoOfRStar"]=DocumentationBuilder`OptionDescriptions["RStarOfRho"];


Options[ROfRStar]=Options[RStarOfRho];
DocumentationBuilder`OptionDescriptions["ROfRStar"]=DocumentationBuilder`OptionDescriptions["RStarOfRho"];


Options[SpecificEnergy]=Options[FourVelocity];
DocumentationBuilder`OptionDescriptions["SpecificEnergy"] = DocumentationBuilder`OptionDescriptions["FourVelocity"];
Options[SpecificAngularMomentum]=Options[FourVelocity];
DocumentationBuilder`OptionDescriptions["SpecificAngularMomentum"] = DocumentationBuilder`OptionDescriptions["FourVelocity"];


Options[RpDotDot]=Options[FourVelocity];
DocumentationBuilder`OptionDescriptions["RpDotDot"] = DocumentationBuilder`OptionDescriptions["FourVelocity"];
Options[RpDotSquared]=Options[FourVelocity];
DocumentationBuilder`OptionDescriptions["RpDotSquared"] = DocumentationBuilder`OptionDescriptions["FourVelocity"];


Options[DRDTau]=Options[FourVelocity];
Options[DTDTau]=Options[FourVelocity];
Options[DPhiDTau]=Options[FourVelocity];
Options[DRpDTauOfChi]=Options[FourVelocity];


rangeTests={"Range" ->Function[x,MemberQ[{"All","Most","Half","MostHalf"},x]]};
metricTests={"Metric" ->Function[x,MemberQ[{"Schwarzschild","Kerr"},x]]};


metricQ="Metric" ->Function[x,MemberQ[{"Schwarzschild","Kerr"},x]];
intConstQ="IntegrationConstant"->Function[x,MatchQ[x,__]];


def@
SchwarzschildMetric[syms_Association,opts:OptionsPattern[]]:=
Module[{ff,t,r,rp,th,diag,diagR,inds,atP,optionsRules,equator,thp,thpVal},

	optionsRules={"Indices"->Function[x,MemberQ[{"Up","Down"},x]],
				"AtParticle"->BooleanQ,
				"Equatorial"->BooleanQ};
	TestOptions[optionsRules,{opts}];

	inds=OptionValue["Indices"];
	atP=OptionValue["AtParticle"];
	equator=OptionValue["Equatorial"];

	ff=SchwarzschildF[syms];
	r=RSymbol[syms];
	rp=RpSymbol[syms];
	t=TSymbol[syms];
	th=ThetaSymbol[syms];
	thp=ThetaPSymbol[syms];
	thpVal=If[equator,\[Pi]/2,thp[t]];

	diagR={-ff,1/ff,r^2,r^2 Sin[th]^2};
	diag=If[atP,#/.{r->rp[t],th->thpVal},#]&@diagR;

	DiagonalMatrix[If[inds=="Up",1/#,#]&@diag]
]
reDef@
SchwarzschildMetric[opts:OptionsPattern[]]:=SchwarzschildMetric[DefaultSymbols[],opts]


def@
TwoSphereMetric[syms_Association,opts:OptionsPattern[]]:=
Module[{t,th,thp,thpVal,diag,diagR,inds,atP,optionsRules,equator},

	optionsRules={"Indices"->Function[x,MemberQ[{"Up","Down"},x]],
					"AtParticle"->BooleanQ,
				"Equatorial"->BooleanQ};
	TestOptions[optionsRules,{opts}];

	inds=OptionValue["Indices"];
	atP=OptionValue["AtParticle"];
	equator=OptionValue["Equatorial"];

	t=TSymbol[syms];
	th=ThetaSymbol[syms];
	thp=ThetaPSymbol[syms];
	thpVal=If[equator,\[Pi]/2,thp[t]];

	diagR={1, Sin[th]^2};
	diag=If[atP,#/.{r->rp[t],th->thpVal},#]&@diagR;

	DiagonalMatrix[If[inds=="Up",1/#,#]&@diag]
]
reDef@
TwoSphereMetric[opts:OptionsPattern[]]:=TwoSphereMetric[DefaultSymbols[],opts]


def@
KerrMetric[syms_Association,opts:OptionsPattern[]]:=
Module[{r,a,th,optionsRules,equator,thp,thpVal,atP,delta,sigma,gK,indices,matrixR,matrix,coords,t,rp},
	optionsRules={"Indices"->Function[x,MemberQ[{"Up","Down"},x]],
				"AtParticle"->BooleanQ,
				"Equatorial"->BooleanQ};

	TestOptions[optionsRules,{opts}];
	indices=OptionValue["Indices"];
	atP=OptionValue["AtParticle"];
	equator=OptionValue["Equatorial"];
		
	r=RSymbol[syms];
	rp=RpSymbol[syms];
	t=TSymbol[syms];
	a=BlackHoleSpinSymbol[syms];
	th=ThetaSymbol[syms];
	thp=ThetaPSymbol[syms];
	thpVal=If[equator,\[Pi]/2,thp[t]];

	delta=DeltaKerr[syms];
	sigma=SigmaKerr[syms];
	coords={"t","r","th","phi"};
	gK["t","t"]=(-delta+a^2 Sin[th]^2)/sigma;
	gK["r","r"]=sigma/delta;
	gK["th","th"]=sigma;
	gK["phi","phi"]=(Sin[th]^2 ((a^2+r^2)^2-a^2 delta Sin[th]^2))/sigma;
	gK["t","phi"]=-((a Sin[th]^2 (a^2+r^2-delta))/sigma);
	gK["phi","t"]=gK["t","phi"];
	gK[_,_]=0;

	matrixR=Table[gK[mu,nu],{mu,coords},{nu,coords}];
	matrix=If[atP,#/.{r->rp[t],th->thpVal},#]&@matrixR;
	If[indices=="Up",Inverse@#,#]&@matrix
]
reDef@
KerrMetric[opts:OptionsPattern[]]:=KerrMetric[DefaultSymbols[],opts]


def@
ROfChi[syms_Association]:=
Module[{e,p,M,chi},
	
	chi=ChiSymbol[syms];
	p =SemiLatusRectumSymbol[syms];
	e =OrbitalEccentricitySymbol[syms];
	M =BlackHoleMassSymbol[syms];
	
	(p M)/(1+e Cos[chi])
]
reDef@
ROfChi[]:=ROfChi[DefaultSymbols[]]


def@
PhiOfChi[syms_Association,opts:OptionsPattern[]]:=
Module[{e,p,chi},

	TestOptions[metricTests,{opts}];

	If[OptionValue["Metric"]==="Schwarzschild",
		chi=ChiSymbol[syms];
		p =SemiLatusRectumSymbol[syms];
		e =OrbitalEccentricitySymbol[syms];
	
		2 Sqrt[p/(-6 - 2 e + p)] EllipticF[chi/2, (4 e)/(6 + 2 e - p)],
		
		Print["Exact phi(chi) is only known for Schwarzschild metric"];
		Aborting[syms]
	]
]
reDef@
PhiOfChi[opts:OptionsPattern[]]:=PhiOfChi[DefaultSymbols[],opts]


def@
DPhiDChi[syms_Association,opts:OptionsPattern[]]:=
Module[{e,p,chi},

	TestOptions[{metricQ},{opts}];

	Which[OptionValue["Metric"]==="Schwarzschild",
		chi=ChiSymbol[syms];
		p =SemiLatusRectumSymbol[syms];
		e =OrbitalEccentricitySymbol[syms];
	
		Sqrt[p/(p-6-2e Cos[chi])],

		OptionValue["Metric"]==="Kerr",
		KerrPotentialPhi[syms]/(KerrJ[syms]Sqrt[KerrPotentialR[syms]]),

		True,
		Print["No metric ", OptionValue["Metric"]];
		Aborting[syms]
	]
]
reDef@
DPhiDChi[opts:OptionsPattern[]]:=DPhiDChi[DefaultSymbols[],opts]


def@
DTDChi[syms_Association,opts:OptionsPattern[]]:=
Module[{e,p,chi,M},
	
	TestOptions[{metricQ},{opts}];

	Which[OptionValue["Metric"]==="Schwarzschild",
		chi=ChiSymbol[syms];
		p =SemiLatusRectumSymbol[syms];
		e =OrbitalEccentricitySymbol[syms];
		M =BlackHoleMassSymbol[syms];
	
		(p^2 M)/((p-2-2e Cos[chi])(1+e Cos[chi])^2) Sqrt[((p-2)^2-4e^2)/(p-6-2e Cos[chi])],

		OptionValue["Metric"]==="Kerr",
		KerrPotentialT[syms]/(KerrJ[syms]Sqrt[KerrPotentialR[syms]]),

		True,
		Print["No metric ", OptionValue["Metric"]];
		Aborting[syms]
	]
]
reDef@
DTDChi[opts:OptionsPattern[]]:=DTDChi[DefaultSymbols[],opts]


def@
DTauDChi[syms_Association,opts:OptionsPattern[]]:=
Module[{e,p,chi,M,t,EE,JJ,dtdtau,rp},

	TestOptions[{metricQ},{opts}];

	Which[OptionValue["Metric"]==="Schwarzschild",
		
		chi=ChiSymbol[syms];
		p =SemiLatusRectumSymbol[syms];
		e =OrbitalEccentricitySymbol[syms];
		M =BlackHoleMassSymbol[syms];
	
		(p^(3/2) M)/(1+e Cos[chi])^2 Sqrt[(p-3-e^2)/(p-6-2e Cos[chi])],
		
		OptionValue["Metric"]==="Kerr",
		JJ=SpecificAngularMomentumSymbol[syms];
		EE=SpecificEnergySymbol[syms];
		rp=RpSymbol[syms];
		t=TSymbol[syms];
		dtdtau=DTDTau[syms,opts]/.{JJ->SpecificAngularMomentum[syms,opts],
									EE->SpecificEnergy[syms,opts],
									rp[t]->ROfChi[syms]};
		DTDChi[syms,opts] / dtdtau,

		True,
		Print["No metric ", OptionValue["Metric"]];
		Aborting[syms]
	]
]
reDef@
DTauDChi[opts:OptionsPattern[]]:=DTauDChi[DefaultSymbols[],opts]


def@
DRDT[syms_Association,n_Integer/;n>=1,opts:OptionsPattern[]]:=
Module[{dtdchi,drdchi,drdt,chi},
	
	TestOptions[metricTests,{opts}];

	chi=ChiSymbol[syms];
	dtdchi=DTDChi[syms,opts];
	drdchi=D[ROfChi[syms],chi];
	drdt=drdchi/dtdchi;
	
	Nest[D[#,chi]/dtdchi&,drdt,n-1]
]
reDef@
DRDT[syms_Association,opts:OptionsPattern[]]:=DRDT[syms,1,opts]
reDef@
DRDT[n_Integer/;n>=1,opts:OptionsPattern[]]:=DRDT[DefaultSymbols[],n,opts]
reDef@
DRDT[opts:OptionsPattern[]]:=DRDT[DefaultSymbols[],opts]


def@
DPhiDT[syms_Association,n_Integer/;n>=1,opts:OptionsPattern[]]:=
Module[{dtdchi,dphidchi,dphidt,chi},
	
	TestOptions[metricTests,{opts}];

	chi=ChiSymbol[syms];
	dtdchi=DTDChi[syms,opts];
	dphidchi=DPhiDChi[syms,opts];
	dphidt=dphidchi/dtdchi;
	
	Nest[D[#,chi]/dtdchi&,dphidt,n-1]
]
reDef@
DPhiDT[syms_Association,opts:OptionsPattern[]]:=DPhiDT[syms,1,opts]
reDef@
DPhiDT[n_Integer/;n>=1,opts:OptionsPattern[]]:=DPhiDT[DefaultSymbols[],n,opts]
reDef@
DPhiDT[opts:OptionsPattern[]]:=DPhiDT[DefaultSymbols[],opts]


def@
DRDTau[syms_Association,opts:OptionsPattern[]]:=RpSymbol[syms]'[TSymbol[syms]] DTDTau[syms,opts]
reDef@
DRDTau[opts:OptionsPattern[]]:=DRDTau[DefaultSymbols[],opts]


def@
DTDTau[syms_Association,opts:OptionsPattern[]]:=
Module[{rp,t,JJ,a,EE,TT,M,r,delta,f},

	TestOptions[{metricQ},{opts}];

	rp = RpSymbol[syms];
	t = TSymbol[syms];
	EE = SpecificEnergySymbol[syms];
	f = SchwarzschildF;
				
	Which[OptionValue["Metric"]==="Schwarzschild",
		M = BlackHoleMassSymbol[syms];
		EE / f[rp[t],M],

		OptionValue["Metric"]==="Kerr",
		JJ=SpecificAngularMomentumSymbol[syms];
		a = BlackHoleSpinSymbol[syms];
		r = RSymbol[syms];
		TT = EE (rp[t]^2+a^2)-JJ a;
		delta=DeltaKerr[syms]/.r->rp[t];

		(-a(a EE - JJ) + ((a^2+rp[t]^2) TT)/delta)/(rp[t]^2),
	
		True,
		Print["No metric ", OptionValue["Metric"]];
		Aborting[syms]
	]
]
reDef@
DTDTau[opts:OptionsPattern[]]:=DTDTau[DefaultSymbols[],opts]


def@
DRpDTauOfChi[syms_Association,opts:OptionsPattern[]]:=
Module[{a,EE,M,chi,x,e,p,drdtau},

	TestOptions[{metricQ},{opts}];

	EE = SpecificEnergy[syms,opts];
	x = KerrX[syms];
	p = SemiLatusRectumSymbol[syms];
	e = OrbitalEccentricitySymbol[syms];
	chi = ChiSymbol[syms];
	a = BlackHoleSpinSymbol[syms];
	M = BlackHoleMassSymbol[syms];

	drdtau=(e Sin[chi])/(M p) (x^2+a^2+2 x a EE-(2 x^2)/p (3+e Cos[chi]))^(1/2);
				
	Which[OptionValue["Metric"]==="Schwarzschild",
		drdtau /. a->0,

		OptionValue["Metric"]==="Kerr",
		drdtau,
	
		True,
		Print["No metric ", OptionValue["Metric"]];
		Aborting[syms]
	]
]
reDef@
DRpDTauOfChi[opts:OptionsPattern[]]:=DRpDTauOfChi[DefaultSymbols[],opts]


def@
DPhiDTau[syms_Association,opts:OptionsPattern[]]:=
Module[{rp,t,JJ,a,EE,TT,r,delta},

	TestOptions[{metricQ},{opts}];

	rp = RpSymbol[syms];
	t = TSymbol[syms];
	JJ=SpecificAngularMomentumSymbol[syms];
		
	Which[OptionValue["Metric"]==="Schwarzschild",
		 JJ/ (rp[t]^2),

		OptionValue["Metric"]==="Kerr",
		EE = SpecificEnergySymbol[syms];
		a = BlackHoleSpinSymbol[syms];
		r = RSymbol[syms];
		TT = EE (rp[t]^2+a^2)-JJ a;
		delta=DeltaKerr[syms]/.r->rp[t];

		(-(a EE - JJ) + (a TT)/delta)/(rp[t]^2)
		,
	
		True,
		Print["No metric ", OptionValue["Metric"]];
		Aborting[syms]
	]
]
reDef@
DPhiDTau[opts:OptionsPattern[]]:=DPhiDTau[DefaultSymbols[],opts]


def@
CoordinatePosition[syms_Association]:={TSymbol[syms],RpSymbol[syms][TSymbol[syms]],\[Pi]/2,PhiPSymbol[syms][TSymbol[syms]]}
reDef@
CoordinatePosition[]:=CoordinatePosition[DefaultSymbols[]]


def@
FourVelocity[syms_Association,opts:OptionsPattern[]]:={DTDTau[syms,opts],DRDTau[syms,opts],0,DPhiDTau[syms,opts]}
reDef@
FourVelocity[opts:OptionsPattern[]]:=FourVelocity[DefaultSymbols[],opts]


def@
CoordinateVelocity[syms_Association,opts:OptionsPattern[]]:=FourVelocity[syms,opts]/DTDTau[syms,opts]
reDef@
CoordinateVelocity[opts:OptionsPattern[]]:=CoordinateVelocity[DefaultSymbols[],opts]


def@
Periapsis[syms_Association] := 
Module[{p, e, M},
	p = SemiLatusRectumSymbol[syms];
	e = OrbitalEccentricitySymbol[syms];
	M = BlackHoleMassSymbol[syms];
	p M/(1+e)
]
reDef@
Periapsis[]:=Periapsis[DefaultSymbols[]]


def@
Apoapsis[syms_Association] := 
Module[{p, e,M},
	p = SemiLatusRectumSymbol[syms];
	e = OrbitalEccentricitySymbol[syms];
	M = BlackHoleMassSymbol[syms];
	p M/(1-e)
]
reDef@
Apoapsis[]:=Apoapsis[DefaultSymbols[]]


def@
SemiLatusRectum[syms_Association] :=
Module[{mass,rMin, rMax},
	rMin = PeriapsisSymbol[syms];
	rMax = ApoapsisSymbol[syms];
	mass = BlackHoleMassSymbol[syms];

	2rMin rMax/(mass(rMax+rMin))
]
reDef@
SemiLatusRectum[]:=SemiLatusRectum[DefaultSymbols[]]


def@
OrbitalEccentricity[syms_Association] :=
Module[{rMin, rMax},
	rMin = PeriapsisSymbol[syms];
	rMax = ApoapsisSymbol[syms];

	(rMax-rMin)/(rMax+rMin)
]
reDef@
OrbitalEccentricity[]:=OrbitalEccentricity[DefaultSymbols[]]


def@
SpecificEnergy[syms_Association,opts:OptionsPattern[]] := 
Module[{p,e,a,M,pM,x},

	TestOptions[{metricQ},{opts}];

	Which[OptionValue["Metric"]==="Schwarzschild",
		p = SemiLatusRectumSymbol[syms];
		e = OrbitalEccentricitySymbol[syms];
		Sqrt[((p-2-2e)(p-2+2e))/(p(p-3-e^2))],

		OptionValue["Metric"]==="Kerr",

		a=BlackHoleSpinSymbol[syms];
		M=BlackHoleMassSymbol[syms];
		p=SemiLatusRectumSymbol[syms];
		e=OrbitalEccentricitySymbol[syms];
		x=KerrX[syms];
		pM=p M;

		Sqrt[1-M/pM (1-e^2)(1-x^2/pM^2 (1-e^2))],

		True,
		Print["No metric ", OptionValue["Metric"]];
		Aborting[syms]
	]
]
reDef@
SpecificEnergy[opts:OptionsPattern[]]:=SpecificEnergy[DefaultSymbols[],opts]


def@
SpecificAngularMomentum[syms_Association,opts:OptionsPattern[]] := 
Module[{p, e, mass,x,a,EE},

	TestOptions[{metricQ},{opts}];

	Which[OptionValue["Metric"]==="Schwarzschild",
		p = SemiLatusRectumSymbol[syms];
		e = OrbitalEccentricitySymbol[syms];
		mass = BlackHoleMassSymbol[syms];
		Sqrt[(p^2 mass^2)/(p-3-e^2)],

		OptionValue["Metric"]==="Kerr",

		a=BlackHoleSpinSymbol[syms];
		x=KerrX[syms];
		EE=SpecificEnergy[syms,opts];
		x+a EE,

		True,
		Print["No metric ", OptionValue["Metric"]];
		Aborting[syms]
	]
]
reDef@
SpecificAngularMomentum[opts:OptionsPattern[]]:=SpecificAngularMomentum[DefaultSymbols[],opts]


def@
USquared[syms_Association]:=
Module[{t,rp,M,JJ,f},

	t =TSymbol[syms];
	rp =RpSymbol[syms];
	M =BlackHoleMassSymbol[syms];
	JJ=SpecificAngularMomentumSymbol[syms];
	f = SchwarzschildF;

	f[rp[t],M](1+JJ^2/rp[t]^2)
];
reDef@
USquared[]:=USquared[DefaultSymbols[]]


def@
RpDotDot[syms_Association,opts:OptionsPattern[]]:=
Module[{rp,t,M,JJ,EE,a,f},

	TestOptions[{metricQ},{opts}];

	t =TSymbol[syms];
	rp =RpSymbol[syms];
	M =BlackHoleMassSymbol[syms];
	a =BlackHoleSpinSymbol[syms];
	JJ=SpecificAngularMomentumSymbol[syms];
	EE=SpecificEnergySymbol[syms];
	f = SchwarzschildF;

	Which[OptionValue["Metric"]==="Schwarzschild",

		2 M f[rp[t],M]/rp[t]^2-f[rp[t],M]^2/(EE^2 rp[t]^2) (3M-JJ^2/rp[t]+(5M JJ^2)/rp[t]^2),

		OptionValue["Metric"]==="Kerr",
		1/(rp[t]^2 (2 a (a EE-JJ) M+a^2 EE rp[t]+EE rp[t]^3)^3) (a^2-2 M rp[t]+rp[t]^2) (-2 a^3 (a EE-JJ)^3 M^2-a (-a EE+JJ)^2 M (3 a^3 EE+4 a EE M^2-4 JJ M^2) rp[t]+a (a^5 (EE-EE^3)-6 a^2 (1+3 EE^2) JJ M^2+24 a EE JJ^2 M^2-10 JJ^3 M^2+a^3 EE (JJ^2+(6+4 EE^2) M^2)) rp[t]^2+a M (-7 a^3 EE+6 a^2 (1+EE^2) JJ-2 a EE (5 JJ^2+6 M^2)+4 (JJ^3+3 JJ M^2)) rp[t]^3-2 (a^4 EE (-1+EE^2)+3 a (3+2 EE^2) JJ M^2-5 EE JJ^2 M^2-a^2 EE (JJ^2+(8+EE^2) M^2)) rp[t]^4+(5 a^2 EE (-2+EE^2)+6 a JJ-7 EE JJ^2) M rp[t]^5+EE (-a^2 (-1+EE^2)+JJ^2+6 M^2) rp[t]^6+EE (-3+2 EE^2) M rp[t]^7)
		,
		True,
		Print["No metric ", OptionValue["Metric"]];
		Aborting[syms]
	]
]
reDef@
RpDotDot[opts:OptionsPattern[]]:=RpDotDot[DefaultSymbols[],opts]


def@
RpDotSquared[syms_Association,opts:OptionsPattern[]]:=
Module[{rp,t,M,EE,JJ,a,f},

	TestOptions[{metricQ},{opts}];

	t =TSymbol[syms];
	rp =RpSymbol[syms];
	M =BlackHoleMassSymbol[syms];
	EE=SpecificEnergySymbol[syms];
	JJ=SpecificAngularMomentumSymbol[syms];
	a = BlackHoleSpinSymbol[syms];
	f = SchwarzschildF;

	Which[OptionValue["Metric"]==="Schwarzschild",
		f[rp[t],M]^2 (1-USquared[syms]/EE^2),

		OptionValue["Metric"]==="Kerr",
		((a^2-2 M rp[t]+rp[t]^2)^2 (2 (-a EE+JJ)^2 M+(a^2 (-1+EE^2)-JJ^2) rp[t]+2 M rp[t]^2+(-1+EE^2) rp[t]^3))/(rp[t] (2 a (a EE-JJ) M+a^2 EE rp[t]+EE rp[t]^3)^2),
		True,
		Print["No metric ", OptionValue["Metric"]];
		Aborting[syms]
	]
]
reDef@
RpDotSquared[opts:OptionsPattern[]]:=RpDotSquared[DefaultSymbols[],opts]


def@
RStarOfR[syms_Association,opts:OptionsPattern[]]:=
Module[{},

	TestOptions[{metricQ,intConstQ},{opts}];

	Which[OptionValue["Metric"]==="Schwarzschild",
		RStarOfR[RSymbol[syms],BlackHoleMassSymbol[syms],FilterRules[{opts},{"IntegrationConstant"}]],
		
		OptionValue["Metric"]==="Kerr",
		RStarOfR[RSymbol[syms],BlackHoleMassSymbol[syms],BlackHoleSpinSymbol[syms]],
	
		True,
		Print["No metric ", OptionValue["Metric"]];
		Aborting[syms]
	]
];

reDef@
RStarOfR[opts:OptionsPattern[]]:=RStarOfR[DefaultSymbols[],opts];

reDef@
RStarOfR[r_,mass_,opts:OptionsPattern[]]:=RStarOfRho[RhoOfR[r,mass],mass,opts];

reDef@
RStarOfR[r_,mass_,a_,opts:OptionsPattern[]]:=RStarKerr[r,mass,a];


def@
RStarKerr[r_,M_,a_]:=r+(2M (M+Sqrt[M^2-a^2]))/((M+Sqrt[M^2-a^2])-(M-Sqrt[M^2-a^2])) Log[(r-(M+Sqrt[M^2-a^2]))/(2M)]-(2M(M-Sqrt[M^2-a^2]))/((M+Sqrt[M^2-a^2])-(M-Sqrt[M^2-a^2])) Log[(r-(M-Sqrt[M^2-a^2]))/(2M)]


def@
rStarConst[mass_,{rVal_,xVal_}]:=xVal-rVal+2mass-2mass Log[(rVal-2mass)/(2mass)]


def@
RStarOfRho[syms_Association,opts:OptionsPattern[]]:=RStarOfRho[RhoSymbol[syms],BlackHoleMassSymbol[syms],opts]

reDef@
RStarOfRho[rho_,mass_,opts:OptionsPattern[]]:=
Module[{constVal,constValN},
	TestOptions[{intConstQ},{opts}];
	
	constVal = If[MatchQ[OptionValue["IntegrationConstant"],{_,_}],
				rStarConst[mass,OptionValue["IntegrationConstant"]],
				OptionValue["IntegrationConstant"]];
	
	constValN = If[NumericQ[mass],constVal/.BlackHoleMassSymbol[DefaultSymbols[]]->mass,constVal];
	
	rho+2 mass Log[rho/(2mass)]+constValN
]

reDef@
RStarOfRho[opts:OptionsPattern[]]:=RStarOfRho[DefaultSymbols[],opts];

reDef@
RStarOfRho[]:=RStarOfRho[DefaultSymbols[]]


def@RhoOfR[r_,mass_]:=r-2mass
reDef@RhoOfR[syms_Association]:=RhoOfR[RSymbol[syms],BlackHoleMassSymbol[syms]]
reDef@
RhoOfR[]:=RhoOfR[DefaultSymbols[]]


def@ROfRho[rho_,mass_]:=rho+2mass
reDef@ROfRho[syms_Association]:=ROfRho[RhoSymbol[syms],BlackHoleMassSymbol[syms]]
reDef@
ROfRho[]:=ROfRho[DefaultSymbols[]]


def@
RhoOfRStar[syms_Association,opts:OptionsPattern[]]:=RhoOfRStar[RStarSymbol[syms],BlackHoleMassSymbol[syms],opts]

reDef@
RhoOfRStar[x_,mass_,opts:OptionsPattern[]]:=
Module[{constVal,constValN},
	TestOptions[{intConstQ},{opts}];
	
	constVal = If[MatchQ[OptionValue["IntegrationConstant"],{_,_}],
				rStarConst[mass,OptionValue["IntegrationConstant"]],
				OptionValue["IntegrationConstant"]];
				
	constValN = If[NumericQ[mass],constVal/.BlackHoleMassSymbol[DefaultSymbols[]]->mass,constVal];
	
	2mass ProductLog[Exp[(x-constValN)/(2mass)]]
]

reDef@
RhoOfRStar[opts:OptionsPattern[]]:=RhoOfRStar[DefaultSymbols[],opts];

reDef@
RhoOfRStar[]:=RhoOfRStar[DefaultSymbols[]]


def@
ROfRStar[syms_Association,opts:OptionsPattern[]]:=
Module[{},
	TestOptions[{intConstQ},{opts}];
	ROfRStar[RStarSymbol[syms],BlackHoleMassSymbol[syms],opts]
];

reDef@
ROfRStar[opts:OptionsPattern[]]:=ROfRStar[DefaultSymbols[],opts];

reDef@
ROfRStar[x_,mass_,opts:OptionsPattern[]]:=ROfRho[RhoOfRStar[x,mass,opts],mass];

reDef@
ROfRStar[]:=ROfRStar[DefaultSymbols[]]


def@SchwarzschildF[r_,M_] := 1 - 2M/ r;
reDef@SchwarzschildF[syms_Association]:=SchwarzschildF[RSymbol[syms],BlackHoleMassSymbol[syms]]
reDef@
SchwarzschildF[]:=SchwarzschildF[DefaultSymbols[]]


def@LambdaOfL[l_]:=(l+2)(l-1)/2;
reDef@LambdaOfL[syms_Association]:=LambdaOfL[LSymbol[syms]];
reDef@
LambdaOfL[]:=LambdaOfL[DefaultSymbols[]]


def@CapitalLambda[r_,M_,la_] := la + 3M / r;
reDef@CapitalLambda[syms_Association]:=CapitalLambda[RSymbol[syms],BlackHoleMassSymbol[syms],LambdaSymbol[syms]]
reDef@
CapitalLambda[]:=CapitalLambda[DefaultSymbols[]]


def@
Separatrix[syms_Association,opts:OptionsPattern[]]:=
Module[{p,e,x,M,a},

	TestOptions[{metricQ},{opts}];

	Which[OptionValue["Metric"]==="Schwarzschild",
		p = SemiLatusRectumSymbol[syms];
		e = OrbitalEccentricitySymbol[syms];
		p-2e-6,
	
		OptionValue["Metric"]==="Kerr",
		p = SemiLatusRectumSymbol[syms];
		e = OrbitalEccentricitySymbol[syms];
		M = BlackHoleMassSymbol[syms];
		a = BlackHoleSpinSymbol[syms];
		x = KerrX[syms];
		
		Simplify[p^2  - x^2 (1+e)(3-e)/M^2,M>0&&a<1&&a>-1]/.Sign[a]->a/Abs[a],

		True,
		Print["No metric ", OptionValue["Metric"]];
		Aborting[syms]
	]
]
reDef@
Separatrix[opts:OptionsPattern[]]:=Separatrix[DefaultSymbols[],opts]


def@
KerrPotentialR[syms_Association]:=
Module[{x,EE,a,e,p,M,pM,chi},
	a=BlackHoleSpinSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	p=SemiLatusRectumSymbol[syms];
	e=OrbitalEccentricitySymbol[syms];
	chi=ChiSymbol[syms];
	pM=p M;
	x=KerrX[syms];
	EE=SpecificEnergy[syms,Metric->"Kerr"];

	x^2+a^2+2a x EE - (2M x^2)/pM (3+e Cos[chi])
]


def@
KerrPotentialPhi[syms_Association]:=
Module[{x,EE,a,e,p,M,pM,chi},
	a=BlackHoleSpinSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	p=SemiLatusRectumSymbol[syms];
	e=OrbitalEccentricitySymbol[syms];
	chi=ChiSymbol[syms];
	pM=p M;
	x=KerrX[syms];
	EE=SpecificEnergy[syms,Metric->"Kerr"];
	
	x+a EE - (2M x)/pM (1+e Cos[chi])
]


def@
KerrPotentialT[syms_Association]:=
Module[{x,EE,a,e,p,M,pM,chi},
	a=BlackHoleSpinSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	p=SemiLatusRectumSymbol[syms];
	e=OrbitalEccentricitySymbol[syms];
	chi=ChiSymbol[syms];
	pM=p M;
	x=KerrX[syms];
	EE=SpecificEnergy[syms,Metric->"Kerr"];
	
	a^2 EE - (2a M x)/pM (1+e Cos[chi])+(EE pM^2)/(1+e Cos[chi])^2
]


def@
KerrX[syms_Association]:=
Module[{N,a,F,C,e,p,M,pM},
	a=BlackHoleSpinSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	p=SemiLatusRectumSymbol[syms];
	e=OrbitalEccentricitySymbol[syms];
	pM=p M;

	F=1/pM^3 (pM^3-2M(3+e^2)pM^2+M^2 (3+e^2)^2 pM-4M a^2 (1-e^2)^2);
	N=2/pM (-M pM^2+(M^2 (3+e^2)-a^2)pM-M a^2 (1+3e^2));
	C=(a^2-M pM)^2;
	Sqrt[(-N-Sign[a]Sqrt[N^2-4F C])/(2F)]
]


def@
KerrJ[syms_Association]:=
Module[{a,chi,e,p,M,pM},
	a=BlackHoleSpinSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	p=SemiLatusRectumSymbol[syms];
	e=OrbitalEccentricitySymbol[syms];
	chi=ChiSymbol[syms];
	pM=p M;

	1-(2M)/pM (1+e Cos[chi])+a^2/pM^2 (1+e Cos[chi])^2

]


def@
RPlusKerr[syms_Association]:=
Module[{M,a},
	M=BlackHoleMassSymbol[syms];
	a=BlackHoleSpinSymbol[syms];
	
	M+Sqrt[M^2-a^2]
];
reDef@
RPlusKerr[]:=RPlusKerr[DefaultSymbols[]]


def@
RMinusKerr[syms_Association]:=
Module[{M,a},
	M=BlackHoleMassSymbol[syms];
	a=BlackHoleSpinSymbol[syms];
	
	M-Sqrt[M^2-a^2]
];
reDef@
RMinusKerr[]:=RMinusKerr[DefaultSymbols[]]


def@
DeltaKerr[syms_Association]:=
Module[{M,a,r},
	M=BlackHoleMassSymbol[syms];
	a=BlackHoleSpinSymbol[syms];
	r=RSymbol[syms];
	
	r^2 - 2M r + a^2
]
reDef@
DeltaKerr[]:=DeltaKerr[DefaultSymbols[]]


def@
SigmaKerr[syms_Association]:=
Module[{th,a,r},
	th=ThetaSymbol[syms];
	a=BlackHoleSpinSymbol[syms];
	r=RSymbol[syms];
	
	r^2 + a^2 Cos[th]^2
]
reDef@
SigmaKerr[]:=SigmaKerr[DefaultSymbols[]]


def@
KKerr[syms_Association]:=
Module[{m,a,r,om},
	m=MSymbol[syms];
	a=BlackHoleSpinSymbol[syms];
	r=RSymbol[syms];
	om=FrequencySymbol[syms];
	
	(r^2+a^2)om-m a
]
reDef@
KKerr[]:=KKerr[DefaultSymbols[]]


End[];

EndPackage[];
