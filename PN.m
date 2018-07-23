(* ::Package:: *)

BeginPackage["BlackHoleAnalysis`PN`",
				{"BlackHoleAnalysis`Utils`",
				"BlackHoleAnalysis`SeriesTools`",
				"BlackHoleAnalysis`OverloadedSymbols`",
				"BlackHoleAnalysis`Coordinates`",
				"BlackHoleAnalysis`Symbols`"}];


RadialPeriodPN::usage="RadialPeriodPN[] returns the radial period of a particle in an eccentric orbit around \
a Schwarzschild black hole, expanded to 1 term using the inverse of p, the semi-latus rectum, as the expansion parameter.
RadialPeriodPN[n] returns the radial period expanded to n terms.
RadialPeriodPN[par] returns the radial period expanded to 1 term, using par (\"p\" or \"x\") as the expansion parameter.
RadialPeriodPN[n,par] returns the radial period expanded to n terms, using par (\"p\" or \"x\") as the expansion parameter.";
OmegaRPN::usage="OmegaRPN[] returns the radial frequency of a particle in an eccentric orbit around \
a Schwarzschild black hole, expanded to 1 term using the inverse of p, the semi-latus rectum, as the expansion parameter.
OmegaRPN[n] returns the radial frequency expanded to n terms.
OmegaRPN[par] returns the radial frequency expanded to 1 term, using par (\"p\" or \"x\") as the expansion parameter.
OmegaRPN[n,par] returns the radial frequency expanded to n terms, using par (\"p\" or \"x\") as the expansion parameter.";
AzimuthalAdvancePN::usage="AzimuthalAdvancePN[] returns the total azimuthal advance during one radial libration of a particle in an eccentric orbit around \
a Schwarzschild black hole, expanded to 1 term using the inverse of p, the semi-latus rectum, as the expansion parameter.
AzimuthalAdvancePN[n] returns the azimuthal advance expanded to n terms.
AzimuthalAdvancePN[par] returns the azimuthal advance expanded to 1 term, using par (\"p\" or \"x\") as the expansion parameter.
AzimuthalAdvancePN[n,par] returns the azimuthal advance expanded to n terms, using par (\"p\" or \"x\") as the expansion parameter.";
OmegaPhiPN::usage="OmegaPhiPN[] returns the phi frequency of a particle in an eccentric orbit around \
a Schwarzschild black hole, expanded to 1 term using the inverse of p, the semi-latus rectum, as the expansion parameter.
OmegaPhiPN[par] returns the phi frequency expanded to 1 term, using par (\"p\" or \"x\") as the expansion parameter.
OmegaPhiPN[n,par] returns the phi frequency expanded to n terms, using par (\"p\" or \"x\") as the expansion parameter.";
SpecificAngularMomentumPN::usage="SpecificAngularMomentumPN[] returns the specific angular momentum of a particle in an eccentric orbit around \
a Schwarzschild black hole, expanded to 1 term using the inverse of p, the semi-latus rectum, as the expansion parameter.
SpecificAngularMomentumPN[n] returns the specific angular momentum expanded to n terms.
SpecificAngularMomentumPN[par] returns the specific angular momentum expanded to 1 term, using par (\"p\" or \"x\") as the expansion parameter.
SpecificAngularMomentumPN[n,par] returns the specific angular momentum expanded to n terms, using par (\"p\" or \"x\") as the expansion parameter.";
SpecificEnergyPN::usage="SpecificEnergyPN[] returns the specific energy of a particle in an eccentric orbit around \
a Schwarzschild black hole, expanded to 1 term using the inverse of p, the semi-latus rectum, as the expansion parameter.
SpecificEnergyPN[n] returns the specific energy expanded to n terms.
SpecificEnergyPN[par] returns the specific energy expanded to 1 term, using par (\"p\" or \"x\") as the expansion parameter.
SpecificEnergyPN[n,par] returns the specific energy expanded to n terms, using par (\"p\" or \"x\") as the expansion parameter.";
SemiLatusRectumPN::usage="SemiLatusRectumPN[] returns the semi-latus rectum of a particle in an eccentric orbit around \
a Schwarzschild black hole, expanded to 1 PN using the inverse of p, the semi-latus rectum, as the expansion parameter.
SemiLatusRectumPN[n] returns the semi-latus rectum of a particle in an eccentric orbit around \
a Schwarzschild black hole, expanded to n PN using the the post-Newtonian parameter x as the expansion parameter.";


Begin["`Private`"];


Options[RadialPeriodPN]=Options[FourVelocity];
DocumentationBuilder`OptionDescriptions["RadialPeriodPN"]=DocumentationBuilder`OptionDescriptions["FourVelocity"];
def@
RadialPeriodPN[syms_Association,takeNum_/;((MatchQ[takeNum,_Integer]&&takeNum>=0)||takeNum===All),expandIn_String,opts:OptionsPattern[]]:=
Module[{seriesFull},
	
	seriesFull = 
	Which[
		OptionValue["Metric"]==="Schwarzschild",
		radialPeriodSchwPN[syms,expandIn],
		
		OptionValue["Metric"]==="Kerr",
		If[expandIn=!="p",Print["Quantities on Kerr are only available as expansions in p"]; Aborting[]];
		radialPeriodKerrPN[syms],

		True,
		Print["No metric ", OptionValue["Metric"]];
		Aborting[syms]
	];
	
	SeriesTake[seriesFull,takeNum]
];
reDef@RadialPeriodPN[syms_Association,takeNum_/;((MatchQ[takeNum,_Integer]&&takeNum>=0)||takeNum===All),opts:OptionsPattern[]]:=RadialPeriodPN[syms,takeNum,"p",opts];
reDef@RadialPeriodPN[syms_Association,expandIn_String,opts:OptionsPattern[]]:=RadialPeriodPN[syms,1,expandIn,opts];
reDef@RadialPeriodPN[syms_Association,opts:OptionsPattern[]]:=RadialPeriodPN[syms,1,"p",opts];

reDef@RadialPeriodPN[takeNum_/;((MatchQ[takeNum,_Integer]&&takeNum>=0)||takeNum===All),expandIn_String,opts:OptionsPattern[]]:=RadialPeriodPN[DefaultSymbols[],takeNum,expandIn,opts]
reDef@RadialPeriodPN[takeNum_/;((MatchQ[takeNum,_Integer]&&takeNum>=0)||takeNum===All),opts:OptionsPattern[]]:=RadialPeriodPN[DefaultSymbols[],takeNum,opts]
reDef@RadialPeriodPN[syms_Association,expandIn_String,opts:OptionsPattern[]]:=RadialPeriodPN[DefaultSymbols[],expandIn,opts];
reDef@RadialPeriodPN[syms_Association,opts:OptionsPattern[]]:=RadialPeriodPN[DefaultSymbols[],opts];


Options[AzimuthalAdvancePN]=Options[FourVelocity];
DocumentationBuilder`OptionDescriptions["AzimuthalAdvancePN"]=DocumentationBuilder`OptionDescriptions["FourVelocity"];
def@
AzimuthalAdvancePN[syms_Association,takeNum_/;((MatchQ[takeNum,_Integer]&&takeNum>=0)||takeNum===All),expandIn_String,opts:OptionsPattern[]]:=
Module[{seriesFull},
	
	seriesFull = 
	Which[
		OptionValue["Metric"]==="Schwarzschild",
		azimuthalAdvanceSchwPN[syms,expandIn],
		
		OptionValue["Metric"]==="Kerr",
		If[expandIn=!="p",Print["Quantities on Kerr are only available as expansions in p"]; Aborting[]];
		azimuthalAdvanceKerrPN[syms],

		True,
		Print["No metric ", OptionValue["Metric"]];
		Aborting[syms]
	];
	
	SeriesTake[seriesFull,takeNum]
];
reDef@AzimuthalAdvancePN[syms_Association,takeNum_/;((MatchQ[takeNum,_Integer]&&takeNum>=0)||takeNum===All),opts:OptionsPattern[]]:=AzimuthalAdvancePN[syms,takeNum,"p",opts];
reDef@AzimuthalAdvancePN[syms_Association,expandIn_String,opts:OptionsPattern[]]:=AzimuthalAdvancePN[syms,1,expandIn,opts];
reDef@AzimuthalAdvancePN[syms_Association,opts:OptionsPattern[]]:=AzimuthalAdvancePN[syms,1,"p",opts];

reDef@AzimuthalAdvancePN[takeNum_/;((MatchQ[takeNum,_Integer]&&takeNum>=0)||takeNum===All),expandIn_String,opts:OptionsPattern[]]:=AzimuthalAdvancePN[DefaultSymbols[],takeNum,expandIn,opts]
reDef@AzimuthalAdvancePN[takeNum_/;((MatchQ[takeNum,_Integer]&&takeNum>=0)||takeNum===All),opts:OptionsPattern[]]:=AzimuthalAdvancePN[DefaultSymbols[],takeNum,opts]
reDef@AzimuthalAdvancePN[syms_Association,expandIn_String,opts:OptionsPattern[]]:=AzimuthalAdvancePN[DefaultSymbols[],expandIn,opts];
reDef@AzimuthalAdvancePN[syms_Association,opts:OptionsPattern[]]:=AzimuthalAdvancePN[DefaultSymbols[],opts];


Options[OmegaRPN]=Options[FourVelocity];
DocumentationBuilder`OptionDescriptions["OmegaRPN"]=DocumentationBuilder`OptionDescriptions["FourVelocity"];
def@
OmegaRPN[syms_Association,takeNum_/;((MatchQ[takeNum,_Integer]&&takeNum>=0)||takeNum===All),expandIn_String,opts:OptionsPattern[]]:=
Module[{seriesFull},
	
	seriesFull = 
	Which[
		OptionValue["Metric"]==="Schwarzschild",
		omegaRSchwPN[syms,expandIn],
		
		OptionValue["Metric"]==="Kerr",
		If[expandIn=!="p",Print["Quantities on Kerr are only available as expansions in p"]; Aborting[]];
		omegaRKerrPN[syms],

		True,
		Print["No metric ", OptionValue["Metric"]];
		Aborting[syms]
	];
	
	SeriesTake[seriesFull,takeNum]
];
reDef@OmegaRPN[syms_Association,takeNum_/;((MatchQ[takeNum,_Integer]&&takeNum>=0)||takeNum===All),opts:OptionsPattern[]]:=OmegaRPN[syms,takeNum,"p",opts];
reDef@OmegaRPN[syms_Association,expandIn_String,opts:OptionsPattern[]]:=OmegaRPN[syms,1,expandIn,opts];
reDef@OmegaRPN[syms_Association,opts:OptionsPattern[]]:=OmegaRPN[syms,1,"p",opts];

reDef@OmegaRPN[takeNum_/;((MatchQ[takeNum,_Integer]&&takeNum>=0)||takeNum===All),expandIn_String,opts:OptionsPattern[]]:=OmegaRPN[DefaultSymbols[],takeNum,expandIn,opts]
reDef@OmegaRPN[takeNum_/;((MatchQ[takeNum,_Integer]&&takeNum>=0)||takeNum===All),opts:OptionsPattern[]]:=OmegaRPN[DefaultSymbols[],takeNum,opts]
reDef@OmegaRPN[expandIn_String,opts:OptionsPattern[]]:=OmegaRPN[DefaultSymbols[],expandIn,opts];
reDef@OmegaRPN[opts:OptionsPattern[]]:=OmegaRPN[DefaultSymbols[],opts];


Options[OmegaPhiPN]=Options[FourVelocity];
DocumentationBuilder`OptionDescriptions["OmegaPhiPN"]=DocumentationBuilder`OptionDescriptions["FourVelocity"];
def@
OmegaPhiPN[syms_Association,takeNum_/;((MatchQ[takeNum,_Integer]&&takeNum>=0)||takeNum===All),expandIn_String,opts:OptionsPattern[]]:=
Module[{seriesFull},
	
	seriesFull = 
	Which[
		OptionValue["Metric"]==="Schwarzschild",
		omegaPhiSchwPN[syms,expandIn],
		
		OptionValue["Metric"]==="Kerr",
		If[expandIn=!="p",Print["Quantities on Kerr are only available as expansions in p"]; Aborting[]];
		omegaPhiKerrPN[syms],

		True,
		Print["No metric ", OptionValue["Metric"]];
		Aborting[syms]
	];
	
	SeriesTake[seriesFull,takeNum]
];
reDef@OmegaPhiPN[syms_Association,takeNum_/;((MatchQ[takeNum,_Integer]&&takeNum>=0)||takeNum===All),opts:OptionsPattern[]]:=OmegaPhiPN[syms,takeNum,"p",opts];
reDef@OmegaPhiPN[syms_Association,expandIn_String,opts:OptionsPattern[]]:=OmegaPhiPN[syms,1,expandIn,opts];
reDef@OmegaPhiPN[syms_Association,opts:OptionsPattern[]]:=OmegaPhiPN[syms,1,"p",opts];

reDef@OmegaPhiPN[takeNum_/;((MatchQ[takeNum,_Integer]&&takeNum>=0)||takeNum===All),expandIn_String,opts:OptionsPattern[]]:=OmegaPhiPN[DefaultSymbols[],takeNum,expandIn,opts]
reDef@OmegaPhiPN[takeNum_/;((MatchQ[takeNum,_Integer]&&takeNum>=0)||takeNum===All),opts:OptionsPattern[]]:=OmegaPhiPN[DefaultSymbols[],takeNum,opts]
reDef@OmegaPhiPN[expandIn_String,opts:OptionsPattern[]]:=OmegaPhiPN[DefaultSymbols[],expandIn,opts];
reDef@OmegaPhiPN[opts:OptionsPattern[]]:=OmegaPhiPN[DefaultSymbols[],opts];


reDef@
XPN[syms_Association,takeNum_/;((MatchQ[takeNum,_Integer]&&takeNum>=0)||takeNum===All)]:=
Module[{p,M,e,seriesFull},

	p=SemiLatusRectumSymbol[syms];
	M=BlackHoleMassSymbol[syms];
	e=OrbitalEccentricitySymbol[syms];
	
	seriesFull=
	SeriesData[p, Infinity, {1 - e^2, 0, -2*e^2*(-1 + e^2), 0, -((-1 + e^2)*(10*e^4 - 10*(-1 + Sqrt[1 - e^2]) + e^2*(-3 + 10*Sqrt[1 - e^2])))/2, 0, 
  -((-1 + e^2)*(40*e^6 + e^2*(102 - 45*Sqrt[1 - e^2]) - 30*(-1 + Sqrt[1 - e^2]) + e^4*(-39 + 75*Sqrt[1 - e^2])))/3, 0, 
  -((-1 + e^2)*(3520*e^8 - 11256*(-1 + Sqrt[1 - e^2]) + 120*e^6*(-71 + 80*Sqrt[1 - e^2]) + 12*e^2*(-202 + 693*Sqrt[1 - e^2]) - 9*e^4*(-2323 + 740*Sqrt[1 - e^2])))/96, 0, 
  -((-1 + e^2)*(4928*e^10 + e^6*(52451 - 18650*Sqrt[1 - e^2]) - 25968*(-1 + Sqrt[1 - e^2]) + 160*e^8*(-141 + 110*Sqrt[1 - e^2]) - 24*e^2*(-1428 + 625*Sqrt[1 - e^2]) + 
      6*e^4*(-3010 + 7003*Sqrt[1 - e^2])))/48, 0, -((-1 + e^2)*(167552*e^12 - 2288088*(-1 + Sqrt[1 - e^2]) + 21120*e^10*(-59 + 35*Sqrt[1 - e^2]) - 153*e^4*(-23614 + 6217*Sqrt[1 - e^2]) + 
      36*e^2*(16745 + 14452*Sqrt[1 - e^2]) - 30*e^8*(-97897 + 41200*Sqrt[1 - e^2]) + 3*e^6*(-778711 + 1071939*Sqrt[1 - e^2])))/576, 0, 
  -((-1 + e^2)*(478720*e^14 + e^6*(23984580 - 8218659*Sqrt[1 - e^2]) - 13179024*(-1 + Sqrt[1 - e^2]) + 29568*e^12*(-178 + 85*Sqrt[1 - e^2]) - 240*e^10*(-55719 + 26422*Sqrt[1 - e^2]) - 
      72*e^2*(-173152 + 77281*Sqrt[1 - e^2]) + 123*e^8*(-144834 + 140815*Sqrt[1 - e^2]) + 18*e^4*(12194 + 748315*Sqrt[1 - e^2])))/576, 0, 
  -((-1 + e^2)*(176168960*e^16 + e^8*(18492092361 - 8598539064*Sqrt[1 - e^2]) - 11017019520*(-1 + Sqrt[1 - e^2]) + 5361664*e^14*(-501 + 200*Sqrt[1 - e^2]) - 
      5632*e^12*(-1365881 + 687260*Sqrt[1 - e^2]) - 576*e^2*(-12713468 + 4336373*Sqrt[1 - e^2]) - 144*e^4*(-97711066 + 5152445*Sqrt[1 - e^2]) + 480*e^10*(-29723847 + 22867856*Sqrt[1 - e^2]) + 
      24*e^6*(-245891492 + 611541923*Sqrt[1 - e^2])))/73728, 0, 
  -((-1 + e^2)*(2290196480*e^18 + e^10*(484384882545 - 271744728876*Sqrt[1 - e^2]) - 303407976960*(-1 + Sqrt[1 - e^2]) + 68935680*e^16*(-671 + 230*Sqrt[1 - e^2]) - 
      50688*e^14*(-3027659 + 1542070*Sqrt[1 - e^2]) + 5184*e^4*(33827731 + 37371863*Sqrt[1 - e^2]) - 3456*e^2*(-88834977 + 52072375*Sqrt[1 - e^2]) - 
      432*e^6*(-1059686836 + 125710967*Sqrt[1 - e^2]) + 1152*e^12*(-309241737 + 202404598*Sqrt[1 - e^2]) + 108*e^8*(-3186112222 + 4118753469*Sqrt[1 - e^2])))/331776, 0, 
  -((-1 + e^2)*(26566279168*e^20 - 7698460931712*(-1 + Sqrt[1 - e^2]) + 1585520640*e^18*(-433 + 130*Sqrt[1 - e^2]) - 1723392*e^16*(-1552779 + 775400*Sqrt[1 - e^2]) + 
      25344*e^14*(-288329155 + 168509506*Sqrt[1 - e^2]) + 432*e^4*(22165532493 + 1598177024*Sqrt[1 - e^2]) - 1728*e^2*(-4566033527 + 2923172366*Sqrt[1 - e^2]) + 
      1944*e^6*(629129155 + 4464211328*Sqrt[1 - e^2]) - 27*e^8*(-445175686486 + 142662835329*Sqrt[1 - e^2]) - 18*e^12*(-611152519777 + 374305890400*Sqrt[1 - e^2]) + 
      27*e^10*(-431613969189 + 401102282177*Sqrt[1 - e^2])))/1327104, 0, 
  -((-1 + e^2)*(77283721216*e^22 + e^10*(79005135700836 - 40725412301169*Sqrt[1 - e^2]) - 48022362190080*(-1 + Sqrt[1 - e^2]) + 4580392960*e^20*(-543 + 145*Sqrt[1 - e^2]) - 
      3829760*e^18*(-2988161 + 1434050*Sqrt[1 - e^2]) + 50688*e^16*(-699809519 + 377246541*Sqrt[1 - e^2]) - 38016*e^2*(-1600423326 + 1176567335*Sqrt[1 - e^2]) + 
      1728*e^6*(39426558361 + 7488116936*Sqrt[1 - e^2]) + 864*e^4*(58392942511 + 18499560212*Sqrt[1 - e^2]) - 144*e^14*(-421404265347 + 264328150636*Sqrt[1 - e^2]) + 
      54*e^8*(-461956976242 + 1219772965879*Sqrt[1 - e^2]) + 9*e^12*(-9043740459202 + 6939402602527*Sqrt[1 - e^2])))/1327104, 0, 
  -((-1 + e^2)*(10819720970240*e^24 - 14539817649600000*(-1 + Sqrt[1 - e^2]) + 318795350016*e^22*(-1331 + 320*Sqrt[1 - e^2]) - 528506880*e^20*(-4356037 + 1985724*Sqrt[1 - e^2]) + 
      1148928*e^18*(-6911026913 + 3510039320*Sqrt[1 - e^2]) + 5184*e^4*(4105729295543 + 7998963420*Sqrt[1 - e^2]) - 19008*e^16*(-818111494935 + 506822154616*Sqrt[1 - e^2]) - 
      20736*e^2*(-984645337478 + 777973219153*Sqrt[1 - e^2]) + 6912*e^6*(1778232485929 + 2123144685695*Sqrt[1 - e^2]) + 108*e^8*(188847861462780 + 3714929449819*Sqrt[1 - e^2]) + 
      864*e^14*(-28254895541329 + 19308955389270*Sqrt[1 - e^2]) + 162*e^10*(-105546054651424 + 131680199129551*Sqrt[1 - e^2]) - 27*e^12*(-927901037415923 + 589361745369094*Sqrt[1 - e^2])))/
   63700992, 0, -((-1 + e^2)*(15813438341120*e^26 - 45689903173002240*(-1 + Sqrt[1 - e^2]) + 463702327296*e^24*(-1601 + 350*Sqrt[1 - e^2]) - 374759424*e^22*(-12618577 + 5429670*Sqrt[1 - e^2]) + 
      11489280*e^20*(-1562857045 + 757821342*Sqrt[1 - e^2]) - 10368*e^4*(-6836323864216 + 140149367455*Sqrt[1 - e^2]) - 41472*e^2*(-1794592052262 + 1509555337231*Sqrt[1 - e^2]) + 
      5184*e^6*(14351818656431 + 5260862060218*Sqrt[1 - e^2]) - 2112*e^18*(-19005315291461 + 11411524603244*Sqrt[1 - e^2]) + 864*e^16*(-82070620794284 + 52176389456411*Sqrt[1 - e^2]) - 
      108*e^10*(-606114917689524 + 175573957260905*Sqrt[1 - e^2]) + 216*e^8*(47406258638588 + 278873435864743*Sqrt[1 - e^2]) - 135*e^14*(-602045232844727 + 411570524690091*Sqrt[1 - e^2]) + 
      81*e^12*(-946567699723804 + 851218888660001*Sqrt[1 - e^2])))/31850496, 0, 
  -((-1 + e^2)*(370486269706240*e^28 - 2306490140609455104*(-1 + Sqrt[1 - e^2]) + 43278883880960*e^26*(-474 + 95*Sqrt[1 - e^2]) - 4830232576*e^24*(-31568287 + 12784960*Sqrt[1 - e^2]) + 
      19218432*e^22*(-32989912192 + 15391362883*Sqrt[1 - e^2]) - 466560*e^4*(-9449326669276 + 1657705800281*Sqrt[1 - e^2]) - 143616*e^20*(-11176269326403 + 6448657528336*Sqrt[1 - e^2]) - 
      41472*e^2*(-101661833452503 + 89347105665520*Sqrt[1 - e^2]) + 17280*e^6*(207294710532754 + 128687929687277*Sqrt[1 - e^2]) + 4224*e^18*(-740734642243507 + 448965090271939*Sqrt[1 - e^2]) + 
      216*e^8*(15927747044999868 + 6462471016048055*Sqrt[1 - e^2]) + 432*e^10*(-2937504020060635 + 7635816144185899*Sqrt[1 - e^2]) - 
      108*e^16*(-37722057070058867 + 25998536595552904*Sqrt[1 - e^2]) - 27*e^12*(-135108028739413804 + 74914795060266823*Sqrt[1 - e^2]) + 
      27*e^14*(-169752681244883470 + 129226186621400959*Sqrt[1 - e^2])))/254803968, 0, 
  -((-1 + e^2)*(3260279173414912*e^30 + e^14*(79205891028770443608 - 54716857016847608709*Sqrt[1 - e^2]) - 43653902910804928512*(-1 + Sqrt[1 - e^2]) + 
      189761260093440*e^28*(-1108 + 205*Sqrt[1 - e^2]) - 35669409792*e^26*(-50777653 + 19340650*Sqrt[1 - e^2]) + 249839616*e^24*(-32926381520 + 14841796233*Sqrt[1 - e^2]) - 
      1244160*e^2*(-72443926165588 + 65913964366639*Sqrt[1 - e^2]) - 156672*e^22*(-149129009275931 + 82450925717900*Sqrt[1 - e^2]) - 62208*e^4*(-1530996042178192 + 398358062312621*Sqrt[1 - e^2]) + 
      12672*e^20*(-3927380518141262 + 2299016580134135*Sqrt[1 - e^2]) + 10368*e^6*(9019950806967080 + 3210881645204539*Sqrt[1 - e^2]) + 
      648*e^10*(87945806027680864 + 14520525109997325*Sqrt[1 - e^2]) - 2592*e^18*(-28353679652087401 + 19129014911105429*Sqrt[1 - e^2]) + 
      1296*e^8*(35187365484117524 + 48342261228577181*Sqrt[1 - e^2]) + 81*e^16*(-1147220818235794004 + 800316946337891907*Sqrt[1 - e^2]) + 
      54*e^12*(-1018421754963350204 + 1209116511496320551*Sqrt[1 - e^2])))/764411904, 0, 
  -((-1 + e^2)*(4903459876816027648*e^32 - 141268885287094518054912*(-1 + Sqrt[1 - e^2]) + 142266727567196160*e^30*(-2561 + 440*Sqrt[1 - e^2]) - 
      22828422266880*e^28*(-158218569 + 56705500*Sqrt[1 - e^2]) + 71338819584*e^26*(-250927022157 + 109484577728*Sqrt[1 - e^2]) - 76873728*e^24*(-735095383274269 + 389494981033912*Sqrt[1 - e^2]) + 
      20054016*e^22*(-6525784545557906 + 3712460577608147*Sqrt[1 - e^2]) - 2985984*e^4*(-122809316877950948 + 45013134623939863*Sqrt[1 - e^2]) - 
      3981312*e^2*(-81660402954131304 + 76291364704366507*Sqrt[1 - e^2]) + 829440*e^6*(407667377933480152 + 150350219414466467*Sqrt[1 - e^2]) - 
      405504*e^20*(-535685807648964619 + 349642218124377358*Sqrt[1 - e^2]) + 72576*e^10*(406301638415656696 + 2987218016624056361*Sqrt[1 - e^2]) + 
      20736*e^8*(13279580134192272258 + 8009993007655612843*Sqrt[1 - e^2]) - 2592*e^12*(-70727491348899029524 + 18778379116917050629*Sqrt[1 - e^2]) + 
      10368*e^18*(-29055157146791171277 + 19321016495102961104*Sqrt[1 - e^2]) + 432*e^14*(-597235014613108491496 + 515971021231394988547*Sqrt[1 - e^2]) - 
      27*e^16*(-10721431704848186177211 + 7853194141138049310160*Sqrt[1 - e^2])))/391378894848, 0, 
  -((-1 + e^2)*(7210970407082393600*e^34 + e^18*(992916456123622321916217 - 723859345706605863166296*Sqrt[1 - e^2]) - 446707534695819760631808*(-1 + Sqrt[1 - e^2]) + 
      208657867098554368*e^32*(-2931 + 470*Sqrt[1 - e^2]) - 144580007690240*e^30*(-47924911 + 16182454*Sqrt[1 - e^2]) + 1141421113344*e^28*(-32814687909 + 13866957830*Sqrt[1 - e^2]) - 
      76873728*e^26*(-1695246241578225 + 862201203322444*Sqrt[1 - e^2]) + 3342336*e^24*(-97883479332090104 + 54253864294023305*Sqrt[1 - e^2]) - 
      7962624*e^2*(-143515082164220479 + 136899552993765643*Sqrt[1 - e^2]) - 3981312*e^4*(-339100030162131798 + 152935751911432135*Sqrt[1 - e^2]) - 
      405504*e^22*(-1489186759353722177 + 937279861613243851*Sqrt[1 - e^2]) + 331776*e^6*(4020309698253604768 + 955213477449046577*Sqrt[1 - e^2]) + 
      165888*e^8*(5483022401469919263 + 4644654467832676895*Sqrt[1 - e^2]) + 62208*e^10*(10729417729297553210 + 6887721674481660201*Sqrt[1 - e^2]) + 
      4608*e^20*(-197364118275428164085 + 127393003415097456146*Sqrt[1 - e^2]) + 3456*e^12*(-83554369547791718870 + 194988899893880263721*Sqrt[1 - e^2]) - 
      864*e^14*(-780718403372029870392 + 470694589948298854367*Sqrt[1 - e^2]) + 216*e^16*(-4641894443703785140222 + 3476169183520117919101*Sqrt[1 - e^2])))/195689447424, 0, 
  -((-1 + e^2)*(764362863150733721600*e^36 - 101814090289336477250519040*(-1 + Sqrt[1 - e^2]) + 44131138891344248832*e^34*(-1663 + 250*Sqrt[1 - e^2]) - 
      21340009135079424*e^32*(-44237407 + 14096720*Sqrt[1 - e^2]) + 34242633400320*e^30*(-162487638483 + 66494173270*Sqrt[1 - e^2]) - 
      40128086016*e^28*(-527901029359601 + 258276645888384*Sqrt[1 - e^2]) + 691863552*e^26*(-83142002397224039 + 44930372144718771*Sqrt[1 - e^2]) - 
      3760128*e^24*(-31084257724369644299 + 18875140183214847460*Sqrt[1 - e^2]) - 8957952*e^4*(-40362835595938393243 + 21474045942083273754*Sqrt[1 - e^2]) - 
      11943936*e^2*(-24058103616068794893 + 23328395114370285614*Sqrt[1 - e^2]) + 1492992*e^6*(238825482276981339587 + 41962269394721568672*Sqrt[1 - e^2]) + 
      186624*e^8*(1555601563159215209771 + 944657084210212022620*Sqrt[1 - e^2]) + 82944*e^22*(-2295090143056550715523 + 1448409136014548724730*Sqrt[1 - e^2]) + 
      93312*e^10*(1237642621518010363427 + 2087805953611157661200*Sqrt[1 - e^2]) + 7776*e^12*(15194973344701216337149 + 5076988708696247684282*Sqrt[1 - e^2]) + 
      3888*e^14*(-36212959141325671834805 + 39785512732996074911128*Sqrt[1 - e^2]) - 243*e^16*(-783185601699879975994174 + 583665695953722779234637*Sqrt[1 - e^2]) + 
      135*e^18*(-1905563907996662811184829 + 1339794329588475004026545*Sqrt[1 - e^2]) - 54*e^20*(-4335265741642895828968407 + 3074643333843597996215360*Sqrt[1 - e^2])))/7044820107264, 0, 
  -((-1 + e^2)*(2252858965075846758400*e^38 - 645039317860270943076679680*(-1 + Sqrt[1 - e^2]) + 129797467327483084800*e^36*(-1873 + 265*Sqrt[1 - e^2]) - 
      55232964820205568*e^34*(-63881433 + 19247440*Sqrt[1 - e^2]) + 4944636263006208*e^32*(-4583573911 + 1815639859*Sqrt[1 - e^2]) - 
      428032917504*e^30*(-219274589242721 + 103431880610948*Sqrt[1 - e^2]) + 691863552*e^28*(-397654440057971514 + 209503454576698303*Sqrt[1 - e^2]) - 
      15040512*e^26*(-40561463074775154713 + 23809922836650771201*Sqrt[1 - e^2]) + 5971968*e^6*(457578827564530697996 + 35228638856048301139*Sqrt[1 - e^2]) - 
      23887872*e^2*(-83700095209433384698 + 82225867774890060355*Sqrt[1 - e^2]) - 5971968*e^4*(-447130306852434838503 + 268944336479904185270*Sqrt[1 - e^2]) + 
      1119744*e^10*(1241012960736230450271 + 1059713204682963950326*Sqrt[1 - e^2]) + 497664*e^24*(-2154238759028907834301 + 1332027300421349947123*Sqrt[1 - e^2]) + 
      373248*e^8*(5812465233610915308871 + 3660464874949747027052*Sqrt[1 - e^2]) - 15552*e^14*(-47204415216464279198070 + 12426655771642026508121*Sqrt[1 - e^2]) + 
      15552*e^12*(3940119342448389013615 + 73240775660668483803542*Sqrt[1 - e^2]) + 486*e^16*(-2637209949195553567684562 + 2158170144184059156618275*Sqrt[1 - e^2]) - 
      432*e^22*(-3394749137763906476803821 + 2327731695590492683897544*Sqrt[1 - e^2]) - 243*e^18*(-6037078396859780105819500 + 4667674549997275443478567*Sqrt[1 - e^2]) + 
      27*e^20*(-64205662955650369218886142 + 43847195487345461033319977*Sqrt[1 - e^2])))/7044820107264}, 2, 42, 2];

	SeriesTake[seriesFull,takeNum]
];
reDef@XPN[syms_Association]:=XPN[syms,1];

reDef@XPN[takeNum_/;((MatchQ[takeNum,_Integer]&&takeNum>=0)||takeNum===All)]:=XPN[DefaultSymbols[],takeNum]
reDef@XPN[]:=XPN[DefaultSymbols[]];


def@
SemiLatusRectumPN[syms_Association,takeNum_/;((MatchQ[takeNum,_Integer]&&takeNum>=0)||takeNum===All)]:=
Module[{e,M,x,seriesFull},
	
	M=BlackHoleMassSymbol[syms];
	e=OrbitalEccentricitySymbol[syms];
	x=XPNSymbol[syms];
	
	seriesFull=
	SeriesData[x, 0, {1 - e^2, 2*e^2, (e^2*(20 - 3*Sqrt[1 - e^2]) + 2*e^4*(-5 + Sqrt[1 - e^2]) + 10*(-1 + Sqrt[1 - e^2]))/
   (2*(1 - e^2)^(3/2)), (e^6*(15 - 2*Sqrt[1 - e^2]) + 30*(-1 + Sqrt[1 - e^2]) - 12*e^4*(5 + Sqrt[1 - e^2]) + 
    3*e^2*(25 + 4*Sqrt[1 - e^2]))/(3*(1 - e^2)^(5/2)), (552*(-1 + Sqrt[1 - e^2]) + 4*e^6*(155 + 282*Sqrt[1 - e^2]) - 
    7*e^4*(256 + 497*Sqrt[1 - e^2]) + 4*e^2*(431 + 598*Sqrt[1 - e^2]))/(32*(1 - e^2)^(7/2)), 
  (984*(-1 + Sqrt[1 - e^2]) + 8*e^10*(-25 + 2*Sqrt[1 - e^2]) - 5*e^8*(325 + 444*Sqrt[1 - e^2]) + 
    12*e^2*(602 + 783*Sqrt[1 - e^2]) + 2*e^6*(4553 + 5599*Sqrt[1 - e^2]) - 3*e^4*(4507 + 6150*Sqrt[1 - e^2]))/
   (24*(1 - e^2)^(9/2)), -(448*e^12*(-15 + Sqrt[1 - e^2]) + 93672*(-1 + Sqrt[1 - e^2]) - 8400*e^10*(35 + 8*Sqrt[1 - e^2]) - 
     393*e^6*(9570 + 5293*Sqrt[1 - e^2]) - 36*e^2*(24698 + 36977*Sqrt[1 - e^2]) + 3*e^8*(598903 + 221186*Sqrt[1 - e^2]) + 
     9*e^4*(360869 + 296974*Sqrt[1 - e^2]))/(576*(1 - e^2)^(11/2)), 
  (-126360*e^12 - 217904*(-1 + Sqrt[1 - e^2]) + 27*e^10*(35661 + 6896*Sqrt[1 - e^2]) + 8*e^2*(20207 + 153566*Sqrt[1 - e^2]) - 
    4*e^8*(704061 + 276068*Sqrt[1 - e^2]) - 2*e^4*(1123327 + 1262968*Sqrt[1 - e^2]) + e^6*(3846851 + 2442512*Sqrt[1 - e^2]))/
   (64*(1 - e^2)^(13/2)), -(-90112*e^16*(-20 + Sqrt[1 - e^2]) + 1533734784*(-1 + Sqrt[1 - e^2]) + 
     22528*e^14*(-14545 + 906*Sqrt[1 - e^2]) + 5632*e^12*(635807 + 155041*Sqrt[1 - e^2]) - 
     576*e^2*(734173 + 15874940*Sqrt[1 - e^2]) + 144*e^4*(114917129 + 163922598*Sqrt[1 - e^2]) - 
     24*e^10*(637330477 + 296075252*Sqrt[1 - e^2]) - 24*e^6*(1450106033 + 1290918364*Sqrt[1 - e^2]) + 
     3*e^8*(10750676864 + 7068189213*Sqrt[1 - e^2]))/(73728*(1 - e^2)^(15/2)), 
  (-3859719552*(-1 + Sqrt[1 - e^2]) - 66560*e^18*(-45 + 2*Sqrt[1 - e^2]) + 14976*e^16*(-30255 + 2468*Sqrt[1 - e^2]) + 
    1872*e^14*(5810109 + 2123768*Sqrt[1 - e^2]) + 1728*e^2*(15019796 + 24192603*Sqrt[1 - e^2]) - 
    117*e^12*(625279939 + 330211224*Sqrt[1 - e^2]) - 432*e^4*(394892104 + 380711571*Sqrt[1 - e^2]) + 
    216*e^6*(1673720648 + 1383872495*Sqrt[1 - e^2]) + 54*e^10*(4242995109 + 2719500317*Sqrt[1 - e^2]) - 
    27*e^8*(14338318573 + 10548239230*Sqrt[1 - e^2]))/(82944*(1 - e^2)^(17/2)), 
  (2315257344*e^16 + 4046201472*(-1 + Sqrt[1 - e^2]) - 10368*e^14*(2767735 + 626306*Sqrt[1 - e^2]) - 
    192*e^2*(98081063 + 321429186*Sqrt[1 - e^2]) + 6*e^12*(23944610863 + 9754001040*Sqrt[1 - e^2]) + 
    16*e^4*(11232851469 + 14981556652*Sqrt[1 - e^2]) - 8*e^6*(56304119685 + 53352680144*Sqrt[1 - e^2]) - 
    e^10*(375633867823 + 214023730295*Sqrt[1 - e^2]) + e^8*(552134950018 + 406778001623*Sqrt[1 - e^2]))/(16384*(-1 + e^2)^9), 
  (3899392*e^22 - 4072521694464*(-1 + Sqrt[1 - e^2]) + 15319040*e^20*(-102 + 7*Sqrt[1 - e^2]) - 
    765952*e^18*(-490721 + 23585*Sqrt[1 - e^2]) - 861696*e^16*(8382931 + 2162371*Sqrt[1 - e^2]) + 
    38016*e^2*(69537216 + 932362763*Sqrt[1 - e^2]) + 1224*e^14*(41348541681 + 17579664586*Sqrt[1 - e^2]) - 
    864*e^4*(111718541207 + 163125872062*Sqrt[1 - e^2]) + 864*e^6*(346260729425 + 341519866089*Sqrt[1 - e^2]) - 
    54*e^8*(8133179405684 + 6502307705151*Sqrt[1 - e^2]) + 27*e^10*(13557049896976 + 9094115078789*Sqrt[1 - e^2]) - 
    9*e^12*(20004212389808 + 11061925362181*Sqrt[1 - e^2]))/(1327104*(-1 + e^2)^10), 
  (258998272*e^24 + 1057243837847040*(-1 + Sqrt[1 - e^2]) + 97124352*e^22*(-1259 + 80*Sqrt[1 - e^2]) - 
    12140544*e^20*(-1739593 + 120300*Sqrt[1 - e^2]) - 3035136*e^18*(335825897 + 94614712*Sqrt[1 - e^2]) + 
    853632*e^16*(11990773431 + 4791322696*Sqrt[1 - e^2]) - 20736*e^2*(92534789798 + 439237970207*Sqrt[1 - e^2]) - 
    32832*e^14*(1460211169735 + 723258021614*Sqrt[1 - e^2]) - 48384*e^6*(2570211564355 + 2251924908107*Sqrt[1 - e^2]) + 
    5184*e^4*(7151847488713 + 8266964291892*Sqrt[1 - e^2]) - 162*e^10*(1282548711954768 + 859917347007175*Sqrt[1 - e^2]) + 
    108*e^8*(1935755603287116 + 1470252543287117*Sqrt[1 - e^2]) + 
    27*e^12*(4729044834175673 + 2763552754193142*Sqrt[1 - e^2]))/(63700992*(-1 + e^2)^11), 
  (-5381565553664*(-1 + Sqrt[1 - e^2]) - 2239488*e^20*(2897917 + 666986*Sqrt[1 - e^2]) + 
    10368*e^18*(8450082041 + 2759431581*Sqrt[1 - e^2]) + 512*e^2*(120000606209 + 153373670552*Sqrt[1 - e^2]) - 
    640*e^4*(956552541788 + 834327534511*Sqrt[1 - e^2]) - 216*e^16*(2371224698643 + 998748315551*Sqrt[1 - e^2]) + 
    128*e^6*(16573500463424 + 13261656012323*Sqrt[1 - e^2]) - 43*e^12*(81474468127440 + 48676418952763*Sqrt[1 - e^2]) - 
    8*e^8*(500993414196442 + 372062630452615*Sqrt[1 - e^2]) + 8*e^10*(583707396539807 + 394091700196364*Sqrt[1 - e^2]) + 
    e^14*(1696658253744992 + 868798912429929*Sqrt[1 - e^2]))/(131072*(-1 + e^2)^12)}, -1, 13, 1];

	SeriesTake[seriesFull,takeNum]

];
reDef@SemiLatusRectumPN[syms_Association]:=SemiLatusRectumPN[syms,1];

reDef@SemiLatusRectumPN[takeNum_/;((MatchQ[takeNum,_Integer]&&takeNum>=0)||takeNum===All)]:=SemiLatusRectumPN[DefaultSymbols[],takeNum]
reDef@SemiLatusRectumPN[]:=SemiLatusRectumPN[DefaultSymbols[]];


Options[SpecificEnergyPN]=Options[FourVelocity];
DocumentationBuilder`OptionDescriptions["SpecificEnergyPN"]=DocumentationBuilder`OptionDescriptions["FourVelocity"];
def@
SpecificEnergyPN[syms_Association,takeNum_/;((MatchQ[takeNum,_Integer]&&takeNum>=0)||takeNum===All),expandIn_String,opts:OptionsPattern[]]:=
Module[{seriesFull},
	
	seriesFull = 
	Which[
		OptionValue["Metric"]==="Schwarzschild",
		specificEnergySchwPN[syms,expandIn],
		
		OptionValue["Metric"]==="Kerr",
		If[expandIn=!="p",Print["Quantities on Kerr are only available as expansions in p"]; Aborting[]];
		specificEnergyKerrPN[syms],

		True,
		Print["No metric ", OptionValue["Metric"]];
		Aborting[syms]
	];
	
	SeriesTake[seriesFull,takeNum]
];
reDef@SpecificEnergyPN[syms_Association,takeNum_/;((MatchQ[takeNum,_Integer]&&takeNum>=0)||takeNum===All),opts:OptionsPattern[]]:=SpecificEnergyPN[syms,takeNum,"p",opts];
reDef@SpecificEnergyPN[syms_Association,expandIn_String,opts:OptionsPattern[]]:=SpecificEnergyPN[syms,1,expandIn,opts];
reDef@SpecificEnergyPN[syms_Association,opts:OptionsPattern[]]:=SpecificEnergyPN[syms,1,"p",opts];

reDef@SpecificEnergyPN[takeNum_/;((MatchQ[takeNum,_Integer]&&takeNum>=0)||takeNum===All),expandIn_String,opts:OptionsPattern[]]:=SpecificEnergyPN[DefaultSymbols[],takeNum,expandIn,opts]
reDef@SpecificEnergyPN[takeNum_/;((MatchQ[takeNum,_Integer]&&takeNum>=0)||takeNum===All),opts:OptionsPattern[]]:=SpecificEnergyPN[DefaultSymbols[],takeNum,opts]
reDef@SpecificEnergyPN[expandIn_String,opts:OptionsPattern[]]:=SpecificEnergyPN[DefaultSymbols[],expandIn,opts];
reDef@SpecificEnergyPN[opts:OptionsPattern[]]:=SpecificEnergyPN[DefaultSymbols[],opts];


Options[SpecificAngularMomentumPN]=Options[FourVelocity];
DocumentationBuilder`OptionDescriptions["SpecificAngularMomentumPN"]=DocumentationBuilder`OptionDescriptions["FourVelocity"];
def@
SpecificAngularMomentumPN[syms_Association,takeNum_/;((MatchQ[takeNum,_Integer]&&takeNum>=0)||takeNum===All),expandIn_String,opts:OptionsPattern[]]:=
Module[{seriesFull},
	
	seriesFull = 
	Which[
		OptionValue["Metric"]==="Schwarzschild",
		specificAngularMomentumSchwPN[syms,expandIn],
		
		OptionValue["Metric"]==="Kerr",
		If[expandIn=!="p",Print["Quantities on Kerr are only available as expansions in p"]; Aborting[]];
		specificAngularMomentumKerrPN[syms],

		True,
		Print["No metric ", OptionValue["Metric"]];
		Aborting[syms]
	];
	
	SeriesTake[seriesFull,takeNum]
];
reDef@SpecificAngularMomentumPN[syms_Association,takeNum_/;((MatchQ[takeNum,_Integer]&&takeNum>=0)||takeNum===All),opts:OptionsPattern[]]:=SpecificAngularMomentumPN[syms,takeNum,"p",opts];
reDef@SpecificAngularMomentumPN[syms_Association,expandIn_String,opts:OptionsPattern[]]:=SpecificAngularMomentumPN[syms,1,expandIn,opts];
reDef@SpecificAngularMomentumPN[syms_Association,opts:OptionsPattern[]]:=SpecificAngularMomentumPN[syms,1,"p",opts];

reDef@SpecificAngularMomentumPN[takeNum_/;((MatchQ[takeNum,_Integer]&&takeNum>=0)||takeNum===All),expandIn_String,opts:OptionsPattern[]]:=SpecificAngularMomentumPN[DefaultSymbols[],takeNum,expandIn,opts]
reDef@SpecificAngularMomentumPN[takeNum_/;((MatchQ[takeNum,_Integer]&&takeNum>=0)||takeNum===All),opts:OptionsPattern[]]:=SpecificAngularMomentumPN[DefaultSymbols[],takeNum,opts]
reDef@SpecificAngularMomentumPN[expandIn_String,opts:OptionsPattern[]]:=SpecificAngularMomentumPN[DefaultSymbols[],expandIn,opts];
reDef@SpecificAngularMomentumPN[opts:OptionsPattern[]]:=SpecificAngularMomentumPN[DefaultSymbols[],opts];


def@
radialPeriodSchwPN[syms_Association,px_String]:=
Module[{M,p,e,x},
	
	M=BlackHoleMassSymbol[syms];
	e=OrbitalEccentricitySymbol[syms];
	p=SemiLatusRectumSymbol[syms];
	x=XPNSymbol[syms];
	
	Which[px==="p",
		SeriesData[p, Infinity, {(2*M*Pi)/(1 - e^2)^(3/2), 0, (6*M*Pi)/Sqrt[1 - e^2], 0, 3*(5 + 4/Sqrt[1 - e^2])*M*Pi, 0, (3*(20 - 4*e^2 + 25*Sqrt[1 - e^2])*M*Pi)/Sqrt[1 - e^2], 0, 
  (3*(1218 + 672/Sqrt[1 - e^2] + e^2*(25 - 160/Sqrt[1 - e^2]))*M*Pi)/8, 0, (3*(2784 - 32*e^4 + 7422*Sqrt[1 - e^2] + e^2*(-704 + 533*Sqrt[1 - e^2]))*M*Pi)/(8*Sqrt[1 - e^2]), 0, 
  ((616*(448 + 1739*Sqrt[1 - e^2]) + e^4*(-6656 + 2049*Sqrt[1 - e^2]) + 8*e^2*(-9088 + 21279*Sqrt[1 - e^2]))*M*Pi)/(64*Sqrt[1 - e^2]), 0, 
  (3*(-512*e^6 + 312*(1216 + 6803*Sqrt[1 - e^2]) + e^4*(-12800 + 21111*Sqrt[1 - e^2]) + 8*e^2*(-12992 + 75731*Sqrt[1 - e^2]))*M*Pi)/(64*Sqrt[1 - e^2]), 0, 
  (3*(e^6*(-102400 + 38869*Sqrt[1 - e^2]) + 2640*(9472 + 75821*Sqrt[1 - e^2]) + 2*e^4*(-501760 + 3097581*Sqrt[1 - e^2]) + 8*e^2*(-890368 + 11371903*Sqrt[1 - e^2]))*M*Pi)/(1024*Sqrt[1 - e^2]), 0, 
  ((-61440*e^8 + 38896*(7936 + 90479*Sqrt[1 - e^2]) + e^6*(-2310144 + 4717315*Sqrt[1 - e^2]) + 2*e^4*(-6836224 + 131730489*Sqrt[1 - e^2]) + 8*e^2*(-11413504 + 294252213*Sqrt[1 - e^2]))*M*Pi)/
   (1024*Sqrt[1 - e^2]), 0, (3*(347776*(19456 + 315155*Sqrt[1 - e^2]) + e^8*(-5373952 + 2265009*Sqrt[1 - e^2]) + 64*e^6*(-1138688 + 9184105*Sqrt[1 - e^2]) + 
     1024*e^2*(-2024960 + 99660189*Sqrt[1 - e^2]) + 32*e^4*(-9986048 + 531413865*Sqrt[1 - e^2]))*M*Pi)/(16384*Sqrt[1 - e^2]), 0, 
  (3*(-917504*e^10 + 198016*(140288 + 3225505*Sqrt[1 - e^2]) + e^8*(-50462720 + 113181813*Sqrt[1 - e^2]) + 64*e^6*(-5877760 + 162213819*Sqrt[1 - e^2]) + 
     1024*e^2*(-8587648 + 775444173*Sqrt[1 - e^2]) + 32*e^4*(-43065344 + 5772135177*Sqrt[1 - e^2]))*M*Pi)/(16384*Sqrt[1 - e^2]), 0, 
  ((667540224*(4096 + 133685*Sqrt[1 - e^2]) + e^10*(-447741952 + 201091617*Sqrt[1 - e^2]) + e^8*(-8582594560 + 77082051426*Sqrt[1 - e^2]) + 576*e^4*(-245923840 + 77239791053*Sqrt[1 - e^2]) + 
     32*e^6*(-1341587456 + 115254285579*Sqrt[1 - e^2]) + 384*e^2*(-2321260544 + 373462534529*Sqrt[1 - e^2]))*M*Pi)/(131072*Sqrt[1 - e^2]), 0, 
  (3*(-22020096*e^12 + 130233600*(28672 + 1329455*Sqrt[1 - e^2]) + 5*e^10*(-337641472 + 797152051*Sqrt[1 - e^2]) + 10*e^8*(-1706557440 + 54325376353*Sqrt[1 - e^2]) + 
     288*e^6*(-224788480 + 55818289193*Sqrt[1 - e^2]) + 64*e^4*(-3149578240 + 2188984384093*Sqrt[1 - e^2]) + 128*e^2*(-9765208064 + 2737482642293*Sqrt[1 - e^2]))*M*Pi)/(131072*Sqrt[1 - e^2]), 0, 
  (3*(68465664*(1785856 + 117788369*Sqrt[1 - e^2]) + 5*e^12*(-855638016 + 401152603*Sqrt[1 - e^2]) + 24*e^10*(-4685037568 + 44355945759*Sqrt[1 - e^2]) + 
     40*e^8*(-18163433472 + 1900733505661*Sqrt[1 - e^2]) + 256*e^6*(-8885895168 + 5987033603605*Sqrt[1 - e^2]) + 1024*e^2*(-40931393536 + 19646006583211*Sqrt[1 - e^2]) + 
     128*e^4*(-53666250752 + 78845103862581*Sqrt[1 - e^2]))*M*Pi)/(1048576*Sqrt[1 - e^2]), 0, 
  ((-1660944384*e^14 + 1103057920*(1359872 + 127780291*Sqrt[1 - e^2]) + e^12*(-170171301888 + 414810986761*Sqrt[1 - e^2]) + 8*e^10*(-288918339584 + 9828504960993*Sqrt[1 - e^2]) + 
     256*e^6*(-115141574656 + 199088066250685*Sqrt[1 - e^2]) + 1024*e^2*(-513000030208 + 415710727201311*Sqrt[1 - e^2]) + 8*e^8*(-1340774285312 + 435651337795827*Sqrt[1 - e^2]) + 
     128*e^4*(-684335955968 + 2048653773220515*Sqrt[1 - e^2]))*M*Pi)/(1048576*Sqrt[1 - e^2]), 0, 
  (3*(21224355840*(3080192 + 413038111*Sqrt[1 - e^2]) + e^14*(-500497907712 + 242069684333*Sqrt[1 - e^2]) + 6*e^12*(-2903599218688 + 28310854941119*Sqrt[1 - e^2]) + 
     8*e^10*(-18450726518784 + 2115728761872151*Sqrt[1 - e^2]) + 48*e^8*(-11113353707520 + 10677993200322491*Sqrt[1 - e^2]) + 1024*e^2*(-22792719171584 + 30828039096667163*Sqrt[1 - e^2]) + 
     128*e^6*(-10514569625600 + 44406077337386727*Sqrt[1 - e^2]) + 256*e^4*(-15475452411904 + 91629424046588059*Sqrt[1 - e^2]))*M*Pi)/(33554432*Sqrt[1 - e^2]), 0, 
  (3*(-57579405312*e^16 + 12847380480*(20774912 + 3982570141*Sqrt[1 - e^2]) + e^14*(-7618198241280 + 18977608296121*Sqrt[1 - e^2]) + 22*e^12*(-6161399021568 + 216924893193769*Sqrt[1 - e^2]) + 
     72*e^10*(-11130541965312 + 4108777745631367*Sqrt[1 - e^2]) + 144*e^8*(-16647276462080 + 45235449047413331*Sqrt[1 - e^2]) + 1024*e^2*(-94687401607168 + 211727941767355051*Sqrt[1 - e^2]) + 
     128*e^6*(-44798530551808 + 442429051047228339*Sqrt[1 - e^2]) + 256*e^4*(-65437758914560 + 747833868489642477*Sqrt[1 - e^2]))*M*Pi)/(33554432*Sqrt[1 - e^2]), 0, 
  ((6758500761600*(15466496 + 4246158883*Sqrt[1 - e^2]) + e^16*(-178112293765120 + 88209538015359*Sqrt[1 - e^2]) + 448*e^14*(-17775795896320 + 176460184251279*Sqrt[1 - e^2]) + 
     1792*e^12*(-48675803889664 + 5845760135984857*Sqrt[1 - e^2]) + 17920*e^10*(-21681129127936 + 24802009254804225*Sqrt[1 - e^2]) + 786432*e^4*(-8622009876480 + 186359644136318747*Sqrt[1 - e^2]) + 
     196608*e^2*(-196212333477888 + 719545218648781981*Sqrt[1 - e^2]) + 2560*e^8*(-394227833896960 + 2904982323887609331*Sqrt[1 - e^2]) + 
     8192*e^6*(-285575689011200 + 6349500373838722939*Sqrt[1 - e^2]))*M*Pi)/(1073741824*Sqrt[1 - e^2]), 0, 
  (3*(-6141803233280*e^18 + 4136405729280*(34340864 + 13525376251*Sqrt[1 - e^2]) + e^16*(-1020767697371136 + 2582973662986025*Sqrt[1 - e^2]) + 
     64*e^14*(-362183317782528 + 12997739535856253*Sqrt[1 - e^2]) + 1792*e^12*(-97313624162304 + 38333731367478043*Sqrt[1 - e^2]) + 524288*e^4*(-18123560910848 + 728313654757780013*Sqrt[1 - e^2]) + 
     1536*e^10*(-403435639799808 + 1381806112066488025*Sqrt[1 - e^2]) + 65536*e^2*(-811458489614336 + 4846906662959952887*Sqrt[1 - e^2]) + 
     2560*e^8*(-567378970673152 + 10895569381406599277*Sqrt[1 - e^2]) + 8192*e^6*(-403845368774656 + 19562002959597691375*Sqrt[1 - e^2]))*M*Pi)/(1073741824*Sqrt[1 - e^2])}, -3, 37, 2],
		px === "x",
		SeriesData[x, 0, {2*M*Pi, 0, (-6*M*Pi)/(-1 + e^2), 0, (-3*(-18 + 7*e^2)*M*Pi)/(2*(-1 + e^2)^2), 0, (3*(-8*e^4 - 10*(7 + 2*Sqrt[1 - e^2]) + e^2*(51 + 20*Sqrt[1 - e^2]))*M*Pi)/(2*(-1 + e^2)^3), 0, 
  -((160*e^6*(27 + Sqrt[1 - e^2]) - 120*(88 + 101*Sqrt[1 - e^2]) - 15*e^4*(1280 + 279*Sqrt[1 - e^2]) + 24*e^2*(1060 + 549*Sqrt[1 - e^2]))*M*Pi)/(32*(1 - e^2)^(9/2)), 0, 
  -((224*e^8*(-45 + 2*Sqrt[1 - e^2]) + 264*e^2*(791 + 321*Sqrt[1 - e^2]) + 24*e^6*(3225 + 587*Sqrt[1 - e^2]) - 24*(3158 + 1945*Sqrt[1 - e^2]) - 3*e^4*(66784 + 19783*Sqrt[1 - e^2]))*M*Pi)/
   (32*(1 - e^2)^(11/2)), 0, (3*(1728*e^10*(-10 + Sqrt[1 - e^2]) + 80*e^8*(3274 + 1125*Sqrt[1 - e^2]) + 48*(12580 + 6131*Sqrt[1 - e^2]) - 5*e^6*(228864 + 107287*Sqrt[1 - e^2]) - 
     8*e^2*(235000 + 111929*Sqrt[1 - e^2]) + 2*e^4*(1087920 + 533237*Sqrt[1 - e^2]))*M*Pi)/(128*(1 - e^2)^(13/2)), 0, 
  -((22528*e^12*Sqrt[1 - e^2] + 2112*e^10*(2635 + 1272*Sqrt[1 - e^2]) - 144*(200762 + 111979*Sqrt[1 - e^2]) + 504*e^2*(206944 + 131935*Sqrt[1 - e^2]) - 48*e^8*(796345 + 489837*Sqrt[1 - e^2]) - 
      18*e^4*(8275816 + 5725169*Sqrt[1 - e^2]) + 3*e^6*(35411360 + 24368619*Sqrt[1 - e^2]))*M*Pi)/(384*(1 - e^2)^(15/2)), 0, 
  ((1171456*e^14 - 139776*e^12*(-2563 + 240*Sqrt[1 - e^2]) - 4992*e^10*(1011049 + 234880*Sqrt[1 - e^2]) + 10368*(706633 + 857072*Sqrt[1 - e^2]) - 2304*e^2*(14917097 + 13002196*Sqrt[1 - e^2]) - 
     672*e^6*(80098217 + 42254644*Sqrt[1 - e^2]) + 288*e^4*(213852913 + 142598504*Sqrt[1 - e^2]) + 3*e^8*(7997345129 + 3201251968*Sqrt[1 - e^2]))*M*Pi)/(24576*(-1 + e^2)^8), 0, 
  (-3*(829440*e^14*(-47 + 12*Sqrt[1 - e^2]) + 6912*e^12*(218373 + 64840*Sqrt[1 - e^2]) + 128*(44976157 + 34772798*Sqrt[1 - e^2]) - 128*e^2*(227720134 + 158878597*Sqrt[1 - e^2]) - 
     16*e^10*(684353291 + 303653400*Sqrt[1 - e^2]) + 32*e^4*(1840067289 + 1192327247*Sqrt[1 - e^2]) - 16*e^6*(3845201270 + 2305810547*Sqrt[1 - e^2]) + e^8*(35498366239 + 19024366160*Sqrt[1 - e^2]))*
    M*Pi)/(8192*(-1 + e^2)^9), 0, -((52084736*e^18 + 41361408*e^16*(467 + 120*Sqrt[1 - e^2]) + 1292544*e^14*(1018759 + 298560*Sqrt[1 - e^2]) - 6912*(1727447825 + 999966436*Sqrt[1 - e^2]) - 
      4896*e^12*(2892995555 + 1205713488*Sqrt[1 - e^2]) + 3456*e^2*(19058474451 + 12365654408*Sqrt[1 - e^2]) - 5184*e^4*(29246874777 + 19794020938*Sqrt[1 - e^2]) + 
      864*e^6*(219899197731 + 145575819016*Sqrt[1 - e^2]) + 27*e^10*(2247269232385 + 1176726999872*Sqrt[1 - e^2]) - 54*e^8*(2594716798903 + 1578676995936*Sqrt[1 - e^2]))*M*Pi)/
   (884736*(-1 + e^2)^10), 0, ((32374784*e^20*(-135 + 2*Sqrt[1 - e^2]) + 364216320*e^18*(-1762 + 317*Sqrt[1 - e^2]) + 9105408*e^16*(2055691 + 432050*Sqrt[1 - e^2]) - 
     21888*e^14*(7144410505 + 2615071676*Sqrt[1 - e^2]) + 20736*(1961945558 + 3244936213*Sqrt[1 - e^2]) - 3456*e^2*(100022122876 + 120719688743*Sqrt[1 - e^2]) + 
     792*e^12*(815319089372 + 389524796947*Sqrt[1 - e^2]) + 1728*e^4*(674619505435 + 646479737124*Sqrt[1 - e^2]) - 1728*e^6*(1222669189322 + 975114671403*Sqrt[1 - e^2]) - 
     81*e^10*(19130328690112 + 11035064053129*Sqrt[1 - e^2]) + 54*e^8*(42484303408336 + 28817628097363*Sqrt[1 - e^2]))*M*Pi)/(884736*(1 - e^2)^(23/2)), 0, 
  (-3*(35831808*e^20*(-880 + 1541*Sqrt[1 - e^2]) + 663552*e^18*(14716505 + 3323871*Sqrt[1 - e^2]) - 6912*e^16*(17124323076 + 6102583457*Sqrt[1 - e^2]) - 
     3072*(18175689016 + 21743737895*Sqrt[1 - e^2]) + 1024*e^2*(492354818764 + 476871908747*Sqrt[1 - e^2]) + 5376*e^6*(695861926616 + 525023214629*Sqrt[1 - e^2]) + 
     32*e^14*(19816461172514 + 8979976080135*Sqrt[1 - e^2]) - 128*e^4*(14460098568768 + 12165797996377*Sqrt[1 - e^2]) - 56*e^8*(83186730885968 + 56744109341999*Sqrt[1 - e^2]) + 
     8*e^10*(467293786022120 + 285837900844541*Sqrt[1 - e^2]) - e^12*(1943787147759104 + 1043165820390083*Sqrt[1 - e^2]))*M*Pi)/(524288*(1 - e^2)^(25/2)), 0, 
  -((204996608*e^24*(-1485 + 26*Sqrt[1 - e^2]) - 153747456*e^22*(921965 + 134451*Sqrt[1 - e^2]) - 105701376*e^20*(59061133 + 10947366*Sqrt[1 - e^2]) + 
      15614976*e^18*(6912092549 + 2121026059*Sqrt[1 - e^2]) - 2239488*(42554667166 + 34213461509*Sqrt[1 - e^2]) + 248832*e^2*(3545678582677 + 2801122487021*Sqrt[1 - e^2]) - 
      39744*e^16*(18226877323252 + 7443391527487*Sqrt[1 - e^2]) - 31104*e^4*(111429604566876 + 86066696462261*Sqrt[1 - e^2]) + 2808*e^14*(971995590553488 + 484579356292535*Sqrt[1 - e^2]) + 
      6912*e^6*(1118725181626327 + 832436629029251*Sqrt[1 - e^2]) + 216*e^10*(47561255748370928 + 30751631013419193*Sqrt[1 - e^2]) - 216*e^8*(50621338585206784 + 35547598201373067*Sqrt[1 - e^2]) - 
      27*e^12*(240711986548176896 + 139183586148390953*Sqrt[1 - e^2]))*M*Pi)/(42467328*(1 - e^2)^(27/2)), 0, 
  -((4099932160*e^28*(-35 + Sqrt[1 - e^2]) - 524288*e^26*(-68356415 + 5014544*Sqrt[1 - e^2]) + 753664*e^24*(-11316473753 + 654057074*Sqrt[1 - e^2]) + 
      24576*e^22*(17028816337101 + 3413260004852*Sqrt[1 - e^2]) - 165888*(14251325102917 + 9733231670258*Sqrt[1 - e^2]) + 165888*e^2*(148247026454557 + 118968175180663*Sqrt[1 - e^2]) - 
      1152*e^20*(4657333416454715 + 1478118978130542*Sqrt[1 - e^2]) - 20736*e^4*(5525706677031089 + 4628505679667801*Sqrt[1 - e^2]) + 
      288*e^18*(118496543652435557 + 48747378084855490*Sqrt[1 - e^2]) + 3456*e^6*(90590622595115456 + 74542482466522921*Sqrt[1 - e^2]) - 
      594*e^16*(219873377074238501 + 109165090864819604*Sqrt[1 - e^2]) - 432*e^8*(1285362362249902581 + 1002000734980958549*Sqrt[1 - e^2]) + 
      216*e^10*(3103125024177456182 + 2234464653215232253*Sqrt[1 - e^2]) - 54*e^12*(10406673842552964271 + 6776453095342817053*Sqrt[1 - e^2]) + 
      27*e^14*(12135797343238511308 + 6992935737320648171*Sqrt[1 - e^2]))*M*Pi)/(169869312*(1 - e^2)^(29/2)), 0, 
  ((8766095360*e^30*(-75 + 2*Sqrt[1 - e^2]) - 23592960*e^28*(-7465455 + 539108*Sqrt[1 - e^2]) + 196608*e^26*(-109227093345 + 10921966592*Sqrt[1 - e^2]) + 
     24576*e^24*(145928458163737 + 25671383725144*Sqrt[1 - e^2]) + 497664*(75804510729781 + 63305918554634*Sqrt[1 - e^2]) - 497664*e^2*(904249170521586 + 826781008713881*Sqrt[1 - e^2]) - 
     4608*e^22*(13251249073453791 + 3723337581935080*Sqrt[1 - e^2]) + 62208*e^4*(38464625440287619 + 35047580375008157*Sqrt[1 - e^2]) - 
     10368*e^6*(716623397635690858 + 623817695164949927*Sqrt[1 - e^2]) + 144*e^20*(3313380497327805629 + 1230470885763040144*Sqrt[1 - e^2]) + 
     6480*e^8*(2310754534712596795 + 1878959607838383157*Sqrt[1 - e^2]) - 54*e^18*(40727839999558686493 + 18503773717238993456*Sqrt[1 - e^2]) - 
     648*e^10*(31858292993930948876 + 23833279262118089871*Sqrt[1 - e^2]) + 54*e^16*(122686730996255751617 + 65326775293155784140*Sqrt[1 - e^2]) + 
     54*e^12*(370193694647585754647 + 251375052334180558045*Sqrt[1 - e^2]) - 27*e^14*(507998302719277843862 + 308429572403327846589*Sqrt[1 - e^2]))*M*Pi)/(509607936*(1 - e^2)^(31/2))}, -3, 29, 2],

		True,
		Print["Expansion parameter is either \"p\" or \"x\""];
		Aborting[syms];
	]
]


def@
azimuthalAdvanceSchwPN[syms_Association,px_String]:=
Module[{p,e,x},
	
	e=OrbitalEccentricitySymbol[syms];
	p=SemiLatusRectumSymbol[syms];
	x=XPNSymbol[syms];
	
	Which[px==="p",
		SeriesData[p, Infinity, {2*Pi, 6*Pi, (3*(18 + e^2)*Pi)/2, (45*(6 + e^2)*Pi)/2, (105*(216 + 72*e^2 + e^4)*Pi)/32, (567*(216 + 120*e^2 + 5*e^4)*Pi)/32, 
  (231*(11664 + 9720*e^2 + 810*e^4 + 5*e^6)*Pi)/128, (1287*(11664 + 13608*e^2 + 1890*e^4 + 35*e^6)*Pi)/128, (6435*(839808 + 1306368*e^2 + 272160*e^4 + 10080*e^6 + 35*e^8)*Pi)/8192, 
  (328185*(93312 + 186624*e^2 + 54432*e^4 + 3360*e^6 + 35*e^8)*Pi)/8192, (415701*(1679616 + 4199040*e^2 + 1632960*e^4 + 151200*e^6 + 3150*e^8 + 7*e^10)*Pi)/32768, 
  (2380833*(1679616 + 5132160*e^2 + 2566080*e^4 + 332640*e^6 + 11550*e^8 + 77*e^10)*Pi)/32768, 
  (2028117*(181398528 + 665127936*e^2 + 415704960*e^4 + 71850240*e^6 + 3742200*e^8 + 49896*e^10 + 77*e^12)*Pi)/524288, 
  (11700675*(181398528 + 786060288*e^2 + 600462720*e^4 + 133436160*e^6 + 9729720*e^8 + 216216*e^10 + 1001*e^12)*Pi)/524288, 
  (15043725*(3265173504 + 16507266048*e^2 + 15131660544*e^4 + 4203239040*e^6 + 408648240*e^8 + 13621608*e^10 + 126126*e^12 + 143*e^14)*Pi)/2097152, 
  (261760815*(1088391168 + 6348948480*e^2 + 6878027520*e^4 + 2335132800*e^6 + 291891600*e^8 + 13621608*e^10 + 210210*e^12 + 715*e^14)*Pi)/2097152, 
  (2704861755*(156728328192 + 1044855521280*e^2 + 1320581283840*e^4 + 538014597120*e^6 + 84064780800*e^8 + 5230697472*e^10 + 121080960*e^12 + 823680*e^14 + 715*e^16)*Pi)/536870912, 
  (15751841985*(156728328192 + 1184169590784*e^2 + 1726913986560*e^4 + 831477104640*e^6 + 158789030400*e^8 + 12703122432*e^10 + 411675264*e^12 + 4667520*e^14 + 12155*e^16)*Pi)/536870912, 
  (2268783825*(25389989167104 + 215814907920384*e^2 + 359691513200640*e^4 + 202048936427520*e^6 + 46302881264640*e^8 + 4630288126464*e^10 + 200074178304*e^12 + 3402622080*e^14 + 17721990*e^16 + 
     12155*e^18)*Pi)/2147483648, (13254473925*(25389989167104 + 241204897087488*e^2 + 455609250054144*e^4 + 295302291701760*e^6 + 79977704002560*e^8 + 9775052711424*e^10 + 543058483968*e^12 + 
     12929963904*e^14 + 112239270*e^16 + 230945*e^18)*Pi)/2147483648, (34461632205*(914039610015744 + 9648195883499520*e^2 + 20502416252436480*e^4 + 15186975001804800*e^6 + 4798662240153600*e^8 + 
     703803795222528*e^10 + 48875263557120*e^12 + 1551595668480*e^14 + 20203068600*e^16 + 83140200*e^18 + 46189*e^20)*Pi)/34359738368}, 0, 21, 1],
		
		px === "x",
		SeriesData[x, 0, {2*Pi, (-6*Pi)/(-1 + e^2), (-3*(-18 + 7*e^2)*Pi)/(2*(-1 + e^2)^2), (3*(20 + 70*Sqrt[1 - e^2] + 4*e^4*(5 + 2*Sqrt[1 - e^2]) - e^2*(40 + 51*Sqrt[1 - e^2]))*Pi)/(2*(1 - e^2)^(7/2)), 
  -((160*e^6*(27 + Sqrt[1 - e^2]) - 120*(88 + 101*Sqrt[1 - e^2]) - 15*e^4*(1280 + 279*Sqrt[1 - e^2]) + 24*e^2*(1060 + 549*Sqrt[1 - e^2]))*Pi)/(32*(1 - e^2)^(9/2)), 
  -((224*e^8*(-45 + 2*Sqrt[1 - e^2]) + 264*e^2*(791 + 321*Sqrt[1 - e^2]) + 24*e^6*(3225 + 587*Sqrt[1 - e^2]) - 24*(3158 + 1945*Sqrt[1 - e^2]) - 3*e^4*(66784 + 19783*Sqrt[1 - e^2]))*Pi)/
   (32*(1 - e^2)^(11/2)), (3*(1728*e^10*(-10 + Sqrt[1 - e^2]) + 80*e^8*(3274 + 1125*Sqrt[1 - e^2]) + 48*(12580 + 6131*Sqrt[1 - e^2]) - 5*e^6*(228864 + 107287*Sqrt[1 - e^2]) - 
     8*e^2*(235000 + 111929*Sqrt[1 - e^2]) + 2*e^4*(1087920 + 533237*Sqrt[1 - e^2]))*Pi)/(128*(1 - e^2)^(13/2)), 
  -((22528*e^12*Sqrt[1 - e^2] + 2112*e^10*(2635 + 1272*Sqrt[1 - e^2]) - 144*(200762 + 111979*Sqrt[1 - e^2]) + 504*e^2*(206944 + 131935*Sqrt[1 - e^2]) - 48*e^8*(796345 + 489837*Sqrt[1 - e^2]) - 
      18*e^4*(8275816 + 5725169*Sqrt[1 - e^2]) + 3*e^6*(35411360 + 24368619*Sqrt[1 - e^2]))*Pi)/(384*(1 - e^2)^(15/2)), 
  ((106496*e^14*(315 + 11*Sqrt[1 - e^2]) + 19968*e^12*(57040 + 17941*Sqrt[1 - e^2]) + 10368*(857072 + 706633*Sqrt[1 - e^2]) - 384*e^10*(28063221 + 13143637*Sqrt[1 - e^2]) - 
     2304*e^2*(16859020 + 14917097*Sqrt[1 - e^2]) + 288*e^4*(246616072 + 213852913*Sqrt[1 - e^2]) - 96*e^6*(723578020 + 560687519*Sqrt[1 - e^2]) + 3*e^8*(12666292224 + 7997345129*Sqrt[1 - e^2]))*
    Pi)/(24576*(1 - e^2)^(17/2)), (-3*(829440*e^14*(-47 + 12*Sqrt[1 - e^2]) + 6912*e^12*(218373 + 64840*Sqrt[1 - e^2]) + 128*(44976157 + 34772798*Sqrt[1 - e^2]) - 
     128*e^2*(227720134 + 158878597*Sqrt[1 - e^2]) - 16*e^10*(684353291 + 303653400*Sqrt[1 - e^2]) + 32*e^4*(1840067289 + 1192327247*Sqrt[1 - e^2]) - 
     16*e^6*(3845201270 + 2305810547*Sqrt[1 - e^2]) + e^8*(35498366239 + 19024366160*Sqrt[1 - e^2]))*Pi)/(8192*(-1 + e^2)^9), 
  -((52084736*e^18 + 41361408*e^16*(467 + 120*Sqrt[1 - e^2]) + 1292544*e^14*(1018759 + 298560*Sqrt[1 - e^2]) - 6912*(1727447825 + 999966436*Sqrt[1 - e^2]) - 
      4896*e^12*(2892995555 + 1205713488*Sqrt[1 - e^2]) + 3456*e^2*(19058474451 + 12365654408*Sqrt[1 - e^2]) - 5184*e^4*(29246874777 + 19794020938*Sqrt[1 - e^2]) + 
      864*e^6*(219899197731 + 145575819016*Sqrt[1 - e^2]) + 27*e^10*(2247269232385 + 1176726999872*Sqrt[1 - e^2]) - 54*e^8*(2594716798903 + 1578676995936*Sqrt[1 - e^2]))*Pi)/(884736*(-1 + e^2)^10), 
  ((32374784*e^20*(-135 + 2*Sqrt[1 - e^2]) + 364216320*e^18*(-1762 + 317*Sqrt[1 - e^2]) + 9105408*e^16*(2055691 + 432050*Sqrt[1 - e^2]) - 21888*e^14*(7144410505 + 2615071676*Sqrt[1 - e^2]) + 
     20736*(1961945558 + 3244936213*Sqrt[1 - e^2]) - 3456*e^2*(100022122876 + 120719688743*Sqrt[1 - e^2]) + 792*e^12*(815319089372 + 389524796947*Sqrt[1 - e^2]) + 
     1728*e^4*(674619505435 + 646479737124*Sqrt[1 - e^2]) - 1728*e^6*(1222669189322 + 975114671403*Sqrt[1 - e^2]) - 81*e^10*(19130328690112 + 11035064053129*Sqrt[1 - e^2]) + 
     54*e^8*(42484303408336 + 28817628097363*Sqrt[1 - e^2]))*Pi)/(884736*(1 - e^2)^(23/2)), 
  (-3*(35831808*e^20*(-880 + 1541*Sqrt[1 - e^2]) + 663552*e^18*(14716505 + 3323871*Sqrt[1 - e^2]) - 6912*e^16*(17124323076 + 6102583457*Sqrt[1 - e^2]) - 
     3072*(18175689016 + 21743737895*Sqrt[1 - e^2]) + 1024*e^2*(492354818764 + 476871908747*Sqrt[1 - e^2]) + 5376*e^6*(695861926616 + 525023214629*Sqrt[1 - e^2]) + 
     32*e^14*(19816461172514 + 8979976080135*Sqrt[1 - e^2]) - 128*e^4*(14460098568768 + 12165797996377*Sqrt[1 - e^2]) - 56*e^8*(83186730885968 + 56744109341999*Sqrt[1 - e^2]) + 
     8*e^10*(467293786022120 + 285837900844541*Sqrt[1 - e^2]) - e^12*(1943787147759104 + 1043165820390083*Sqrt[1 - e^2]))*Pi)/(524288*(1 - e^2)^(25/2)), 
  -((204996608*e^24*(-1485 + 26*Sqrt[1 - e^2]) - 153747456*e^22*(921965 + 134451*Sqrt[1 - e^2]) - 105701376*e^20*(59061133 + 10947366*Sqrt[1 - e^2]) + 
      15614976*e^18*(6912092549 + 2121026059*Sqrt[1 - e^2]) - 2239488*(42554667166 + 34213461509*Sqrt[1 - e^2]) + 248832*e^2*(3545678582677 + 2801122487021*Sqrt[1 - e^2]) - 
      39744*e^16*(18226877323252 + 7443391527487*Sqrt[1 - e^2]) - 31104*e^4*(111429604566876 + 86066696462261*Sqrt[1 - e^2]) + 2808*e^14*(971995590553488 + 484579356292535*Sqrt[1 - e^2]) + 
      6912*e^6*(1118725181626327 + 832436629029251*Sqrt[1 - e^2]) + 216*e^10*(47561255748370928 + 30751631013419193*Sqrt[1 - e^2]) - 216*e^8*(50621338585206784 + 35547598201373067*Sqrt[1 - e^2]) - 
      27*e^12*(240711986548176896 + 139183586148390953*Sqrt[1 - e^2]))*Pi)/(42467328*(1 - e^2)^(27/2)), 
  ((996147200*e^26*(-1638 + 29*Sqrt[1 - e^2]) - 1116930048000*e^24*(1926 + 67*Sqrt[1 - e^2]) - 121405440*e^22*(548617540 + 41423287*Sqrt[1 - e^2]) + 
     25681920*e^20*(60684515890 + 14542942167*Sqrt[1 - e^2]) + 2820096*(873591439796 + 537264840979*Sqrt[1 - e^2]) - 82944*e^2*(286528845007256 + 208378760122175*Sqrt[1 - e^2]) - 
     10560*e^18*(1219318477855948 + 434837804189291*Sqrt[1 - e^2]) + 20736*e^4*(4865785543800888 + 3764182873262209*Sqrt[1 - e^2]) - 
     10368*e^6*(24074153404481184 + 18642533223193307*Sqrt[1 - e^2]) + 432*e^16*(135868314533444164 + 61964742244655975*Sqrt[1 - e^2]) + 
     432*e^8*(925496541508661464 + 688105048105219253*Sqrt[1 - e^2]) - 216*e^10*(2012904029000529712 + 1390647929166897309*Sqrt[1 - e^2]) - 
     27*e^14*(6261882647150776320 + 3409225664219278279*Sqrt[1 - e^2]) + 54*e^12*(6050757044820563552 + 3770773400650555113*Sqrt[1 - e^2]))*Pi)/(169869312*(1 - e^2)^(29/2))}, 0, 15, 1],

		True,
		Print["Expansion parameter is either \"p\" or \"x\""];
		Aborting[syms];
	]
]


def@
omegaPhiSchwPN[syms_Association,px_String]:=
Module[{M,p,e,x},
	
	M=BlackHoleMassSymbol[syms];
	e=OrbitalEccentricitySymbol[syms];
	p=SemiLatusRectumSymbol[syms];
	x=XPNSymbol[syms];
	
	Which[px==="p",
		SeriesData[p, Infinity, {(1 - e^2)^(3/2)/M, 0, (3*e^2*(1 - e^2)^(3/2))/M, 0, (3*(1 - e^2)^(3/2)*(12*e^4 - 10*(-1 + Sqrt[1 - e^2]) + e^2*(-3 + 10*Sqrt[1 - e^2])))/(4*M), 0, 
  (3*(1 - e^2)^(3/2)*(36*e^6 + e^2*(78 - 40*Sqrt[1 - e^2]) - 20*(-1 + Sqrt[1 - e^2]) + e^4*(-29 + 60*Sqrt[1 - e^2])))/(4*M), 0, 
  (3*(1 - e^2)^(3/2)*(1728*e^8 + e^4*(8995 - 3140*Sqrt[1 - e^2]) - 4152*(-1 + Sqrt[1 - e^2]) + 96*e^6*(-37 + 45*Sqrt[1 - e^2]) + 4*e^2*(-302 + 743*Sqrt[1 - e^2])))/(64*M), 0, 
  (3*(1 - e^2)^(3/2)*(5184*e^10 + e^6*(49753 - 17880*Sqrt[1 - e^2]) - 18912*(-1 + Sqrt[1 - e^2]) + 3456*e^8*(-6 + 5*Sqrt[1 - e^2]) - 8*e^2*(-3535 + 2029*Sqrt[1 - e^2]) + 
     16*e^4*(-1059 + 2234*Sqrt[1 - e^2])))/(64*M), 0, ((1 - e^2)^(3/2)*(186624*e^12 - 1761712*(-1 + Sqrt[1 - e^2]) + 5184*e^10*(-239 + 150*Sqrt[1 - e^2]) - 36*e^8*(-82921 + 34140*Sqrt[1 - e^2]) + 
     8*e^2*(41277 + 37544*Sqrt[1 - e^2]) - 30*e^4*(-110393 + 37950*Sqrt[1 - e^2]) + e^6*(-2208553 + 3051300*Sqrt[1 - e^2])))/(256*M), 0, 
  (3*(1 - e^2)^(3/2)*(186624*e^14 + e^6*(8057494 - 3031056*Sqrt[1 - e^2]) + e^2*(3350416 - 2056640*Sqrt[1 - e^2]) - 3408992*(-1 + Sqrt[1 - e^2]) + 5184*e^12*(-359 + 180*Sqrt[1 - e^2]) - 
     324*e^10*(-14721 + 6800*Sqrt[1 - e^2]) + 4*e^4*(-80893 + 960418*Sqrt[1 - e^2]) + e^8*(-5920989 + 5925096*Sqrt[1 - e^2])))/(256*M), 0, 
  (3*(1 - e^2)^(3/2)*(35831808*e^16 + e^8*(3321800963 - 1556100168*Sqrt[1 - e^2]) - 1474036096*(-1 + Sqrt[1 - e^2]) + 41803776*e^14*(-12 + 5*Sqrt[1 - e^2]) - 
     6912*e^12*(-206957 + 102060*Sqrt[1 - e^2]) + 576*e^10*(-4357135 + 3460224*Sqrt[1 - e^2]) - 64*e^2*(-13369956 + 6945383*Sqrt[1 - e^2]) - 48*e^4*(-45093238 + 8626961*Sqrt[1 - e^2]) + 
     24*e^6*(-44246908 + 99669405*Sqrt[1 - e^2])))/(16384*M), 0, ((1 - e^2)^(3/2)*(322486272*e^18 + e^10*(60719018715 - 33492512016*Sqrt[1 - e^2]) - 27522343936*(-1 + Sqrt[1 - e^2]) + 
     17915904*e^16*(-337 + 120*Sqrt[1 - e^2]) - 933120*e^14*(-21155 + 10648*Sqrt[1 - e^2]) + 5184*e^12*(-8484467 + 5710176*Sqrt[1 - e^2]) + 192*e^4*(74035538 + 87188183*Sqrt[1 - e^2]) - 
     128*e^2*(-206045967 + 152238259*Sqrt[1 - e^2]) - 16*e^6*(-3293219990 + 673907171*Sqrt[1 - e^2]) + 16*e^8*(-2544646419 + 3295508600*Sqrt[1 - e^2])))/(16384*M), 0, 
  (3*(1 - e^2)^(3/2)*(1289945088*e^20 + e^8*(502618184122 - 178658820956*Sqrt[1 - e^2]) - 238579468032*(-1 + Sqrt[1 - e^2]) + 35831808*e^18*(-869 + 270*Sqrt[1 - e^2]) - 
     746496*e^16*(-159031 + 79060*Sqrt[1 - e^2]) + 76032*e^14*(-4138007 + 2472924*Sqrt[1 - e^2]) - 384*e^2*(-563163065 + 443766182*Sqrt[1 - e^2]) - 32*e^4*(-9797504192 + 843317355*Sqrt[1 - e^2]) + 
     192*e^6*(102381684 + 1600580425*Sqrt[1 - e^2]) - 108*e^12*(-4407050873 + 2651729640*Sqrt[1 - e^2]) + e^10*(-478587461267 + 455028045788*Sqrt[1 - e^2])))/(65536*M), 0, 
  (3*(1 - e^2)^(3/2)*(3869835264*e^22 - 1515951269376*(-1 + Sqrt[1 - e^2]) + 322486272*e^20*(-363 + 100*Sqrt[1 - e^2]) - 2239488*e^18*(-234087 + 112480*Sqrt[1 - e^2]) + 
     20736*e^16*(-76624033 + 42013080*Sqrt[1 - e^2]) + 128*e^6*(19971814648 + 1114323247*Sqrt[1 - e^2]) + 64*e^4*(23069258922 + 4976221771*Sqrt[1 - e^2]) - 
     256*e^2*(-6757804297 + 5848614748*Sqrt[1 - e^2]) - 108*e^14*(-25061739515 + 15533060256*Sqrt[1 - e^2]) - 6*e^10*(-579737981757 + 298875743240*Sqrt[1 - e^2]) + 
     4*e^8*(-269252256491 + 656236186210*Sqrt[1 - e^2]) + e^12*(-3493443389101 + 2746431054312*Sqrt[1 - e^2])))/(65536*M), 0, 
  ((1 - e^2)^(3/2)*(557256278016*e^24 + e^12*(1138096810271793 - 709074176545128*Sqrt[1 - e^2]) + e^8*(864042319345224 - 65116821889712*Sqrt[1 - e^2]) - 468326949217280*(-1 + Sqrt[1 - e^2]) + 
     30958682112*e^22*(-667 + 165*Sqrt[1 - e^2]) - 107495424*e^20*(-1011329 + 463500*Sqrt[1 - e^2]) + 8957952*e^18*(-41116623 + 21148592*Sqrt[1 - e^2]) - 
     5184*e^16*(-137693701769 + 84804528696*Sqrt[1 - e^2]) - 384*e^4*(-1723786216429 + 246521764514*Sqrt[1 - e^2]) - 512*e^2*(-1129895360298 + 1029980181443*Sqrt[1 - e^2]) + 
     576*e^14*(-1893746680430 + 1320169681221*Sqrt[1 - e^2]) + 128*e^6*(3041223779030 + 3744076373257*Sqrt[1 - e^2]) + 24*e^10*(-30421865134627 + 38323633216505*Sqrt[1 - e^2])))/(1048576*M), 0, 
  (3*(1 - e^2)^(3/2)*(557256278016*e^26 + e^14*(2521664597842913 - 1695594485531472*Sqrt[1 - e^2]) - 998543868956672*(-1 + Sqrt[1 - e^2]) + 61917364224*e^24*(-401 + 90*Sqrt[1 - e^2]) - 
     322486272*e^22*(-475099 + 206120*Sqrt[1 - e^2]) + 8957952*e^20*(-63956395 + 31307584*Sqrt[1 - e^2]) - 585792*e^18*(-2155578233 + 1292886288*Sqrt[1 - e^2]) - 
     1024*e^4*(-1363963209187 + 196848062368*Sqrt[1 - e^2]) - 5120*e^2*(-280266525419 + 266328690373*Sqrt[1 - e^2]) + 896*e^6*(1970562525749 + 450544704254*Sqrt[1 - e^2]) + 
     864*e^16*(-2533117972863 + 1634084573320*Sqrt[1 - e^2]) + 64*e^12*(-35415410484579 + 32657300764457*Sqrt[1 - e^2]) + 32*e^8*(5901184541207 + 48224212639094*Sqrt[1 - e^2]) - 
     8*e^10*(-250574564242427 + 81457074669622*Sqrt[1 - e^2])))/(1048576*M), 0, 
  (3*(1 - e^2)^(3/2)*(6687075336192*e^28 + e^16*(64663783070008236 - 44129889957799584*Sqrt[1 - e^2]) + e^12*(58082514196804586 - 31821581661471032*Sqrt[1 - e^2]) - 
     25649344091572224*(-1 + Sqrt[1 - e^2]) + 557256278016*e^26*(-633 + 130*Sqrt[1 - e^2]) - 6449725440*e^24*(-393533 + 161004*Sqrt[1 - e^2]) + 107495424*e^22*(-96796027 + 45493836*Sqrt[1 - e^2]) - 
     186624*e^20*(-138929217675 + 80333270104*Sqrt[1 - e^2]) + 8640*e^18*(-5766766031479 + 3530815091652*Sqrt[1 - e^2]) + 896*e^6*(43563764296933 + 23921953465920*Sqrt[1 - e^2]) - 
     1024*e^2*(-40080622018411 + 39230539974916*Sqrt[1 - e^2]) - 256*e^4*(-172604643986367 + 53361985701728*Sqrt[1 - e^2]) + 16*e^8*(2935191459980661 + 780304326493886*Sqrt[1 - e^2]) + 
     8*e^10*(-2357444433013221 + 5899173303327008*Sqrt[1 - e^2]) + e^14*(-70426467211523939 + 54882713925225464*Sqrt[1 - e^2])))/(4194304*M), 0, 
  ((1 - e^2)^(3/2)*(60183678025728*e^30 + e^14*(1285702502759169378 - 866339177673537632*Sqrt[1 - e^2]) - 493389867745628160*(-1 + Sqrt[1 - e^2]) + 11702381838336*e^28*(-317 + 60*Sqrt[1 - e^2]) - 
     11609505792*e^26*(-2664433 + 1026480*Sqrt[1 - e^2]) + 322486272*e^24*(-429645157 + 194849544*Sqrt[1 - e^2]) - 5038848*e^22*(-76831516073 + 42654744000*Sqrt[1 - e^2]) + 
     15552*e^20*(-52407643270691 + 30892547406744*Sqrt[1 - e^2]) - 6144*e^2*(-145165603856861 + 145195296900580*Sqrt[1 - e^2]) - 1536*e^4*(-609393729586977 + 236536453936880*Sqrt[1 - e^2]) + 
     256*e^6*(4121767808783119 + 969180804811360*Sqrt[1 - e^2]) + 48*e^10*(18390677504393517 + 1001931506084200*Sqrt[1 - e^2]) - 108*e^18*(-11052340583881481 + 7425356142150048*Sqrt[1 - e^2]) + 
     32*e^8*(17036400969259833 + 23358776676983182*Sqrt[1 - e^2]) + 4*e^12*(-207080756929174159 + 251825813566311644*Sqrt[1 - e^2]) + 
     3*e^16*(-492596837279244349 + 349649551232655120*Sqrt[1 - e^2])))/(4194304*M), 0, 
  (3*(1 - e^2)^(3/2)*(15407021574586368*e^32 + e^16*(795644544923777984067 - 572549462095607831184*Sqrt[1 - e^2]) - 270318634116421877760*(-1 + Sqrt[1 - e^2]) + 
     855945643032576*e^30*(-1282 + 225*Sqrt[1 - e^2]) - 80244904034304*e^28*(-131263 + 47620*Sqrt[1 - e^2]) + 247669456896*e^26*(-207824671 + 91173120*Sqrt[1 - e^2]) - 
     143327232*e^24*(-1115527206025 + 594177619608*Sqrt[1 - e^2]) + 23887872*e^22*(-15338338238631 + 8769426346702*Sqrt[1 - e^2]) - 414720*e^20*(-1450783968774251 + 946592747500824*Sqrt[1 - e^2]) - 
     81920*e^2*(-6634108797911464 + 6743300325388519*Sqrt[1 - e^2]) - 12288*e^4*(-49121040378172060 + 24355876698042119*Sqrt[1 - e^2]) + 
     2048*e^6*(297597796585528552 + 81779630212166981*Sqrt[1 - e^2]) + 4608*e^18*(-178522119438729047 + 120082689981722640*Sqrt[1 - e^2]) + 
     256*e^8*(2311230964013559898 + 1087737236350163805*Sqrt[1 - e^2]) + 128*e^10*(462057140659719832 + 3870022815914942713*Sqrt[1 - e^2]) - 
     32*e^12*(-15953474252409905564 + 4758158064036245667*Sqrt[1 - e^2]) + 16*e^14*(-42162552696557674424 + 37592104991930361815*Sqrt[1 - e^2])))/(1073741824*M), 0, 
  (3*(1 - e^2)^(3/2)*(46221064723759104*e^34 + e^18*(5553506785173571930569 - 4010613690838708805472*Sqrt[1 - e^2]) - 1735130119345494556672*(-1 + Sqrt[1 - e^2]) + 
     7703510787293184*e^32*(-489 + 80*Sqrt[1 - e^2]) - 8916100448256*e^30*(-4637669 + 1585800*Sqrt[1 - e^2]) + 2229025112064*e^28*(-98825361 + 41980384*Sqrt[1 - e^2]) - 
     1289945088*e^26*(-584971504643 + 299240405712*Sqrt[1 - e^2]) + 95551488*e^24*(-19625872795225 + 10920564580188*Sqrt[1 - e^2]) - 
     746496*e^22*(-4583214742807197 + 2890252627521712*Sqrt[1 - e^2]) - 704512*e^4*(-6277631230166004 + 3630410990960219*Sqrt[1 - e^2]) + 
     110592*e^20*(-46104961223418457 + 29974973491375380*Sqrt[1 - e^2]) - 32768*e^2*(-118365545721064265 + 121730996235655329*Sqrt[1 - e^2]) + 
     4096*e^6*(1179459326680003844 + 151446601646625393*Sqrt[1 - e^2]) + 1280*e^10*(2594785935016804542 + 1239859061578700795*Sqrt[1 - e^2]) + 
     2048*e^8*(1780541977307400894 + 1384532253413649691*Sqrt[1 - e^2]) + 128*e^12*(-11248808103428488468 + 26746582599556410659*Sqrt[1 - e^2]) - 
     32*e^14*(-120300210424046774684 + 70277107716280698457*Sqrt[1 - e^2]) + 32*e^16*(-170046122727831235827 + 130617998034847233104*Sqrt[1 - e^2])))/(1073741824*M), 0, 
  ((1 - e^2)^(3/2)*(1663958330055327744*e^36 - 133719893669734180257792*(-1 + Sqrt[1 - e^2]) + 46221064723759104*e^34*(-3329 + 510*Sqrt[1 - e^2]) - 
     962938848411648*e^32*(-1992879 + 643220*Sqrt[1 - e^2]) + 80244904034304*e^30*(-138829885 + 57112148*Sqrt[1 - e^2]) - 46438023168*e^28*(-899807143723 + 442862049528*Sqrt[1 - e^2]) + 
     429981696*e^26*(-261234067348253 + 141655206221460*Sqrt[1 - e^2]) - 2985984*e^24*(-75548625873587173 + 46021147571282232*Sqrt[1 - e^2]) + 
     248832*e^22*(-1463658117910985177 + 927947901832633656*Sqrt[1 - e^2]) - 98304*e^2*(-3354117122993806915 + 3479598883745930078*Sqrt[1 - e^2]) + 
     8192*e^6*(51395530821148674499 + 3854295270935569756*Sqrt[1 - e^2]) - 24576*e^4*(-16127946731034481734 + 10611606116665296925*Sqrt[1 - e^2]) + 
     1536*e^10*(112138616224630986878 + 181449779585696915189*Sqrt[1 - e^2]) + 512*e^8*(771939539682288736140 + 384041853102387004841*Sqrt[1 - e^2]) + 
     64*e^12*(3508411605199805110182 + 680090486158628932507*Sqrt[1 - e^2]) - 108*e^20*(-4118327059933817863571 + 2910313578086213467440*Sqrt[1 - e^2]) + 
     64*e^14*(-3839427522507117656763 + 4407501469090091382542*Sqrt[1 - e^2]) - 6*e^16*(-60739760406830572255949 + 43787500228174473005262*Sqrt[1 - e^2]) + 
     e^18*(-480086837539668865676609 + 343276140081702491949780*Sqrt[1 - e^2])))/(4294967296*M), 0, 
  (3*(1 - e^2)^(3/2)*(1663958330055327744*e^38 + e^18*(944179854647286204066846 - 715514139715362173573104*Sqrt[1 - e^2]) - 286273110035348771438592*(-1 + Sqrt[1 - e^2]) + 
     46221064723759104*e^36*(-3749 + 540*Sqrt[1 - e^2]) - 4814694242058240*e^34*(-506867 + 154688*Sqrt[1 - e^2]) + 8916100448256*e^32*(-1730714761 + 689314488*Sqrt[1 - e^2]) - 
     139314069504*e^30*(-452186878505 + 214565255328*Sqrt[1 - e^2]) + 6449725440*e^28*(-28338973109671 + 14979428927928*Sqrt[1 - e^2]) - 
     2985984*e^26*(-134207011445065733 + 79076407950027072*Sqrt[1 - e^2]) - 16384*e^6*(-65329484550340966683 + 1876823458373645650*Sqrt[1 - e^2]) + 
     82944*e^24*(-8421838018045413187 + 5224024536533971632*Sqrt[1 - e^2]) - 65536*e^2*(-11837836344639471329 + 12357968501846861108*Sqrt[1 - e^2]) - 
     16384*e^4*(-59691154629373525036 + 43276264211617705191*Sqrt[1 - e^2]) + 5760*e^12*(7341706990108039582 + 106732812679203148347*Sqrt[1 - e^2]) + 
     3072*e^8*(308958461486663033540 + 169006047832322746957*Sqrt[1 - e^2]) + 2048*e^10*(353067503474000772227 + 248905211967027107295*Sqrt[1 - e^2]) - 
     128*e^14*(-3887507033560816223405 + 1111868735819618338314*Sqrt[1 - e^2]) - 108*e^22*(-8769360505123692720691 + 6012376114099009396800*Sqrt[1 - e^2]) + 
     e^16*(-781565570256934891655628 + 666515555308015574767432*Sqrt[1 - e^2]) + e^20*(-1106963777116432799307125 + 763554042642174349008168*Sqrt[1 - e^2])))/(4294967296*M)}, 3, 43, 2],

		px === "x",
		SeriesData[x, 0, {1/M}, 3, 3+2*20, 2]
		,

		True,
		Print["Expansion parameter is either \"p\" or \"x\""];
		Aborting[syms];
	]
]


def@
omegaRSchwPN[syms_Association,px_String]:=
Module[{M,p,e,x},
	
	M=BlackHoleMassSymbol[syms];
	e=OrbitalEccentricitySymbol[syms];
	p=SemiLatusRectumSymbol[syms];
	x=XPNSymbol[syms];
	
	Which[px==="p",
		SeriesData[p, Infinity, {(1 - e^2)^(3/2)/M, 0, (3*(1 - e^2)^(3/2)*(-1 + e^2))/M, 0, (3*(1 - e^2)^(3/2)*(-1 + e^2)*(-2 + 6*e^2 + 5*Sqrt[1 - e^2]))/(2*M), 0, 
  (3*(1 - e^2)^(3/2)*(-1 + e^2)*(14 + 18*e^4 - 5*Sqrt[1 - e^2] + 2*e^2*(-8 + 15*Sqrt[1 - e^2])))/(2*M), 0, 
  (3*(1 - e^2)^(3/2)*(-1 + e^2)*(-348 + 432*e^6 + 618*Sqrt[1 - e^2] + e^2*(1352 - 455*Sqrt[1 - e^2]) + 12*e^4*(-77 + 90*Sqrt[1 - e^2])))/(16*M), 0, 
  (3*(1 - e^2)^(3/2)*(-1 + e^2)*(420 + 1296*e^8 + 714*Sqrt[1 - e^2] + e^4*(8908 - 3570*Sqrt[1 - e^2]) + 108*e^6*(-49 + 40*Sqrt[1 - e^2]) + e^2*(-3284 + 6491*Sqrt[1 - e^2])))/(16*M), 0, 
  ((1 - e^2)^(3/2)*(-1 + e^2)*(93312*e^10 + e^2*(634552 - 91776*Sqrt[1 - e^2]) + 5184*e^8*(-121 + 75*Sqrt[1 - e^2]) - 360*e^6*(-3287 + 1545*Sqrt[1 - e^2]) + 8*(-28918 + 44227*Sqrt[1 - e^2]) + 
     e^4*(-855968 + 1148865*Sqrt[1 - e^2])))/(128*M), 0, (3*(1 - e^2)^(3/2)*(-1 + e^2)*(93312*e^12 + e^4*(2114344 - 772971*Sqrt[1 - e^2]) + 5184*e^10*(-181 + 90*Sqrt[1 - e^2]) - 
     216*e^8*(-9379 + 4860*Sqrt[1 - e^2]) + 8*(-31054 + 55111*Sqrt[1 - e^2]) + 18*e^6*(-132236 + 129901*Sqrt[1 - e^2]) + 8*e^2*(-50549 + 165820*Sqrt[1 - e^2])))/(128*M), 0, 
  (3*(1 - e^2)^(3/2)*(-1 + e^2)*(-44314208 + 4478976*e^14 + 59325776*Sqrt[1 - e^2] + 373248*e^12*(-169 + 70*Sqrt[1 - e^2]) - 3456*e^10*(-46139 + 24885*Sqrt[1 - e^2]) + 
     72*e^8*(-3620837 + 2850774*Sqrt[1 - e^2]) + 8*e^2*(7485372 + 4658723*Sqrt[1 - e^2]) + 18*e^4*(-5376996 + 10523053*Sqrt[1 - e^2]) - 9*e^6*(-28648688 + 14890483*Sqrt[1 - e^2])))/(2048*M), 0, 
  ((1 - e^2)^(3/2)*(-1 + e^2)*(-723052448 + 40310784*e^16 + 948225968*Sqrt[1 - e^2] + e^4*(2556492808 - 62763234*Sqrt[1 - e^2]) + 1119744*e^14*(-677 + 240*Sqrt[1 - e^2]) - 
     93312*e^12*(-24397 + 13130*Sqrt[1 - e^2]) + 1944*e^10*(-2415509 + 1627432*Sqrt[1 - e^2]) - 1782*e^8*(-2964452 + 1819937*Sqrt[1 - e^2]) + 8*e^2*(-32653816 + 247616319*Sqrt[1 - e^2]) + 
     e^6*(-3516564168 + 4310921821*Sqrt[1 - e^2])))/(2048*M), 0, 
  (3*(1 - e^2)^(3/2)*(-1 + e^2)*(644972544*e^18 + 35831808*e^16*(-436 + 135*Sqrt[1 - e^2]) - 1492992*e^14*(-37556 + 19615*Sqrt[1 - e^2]) + 6912*e^12*(-19889153 + 11998317*Sqrt[1 - e^2]) + 
     128*(-222849182 + 270698555*Sqrt[1 - e^2]) - 432*e^10*(-415749833 + 272320715*Sqrt[1 - e^2]) + 128*e^2*(102638677 + 355584591*Sqrt[1 - e^2]) - 16*e^6*(-7689391103 + 2771736127*Sqrt[1 - e^2]) + 
     32*e^4*(-505092505 + 2983044826*Sqrt[1 - e^2]) + e^8*(-170408599680 + 157134696913*Sqrt[1 - e^2])))/(32768*M), 0, 
  (3*(1 - e^2)^(3/2)*(-1 + e^2)*(1934917632*e^20 + e^8*(995631740080 - 576817443313*Sqrt[1 - e^2]) + 644972544*e^18*(-91 + 25*Sqrt[1 - e^2]) - 44789760*e^16*(-5619 + 2800*Sqrt[1 - e^2]) + 
     21888*(-8062286 + 9512267*Sqrt[1 - e^2]) + 20736*e^14*(-34140143 + 19014030*Sqrt[1 - e^2]) + 384*e^2*(-126239431 + 1070057373*Sqrt[1 - e^2]) - 
     432*e^12*(-2504739601 + 1653174204*Sqrt[1 - e^2]) + 96*e^4*(3739748803 + 2542366004*Sqrt[1 - e^2]) + 16*e^6*(-24339098419 + 48006466967*Sqrt[1 - e^2]) + 
     14*e^10*(-92295178840 + 71620233693*Sqrt[1 - e^2])))/(32768*M), 0, 
  ((1 - e^2)^(3/2)*(-1 + e^2)*(139314069504*e^22 + 3869835264*e^20*(-1337 + 330*Sqrt[1 - e^2]) - 107495424*e^18*(-245522 + 115605*Sqrt[1 - e^2]) + 
     2239488*e^16*(-37258207 + 19544222*Sqrt[1 - e^2]) - 10368*e^14*(-14344902106 + 9252992109*Sqrt[1 - e^2]) + 256*(-111097148738 + 126717794051*Sqrt[1 - e^2]) + 
     64*e^6*(1288817224683 + 128189776474*Sqrt[1 - e^2]) + 128*e^2*(-27014376304 + 456895307067*Sqrt[1 - e^2]) + 64*e^4*(158297691097 + 1353722614512*Sqrt[1 - e^2]) + 
     72*e^12*(-2903891547841 + 2026639878717*Sqrt[1 - e^2]) + 6*e^8*(-19864347389548 + 23025099053487*Sqrt[1 - e^2]) - 3*e^10*(-60850808495856 + 41870736919477*Sqrt[1 - e^2])))/(262144*M), 0, 
  (3*(1 - e^2)^(3/2)*(-1 + e^2)*(139314069504*e^24 + e^12*(439055322628952 - 317019543981702*Sqrt[1 - e^2]) + e^8*(232321552137320 - 81483534903714*Sqrt[1 - e^2]) + 
     3869835264*e^22*(-1607 + 360*Sqrt[1 - e^2]) - 644972544*e^20*(-58129 + 25735*Sqrt[1 - e^2]) + 2239488*e^18*(-58797637 + 29413984*Sqrt[1 - e^2]) - 
     20736*e^16*(-13080599803 + 8123076963*Sqrt[1 - e^2]) + 5376*(-11271998798 + 12588023861*Sqrt[1 - e^2]) + 384*e^6*(-66632991587 + 582717367429*Sqrt[1 - e^2]) + 
     192*e^4*(448411839661 + 756687089188*Sqrt[1 - e^2]) + 128*e^2*(-242521646300 + 1139147445347*Sqrt[1 - e^2]) + 216*e^14*(-2005303988481 + 1308261200068*Sqrt[1 - e^2]) + 
     e^10*(-375873971015464 + 331415663896945*Sqrt[1 - e^2])))/(262144*M), 0, 
  (3*(1 - e^2)^(3/2)*(-1 + e^2)*(3343537668096*e^26 + 557256278016*e^24*(-317 + 65*Sqrt[1 - e^2]) - 2579890176*e^22*(-484201 + 201165*Sqrt[1 - e^2]) + 
     322486272*e^20*(-15017042 + 7216801*Sqrt[1 - e^2]) - 746496*e^18*(-15268244001 + 9071720021*Sqrt[1 - e^2]) + 3072*(-1030922158726 + 1129624038451*Sqrt[1 - e^2]) + 
     1728*e^16*(-11738740352926 + 7314441674583*Sqrt[1 - e^2]) + 1024*e^2*(-1879300314679 + 7524956134402*Sqrt[1 - e^2]) + 256*e^6*(27277973354746 + 23562768301293*Sqrt[1 - e^2]) + 
     128*e^4*(17948054548732 + 81659471436201*Sqrt[1 - e^2]) + 72*e^8*(-95669039597788 + 181324468356669*Sqrt[1 - e^2]) - 48*e^10*(-327141991157965 + 205926522572403*Sqrt[1 - e^2]) - 
     24*e^14*(-995283091063442 + 713907110192679*Sqrt[1 - e^2]) + e^12*(-24166414486614976 + 18474650465641735*Sqrt[1 - e^2])))/(2097152*M), 0, 
  ((1 - e^2)^(3/2)*(-1 + e^2)*(30091839012864*e^28 + e^12*(389825413173226320 - 290065432313457013*Sqrt[1 - e^2]) + 1671768834048*e^26*(-1111 + 210*Sqrt[1 - e^2]) - 
     23219011584*e^24*(-658303 + 256620*Sqrt[1 - e^2]) + 644972544*e^22*(-101056939 + 46837641*Sqrt[1 - e^2]) - 6718464*e^20*(-25787796499 + 14638476840*Sqrt[1 - e^2]) + 
     31104*e^18*(-10933125534425 + 6579518020137*Sqrt[1 - e^2]) + 3072*(-19916406536158 + 21515376987703*Sqrt[1 - e^2]) + 3072*e^2*(-17818130422785 + 53227967430062*Sqrt[1 - e^2]) + 
     1024*e^6*(45653010253531 + 229997735429659*Sqrt[1 - e^2]) + 384*e^4*(171808081280916 + 526061871266159*Sqrt[1 - e^2]) - 432*e^16*(-1066048883738395 + 742319133924246*Sqrt[1 - e^2]) + 
     8*e^8*(20050638979102100 + 4811999199105057*Sqrt[1 - e^2]) + 16*e^10*(-16392719199480197 + 17791655019927261*Sqrt[1 - e^2]) + 6*e^14*(-87651265873236040 + 62156718116564049*Sqrt[1 - e^2])))/
   (2097152*M), 0, (3*(1 - e^2)^(3/2)*(-1 + e^2)*(962938848411648*e^30 + 26748301344768*e^28*(-2567 + 450*Sqrt[1 - e^2]) - 11145125560320*e^26*(-58547 + 21435*Sqrt[1 - e^2]) + 
     15479341056*e^24*(-197323337 + 88362630*Sqrt[1 - e^2]) - 71663616*e^22*(-126602631788 + 68728878297*Sqrt[1 - e^2]) + 1492992*e^20*(-13074576986325 + 7639183061899*Sqrt[1 - e^2]) + 
     30720*(-137560531785578 + 146834560404539*Sqrt[1 - e^2]) - 20736*e^18*(-1446563081225116 + 969890356101357*Sqrt[1 - e^2]) + 1024*e^2*(-4502700343877468 + 11618408616727655*Sqrt[1 - e^2]) + 
     256*e^4*(11925808615995848 + 65164413138108127*Sqrt[1 - e^2]) + 128*e^6*(68912445507090848 + 111928764940995787*Sqrt[1 - e^2]) + 
     144*e^16*(-263090241054888841 + 178848901547753142*Sqrt[1 - e^2]) - 24*e^10*(-581612831632653880 + 208112972503763259*Sqrt[1 - e^2]) + 
     16*e^8*(-134276370998410404 + 1061984856438543721*Sqrt[1 - e^2]) - 9*e^14*(-3633478610991630304 + 2787740511485792763*Sqrt[1 - e^2]) + 
     e^12*(-26706619436336644304 + 22417357479923173402*Sqrt[1 - e^2])))/(67108864*M), 0, 
  (3*(1 - e^2)^(3/2)*(-1 + e^2)*(2888816545234944*e^32 + e^12*(124192026659038438256 - 86115545343183681958*Sqrt[1 - e^2]) + 240734712102912*e^30*(-979 + 160*Sqrt[1 - e^2]) - 
     28977326456832*e^28*(-88589 + 30510*Sqrt[1 - e^2]) + 46438023168*e^26*(-283691707 + 122818632*Sqrt[1 - e^2]) - 3869835264*e^24*(-11195543782 + 5824674133*Sqrt[1 - e^2]) + 
     1492992*e^22*(-68157430606261 + 38744676199272*Sqrt[1 - e^2]) - 124416*e^20*(-1407448212217678 + 907402624276191*Sqrt[1 - e^2]) + 
     22032*e^18*(-10999572036520313 + 7272376021209416*Sqrt[1 - e^2]) + 2048*(-13301649913030334 + 14062665790880369*Sqrt[1 - e^2]) + 
     1024*e^2*(-36132667925123792 + 80740984446517705*Sqrt[1 - e^2]) + 256*e^4*(77003845983759160 + 456120889391635287*Sqrt[1 - e^2]) + 
     168*e^10*(-394359647517641392 + 670562256637135973*Sqrt[1 - e^2]) + 128*e^6*(338642714031246256 + 997306812610244889*Sqrt[1 - e^2]) + 
     16*e^8*(3716764510311495180 + 4333006305943191629*Sqrt[1 - e^2]) - 6*e^16*(-40308987874638545144 + 30356521355424350787*Sqrt[1 - e^2]) + 
     3*e^14*(-73809462696761367632 + 55248096540218831149*Sqrt[1 - e^2])))/(67108864*M), 0, 
  ((1 - e^2)^(3/2)*(-1 + e^2)*(831979165027663872*e^34 + 785758100303904768*e^32*(-98 + 15*Sqrt[1 - e^2]) - 1925877696823296*e^30*(-495678 + 160895*Sqrt[1 - e^2]) + 
     26748301344768*e^28*(-200583281 + 83951847*Sqrt[1 - e^2]) - 185752092672*e^26*(-104349410687 + 52157840937*Sqrt[1 - e^2]) + 429981696*e^24*(-115210854105868 + 63755935519419*Sqrt[1 - e^2]) - 
     5971968*e^22*(-15802341630051502 + 9811502696393247*Sqrt[1 - e^2]) + 1081344*(-15609113758339826 + 16370129636189861*Sqrt[1 - e^2]) + 
     248832*e^20*(-572342309209322467 + 369971947356088995*Sqrt[1 - e^2]) + 98304*e^2*(-275175510385531947 + 554787372222617513*Sqrt[1 - e^2]) + 
     24576*e^4*(289721235590541765 + 3370824993450115339*Sqrt[1 - e^2]) + 4096*e^6*(9407634559202146031 + 20591010167578762583*Sqrt[1 - e^2]) + 
     256*e^10*(144447054503732299715 + 60108757835538635307*Sqrt[1 - e^2]) - 864*e^18*(-187261206200872159399 + 136257247615023950097*Sqrt[1 - e^2]) + 
     512*e^8*(28183327554003471685 + 156662509416234955194*Sqrt[1 - e^2]) + 64*e^12*(-1193842310417009831899 + 1182260569919555324683*Sqrt[1 - e^2]) - 
     32*e^14*(-3312016666549094639041 + 2648387456342948213127*Sqrt[1 - e^2]) + e^16*(-161761203266490404007424 + 115281447634315049992767*Sqrt[1 - e^2])))/(2147483648*M), 0, 
  (3*(1 - e^2)^(3/2)*(-1 + e^2)*(831979165027663872*e^36 + 92442129447518208*e^34*(-938 + 135*Sqrt[1 - e^2]) - 15407021574586368*e^32*(-78889 + 24185*Sqrt[1 - e^2]) + 
     8916100448256*e^30*(-838182653 + 339049974*Sqrt[1 - e^2]) - 557256278016*e^28*(-52882381327 + 25456726172*Sqrt[1 - e^2]) + 7739670528*e^26*(-10558056075842 + 5687508938031*Sqrt[1 - e^2]) - 
     11943936*e^24*(-14291020306149901 + 8566781708915352*Sqrt[1 - e^2]) + 98304*(-368824894666571318 + 384245479559848343*Sqrt[1 - e^2]) + 
     82944*e^22*(-3375157418726710703 + 2135691589376841030*Sqrt[1 - e^2]) + 32768*e^2*(-2076298690256926613 + 3827301093695972321*Sqrt[1 - e^2]) + 
     8192*e^4*(901485574138037907 + 24171008034825512171*Sqrt[1 - e^2]) + 4096*e^6*(19401651664787633863 + 54324523136716111387*Sqrt[1 - e^2]) - 
     2592*e^20*(-137482525652744214055 + 96384082554356061892*Sqrt[1 - e^2]) + 512*e^8*(171637144195607276129 + 327033574628958171456*Sqrt[1 - e^2]) + 
     256*e^10*(-123721344649112966229 + 624680209759001913031*Sqrt[1 - e^2]) - 64*e^12*(-1579415893717357291761 + 646109960570898160351*Sqrt[1 - e^2]) + 
     32*e^14*(-7487025958129521659047 + 5907378026869962250029*Sqrt[1 - e^2]) - 3*e^16*(-99169929463317207380128 + 79806123494952064254791*Sqrt[1 - e^2]) + 
     e^18*(-386206448734816129598752 + 268951411506914623488222*Sqrt[1 - e^2])))/(2147483648*M)}, 3, 43, 2],

		px === "x",
		SeriesData[x, 0, {M^(-1), 0, 3/((-1 + e^2)*M), 0, (3*(-6 + 7*e^2))/(4*(-1 + e^2)^2*M), 0, (-3*(2 - 8*e^4 - 20*Sqrt[1 - e^2] + e^2*(9 + 20*Sqrt[1 - e^2])))/(4*(-1 + e^2)^3*M), 0, 
  (160*e^6*(27 + Sqrt[1 - e^2]) + 120*(-40 + 13*Sqrt[1 - e^2]) - 3*e^4*(4480 + 39*Sqrt[1 - e^2]) - 24*e^2*(-580 + 63*Sqrt[1 - e^2]))/(64*(1 - e^2)^(9/2)*M), 0, 
  -(-224*e^8*(-45 + 2*Sqrt[1 - e^2]) + 168*(74 + 7*Sqrt[1 - e^2]) - 24*e^6*(1725 + 379*Sqrt[1 - e^2]) - 24*e^2*(1921 + 657*Sqrt[1 - e^2]) + 3*e^4*(21664 + 8137*Sqrt[1 - e^2]))/
   (64*(1 - e^2)^(11/2)*M), 0, (-3*(1728*e^10*(-10 + Sqrt[1 - e^2]) + 48*(748 + 953*Sqrt[1 - e^2]) + 16*e^8*(6590 + 5517*Sqrt[1 - e^2]) - 8*e^2*(20176 + 35585*Sqrt[1 - e^2]) - 
     5*e^6*(49728 + 73501*Sqrt[1 - e^2]) + e^4*(285984 + 516826*Sqrt[1 - e^2])))/(256*(1 - e^2)^(13/2)*M), 0, 
  -(-22528*e^12*Sqrt[1 - e^2] - 4224*e^10*(635 + 666*Sqrt[1 - e^2]) + 1584*(502 + 1685*Sqrt[1 - e^2]) - 72*e^2*(105652 + 216127*Sqrt[1 - e^2]) + 24*e^8*(544655 + 640986*Sqrt[1 - e^2]) + 
     18*e^4*(1140868 + 1817795*Sqrt[1 - e^2]) - 3*e^6*(8037776 + 10809159*Sqrt[1 - e^2]))/(768*(1 - e^2)^(15/2)*M), 0, 
  (-1171456*e^14 + 419328*e^12*(-909 + 80*Sqrt[1 - e^2]) + 134784*(-8963 + 944*Sqrt[1 - e^2]) + 4992*e^10*(583783 + 194080*Sqrt[1 - e^2]) + 11520*e^2*(532033 + 231692*Sqrt[1 - e^2]) - 
    288*e^4*(45138181 + 31666136*Sqrt[1 - e^2]) + 96*e^6*(152416985 + 112782196*Sqrt[1 - e^2]) - 3*e^8*(3021660917 + 1835321728*Sqrt[1 - e^2]))/(49152*(-1 + e^2)^8*M), 0, 
  (3*(640*(1278569 - 340346*Sqrt[1 - e^2]) + 165888*e^14*(-259 + 60*Sqrt[1 - e^2]) + 6912*e^12*(122299 + 66760*Sqrt[1 - e^2]) - 128*e^2*(35022658 + 11640877*Sqrt[1 - e^2]) - 
     80*e^10*(55433995 + 41166408*Sqrt[1 - e^2]) + 32*e^4*(337331523 + 230038523*Sqrt[1 - e^2]) - 16*e^6*(894306926 + 734371199*Sqrt[1 - e^2]) + e^8*(10803603697 + 8918474384*Sqrt[1 - e^2])))/
   (16384*(-1 + e^2)^9*M), 0, (52084736*e^18 + 41361408*e^16*(439 + 120*Sqrt[1 - e^2]) - 117504*(8150299 + 293708*Sqrt[1 - e^2]) + 1292544*e^14*(719389 + 329920*Sqrt[1 - e^2]) + 
    148608*e^2*(48784521 + 28707872*Sqrt[1 - e^2]) - 34272*e^12*(224468441 + 115961424*Sqrt[1 - e^2]) - 5184*e^4*(4586764161 + 3472299118*Sqrt[1 - e^2]) + 
    864*e^6*(48772267281 + 36594119116*Sqrt[1 - e^2]) - 54*e^8*(794449224943 + 540744170016*Sqrt[1 - e^2]) + 27*e^10*(925838374699 + 551643990848*Sqrt[1 - e^2]))/(1769472*(-1 + e^2)^10*M), 0, 
  (64749568*e^20 + 24281088*e^18*(4757 + 180*Sqrt[1 - e^2]) + 91054080*e^16*(40451 + 8224*Sqrt[1 - e^2]) + 3545856*(92659 + 1357322*Sqrt[1 - e^2]) - 
    87552*e^14*(441576227 + 135319704*Sqrt[1 - e^2]) - 3456*e^2*(6645178937 + 12969918052*Sqrt[1 - e^2]) - 15552*e^6*(20602184641 + 18601278214*Sqrt[1 - e^2]) + 
    1728*e^4*(75534929250 + 91533420791*Sqrt[1 - e^2]) + 72*e^12*(2167795865207 + 921920110672*Sqrt[1 - e^2]) + 54*e^8*(7956548236345 + 5631983859052*Sqrt[1 - e^2]) - 
    27*e^10*(12531999299463 + 6973428477592*Sqrt[1 - e^2]))/(1769472*(-1 + e^2)^11*M), 0, 
  (3*(35831808*e^20*(-1760 + 1553*Sqrt[1 - e^2]) + 663552*e^18*(10398215 + 3632049*Sqrt[1 - e^2]) + 21504*(-403195576 + 155248825*Sqrt[1 - e^2]) - 
     6912*e^16*(9616900116 + 4457084957*Sqrt[1 - e^2]) + 1024*e^2*(74998631764 + 11774215991*Sqrt[1 - e^2]) + 2304*e^6*(326498440916 + 204379931667*Sqrt[1 - e^2]) - 
     128*e^4*(2454193127760 + 1172055363733*Sqrt[1 - e^2]) + 32*e^14*(9014096182172 + 4874321090205*Sqrt[1 - e^2]) + 8*e^10*(140368938166064 + 91533458211515*Sqrt[1 - e^2]) - 
     8*e^8*(142448679524000 + 95209513922861*Sqrt[1 - e^2]) - e^12*(718421794519040 + 434492590238549*Sqrt[1 - e^2])))/(1048576*(1 - e^2)^(25/2)*M), 0, 
  -(-204996608*e^24*(-1485 + 26*Sqrt[1 - e^2]) + 153747456*e^22*(879065 + 135699*Sqrt[1 - e^2]) - 681053184*(-16296062 + 6198683*Sqrt[1 - e^2]) + 
     105701376*e^20*(49944643 + 13028346*Sqrt[1 - e^2]) - 1201152*e^18*(59488717205 + 20337907771*Sqrt[1 - e^2]) - 248832*e^2*(417394485377 + 108432578999*Sqrt[1 - e^2]) + 
     198720*e^16*(1932353709959 + 829236430493*Sqrt[1 - e^2]) + 93312*e^4*(5131068992504 + 3119712258821*Sqrt[1 - e^2]) - 6912*e^6*(192695950863772 + 140424081532001*Sqrt[1 - e^2]) - 
     216*e^14*(5394326457146022 + 2789668621337717*Sqrt[1 - e^2]) + 216*e^8*(11028542504347252 + 8051309225275269*Sqrt[1 - e^2]) - 216*e^10*(13059185719048382 + 8847825798440469*Sqrt[1 - e^2]) + 
     27*e^12*(82656413252714048 + 49762025013187433*Sqrt[1 - e^2]))/(84934656*(1 - e^2)^(27/2)*M), 0, 
  (4099932160*e^28*(-35 + Sqrt[1 - e^2]) - 524288*e^26*(-68356415 + 5014544*Sqrt[1 - e^2]) + 32768*e^24*(-260005022719 + 15064416382*Sqrt[1 - e^2]) - 
    829440*(47562682697 + 130100700808*Sqrt[1 - e^2]) + 24576*e^22*(17101868859501 + 3495340545212*Sqrt[1 - e^2]) + 165888*e^2*(15573789749521 + 23043673612747*Sqrt[1 - e^2]) - 
    8064*e^20*(615381219101485 + 199591996054946*Sqrt[1 - e^2]) - 20736*e^4*(1167711419634713 + 1353012983723465*Sqrt[1 - e^2]) + 2016*e^18*(14259196115295691 + 6032335202688290*Sqrt[1 - e^2]) + 
    3456*e^6*(28765425711015392 + 28400629892363035*Sqrt[1 - e^2]) - 432*e^8*(536579416321555581 + 466262362229775941*Sqrt[1 - e^2]) + 
    189*e^14*(1179007695434153428 + 707070218281055093*Sqrt[1 - e^2]) - 54*e^16*(1840864483873066999 + 945143713984861804*Sqrt[1 - e^2]) + 
    216*e^10*(1583324518886165126 + 1223082676890294679*Sqrt[1 - e^2]) - 54*e^12*(6214095904363407823 + 4255721235379705423*Sqrt[1 - e^2]))/(339738624*(1 - e^2)^(29/2)*M), 0, 
  -(8766095360*e^30*(-75 + 2*Sqrt[1 - e^2]) - 117964800*e^28*(-1471195 + 107196*Sqrt[1 - e^2]) + 983040*e^26*(-20989491693 + 2136782656*Sqrt[1 - e^2]) + 
     2488320*(-1286677647349 + 2246059918276*Sqrt[1 - e^2]) - 2488320*e^2*(2578304977866 + 23296494072061*Sqrt[1 - e^2]) + 24576*e^24*(141555650630881 + 26344026631144*Sqrt[1 - e^2]) + 
     311040*e^4*(721337738859125 + 1110875877198331*Sqrt[1 - e^2]) - 4608*e^22*(11140408140526527 + 3300040413471208*Sqrt[1 - e^2]) - 
     10368*e^6*(117889382053793458 + 124902285740237891*Sqrt[1 - e^2]) + 144*e^20*(2405467332015610253 + 943001192993977264*Sqrt[1 - e^2]) + 
     1296*e^8*(2687372361219772565 + 2402892553552747379*Sqrt[1 - e^2]) - 648*e^10*(9606268208852620784 + 7648590092449672767*Sqrt[1 - e^2]) - 
     54*e^18*(25434202660706047421 + 12201132066814534960*Sqrt[1 - e^2]) + 54*e^16*(65433648255411486635 + 36753612049234161396*Sqrt[1 - e^2]) + 
     54*e^12*(138278262524564977841 + 99034503254203967689*Sqrt[1 - e^2]) - 27*e^14*(228738895888534268318 + 146331810475803364743*Sqrt[1 - e^2]))/(1019215872*(1 - e^2)^(31/2)*M)}, 3, 35, 2],

		True,
		Print["Expansion parameter is either \"p\" or \"x\""];
		Aborting[syms];
	]
]


def@
specificEnergySchwPN[syms_Association,px_String]:=
Module[{M,p,e,x},
	
	M=BlackHoleMassSymbol[syms];
	e=OrbitalEccentricitySymbol[syms];
	p=SemiLatusRectumSymbol[syms];
	x=XPNSymbol[syms];
	
	Which[px==="p",
		SeriesData[p, Infinity, {1, (-1 + e^2)/2, (3*(-1 + e^2)^2)/8, ((-1 + e^2)^2*(27 + 5*e^2))/16, ((-1 + e^2)^2*(675 + 314*e^2 + 35*e^4))/128, ((-1 + e^2)^2*(3969 + 3389*e^2 + 771*e^4 + 63*e^6))/256, 
  ((-1 + e^2)^2*(45927 + 59620*e^2 + 21738*e^4 + 3556*e^6 + 231*e^8))/1024, ((-1 + e^2)^2*(264627 + 466657*e^2 + 247678*e^4 + 61250*e^6 + 7935*e^8 + 429*e^10))/2048, 
  ((-1 + e^2)^2*(12196899 + 27202094*e^2 + 19642125*e^4 + 6634308*e^6 + 1288205*e^8 + 138798*e^10 + 6435*e^12))/32768, 
  ((-1 + e^2)^2*(70366725 + 189182813*e^2 + 176252265*e^4 + 77422545*e^6 + 20096175*e^8 + 3238935*e^10 + 299299*e^12 + 12155*e^14))/65536, 
  ((-1 + e^2)^2*(813439341 + 2549260696*e^2 + 2938262764*e^4 + 1625338152*e^6 + 532317390*e^8 + 114210600*e^10 + 15781612*e^12 + 1277848*e^14 + 46189*e^16))/262144, 
  ((-1 + e^2)^2*(4710988269 + 16795906315*e^2 + 23164795476*e^4 + 15719286972*e^6 + 6290621862*e^8 + 1687452570*e^10 + 310080708*e^12 + 37549356*e^14 + 2707029*e^16 + 88179*e^18))/524288, 
  ((-1 + e^2)^2*(109344517191 + 435520015546*e^2 + 699929571579*e^4 + 569431223480*e^6 + 272839093310*e^8 + 88127830812*e^10 + 20203612478*e^12 + 3253081272*e^14 + 350833275*e^16 + 22800570*e^18 + 
     676039*e^20))/4194304, ((-1 + e^2)^2*(635640105429 + 2789464356889*e^2 + 5113970923075*e^4 + 4887285114103*e^6 + 2761402105490*e^8 + 1049037614618*e^10 + 288336398182*e^12 + 57890346990*e^14 + 
     8302585929*e^16 + 807416445*e^18 + 47777191*e^20 + 1300075*e^22))/8388608, 
  ((-1 + e^2)^2*(7402640979375 + 35407532181836*e^2 + 72792915369630*e^4 + 80257737731100*e^6 + 52780205753825*e^8 + 23223429476760*e^10 + 7452788416164*e^12 + 1791946635160*e^14 + 
     320452157025*e^16 + 41452220700*e^18 + 3671350430*e^20 + 199424076*e^22 + 5014575*e^24))/33554432, 
  ((-1 + e^2)^2*(43172202191715 + 223138141004249*e^2 + 507336999398826*e^4 + 635201473877798*e^6 + 480379396584705*e^8 + 242177745666411*e^10 + 89114321813148*e^12 + 24962509953252*e^14 + 
     5344570476981*e^16 + 862170564255*e^18 + 101595666458*e^20 + 8261994390*e^22 + 414798215*e^24 + 9694845*e^26))/67108864, 
  ((-1 + e^2)^2*(4034202449248035 + 22374829449992726*e^2 + 55628304365316977*e^4 + 78008618483957308*e^6 + 67076366841502683*e^8 + 38415427721812554*e^10 + 16006800184610193*e^12 + 
     5121919406828232*e^14 + 1276816526395281*e^16 + 246611528376906*e^18 + 36247488075227*e^20 + 3922731212348*e^22 + 294909169009*e^24 + 13764674070*e^26 + 300540195*e^28))/2147483648, 
  ((-1 + e^2)^2*(23591492213351085 + 139603621241645789*e^2 + 376019585651293941*e^4 + 583581938691944325*e^6 + 564463234198782065*e^8 + 364500981044381601*e^10 + 170466405045406281*e^12 + 
     61422090053824089*e^14 + 17470469581777575*e^16 + 3928386437763255*e^18 + 691428351810911*e^20 + 93353133692559*e^22 + 9341497736571*e^24 + 653009700875*e^26 + 28477181475*e^28 + 
     583401555*e^30))/4294967296, ((-1 + e^2)^2*(276248763659562705 + 1735788550584528112*e^2 + 5026353841698126840*e^4 + 8544950507381006544*e^6 + 9204789693379055580*e^8 + 
     6654705157816505328*e^10 + 3470125918911253448*e^12 + 1393247104653186640*e^14 + 445344319376228646*e^16 + 114222771395310160*e^18 + 23406881243469768*e^20 + 3785041837969392*e^22 + 
     472620062933980*e^24 + 43982481673424*e^26 + 2873093470200*e^28 + 117576038640*e^30 + 2268783825*e^32))/17179869184, 
  ((-1 + e^2)^2*(1619161413794087625 + 10759088081214464263*e^2 + 33281154023920747720*e^4 + 61427190954324662040*e^6 + 73018340861508182700*e^8 + 58695554629681047540*e^10 + 
     33945366606195642136*e^12 + 15070159606275290440*e^14 + 5350976077201033510*e^16 + 1541363566129991450*e^18 + 360275381024965560*e^20 + 67839246160037352*e^22 + 10147166061419660*e^24 + 
     1178530474653140*e^26 + 102504331265000*e^28 + 6284375417400*e^30 + 242261760345*e^32 + 4418157975*e^34))/34359738368, 
  ((-1 + e^2)^2*(37999405294013245005 + 266093780703232375318*e^2 + 874574850806316452997*e^4 + 1739179592965264000656*e^6 + 2262155652939643151460*e^8 + 2008269161230552353000*e^10 + 
     1282049755739071462836*e^12 + 625771593113546321904*e^14 + 244630450866121865622*e^16 + 78190520800295511300*e^18 + 20522650119880554646*e^20 + 4408178486958039024*e^22 + 
     767911250515121076*e^24 + 106856522864777448*e^26 + 11601044004124260*e^28 + 947123738850960*e^30 + 54705481000965*e^32 + 1993329586710*e^34 + 34461632205*e^36))/274877906944}, 0, 21, 1],

		px === "x",
	SeriesData[x, 0, {1, -1/2, (3 + 5*e^2)/(8 - 8*e^2), (-40 + 67*Sqrt[1 - e^2] + e^2*(80 - 58*Sqrt[1 - e^2]) - 5*e^4*(8 + Sqrt[1 - e^2]))/(16*(1 - e^2)^(5/2)), 
  -(480 - 2505*Sqrt[1 - e^2] + e^4*(7200 - 3723*Sqrt[1 - e^2]) + 5*e^6*(-672 + 37*Sqrt[1 - e^2]) + e^2*(-4320 + 6171*Sqrt[1 - e^2]))/(384*(1 - e^2)^(7/2)), 
  (37776 - 25869*Sqrt[1 - e^2] + e^4*(76896 - 40674*Sqrt[1 - e^2]) + 12*e^6*(-340 + 753*Sqrt[1 - e^2]) + e^8*(-10320 + 931*Sqrt[1 - e^2]) + 12*e^2*(-8356 + 4681*Sqrt[1 - e^2]))/
   (768*(1 - e^2)^(9/2)), -(e^6*(720992 - 513714*Sqrt[1 - e^2]) + e^2*(753024 - 507397*Sqrt[1 - e^2]) + e^10*(-3040 + 879*Sqrt[1 - e^2]) + 21*(-9376 + 7189*Sqrt[1 - e^2]) + 
     6*e^4*(-182960 + 121227*Sqrt[1 - e^2]) + e^8*(-176320 + 140941*Sqrt[1 - e^2]))/(1024*(1 - e^2)^(11/2)), 
  (e^8*(5130120 - 20861175*Sqrt[1 - e^2]) + e^12*(573000 - 20405*Sqrt[1 - e^2]) + 348*e^6*(-4136 + 38355*Sqrt[1 - e^2]) + 9*(182632 + 81995*Sqrt[1 - e^2]) + 
    30*e^10*(-110096 + 243511*Sqrt[1 - e^2]) - 18*e^2*(115728 + 497663*Sqrt[1 - e^2]) + 9*e^4*(-57944 + 943257*Sqrt[1 - e^2]))/(18432*(1 - e^2)^(13/2)), 
  -(e^8*(8702790336 - 2898707313*Sqrt[1 - e^2]) + 81*(3631040 - 4986251*Sqrt[1 - e^2]) + e^14*(18943680 - 992485*Sqrt[1 - e^2]) + 3*e^12*(185455680 + 51161537*Sqrt[1 - e^2]) + 
     3*e^10*(-1188728000 + 57384893*Sqrt[1 - e^2]) - 99*e^4*(-74173504 + 67174589*Sqrt[1 - e^2]) + 9*e^2*(-272790080 + 321966453*Sqrt[1 - e^2]) + 3*e^6*(-3631365824 + 2245416347*Sqrt[1 - e^2]))/
   (294912*(1 - e^2)^(15/2)), (306977189 - 176315*e^16 - 236610464*Sqrt[1 - e^2] + e^8*(2887393842 - 5625811648*Sqrt[1 - e^2]) + e^12*(308474020 - 636398400*Sqrt[1 - e^2]) - 
    40*e^14*(259823 + 48900*Sqrt[1 - e^2]) + 8*e^2*(-183067107 + 111596240*Sqrt[1 - e^2]) + 8*e^10*(-171017641 + 379886400*Sqrt[1 - e^2]) - 4*e^4*(-754984057 + 638873952*Sqrt[1 - e^2]) + 
    8*e^6*(-461200193 + 640551400*Sqrt[1 - e^2]))/(65536*(-1 + e^2)^8), (-944447430717 + 66619685*e^18 + 878558844096*Sqrt[1 - e^2] + 45*e^16*(-951729529 + 56348864*Sqrt[1 - e^2]) - 
    1350*e^8*(-15343837267 + 1935721344*Sqrt[1 - e^2]) - 180*e^14*(2748169115 + 2681832464*Sqrt[1 - e^2]) - 324*e^4*(-10875507319 + 9350472640*Sqrt[1 - e^2]) - 
    54*e^10*(258432279091 + 32010142912*Sqrt[1 - e^2]) - 27*e^2*(-69321964695 + 34730105408*Sqrt[1 - e^2]) + 36*e^12*(127672697683 + 52412526048*Sqrt[1 - e^2]) + 
    108*e^6*(-141250450137 + 55776128480*Sqrt[1 - e^2]))/(21233664*(-1 + e^2)^9), (447005533*e^20 + e^4*(31171021139313 - 32955358570272*Sqrt[1 - e^2]) + 
    6*e^18*(-24749909117 + 2003694420*Sqrt[1 - e^2]) + 936*e^14*(73744004769 + 3407317420*Sqrt[1 - e^2]) - 648*e^6*(-190995252837 + 8767119664*Sqrt[1 - e^2]) - 
    18*e^12*(11916647762849 + 23370781600*Sqrt[1 - e^2]) - 243*(-58854518839 + 57284189416*Sqrt[1 - e^2]) - 9*e^16*(994046393503 + 111840793880*Sqrt[1 - e^2]) - 
    108*e^10*(-3273177595555 + 125289630736*Sqrt[1 - e^2]) + 54*e^8*(-5901969308819 + 462903954336*Sqrt[1 - e^2]) + 54*e^2*(-917493670517 + 728091604964*Sqrt[1 - e^2]))/(42467328*(-1 + e^2)^10), 
  (37656535*e^22 + e^6*(7000782391091 - 94414817821824*Sqrt[1 - e^2]) + e^2*(12967095244973 - 10573087236480*Sqrt[1 - e^2]) + 37*e^20*(-46597849 + 18589600*Sqrt[1 - e^2]) + 
    5*e^18*(-621756158707 + 20828533120*Sqrt[1 - e^2]) + 45*e^16*(516956720209 + 46727478112*Sqrt[1 - e^2]) + 3*(-931879450637 + 895431278240*Sqrt[1 - e^2]) + 
    22*e^8*(2595755967311 + 7058499768128*Sqrt[1 - e^2]) - 2*e^14*(37144916471285 + 10907740447168*Sqrt[1 - e^2]) + e^4*(-23762193718705 + 34971617249056*Sqrt[1 - e^2]) + 
    2*e^12*(64411815766775 + 39608812104384*Sqrt[1 - e^2]) - 2*e^10*(62592832510311 + 73783355617280*Sqrt[1 - e^2]))/(4194304*(-1 + e^2)^11), 
  (-85*e^24*(-11235054864 + 234641837*Sqrt[1 - e^2]) + 12*e^22*(-23140017288540 + 1702264088639*Sqrt[1 - e^2]) - 243*(-63540106505296 + 62904466399867*Sqrt[1 - e^2]) + 
    972*e^2*(-96310629413964 + 86675606963735*Sqrt[1 - e^2]) - 6*e^20*(1528265380125840 + 497431957780501*Sqrt[1 - e^2]) + 216*e^14*(5197925785612508 + 1122420366518769*Sqrt[1 - e^2]) - 
    162*e^4*(-1176073642570096 + 1278354327750895*Sqrt[1 - e^2]) + 12*e^18*(8606879540235596 + 2069415862273027*Sqrt[1 - e^2]) + 216*e^10*(6983350018696076 + 2109434796014697*Sqrt[1 - e^2]) + 
    108*e^6*(-33324187404740 + 3025530318834179*Sqrt[1 - e^2]) - 108*e^12*(15499524820467920 + 3648604760566733*Sqrt[1 - e^2]) - 27*e^16*(16943085304665136 + 3676184879611489*Sqrt[1 - e^2]) - 
    27*e^8*(26012261437610768 + 15380749336287825*Sqrt[1 - e^2]))/(2038431744*(1 - e^2)^(25/2)), 
  (81*(7191158374017184 - 7168950451079059*Sqrt[1 - e^2]) + 95*e^26*(-103952885088 + 3001411445*Sqrt[1 - e^2]) - 3*e^24*(-537896502449600 + 53790468197661*Sqrt[1 - e^2]) + 
    6*e^22*(28603359834445680 + 1562459557372411*Sqrt[1 - e^2]) - 78*e^20*(24042177819348272 + 2975319578838343*Sqrt[1 - e^2]) + 81*e^2*(-44613175818609408 + 40758099702823087*Sqrt[1 - e^2]) - 
    162*e^4*(-56914045765372880 + 56915319666175433*Sqrt[1 - e^2]) + 162*e^6*(-63712058839758032 + 113925568618960665*Sqrt[1 - e^2]) + 
    108*e^14*(386434287719084656 + 199541361526606453*Sqrt[1 - e^2]) - 27*e^16*(897380985166732384 + 315351265915467511*Sqrt[1 - e^2]) - 
    108*e^12*(421360405654847696 + 323284056089602941*Sqrt[1 - e^2]) + 3*e^18*(2955777653601097792 + 661393888846789547*Sqrt[1 - e^2]) - 
    27*e^8*(113571353554724096 + 1113987649863904555*Sqrt[1 - e^2]) + 27*e^10*(1038330679619260384 + 1416250727393837247*Sqrt[1 - e^2]))/(8153726976*(1 - e^2)^(27/2)), 
  (1215*(2565511477968488 - 2556877037530145*Sqrt[1 - e^2]) + e^28*(15131906074040 - 572516657095*Sqrt[1 - e^2]) + 2*e^26*(237226296651520 + 69442088185911*Sqrt[1 - e^2]) - 
    486*e^2*(79594704919532512 + 33447876639027875*Sqrt[1 - e^2]) + e^24*(-798116119131877960 + 33447950433398119*Sqrt[1 - e^2]) + 12*e^22*(485265748757387344 + 66877222394796603*Sqrt[1 - e^2]) - 
    2808*e^14*(642889800732146672 + 291304213176508877*Sqrt[1 - e^2]) - 3*e^20*(895331188837085144 + 1618249609543041909*Sqrt[1 - e^2]) - 
    6*e^18*(20938720775174113696 + 2732985945876416393*Sqrt[1 - e^2]) + 81*e^4*(3317818962355115096 + 2799265708330699519*Sqrt[1 - e^2]) + 
    27*e^16*(24876392020794496648 + 7945460636014175277*Sqrt[1 - e^2]) - 108*e^6*(9436007791125410128 + 8094966910215990557*Sqrt[1 - e^2]) - 
    270*e^10*(11995510870099688000 + 8139872402932623581*Sqrt[1 - e^2]) + 189*e^12*(15853317946662093784 + 9039188661112952627*Sqrt[1 - e^2]) + 
    27*e^8*(84640259640934715912 + 65930294682250591371*Sqrt[1 - e^2]))/(16307453952*(1 - e^2)^(29/2)), 
  -(617609486715367616640 - 618589797910534889145*Sqrt[1 - e^2] + e^22*(4651602551968106139264 - 150309994691695001811*Sqrt[1 - e^2]) + e^28*(336504503451057280 - 19598584748382999*Sqrt[1 - e^2]) + 
     e^30*(-671878799149440 + 8576909672313*Sqrt[1 - e^2]) + e^26*(-32956560310932934016 + 4042019277177258017*Sqrt[1 - e^2]) + e^24*(-41882943095399805824 + 97926837965335095793*Sqrt[1 - e^2]) + 
     81*e^2*(-17990705767143398784 + 109005335467339914023*Sqrt[1 - e^2]) - 81*e^4*(288234606495738940288 + 729728508245424492305*Sqrt[1 - e^2]) - 
     9*e^20*(4529878617739686019968 + 917637861135133988363*Sqrt[1 - e^2]) + 27*e^6*(6075458488400474109824 + 8277988223758227545821*Sqrt[1 - e^2]) - 
     27*e^16*(18351618563320888252544 + 9092530896748425473489*Sqrt[1 - e^2]) - 27*e^8*(18707168276353906914944 + 19339911903981338830103*Sqrt[1 - e^2]) + 
     27*e^14*(33309761631989855142272 + 20438230868954693311633*Sqrt[1 - e^2]) + 3*e^18*(60288759355550105891200 + 21924175753270768490807*Sqrt[1 - e^2]) + 
     27*e^10*(34285985613208268700544 + 29437785116610318795033*Sqrt[1 - e^2]) - 27*e^12*(41074931063612746005632 + 29964812601653961396479*Sqrt[1 - e^2]))/(521838526464*(1 - e^2)^(31/2))}, 0, 17, 1],

		True,
		Print["Expansion parameter is either \"p\" or \"x\""];
		Aborting[syms];
	]
]


def@
specificAngularMomentumSchwPN[syms_Association,px_String]:=
Module[{M,p,e,x},
	
	M=BlackHoleMassSymbol[syms];
	e=OrbitalEccentricitySymbol[syms];
	p=SemiLatusRectumSymbol[syms];
	x=XPNSymbol[syms];
	
	Which[px==="p",
		SeriesData[p, Infinity, {Sqrt[M^2], 0, ((3 + e^2)*Sqrt[M^2])/2, 0, (3*(3 + e^2)^2*Sqrt[M^2])/8, 0, (5*(3 + e^2)^3*Sqrt[M^2])/16, 0, (35*(3 + e^2)^4*Sqrt[M^2])/128, 0, 
  (63*(3 + e^2)^5*Sqrt[M^2])/256, 0, (231*(3 + e^2)^6*Sqrt[M^2])/1024, 0, (429*(3 + e^2)^7*Sqrt[M^2])/2048, 0, (6435*(3 + e^2)^8*Sqrt[M^2])/32768, 0, (12155*(3 + e^2)^9*Sqrt[M^2])/65536, 0, 
  (46189*(3 + e^2)^10*Sqrt[M^2])/262144, 0, (88179*(3 + e^2)^11*Sqrt[M^2])/524288, 0, (676039*(3 + e^2)^12*Sqrt[M^2])/4194304, 0, (1300075*(3 + e^2)^13*Sqrt[M^2])/8388608, 0, 
  (5014575*(3 + e^2)^14*Sqrt[M^2])/33554432, 0, (9694845*(3 + e^2)^15*Sqrt[M^2])/67108864, 0, (300540195*(3 + e^2)^16*Sqrt[M^2])/2147483648, 0, (583401555*(3 + e^2)^17*Sqrt[M^2])/4294967296, 0, 
  (2268783825*(3 + e^2)^18*Sqrt[M^2])/17179869184, 0, (4418157975*(3 + e^2)^19*Sqrt[M^2])/34359738368, 0, (34461632205*(3 + e^2)^20*Sqrt[M^2])/274877906944}, -1, 41, 2],

		px === "x",
		
		SeriesData[x, 0, {Sqrt[1 - e^2]*M, 0, (3*(1 + e^2)*M)/(2*Sqrt[1 - e^2]), 0, ((47 - e^4 - 20*Sqrt[1 - e^2] + 20*e^2*Sqrt[1 - e^2])*M)/(8*(1 - e^2)^(3/2)), 0, 
  -((-465 + 31*e^6 + 60*Sqrt[1 - e^2] + e^2*(111 - 360*Sqrt[1 - e^2]) + 3*e^4*(53 + 100*Sqrt[1 - e^2]))*M)/(48*(1 - e^2)^(5/2)), 0, 
  ((3896 - 1061*Sqrt[1 - e^2] + e^8*(-920 + 123*Sqrt[1 - e^2]) + 8*e^6*(505 + 391*Sqrt[1 - e^2]) + 8*e^2*(-699 + 439*Sqrt[1 - e^2]) - 8*e^4*(178 + 1057*Sqrt[1 - e^2]))*M)/(128*(-1 + e^2)^4), 0, 
  ((e^2*(62144 - 28829*Sqrt[1 - e^2]) + e^10*(440 + 91*Sqrt[1 - e^2]) + 9*(-4136 + 2435*Sqrt[1 - e^2]) + 3*e^8*(7440 + 7969*Sqrt[1 - e^2]) - 4*e^6*(14566 + 23215*Sqrt[1 - e^2]) + 
     4*e^4*(2646 + 24737*Sqrt[1 - e^2]))*M)/(256*(-1 + e^2)^5), 0, 
  -((e^12*(-197700 + 10237*Sqrt[1 - e^2]) + 72*e^2*(54568 + 12903*Sqrt[1 - e^2]) - 384*e^6*(85661 + 52794*Sqrt[1 - e^2]) - 24*e^10*(160680 + 77563*Sqrt[1 - e^2]) + 
      9*(-415636 + 247237*Sqrt[1 - e^2]) + 27*e^4*(589228 + 333715*Sqrt[1 - e^2]) + 3*e^8*(6950172 + 3804539*Sqrt[1 - e^2]))*M)/(9216*(-1 + e^2)^6), 0, 
  -((81*(82700 - 47951*Sqrt[1 - e^2]) + e^14*(-208460 + 13777*Sqrt[1 - e^2]) - 3*e^2*(3780932 + 178909*Sqrt[1 - e^2]) - 5*e^12*(2268356 + 243483*Sqrt[1 - e^2]) - 
      3*e^4*(19909492 + 11982213*Sqrt[1 - e^2]) + e^10*(77643388 + 21941089*Sqrt[1 - e^2]) - e^8*(182745564 + 79560103*Sqrt[1 - e^2]) + e^6*(181024988 + 99739945*Sqrt[1 - e^2]))*M)/
   (6144*(-1 + e^2)^7), 0, -((-25659923 + 37293*e^16 - 16560112*Sqrt[1 - e^2] + 48*e^14*(146057 + 2795*Sqrt[1 - e^2]) + 16*e^2*(-12187669 + 22517174*Sqrt[1 - e^2]) + 
      8*e^12*(48927149 + 22622320*Sqrt[1 - e^2]) - 80*e^6*(71563997 + 51013546*Sqrt[1 - e^2]) - 16*e^10*(160351777 + 89464792*Sqrt[1 - e^2]) + 8*e^4*(278959085 + 141343664*Sqrt[1 - e^2]) + 
      8*e^8*(729139847 + 482117956*Sqrt[1 - e^2]))*M)/(32768*(1 - e^2)^(15/2)), 0, 
  -((14873903*e^18 + 9*e^16*(-641010713 + 47582480*Sqrt[1 - e^2]) + 3168*e^12*(682863805 + 238292094*Sqrt[1 - e^2]) - 144*e^14*(1700363986 + 373886615*Sqrt[1 - e^2]) + 
      2592*e^4*(695830111 + 513581639*Sqrt[1 - e^2]) + 135*(-1674520039 + 1530971920*Sqrt[1 - e^2]) - 27*e^2*(-12712321485 + 5155684688*Sqrt[1 - e^2]) - 
      864*e^6*(8358682747 + 5687970361*Sqrt[1 - e^2]) - 108*e^10*(63313939721 + 30425080104*Sqrt[1 - e^2]) + 108*e^8*(94445877975 + 56482738784*Sqrt[1 - e^2]))*M)/(5308416*(1 - e^2)^(17/2)), 0, 
  ((46098715*e^20 + 27*(92089076881 - 89361662620*Sqrt[1 - e^2]) + 4*e^18*(-2658464804 + 262727205*Sqrt[1 - e^2]) - 15*e^16*(73281631387 + 3434834748*Sqrt[1 - e^2]) + 
     96*e^14*(126237924796 + 32382296227*Sqrt[1 - e^2]) + 1296*e^6*(42489271144 + 47415318091*Sqrt[1 - e^2]) + 108*e^2*(-69750844012 + 54300146699*Sqrt[1 - e^2]) + 
     792*e^10*(131326559968 + 80026163893*Sqrt[1 - e^2]) - 168*e^12*(300117053053 + 131243198282*Sqrt[1 - e^2]) - 27*e^4*(117901064351 + 757510117120*Sqrt[1 - e^2]) - 
     72*e^8*(1546825752457 + 1233878102135*Sqrt[1 - e^2]))*M)/(7077888*(1 - e^2)^(19/2)), 0, 
  -((-681655733245 + 2021171*e^22 + 666035087932*Sqrt[1 - e^2] + e^2*(2660705287763 - 2212833869592*Sqrt[1 - e^2]) + e^20*(251199171 + 22503500*Sqrt[1 - e^2]) + 
      5*e^18*(-40433643617 + 1865248520*Sqrt[1 - e^2]) + 5*e^16*(620831869199 + 152379852348*Sqrt[1 - e^2]) - 28*e^14*(631916067135 + 276694742948*Sqrt[1 - e^2]) + 
      4*e^12*(12627782559575 + 7561369530018*Sqrt[1 - e^2]) + e^4*(1002177473931 + 10021782381404*Sqrt[1 - e^2]) - 4*e^10*(19785361704889 + 14966157513188*Sqrt[1 - e^2]) - 
      e^6*(27172864120069 + 35730979367728*Sqrt[1 - e^2]) + e^8*(67610656024924 + 63851352495368*Sqrt[1 - e^2]))*M)/(524288*(1 - e^2)^(21/2)), 0, 
  -((8326480099*e^24 + e^20*(624313952376096 - 65248007710320*Sqrt[1 - e^2]) + 729*(1304761082171 - 1424519362904*Sqrt[1 - e^2]) + 120*e^22*(-47180850049 + 2646688231*Sqrt[1 - e^2]) + 
      648*e^2*(-3618791356879 + 3951810851896*Sqrt[1 - e^2]) + 1296*e^4*(58411746147874 + 54007691218881*Sqrt[1 - e^2]) - 5184*e^14*(114293393789124 + 61416499339165*Sqrt[1 - e^2]) + 
      8640*e^12*(148824040486468 + 95955472123065*Sqrt[1 - e^2]) - 10368*e^10*(157698418091054 + 117646502211971*Sqrt[1 - e^2]) - 24*e^18*(770279792864101 + 204052301764556*Sqrt[1 - e^2]) - 
      216*e^6*(2173271175690029 + 1999148831208127*Sqrt[1 - e^2]) + 27*e^16*(5630236021485845 + 2357665714690984*Sqrt[1 - e^2]) + 27*e^8*(44543459286660021 + 37433032615566464*Sqrt[1 - e^2]))*M)/
   (1019215872*(1 - e^2)^(23/2)), 0, ((-30239989847279127 + 14104195881*e^26 + 30407881744691352*Sqrt[1 - e^2] + e^22*(193182619270172 - 49964173638600*Sqrt[1 - e^2]) + 
     65*e^24*(-93308128111 + 6609161096*Sqrt[1 - e^2]) - 621*e^2*(-200182766521589 + 178262043314344*Sqrt[1 - e^2]) - 44*e^20*(840629588847307 + 210457421158612*Sqrt[1 - e^2]) - 
     108*e^4*(5118022776514155 + 848193969312764*Sqrt[1 - e^2]) - 432*e^12*(24255852437050877 + 15491826982867348*Sqrt[1 - e^2]) + 108*e^6*(23719441294399477 + 16300392704973574*Sqrt[1 - e^2]) + 
     144*e^14*(42830035291432201 + 23638531433870822*Sqrt[1 - e^2]) - 9*e^16*(243612916646639047 + 111708113205537880*Sqrt[1 - e^2]) + e^18*(432640280848069961 + 155694122390978632*Sqrt[1 - e^2]) - 
     27*e^8*(258803502107240341 + 195466057673405912*Sqrt[1 - e^2]) + 27*e^10*(407120842333285971 + 290242526984200480*Sqrt[1 - e^2]))*M)/(679477248*(1 - e^2)^(25/2)), 0, 
  -((142865641703*e^28 + 4*e^26*(-4624786379082 + 793423698655*Sqrt[1 - e^2]) - 2448*e^22*(298351145919524 + 63062007366645*Sqrt[1 - e^2]) + 
      e^24*(-8207546191843853 + 283442224858220*Sqrt[1 - e^2]) - 729*(-2471703389302607 + 2479698241560332*Sqrt[1 - e^2]) + 972*e^2*(8742268233746002 + 17781281737893347*Sqrt[1 - e^2]) - 
      4320*e^14*(51512485845866011 + 36065520723307576*Sqrt[1 - e^2]) + 864*e^6*(352216542567856970 + 295301961103753039*Sqrt[1 - e^2]) + 
      81*e^16*(1957985490383180107 + 1080712140260338444*Sqrt[1 - e^2]) + 3*e^20*(3450975540112402029 + 1108574018345609888*Sqrt[1 - e^2]) - 
      81*e^4*(1228709877898176653 + 1205714846864716336*Sqrt[1 - e^2]) + 108*e^10*(2387673193285278250 + 1298917869764007131*Sqrt[1 - e^2]) - 
      12*e^18*(4731429805810651174 + 2052798169231637627*Sqrt[1 - e^2]) + 27*e^12*(2595822289790069489 + 3440604812276393472*Sqrt[1 - e^2]) - 
      27*e^8*(15997975270966059339 + 11718735043601956196*Sqrt[1 - e^2]))*M)/(8153726976*(1 - e^2)^(27/2)), 0, 
  -((227390859263*e^30 + e^26*(45789584887687959 - 4532207011544240*Sqrt[1 - e^2]) + 3*e^28*(-103827897443891 + 4307001762540*Sqrt[1 - e^2]) + 
      405*(-6113302231686445 + 6029835974115796*Sqrt[1 - e^2]) + e^24*(2543628201663429839 + 216129107480085116*Sqrt[1 - e^2]) + 81*e^2*(1933542530478255687 + 1044509647398470704*Sqrt[1 - e^2]) - 
      3*e^22*(16804577089392096163 + 4095941419530692272*Sqrt[1 - e^2]) - 27*e^12*(64255170410395105363 + 7928677234086917276*Sqrt[1 - e^2]) - 
      81*e^4*(14841782971194686761 + 11089050660847760516*Sqrt[1 - e^2]) - 189*e^8*(35935720160667907341 + 25970915451411247948*Sqrt[1 - e^2]) + 
      3*e^20*(119502853383406786613 + 44841636763585824460*Sqrt[1 - e^2]) + 81*e^10*(75439350424003022591 + 45390159969600138992*Sqrt[1 - e^2]) + 
      27*e^16*(93446932881844554629 + 60244161516776172676*Sqrt[1 - e^2]) - 27*e^14*(74733551109093890283 + 70355522454434572864*Sqrt[1 - e^2]) - 
      9*e^18*(144357705678228342889 + 72299787200325286256*Sqrt[1 - e^2]) + 27*e^6*(146204237412251821421 + 113273934106866744592*Sqrt[1 - e^2]))*M)/(16307453952*(1 - e^2)^(29/2)), 0, 
  ((28964232235865*e^32 + 81*(37732455705662790409 - 37693643895892438624*Sqrt[1 - e^2]) + 32*e^30*(-627416016866413 + 35028124938325*Sqrt[1 - e^2]) - 
     16*e^28*(-73922099714481207 + 13224711658384360*Sqrt[1 - e^2]) - 12960*e^2*(2240437718353325481 + 99022093462393814*Sqrt[1 - e^2]) - 
     32*e^26*(-4519876391993446583 + 360601014626122272*Sqrt[1 - e^2]) + 480*e^22*(78683297830234775149 + 26644841766666212210*Sqrt[1 - e^2]) - 
     8*e^24*(510701014796919490183 + 95726833678697962448*Sqrt[1 - e^2]) + 1296*e^4*(143062196033899770247 + 96235118783167486808*Sqrt[1 - e^2]) + 
     864*e^14*(134904491874722163629 + 330262608391972438069*Sqrt[1 - e^2]) - 864*e^6*(790435779954894858811 + 638445933854408425448*Sqrt[1 - e^2]) + 
     432*e^12*(2185093982182541934849 + 1016146228104459339536*Sqrt[1 - e^2]) - 864*e^10*(1933822633738995097879 + 1310588377638107726880*Sqrt[1 - e^2]) - 
     48*e^20*(3634830779215381785671 + 1686674562626185625040*Sqrt[1 - e^2]) + 96*e^18*(4663796201135474011349 + 2762985747733449091582*Sqrt[1 - e^2]) + 
     216*e^8*(6580330108771940588219 + 5110398398573818919736*Sqrt[1 - e^2]) - 54*e^16*(11011637616312281774037 + 8534905334413269123760*Sqrt[1 - e^2]))*M)/(521838526464*(1 - e^2)^(31/2))}, -1, 33, 2],

		True,
		Print["Expansion parameter is either \"p\" or \"x\""];
		Aborting[syms];
	]
]


def@
omegaRKerrPN[syms_Association]:=
Module[{M,p,e,a},
	
	M=BlackHoleMassSymbol[syms];
	a=BlackHoleSpinSymbol[syms];
	e=OrbitalEccentricitySymbol[syms];
	p=SemiLatusRectumSymbol[syms];
	
	SeriesData[p, Infinity, {(1 - e^2)^(3/2)/M, 0, (-3*(1 - e^2)^(5/2))/M, (3*a*(1 - e^2)^(5/2))/M^2, (-3*(1 - e^2)^(5/2)*(a^2 + (-2 + 6*e^2 + 5*Sqrt[1 - e^2])*M^2))/(2*M^3), 
  (2*a*(1 - e^2)^(5/2)*(1 + 8*e^2 + 6*Sqrt[1 - e^2]))/M^2, -((1 - e^2)^(5/2)*(a^2*(6 + 30*e^2 + 22*Sqrt[1 - e^2]) + 3*(14 + 18*e^4 - 5*Sqrt[1 - e^2] + 2*e^2*(-8 + 15*Sqrt[1 - e^2]))*M^2))/
   (2*M^3), (a*(1 - e^2)^(5/2)*(12*a^2*(e^2 + Sqrt[1 - e^2]) + (74 + 134*e^4 + 39*Sqrt[1 - e^2] + e^2*(-28 + 225*Sqrt[1 - e^2]))*M^2))/(2*M^4), 
  -((1 - e^2)^(5/2)*(2*a^4*(-7 + 7*e^2 + 12*Sqrt[1 - e^2]) + 4*a^2*(128 + 340*e^4 + 279*Sqrt[1 - e^2] + 3*e^2*(4 + 189*Sqrt[1 - e^2]))*M^2 + 
      3*(-348 + 432*e^6 + 618*Sqrt[1 - e^2] + e^2*(1352 - 455*Sqrt[1 - e^2]) + 12*e^4*(-77 + 90*Sqrt[1 - e^2]))*M^4))/(16*M^5), 
  (a*(1 - e^2)^(5/2)*(2*a^2*(2 + 54*e^4 + 87*Sqrt[1 - e^2] + e^2*(4 + 105*Sqrt[1 - e^2])) + (512*e^6 + 284*(-1 + 3*Sqrt[1 - e^2]) + 3*e^2*(440 + 43*Sqrt[1 - e^2]) + 
       3*e^4*(-236 + 443*Sqrt[1 - e^2]))*M^2))/(2*M^4), 
  -((1 - e^2)^(5/2)*(a^4*(-202 + 230*e^4 + 864*Sqrt[1 - e^2] + 4*e^2*(-7 + 186*Sqrt[1 - e^2])) + 4*a^2*(-958 + 1618*e^6 + 3730*Sqrt[1 - e^2] + 5*e^4*(-346 + 869*Sqrt[1 - e^2]) + 
        e^2*(3758 + 2182*Sqrt[1 - e^2]))*M^2 + 3*(420 + 1296*e^8 + 714*Sqrt[1 - e^2] + e^4*(8908 - 3570*Sqrt[1 - e^2]) + 108*e^6*(-49 + 40*Sqrt[1 - e^2]) + e^2*(-3284 + 6491*Sqrt[1 - e^2]))*M^4))/
   (16*M^5), (a*(1 - e^2)^(5/2)*(6*a^4*(-19 - 3*e^4 + 44*Sqrt[1 - e^2] + e^2*(6 + 28*Sqrt[1 - e^2])) + 4*a^2*(-1442 + 1370*e^6 + 5136*Sqrt[1 - e^2] + e^2*(3374 + 3840*Sqrt[1 - e^2]) + 
       e^4*(-1622 + 4179*Sqrt[1 - e^2]))*M^2 + (-7268 + 14896*e^8 + 31206*Sqrt[1 - e^2] + e^4*(82116 - 9999*Sqrt[1 - e^2]) + 12*e^6*(-4233 + 4336*Sqrt[1 - e^2]) + 
       e^2*(-8708 + 76281*Sqrt[1 - e^2]))*M^4))/(16*M^6), 
  -((1 - e^2)^(5/2)*(8*a^6*(-13 - 13*e^4 + 28*Sqrt[1 - e^2] + 2*e^2*(13 + 6*Sqrt[1 - e^2])) + 8*a^4*(-6068 + 2176*e^6 + 17965*Sqrt[1 - e^2] + 4*e^2*(2258 + 3601*Sqrt[1 - e^2]) + 
        e^4*(-5140 + 9661*Sqrt[1 - e^2]))*M^2 + 4*a^2*(-78180 + 56336*e^8 + 226142*Sqrt[1 - e^2] + 4*e^6*(-44663 + 51758*Sqrt[1 - e^2]) + e^4*(259108 + 58879*Sqrt[1 - e^2]) + 
        e^2*(51980 + 355609*Sqrt[1 - e^2]))*M^4 + (93312*e^10 + e^2*(634552 - 91776*Sqrt[1 - e^2]) + 5184*e^8*(-121 + 75*Sqrt[1 - e^2]) - 360*e^6*(-3287 + 1545*Sqrt[1 - e^2]) + 
        8*(-28918 + 44227*Sqrt[1 - e^2]) + e^4*(-855968 + 1148865*Sqrt[1 - e^2]))*M^6))/(128*M^7), 
  (a*(1 - e^2)^(5/2)*(2*a^4*(-1953 - 69*e^6 + 4948*Sqrt[1 - e^2] + e^4*(-1271 + 1456*Sqrt[1 - e^2]) + e^2*(2621 + 4132*Sqrt[1 - e^2])) + 
     4*a^2*(-22308 + 7436*e^8 + 55066*Sqrt[1 - e^2] + 34*e^6*(-788 + 905*Sqrt[1 - e^2]) + e^4*(30248 + 21795*Sqrt[1 - e^2]) + e^2*(21496 + 70449*Sqrt[1 - e^2]))*M^2 + 
     (52480*e^10 + e^6*(518232 - 167547*Sqrt[1 - e^2]) + 24*e^8*(-13621 + 9566*Sqrt[1 - e^2]) + 4*(-42406 + 74625*Sqrt[1 - e^2]) + 2*e^2*(152332 + 99711*Sqrt[1 - e^2]) + 
       4*e^4*(-61448 + 153669*Sqrt[1 - e^2]))*M^4))/(16*M^6), 
  -((1 - e^2)^(5/2)*(16*a^6*(-728 - 226*e^6 + 1657*Sqrt[1 - e^2] + e^4*(-276 + 43*Sqrt[1 - e^2]) + 10*e^2*(123 + 151*Sqrt[1 - e^2])) + 
      8*a^4*(-122200 + 16116*e^8 + 271877*Sqrt[1 - e^2] + 3*e^4*(22556 + 35925*Sqrt[1 - e^2]) + e^6*(-95812 + 89668*Sqrt[1 - e^2]) + 2*e^2*(67114 + 157137*Sqrt[1 - e^2]))*M^2 + 
      2*a^2*(-2302688 + 462976*e^10 + 4232592*Sqrt[1 - e^2] + e^6*(3575896 - 444270*Sqrt[1 - e^2]) + 160*e^8*(-17929 + 13387*Sqrt[1 - e^2]) + 4*e^2*(753430 + 1085081*Sqrt[1 - e^2]) + 
        e^4*(-799920 + 6022209*Sqrt[1 - e^2]))*M^4 + 3*(93312*e^12 + e^4*(2114344 - 772971*Sqrt[1 - e^2]) + 5184*e^10*(-181 + 90*Sqrt[1 - e^2]) - 216*e^8*(-9379 + 4860*Sqrt[1 - e^2]) + 
        8*(-31054 + 55111*Sqrt[1 - e^2]) + 18*e^6*(-132236 + 129901*Sqrt[1 - e^2]) + 8*e^2*(-50549 + 165820*Sqrt[1 - e^2]))*M^6))/(128*M^7), 
  (a*(1 - e^2)^(5/2)*(-48*a^6*(47 + 23*e^6 - 102*Sqrt[1 - e^2] + e^4*(1 + 34*Sqrt[1 - e^2]) - e^2*(103 + 120*Sqrt[1 - e^2])) + 
     8*a^4*(-110702 + 578*e^8 + 226687*Sqrt[1 - e^2] + e^6*(-61444 + 34941*Sqrt[1 - e^2]) + e^4*(18964 + 78147*Sqrt[1 - e^2]) + 3*e^2*(46836 + 83395*Sqrt[1 - e^2]))*M^2 + 
     4*a^2*(-2595952 + 293568*e^10 + 4732736*Sqrt[1 - e^2] + 4*e^6*(425694 + 77587*Sqrt[1 - e^2]) + 8*e^8*(-258667 + 189232*Sqrt[1 - e^2]) + 8*e^2*(362189 + 688394*Sqrt[1 - e^2]) + 
       e^4*(214952 + 5066133*Sqrt[1 - e^2]))*M^4 + (-8899888 + 1446272*e^12 + 14536920*Sqrt[1 - e^2] + 192*e^10*(-73850 + 39509*Sqrt[1 - e^2]) - 24*e^8*(-1017415 + 481522*Sqrt[1 - e^2]) + 
       24*e^2*(41327 + 1092043*Sqrt[1 - e^2]) + 3*e^4*(8726264 + 1188771*Sqrt[1 - e^2]) + e^6*(-25343176 + 32822703*Sqrt[1 - e^2]))*M^6))/(128*M^8)}, 3, 19, 2]
]


def@
omegaPhiKerrPN[syms_Association]:=
Module[{M,p,e,a},
	
	M=BlackHoleMassSymbol[syms];
	a=BlackHoleSpinSymbol[syms];
	e=OrbitalEccentricitySymbol[syms];
	p=SemiLatusRectumSymbol[syms];
	
	SeriesData[p, Infinity, {(1 - e^2)^(3/2)/M, 0, (3*e^2*(1 - e^2)^(3/2))/M, -(((1 - e^2)^(3/2)*(a + 3*a*e^2))/M^2), 
  (3*(1 - e^2)^(3/2)*(2*a^2*e^2 + (12*e^4 - 10*(-1 + Sqrt[1 - e^2]) + e^2*(-3 + 10*Sqrt[1 - e^2]))*M^2))/(4*M^3), 
  (a*(-1 + e^2)*(-12 + 13*Sqrt[1 - e^2] + 4*e^4*(-3 + 4*Sqrt[1 - e^2]) + e^2*(24 + 7*Sqrt[1 - e^2])))/M^2, 
  ((1 - e^2)^(3/2)*(2*a^2*(27 + 30*e^4 - 22*Sqrt[1 - e^2] + e^2*(9 + 22*Sqrt[1 - e^2])) + 3*(36*e^6 + e^2*(78 - 40*Sqrt[1 - e^2]) - 20*(-1 + Sqrt[1 - e^2]) + e^4*(-29 + 60*Sqrt[1 - e^2]))*M^2))/
   (4*M^3), -(a*(1 - e^2)^(3/2)*(2*a^2*(13 + 10*e^4 - 12*Sqrt[1 - e^2] + e^2*(1 + 12*Sqrt[1 - e^2])) + (362 + 268*e^6 - 342*Sqrt[1 - e^2] + e^2*(537 - 108*Sqrt[1 - e^2]) + 
        3*e^4*(7 + 150*Sqrt[1 - e^2]))*M^2))/(4*M^4), ((1 - e^2)^(3/2)*(24*a^4*(4 + e^4 - 4*Sqrt[1 - e^2] + e^2*(-2 + 4*Sqrt[1 - e^2])) + 
     4*a^2*(1360*e^6 + 32*(86 - 81*Sqrt[1 - e^2]) + 4*e^2*(713 + 81*Sqrt[1 - e^2]) + 7*e^4*(53 + 324*Sqrt[1 - e^2]))*M^2 + 
     3*(1728*e^8 + e^4*(8995 - 3140*Sqrt[1 - e^2]) - 4152*(-1 + Sqrt[1 - e^2]) + 96*e^6*(-37 + 45*Sqrt[1 - e^2]) + 4*e^2*(-302 + 743*Sqrt[1 - e^2]))*M^4))/(64*M^5), 
  -(a*(1 - e^2)^(3/2)*(a^2*(694 + 200*e^6 - 668*Sqrt[1 - e^2] + 4*e^2*(137 + 62*Sqrt[1 - e^2]) + e^4*(38 + 420*Sqrt[1 - e^2])) + 
      (3622 + 1024*e^8 - 3546*Sqrt[1 - e^2] + e^4*(4845 - 294*Sqrt[1 - e^2]) + e^2*(1301 + 1182*Sqrt[1 - e^2]) + 2*e^6*(-578 + 1329*Sqrt[1 - e^2]))*M^2))/(4*M^4), 
  ((1 - e^2)^(3/2)*(8*a^4*(71*e^6 - 792*(-1 + Sqrt[1 - e^2]) + 31*e^4*(-1 + 12*Sqrt[1 - e^2]) + 14*e^2*(37 + 30*Sqrt[1 - e^2])) + 
     4*a^2*(33820 + 6472*e^8 - 33124*Sqrt[1 - e^2] + 30*e^2*(641 + 324*Sqrt[1 - e^2]) + 5*e^6*(-1029 + 3476*Sqrt[1 - e^2]) + e^4*(30883 + 6024*Sqrt[1 - e^2]))*M^2 + 
     3*(5184*e^10 + e^6*(49753 - 17880*Sqrt[1 - e^2]) - 18912*(-1 + Sqrt[1 - e^2]) + 3456*e^8*(-6 + 5*Sqrt[1 - e^2]) - 8*e^2*(-3535 + 2029*Sqrt[1 - e^2]) + 16*e^4*(-1059 + 2234*Sqrt[1 - e^2]))*
      M^4))/(64*M^5), -(a*(1 - e^2)^(3/2)*(8*a^4*(248 - 29*e^6 - 252*Sqrt[1 - e^2] + 21*e^4*(1 + 4*Sqrt[1 - e^2]) + 24*e^2*(5 + 7*Sqrt[1 - e^2])) + 
      4*a^2*(47036 + 5272*e^8 - 46524*Sqrt[1 - e^2] + 42*e^2*(733 + 372*Sqrt[1 - e^2]) + e^4*(30211 + 14304*Sqrt[1 - e^2]) + e^6*(-5449 + 16596*Sqrt[1 - e^2]))*M^2 + 
      (421448 + 59584*e^10 - 417288*Sqrt[1 - e^2] + e^6*(493035 - 63300*Sqrt[1 - e^2]) + 32*e^8*(-5927 + 6504*Sqrt[1 - e^2]) - 12*e^2*(-31656 + 8483*Sqrt[1 - e^2]) + 
        e^4*(80305 + 374256*Sqrt[1 - e^2]))*M^4))/(64*M^6), ((1 - e^2)^(3/2)*(16*a^6*(-17*e^6 - 64*(-1 + Sqrt[1 - e^2]) + e^4*(35 + 12*Sqrt[1 - e^2]) + 4*e^2*(5 + 13*Sqrt[1 - e^2])) + 
     8*a^4*(83764 + 3472*e^8 - 83576*Sqrt[1 - e^2] + 4*e^2*(13996 + 8361*Sqrt[1 - e^2]) + e^6*(-11329 + 18818*Sqrt[1 - e^2]) + e^4*(40799 + 31314*Sqrt[1 - e^2]))*M^2 + 
     2*a^2*(225344*e^10 + 64*(41367 - 41033*Sqrt[1 - e^2]) - 8*e^2*(-259151 + 9499*Sqrt[1 - e^2]) + 56*e^8*(-11663 + 14788*Sqrt[1 - e^2]) + e^6*(1722261 + 186560*Sqrt[1 - e^2]) + 
       4*e^4*(300763 + 421854*Sqrt[1 - e^2]))*M^4 + (186624*e^12 - 1761712*(-1 + Sqrt[1 - e^2]) + 5184*e^10*(-239 + 150*Sqrt[1 - e^2]) - 36*e^8*(-82921 + 34140*Sqrt[1 - e^2]) + 
       8*e^2*(41277 + 37544*Sqrt[1 - e^2]) - 30*e^4*(-110393 + 37950*Sqrt[1 - e^2]) + e^6*(-2208553 + 3051300*Sqrt[1 - e^2]))*M^6))/(256*M^7)}, 3, 16, 2]
]


def@
azimuthalAdvanceKerrPN[syms_Association]:=
Module[{M,p,e,a},
	
	M=BlackHoleMassSymbol[syms];
	a=BlackHoleSpinSymbol[syms];
	e=OrbitalEccentricitySymbol[syms];
	p=SemiLatusRectumSymbol[syms];
	
	SeriesData[p, Infinity, {2*Pi, 0, 6*Pi, (-8*a*Pi)/M, (3*(18 + e^2 + (2*a^2)/M^2)*Pi)/2, (-72*a*Pi)/M, (3*(a^2*(50 - 6*e^2) + 15*(6 + e^2)*M^2)*Pi)/(2*M^2), 
  (2*a*(a^2*(-17 + 4*e^2 + e^4) - 27*(10 + e^2)*M^2)*Pi)/M^3, ((-8*a^4*(-23 + 10*e^2 + 4*e^4) - 4*a^2*(-7316 - 98*e^2 + 79*e^4)*M^2 + 105*(216 + 72*e^2 + e^4)*M^4)*Pi)/(32*M^4), 
  (2*a*(a^2*(-408 + 17*e^2 + 20*e^4 + e^6) - 3*(630 + 173*e^2)*M^2)*Pi)/M^3, 
  ((-8*a^4*(-1633 + 124*e^2 + 151*e^4 + 8*e^6) - 8*a^2*(-35462 - 7899*e^2 + 699*e^4 + 32*e^6)*M^2 + 567*(216 + 120*e^2 + 5*e^4)*M^4)*Pi)/(32*M^4), 
  -(a*(8*a^4*(109 - 7*e^2 - 13*e^4 + e^6) - 4*a^2*(-23606 - 4775*e^2 + 1286*e^4 + 127*e^6 + 4*e^8)*M^2 + 3*(68040 + 34936*e^2 + 739*e^4)*M^4)*Pi)/(8*M^5), 
  ((16*a^6*(99 + 2*e^2 - 7*e^4 + 8*e^6) - 8*a^4*(-156954 - 32753*e^2 + 15600*e^4 + 1385*e^6 + 32*e^8)*M^2 - 2*a^2*(-4899592 - 2412152*e^2 + 80217*e^4 + 12790*e^6 + 512*e^8)*M^4 + 
     231*(11664 + 9720*e^2 + 810*e^4 + 5*e^6)*M^6)*Pi)/(128*M^6)}, 0, 13, 2]
]


def@
radialPeriodKerrPN[syms_Association]:=
Module[{M,p,e,a},
	
	M=BlackHoleMassSymbol[syms];
	a=BlackHoleSpinSymbol[syms];
	e=OrbitalEccentricitySymbol[syms];
	p=SemiLatusRectumSymbol[syms];
	
	SeriesData[p, Infinity, {(2*M*Pi)/(1 - e^2)^(3/2), 0, (6*M*Pi)/Sqrt[1 - e^2], (-6*a*Pi)/Sqrt[1 - e^2], (3*(a^2 + (4 + 5*Sqrt[1 - e^2])*M^2)*Pi)/(Sqrt[1 - e^2]*M), 
  (4*a*(-10 + e^2 - 6*Sqrt[1 - e^2])*Pi)/Sqrt[1 - e^2], ((a^2*(42 - 6*e^2 + 22*Sqrt[1 - e^2]) + 3*(20 - 4*e^2 + 25*Sqrt[1 - e^2])*M^2)*Pi)/(Sqrt[1 - e^2]*M), 
  (a*(-6*a^2*(3 - e^2 + 2*Sqrt[1 - e^2]) + (4*e^4 + e^2*(40 + 9*Sqrt[1 - e^2]) - 7*(32 + 39*Sqrt[1 - e^2]))*M^2)*Pi)/(Sqrt[1 - e^2]*M^2), 
  ((a^4*(22 - 22*e^2 + 24*Sqrt[1 - e^2]) - 4*a^2*(-698 + 26*e^4 - 921*Sqrt[1 - e^2] + 3*e^2*(64 + 25*Sqrt[1 - e^2]))*M^2 + 3*(5*e^2*(-32 + 5*Sqrt[1 - e^2]) + 42*(16 + 29*Sqrt[1 - e^2]))*M^4)*Pi)/
   (8*Sqrt[1 - e^2]*M^3), (a*(2*a^2*(-134 + 12*e^4 - 225*Sqrt[1 - e^2] + e^2*(62 + 33*Sqrt[1 - e^2])) + 
     (4*e^6 + 3*e^4*(4 + 5*Sqrt[1 - e^2]) + 9*e^2*(36 + 5*Sqrt[1 - e^2]) - 10*(118 + 237*Sqrt[1 - e^2]))*M^2)*Pi)/(Sqrt[1 - e^2]*M^2), 
  ((-2*a^4*(-415 + 89*e^4 - 1056*Sqrt[1 - e^2] + e^2*(326 + 252*Sqrt[1 - e^2])) - 4*a^2*(30*e^6 + e^4*(134 + 125*Sqrt[1 - e^2]) + 2*e^2*(989 + 454*Sqrt[1 - e^2]) - 10*(483 + 1129*Sqrt[1 - e^2]))*
      M^2 + 3*(2784 - 32*e^4 + 7422*Sqrt[1 - e^2] + e^2*(-704 + 533*Sqrt[1 - e^2]))*M^4)*Pi)/(8*Sqrt[1 - e^2]*M^3), 
  (a*(6*a^4*(-21 + 11*e^4 - 116*Sqrt[1 - e^2] + e^2*(26 + 44*Sqrt[1 - e^2])) + 4*a^2*(46*e^6 + e^4*(350 + 307*Sqrt[1 - e^2]) + e^2*(3202 + 2416*Sqrt[1 - e^2]) - 2*(2639 + 7939*Sqrt[1 - e^2]))*
      M^2 + (32*e^8 + e^2*(16832 - 3855*Sqrt[1 - e^2]) + 24*e^6*(8 + 5*Sqrt[1 - e^2]) + 3*e^4*(64 + 355*Sqrt[1 - e^2]) - 2*(23744 + 73425*Sqrt[1 - e^2]))*M^4)*Pi)/(8*Sqrt[1 - e^2]*M^4), 
  ((-8*a^6*(1 + e^4 - 100*Sqrt[1 - e^2] + e^2*(-2 + 60*Sqrt[1 - e^2])) - 8*a^4*(-13046 + 2*e^6 - 57265*Sqrt[1 - e^2] + 5*e^4*(366 + 331*Sqrt[1 - e^2]) + 14*e^2*(801 + 970*Sqrt[1 - e^2]))*M^2 - 
     4*a^2*(-240368 + 272*e^8 - 893594*Sqrt[1 - e^2] + 32*e^6*(53 + 35*Sqrt[1 - e^2]) + e^4*(2720 + 13931*Sqrt[1 - e^2]) + e^2*(125088 + 30881*Sqrt[1 - e^2]))*M^4 + 
     (616*(448 + 1739*Sqrt[1 - e^2]) + e^4*(-6656 + 2049*Sqrt[1 - e^2]) + 8*e^2*(-9088 + 21279*Sqrt[1 - e^2]))*M^6)*Pi)/(64*Sqrt[1 - e^2]*M^5), 
  (a*(2*a^4*(-2081 - 169*e^6 - 16852*Sqrt[1 - e^2] + e^4*(593 + 584*Sqrt[1 - e^2]) + e^2*(2329 + 5732*Sqrt[1 - e^2])) + 
     4*a^2*(-42452 + 48*e^8 - 204422*Sqrt[1 - e^2] + e^6*(292 + 302*Sqrt[1 - e^2]) + e^4*(1060 + 5709*Sqrt[1 - e^2]) + 3*e^2*(10324 + 6777*Sqrt[1 - e^2]))*M^2 + 
     (32*e^10 + 24*e^8*(12 + 5*Sqrt[1 - e^2]) + 3*e^6*(256 + 545*Sqrt[1 - e^2]) - 104*e^2*(-932 + 1143*Sqrt[1 - e^2]) + 2*e^4*(64 + 3093*Sqrt[1 - e^2]) - 8*(28900 + 132963*Sqrt[1 - e^2]))*M^4)*
    Pi)/(8*Sqrt[1 - e^2]*M^4), -((16*a^6*(-178 - 212*e^6 - 6285*Sqrt[1 - e^2] + 3*e^4*(82 + 55*Sqrt[1 - e^2]) + 6*e^2*(24 + 485*Sqrt[1 - e^2])) + 
      8*a^4*(-144442 - 222*e^8 - 993043*Sqrt[1 - e^2] + 6*e^6*(-266 + 137*Sqrt[1 - e^2]) + 4*e^2*(34975 + 41699*Sqrt[1 - e^2]) + e^4*(6360 + 41831*Sqrt[1 - e^2]))*M^2 + 
      2*a^2*(608*e^10 + e^2*(1705568 - 913460*Sqrt[1 - e^2]) + 16*e^8*(334 + 155*Sqrt[1 - e^2]) + 2*e^6*(8608 + 19041*Sqrt[1 - e^2]) + 3*e^4*(-7488 + 92617*Sqrt[1 - e^2]) - 
        32*(87113 + 490554*Sqrt[1 - e^2]))*M^4 - 3*(-512*e^6 + 312*(1216 + 6803*Sqrt[1 - e^2]) + e^4*(-12800 + 21111*Sqrt[1 - e^2]) + 8*e^2*(-12992 + 75731*Sqrt[1 - e^2]))*M^6)*Pi)/
   (64*Sqrt[1 - e^2]*M^5), (a*(-96*a^6*(17*e^6 + e^2*(33 - 136*Sqrt[1 - e^2]) + e^4*(-22 + 5*Sqrt[1 - e^2]) + 3*(-4 + 75*Sqrt[1 - e^2])) - 
     8*a^4*(71900 + 1092*e^8 + 833817*Sqrt[1 - e^2] + e^6*(7132 + 2007*Sqrt[1 - e^2]) - e^4*(5672 + 46639*Sqrt[1 - e^2]) - e^2*(86548 + 199225*Sqrt[1 - e^2]))*M^2 + 
     4*a^2*(432*e^10 + e^2*(2013968 - 108478*Sqrt[1 - e^2]) + 8*e^8*(418 + 317*Sqrt[1 - e^2]) + e^6*(8704 + 54890*Sqrt[1 - e^2]) + e^4*(-56768 + 598407*Sqrt[1 - e^2]) - 
       20*(120660 + 883879*Sqrt[1 - e^2]))*M^4 + (256*e^12 + 192*e^10*(16 + 5*Sqrt[1 - e^2]) + 24*e^8*(544 + 665*Sqrt[1 - e^2]) - 3*e^4*(3328 + 6717*Sqrt[1 - e^2]) + 
       e^6*(19456 + 118863*Sqrt[1 - e^2]) - 48*e^2*(-86464 + 297419*Sqrt[1 - e^2]) - 8*(1098592 + 7375461*Sqrt[1 - e^2]))*M^6)*Pi)/(64*Sqrt[1 - e^2]*M^6)}, -3, 13, 2]
]


def@
specificEnergyKerrPN[syms_Association]:=
Module[{M,p,e,a},
	
	M=BlackHoleMassSymbol[syms];
	a=BlackHoleSpinSymbol[syms];
	e=OrbitalEccentricitySymbol[syms];
	p=SemiLatusRectumSymbol[syms];
	
	SeriesData[p, Infinity, {1, 0, (-1 + e^2)/2, 0, (3*(-1 + e^2)^2)/8, -((a*(-1 + e^2)^2)/M), ((-1 + e^2)^2*(8*a^2 + (27 + 5*e^2)*M^2))/(16*M^2), (-3*a*(-1 + e^2)^2*(3 + e^2))/(2*M), 
  ((-1 + e^2)^2*(96*a^2*(5 + 3*e^2) + (675 + 314*e^2 + 35*e^4)*M^2))/(128*M^2), -(a*(-1 + e^2)^2*(8*a^2*(1 + e^2) + 15*(3 + e^2)^2*M^2))/(8*M^3), 
  ((-1 + e^2)^2*(80*a^2*(63 + 50*e^2 + 15*e^4) + (3969 + 3389*e^2 + 771*e^4 + 63*e^6)*M^2))/(256*M^2), (-5*a*(-1 + e^2)^2*(16*a^2*(2 + e^2 + e^4) + 7*(3 + e^2)^3*M^2))/(16*M^3), 
  ((-1 + e^2)^2*(1920*a^4*(-1 + e^2)^2 + 224*a^2*(405 + 377*e^2 + 207*e^4 + 35*e^6)*M^2 + (45927 + 59620*e^2 + 21738*e^4 + 3556*e^6 + 231*e^8)*M^4))/(1024*M^4), 
  -(a*(-1 + e^2)^2*(-256*a^4*e^2 + 560*a^2*(15 + 5*e^2 + 9*e^4 + 3*e^6)*M^2 + 315*(3 + e^2)^4*M^4))/(128*M^5), 
  ((-1 + e^2)^2*(4480*a^4*(-1 + e^2)^2*(11 + 5*e^2) + 72*a^2*(10395 + 11252*e^2 + 8066*e^4 + 2740*e^6 + 315*e^8)*M^2 + (264627 + 466657*e^2 + 247678*e^4 + 61250*e^6 + 7935*e^8 + 429*e^10)*M^4))/
   (2048*M^4), (-7*a*(-1 + e^2)^2*(128*a^4*(1 - 7*e^2 - 3*e^4 + e^6) + 480*a^2*(3 + e^2)^2*(3 - e^2 + 2*e^4)*M^2 + 99*(3 + e^2)^5*M^4))/(256*M^5), 
  ((-1 + e^2)^2*(32256*a^4*(-1 + e^2)^2*(195 + 154*e^2 + 35*e^4) + 704*a^2*(66339 + 83753*e^2 + 68862*e^4 + 34482*e^6 + 8015*e^8 + 693*e^10)*M^2 + 
     (12196899 + 27202094*e^2 + 19642125*e^4 + 6634308*e^6 + 1288205*e^8 + 138798*e^10 + 6435*e^12)*M^4))/(32768*M^4), 
  -(a*(-1 + e^2)^2*(2048*a^6*(e^2 + e^4) + 8064*a^4*(7 - 26*e^2 - 14*e^4 - 2*e^6 + 3*e^8)*M^2 + 9240*a^2*(3 + e^2)^3*(7 - 4*e^2 + 5*e^4)*M^4 + 3003*(3 + e^2)^6*M^6))/(1024*M^7), 
  ((-1 + e^2)^2*(430080*a^6*(-1 + e^2)^4 + 50688*a^4*(-1 + e^2)^2*(1575 + 1691*e^2 + 725*e^4 + 105*e^6)*M^2 + 
     416*a^2*(841995 + 1238158*e^2 + 1109781*e^4 + 708932*e^6 + 248965*e^8 + 43470*e^10 + 3003*e^12)*M^4 + 
     (70366725 + 189182813*e^2 + 176252265*e^4 + 77422545*e^6 + 20096175*e^8 + 3238935*e^10 + 299299*e^12 + 12155*e^14)*M^6))/(65536*M^6), 
  (-3*a*(-1 + e^2)^2*(12288*a^6*(e^2 + 3*e^4) + 19712*a^4*(18 - 48*e^2 - 27*e^4 - 15*e^6 + 5*e^8 + 3*e^10)*M^2 + 16016*a^2*(3 + e^2)^4*(4 - 3*e^2 + 3*e^4)*M^4 + 2145*(3 + e^2)^7*M^6))/(2048*M^7), 
  ((-1 + e^2)^2*(1892352*a^6*(-1 + e^2)^4*(17 + 7*e^2) + 36608*a^4*(-1 + e^2)^2*(48195 + 64436*e^2 + 38994*e^4 + 11060*e^6 + 1155*e^8)*M^2 + 
     960*a^2*(5316597 + 9050341*e^2 + 8713993*e^4 + 6505705*e^6 + 3044767*e^8 + 804559*e^10 + 112035*e^12 + 6435*e^14)*M^4 + 
     (813439341 + 2549260696*e^2 + 2938262764*e^4 + 1625338152*e^6 + 532317390*e^8 + 114210600*e^10 + 15781612*e^12 + 1277848*e^14 + 46189*e^16)*M^6))/(262144*M^6), 
  -(a*(-1 + e^2)^2*(-65536*a^8*(e^2 + 3*e^4 + e^6) + 405504*a^6*(1 + 3*e^2 + 52*e^4 + 12*e^6 - 5*e^8 + e^10)*M^2 + 1537536*a^4*(3 + e^2)^2*(9 - 25*e^2 + 4*e^4 - 9*e^6 + 5*e^8)*M^4 + 
      480480*a^2*(3 + e^2)^5*(9 - 8*e^2 + 7*e^4)*M^6 + 109395*(3 + e^2)^8*M^8))/(32768*M^9)}, 0, 22, 2]
]


def@
specificAngularMomentumKerrPN[syms_Association]:=
Module[{M,p,e,a},
	
	M=BlackHoleMassSymbol[syms];
	a=BlackHoleSpinSymbol[syms];
	e=OrbitalEccentricitySymbol[syms];
	p=SemiLatusRectumSymbol[syms];
	
	SeriesData[p, Infinity, {M, 0, ((3 + e^2)*M)/2, -(a*(3 + e^2)), (8*a^2*(1 + e^2) + 3*(3 + e^2)^2*M^2)/(8*M), -(a*(15 + 14*e^2 + 3*e^4))/2, (16*a^2*(5 + 5*e^2 + 2*e^4) + 5*(3 + e^2)^3*M^2)/(16*M), 
  -(a*(8*a^2*(-1 + e^2)^2 + (189 + 213*e^2 + 95*e^4 + 15*e^6)*M^2))/(8*M^2), (-256*a^4*e^2 + 48*a^2*(63 + 45*e^2 + 41*e^4 + 11*e^6)*M^2 + 35*(3 + e^2)^4*M^4)/(128*M^3), 
  -(a*(32*a^2*(-1 + e^2)^2*(5 + 2*e^2) + (1215 + 1536*e^2 + 998*e^4 + 312*e^6 + 35*e^8)*M^2))/(16*M^2), 
  (128*a^4*(3 - 33*e^2 - 13*e^4 + 3*e^6) + 160*a^2*(3 + e^2)^2*(18 - e^2 + 11*e^4)*M^2 + 63*(3 + e^2)^5*M^4)/(256*M^3), 
  -(a*(16*a^2*(-1 + e^2)^2*(525 + 414*e^2 + 85*e^4) + (31185 + 44151*e^2 + 35450*e^4 + 16286*e^6 + 3685*e^8 + 315*e^10)*M^2))/(128*M^2), 
  (2048*a^6*(e^2 + e^4) + 640*a^4*(33 - 158*e^2 - 106*e^4 - 6*e^6 + 13*e^8)*M^2 + 280*a^2*(3 + e^2)^3*(55 - 20*e^2 + 37*e^4)*M^4 + 231*(3 + e^2)^6*M^6)/(1024*M^5), 
  -(a*(640*a^4*(-1 + e^2)^4 + 32*a^2*(-1 + e^2)^2*(2835 + 3145*e^2 + 1269*e^4 + 175*e^6)*M^2 + (199017 + 317598*e^2 + 290339*e^4 + 172308*e^6 + 58527*e^8 + 10094*e^10 + 693*e^12)*M^4))/(256*M^4), 
  (4096*a^6*e^2*(8 + 19*e^2 + e^4) + 8960*a^4*(39 - 119*e^2 - 95*e^4 - 35*e^6 + 12*e^8 + 6*e^10)*M^2 + 1008*a^2*(3 + e^2)^4*(39 - 23*e^2 + 28*e^4)*M^4 + 429*(3 + e^2)^7*M^6)/(2048*M^5), 
  -(a*(896*a^4*(-1 + e^2)^4*(49 + 19*e^2) + 8*a^2*(-1 + e^2)^2*(218295 + 303764*e^2 + 178170*e^4 + 48500*e^6 + 4935*e^8)*M^2 + 
      (2525985 + 4556469*e^2 + 4567501*e^4 + 3236577*e^6 + 1455827*e^8 + 379375*e^10 + 52479*e^12 + 3003*e^14)*M^4))/(1024*M^4), 
  (-65536*a^8*(e^2 + 3*e^4 + e^6) + 258048*a^6*(1 + 3*e^2 + 52*e^4 + 12*e^6 - 5*e^8 + e^10)*M^2 + 591360*a^4*(3 + e^2)^2*(9 - 25*e^2 + 4*e^4 - 9*e^6 + 5*e^8)*M^4 + 
    96096*a^2*(3 + e^2)^5*(9 - 8*e^2 + 7*e^4)*M^6 + 6435*(3 + e^2)^8*M^8 - 896*a^2*(-1 + e^2)^2*M^2*(128*a^4*(1 - 7*e^2 - 3*e^4 + e^6) + 480*a^2*(3 + e^2)^2*(3 - e^2 + 2*e^4)*M^2 + 
      99*(3 + e^2)^5*M^4))/(32768*M^7), -(a*(4608*a^4*(-1 + e^2)^4*(189 + 143*e^2 + 28*e^4) + 32*a^2*(-1 + e^2)^2*(486486 + 805823*e^2 + 606824*e^4 + 243186*e^6 + 49210*e^8 + 3927*e^10)*M^2 + 
      (15949791 + 32467620*e^2 + 35192320*e^4 + 28231108*e^6 + 15640006*e^8 + 5458444*e^10 + 1140664*e^12 + 131340*e^14 + 6435*e^16)*M^4))/(2048*M^4), 
  (-589824*a^8*e^2*(2 + 11*e^2 + 7*e^4) + 1892352*a^6*(5 - 15*e^2 + 102*e^4 + 44*e^6 - 5*e^8 - 5*e^10 + 2*e^12)*M^2 + 768768*a^4*(3 + e^2)^3*(25 - 68*e^2 + 28*e^4 - 32*e^6 + 15*e^8)*M^4 + 
    411840*a^2*(3 + e^2)^6*(5 - 5*e^2 + 4*e^4)*M^6 + 12155*(3 + e^2)^9*M^8 - 64*a^2*(-1 + e^2)^2*(2048*a^6*(e^2 + e^4) + 8064*a^4*(7 - 26*e^2 - 14*e^4 - 2*e^6 + 3*e^8)*M^2 + 
      9240*a^2*(3 + e^2)^3*(7 - 4*e^2 + 5*e^4)*M^4 + 3003*(3 + e^2)^6*M^6))/(65536*M^7), 
  -(a*(258048*a^6*(-1 + e^2)^6 + 5632*a^4*(-1 + e^2)^4*(18711 + 20503*e^2 + 7885*e^4 + 1029*e^6)*M^2 + 32*a^2*(-1 + e^2)^2*(32837805 + 62911738*e^2 + 56965371*e^4 + 29754572*e^6 + 8949715*e^8 + 
        1425690*e^10 + 93093*e^12)*M^4 + (802180665 + 1834264203*e^2 + 2143826084*e^4 + 1879261596*e^6 + 1219955310*e^8 + 532497210*e^10 + 149334804*e^12 + 25949772*e^14 + 2555553*e^16 + 
        109395*e^18)*M^6))/(32768*M^6), (524288*a^10*(e^2 + 6*e^4 + 6*e^6 + e^8) - 6488064*a^8*e^2*(4 + 33*e^2 + 42*e^4 + e^6)*M^2 + 
    12300288*a^6*(33 - 124*e^2 + 378*e^4 + 225*e^6 + 37*e^8 - 42*e^10 + 5*e^14)*M^4 + 3843840*a^4*(3 + e^2)^4*(33 - 90*e^2 + 54*e^4 - 50*e^6 + 21*e^8)*M^6 + 
    875160*a^2*(3 + e^2)^7*(11 - 12*e^2 + 9*e^4)*M^8 + 46189*(3 + e^2)^10*M^10 - 384*a^2*(-1 + e^2)^2*M^2*(12288*a^6*(e^2 + 3*e^4) + 19712*a^4*(18 - 48*e^2 - 27*e^4 - 15*e^6 + 5*e^8 + 3*e^10)*M^2 + 
      16016*a^2*(3 + e^2)^4*(4 - 3*e^2 + 3*e^4)*M^4 + 2145*(3 + e^2)^7*M^6))/(262144*M^9), 
  -(a*(270336*a^6*(-1 + e^2)^6*(45 + 17*e^2) + 3328*a^4*(-1 + e^2)^4*(405405 + 572756*e^2 + 323094*e^4 + 83300*e^6 + 8085*e^8)*M^2 + 
      64*a^2*(-1 + e^2)^2*(132914925 + 289207247*e^2 + 302986485*e^4 + 192821935*e^6 + 76397855*e^8 + 18187365*e^10 + 2374911*e^12 + 130845*e^14)*M^4 + 
      (5024184165 + 12825661362*e^2 + 16184694365*e^4 + 15173777592*e^6 + 11116865930*e^8 + 5797804620*e^10 + 2043225186*e^12 + 476243320*e^14 + 70710321*e^16 + 6078930*e^18 + 230945*e^20)*M^6))/
   (65536*M^6), (11534336*a^10*(e^2 + 9*e^4 + 14*e^6 + 4*e^8) + 14057472*a^8*(1 - 23*e^2 - 159*e^4 - 407*e^6 - 67*e^8 + 21*e^10 - 7*e^12 + e^14)*M^2 + 
    73801728*a^6*(3 + e^2)^2*(10 - 44*e^2 + 102*e^4 - 7*e^6 + 15*e^8 - 17*e^10 + 5*e^12)*M^4 + 18670080*a^4*(3 + e^2)^5*(21 - 58*e^2 + 43*e^4 - 36*e^6 + 14*e^8)*M^6 + 
    3695120*a^2*(3 + e^2)^8*(6 - 7*e^2 + 5*e^4)*M^8 + 88179*(3 + e^2)^11*M^10 + 16*a^2*(-1 + e^2)^2*(65536*a^8*(e^2 + 3*e^4 + e^6) - 405504*a^6*(1 + 3*e^2 + 52*e^4 + 12*e^6 - 5*e^8 + e^10)*M^2 - 
      1537536*a^4*(3 + e^2)^2*(9 - 25*e^2 + 4*e^4 - 9*e^6 + 5*e^8)*M^4 - 480480*a^2*(3 + e^2)^5*(9 - 8*e^2 + 7*e^4)*M^6 - 109395*(3 + e^2)^8*M^8))/(524288*M^9)}, -1, 22, 2]
]


End[];

EndPackage[];
