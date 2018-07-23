(* ::Package:: *)

BeginPackage["BlackHoleAnalysis`SeriesTools`",
				{"BlackHoleAnalysis`Utils`",
				"BlackHoleAnalysis`Symbols`"}];


SeriesCoefficientList;
SeriesVariable;
SeriesExpansionPoint;
TruncateSeries;
SeriesTake;
SeriesStepSize;
SeriesOrderError;
SeriesMinOrder;
SeriesMap;


Begin["`Private`"];


def@
SeriesCoefficientList[ser_SeriesData] := ser[[3]]


def@
SeriesVariable[ser_SeriesData] := ser[[1]]


def@
SeriesExpansionPoint[ser_SeriesData] := ser[[2]]


def@
TruncateSeries[ser_SeriesData, newMax_Integer] := SeriesData[ser[[1]], ser[[2]], Take[SeriesCoefficientList[ser], (newMax + 1) SeriesStepSize[ser] - SeriesMinOrder[ser]], ser[[4]], newMax + 1, ser[[6]]]


Clear[takeNumQ]
takeNumQ=((MatchQ[#,_Integer]&&#>=0)||#===All)&;


def@
SeriesTake[ser_SeriesData, takeNum_?takeNumQ] := 
Module[{take},
take[x_,num_]:=If[Length[x]<num,Take[x,All],Take[x,num]];
If[takeNum===All,
	ser,
	SeriesData[ser[[1]],
			 ser[[2]],
			 take[SeriesCoefficientList[ser], (takeNum-1) SeriesStepSize[ser]+1], 
			ser[[4]], 
			(takeNum-1+ SeriesMinOrder[ser])SeriesStepSize[ser]+1, 
			ser[[6]]]
]
]


reDef@
SeriesTake[ser_SeriesData, takeNum1_?takeNumQ, takeNum2_?takeNumQ] :=  SeriesMap[If[MatchQ[#,_SeriesData],SeriesTake[#,takeNum2],#]&,SeriesTake[ser,takeNum1]]


(*def@
SeriesTake[ser_SeriesData, takeNum_?takeNumQ] := 
If[takeNum===All,
	ser,
	SeriesData[ser[[1]], ser[[2]], Take[SeriesCoefficientList[ser], takeNum SeriesStepSize[ser]], ser[[4]], (takeNum + SeriesMinOrder[ser])SeriesStepSize[ser], ser[[6]]]
]*)


(*reDef@
SeriesTake[ser_SeriesData, takeNum1_?takeNumQ, takeNum2_?takeNumQ] :=  SeriesMap[If[MatchQ[#,_SeriesData],SeriesTake[#,takeNum2],#]&,SeriesTake[ser,takeNum1]]*)


def@SeriesMap[fn_,expr_]:=expr
reDef@SeriesMap[fn_,expr_,{n_Integer}]:=expr
reDef@SeriesMap[fn_,ser_SeriesData,{1}]:=SeriesMap[fn,ser];
reDef@SeriesMap[fn_,ser_SeriesData]:=SeriesData[ser[[1]],ser[[2]],(fn/@ser[[3]]),ser[[4]],ser[[5]],ser[[6]]]
reDef@SeriesMap[fn_,ser_SeriesData,{n_Integer}]:=SeriesData[ser[[1]],ser[[2]],Map[SeriesMap[fn,#,{n-1}]&,ser[[3]]],ser[[4]],ser[[5]],ser[[6]]]


(*Clear[seriesCoeff]
seriesCoeff[ser_SeriesData, ord_] := seriesCoeffs[ser][[]]*)


def@
SeriesStepSize[ser_SeriesData] := ser[[-1]]


def@
SeriesMinOrder[ser_SeriesData] := ser[[-3]]/SeriesStepSize[ser]


def@
SeriesMaxOrder[ser_SeriesData] := ser[[-2]]/SeriesStepSize[ser]


def@
SeriesOrderError[ser_SeriesData] := ser[[-2]]/SeriesStepSize[ser]


End[];

EndPackage[];
