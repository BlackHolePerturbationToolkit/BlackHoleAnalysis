(* ::Package:: *)

<< BlackHoleAnalysis`;
<< ApplicationTools`;

packages =
{"OverloadedSymbols",
          "AnalyticTools",
          "AssociationNested",
          "Coordinates",
          "Discontinuities",
          "Fields",
          "GSFEqs",
          "Harmonics",
          "KerrEqs",
          "Labels",
          "PN",
          "SchwEqs",
          "SeriesTools",
          "Sources",
          "Symbols",
          "Utils",
          "ValidityTests"};

packageSymbols = Map[# -> DocumentedSymbols["BlackHoleAnalysis", #] &, packages];

appSymbols = "BlackHoleAnalysis" -> DocumentedSymbols["BlackHoleAnalysis"];
$PackageSymbols = Join[packageSymbols, {appSymbols}]; (* Used in the Overview.md file *)

undocumentedSymbols = Map[# -> UndocumentedSymbols["BlackHoleAnalysis", #] &, packages] /. (_ -> {}) -> Sequence[]

Map[Print["Undocumented symbols for package "<>#[[1]]<>" skipped:\n", #[[2]]]&, undocumentedSymbols];

Print["Building symbol reference pages"];
docPackage[package_ -> symbols_] :=
  Map[(Print[#]; BuildSymbolReference["BlackHoleAnalysis", #, "Source"]) &, symbols];
Scan[docPackage, packageSymbols];
docPackage[appSymbols];

Print["Building guides"];
sourceGuides = FileNames["*.md", FileNameJoin[{"Source", "Documentation", "English", "Guides"}], Infinity];
destGuides =
  FileNameJoin[{Directory[], FileNameDrop[DirectoryName[#], 1],
      FileBaseName[#] <> ".nb"}] & /@ sourceGuides;
MapThread[BuildGuide, {sourceGuides, destGuides}];

Print["Building tutorials"];
tutorialSources = FileNames["*.md", FileNameJoin[{"Source", "Documentation", "English", "Tutorials"}], Infinity];
Map[(Print[#]; BuildTutorial[FileNameJoin[{Directory[], #}]])&, tutorialSources];

Print["Indexing Documentation"];
BuildIndex["BlackHoleAnalysis"];

Print["Done"];
