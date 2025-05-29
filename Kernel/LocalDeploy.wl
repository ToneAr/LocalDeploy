(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::AbbreviatedStringPatterns:: *)
Enclose[
	(Confirm @* Get @* Function["TonyAristeidou`LocalDeploy`"<>#]) /@ {
		(* Relative context to TonyAristeidou`LocalDeploy` *)
		"Public`",
		"Private`",
		"Source`Common`",
		"Source`Objects`",
		"Source`Deployment`"
	};

]
(* :!CodeAnalysis::EndBlock:: *)
