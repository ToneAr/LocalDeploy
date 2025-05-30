Enclose[
	(Confirm @* Get @* Function["ToneAr`LocalDeploy`"<>#]) /@ {
		(* Relative context to ToneAr`LocalDeploy` *)
		"Public`",
		"Private`",
		"Source`Common`",
		"Source`HTTP`",
		"Source`Objects`",
		"Source`Deployment`"
	};

]
