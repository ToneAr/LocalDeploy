BeginPackage["TonyAristeidou`LocalDeploy`", {
	"TonyAristeidou`LocalDeploy`",
	"TonyAristeidou`LocalDeploy`Private`",

	"GeneralUtilities`",
	"ZeroMQLink`",
	"Parallel`Developer`"
}];

Begin["`FileScope`Objects`Private`"];

(* -------------------------------------------------------------------------- *)
(* ::Section:: *)(* keys *)
(* Description:  Known keys for values contained in LocalDeploymentObject.
 * Return:       {__String}
 *)
keys = {
	"Listener",
	"Socket",
	"Hostname",
	"Port",
	"BaseURL",
	"Endpoints",
	"EvaluationQueueTask",
	"ResponseQueue"
};

(* -------------------------------------------------------------------------- *)
(* ::Section:: *)(* LocalDeploymentObject *)
(* Description:  Symbolic representation of a local deployment server.
 * Return:       _LocalDeploymentObject
 *)
LocalDeploymentObject /: MakeBoxes[
	obj:( LocalDeploymentObject[asc: _Association?localDeploymentQ]),
	form: (StandardForm | TraditionalForm )
] :=
	Module[{above, below},
		above = {
			{BoxForm`SummaryItem[{"Hostname: ", asc["Hostname"]}]},
			{BoxForm`SummaryItem[{"Port: ",     asc["Port"]}]}
		};
		below = {
			BoxForm`SummaryItem[{"Socket: ",        asc["Socket"]}],
			BoxForm`SummaryItem[{"Listener: ",      asc["Listener"]}],
			BoxForm`SummaryItem[{"Endpoints: ",     asc["Endpoints"]}],
			BoxForm`SummaryItem[{"Message Queue: ", asc["MessageQueue"]}]
		};

		BoxForm`ArrangeSummaryBox[
			LocalDeploymentObject, (* head *)
			obj,      (* interpretation *)
			$icon,
			above,    (* always shown content *)
			below,    (* expandable content *)
			form,
			"Interpretable" -> Automatic
		]
	];


(* -------------------------------------------------------------------------- *)
(* ::Section:: *)(* Up-Values *)
(* Description:  Up-Value definitions for LocalDeploymentObject
 *)
LocalDeploymentObject /: (Close|DeleteObject)[
	dep:LocalDeploymentObject[assoc : _Association?localDeploymentQ]
] := (
	$localDeployments["KeyDrop", {assoc["Hostname"], assoc["Port"]}];
	Quiet @ TaskRemove[assoc["QueueTask"]];
	Close @ assoc["Socket"]
);
LocalDeploymentObject[asc: _Association?localDeploymentQ][prop_] :=
	Lookup[asc, prop];
LocalDeploymentObject[_Association?localDeploymentQ]["Properties"] := keys;
LocalDeploymentObject /: (
	URLExecute[
		LocalDeploymentObject[asc: _Association?localDeploymentQ],
			rest___
	]
) := (
		URLExecute[asc["BaseURL"], rest]
	);
LocalDeploymentObject /: Normal[
	LocalDeploymentObject[asc: _Association?localDeploymentQ]
] := asc;

End[];
EndPackage[];
