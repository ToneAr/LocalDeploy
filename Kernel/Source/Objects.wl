(* ::Section:: *)(* Dependencies & Context *)
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
	"HostAddress",
	"HostPort",
	"URL",
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
			{BoxForm`SummaryItem[{"Host Address: ", asc["HostAddress"]}]},
			{BoxForm`SummaryItem[{"Host Port: ",    asc["HostPort"]}]}
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
(* ::Section:: *)(* Up-Value: Close | DeleteObject *)
(* Description:  Allows for graceful closing of local deployments using Close
 *               or DeleteObject.
 * Return:       _String | _Failure
 *)
LocalDeploymentObject /: (Close|DeleteObject)[
	dep:LocalDeploymentObject[assoc : _Association?localDeploymentQ]
] := (
	$localDeployments["KeyDrop", {assoc["HostAddress"], assoc["HostPort"]}];
	Quiet @ TaskRemove[assoc["EvaluationQueueTask"]];
	Close @ assoc["Socket"]
);


(* -------------------------------------------------------------------------- *)
(* ::Section:: *)(* Up-Value: localDeployment[_String] *)
(* Description:  Allows LocalDeploymentObject queried like an Association.
 * Return:       _
 *)
LocalDeploymentObject[asc: _Association?localDeploymentQ][prop_] :=
	Lookup[asc, prop];
(* -------------------------------------------------------------------------- *)
(* ::Section:: *)(* Up-Value: localDeployment["Properties"] *)
(* Description:  Description
 * Return:       ReturnPattern
 *)
LocalDeploymentObject[_Association?localDeploymentQ]["Properties"] := keys;
(* -------------------------------------------------------------------------- *)
(* ::Section:: *)(* Up-Value: URLExecute*)
(* Description:  Allows URLExecute to be used with LocalDeploymentObjects directly.
 * Return:       _
 *)
LocalDeploymentObject /: (
	URLExecute[
		LocalDeploymentObject[asc: _Association?localDeploymentQ],
			rest___
	]
) := (
	URLExecute[asc["URL"], rest]
);


(* -------------------------------------------------------------------------- *)
(* ::Section:: *)(* Up-Value: Normal *)
(* Description:  Returns the underlying Association of a LocalDeploymentObject.
 * Return:       _Association?localDeploymentQ
 *)
LocalDeploymentObject /: Normal[
	LocalDeploymentObject[asc: _Association?localDeploymentQ]
] := asc;

(* ::Section:: *)(* End *)
End[];
EndPackage[];
