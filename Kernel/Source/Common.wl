(* ::Section:: *)(* Dependencies & Context *)
BeginPackage["TonyAristeidou`LocalDeploy`", {
	"TonyAristeidou`LocalDeploy`",
	"TonyAristeidou`LocalDeploy`Private`"
}];

Begin["`FileScope`Common`Private`"];


(* -------------------------------------------------------------------------- *)
(* ::Section:: *)(* $localDeployments *)
(* Description:  Hash table that stores LocalDeploymentObject associations.
 * Return:       DataStructure["HashTable", ___]
 *)
$localDeployments = Replace[$localDeployments,
	Except[_DataStructure] :> CreateDataStructure["HashTable"]
];


(* -------------------------------------------------------------------------- *)
(* ::Section:: *)(* $icon *)
(* Description:  Icon graphic used for LocalDeploymentObject.
 * Return:       _Graphics
 *)
$icon = Import[
	PacletObject["TonyAristeidou/LocalDeploy"]["AssetLocation", "icon.svg"],
	"Graphics"
];


(* -------------------------------------------------------------------------- *)
(* ::Section:: *)(* localDeploymentQ *)
(* Description:  Function that checks if an association is a valid LocalDeploymentObject.
 * Return:       _?BooleanQ
 *)
localDeploymentQ = {asc} |-> (
	AllTrue[keys, KeyExistsQ[asc, #]&]
);

(* -------------------------------------------------------------------------- *)
(* ::Section:: *)(* portP *)
(* Description:  Pattern that matches a numeric port or Automatic.
 * Return:       _?NumericQ|Automatic
 *)
portP = _?NumericQ|Automatic;


(* -------------------------------------------------------------------------- *)
(* ::Section:: *)(* handleClient *)
(* Description:  Creates the callback called upon client connection.
 * Return:       _String | Null
 *)
removeLocalDeploymentIfExists[base_String, port_Integer] := Quiet[
	Replace[
		Close @ $localDeployments["Lookup", {base, port}],
		_Close -> Null
	],
	{Close::stream}
];


(* ::Section:: *)(* End *)
End[];
EndPackage[];
