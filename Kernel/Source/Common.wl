BeginPackage["TonyAristeidou`LocalDeploy`", {
	"TonyAristeidou`LocalDeploy`",
	"TonyAristeidou`LocalDeploy`Private`"
}];

Begin["`FileScope`Common`Private`"];

$localDeployments = Replace[$localDeployments,
	Except[_DataStructure] :> CreateDataStructure["HashTable"]
];

$icon = Import[
	PacletObject["TonyAristeidou/LocalDeploy"]["AssetLocation", "icon.svg"],
	"Graphics"
];

localDeploymentQ = {asc} |-> (
	AllTrue[keys, KeyExistsQ[asc, #]&]
);
portP = _?NumericQ|Automatic;

CORSHeaders = <|
	"Access-Control-Allow-Origin"->"*",
	"Access-Control-Allow-Methods"->"GET, POST, OPTIONS",
	"Access-Control-Allow-Headers"->"Origin, Content-Type, Accept"
|>;

generateCORSHTTPResponse[expr_,req_] := Module[{
		response = GenerateHTTPResponse[expr, req]
	},
	HTTPResponse[
		response["Body"],
		<|
			"Headers" -> <|
				<|response["Headers"]|>,
				CORSHeaders
			|>
		|>
	]
];

removeLocalDeploymentIfExists[base_String, port_Integer] := Quiet[
	Close @ $localDeployments["Lookup", {base, port}],
	{Close::stream}
];


End[];
EndPackage[];
