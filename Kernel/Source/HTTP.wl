(* ::Section:: *)(* Dependencies & Context *)
BeginPackage["TonyAristeidou`LocalDeploy`", {
	"TonyAristeidou`LocalDeploy`",
	"TonyAristeidou`LocalDeploy`Private`"
}];

Begin["`FileScope`HTTP`Private`"];


(* -------------------------------------------------------------------------- *)
(* ::Section:: *)(* handleClient *)
(* Description:  Creates the callback called upon client connection.
 * Return:       _Function
 *)
handleClient[
	expr_,
	responseQueue_DataStructure,
	handlers_Association
] := Function[{data},
	Block[{ req, cTask,
			x = expr,
			client = data["SourceSocket"],
			dataBa = data["DataByteArray"]
		},
		Enclose[
			(* Import HTTPRequest *)
			req = ImportByteArray[dataBa, "HTTPRequest"];
			(* HTTPResponseReceived handler *)
			Lookup[handlers, "HTTPRequestReceived", Identity][
				<|
					data,
					<|
						"HTTPRequest" -> req,
						"HTTPResponse" -> Missing[]
					|>
				|>
			];
			(* Start the HTTPResponse task *)
			DistributeDefinitions[CORSHeaders, generateCORSHTTPResponse];
			cTask = ParallelSubmit[{req, x},
				If[req["Method"] === "OPTIONS",
					(* Handle OPTIONS request *)
					HTTPResponse[
						"",
						<|"Headers" -> CORSHeaders|>
					],
					(* Handle actual request *)
					generateCORSHTTPResponse[x, req]
				]
			];
			(* Update message queue *)
			responseQueue["Push", {client , cTask}];
		]
	]
];
(* -------------------------------------------------------------------------- *)
(* ::Section:: *)(* CORSHeaders *)
(* Description:  Association of headers for CORS support.
 * Return:       _Association
 *)
CORSHeaders = <|
	"Access-Control-Allow-Origin"->"*",
	"Access-Control-Allow-Methods"->"GET, POST, OPTIONS",
	"Access-Control-Allow-Headers"->"Origin, Content-Type, Accept"
|>;


(* -------------------------------------------------------------------------- *)
(* ::Section:: *)(* generateCORSHTTPResponse *)
(* Description:  Generates an HTTP response with CORS headers.
 * Return:       _HTTPResponse
 *)
generateCORSHTTPResponse[expr_, req_] := Module[{
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


(* ::Section:: *)(* End *)
End[];
EndPackage[];
