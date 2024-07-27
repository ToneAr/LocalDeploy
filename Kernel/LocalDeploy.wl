BeginPackage["LocalDeploy`", {
	"GeneralUtilities`",
	"ZeroMQLink`"
}];

SetUsage[
	LocalDeploy,
	"LocalDeploy[ expr$ ] deploys a socket listener on an available local which can take request and return a LocalDeploymentObject.
LocalDeploy[ api$, port$ ] deploys a socket listener on port$ which can take request and return a LocalDeploymentObject."
];
SetUsage[
	LocalDeployments,
	"LocalDeployments[] returns a list with all currently active LocalDeploymentObjects."
]
SetUsage[
	LocalDeploymentObject,
	"LocalDeploymentObject is an object representation of a local deployment.
The following properties can be returned using LocalDeploymentObject[property$]:
| Property | Description |
| --- | --- |
| \"Listener\" | Returns the deployment's SocketListener. |
| \"Socket\" | Returns the server SocketObject. |
| \"LocalIPAddress\" | Returns the IPAdress of the deployment. |
| \"Port\" | Returns the port the deployment is listening on. |
| \"BaseURL\" | Returns the url required for an HTTP request to the deployment. |
| \"Endpoints\" | Returns an association of all endpoints deployed and their Iconized expressions |
The deployment can be closed using Close[ $localDeployment ] or DeleteObject[ $localDeployment ]"
];

Begin["`Private`"];

(* ::Subsection:: *)(* LocalDeploymentObject *)
	(* ::Subsubsection:: *)(* Patterns *)
	localDeploymentP = (
		_Association?({in} |-> AllTrue[keys, KeyExistsQ[in, #]&])
	);

	(* ::Subsubsection:: *)(* Utilities *)
	keys = {
		"Listener",
		"Socket",
		"LocalIPAddress",
		"Port",
		"BaseURL",
		"Endpoints"
	};
	$icon = Graphics[Import[PacletObject["LocalDeploy"]["AssetLocation", "Icon"]],
		ImageSize -> Dynamic[{ 
			Automatic,
			3.5 * CurrentValue["FontCapHeight"] / AbsoluteCurrentValue[Magnification]
		}]
	];

	(* ::Subsubsection:: *)(* Main *)
	LocalDeploymentObject /: MakeBoxes[
		(*
			For some reason localDeploymentP breaks this definition so i hardcoded the pattern.
			TODO: fix this
		*)
		obj: LocalDeploymentObject[asc: _Association?({in} |-> AllTrue[keys, KeyExistsQ[in, #]&])],
		form: (StandardForm | TraditionalForm )
	] :=
		Module[{above, below},
			above = {
				{BoxForm`SummaryItem[{"Local IP Address: ",	asc["LocalIPAddress"]}]},
				{BoxForm`SummaryItem[{"Local Port: ",		asc["Port"]}]}
			};
			below = {
				BoxForm`SummaryItem[{"Socket: ",	asc["Socket"]}],
				BoxForm`SummaryItem[{"Listener: ",	asc["Listener"]}],
				BoxForm`SummaryItem[{"Endpoints: ",	asc["Endpoints"]}]
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
	LocalDeploymentObject /: (Close|DeleteObject)[
		dep:LocalDeploymentObject[assoc : localDeploymentP]
	] := (
		DeleteCases[$LocalDeployments, dep];
		Close[assoc["Socket"]]
	);
	LocalDeploymentObject[asc: localDeploymentP][prop_] :=
		Lookup[asc, prop];

(* ::Subsection:: *)(* LocalDeployments *)
$LocalDeployments = {};
LocalDeployments[]:= Set[
	$LocalDeployments
	,
	Select[$LocalDeployments,
		Not@FailureQ[ #["Socket"]["Properties"] ]&
	]
];

(* ::Subsection:: *)(* LocalDeploy *)
	(* ::Subsubsection:: *)(* Patterns *)
	portP = _?NumericQ|Automatic;

	(* ::Subsubsection:: *)(* Utilities *)
	CORSHeaders = <|
		"Access-Control-Allow-Origin"->"*",
		"Access-Control-Allow-Methods"->"GET, POST, OPTIONS",
		"Access-Control-Allow-Headers"->"Origin, Content-Type, Accept"
	|>;
	
	generateCORSHTTPResponse[expr_,req_] := Module[{
			response = GenerateHTTPResponse[expr,req]
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

	(* ::Subsubsection:: *)(* Main *)
	Options[LocalDeploy] = {
		(* OverwriteTarget -> True, *) (* TODO *)
		HandlerFunctions -> <||>
	};
	LocalDeploy[
		expr_,
		port: portP : Automatic,
		OptionsPattern[]
	] := Module[{
			listener,server,url,endpoints,localObj, enclose,
			handlers = OptionValue[HandlerFunctions]
		},
		enclose = Enclose[
			(*
				Create listener socket
			*)
			listener =
				ConfirmMatch[#, SocketListener[__]]& @
				SocketListen[
					(*
						Open server socket and add to $LocalDeployments
					*)
					server = SocketOpen[port, "TCP"]
					,
					Function[{data},
						Module[{res,req,
								client = data["SourceSocket"]
							},

							WithCleanup[
								(*
									Import HTTPRequest
								*)
								req = ImportByteArray[data["DataByteArray"], "HTTPRequest"];

								(*
									HTTPResponseReceived handler
								*)
								Lookup[handlers, "HTTPResponseReceived", Identity][
									<|
										data,
										<|"HTTPRequest" -> req, "HTTPResponse" -> Missing[]|>
									|>
								];

								(*
									Handle OPTIONS request
								*)
								res = If[req["Method"] === "OPTIONS",
									HTTPResponse[
										"",
										<|"Headers" -> CORSHeaders|>
									],
									(*
										Handle actual request
									*)
									generateCORSHTTPResponse[expr, req]
								];
								(*
									HTTPResponseSent handler
								*)
								Lookup[handlers, "HTTPResponseSent", Identity][
									<|
										data,
										<|"HTTPRequest" -> req, "HTTPResponse" -> res|>
									|>
								];

								(*
									Write response to client socket
								*)
								SocketWriteMessage[ client, ExportByteArray[res, "HTTPResponse"] ];
								,
								(*
									Close client socket
								*)
								Close[client]
							]
						]
					]
				];

			(*
				Build base URL
			*)
			url = URLBuild[
				<|
					"Scheme" -> "http",
					"Domain" -> server["DestinationHostname"],
					"Port" 	 -> server["DestinationPort"]
				|>
			];

			(*
				Extract endpoints and Iconize their expressions for
				display in the LocalDeploymentObject
			*)
			endpoints = <|
				#[[0]][ #[[1]], Evaluate[Iconize[ #[[2]] ] ]]& /@
					If[MatchQ[expr, _URLDispatcher],
						First[expr],
						{"/" :> expr}
					]
			|>;

			(*
				Return
			*)
			localObj = LocalDeploymentObject[
				<|
					"Listener" 		 -> listener,
					"Socket" 		 -> server,
					"LocalIPAddress" -> server["DestinationHostname"],
					"Port" 			 -> server["DestinationPort"],
					"BaseURL" 		 -> url,
					"Endpoints" 	 -> endpoints
				|>
			];
			AppendTo[$LocalDeployments, localObj];
			localObj
		];

		If[!FailureQ[enclose],
			enclose,
			enclose["Expression"]
		]
	];

End[];
EndPackage[];
