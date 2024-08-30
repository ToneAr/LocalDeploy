(* ::Package:: *)

BeginPackage["TonyAristeidou`LocalDeploy`", {
	"GeneralUtilities`",
	"ZeroMQLink`"
}];

SetUsage[
	LocalDeploy,
	"LocalDeploy[ expr$ ] deploys a socket listener on an available local which can take request and return a LocalDeploymentObject.
LocalDeploy[ api$, port$ ] deploys a socket listener on port$ which can take request and return a LocalDeploymentObject."
];
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
	localDeploymentQ := ({asc} |-> AllTrue[keys, KeyExistsQ[asc, #]&]);

	(* ::Subsubsection:: *)(* Utilities *)
	keys = {
		"Listener",
		"Socket",
		"LocalIPAddress",
		"Port",
		"BaseURL",
		"Endpoints"
	};
	$icon = Graphics[Graphics[GeometricTransformation[{Thickness[0.], FilledCurve[{{{1, 4, 3}, {0, 1, 0}, {1, 3, 3}, {0, 1, 0}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {0, 1, 0}, {1, 3, 3}, {0, 1, 0}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {0, 1, 0}, {1, 3, 3}, {0, 1, 0}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {0, 1, 0}, {1, 3, 3}, {0, 1, 0}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}}, {{1, 4, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}}, {{1, 4, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}}, {{1, 4, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}}, {{1, 4, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}}, {{1, 4, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}}}, {CompressedData["
1:eJyFlDFLAzEUx0M/gC4dSkE4WxE6tKBnr9XKcbNOraMdSmknByfB8WY3F6Wd
ih/Az+Ds6upwOAmdu4qXl/xzL+mFBo7HP/fy8t4vLzmc3o/mFSHEMP+kVeMi
UTaSdnV6fS5t8KP0pL7s0//bMNJ+pNP1M+mPze+ZtNnwMST/cKridGYnln7r
tbl/MN5v8Xii9d2w9rtaNHk+q8FfM7HzPaI4T91LrvMRS5tU7wKu81HjOnu/
2XPWCyc+6ezhs+f8j/h6aMQHt7T7Vad5zW3y8npA85qbdKU4mlta7RxTneC2
GKj6NTdocIM/uCEeuGE/cDP5FJxqXKMexk04fsKpW+yYt9YhLvMrzQN5Im/U
gbpMnbpucAAXHzdwhT+4Ix7Oxeynzw354FxZvr4+8PWN1VfoO7a+tE9L+tjX
9xQf3HBvTD/gXhX9QhrccC8NN889Bjf4s3623gnsB24mn4KT9e4YHgW32PGL
nbrjHfPWOsRlftt5/APV7MIf
"], {{26., 4.}, {27.104570388793945`, 4.}, {28., 4.895430564880371}, {28., 6.}, {28., 7.104569435119629}, {27.104570388793945`, 8.}, {26., 8.}, {24.895429611206055`, 8.}, {24., 7.104569435119629}, {24., 6.}, {24., 4.895430564880371}, {24.895429611206055`, 4.}, {26., 4.}}, {{4., 6.}, {4., 4.895430564880371}, {4.895430564880371, 4.}, {6., 4.}, {7.104569435119629, 4.}, {8., 4.895430564880371}, {8., 6.}, {8., 7.104569435119629}, {7.104569435119629, 8.}, {6., 8.}, {4.895430564880371, 8.}, {4., 7.104569435119629}, {4., 6.}}, {{6., 28.}, {4.895430564880371, 28.}, {4., 27.104570388793945`}, {4., 26.}, {4., 24.895429611206055`}, {4.895430564880371, 24.}, {6., 24.}, {7.104569435119629, 24.}, {8., 24.895429611206055`}, {8., 26.}, {8., 27.104570388793945`}, {7.104569435119629, 28.}, {6., 28.}}, {{16., 20.}, {13.790861129760742`, 20.}, {12., 18.209138870239258`}, {12., 16.}, {12., 13.790861129760742`}, {13.790861129760742`, 12.}, {16., 12.}, {18.209138870239258`, 12.}, {20., 13.790861129760742`}, {20., 16.}, {20., 18.209138870239258`}, {18.209138870239258`, 20.}, {16., 20.}}, {{26., 28.}, {24.895429611206055`, 28.}, {24., 27.104570388793945`}, {24., 26.}, {24., 24.895429611206055`}, {24.895429611206055`, 24.}, {26., 24.}, {27.104570388793945`, 24.}, {28., 24.895429611206055`}, {28., 26.}, {28., 27.104570388793945`}, {27.104570388793945`, 28.}, {26., 28.}}}]}, {{{1, 0}, {0, -1}}, {0, 0}}], AspectRatio -> Automatic, ImageSize -> {32., 32.}, PlotRange -> {{0., 32.}, {0., -32.}}],
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
		obj:( LocalDeploymentObject[asc: _Association?localDeploymentQ]),
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
		dep:LocalDeploymentObject[assoc : _Association?localDeploymentQ]
	] := ( 
		Close[assoc["Socket"]]
	);
	
	LocalDeploymentObject[asc: _Association?localDeploymentQ][prop_] :=
		Lookup[asc, prop];
	LocalDeploymentObject[asc: _Association?localDeploymentQ]["Properties"] :=
		keys
		
	LocalDeploymentObject /:( 
		URLExecute[
			LocalDeploymentObject[asc: _Association?localDeploymentQ],
			 rest___
		]
	):=( 
		URLExecute[asc["BaseURL"], rest]
	);
	
	LocalDeploymentObject/:Normal[obj:LocalDeploymentObject[asc: _Association?localDeploymentQ]]:=asc;

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
						Open server socket 
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
								ZeroMQLink`SocketWriteMessage[ client, ExportByteArray[res, "HTTPResponse"] ];
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
			localObj
		];

		If[!FailureQ[enclose],
			enclose,
			enclose["Expression"]
		]
	];


End[];
EndPackage[];
