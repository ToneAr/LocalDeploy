BeginPackage["LocalDeploy`", {"GeneralUtilities`","ZeroMQLink`"}];

SetUsage[
	LocalDeploy,
	"LocalDeploy[ expr$ ] deploys a socket listener on an available local which can take request and return a result.
LocalDeploy[ api$, port$ ] deploys a socket listener on port$ which can take request and return a result."
];
SetUsage[
	LocalDeploymentFind,
	"LocalDeploymentFind[] returns all active LocalDeployments
LocalDeploymentFind[ uuid$ ] returns LocalDeployment matching uuid$"
];
SetUsage[
	LocalDeploymentClose,
	"LocalDeploymentClose[ localDeployment$ ] will close all sockets associated with localDeployment$"
];

LocalDeploymentObject;
$LocalDeployments;

Begin["`Private`"];

If[!ValueQ[$LocalDeployments],
	$LocalDeployments = {}
];

keys = {"Name","Listener","Socket","LocalIPAddress","Port","BaseURL","Endpoints"};
localDeploymentQ[in_] := AssociationQ[in] && AllTrue[keys, KeyExistsQ[in,#]&];
$icon = Graphics[Import[PacletObject["LocalDeploy"]["AssetLocation", "Icon"]],
		ImageSize -> Dynamic[{ 
			Automatic,
			3.5 * CurrentValue["FontCapHeight"] / AbsoluteCurrentValue[Magnification]
		}]
	];

LocalDeploymentObject /: MakeBoxes[obj : LocalDeploymentObject[asc_ ? localDeploymentQ], form : (StandardForm | TraditionalForm)] := 
    Module[{above, below},
        
		above = {
        	{BoxForm`SummaryItem[{"Local IP Address: ", asc["LocalIPAddress"]}]},
        	{BoxForm`SummaryItem[{"Local Port: ", asc["Port"]}]}
        };
        
		below = { 
			BoxForm`SummaryItem[{"Socket: ", asc["Socket"]}],
			BoxForm`SummaryItem[{"Listener: ", asc["Listener"]}],
			BoxForm`SummaryItem[{"Endpoints: ", asc["Endpoints"]}]
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
LocalDeploymentObject[asc_?localDeploymentQ][prop_] :=
	Lookup[asc, prop]

logFunction = {expr,label} |->
	Module[{
			cache = Import[
				FileNameJoin[{
					OptionValue["LogDirectory"],
					OptionValue["LogName"]<>".wl"
				}]
			]
		},
		Export[
			FileNameJoin[{
				OptionValue["LogDirectory"],
				OptionValue["LogName"]<>".wl"
			}],
			<|
				cache,
				<|
					"timestamp"->Now,
					"lbl"->label,
					"expr"->expr
				|>
			|>,
			OverwriteTarget -> True
		];
		
	];
generateCORSHTTPResponse[expr_,req_]:=Module[{
  		response = GenerateHTTPResponse[expr,req]
	},
	Print[HTTPResponse[
		response["Body"],
		<|
			"Headers" -> <|
				<|response["Headers"]|>,
				"Access-Control-Allow-Origin"->"*",
				"Access-Control-Allow-Methods"->"GET, POST, OPTIONS",
				"Access-Control-Allow-Headers"->"Origin, Content-Type, Accept"
			|>
		|>
	]["Headers"]];
	HTTPResponse[
		response["Body"],
		<|
			"Headers" -> <|
				<|response["Headers"]|>,
				"Access-Control-Allow-Origin"->"*",
				"Access-Control-Allow-Methods"->"GET, POST, OPTIONS",
				"Access-Control-Allow-Headers"->"Origin, Content-Type, Accept"
			|>
		|>
	]
];

ClearAll[LocalDeploy];
Options[LocalDeploy] = {
	"LogDirectory" -> FileNameJoin[{$HomeDirectory, "LocalDeploy-logs"}],
	"OverwriteTarget" -> True,
	"Log" -> False,
	"LogFunction" -> logFunction,
	HandlerFunctions -> <||>
};
LocalDeploy[expr_, arg_, opts:OptionsPattern[]] := 
	LocalDeploy[expr, {arg}, opts];
LocalDeploy[expr_, {name_String : CreateUUID[], port:(_?NumericQ|Automatic) : Automatic}, opts:OptionsPattern[]] := Enclose@Module[{
		listener,server,url,endpoints,
		handlers = OptionValue[HandlerFunctions],
		log = If[OptionValue["Log"],
			OptionValue["LogFunction"][ ## ]&,
			Identity[#1]&
		]
	},

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

	(* AppendTo[$LocalDeployments, server]; *)

	(*
		Main
	*)
	listener = ConfirmMatch[#, SocketListener[__]]& @
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
					Print["HTTPRequest:", "\n", data["Data"], "\n"];
					WithCleanup[
						(*
							Import HTTPRequest
						*)
						req = ImportByteArray[data["DataByteArray"], "HTTPRequest"];
						
						(*
							HTTPResponseSent handler
						*)
						Lookup[handlers, "HTTPResponseSent", Identity][
							<|
								data,
								<|"HTTPRequest" -> req, "HTTPResponse" -> Missing[]|>
							|>
						];
						
						(*
							Handle OPTIONS request
						*)
						res = If[req["Method"] === "OPTIONS",
							Print[HTTPResponse[
								"",
								<|
									"Headers" -> <|
										"Access-Control-Allow-Origin"->"*",
										"Access-Control-Allow-Methods"->"GET, POST, OPTIONS",
										"Access-Control-Allow-Headers"->"Origin, Content-Type, Accept"
									|>
								|>
							]["Headers"]];
							HTTPResponse[
								"",
								<|
									"Headers" -> <|
										"Access-Control-Allow-Origin"->"*",
										"Access-Control-Allow-Methods"->"GET, POST, OPTIONS",
										"Access-Control-Allow-Headers"->"Origin, Content-Type, Accept"
									|>
								|>
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
								<|"HTTPRequest" -> req, "HTTPResponse" -> Missing[]|>
							|>
						];

						Print["Response Headers: ", "\n", res["Headers"], "\n"];
						
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
				];
			]
			
		];
	
	(*
		Build base URL
	*)
	url = URLBuild[
		<|
			"Scheme" -> "http",
			"Domain" -> server["DestinationHostname"],
			"Port" -> server["DestinationPort"]
		|>
	];
		
	(*
		Return
	*)
	LocalDeploymentObject[
		<|
			"Name"->name,
			"Listener" -> listener,
			"Socket" -> server,
			"LocalIPAddress" -> server["DestinationHostname"],
			"Port" -> server["DestinationPort"],
			"BaseURL" -> url,
			"Endpoints" -> endpoints
		|>
	]
];

End[];
EndPackage[];
