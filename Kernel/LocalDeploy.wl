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

$LocalDeployments = <||>;

keys = {"Listener","Socket","LocalIPAddress","Port","BaseURL","Endpoints"(*,"Hyperlinks"*)};
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

ClearAll[LocalDeploy];
Options[LocalDeploy] = {
	"LogDirectory" -> FileNameJoin[{$HomeDirectory, "LocalDeploy-logs"}],
	OverwriteTarget -> True,
	"Log" -> False,
	"LogFunction" -> logFunction,
	HandlerFunctions -> <||>
};
portP = _?NumericQ|Automatic;
LocalDeploy[
	expr_,
	port: portP : Automatic,
	OptionsPattern[]
] := Enclose@Module[{
		listener,server,url,endpoints,
		handlers = OptionValue[HandlerFunctions],
		log = If[OptionValue["Log"],
			OptionValue["LogFunction"][ ## ]&,
			Identity[#1]&
		]
	},

	(*
		Create listener socket
	*)
	listener = ConfirmMatch[#, SocketListener[__]]& @
		SocketListen[
			(*
				Open server socket and add to $LocalDeployments
			*)
			server = SocketOpen[port, "TCP"];
			AppendTo[$LocalDeployments, port -> server];
			server
			,
			Function[{data},
				Module[{res,req,
						client = data["SourceSocket"]
					},
					
					(*
						Print incoming request to console
					*)
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
								<|"HTTPRequest" -> req, "HTTPResponse" -> Missing[]|>
							|>
						];
						
						(*
							Print outgoing response to console
						*)
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
	LocalDeploymentObject[
		<|
			"Listener" 		 -> listener,
			"Socket" 		 -> server,
			"LocalIPAddress" -> server["DestinationHostname"],
			"Port" 			 -> server["DestinationPort"],
			"BaseURL" 		 -> url,
			"Endpoints" 	 -> endpoints
		|>
	]
];


LocalDeploymentObject /: Close[socket_SocketObject] /; MemberQ[Values[$LocalDeployments], socket] :=
	KeyDrop[$LocalDeployments, socket["DestinationPost"]];
LocalDeploymentObject /: DeleteObject[socket_SocketListener] /; MemberQ[Values[$LocalDeployments], socket["Socket"]] :=
	KeyDrop[$LocalDeployments, socket["Socket"]["DestinationPost"]];

LocalDeploymentObject /: (Close|DeleteObject)[LocalDeploymentObject[assoc_Association]] :=
	Close[assoc["Socket"]];



End[];
EndPackage[];
