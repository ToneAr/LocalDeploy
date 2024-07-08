BeginPackage["LocalDeploy`", {"GeneralUtilities`"}];

SetUsage[
	LocalDeploy,
	"LocalDeploy[ api$ ] deploys a socket listener on an available local which can take request and return a result.
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

LocalDeployment;
$LocalDeployments;

Begin["`Private`"];

If[!ValueQ[$LocalDeployments],
	$LocalDeployments = {}
];

ClearAll[LocalDeploy];
Options[LocalDeploy] = {
	"LogDirectory" -> FileNameJoin[{$HomeDirectory, "LocalDeploy-logs"}],
	"OverwriteTarget" -> True
};
LocalDeploy[api_APIFunction, port:_?NumericQ:Automatic, opts:OptionsPattern[]] := Enclose@Module[{
		listener,argStringToAssoc,extractRequiredArgs,
		extractArgsFromReqString,server,uuid,url
	},
	
	(* Helper Functions *)
	extractArgsFromReqString[str_String] := StringReplace[
		str,
		("GET"|"POST")~~" "~~"/"~~(""|"?")~~req___~~" "~~"HTTP/"~~NumberString ~~___ :> req
	];
	argStringToAssoc[str_String]:= <|
		Rule@@@(StringSplit[#,"="]&/@StringSplit[str, "&"])
	|>;
	extractRequiredArgs[apiFunc_APIFunction] := (
		Association@Cases[apiFunc[[1]],
			(r:Rule[key_String,value_String])
			|
			(a:Rule[key_String,value_Association?(Or[#["Required"], MissingQ[#["Required"]]]&)])
		]) /. {
			<||> :> <|"NullArgument" -> Null|>
	};
	
	(* Main *)
	server = SocketOpen[port, "TCP"];

	AppendTo[$LocalDeployments, server];

	listener = ConfirmMatch[#, SocketListener[__]]& @
		SocketListen[
			server, 
			Function[{data},
				WithCleanup[
					Module[{responseData,
							client = data["SourceSocket"],
							args = argStringToAssoc[ extractArgsFromReqString @ data["Data"] ],
							statusCode = "200 OK"
						},
						
						(* Validation *)
						If[(* Check required arguments are provided *)
							And[
								Not@*And @@
								KeyValueMap[{key,value} |-> (
										KeyExistsQ[extractRequiredArgs[api], key]
									),
									args /. {<||> :> <|"NullArgument" -> Null|>}
								]
							],
							statusCode = "400 Bad Request";
							api = ""&
						];
								
						(* Response *)
						responseData = ExportString[api[args], If[MissingQ[api[[3]]], "WXF", api[[3]]]];
						
						WriteLine[client,
							Echo @
							StringRiffle[
								(* HTTP Response *)
								{
									"HTTP/1.1 "<>statusCode,
									"Date: "<>DateString[]<>" "<>$TimeZoneEntity["ShortName"],
									"Content-Length: "<>StringLength[responseData],
									"Content-Type: application/vnd.wolfram.wxf",
									"",
									responseData
								},
								"\n"
							]
						]
					],
					Close[ data["SourceSocket"] ]
				]
			]
		];
	url = URLBuild[
			<|
				"Scheme" -> "http",
				"Domain" -> First[server["DestinationIPAddress"]],
				"Port" -> server["DestinationPort"]
			|>
		];
		
	(* Return *)
	Interpretation[
		Pane[LocalDeployment[
			Framed[TextGrid[{
					{
						Style["Local IP Address:", FontColor->Gray], server["DestinationIPAddress"][[1]],
						"",
						Style["Local port:", FontColor->Gray], server["DestinationPort"]
					}
				},
				BaseStyle->{FontSize->10}
				],
				RoundingRadius->3,
				FrameStyle->GrayLevel[0.30],
				Background->GrayLevel[0.15]
			]
		]]
		,
		<|
			"Listener" -> listener,
			"Socket" -> server,
			"API" -> api,
			"URL" -> url
		|>
	]
];

End[];
EndPackage[];
