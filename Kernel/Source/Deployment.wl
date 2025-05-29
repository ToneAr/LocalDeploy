BeginPackage["TonyAristeidou`LocalDeploy`", {
	"TonyAristeidou`LocalDeploy`",
	"TonyAristeidou`LocalDeploy`Private`"
}];

Begin["`FileScope`Deploy`Private`"];

(* -------------------------------------------------------------------------- *)
(* ::Section:: *)(* handleClient *)
(* Description:  Creates the callback called on client connection to the socket
 *               listener.
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
			(* HTTPResponseSent handler *)
			(* Lookup[handlers, "HTTPResponseSent", Identity][
				<|
					data,
					<|"HTTPRequest" -> req, "HTTPResponse" -> res|>
				|>
			]; *)
		]
	]
];


(* -------------------------------------------------------------------------- *)
(* ::Section:: *)(* submitQueueEvaluationScheduledTask *)
(* Description:  Submits a scheduled task to evaluate the message queue recursively.
 * Return:       _TaskObject
 *)
submitQueueEvaluationScheduledTask[
	responseQueue_DataStructure,
	evalFreq_Quantity
] := SessionSubmit @ ScheduledTask[
	(* Initiate parallel queue evaluation *)
	Parallel`Developer`QueueRun[];
	(* Handle top message in the queue *)
	If[responseQueue["Length"] > 0,
		Block[{client, resp,
				queueItem = Quiet[responseQueue["Pop"]]
			},
			client = First[queueItem, $Failed];
			resp = Last[queueItem, <||>];
			If[resp["State"] =!= "received",
				(* If task not finished, push to back of queue *)
				responseQueue["Push", queueItem]
			,(* Else *)
				ZeroMQLink`SocketWriteMessage[
					client,
					ExportByteArray[ReleaseHold[resp["Result"]], "HTTPResponse"]
				];
				Close @ client;
			]

		]
	],
	evalFreq
]


(* -------------------------------------------------------------------------- *)
(* ::Section:: *)(* LocalDeployments *)
(* Description:  Returns a list of all active local deployments.
 * Return:       {___LocalDeploymentObject}
 *)
LocalDeployments[] := $localDeployments["Values"];


(* -------------------------------------------------------------------------- *)
(* ::Section:: *)(* LocalDeploy *)
(* Description:  LocalDeploy is a function that deploys a local async HTTP server
 * Return:       _LocalDeploymentObject | _Failure
 *)
LocalDeploy // Options = {
	OverwriteTarget       -> True,
	"HostAddress"         -> "127.0.0.1",
	"EvaluationFrequency" -> Quantity[50, "Milliseconds"],
	HandlerFunctions      -> <||>,
	"LaunchKernels"       -> True,
	"InitialKernelCount"  -> Min[$ProcessorCount * 2, 12]
};
LocalDeploy[expr_, port: portP : Automatic, OptionsPattern[]] := Module[{
		listener,server,url,endpoints, enclose, task, ldObj,
		responseQueue = CreateDataStructure["Queue"],
		base = OptionValue["HostAddress"],
		handlers = OptionValue[HandlerFunctions]
	},
	enclose = Enclose[
		(* Remove socket bound to 'port' if it exists *)
		If[OptionValue[OverwriteTarget] && port =!= Automatic,
			removeLocalDeploymentIfExists[base, port]
		];
		(* Bind to socket and deploy TCP listener  *)
		listener =
			ConfirmMatch[#, _SocketListener, "Failed to create SocketListener"]& @
			SocketListen[
				(* Open server socket *)
				server = SocketOpen[{base, port}, "TCP"],
				(* Create callback function called on client connection *)
				handleClient[expr, responseQueue, handlers]
			];
		(* Launch parallel kernels if needed *)
		With[{kCount = Length[Kernels[]]},
			If[ And[
					OptionValue["LaunchKernels"],
					kCount < OptionValue["InitialKernelCount"]
				],
				LaunchKernels[OptionValue["InitialKernelCount"] - kCount]
			]
		];
		(* Deploy queue evaluation loop task *)
		task = ConfirmMatch[
			submitQueueEvaluationScheduledTask[
				responseQueue,
				OptionValue["EvaluationFrequency"]
			],
			_TaskObject,
			"Failed to create evaluation task"
		];
		(* Build base URL *)
		url = ConfirmMatch[
			URLBuild[
				<|
					"Scheme" -> "http",
					"Domain" -> server["DestinationHostname"],
					"Port" 	 -> server["DestinationPort"]
				|>
			],
			_String,
			"Failed to generate base URL"
		];
		(*
		 * Extract endpoints and Iconize their expressions for display in
		 * the LocalDeploymentObject
		 *)
		endpoints = ConfirmMatch[
			<|
				#[[0]][ #[[1]], Evaluate[Iconize[ #[[2]] ] ]]& /@
					If[MatchQ[expr, _URLDispatcher],
						First[expr],
						{"/" :> expr}
					]
			|>,
			<|
				Repeated[_String -> Except[_?FailureQ]]
			|>,
			"Failed to generate endpoints"
		];
		(* Create LocalDeploymentObject *)
		ConfirmMatch[
			ldObj = LocalDeploymentObject[
				<|
					"Listener"    -> listener,
					"Socket"      -> server,
					"HostAddress" -> server["DestinationHostname"],
					"Port"        -> server["DestinationPort"],
					"BaseURL"     -> url,
					"Endpoints"   -> endpoints,
					"EvaluationQueueTask" -> task,
					"ResponseQueue"       -> responseQueue
				|>
			],
			_LocalDeploymentObject,
			"Failed to create LocalDeploymentObject"
		];
		(* Insert into global deployments table *)
		$localDeployments["Insert", {base, port} -> ldObj];
		(* Return LocalDeploymentObject *)
		ldObj
		,
		(* OnError *)
		Function[e,
			Quiet[
				TaskRemove[task];
				Close[server]
			];
			e
		]
	]
];

End[];
EndPackage[];
