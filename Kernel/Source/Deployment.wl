(* ::Section:: *)(* Dependencies & Context *)
BeginPackage["ToneAr`LocalDeploy`", {
	"ToneAr`LocalDeploy`",
	"ToneAr`LocalDeploy`Private`"
}];

Begin["`FileScope`Deploy`Private`"];


(* -------------------------------------------------------------------------- *)
(* ::Section:: *)(* submitQueueEvaluationScheduledTask *)
(* Description:  Submits a scheduled task to evaluate the message queue recursively.
 * Return:       _TaskObject
 *)
submitQueueEvaluationScheduledTask[
	responseQueue_DataStructure,
	evalFreq_Quantity,
	handlers_Association
] :=
	SessionSubmit @ ScheduledTask[
		(* Initiate parallel queue evaluation *)
		Parallel`Developer`QueueRun[];
		(* Handle top message in the queue *)
		If[responseQueue["Length"] > 0,
			Block[{client, resp, data,
					queueItem = Quiet[responseQueue["Pop"]]
				},
				data = First[queueItem, $Failed];
				client = data["SourceSocket"];
				resp = Last[queueItem, <||>];
				If[resp["State"] =!= "received",
					(* If task not finished, push to back of queue *)
					responseQueue["Push", queueItem]
				,(* Else *)
					(* Send HTTPResponse to client *)
					resp = ReleaseHold[resp["Result"]];
					ZeroMQLink`SocketWriteMessage[
						client,
						ExportByteArray[resp, "HTTPResponse"]
					];
					(* HTTPResponseSent handler *)
					Lookup[handlers, "HTTPResponseSent", Identity][
						<|
							data,
							<|
								"HTTPRequest" -> ImportByteArray[
									data["DataByteArray"],
									"HTTPRequest"
								];,
								"HTTPResponse" -> resp
							|>
						|>
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
	"HostAddress"         -> "localhost",
	"EvaluationInterval" -> Quantity[50, "Milliseconds"],
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
				OptionValue["EvaluationInterval"],
				handlers
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
				Repeated[(Rule|RuleDelayed)[_String, Except[_?(FailureQ)]]]
			|>,
			"Failed to generate endpoints"
		];
		(* Create LocalDeploymentObject *)
		ConfirmMatch[
			ldObj = LocalDeploymentObject[
				<|
					"Listener"    -> listener,
					"Socket"      -> server,
					"HostAddress" -> base,
					"HostPort"    -> port,
					"URL"         -> url,
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


(* ::Section:: *)(* End *)
End[];
EndPackage[];
