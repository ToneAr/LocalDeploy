BeginPackage["TonyAristeidou`LocalDeploy`", {
	"GeneralUtilities`"
}];


SetUsage[
	LocalDeploy,
	"LocalDeploy[ expr$ ] deploys a socket listener on an available local which can take request and return a LocalDeploymentObject.
LocalDeploy[ api$, port$ ] deploys a socket listener on port$ which can take request and return a LocalDeploymentObject."
];

SetUsage[
	LocalDeploymentObject,
	StringRiffle[{
		"LocalDeploymentObject is an object representation of a local deployment.",
		"The following properties can be returned using LocalDeploymentObject[property$]:",
		"| Property | Description |",
		"| --- | --- |",
		"| \"Listener\" | Returns the deployment's SocketListener. |",
		"| \"Socket\" | Returns the server SocketObject. |",
		"| \"Hostname\" | Returns the IPAdress of the deployment. |",
		"| \"Port\" | Returns the port the deployment is listening on. |",
		"| \"BaseURL\" | Returns the url required for an HTTP request to the deployment. |",
		"| \"Endpoints\" | Returns an association of all endpoints deployed and their Iconized expressions |",
		"The deployment can be closed using Close[ $localDeployment ] or DeleteObject[ $localDeployment ]"
	}, "\n"]
];

SetUsage[
	LocalDeployments,
	"LocalDeployments[] returns a list of all active local deployments."
];

EndPackage[];
