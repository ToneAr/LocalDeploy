BeginPackage["TonyAristeidou`LocalDeploy`Private`"];

$localDeployments::usage = "$localDeployments is a hash table that stores LocalDeploymentObject associations.";
$icon::usage = "$icon is the icon graphic used for LocalDeploymentObject.";
localDeploymentQ::usage = "localDeploymentQ is a function that checks if an association is a valid LocalDeploymentObject.";
portP::usage = "portP is a pattern that matches a numeric port or Automatic.";
CORSHeaders::usage = "CORSHeaders is an association of headers for CORS support.";
generateCORSHTTPResponse::usage = "generateCORSHTTPResponse[expr_, req_] generates an HTTP response with CORS headers.";
removeLocalDeploymentIfExists::usage = "removeLocalDeploymentIfExists[base_String, port_Integer] removes a local deployment bound to the specified base and port if it exists.";

EndPackage[];
