
TestCreate[
	PacletDirectoryLoad @ ParentDirectory[DirectoryName[$TestFileName], 2];
	<<ToneAr`LocalDeploy`,
	Null,
	TestID -> "LocalDeploy-init"
]

TestCreate[
	LocalDeployments[],
	{},
	TestID -> "LocalDeploy-LocalDeployments-empty"
]

TestCreate[
	dep = LocalDeploy[ RandomInteger[] ],
	_LocalDeploymentObject,
	TestID -> "LocalDeploy-LocalDeploy-createStatic"
]
