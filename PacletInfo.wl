(* ::Package:: *)

PacletObject[
  <|
    "Name" -> "TonyAristeidou/LocalDeploy",
    "Description" -> "Locally deploy TCP socket listeners able to emulate the Wolfram Cloud",
    "Creator" -> "Antonis Aristeidou",
    "License" -> "MIT",
    "PublisherID" -> "TonyAristeidou",
    "Version" -> "1.1.0",
    "WolframVersion" -> "12.2+",
    "PrimaryContext" -> "TonyAristeidou`LocalDeploy`",
    "Dependencies" -> {
			"KirillBelov/CSockets" -> "=>1.0.24"
		},
    "Extensions" -> {
      {
        "Kernel",
        "Root" -> "Kernel",
        "Context" -> {
          {
            "TonyAristeidou`LocalDeploy`",
            "LocalDeploy.wl"
          }
        },
        "Symbols" -> {
          "TonyAristeidou`LocalDeploy`LocalDeploy",
          "TonyAristeidou`LocalDeploy`LocalDeploymentObject"
        }
      },
      {"Documentation", "Language" -> "English"}
    }
  |>
]
