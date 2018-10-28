System.IO.Directory.SetCurrentDirectory __SOURCE_DIRECTORY__

#r "paket:
nuget Fake.Core.Target
nuget Fake.DotNet.Cli
nuget Fake.DotNet.MSBuild
nuget Fake.DotNet.Paket
nuget Fake.DotNet.Testing.XUnit2
nuget Fake.IO.FileSystem
//"

#load ".fake/build.fsx/intellisense.fsx"

open Fake.Core
open Fake.Core.TargetOperators
open Fake.DotNet
open Fake.DotNet.Testing
open Fake.IO
open Fake.IO.Globbing.Operators

let solutionFile  = "FilenameEmbeddedMetadataOrganizer.sln"

let gitOwner = "TeaDrivenDev"
let gitHome = "https://github.com/" + gitOwner
let gitName = "FilenameEmbeddedMetadataOrganizer"
let gitRaw = Environment.environVarOrDefault "gitRaw" ("https://raw.github.com/" + gitOwner)

let outputDirectory = "bin"

let configuration = Environment.environVarOrDefault "Configuration" "Release"

Target.create "Restore" (fun _ -> Paket.restore id)

Target.create "Clean" (fun _ -> Shell.cleanDirs [ outputDirectory ])

Target.create "Build" (fun _ ->
    !! solutionFile
    |> MSBuild.run id "" "Rebuild" [ "Configuration", configuration ]
    |> ignore)

Target.create "RunTests" (fun _ ->
    !! (sprintf "**/bin/%s/**/*.Tests.dll" configuration)
    |> XUnit2.run id)

Target.create "All" ignore

"Restore"
==> "Build"
==> "RunTests"
==> "All"

Target.runOrDefault "All"

printfn "Finished %s" (System.DateTime.Now.ToString "HH:mm:ss")
