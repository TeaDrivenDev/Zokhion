System.IO.Directory.SetCurrentDirectory __SOURCE_DIRECTORY__

#r @"packages/build/FAKE/tools/FakeLib.dll"

open System
open System.IO

open Fake
open Fake.Git
open Fake.Testing

let solutionFile  = "FilenameEmbeddedMetadataOrganizer.sln"

let gitOwner = "TeaDrivenDev"
let gitHome = "https://github.com/" + gitOwner
let gitName = "FilenameEmbeddedMetadataOrganizer"
let gitRaw = environVarOrDefault "gitRaw" ("https://raw.github.com/" + gitOwner)

let outputDirectory = "bin"

Target "Clean" (fun _ -> CleanDirs [ outputDirectory ])

Target "Build" (fun _ ->
    !! solutionFile
    |> MSBuildRelease "" "Rebuild"
    |> ignore)

Target "RunTests" (fun _ ->
    !! "**/bin/Release/**/*.Tests.dll"
    |> xUnit2 id)

Target "All" DoNothing

"Build"
==> "RunTests"
==> "All"

RunTargetOrDefault "All"