namespace TeaDriven.Zokhion.FileSystem

open System.IO

module Utilities =

    let inline getDirectoryName path = Path.GetDirectoryName path

    let inline getFileNameWithoutExtension path =
        Path.GetFileNameWithoutExtension path