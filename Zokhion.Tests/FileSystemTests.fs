namespace TeaDriven.Zokhion.Tests

open Xunit

open TeaDriven.Zokhion.FileSystem

module FileSystemTests =
    [<Theory>]
    [<InlineData(@"\\server\root\subdirectory1", @"\\server\root\subdirectory2")>]
    [<InlineData(@"C:\root\subdirectory1", @"C:\root\subdirectory2")>]
    let ``Non-subdirectory is detected correctly`` (baseDirectory, subDirectory) =
        // Act
        let result = Path.isSubDirectoryOf baseDirectory subDirectory

        // Assert
        Assert.False (result)

    [<Theory>]
    [<InlineData(@"\\server\root\subdirectory1", @"\\server\root\subdirectory1")>]
    [<InlineData(@"C:\server\root\subdirectory1", @"C:\server\root\subdirectory1")>]
    let ``Subdirectory is detected correctly`` (baseDirectory, subDirectory) =
        // Act
        let result = Path.isSubDirectoryOf baseDirectory subDirectory

        // Assert
        Assert.True (result)
