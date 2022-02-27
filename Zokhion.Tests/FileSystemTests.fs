namespace TeaDriven.Zokhion.Tests

open Xunit

open TeaDriven.Zokhion.FileSystem

module FileSystemTests =
    [<Theory>]
    [<InlineData(@"\\server\root\subdirectory1", @"\\server\root\subdirectory2")>]
    [<InlineData(@"\\server\root\subdirectory", @"\\server\root\subdirectory2")>]
    [<InlineData(@"C:\root\subdirectory1", @"C:\root\subdirectory2")>]
    [<InlineData(@"C:\root\subdirectory", @"C:\root\subdirectory2")>]
    [<InlineData(@"C:\root\subdirectory", @"C:\root\subdirectory2\")>]
    [<InlineData(@"C:\root\subdirectory\aaa", @"C:\root\subdirectory\")>]
    let ``Non-subdirectory is detected correctly`` (baseDirectory, subDirectory) =
        // Act
        let result = Path.isSubDirectoryOf baseDirectory subDirectory

        // Assert
        Assert.False (result)

    [<Theory>]
    [<InlineData(@"\\server\root\subdirectory1", @"\\server\root\subdirectory1")>]
    [<InlineData(@"C:\server\root\subdirectory1", @"C:\server\root\subdirectory1")>]
    [<InlineData(@"C:\server\root\subdirectory", @"C:\server\root\subdirectory\")>]
    [<InlineData(@"C:\server\root\subdirectory\", @"C:\server\root\subdirectory")>]
    [<InlineData(@"C:\server\root\subdirectory\", @"C:\server\root\subdirectory\aaaa")>]
    let ``Subdirectory is detected correctly`` (baseDirectory, subDirectory) =
        // Act
        let result = Path.isSubDirectoryOf baseDirectory subDirectory

        // Assert
        Assert.True (result)
