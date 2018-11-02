namespace FilenameEmbeddedMetadataOrganizer

module Interop =
    open System
    open System.Runtime.InteropServices

    // see https://www.pinvoke.net/default.aspx/shell32/ShellExecuteEx.html
    [<StructLayout(LayoutKind.Sequential)>]
    [<Struct>]
    type SHELLEXECUTEINFO =
        val mutable cbSize : int
        val mutable fMask : uint32
        val mutable hwnd : IntPtr
        [<MarshalAs(UnmanagedType.LPTStr)>]
        val mutable lpVerb : string
        [<MarshalAs(UnmanagedType.LPTStr)>]
        val mutable lpFile : string
        [<MarshalAs(UnmanagedType.LPTStr)>]
        val mutable lpParameters : string
        [<MarshalAs(UnmanagedType.LPTStr)>]
        val mutable lpDirectory : string
        val mutable nShow : int
        val mutable hInstApp : IntPtr
        val mutable lpIDList : IntPtr
        [<MarshalAs(UnmanagedType.LPTStr)>]
        val mutable lpClass : string
        val mutable hkeyClass : IntPtr
        val mutable dwHotKey : uint32
        val mutable hIcon : IntPtr
        val mutable hProcess : IntPtr

    [<DllImport("shell32.dll", CharSet = CharSet.Auto)>]
    extern bool ShellExecuteEx(SHELLEXECUTEINFO& lpExecInfo)

    type ShowCommands =
        | SW_HIDE = 0
        | SW_SHOWNORMAL = 1
        | SW_NORMAL = 1
        | SW_SHOWMINIMIZED = 2
        | SW_SHOWMAXIMIZED = 3
        | SW_MAXIMIZE = 3
        | SW_SHOWNOACTIVATE = 4
        | SW_SHOW = 5
        | SW_MINIMIZE = 6
        | SW_SHOWMINNOACTIVE = 7
        | SW_SHOWNA = 8
        | SW_RESTORE = 9
        | SW_SHOWDEFAULT = 10
        | SW_FORCEMINIMIZE = 11
        | SW_MAX = 1

    [<Flags>]
    type ShellExecuteMaskFlags =
        | SEE_MASK_DEFAULT = 0x00000000u
        | SEE_MASK_CLASSNAME = 0x00000001u
        | SEE_MASK_CLASSKEY = 0x00000003u
        | SEE_MASK_IDLIST = 0x00000004u
        | SEE_MASK_INVOKEIDLIST = 0x0000000cu // Note SEE_MASK_INVOKEIDLIST(0xC) implies SEE_MASK_IDLIST(0x04)
        | SEE_MASK_HOTKEY = 0x00000020u
        | SEE_MASK_NOCLOSEPROCESS = 0x00000040u
        | SEE_MASK_CONNECTNETDRV = 0x00000080u
        | SEE_MASK_NOASYNC = 0x00000100u
        | SEE_MASK_FLAG_DDEWAIT = 0x00000100u
        | SEE_MASK_DOENVSUBST = 0x00000200u
        | SEE_MASK_FLAG_NO_UI = 0x00000400u
        | SEE_MASK_UNICODE = 0x00004000u
        | SEE_MASK_NO_CONSOLE = 0x00008000u
        | SEE_MASK_ASYNCOK = 0x00100000u
        | SEE_MASK_HMONITOR = 0x00200000u
        | SEE_MASK_NOZONECHECKS = 0x00800000u
        | SEE_MASK_NOQUERYCLASSSTORE = 0x01000000u
        | SEE_MASK_WAITFORINPUTIDLE = 0x02000000u
        | SEE_MASK_FLAG_LOG_USAGE = 0x04000000u

    let showFileProperties fileName =
        let mutable info = SHELLEXECUTEINFO()
        info.cbSize <- Marshal.SizeOf(info)
        info.lpVerb <- "properties"
        info.lpFile <- fileName
        info.nShow <- int ShowCommands.SW_SHOW
        info.fMask <- uint32 ShellExecuteMaskFlags.SEE_MASK_INVOKEIDLIST
        ShellExecuteEx(&info);

    [<DllImport("shlwapi.dll", CharSet = CharSet.Unicode, ExactSpelling = true)>]
    extern int StrCmpLogicalW(String x, String y)
