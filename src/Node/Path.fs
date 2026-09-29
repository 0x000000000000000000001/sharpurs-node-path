// Node.Path FFI for the POSIX targets used by b8x (Linux containers and macOS
// hosts), mirroring the node:path operations reached by the application.

let private pathSep = "/"

let private normalizePath (path: string) : string =
    if path = "" then "."
    else
        let isAbsolute = path.StartsWith(pathSep)
        let segments = path.Split('/')
        let stack = System.Collections.Generic.List<string>()

        for segment in segments do
            match segment with
            | "" | "." -> ()
            | ".." ->
                if stack.Count > 0 && stack.[stack.Count - 1] <> ".." then
                    stack.RemoveAt(stack.Count - 1)
                elif not isAbsolute then
                    stack.Add ".."
            | value -> stack.Add value

        let body = System.String.Join(pathSep, stack)
        let keepsTrailing = path.EndsWith("/.") || path.EndsWith("/..") || (path.EndsWith("/") && stack.Count > 0)

        if body = "" then
            if isAbsolute then pathSep else "."
        else
            (if isAbsolute then pathSep else "") + body + (if keepsTrailing then pathSep else "")

let normalize (path: obj) : obj =
    box (normalizePath (unbox<string> path))

let concat (segments: obj) : obj =
    let parts =
        unbox<obj[]> segments
        |> Array.map unbox<string>
        |> Array.filter (fun segment -> segment <> "")
    box (normalizePath (System.String.Join(pathSep, parts)))

let resolve (fromSegments: obj) (toPath: obj) : obj =
    box (fun (_: obj) ->
        let segments =
            Array.append
                (unbox<obj[]> fromSegments |> Array.map unbox<string>)
                [| unbox<string> toPath |]

        let mutable result = ""
        let mutable index = segments.Length - 1
        let mutable absolute = false

        while index >= 0 && not absolute do
            let segment = segments.[index]
            if segment <> "" then
                result <- if result = "" then segment else segment + pathSep + result
                absolute <- segment.StartsWith(pathSep)
            index <- index - 1

        if not absolute then
            result <- System.IO.Directory.GetCurrentDirectory() + pathSep + result

        box (normalizePath result))

let relative (fromPath: obj) (toPath: obj) : obj =
    box (System.IO.Path.GetRelativePath(System.IO.Path.GetFullPath(unbox<string> fromPath), System.IO.Path.GetFullPath(unbox<string> toPath)))

let dirname (value: obj) : obj =
    let path = unbox<string> value

    if path = "" then
        box "."
    else
        let mutable normalized = normalizePath path
        if normalized.Length > 1 && normalized.EndsWith(pathSep) then
            normalized <- normalized.TrimEnd('/')

        let index = normalized.LastIndexOf('/')
        if index < 0 then box "."
        elif index = 0 then box pathSep
        else box (normalized.Substring(0, index))

let basename (value: obj) : obj =
    let path = unbox<string> value
    let mutable trimmed = path
    if trimmed.Length > 1 && trimmed.EndsWith(pathSep) then
        trimmed <- trimmed.TrimEnd('/')
    let index = trimmed.LastIndexOf('/')
    box (if index < 0 then trimmed else trimmed.Substring(index + 1))

let basenameWithoutExt (value: obj) (extension: obj) : obj =
    let baseName = unbox<string> (basename value)
    let ext = unbox<string> extension
    if ext <> "" && baseName.EndsWith(ext) then
        box (baseName.Substring(0, baseName.Length - ext.Length))
    else
        box baseName

let extname (value: obj) : obj =
    let baseName = unbox<string> (basename value)
    let index = baseName.LastIndexOf('.')
    if index <= 0 then box "" else box (baseName.Substring(index))

let sep : obj = box pathSep

let delimiter : obj = box ":"

let parse (value: obj) : obj =
    let path = unbox<string> value
    let baseName = unbox<string> (basename value)
    let extension = unbox<string> (extname value)
    let name =
        if extension = "" then baseName
        else baseName.Substring(0, baseName.Length - extension.Length)

    let isAbsolute = path.StartsWith(pathSep) && path.Length > 1

    let dir =
        if baseName = path then ""
        else
            let mutable normalized = normalizePath path
            if normalized.Length > 1 && normalized.EndsWith(pathSep) then
                normalized <- normalized.TrimEnd('/')
            let index = normalized.LastIndexOf('/')
            if index < 0 then "."
            elif index = 0 then pathSep
            else normalized.Substring(0, index)

    box (
        Map.ofList
            [ "root", box (if isAbsolute then pathSep else "")
              "dir", box dir
              "base", box baseName
              "ext", box extension
              "name", box name ]
    )

let isAbsolute (value: obj) : obj =
    box (unbox<string> value |> fun path -> path.StartsWith(pathSep))
