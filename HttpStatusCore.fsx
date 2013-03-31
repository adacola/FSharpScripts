type HttpStatus = { Code : string; Status : string } with
    static member create code status = { Code = code; Status = status }
    override this.ToString() = sprintf "%s %s" this.Code this.Status

let filterStatus input =
    let status =
        [
            "100", "Continue"
            "101", "Switching Protocols"
            "102", "Processing"
            "200", "OK"
            "201", "Created"
            "202", "Accepted"
            "203", "Non-Authoritative Information"
            "204", "No Content"
            "205", "Reset Content"
            "206", "Partial Content"
            "207", "Multi-Status"
            "208", "Already Reported"
            "300", "Multiple Choices"
            "301", "Moved Permanently"
            "302", "Found"
            "303", "See Other"
            "304", "Not Modified"
            "305", "Use Proxy"
            "307", "Temporary Redirect"
            "400", "Bad Request"
            "401", "Unauthorized"
            "402", "Payment Required"
            "403", "Forbidden"
            "404", "Not Found"
            "405", "Method Not Allowed"
            "406", "Not Acceptable"
            "407", "Proxy Authentication Required"
            "408", "Request Timeout"
            "409", "Conflict"
            "410", "Gone"
            "411", "Length Required"
            "412", "Precondition Failed"
            "413", "Request Entity Too Large"
            "414", "Request-URI Too Large"
            "415", "Unsupported Media Type"
            "416", "Request Range Not Satisfiable"
            "417", "Expectation Failed"
            "418", "I'm a teapot"
            "422", "Unprocessable Entity"
            "423", "Locked"
            "424", "Failed Dependency"
            "425", "No code"
            "426", "Upgrade Required"
            "428", "Precondition Required"
            "429", "Too Many Requests"
            "431", "Request Header Fields Too Large"
            "449", "Retry with"
            "500", "Internal Server Error"
            "501", "Not Implemented"
            "502", "Bad Gateway"
            "503", "Service Unavailable"
            "504", "Gateway Timeout"
            "505", "HTTP Version Not Supported"
            "506", "Variant Also Negotiates"
            "507", "Insufficient Storage"
            "509", "Bandwidth Limit Exceeded"
            "510", "Not Extended"
            "511", "Network Authentication Required"
        ] |> List.map ((<||) HttpStatus.create)

    let (|CompletelyMatchCode|_|) input =
        status |> List.tryPick (fun hs -> if hs.Code = input then Some hs.Status else None)
    let (|PartialMatch|_|) matches input =
        status |> List.filter (fun s -> matches s input) |> List.map (sprintf "%O") |> function [] -> None | r -> Some r
    let matchesCode { Code = c } = c.StartsWith
    let matchesStatus { Status = s } (input : string) = s.ToLower().Contains <| input.ToLower()

    match input with
    | "" -> status |> List.map (sprintf "%O")
    | CompletelyMatchCode result -> [result]
    | PartialMatch matchesCode result -> result
    | PartialMatch matchesStatus result -> result
    | _ -> []
