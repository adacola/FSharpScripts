#load @"HttpStatusCore.fsx"

open HttpStatusCore

match fsi.CommandLineArgs with [|arg|] -> filterStatus arg | _ -> filterStatus ""
|> List.iter (printfn "%s")
