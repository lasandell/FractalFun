// Boiler plate module for launching our FunScript code.
module FractalFun.Startup

open System
open System.Diagnostics
open System.IO
open FunScript.Compiler

[<EntryPoint>]
let start(_) =

    // Compile the main() function into javascript code.
    let text = Compiler.Compile(<@ FractalFun.Fractal.main @>, noReturn = true)

    // Write the script to a file. Note the appended "();" - this seems
    // to be needed due to a bug with this version of FunScript
    let path = Path.Combine(Environment.CurrentDirectory, "script.js")
    File.WriteAllText(path, text + "();")

    // Open the file in the default web browser.
    Process.Start("index.html") |> ignore    
    
    // Return exit code of 0
    0
