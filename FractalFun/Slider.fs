// Simple module for creating HTML sliders. Each slider
// reads a state from a reference cell, updates the state,
// and performs a followup action. The sliders also persist
// their values to and from the anchor portion of current URL.
[<ReflectedDefinition>]
module FractalFun.Slider

open System

// Global values
let document = Globals.document
let location = Globals.window.location

// Format a param name nicely for the URL.
let paramName (name:string) =
    name.Replace(" ", "")

// Read all params from the URL
let readParams() =
    if location.hash = "" then Map.empty else
    location.hash.Replace("#", "").Split('&')
    |> Seq.map (fun s -> s.Split('='))
    |> Seq.map (fun a -> a.[0], a.[1])
    |> Map.ofSeq

// Write all params to the URL
let writeParams map =
    Map.toSeq map
    |> Seq.map (fun (k, v) -> k + "=" + v)
    |> String.concat("&")
    |> fun p -> location.hash <- p

// Set a particular parameter on the URL
let setParam name value =
    readParams().Add(paramName name, value)
    |> writeParams

// Get a particular parameter from the URL with a default value
let getParam name def =
    match readParams().TryFind(paramName name) with
    | Some v -> v
    | None -> def

// Append HTML child nodes 
let appendChild (parent:Node) child =
    parent.appendChild(child) |> ignore
    child

// Create and configure a slider
let slider parent applyState stateRef (name, min, max, def, mapState) =
    let value = Double.Parse(getParam name (def.ToString()))
    setParam name (value.ToString())
    let sliders = document.getElementById parent
    let div = document.createElement_div() |> appendChild sliders
    let label = document.createElement_label() |> appendChild div
    label.textContent <- name
    let input = document.createElement_input() |> appendChild div
    input._type <- "range"
    input.min <- min.ToString() 
    input.max <- max.ToString()
    input.value <- value.ToString()
    input.addEventListener_change (fun e ->
        let value = input.valueAsNumber
        setParam name (value.ToString())
        stateRef := mapState !stateRef value
        applyState !stateRef
        obj())
    stateRef := mapState !stateRef value
