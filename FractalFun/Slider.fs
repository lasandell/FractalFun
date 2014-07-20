// Module for creating HTML sliders. Each slider reads and
// writes parameters to and from the hash portion of the 
// current URL. Whenever a parameter changes, an asynchronous
// function is called to apply the new state.
[<ReflectedDefinition>]
module FractalFun.Slider

open System
open FunScript.Core.Async
open FunScript.TypeScript

// Global values
let document = Globals.document
let window = Globals.window
let location = Globals.window.location

// Operator used in conjunction with DOM event handlers so that 
// I don't have to return an object at the end of every handler.
let (~%) handler =
    Func<'event, obj>(handler >> box)

// Format a param name nicely for the URL.
let paramName (name:string) =
    name.Replace(" ", "")

// Read all params from the URL
let readParams() =
    if location.hash = "" then Map.empty else
    location.hash.Replace("#", "").Split '&'
    |> Seq.map (fun s -> s.Split '=')
    |> Seq.map (fun a -> a.[0], a.[1])
    |> Map.ofSeq

// Write all params to the URL
let writeParams map =
    Map.toSeq map
    |> Seq.sortBy fst
    |> Seq.map (fun (n, v) -> n + "=" + v)
    |> String.concat "&"
    |> (+) (location.href.Split('#').[0] + "#")
    |> location.replace

// Set a particular parameter on the URL
let setParam name value =
    readParams().Add(paramName name, value)
    |> writeParams

// Get a particular parameter from the URL with a default value
let getParam name def =
    match readParams().TryFind(paramName name) with
    | Some v -> v
    | None -> setParam name def
              def

// Append HTML child nodes 
let appendChild (parent:Node) child =
    parent.appendChild child |> ignore
    child

// Returns a function that runs the given asynchronous function,
// cancelling the previous run on each subsequent call. The function 
// must accept a data argument and a cancellation token.
let asyncRunner func =
    let tokenSource = ref (CancellationTokenSource())
    fun arg -> (!tokenSource).Cancel()
               tokenSource := CancellationTokenSource()
               Async.StartImmediate(func arg (!tokenSource).Token)

// Creates an individual slider. Returns a function that can
// compute the new state based on the current URL parameter.
let createSlider (name, min, max, def, mapState) =
    let sliders = document.getElementById "sliders"
    let div = document.createElement_div() |> appendChild sliders
    let label = document.createElement_label() |> appendChild div
    label.textContent <- name
    let input = document.createElement_input() |> appendChild div
    input._type <- "range"
    input.min <- min.ToString() 
    input.max <- max.ToString()
    input.addEventListener_change %(fun _ -> setParam name input.value)
    fun state -> input.value <- getParam name (def.ToString())
                 mapState state (input.value |> Double.Parse)

// Create and configure the sliders
let initSliders sliders applyState state =
    let state = ref state
    let mapStates = sliders |> List.map createSlider
    let applyChanges() =
        mapStates |> Seq.iter (fun mapState -> state := mapState !state)
        applyState !state
    window.addEventListener_hashchange %(fun _ -> applyChanges())
    applyChanges()
