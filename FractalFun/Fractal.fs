// This is the main module responsible for setting up
// the UI and rendering the fractals.
[<ReflectedDefinition>]
module FractalFun.Fractal

open FunScript
open FunScript.Core.Async
open FractalFun.Slider

// Record types
type Point = { x: float; y: float }
type Spec = { iterations: int; scale: float; scaleFactor: float; angle: float; leftAngle: float;
              rightAngle: float; rotate: float; translateX: float; translateY: float }

// Global values
let document = Globals.document
let canvas = document.getElementById("canvas") :?> HTMLCanvasElement
let context = canvas.getContext_2d()
let height = canvas.clientHeight
let mutable tokenSource = new CancellationTokenSource()
let mutable lastHash = ""
let spec = ref { iterations = 0; scale = 0.; scaleFactor = 0.; angle = 0.; leftAngle = 0.; 
                 rightAngle = 0.; rotate = 0.;  translateX = 0.; translateY = 0.;  }

// Function to convert degrees to radians
let radians degrees = 
    degrees * System.Math.PI / 180.

// Configuration for our sliders
let sliders = [   (* min   max   def *)
    "Iterations",    1.,   20.,  10., (fun spec value -> { spec with iterations = int value })
    "Scale",         0.,   200., 14., (fun spec value -> { spec with scale = value / 100. * height })
    "Scale Factor",  0.,   100., 85., (fun spec value -> { spec with scaleFactor = value / 100. })
    "Angle",         0.,   180., 20., (fun spec value -> { spec with angle = radians value })
    "Left Angle",   -180., 180., 0.,  (fun spec value -> { spec with leftAngle = radians value })
    "Right Angle",  -180., 180., 0.,  (fun spec value -> { spec with rightAngle = radians value })
    "Rotate",       -180., 180., 0.,  (fun spec value -> { spec with rotate = radians(90. - value) })
    "Translate X",  -999., 999., 0.,  (fun spec value -> { spec with translateX = value })
    "Translate Y",  -999., 999., 0.,  (fun spec value -> { spec with translateY = value })
]

// Computes the endpoint of a line
let endpoint point angle length =
    { x = point.x + length * cos angle;
      y = point.y + length * sin angle }

// Draws a line
let draw point angle length =
    let endpoint = endpoint point angle length
    context.lineWidth <- 2.
    context.beginPath()
    context.moveTo(point.x, point.y)
    context.lineTo(endpoint.x, endpoint.y)
    context.stroke()
    endpoint
  
// Generates a fractal asynchronously, allowing for cancellation
let fractal spec (token:CancellationToken) =
    let rec fractal' point scale angle iterations = 
        async { if iterations = 0 then () else
                token.ThrowIfCancellationRequested()
                let endpoint = draw point angle scale
                do! fractal' endpoint (scale * spec.scaleFactor) (angle + spec.angle + spec.leftAngle) (iterations - 1)
                do! fractal' endpoint (scale * spec.scaleFactor) (angle - spec.angle - spec.rightAngle) (iterations - 1) }
    fractal' { x = canvas.width / 2. + spec.translateX; y = spec.translateY } spec.scale spec.rotate spec.iterations

// Kicks off an async rendering with the given fractal spec
let render spec =
    lastHash <- Globals.window.location.hash
    tokenSource.Cancel()
    tokenSource <- new CancellationTokenSource()
    context.clearRect(0., 0., canvas.width, canvas.height)
    Async.StartImmediate(fractal spec tokenSource.Token)

// Compiled entry point (referenced by the Startup module)
let main() = 
    canvas.width <- canvas.clientWidth
    canvas.height <- canvas.clientHeight
    context.translate(0., canvas.height)
    context.scale(1., -1.)
    sliders |> Seq.iter (slider "sliders" render spec)
    let downloadLink = document.getElementById("download") :?> HTMLLinkElement
    downloadLink.addEventListener_click(fun _ -> 
        downloadLink.href <- canvas.toDataURL()
        obj()
    )
    Globals.window.addEventListener_hashchange(fun _ ->
        if Globals.window.location.hash <> lastHash then
            Globals.window.location.reload()
        obj()
    )
    render !spec