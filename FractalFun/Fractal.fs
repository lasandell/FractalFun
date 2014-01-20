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
              rightAngle: float; rotate: float; translateX: float; translateY: float; lineWidth: float }

// Global values
let document = Globals.document
let canvas = document.getElementById("canvas") :?> HTMLCanvasElement
let context = canvas.getContext_2d()
let width = canvas.clientWidth
let height = canvas.clientHeight
let emptySpec = { iterations = 0; scale = 0.; scaleFactor = 0.; angle = 0.; leftAngle = 0.; 
                 rightAngle = 0.; rotate = 0.;  translateX = 0.; translateY = 0.; lineWidth = 0. }

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
    "Line Width",    1.,   5.,   2.,  (fun spec value -> { spec with lineWidth = value })
]

// Computes the endpoint of a line
let endpoint point angle length =
    { x = point.x + length * cos angle;
      y = point.y + length * sin angle }

// Draws a line
let draw point angle length lineWidth =
    let endpoint = endpoint point angle length
    context.lineWidth <- lineWidth
    context.beginPath()
    context.moveTo(point.x, point.y)
    context.lineTo(endpoint.x, endpoint.y)
    context.stroke()
    endpoint
  
// Generates a fractal asynchronously, allowing for cancellation
let render spec throwIfCancelled =
    let rec fractal point scale angle iterations = 
        async { if iterations = 0 then () else
                throwIfCancelled()
                let endpoint = draw point angle scale spec.lineWidth
                do! fractal endpoint (scale * spec.scaleFactor) (angle + spec.angle + spec.leftAngle) (iterations - 1)
                do! fractal endpoint (scale * spec.scaleFactor) (angle - spec.angle - spec.rightAngle) (iterations - 1) }
    context.clearRect(0., 0., width, height)
    fractal { x = width / 2. + spec.translateX; y = spec.translateY } spec.scale spec.rotate spec.iterations

// Compiled entry point (referenced by the Startup module)
let main() = 
    canvas.width <- width
    canvas.height <- height
    context.translate(0., height)
    context.scale(1., -1.)
    initSliders sliders render emptySpec
    let downloadLink = document.getElementById("download") :?> HTMLLinkElement
    downloadLink.addEventListener_click(fun _ -> 
        downloadLink.href <- canvas.toDataURL()
        obj())
