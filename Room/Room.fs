module Program

// open Suave
// open Suave.Successful
open Serial

type ControlPanels = DoorPanel | PCPanel | BedPanel
type ControlButtons = AllOn | AllOff | Off | PresetOne | PresetTwo | PresetThree
type ButtonPress = {
    panel: ControlPanels
    button: ControlButtons
    }

let printPanel x =
    match x with
    | DoorPanel -> "Door Panel"
    | PCPanel -> "PC Panel"
    | BedPanel -> "Bed Panel"

let printButton x =
    match x with
    | AllOn -> "All On"
    | AllOff -> "All Off"
    | Off -> "Off"
    | PresetOne -> "Preset One"
    | PresetTwo -> "Preset Two"
    | PresetThree -> "Preset Three"

let buttonToPanel x =
    match x with
    | 1 -> Some DoorPanel
    | 2 -> Some PCPanel
    | 3 -> Some BedPanel
    | _ -> None

let buttonToControl x =
    match x with
    | 0 -> Some AllOff
    | 1 -> Some AllOn
    | 2 -> Some Off
    | 3 -> Some PresetOne
    | 4 -> Some PresetTwo
    | 5 -> Some PresetThree
    | _ -> None

let stringToButtonPress input =
    let buttonNumber = System.Int32.Parse input
    printfn "Parsed number %i" buttonNumber
    let panel = buttonNumber / 10 |> buttonToPanel
    let control = buttonNumber % 10 |> buttonToControl

    match panel with
    | Some x ->
        match control with
        | Some y -> Some { panel = x; button = y}
        | None -> None
    | None -> None

let processButton (input: string) =
    printfn "Got input %s" input
    let buttonPress = stringToButtonPress input

    match buttonPress with
    | Some x -> printfn "Got button press for panel %s and button %s" (printPanel x.panel) (printButton x.button)
    | None -> ()

[<EntryPoint>]
let main argv =
    let serialPort = "COM3" // "/dev/tty0"
    let serial = create192 serialPort

    let callback _ = readLine serial |> processButton |> ignore
    serial.DataReceived.Add callback
    serial.Open()

    // startWebServer defaultConfig (OK "Hello World!")

    let rec forever _ = forever()
    forever()

    0
