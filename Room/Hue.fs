module Hue

open HttpFs.Client
open Hopac
open System
open System.Web
open System.IO
open System.Text
open System.Runtime.Serialization
open System.Runtime.Serialization.Json

let upnpURL = "https://www.meethue.com/api/nupnp"

type Result<'TSuccess,'TFailure> =
    | Success of 'TSuccess
    | Failure of 'TFailure

type IPAddress = IPAddress of string

[<DataContract>]
type UPNPBridgeResponse = {
    [<field: DataMember(Name="id")>]
    id: string
    [<field: DataMember(Name="internalipaddress")>]
    internalipaddress: string
    };

type Bridge = {
    ipAddress: IPAddress
    username: string
    id: string
    authorised: bool
    } with

    static member Create ipAddress id username =
        {
            ipAddress = ipAddress
            id = id
            username = username
            authorised = match username with
                         | "" -> false
                         | _ -> true
        }

    static member CreateAuthorized bridge username =
        Bridge.Create bridge.ipAddress bridge.id username

    member this.Authorise username =
        Bridge.CreateAuthorized this username

[<DataContract>]
type DeviceType = {
    [<field: DataMember(Name="devicetype")>]
    devicetype: string
}

/// Object to Json
let internal json<'t> (myObj:'t) =
        use ms = new MemoryStream()
        (new DataContractJsonSerializer(typeof<'t>)).WriteObject(ms, myObj)
        Encoding.Default.GetString(ms.ToArray())


/// Object from Json
let internal unjson<'t> (jsonString:string)  : 't =
        use ms = new MemoryStream(ASCIIEncoding.Default.GetBytes(jsonString))
        let obj = (new DataContractJsonSerializer(typeof<'t>)).ReadObject(ms)
        obj :?> 't

let upnpBridgeToBridge upnpBridge =
    Bridge.Create (IPAddress upnpBridge.internalipaddress) upnpBridge.id ""

let doRequest method url data =
    let request = Request.createUrl method url
    let request = match data with
                  | Some x -> Request.body x request
                  | None -> request

    run <| job {
        try
            use! response = getResponse request
            let statusCode = response.statusCode / 100
            let! body = Response.readBodyAsString response

            return match statusCode with
                   | 2 -> Success body
                   | _ -> new Exception(body) |> Failure
        with
            | ex -> return Failure ex
    }


let doBridgeRequest method bridge path data =
    let ipAddress = bridge.ipAddress
    let url = "http://" + ipAddress.ToString() + "/api/"

    let url = match bridge.username with
              | "" -> url + path
              | username -> url + username + "/" + path

    doRequest method url data

let getRequest bridge path = doBridgeRequest Get bridge path None
let putRequest = doBridgeRequest Put
let postRequest = doBridgeRequest Post
let deleteRequest = doBridgeRequest Delete

let parseBridgesFromJson (bridgeJson: string) =
    try
        bridgeJson |> unjson<UPNPBridgeResponse list>
                   |> List.map upnpBridgeToBridge
    with
        | ex -> []

let findBridges () =
    let bridgesResult = doRequest Get upnpURL None

    match bridgesResult with
    | Success bridgesJson -> parseBridgesFromJson bridgesJson
    | Failure _ -> []

let authorise bridge deviceType =
    let deviceTypeJson = json<DeviceType> deviceType
    let requestBody = BodyString deviceTypeJson |> Some
    let result = postRequest bridge "" requestBody

    match result with
    | Success username -> Success bridge.Authorise
    | Failure _ -> Failure bridge

type HSVColor = {
    hue: uint16
    bri: uint8
    sat: uint8
    } with
    static member Create hue sat bri = {
        hue = hue
        sat = sat
        bri = bri
    }

type RGBColor = {
    red: uint8
    green: uint8
    blue: uint8
    } with
    static member Create red green blue = {
        red = red
        green = green
        blue = blue
    }

type XYColor = {
    x: float
    y: float
    bri: uint8
    } with

    static member Create (xy: float list) bri = {
        x = xy.[0]
        y = xy.[1]
        bri = bri
    }

let hsvToRgb (hsvColor: HSVColor) =
    let v = (float hsvColor.bri) / 254.0
    let s = (float hsvColor.sat) / 254.0
    let c = v * s
    let hueToDegreesDividedBy60 = (float hsvColor.hue) / 10920.0
    let x = hueToDegreesDividedBy60 |> (%) 2.0 |> (-) 1.0 |> abs |> (-) 1.0 |> (*) c
    let m = v - c
    let intHueOver60 = floor hueToDegreesDividedBy60 |> int
    let convertToRGBValue = (+) m >> (*) 255.0 >> floor >> uint8

    let r = match intHueOver60 with
            | 0 | 5 -> c
            | 1 | 4 -> x
            | _ -> 0.0
            |> convertToRGBValue
    let g = match intHueOver60 with
            | 0 | 5 -> c
            | 1 | 4 -> x
            | _ -> 0.0
            |> convertToRGBValue
    let b = match intHueOver60 with
            | 0 | 5 -> c
            | 1 | 4 -> x
            | _ -> 0.0
            |> convertToRGBValue

    RGBColor.Create r g b

let rgbToHsv rgbColor =
    let r' = float rgbColor.red |> (/) 255.0
    let g' = float rgbColor.green |> (/) 255.0
    let b' = float rgbColor.blue |> (/) 255.0

    let cMax = [r'; g'; b'] |> List.max
    let cMin = [r'; g'; b'] |> List.min
    let delta = cMax - cMin

    let s = match cMax with
            | 0.0 -> uint8 0
            | _ -> delta / cMax |> (*) 255.0 |> floor |> uint8

    let h = match cMax with
            | i when i = r' -> g' - b' |> (/) delta |> (%) 6.0
            | i when i = g' -> b' - r' |> (/) delta |> (+) 2.0
            | i when i = b' -> r' - g' |> (/) delta |> (+) 4.0
            | _ -> 0.0
            |> (*) 10920.0 |> floor |> uint16

    let v = cMax * 255.0 |> floor |> uint8

    HSVColor.Create h s v

type HSVColor with
    member this.ToRGB = hsvToRgb this

type RGBColor with
    member this.ToHSV = rgbToHsv this

[<DataContract>]
type LightState = {
    [<field: DataMember(Name="on")>]
    on: bool
    [<field: DataMember(Name="bri")>]
    bri: uint8
    [<field: DataMember(Name="hue")>]
    hue: uint16
    [<field: DataMember(Name="sat")>]
    sat: uint8
    [<field: DataMember(Name="xy")>]
    xy: float list
    [<field: DataMember(Name="effect")>]
    effect: string
    [<field: DataMember(Name="ct")>]
    ct: uint16
    [<field: DataMember(Name="alert")>]
    alert: string
    [<field: DataMember(Name="colormode")>]
    colormode: string
    [<field: DataMember(Name="reachable")>]
    reachable: bool
}

[<DataContract>]
type LightJSON = {
    [<field: DataMember(Name="type")>]
    lightType: string
    [<field: DataMember(Name="name")>]
    name: string
    [<field: DataMember(Name="state")>]
    state: LightState
    [<field: DataMember(Name="modelid")>]
    modelid: string
    [<field: DataMember(Name="manufacturername")>]
    manufacturername: string
    [<field: DataMember(Name="uniqueid")>]
    uniqueid: string
    [<field: DataMember(Name="swversion")>]
    swversion: string
    [<field: DataMember(Name="swconfigid")>]
    swconfigid: string
    [<field: DataMember(Name="productid")>]
    productid: string
    }

type Light = {
    bridge: Bridge
    name: string
    on: bool
    rgbColor: RGBColor
    hsvColor: HSVColor
    xyColor: XYColor
    } with

    static member FromJson bridge (lightJson: LightJSON) =
        let hsvColor = HSVColor.Create lightJson.state.hue lightJson.state.sat lightJson.state.bri
        let rgbColor = hsvColor.ToRGB
        let xyColor = XYColor.Create lightJson.state.xy lightJson.state.bri
        {
            bridge = bridge
            name = lightJson.name
            on = lightJson.state.on
            hsvColor = hsvColor
            rgbColor = rgbColor
            xyColor = xyColor
        }


let parseLightsFromJson bridge lightsJson =
    let lights = unjson<LightJSON list> lightsJson
    let createLights = Light.FromJson bridge
    lights |> List.map createLights

let getLights bridge =
    let result = getRequest bridge "lights"

    match result with
    | Success x -> parseLightsFromJson bridge x
    | Failure _ -> []