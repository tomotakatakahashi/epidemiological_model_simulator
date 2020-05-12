port module Main exposing (main)

import Array exposing (Array)
import Browser
import Html exposing (Html, button, div, input, label, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Set exposing (Set)
import Task
import Time



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


port updateChart : Solution -> Cmd msg



-- MODEL


type alias Model =
    { initialValues : State
    , solution : Solution
    , parameters : Parameters
    , simulationSettings : SimulationSettings
    , formValues : FormValues
    , errorMessages : Set String
    }


defaultModel : Model
defaultModel =
    { initialValues = defaultInitialValues
    , solution = defaultSolution
    , parameters = defaultParameters
    , simulationSettings = defaultSimulationSettings
    , formValues = defaultFormValues
    , errorMessages = Set.empty
    }


type alias FormValues =
    { k : String
    , delta : String
    , gamma : String
    , s0 : String
    , i0 : String
    , r0 : String
    , dt : String
    , steps : String
    }


defaultFormValues =
    { k = String.fromFloat defaultParameters.k
    , delta = String.fromFloat defaultParameters.delta
    , gamma = String.fromFloat defaultParameters.gamma
    , s0 = String.fromFloat defaultInitialValues.s
    , i0 = String.fromFloat defaultInitialValues.i
    , r0 = String.fromFloat defaultInitialValues.r
    , dt = String.fromFloat defaultSimulationSettings.dt
    , steps = String.fromInt defaultSimulationSettings.steps
    }


type alias Solution =
    Array State


defaultSolution =
    Array.empty


type alias State =
    { s : Float
    , i : Float
    , r : Float
    }


defaultInitialValues =
    { s = 10000
    , i = 10
    , r = 0
    }


type alias Parameters =
    { k : Float, delta : Float, gamma : Float }


defaultParameters =
    { k = 0.1, delta = 0.0, gamma = 1.0 }


type alias SimulationSettings =
    { dt : Float, steps : Int }


defaultSimulationSettings =
    { dt = 0.001, steps = 100 }


init : () -> ( Model, Cmd Msg )
init _ =
    ( defaultModel, Cmd.batch [ Task.perform (\_ -> UpdateSolution) Time.now, updateChart defaultModel.solution ] )



-- UPDATE


type Msg
    = UpdateSolution
    | UpdateK String
    | UpdateDelta String
    | UpdateGamma String
    | UpdateS0 String
    | UpdateI0 String
    | UpdateR0 String
    | UpdateDt String
    | UpdateSteps String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        formValues =
            model.formValues

        parameters =
            model.parameters

        initialValues =
            model.initialValues

        simulationSettings =
            model.simulationSettings

        errorMessages =
            model.errorMessages
    in
    case msg of
        UpdateSolution ->
            let
                newSolution =
                    rungeKuttaMethod initialValues parameters simulationSettings
            in
            ( { model | solution = newSolution }, updateChart newSolution )

        UpdateK newK ->
            case String.toFloat newK of
                Nothing ->
                    ( { model | formValues = { formValues | k = newK }, errorMessages = Set.insert "Invalid k" errorMessages }, Cmd.none )

                Just k ->
                    ( { model | formValues = { formValues | k = newK }, errorMessages = Set.remove "Invalid k" errorMessages, parameters = { parameters | k = k } }, Cmd.none )

        UpdateDelta newDelta ->
            case String.toFloat newDelta of
                Nothing ->
                    ( { model | formValues = { formValues | delta = newDelta }, errorMessages = Set.insert "Invalid Delta" errorMessages }, Cmd.none )

                Just delta ->
                    ( { model | formValues = { formValues | delta = newDelta }, errorMessages = Set.remove "Invalid Delta" errorMessages, parameters = { parameters | delta = delta } }, Cmd.none )

        UpdateGamma newGamma ->
            case String.toFloat newGamma of
                Nothing ->
                    ( { model | formValues = { formValues | gamma = newGamma }, errorMessages = Set.insert "Invalid Gamma" errorMessages }, Cmd.none )

                Just gamma ->
                    ( { model | formValues = { formValues | gamma = newGamma }, errorMessages = Set.remove "Invalid Gamma" errorMessages, parameters = { parameters | gamma = gamma } }, Cmd.none )

        UpdateS0 newS0 ->
            case String.toFloat newS0 of
                Nothing ->
                    ( { model | formValues = { formValues | s0 = newS0 }, errorMessages = Set.insert "Invalid S_0" errorMessages }, Cmd.none )

                Just s0 ->
                    ( { model | formValues = { formValues | s0 = newS0 }, errorMessages = Set.remove "Invalid S_0" errorMessages, initialValues = { initialValues | s = s0 } }, Cmd.none )

        UpdateI0 newI0 ->
            case String.toFloat newI0 of
                Nothing ->
                    ( { model | formValues = { formValues | i0 = newI0 }, errorMessages = Set.insert "Invalid I_0" errorMessages }, Cmd.none )

                Just i0 ->
                    ( { model | formValues = { formValues | i0 = newI0 }, errorMessages = Set.remove "Invalid I_0" errorMessages, initialValues = { initialValues | i = i0 } }, Cmd.none )

        UpdateR0 newR0 ->
            case String.toFloat newR0 of
                Nothing ->
                    ( { model | formValues = { formValues | r0 = newR0 }, errorMessages = Set.insert "Invalid R_0" errorMessages }, Cmd.none )

                Just r0 ->
                    ( { model | formValues = { formValues | r0 = newR0 }, errorMessages = Set.remove "Invalid R_0" errorMessages, initialValues = { initialValues | r = r0 } }, Cmd.none )

        UpdateDt newDt ->
            case String.toFloat newDt of
                Nothing ->
                    ( { model | formValues = { formValues | dt = newDt }, errorMessages = Set.insert "Invalid dt" errorMessages }, Cmd.none )

                Just dt ->
                    ( { model | formValues = { formValues | dt = newDt }, errorMessages = Set.remove "Invalid dt" errorMessages, simulationSettings = { simulationSettings | dt = dt } }, Cmd.none )

        UpdateSteps newSteps ->
            case String.toInt newSteps of
                Nothing ->
                    ( { model | formValues = { formValues | steps = newSteps }, errorMessages = Set.insert "Invalid steps" errorMessages }, Cmd.none )

                Just steps ->
                    ( { model | formValues = { formValues | steps = newSteps }, errorMessages = Set.remove "Invalid steps" errorMessages, simulationSettings = { simulationSettings | steps = steps } }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        _ ->
            Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        ([ viewInput "k" "k" model.formValues.k UpdateK
         , viewInput "delta" "Delta" model.formValues.delta UpdateDelta
         , viewInput "gamma" "Gamma" model.formValues.gamma UpdateGamma
         , viewInput "s0" "S_0" model.formValues.s0 UpdateS0
         , viewInput "i0" "I_0" model.formValues.i0 UpdateI0
         , viewInput "r0" "R_0" model.formValues.r0 UpdateR0
         , viewInput "dt" "dt" model.formValues.dt UpdateDt
         , viewInput "steps" "steps" model.formValues.steps UpdateSteps
         ]
            ++ List.map (\message -> div [ class "notification is-danger is-light" ] [ text message ]) (Set.toList model.errorMessages)
            ++ [ div [ class "field is-horizontal" ] [ div [ class "field-label" ] [], div [ class "field-body" ] [ div [ class "field is-grouped" ] [ div [ class "control" ] [ button [ onClick UpdateSolution, class "button is-primary" ] [ text "Update" ] ] ] ] ] ]
        )


viewInput : String -> String -> String -> (String -> Msg) -> Html Msg
viewInput labelId labelText val inputHandler =
    div
        [ class "field is-horizontal" ]
        [ div [ class "field-label" ] [ label [ for labelId, class "label" ] [ text labelText ] ]
        , div [ class "field-body" ]
            [ div [ class "field is-grouped" ]
                [ div [ class "control" ]
                    [ input
                        [ id labelId
                        , type_ "number"
                        , class "input"
                        , value val
                        , onInput inputHandler
                        ]
                        []
                    ]
                ]
            ]
        ]



-- Utils
-- Runge Kutta Method (RK4)


rungeKuttaMethod : State -> Parameters -> SimulationSettings -> Array State
rungeKuttaMethod initialValues parameters simulationSettings =
    rungeKuttaHelper parameters simulationSettings (Array.fromList [ initialValues ])


diffEqF : Parameters -> SimulationSettings -> State -> State
diffEqF parameters simulationSettings nowState =
    let
        k =
            parameters.k

        delta =
            parameters.delta

        gamma =
            parameters.gamma

        nowS =
            nowState.s

        nowI =
            nowState.i

        nowR =
            nowState.r

        fs =
            -k * nowI * nowS + delta * nowI

        fi =
            k * nowI * nowS - (gamma + delta) * nowI

        fr =
            gamma * nowI
    in
    { s = fs, i = fi, r = fr }


sum : State -> State -> State
sum lhs rhs =
    { s = lhs.s + rhs.s, i = lhs.i + rhs.i, r = lhs.r + rhs.r }


prod : Float -> State -> State
prod lhs rhs =
    { s = lhs * rhs.s, i = lhs * rhs.i, r = lhs * rhs.r }


rungeKuttaHelper : Parameters -> SimulationSettings -> Solution -> Solution
rungeKuttaHelper parameters simulationSettings solution =
    if Array.length solution < simulationSettings.steps then
        let
            maybeLastState =
                Array.get (Array.length solution - 1) solution
        in
        case maybeLastState of
            Nothing ->
                Array.empty

            Just lastState ->
                let
                    dt =
                        simulationSettings.dt

                    -- RK4
                    k1 =
                        diffEqF parameters simulationSettings lastState

                    k2Arg =
                        sum lastState (prod (dt / 2) k1)

                    k2 =
                        diffEqF parameters simulationSettings k2Arg

                    k3Arg =
                        sum lastState (prod (dt / 2) k2)

                    k3 =
                        diffEqF parameters simulationSettings k3Arg

                    k4Arg =
                        sum lastState (prod dt k3)

                    k4 =
                        diffEqF parameters simulationSettings k4Arg

                    nowResid =
                        prod (dt / 6) (sum k1 (sum (prod 2 k2) (sum (prod 2 k3) k4)))

                    nowState =
                        sum lastState nowResid
                in
                rungeKuttaHelper parameters simulationSettings (Array.push nowState solution)

    else
        solution



-- Euler Method


eulerMethod : State -> Parameters -> SimulationSettings -> Array State
eulerMethod initialValues parameters simulationSettings =
    eulerMethodHelper parameters simulationSettings (Array.fromList [ initialValues ])


eulerMethodHelper : Parameters -> SimulationSettings -> Solution -> Solution
eulerMethodHelper parameters simulationSettings solution =
    if Array.length solution < simulationSettings.steps then
        let
            maybeLastState =
                Array.get (Array.length solution - 1) solution
        in
        case maybeLastState of
            Nothing ->
                Array.empty

            Just lastState ->
                eulerMethodHelper parameters simulationSettings (Array.push (eulerNextState parameters simulationSettings lastState) solution)

    else
        solution


eulerNextState : Parameters -> SimulationSettings -> State -> State
eulerNextState parameters simulationSettings nowState =
    let
        k =
            parameters.k

        delta =
            parameters.delta

        gamma =
            parameters.gamma

        dt =
            simulationSettings.dt

        nowS =
            nowState.s

        nowI =
            nowState.i

        nowR =
            nowState.r

        nextS =
            nowS + dt * (-k * nowI * nowS + delta * nowI)

        nextI =
            nowI + dt * (k * nowI * nowS - (gamma + delta) * nowI)

        nextR =
            nowR + dt * gamma * nowI
    in
    { s = nextS, i = nextI, r = nextR }
