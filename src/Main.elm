module Main exposing (Msg(..), Submarine, init, main, subscriptions, update, view)

import Browser
import Html exposing (Html, br, div, input, option, pre, select, text, textarea)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode exposing (Decoder, field, string)
import YamlUtils



-- MAIN


main : Program Flag Submarine Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Flag =
    { openShiftUrl : String
    , authenticationToken : String
    }



-- MODEL


init : Flag -> ( Submarine, Cmd Msg )
init flag =
    ( { openShiftUrl = flag.openShiftUrl
      , authenticationToken = flag.authenticationToken
      , openShiftProjects = Loading
      , gitHubServiceAccount = GitHubResourceLoading
      , runtime = Quarkus
      , replicas = Nothing
      , incremental = True
      , gitUrl = ""
      , reference = ""
      , contextDir = ""
      }
    , getOpenShiftProjects flag.openShiftUrl flag.authenticationToken
    )


type alias Submarine =
    { openShiftUrl : String
    , authenticationToken : String
    , openShiftProjects : Projects
    , gitHubServiceAccount : GitHubResource
    , runtime : Runtime
    , replicas : Maybe Int
    , incremental : Bool
    , gitUrl : String
    , reference : String
    , contextDir : String
    }


type Projects
    = Loading
    | Success (List String) String
    | Error Http.Error


type Runtime
    = Quarkus
    | SpringBoot


type GitHubResource
    = GitHubResourceLoading
    | GitHubResourceSuccess String
    | GitHubResourceError



-- UPDATE


type Msg
    = UpdateGitUrl String
    | UpdateRuntime Runtime
    | UpdateReplicas String
    | SelectIncremental
    | UpdateReference String
    | UpdateContextDirectory String
    | GotOpenShiftProjects (Result Http.Error (List String))
    | ChangeOpenShiftProject String
    | GotSubmarineServiceAccountYaml (Result Http.Error String)


update : Msg -> Submarine -> ( Submarine, Cmd Msg )
update msg submarine =
    case msg of
        UpdateGitUrl newGitUrl ->
            ( { submarine | gitUrl = newGitUrl }
            , Cmd.none
            )

        UpdateRuntime newRuntime ->
            ( { submarine | runtime = newRuntime }
            , Cmd.none
            )

        UpdateReplicas newReplicas ->
            case String.toInt newReplicas of
                Just newReplicasNumber ->
                    ( { submarine | replicas = Just newReplicasNumber }
                    , Cmd.none
                    )

                Nothing ->
                    ( { submarine | replicas = Nothing }
                    , Cmd.none
                    )

        SelectIncremental ->
            ( { submarine | incremental = not submarine.incremental }
            , Cmd.none
            )

        UpdateReference newReference ->
            ( { submarine | reference = newReference }
            , Cmd.none
            )

        UpdateContextDirectory newContextDir ->
            ( { submarine | contextDir = newContextDir }
            , Cmd.none
            )

        GotOpenShiftProjects result ->
            case result of
                Ok (firstProject :: otherProjects) ->
                    ( { submarine | openShiftProjects = Success ([ firstProject ] ++ otherProjects) firstProject }
                    , getSubmarineServiceAccountYaml
                    )

                Ok [] ->
                    ( { submarine | openShiftProjects = Success [] "" }
                    , getSubmarineServiceAccountYaml
                    )

                Err error ->
                    ( { submarine | openShiftProjects = Error error }
                    , Cmd.none
                    )

        ChangeOpenShiftProject newOpenShiftProject ->
            case submarine.openShiftProjects of
                Success projects _ ->
                    ( { submarine | openShiftProjects = Success projects newOpenShiftProject }
                    , Cmd.none
                    )

                _ ->
                    ( submarine
                    , Cmd.none
                    )

        GotSubmarineServiceAccountYaml result ->
            case result of
                Ok loadedSubmarineServiceAccountYaml ->
                    ( { submarine | gitHubServiceAccount = GitHubResourceSuccess loadedSubmarineServiceAccountYaml }
                    , Cmd.none
                    )

                Err error ->
                    ( { submarine | gitHubServiceAccount = GitHubResourceError }
                    , Cmd.none
                    )



-- SUBSCRIPTIONS


subscriptions : Submarine -> Sub Msg
subscriptions submarine =
    Sub.none



-- VIEW


view : Submarine -> Html Msg
view submarine =
    div []
        [ div [ style "width" "60%", style "float" "left" ]
            ([ text "OpenShift URL: ", text submarine.openShiftUrl, br [] [] ]
                ++ viewOpenShiftProjects submarine
                ++ viewGitHubResourceStatus submarine
                ++ [ Html.fieldset []
                        ([ Html.legend [] [ text "Submarine configuration" ] ]
                            ++ viewRuntime submarine.runtime
                            ++ viewReplicas submarine
                            ++ [ text "Incremental build: "
                               , input [ type_ "checkbox", checked submarine.incremental, onClick SelectIncremental ] []
                               , br [] []
                               , text "Git URL: "
                               , input [ placeholder "Git URL", value submarine.gitUrl, onInput UpdateGitUrl ] []
                               , br [] []
                               , text "Reference: "
                               , input [ placeholder "Reference", value submarine.reference, onInput UpdateReference ] []
                               , br [] []
                               , text "Context directory: "
                               , input [ placeholder "Context directory", value submarine.contextDir, onInput UpdateContextDirectory ] []
                               , br [] []
                               ]
                        )
                   ]
            )
        , div [ style "width" "40%", style "float" "left" ] [ text "Submarine app custom resource YAML: ", textarea [ cols 80, rows 25, readonly True ] [ text (getSubAppAsYaml submarine) ] ]
        ]


viewReplicas : Submarine -> List (Html Msg)
viewReplicas submarine =
    case submarine.replicas of
        Just replicas ->
            [ text "Replicas: "
            , input [ placeholder "Replicas", value (String.fromInt replicas), onInput UpdateReplicas ] []
            , br [] []
            ]

        Nothing ->
            [ text "Replicas: "
            , input [ placeholder "Replicas", value "", onInput UpdateReplicas ] []
            , br [] []
            ]


viewRuntime : Runtime -> List (Html Msg)
viewRuntime runtime =
    let
        isQuarkusSelected =
            case runtime of
                Quarkus ->
                    True

                SpringBoot ->
                    False

        isSpringBootSelected =
            case runtime of
                Quarkus ->
                    False

                SpringBoot ->
                    True
    in
    [ input [ type_ "radio", name "runtime", checked isQuarkusSelected, onClick (UpdateRuntime Quarkus) ] []
    , text "Quarkus"
    , input [ type_ "radio", name "runtime", checked isSpringBootSelected, onClick (UpdateRuntime SpringBoot) ] []
    , text "SpringBoot"
    , br [] []
    ]


viewOpenShiftProjects : Submarine -> List (Html Msg)
viewOpenShiftProjects submarine =
    case submarine.openShiftProjects of
        Loading ->
            [ text "Loading OpenShift projects..."
            , br [] []
            ]

        Success projects selectedProject ->
            [ text "OpenShift projects: "
            , select [ onInput ChangeOpenShiftProject ]
                (List.map (\p -> option [ value p, selected (p == selectedProject) ] [ text p ]) projects)
            , br [] []
            ]

        Error errorHttp ->
            case errorHttp of
                Http.BadUrl url ->
                    [ text "No valid URL for OpenShift projects."
                    , br [] []
                    ]

                Http.NetworkError ->
                    [ text "Network error while loading OpenShift projects."
                    , br [] []
                    ]

                Http.BadStatus statusCode ->
                    [ text "Bad status code while loading OpenShift projects: "
                    , text (String.fromInt statusCode)
                    , br [] []
                    ]

                _ ->
                    [ text "Error while loading OpenShift projects."
                    , br [] []
                    ]


viewGitHubResourceStatus : Submarine -> List (Html Msg)
viewGitHubResourceStatus submarine =
    case submarine.gitHubServiceAccount of
        GitHubResourceLoading ->
            [ text "Loading Service account YAML.", br [] [] ]

        GitHubResourceSuccess _ ->
            [ text "Service account YAML loaded.", br [] [] ]

        GitHubResourceError ->
            [ text "Error while loading Service account YAML.", br [] [] ]



-- HTTP


getOpenShiftProjects : String -> String -> Cmd Msg
getOpenShiftProjects openShiftUrl authenticationToken =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" ("Bearer " ++ authenticationToken) ]
        , url = openShiftUrl ++ "/apis/project.openshift.io/v1/projects"
        , body = Http.emptyBody
        , expect = Http.expectJson GotOpenShiftProjects openShiftProjectsDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


openShiftProjectsDecoder : Decoder (List String)
openShiftProjectsDecoder =
    field "items" (Json.Decode.list (field "metadata" (field "name" string)))


getSubmarineServiceAccountYaml : Cmd Msg
getSubmarineServiceAccountYaml =
    Http.get
        { url = "https://raw.githubusercontent.com/kiegroup/submarine-cloud-operator/master/deploy/service_account.yaml"
        , expect = Http.expectString GotSubmarineServiceAccountYaml
        }



-- YAML


getSubAppAsYaml : Submarine -> String
getSubAppAsYaml submarine =
    YamlUtils.getNameAndValueWithIntendation "apiVersion" "app.kiegroup.org/v1alpha1" 0
        ++ YamlUtils.getNameAndValueWithIntendation "kind" "SubApp" 0
        ++ YamlUtils.getNameWithIntendation "metadata" 0
        ++ YamlUtils.getNameAndValueWithIntendation "name" "sub-cr" 1
        ++ YamlUtils.getNameWithIntendation "spec" 0
        ++ (case submarine.runtime of
                Quarkus ->
                    YamlUtils.getNameAndValueWithIntendation "runtime" "quarkus" 1

                SpringBoot ->
                    YamlUtils.getNameAndValueWithIntendation "runtime" "springboot" 1
           )
        ++ (case submarine.replicas of
                Just replicas ->
                    YamlUtils.getNameAndValueWithIntendation "replicas" (String.fromInt replicas) 1

                Nothing ->
                    ""
           )
        ++ YamlUtils.getNameWithIntendation "build" 1
        ++ (if submarine.incremental then
                YamlUtils.getNameAndValueWithIntendation "incremental" "true" 2

            else
                YamlUtils.getNameAndValueWithIntendation "incremental" "false" 2
           )
        ++ YamlUtils.getNameWithIntendation "gitSource" 2
        ++ YamlUtils.getNameAndValueWithIntendation "uri" submarine.gitUrl 3
        ++ (if String.length submarine.reference > 0 then
                YamlUtils.getNameAndValueWithIntendation "reference" submarine.reference 3

            else
                ""
           )
        ++ (if String.length submarine.contextDir > 0 then
                YamlUtils.getNameAndValueWithIntendation "contextDir" submarine.contextDir 3

            else
                ""
           )
