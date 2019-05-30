module Main exposing (Msg(..), Submarine, init, main, subscriptions, update, view)

import Browser
import Html exposing (Html, br, button, div, input, option, pre, select, text, textarea)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode exposing (Decoder, field, string)
import Task
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
      , availableOpenShiftProjects = OpenShiftProjectsLoading
      , gitHubServiceAccount = GitHubResourceNotLoaded
      , gitHubRole = GitHubResourceNotLoaded
      , gitHubRoleBinding = GitHubResourceNotLoaded
      , gitHubOperator = GitHubResourceNotLoaded
      , operatorDeploymentStatus = OperatorNotDeployed
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
    , availableOpenShiftProjects : OpenShiftProjects
    , gitHubServiceAccount : GitHubResource
    , gitHubRole : GitHubResource
    , gitHubRoleBinding : GitHubResource
    , gitHubOperator : GitHubResource
    , operatorDeploymentStatus : OperatorDeploymentStatus
    , runtime : Runtime
    , replicas : Maybe Int
    , incremental : Bool
    , gitUrl : String
    , reference : String
    , contextDir : String
    }


type alias OpenShiftProject =
    { name : String
    }


type OpenShiftProjects
    = OpenShiftProjectsLoading
    | OpenShiftProjectsEmpty
    | OpenShiftProjectsLoaded (List String) (Maybe OpenShiftProject)
    | OpenShiftProjectsError Http.Error


type Runtime
    = Quarkus
    | SpringBoot


type GitHubResource
    = GitHubResourceNotLoaded
    | GitHubResourceLoading
    | GitHubResourceSuccess String
    | GitHubResourceError


type OperatorDeploymentStatus
    = OperatorNotDeployed
    | OperatorDeploying
    | OperatorDeployed
    | OperatorDeploymentError String



-- UPDATE


type Msg
    = UpdateGitUrl String
    | UpdateRuntime Runtime
    | UpdateReplicas String
    | SelectIncremental
    | UpdateReference String
    | UpdateContextDirectory String
    | GotOpenShiftProjects (Result Http.Error (List String))
    | ChangeOpenShiftProject (List String) String
    | GotSubmarineServiceAccountYaml (Result Http.Error String)
    | GotSubmarineRoleYaml (Result Http.Error String)
    | GotSubmarineRoleBindingYaml (Result Http.Error String)
    | GotSubmarineOperatorYaml (Result Http.Error String)
    | SubmarineServiceAccountCreated String (Result Http.Error ())
    | SubmarineRoleCreated String (Result Http.Error ())
    | SubmarineRoleBindingCreated String (Result Http.Error ())
    | SubmarineOperatorCreated String (Result Http.Error ())
    | DeploySubmarineOperator String
    | DeploySubmarineCustomResource String


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
                    ( { submarine | availableOpenShiftProjects = OpenShiftProjectsLoaded ([ firstProject ] ++ otherProjects) Nothing }
                        |> (\s -> { s | gitHubServiceAccount = GitHubResourceLoading })
                    , Task.attempt GotSubmarineServiceAccountYaml getSubmarineServiceAccountYaml
                    )

                Ok [] ->
                    ( { submarine | availableOpenShiftProjects = OpenShiftProjectsEmpty }
                        |> (\s -> { s | gitHubServiceAccount = GitHubResourceLoading })
                    , Task.attempt GotSubmarineServiceAccountYaml getSubmarineServiceAccountYaml
                    )

                Err error ->
                    ( { submarine | availableOpenShiftProjects = OpenShiftProjectsError error }
                    , Cmd.none
                    )

        ChangeOpenShiftProject projects newOpenShiftProject ->
            ( { submarine | availableOpenShiftProjects = OpenShiftProjectsLoaded projects (Just { name = newOpenShiftProject }) }
            , Cmd.none
            )

        GotSubmarineServiceAccountYaml result ->
            case result of
                Ok loadedSubmarineServiceAccountYaml ->
                    ( { submarine | gitHubServiceAccount = GitHubResourceSuccess loadedSubmarineServiceAccountYaml }
                        |> (\s -> { s | gitHubRole = GitHubResourceLoading })
                    , Task.attempt GotSubmarineRoleYaml getSubmarineRoleYaml
                    )

                Err error ->
                    ( { submarine | gitHubServiceAccount = GitHubResourceError }
                        |> (\s -> { s | gitHubRole = GitHubResourceLoading })
                    , Task.attempt GotSubmarineRoleYaml getSubmarineRoleYaml
                    )

        GotSubmarineRoleYaml result ->
            case result of
                Ok loadedSubmarineRoleYaml ->
                    ( { submarine | gitHubRole = GitHubResourceSuccess loadedSubmarineRoleYaml }
                        |> (\s -> { s | gitHubRoleBinding = GitHubResourceLoading })
                    , Task.attempt GotSubmarineRoleBindingYaml getSubmarineRoleBindingYaml
                    )

                Err error ->
                    ( { submarine | gitHubRole = GitHubResourceError }
                        |> (\s -> { s | gitHubRoleBinding = GitHubResourceLoading })
                    , Task.attempt GotSubmarineRoleBindingYaml getSubmarineRoleBindingYaml
                    )

        GotSubmarineRoleBindingYaml result ->
            case result of
                Ok loadedSubmarineRoleBindingYaml ->
                    ( { submarine | gitHubRoleBinding = GitHubResourceSuccess loadedSubmarineRoleBindingYaml }
                        |> (\s -> { s | gitHubOperator = GitHubResourceLoading })
                    , Task.attempt GotSubmarineOperatorYaml getSubmarineOperatorYaml
                    )

                Err error ->
                    ( { submarine | gitHubRoleBinding = GitHubResourceError }
                        |> (\s -> { s | gitHubOperator = GitHubResourceLoading })
                    , Task.attempt GotSubmarineOperatorYaml getSubmarineOperatorYaml
                    )

        GotSubmarineOperatorYaml result ->
            case result of
                Ok loadedSubmarineOperatorYaml ->
                    ( { submarine | gitHubOperator = GitHubResourceSuccess loadedSubmarineOperatorYaml }
                    , Cmd.none
                    )

                Err error ->
                    ( { submarine | gitHubOperator = GitHubResourceError }
                    , Cmd.none
                    )

        DeploySubmarineOperator selectedProject ->
            case submarine.gitHubServiceAccount of
                GitHubResourceSuccess submarineServiceAccount ->
                    ( { submarine | operatorDeploymentStatus = OperatorDeploying }
                    , createSubmarineServiceAccount submarine.openShiftUrl submarine.authenticationToken selectedProject submarineServiceAccount
                    )

                _ ->
                    ( { submarine | operatorDeploymentStatus = OperatorDeploymentError "Service account from GitHub not loaded." }
                    , Cmd.none
                    )

        SubmarineServiceAccountCreated selectedProject result ->
            case result of
                Ok _ ->
                    case submarine.gitHubRole of
                        GitHubResourceSuccess submarineRole ->
                            ( submarine
                            , createSubmarineRole submarine.openShiftUrl submarine.authenticationToken selectedProject submarineRole
                            )

                        _ ->
                            ( { submarine | operatorDeploymentStatus = OperatorDeploymentError "Role from GitHub not loaded." }
                            , Cmd.none
                            )

                Err error ->
                    case error of
                        Http.BadUrl url ->
                            ( { submarine | operatorDeploymentStatus = OperatorDeploymentError ("No valid URL for service account creation: " ++ url) }
                            , Cmd.none
                            )

                        Http.NetworkError ->
                            ( { submarine | operatorDeploymentStatus = OperatorDeploymentError "Network error while creating service account." }
                            , Cmd.none
                            )

                        Http.BadStatus statusCode ->
                            ( { submarine | operatorDeploymentStatus = OperatorDeploymentError ("Bad status code while creating service account: " ++ String.fromInt statusCode) }
                            , Cmd.none
                            )

                        _ ->
                            ( { submarine | operatorDeploymentStatus = OperatorDeploymentError "Error while creating service account." }
                            , Cmd.none
                            )

        SubmarineRoleCreated selectedProject result ->
            case result of
                Ok _ ->
                    case submarine.gitHubRoleBinding of
                        GitHubResourceSuccess submarineRoleBinding ->
                            ( submarine
                            , createSubmarineRoleBinding submarine.openShiftUrl submarine.authenticationToken selectedProject submarineRoleBinding
                            )

                        _ ->
                            ( { submarine | operatorDeploymentStatus = OperatorDeploymentError "Role binding from GitHub not loaded." }
                            , Cmd.none
                            )

                Err error ->
                    case error of
                        Http.BadUrl url ->
                            ( { submarine | operatorDeploymentStatus = OperatorDeploymentError ("No valid URL for role creation: " ++ url) }
                            , Cmd.none
                            )

                        Http.NetworkError ->
                            ( { submarine | operatorDeploymentStatus = OperatorDeploymentError "Network error while creating role." }
                            , Cmd.none
                            )

                        Http.BadStatus statusCode ->
                            ( { submarine | operatorDeploymentStatus = OperatorDeploymentError ("Bad status code while creating role: " ++ String.fromInt statusCode) }
                            , Cmd.none
                            )

                        _ ->
                            ( { submarine | operatorDeploymentStatus = OperatorDeploymentError "Error while creating role." }
                            , Cmd.none
                            )

        SubmarineRoleBindingCreated selectedProject result ->
            case result of
                Ok _ ->
                    case submarine.gitHubOperator of
                        GitHubResourceSuccess submarineOperator ->
                            ( submarine
                            , createSubmarineDeployment submarine.openShiftUrl submarine.authenticationToken selectedProject submarineOperator
                            )

                        _ ->
                            ( { submarine | operatorDeploymentStatus = OperatorDeploymentError "Role binding from GitHub not loaded." }
                            , Cmd.none
                            )

                Err error ->
                    case error of
                        Http.BadUrl url ->
                            ( { submarine | operatorDeploymentStatus = OperatorDeploymentError ("No valid URL for role binding creation: " ++ url) }
                            , Cmd.none
                            )

                        Http.NetworkError ->
                            ( { submarine | operatorDeploymentStatus = OperatorDeploymentError "Network error while creating role binding." }
                            , Cmd.none
                            )

                        Http.BadStatus statusCode ->
                            ( { submarine | operatorDeploymentStatus = OperatorDeploymentError ("Bad status code while creating role binding: " ++ String.fromInt statusCode) }
                            , Cmd.none
                            )

                        _ ->
                            ( { submarine | operatorDeploymentStatus = OperatorDeploymentError "Error while creating role binding." }
                            , Cmd.none
                            )

        SubmarineOperatorCreated selectedProject result ->
            case result of
                Ok _ ->
                    ( { submarine | operatorDeploymentStatus = OperatorDeployed }
                    , Cmd.none
                    )

                Err error ->
                    case error of
                        Http.BadUrl url ->
                            ( { submarine | operatorDeploymentStatus = OperatorDeploymentError ("No valid URL for operator creation: " ++ url) }
                            , Cmd.none
                            )

                        Http.NetworkError ->
                            ( { submarine | operatorDeploymentStatus = OperatorDeploymentError "Network error while creating operator." }
                            , Cmd.none
                            )

                        Http.BadStatus statusCode ->
                            ( { submarine | operatorDeploymentStatus = OperatorDeploymentError ("Bad status code while creating operator: " ++ String.fromInt statusCode) }
                            , Cmd.none
                            )

                        _ ->
                            ( { submarine | operatorDeploymentStatus = OperatorDeploymentError "Error while creating operator." }
                            , Cmd.none
                            )

        DeploySubmarineCustomResource selectedProject ->
            ( submarine
            , createSubmarineCustomResource submarine.openShiftUrl submarine.authenticationToken selectedProject (getSubAppAsYaml submarine)
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
                ++ viewDeploySubmarineOperator submarine
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
                ++ viewDeploySubmarineCustomResource submarine
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
    case submarine.availableOpenShiftProjects of
        OpenShiftProjectsLoading ->
            [ text "Loading OpenShift projects..."
            , br [] []
            ]

        OpenShiftProjectsLoaded projects selectedProject ->
            case selectedProject of
                Just project ->
                    [ text "OpenShift projects: "
                    , select [ onInput (ChangeOpenShiftProject projects) ]
                        (List.map (\p -> option [ value p, selected (p == project.name) ] [ text p ]) projects)
                    , br [] []
                    ]

                Nothing ->
                    [ text "OpenShift projects: "
                    , select [ onInput (ChangeOpenShiftProject projects) ]
                        ([ option [ value "", selected True ] [ text "Select project" ] ]
                            ++ List.map (\p -> option [ value p, selected False ] [ text p ]) projects
                        )
                    , br [] []
                    ]

        OpenShiftProjectsEmpty ->
            [ text "No OpenShift project found."
            , br [] []
            ]

        OpenShiftProjectsError errorHttp ->
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
    (case submarine.gitHubServiceAccount of
        GitHubResourceLoading ->
            [ text "Loading Service account YAML.", br [] [] ]

        GitHubResourceSuccess _ ->
            [ text "Service account YAML loaded.", br [] [] ]

        GitHubResourceError ->
            [ text "Error while loading Service account YAML.", br [] [] ]

        GitHubResourceNotLoaded ->
            []
    )
        ++ (case submarine.gitHubRole of
                GitHubResourceLoading ->
                    [ text "Loading Role YAML.", br [] [] ]

                GitHubResourceSuccess _ ->
                    [ text "Role YAML loaded.", br [] [] ]

                GitHubResourceError ->
                    [ text "Error while loading Role YAML.", br [] [] ]

                GitHubResourceNotLoaded ->
                    []
           )
        ++ (case submarine.gitHubRoleBinding of
                GitHubResourceLoading ->
                    [ text "Loading Role binding YAML.", br [] [] ]

                GitHubResourceSuccess _ ->
                    [ text "Role binding YAML loaded.", br [] [] ]

                GitHubResourceError ->
                    [ text "Error while loading Role binding YAML.", br [] [] ]

                GitHubResourceNotLoaded ->
                    []
           )
        ++ (case submarine.gitHubOperator of
                GitHubResourceLoading ->
                    [ text "Loading Operator YAML.", br [] [] ]

                GitHubResourceSuccess _ ->
                    [ text "Operator YAML loaded.", br [] [] ]

                GitHubResourceError ->
                    [ text "Error while loading Operator YAML.", br [] [] ]

                GitHubResourceNotLoaded ->
                    []
           )


viewDeploySubmarineOperator : Submarine -> List (Html Msg)
viewDeploySubmarineOperator submarine =
    case submarine.availableOpenShiftProjects of
        OpenShiftProjectsLoaded projects selectedProject ->
            case selectedProject of
                Just project ->
                    [ button [ onClick (DeploySubmarineOperator project.name) ] [ text "Deploy Submarine Operator" ]
                    , br [] []
                    ]
                        ++ (case submarine.operatorDeploymentStatus of
                                OperatorNotDeployed ->
                                    []

                                OperatorDeploying ->
                                    [ text "Submarine Operator is being deployed."
                                    , br [] []
                                    ]

                                OperatorDeployed ->
                                    [ text "Submarine Operator deployed."
                                    , br [] []
                                    ]

                                OperatorDeploymentError deploymentError ->
                                    [ text ("Error while deploying Submarine Operator: " ++ deploymentError)
                                    , br [] []
                                    ]
                           )

                Nothing ->
                    []

        _ ->
            []


viewDeploySubmarineCustomResource : Submarine -> List (Html Msg)
viewDeploySubmarineCustomResource submarine =
    case submarine.availableOpenShiftProjects of
        OpenShiftProjectsLoaded projects selectedProject ->
            case selectedProject of
                Just project ->
                    [ button [ onClick (DeploySubmarineCustomResource project.name) ] [ text "Deploy Submarine Custom resource" ]
                    , br [] []
                    ]

                Nothing ->
                    []

        _ ->
            []



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


getSubmarineServiceAccountYaml : Task.Task Http.Error String
getSubmarineServiceAccountYaml =
    Http.task
        { method = "GET"
        , headers = []
        , url = "https://raw.githubusercontent.com/kiegroup/submarine-cloud-operator/master/deploy/service_account.yaml"
        , body = Http.emptyBody
        , resolver = Http.stringResolver handleResponse
        , timeout = Nothing
        }


getSubmarineRoleYaml : Task.Task Http.Error String
getSubmarineRoleYaml =
    Http.task
        { method = "GET"
        , headers = []
        , url = "https://raw.githubusercontent.com/kiegroup/submarine-cloud-operator/master/deploy/role.yaml"
        , body = Http.emptyBody
        , resolver = Http.stringResolver handleResponse
        , timeout = Nothing
        }


getSubmarineRoleBindingYaml : Task.Task Http.Error String
getSubmarineRoleBindingYaml =
    Http.task
        { method = "GET"
        , headers = []
        , url = "https://raw.githubusercontent.com/kiegroup/submarine-cloud-operator/master/deploy/role_binding.yaml"
        , body = Http.emptyBody
        , resolver = Http.stringResolver handleResponse
        , timeout = Nothing
        }


getSubmarineOperatorYaml : Task.Task Http.Error String
getSubmarineOperatorYaml =
    Http.task
        { method = "GET"
        , headers = []
        , url = "https://raw.githubusercontent.com/kiegroup/submarine-cloud-operator/master/deploy/operator.yaml"
        , body = Http.emptyBody
        , resolver = Http.stringResolver handleResponse
        , timeout = Nothing
        }


createSubmarineServiceAccount : String -> String -> String -> String -> Cmd Msg
createSubmarineServiceAccount openShiftUrl authenticationToken namespace yamlContent =
    Http.request
        { method = "POST"
        , headers = [ Http.header "Authorization" ("Bearer " ++ authenticationToken) ]
        , url = openShiftUrl ++ "/api/v1/namespaces/" ++ namespace ++ "/serviceaccounts"
        , body = Http.stringBody "application/yaml" yamlContent
        , expect = Http.expectWhatever (SubmarineServiceAccountCreated namespace)
        , timeout = Nothing
        , tracker = Nothing
        }


createSubmarineRole : String -> String -> String -> String -> Cmd Msg
createSubmarineRole openShiftUrl authenticationToken namespace yamlContent =
    Http.request
        { method = "POST"
        , headers = [ Http.header "Authorization" ("Bearer " ++ authenticationToken) ]
        , url = openShiftUrl ++ "/apis/rbac.authorization.k8s.io/v1/namespaces/" ++ namespace ++ "/roles"
        , body = Http.stringBody "application/yaml" yamlContent
        , expect = Http.expectWhatever (SubmarineRoleCreated namespace)
        , timeout = Nothing
        , tracker = Nothing
        }


createSubmarineRoleBinding : String -> String -> String -> String -> Cmd Msg
createSubmarineRoleBinding openShiftUrl authenticationToken namespace yamlContent =
    Http.request
        { method = "POST"
        , headers = [ Http.header "Authorization" ("Bearer " ++ authenticationToken) ]
        , url = openShiftUrl ++ "/apis/rbac.authorization.k8s.io/v1/namespaces/" ++ namespace ++ "/rolebindings"
        , body = Http.stringBody "application/yaml" yamlContent
        , expect = Http.expectWhatever (SubmarineRoleBindingCreated namespace)
        , timeout = Nothing
        , tracker = Nothing
        }


createSubmarineDeployment : String -> String -> String -> String -> Cmd Msg
createSubmarineDeployment openShiftUrl authenticationToken namespace yamlContent =
    Http.request
        { method = "POST"
        , headers = [ Http.header "Authorization" ("Bearer " ++ authenticationToken) ]
        , url = openShiftUrl ++ "/apis/apps/v1/namespaces/" ++ namespace ++ "/deployments"
        , body = Http.stringBody "application/yaml" yamlContent
        , expect = Http.expectWhatever (SubmarineOperatorCreated namespace)
        , timeout = Nothing
        , tracker = Nothing
        }


createSubmarineCustomResource : String -> String -> String -> String -> Cmd Msg
createSubmarineCustomResource openShiftUrl authenticationToken namespace yamlContent =
    Http.request
        { method = "POST"
        , headers = [ Http.header "Authorization" ("Bearer " ++ authenticationToken) ]
        , url = openShiftUrl ++ "/apis/app.kiegroup.org/v1alpha1/namespaces/" ++ namespace ++ "/subapps"
        , body = Http.stringBody "application/yaml" yamlContent
        , expect = Http.expectWhatever (SubmarineOperatorCreated namespace)
        , timeout = Nothing
        , tracker = Nothing
        }


handleResponse : Http.Response a -> Result Http.Error a
handleResponse response =
    case response of
        Http.BadUrl_ url ->
            Err (Http.BadUrl url)

        Http.Timeout_ ->
            Err Http.Timeout

        Http.BadStatus_ { statusCode } _ ->
            Err (Http.BadStatus statusCode)

        Http.NetworkError_ ->
            Err Http.NetworkError

        Http.GoodStatus_ _ body ->
            Ok body



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
