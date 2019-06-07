module Main exposing (Msg(..), OpenShift, init, main, subscriptions, update, view)

import Browser
import Html exposing (Html, br, button, div, input, option, pre, select, text, textarea)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode exposing (Decoder, field, string)
import Task
import YamlUtils



-- MAIN


main : Program Flag OpenShift Msg
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


init : Flag -> ( OpenShift, Cmd Msg )
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


type alias OpenShift =
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
    | SubmarineServiceAccountCreated String (Result Http.Error String)
    | SubmarineRoleCreated String (Result Http.Error String)
    | SubmarineRoleBindingCreated String (Result Http.Error String)
    | SubmarineOperatorCreated String (Result Http.Error String)
    | SubmarineCustomResourceCreated String (Result Http.Error ())
    | DeploySubmarineOperator String
    | DeploySubmarineCustomResource String


update : Msg -> OpenShift -> ( OpenShift, Cmd Msg )
update msg openShift =
    case msg of
        UpdateGitUrl newGitUrl ->
            ( { openShift | gitUrl = newGitUrl }
            , Cmd.none
            )

        UpdateRuntime newRuntime ->
            ( { openShift | runtime = newRuntime }
            , Cmd.none
            )

        UpdateReplicas newReplicas ->
            case String.toInt newReplicas of
                Just newReplicasNumber ->
                    ( { openShift | replicas = Just newReplicasNumber }
                    , Cmd.none
                    )

                Nothing ->
                    ( { openShift | replicas = Nothing }
                    , Cmd.none
                    )

        SelectIncremental ->
            ( { openShift | incremental = not openShift.incremental }
            , Cmd.none
            )

        UpdateReference newReference ->
            ( { openShift | reference = newReference }
            , Cmd.none
            )

        UpdateContextDirectory newContextDir ->
            ( { openShift | contextDir = newContextDir }
            , Cmd.none
            )

        GotOpenShiftProjects result ->
            case result of
                Ok (firstProject :: otherProjects) ->
                    ( { openShift | availableOpenShiftProjects = OpenShiftProjectsLoaded ([ firstProject ] ++ otherProjects) Nothing }
                        |> (\s -> { s | gitHubServiceAccount = GitHubResourceLoading })
                    )

                Ok [] ->
                    ( { openShift | availableOpenShiftProjects = OpenShiftProjectsEmpty }
                        |> (\s -> { s | gitHubServiceAccount = GitHubResourceLoading })
                    , Task.attempt GotSubmarineServiceAccountYaml getSubmarineServiceAccountYaml
                    )

                Err error ->
                    ( { openShift | availableOpenShiftProjects = OpenShiftProjectsError error }
                    , Cmd.none
                    )

        ChangeOpenShiftProject projects newOpenShiftProject ->
            ( { openShift | availableOpenShiftProjects = OpenShiftProjectsLoaded projects (Just { name = newOpenShiftProject }) }
            , Cmd.none
            )

        GotSubmarineServiceAccountYaml result ->
            case result of
                Ok loadedSubmarineServiceAccountYaml ->
                    ( { openShift | gitHubServiceAccount = GitHubResourceSuccess loadedSubmarineServiceAccountYaml }
                        |> (\s -> { s | gitHubRole = GitHubResourceLoading })
                    , Task.attempt GotSubmarineRoleYaml getSubmarineRoleYaml
                    )

                Err error ->
                    ( { openShift | gitHubServiceAccount = GitHubResourceError }
                        |> (\s -> { s | gitHubRole = GitHubResourceLoading })
                    , Task.attempt GotSubmarineRoleYaml getSubmarineRoleYaml
                    )

        GotSubmarineRoleYaml result ->
            case result of
                Ok loadedSubmarineRoleYaml ->
                    ( { openShift | gitHubRole = GitHubResourceSuccess loadedSubmarineRoleYaml }
                        |> (\s -> { s | gitHubRoleBinding = GitHubResourceLoading })
                    , Task.attempt GotSubmarineRoleBindingYaml getSubmarineRoleBindingYaml
                    )

                Err error ->
                    ( { openShift | gitHubRole = GitHubResourceError }
                        |> (\s -> { s | gitHubRoleBinding = GitHubResourceLoading })
                    , Task.attempt GotSubmarineRoleBindingYaml getSubmarineRoleBindingYaml
                    )

        GotSubmarineRoleBindingYaml result ->
            case result of
                Ok loadedSubmarineRoleBindingYaml ->
                    ( { openShift | gitHubRoleBinding = GitHubResourceSuccess loadedSubmarineRoleBindingYaml }
                        |> (\s -> { s | gitHubOperator = GitHubResourceLoading })
                    , Task.attempt GotSubmarineOperatorYaml getSubmarineOperatorYaml
                    )

                Err error ->
                    ( { openShift | gitHubRoleBinding = GitHubResourceError }
                        |> (\s -> { s | gitHubOperator = GitHubResourceLoading })
                    , Task.attempt GotSubmarineOperatorYaml getSubmarineOperatorYaml
                    )

        GotSubmarineOperatorYaml result ->
            case result of
                Ok loadedSubmarineOperatorYaml ->
                    ( { openShift | gitHubOperator = GitHubResourceSuccess loadedSubmarineOperatorYaml }
                    , Cmd.none
                    )

                Err error ->
                    ( { openShift | gitHubOperator = GitHubResourceError }
                    , Cmd.none
                    )

        DeploySubmarineOperator selectedProject ->
            case openShift.gitHubServiceAccount of
                GitHubResourceSuccess submarineServiceAccount ->
                    ( { openShift | operatorDeploymentStatus = OperatorDeploying }
                    , Task.attempt (SubmarineServiceAccountCreated selectedProject) (createSubmarineServiceAccount openShift.openShiftUrl openShift.authenticationToken selectedProject submarineServiceAccount)
                    )

                _ ->
                    ( { openShift | operatorDeploymentStatus = OperatorDeploymentError "Service account from GitHub not loaded." }
                    , Cmd.none
                    )

        SubmarineServiceAccountCreated selectedProject result ->
            case result of
                Ok _ ->
                    case openShift.gitHubRole of
                        GitHubResourceSuccess submarineRole ->
                            ( openShift
                            , Task.attempt (SubmarineRoleCreated selectedProject) (createSubmarineRole openShift.openShiftUrl openShift.authenticationToken selectedProject submarineRole)
                            )

                        _ ->
                            ( { openShift | operatorDeploymentStatus = OperatorDeploymentError "Role from GitHub not loaded." }
                            , Cmd.none
                            )

                Err error ->
                    case error of
                        Http.BadUrl url ->
                            ( { openShift | operatorDeploymentStatus = OperatorDeploymentError ("No valid URL for service account creation: " ++ url) }
                            , Cmd.none
                            )

                        Http.NetworkError ->
                            ( { openShift | operatorDeploymentStatus = OperatorDeploymentError "Network error while creating service account." }
                            , Cmd.none
                            )

                        Http.BadStatus statusCode ->
                            ( { openShift | operatorDeploymentStatus = OperatorDeploymentError ("Bad status code while creating service account: " ++ String.fromInt statusCode) }
                            , Cmd.none
                            )

                        _ ->
                            ( { openShift | operatorDeploymentStatus = OperatorDeploymentError "Error while creating service account." }
                            , Cmd.none
                            )

        SubmarineRoleCreated selectedProject result ->
            case result of
                Ok _ ->
                    case openShift.gitHubRoleBinding of
                        GitHubResourceSuccess submarineRoleBinding ->
                            ( openShift
                            , Task.attempt (SubmarineRoleBindingCreated selectedProject) (createSubmarineRoleBinding openShift.openShiftUrl openShift.authenticationToken selectedProject submarineRoleBinding)
                            )

                        _ ->
                            ( { openShift | operatorDeploymentStatus = OperatorDeploymentError "Role binding from GitHub not loaded." }
                            , Cmd.none
                            )

                Err error ->
                    case error of
                        Http.BadUrl url ->
                            ( { openShift | operatorDeploymentStatus = OperatorDeploymentError ("No valid URL for role creation: " ++ url) }
                            , Cmd.none
                            )

                        Http.NetworkError ->
                            ( { openShift | operatorDeploymentStatus = OperatorDeploymentError "Network error while creating role." }
                            , Cmd.none
                            )

                        Http.BadStatus statusCode ->
                            ( { openShift | operatorDeploymentStatus = OperatorDeploymentError ("Bad status code while creating role: " ++ String.fromInt statusCode) }
                            , Cmd.none
                            )

                        _ ->
                            ( { openShift | operatorDeploymentStatus = OperatorDeploymentError "Error while creating role." }
                            , Cmd.none
                            )

        SubmarineRoleBindingCreated selectedProject result ->
            case result of
                Ok _ ->
                    case openShift.gitHubOperator of
                        GitHubResourceSuccess submarineOperator ->
                            ( openShift
                            , Task.attempt (SubmarineOperatorCreated selectedProject) (createSubmarineDeployment openShift.openShiftUrl openShift.authenticationToken selectedProject submarineOperator)
                            )

                        _ ->
                            ( { openShift | operatorDeploymentStatus = OperatorDeploymentError "Role binding from GitHub not loaded." }
                            , Cmd.none
                            )

                Err error ->
                    case error of
                        Http.BadUrl url ->
                            ( { openShift | operatorDeploymentStatus = OperatorDeploymentError ("No valid URL for role binding creation: " ++ url) }
                            , Cmd.none
                            )

                        Http.NetworkError ->
                            ( { openShift | operatorDeploymentStatus = OperatorDeploymentError "Network error while creating role binding." }
                            , Cmd.none
                            )

                        Http.BadStatus statusCode ->
                            ( { openShift | operatorDeploymentStatus = OperatorDeploymentError ("Bad status code while creating role binding: " ++ String.fromInt statusCode) }
                            , Cmd.none
                            )

                        _ ->
                            ( { openShift | operatorDeploymentStatus = OperatorDeploymentError "Error while creating role binding." }
                            , Cmd.none
                            )

        SubmarineOperatorCreated selectedProject result ->
            case result of
                Ok _ ->
                    ( { openShift | operatorDeploymentStatus = OperatorDeployed }
                    , Cmd.none
                    )

                Err error ->
                    case error of
                        Http.BadUrl url ->
                            ( { openShift | operatorDeploymentStatus = OperatorDeploymentError ("No valid URL for operator creation: " ++ url) }
                            , Cmd.none
                            )

                        Http.NetworkError ->
                            ( { openShift | operatorDeploymentStatus = OperatorDeploymentError "Network error while creating operator." }
                            , Cmd.none
                            )

                        Http.BadStatus statusCode ->
                            ( { openShift | operatorDeploymentStatus = OperatorDeploymentError ("Bad status code while creating operator: " ++ String.fromInt statusCode) }
                            , Cmd.none
                            )

                        _ ->
                            ( { openShift | operatorDeploymentStatus = OperatorDeploymentError "Error while creating operator." }
                            , Cmd.none
                            )

        DeploySubmarineCustomResource selectedProject ->
            ( openShift
            , createSubmarineCustomResource openShift.openShiftUrl openShift.authenticationToken selectedProject (getSubAppAsYaml openShift)
            )

        SubmarineCustomResourceCreated selectedProject result ->
            --todo: Add custom resource handling
            ( openShift
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : OpenShift -> Sub Msg
subscriptions openShift =
    Sub.none



-- VIEW


view : OpenShift -> Html Msg
view openShift =
    div []
        [ div [ style "width" "60%", style "float" "left" ]
            ([ text "OpenShift URL: ", text openShift.openShiftUrl, br [] [] ]
                ++ viewOpenShiftProjects openShift
                ++ viewGitHubResourceStatus openShift
                ++ viewDeploySubmarineOperator openShift
                ++ [ Html.fieldset []
                        ([ Html.legend [] [ text "Submarine configuration" ] ]
                            ++ viewRuntime openShift.runtime
                            ++ viewReplicas openShift
                            ++ [ text "Incremental build: "
                               , input [ type_ "checkbox", checked openShift.incremental, onClick SelectIncremental ] []
                               , br [] []
                               , text "Git URL: "
                               , input [ placeholder "Git URL", value openShift.gitUrl, onInput UpdateGitUrl ] []
                               , br [] []
                               , text "Reference: "
                               , input [ placeholder "Reference", value openShift.reference, onInput UpdateReference ] []
                               , br [] []
                               , text "Context directory: "
                               , input [ placeholder "Context directory", value openShift.contextDir, onInput UpdateContextDirectory ] []
                               , br [] []
                               ]
                        )
                   ]
                ++ viewDeploySubmarineCustomResource openShift
            )
        , div [ style "width" "40%", style "float" "left" ] [ text "Submarine app custom resource YAML: ", textarea [ cols 80, rows 25, readonly True ] [ text (getSubAppAsYaml openShift) ] ]
        ]


viewReplicas : OpenShift -> List (Html Msg)
viewReplicas openShift =
    case openShift.replicas of
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


viewOpenShiftProjects : OpenShift -> List (Html Msg)
viewOpenShiftProjects openShift =
    case openShift.availableOpenShiftProjects of
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


viewGitHubResourceStatus : OpenShift -> List (Html Msg)
viewGitHubResourceStatus openShift =
    (case openShift.gitHubServiceAccount of
        GitHubResourceLoading ->
            [ text "Loading Service account YAML.", br [] [] ]

        GitHubResourceSuccess _ ->
            [ text "Service account YAML loaded.", br [] [] ]

        GitHubResourceError ->
            [ text "Error while loading Service account YAML.", br [] [] ]

        GitHubResourceNotLoaded ->
            []
    )
        ++ (case openShift.gitHubRole of
                GitHubResourceLoading ->
                    [ text "Loading Role YAML.", br [] [] ]

                GitHubResourceSuccess _ ->
                    [ text "Role YAML loaded.", br [] [] ]

                GitHubResourceError ->
                    [ text "Error while loading Role YAML.", br [] [] ]

                GitHubResourceNotLoaded ->
                    []
           )
        ++ (case openShift.gitHubRoleBinding of
                GitHubResourceLoading ->
                    [ text "Loading Role binding YAML.", br [] [] ]

                GitHubResourceSuccess _ ->
                    [ text "Role binding YAML loaded.", br [] [] ]

                GitHubResourceError ->
                    [ text "Error while loading Role binding YAML.", br [] [] ]

                GitHubResourceNotLoaded ->
                    []
           )
        ++ (case openShift.gitHubOperator of
                GitHubResourceLoading ->
                    [ text "Loading Operator YAML.", br [] [] ]

                GitHubResourceSuccess _ ->
                    [ text "Operator YAML loaded.", br [] [] ]

                GitHubResourceError ->
                    [ text "Error while loading Operator YAML.", br [] [] ]

                GitHubResourceNotLoaded ->
                    []
           )


viewDeploySubmarineOperator : OpenShift -> List (Html Msg)
viewDeploySubmarineOperator openShift =
    case openShift.availableOpenShiftProjects of
        OpenShiftProjectsLoaded projects selectedProject ->
            case selectedProject of
                Just project ->
                    [ button [ onClick (DeploySubmarineOperator project.name) ] [ text "Deploy Submarine Operator" ]
                    , br [] []
                    ]
                        ++ (case openShift.operatorDeploymentStatus of
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


viewDeploySubmarineCustomResource : OpenShift -> List (Html Msg)
viewDeploySubmarineCustomResource openShift =
    case openShift.availableOpenShiftProjects of
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


createSubmarineServiceAccount : String -> String -> String -> String -> Task.Task Http.Error String
createSubmarineServiceAccount openShiftUrl authenticationToken namespace yamlContent =
    Http.task
        { method = "POST"
        , headers = [ Http.header "Authorization" ("Bearer " ++ authenticationToken) ]
        , url = openShiftUrl ++ "/api/v1/namespaces/" ++ namespace ++ "/serviceaccounts"
        , body = Http.stringBody "application/yaml" yamlContent
        , resolver = Http.stringResolver handleResponse
        , timeout = Nothing
        }


createSubmarineRole : String -> String -> String -> String -> Task.Task Http.Error String
createSubmarineRole openShiftUrl authenticationToken namespace yamlContent =
    Http.task
        { method = "POST"
        , headers = [ Http.header "Authorization" ("Bearer " ++ authenticationToken) ]
        , url = openShiftUrl ++ "/apis/rbac.authorization.k8s.io/v1/namespaces/" ++ namespace ++ "/roles"
        , body = Http.stringBody "application/yaml" yamlContent
        , resolver = Http.stringResolver handleResponse
        , timeout = Nothing
        }


createSubmarineRoleBinding : String -> String -> String -> String -> Task.Task Http.Error String
createSubmarineRoleBinding openShiftUrl authenticationToken namespace yamlContent =
    Http.task
        { method = "POST"
        , headers = [ Http.header "Authorization" ("Bearer " ++ authenticationToken) ]
        , url = openShiftUrl ++ "/apis/rbac.authorization.k8s.io/v1/namespaces/" ++ namespace ++ "/rolebindings"
        , body = Http.stringBody "application/yaml" yamlContent
        , resolver = Http.stringResolver handleResponse
        , timeout = Nothing
        }


createSubmarineDeployment : String -> String -> String -> String -> Task.Task Http.Error String
createSubmarineDeployment openShiftUrl authenticationToken namespace yamlContent =
    Http.task
        { method = "POST"
        , headers = [ Http.header "Authorization" ("Bearer " ++ authenticationToken) ]
        , url = openShiftUrl ++ "/apis/apps/v1/namespaces/" ++ namespace ++ "/deployments"
        , body = Http.stringBody "application/yaml" yamlContent
        , resolver = Http.stringResolver handleResponse
        , timeout = Nothing
        }


createSubmarineCustomResource : String -> String -> String -> String -> Cmd Msg
createSubmarineCustomResource openShiftUrl authenticationToken namespace yamlContent =
    Http.request
        { method = "POST"
        , headers = [ Http.header "Authorization" ("Bearer " ++ authenticationToken) ]
        , url = openShiftUrl ++ "/apis/app.kiegroup.org/v1alpha1/namespaces/" ++ namespace ++ "/subapps"
        , body = Http.stringBody "application/yaml" yamlContent
        , expect = Http.expectWhatever (SubmarineCustomResourceCreated namespace)
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


getSubAppAsYaml : OpenShift -> String
getSubAppAsYaml openShift =
    YamlUtils.getNameAndValueWithIntendation "apiVersion" "app.kiegroup.org/v1alpha1" 0
        ++ YamlUtils.getNameAndValueWithIntendation "kind" "SubApp" 0
        ++ YamlUtils.getNameWithIntendation "metadata" 0
        ++ YamlUtils.getNameAndValueWithIntendation "name" "sub-cr" 1
        ++ YamlUtils.getNameWithIntendation "spec" 0
        ++ (case openShift.runtime of
                Quarkus ->
                    YamlUtils.getNameAndValueWithIntendation "runtime" "quarkus" 1

                SpringBoot ->
                    YamlUtils.getNameAndValueWithIntendation "runtime" "springboot" 1
           )
        ++ (case openShift.replicas of
                Just replicas ->
                    YamlUtils.getNameAndValueWithIntendation "replicas" (String.fromInt replicas) 1

                Nothing ->
                    ""
           )
        ++ YamlUtils.getNameWithIntendation "build" 1
        ++ (if openShift.incremental then
                YamlUtils.getNameAndValueWithIntendation "incremental" "true" 2

            else
                YamlUtils.getNameAndValueWithIntendation "incremental" "false" 2
           )
        ++ YamlUtils.getNameWithIntendation "gitSource" 2
        ++ YamlUtils.getNameAndValueWithIntendation "uri" openShift.gitUrl 3
        ++ (if String.length openShift.reference > 0 then
                YamlUtils.getNameAndValueWithIntendation "reference" openShift.reference 3

            else
                ""
           )
        ++ (if String.length openShift.contextDir > 0 then
                YamlUtils.getNameAndValueWithIntendation "contextDir" openShift.contextDir 3

            else
                ""
           )
