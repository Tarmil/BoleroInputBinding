module BoleroInputBinding.Client.Main

open System
open System.Net.Http
open System.Net.Http.Json
open Microsoft.AspNetCore.Components
open Elmish
open Bolero
open Bolero.Html

/// Routing endpoints definition.
type Page =
    | [<EndPoint "/">] Home
    | [<EndPoint "/counter">] Counter
    | [<EndPoint "/input">] Input
    | [<EndPoint "/data">] Data

/// The Elmish application's model.
type Model =
    {
        page: Page
        counter: int
        inputString: string
        label: string
        books: Book[] option
        error: string option
    }

and Book =
    {
        title: string
        author: string
        publishDate: DateTime
        isbn: string
    }

let initModel =
    {
        page = Home
        counter = 0
        inputString = ""
        label = ""
        books = None
        error = None
    }


/// The Elmish application's update messages.
type Message =
    | SetPage of Page
    | Increment
    | Decrement
    | SetCounter of int
    | ResetInput
    | SetLabel of string
    | GetBooks
    | GotBooks of Book[]
    | Error of exn
    | ClearError

let update (http: HttpClient) message model =
    match message with
    | SetPage page ->
        { model with page = page }, Cmd.none

    | Increment ->
        { model with counter = model.counter + 1 }, Cmd.none
    | Decrement ->
        { model with counter = model.counter - 1 }, Cmd.none
    | SetCounter value ->
        { model with counter = value }, Cmd.none
    
    | ResetInput ->
        { model with inputString = ""}, Cmd.none
    | SetLabel s ->
        { model with label = s}, Cmd.none

    | GetBooks ->
        let getBooks() = http.GetFromJsonAsync<Book[]>("books.json")
        let cmd = Cmd.OfTask.either getBooks () GotBooks Error
        { model with books = None }, cmd
    | GotBooks books ->
        { model with books = Some books }, Cmd.none

    | Error exn ->
        { model with error = Some exn.Message }, Cmd.none
    | ClearError ->
        { model with error = None }, Cmd.none

/// Connects the routing system to the Elmish application.
let router = Router.infer SetPage (fun model -> model.page)

let homePage model dispatch =
    div [attr.``class`` "content"] [
        h1 [attr.``class`` "title"] [text "Welcome to Bolero!"]
        p [] [text "This application demonstrates Bolero's major features."]
        ul [] [
            li [] [
                text "The entire application is driven by "
                a [
                    attr.target "_blank"
                    attr.href "https://fsbolero.github.io/docs/Elmish"
                ] [text "Elmish"]
                text "."
            ]
            li [] [
                text "The menu on the left switches pages based on "
                a [
                    attr.target "_blank"
                    attr.href "https://fsbolero.github.io/docs/Routing"
                ] [text "routes"]
                text "."
            ]
            li [] [
                text "The "
                a [router.HRef Counter] [text "Counter"]
                text " page demonstrates event handlers and data binding in "
                a [
                    attr.target "_blank"
                    attr.href "https://fsbolero.github.io/docs/Templating"
                ] [text "HTML templates"]
                text "."
            ]
            li [] [
                text "The "
                a [router.HRef Data] [text "Download data"]
                text " page demonstrates the use of HTTP requests to the server."
            ]
            p [] [text "Enjoy writing awesome apps!"]
        ]
    ]

let counterPage model dispatch =
    concat [
        h1 [attr.``class`` "title"] [text "A simple counter"]
        p [] [
            button [
                on.click (fun _ -> dispatch Decrement)
                attr.``class`` "button"
            ] [text "-"]
            input [
                attr.``type`` "number"
                attr.id "counter"
                attr.``class`` "input"
                bind.input.int model.counter (fun v -> dispatch (SetCounter v))
            ]
            button [
                on.click (fun _ -> dispatch Increment)
                attr.``class`` "button"
            ] [text "+"]
        ]
    ]

let inputPage model dispatch =
    concat [
        h1 [attr.``class`` "title"] [text "A simple text transformer"]
        p [] [
            input [
                attr.``type`` "text"
                attr.id "textInput"
                attr.``class`` "input"
                bind.input.string model.inputString (fun v -> dispatch (SetLabel (v.ToUpper())))
            ]
        ]
        p [] [
            label [] [text (model.label)]
        ]
        p [] [
            button [
                on.click (fun _ -> dispatch ResetInput)
                attr.``class`` "button"
            ] [text "Reset"]
        ]
    ]

let dataPage model dispatch =
    concat [
        h1 [attr.``class`` "title"] [
            text "Download data "
            button [
                attr.``class`` "button"
                on.click (fun _ -> dispatch GetBooks)
            ] [text "Reload"]
        ]
        table [attr.``class`` "table is-fullwidth"] [
            thead [] [
                tr [] [
                    th [] [text "Title"]
                    th [] [text "Author"]
                    th [] [text "Published"]
                    th [] [text "ISBN"]
                ]
            ]
            tbody [] [
                cond model.books <| function
                | None ->
                    tr [] [
                        td [attr.colspan 4] [text "Downloading book list..."]
                    ]
                | Some books ->
                    forEach books <| fun book ->
                        tr [] [
                            td [] [text book.title]
                            td [] [text book.author]
                            td [] [text (book.publishDate.ToString("yyyy-MM-dd"))]
                            td [] [text book.isbn]
                        ]
            ]
        ]
    ]

let errorNotification errorText closeCallback =
    div [attr.``class`` "notification is-warning"] [
        cond closeCallback <| function
        | None -> empty
        | Some closeCallback -> button [attr.``class`` "delete"; on.click closeCallback] []
        text errorText
    ]

let menuItem (model: Model) (page: Page) (itemText: string) =
    li [] [
        a [
            attr.``class`` (if model.page = page then "is-active" else "")
            router.HRef page
        ] [text itemText]
    ]

let view model dispatch =
    div [attr.``class`` "columns"] [
        aside [attr.``class`` "column sidebar is-narrow"] [
            section [attr.``class`` "section"] [
                nav [attr.``class`` "menu"] [
                    ul [attr.``class`` "menu-list"] [
                        menuItem model Home "Home"
                        menuItem model Counter "Counter"
                        menuItem model Input "Input test"
                        menuItem model Data "Download data"
                    ]
                ]
            ]
        ]
        div [attr.``class`` "column"] [
            section [attr.``class`` "section"] [
                cond model.page <| function
                | Home -> homePage model dispatch
                | Counter -> counterPage model dispatch
                | Input -> inputPage model dispatch
                | Data ->
                    dataPage model dispatch
                div [attr.id "notification-area"] [
                    cond model.error <| function
                    | None -> empty
                    | Some err -> errorNotification err (Some (fun _ -> dispatch ClearError))
                ]
            ]
        ]
    ]

type MyApp() =
    inherit ProgramComponent<Model, Message>()

    [<Inject>]
    member val HttpClient = Unchecked.defaultof<HttpClient> with get, set

    override this.Program =
        let update = update this.HttpClient
        Program.mkProgram (fun _ -> initModel, Cmd.ofMsg GetBooks) update view
        |> Program.withRouter router
